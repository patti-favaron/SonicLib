! mfc2 - Meteoflux Core V2 adapter to SonicLib standard format
!
! Written by: Patrizia Favaron
! e-mail:     patti.favaron@gmail.com
!
! With many thanks to the Environmental Physics research group
! within the Physics Department "Aldo Pontremoli" of the
! University of Milan for their help, support, and encouragement.
!
!------------------------------------------------------------------
! Statement of Licensing Conditions
!------------------------------------------------------------------
!
! Copyright 2022 Patrizia Favaron
!
! Permission is hereby granted, free of charge, to any person
! obtaining a copy of this software and associated documentation
! files (the "Software"), to deal in the Software without
! restriction, including without limitation the rights to use,
! copy, modify, merge, publish, distribute, sublicense, and/or
! sell copies of the Software, and to permit persons to whom the
! Software is furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be
! included in all copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
! EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
! OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
! NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
! HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
! WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
! FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
! OTHER DEALINGS IN THE SOFTWARE.
!
!------------------------------------------------------------------
PROGRAM MFC2_Adapter

	IMPLICIT NONE
	
	! Locals
	CHARACTER(LEN=256)									:: sInputFile
	CHARACTER(LEN=256)									:: sCvtFile
	CHARACTER(LEN=256)									:: sOutputFile
	INTEGER												:: iRetCode
	CHARACTER(LEN=256)									:: sBuffer
	INTEGER, DIMENSION(:,:), ALLOCATABLE				:: imRecord
	REAL, DIMENSION(:), ALLOCATABLE						:: rvTimeStamp
	INTEGER, PARAMETER									:: NUM_COLS = 14
	CHARACTER(LEN=*), DIMENSION(NUM_COLS), PARAMETER	:: COL_NAME = [ &
		"u  ", "v  ", "w  ", "t  ", "a1 ", "a2 ", "a3 ", "a4 ", "a5 ", "a6 ", "a7 ", "a8 ", "a9 ", "a10" &
	]
	CHARACTER(LEN=*), DIMENSION(10), PARAMETER			:: KNOWN_ANALOG = [ &
		"u   ", "v   ", "w   ", "t   ", "q   ", "c   ", "a   ", "m   ", "temp", "hrel" &
	]
	REAL												:: rHiResTime
	INTEGER												:: iSecond
	INTEGER												:: iNumData
	INTEGER												:: iData
	INTEGER												:: iRecordType
	INTEGER(2), DIMENSION(4)							:: ivValues
	REAL												:: rBaseTime
	INTEGER												:: iColumn
	INTEGER, DIMENSION(NUM_COLS)						:: ivColumnIndex
	INTEGER												:: iNonEmptyCols
	INTEGER												:: iPosLastChar
	LOGICAL												:: lConvertAnalog
	INTEGER												:: iChannel
	REAL												:: rMultiplier
	REAL												:: rOffset
	CHARACTER(LEN=4)									:: sVarName
	INTEGER, DIMENSION(:), ALLOCATABLE					:: ivChannel
	REAL, DIMENSION(:), ALLOCATABLE						:: rvMultiplier
	REAL, DIMENSION(:), ALLOCATABLE						:: rvOffset
	CHARACTER(LEN=4), DIMENSION(:), ALLOCATABLE			:: svVarName
	INTEGER, DIMENSION(NUM_COLS)						:: ivUsedChannels
	REAL, DIMENSION(NUM_COLS)							:: rvUsedMultiplier
	REAL, DIMENSION(NUM_COLS)							:: rvUsedOffset
	CHARACTER(LEN=4), DIMENSION(NUM_COLS)				:: svUsedVarName
	REAL, DIMENSION(NUM_COLS)							:: rvRecord
	INTEGER												:: iNumUsedChannels
	INTEGER												:: iSonicData
	INTEGER												:: iCvtSpec
	INTEGER												:: iNumCvtSpecs
	LOGICAL												:: lTwoEquals
	INTEGER												:: i, j
	LOGICAL												:: lQuantityFound
	
	! Get parameters
	IF(COMMAND_ARGUMENT_COUNT() /= 2 .AND. COMMAND_ARGUMENT_COUNT() /= 3) THEN
		PRINT *, "mfc2 - Meteoflux Core V2 adapter for SonicLib"
		PRINT *
		PRINT *,"Usage:"
		PRINT *
		PRINT *,"  ./mfc2 <InputFile> [<CvtFile>] <OutputFile>"
		PRINT *
		PRINT *,"where <CvtFile> designates the name of the conversion file, an ASCII"
		PRINT *,"text file whose lines are identically structured and structured as"
		PRINT *,"shown here:"
		PRINT *
		PRINT *,"  <ChannelNo> <Multiplier> <Offset> <Meanings>"
		PRINT *
		PRINT *,"<ChannelNo> is an integer from 1 to the maximum channel ID (14 in case"
		PRINT *,"            of Meteoflux Core / Metek USA1)"
		PRINT *
		PRINT *,"<Multiplier> and <Offset> specify the linear conversion to physical units."
		PRINT *,"Copyright 2012 by the SonicLib Team - Università Statale di Milano - Dip.Fisica"
		PRINT *
		PRINT *,"<Meanings> is a string chosen in the following set:"
		PRINT *
		PRINT *,'   "q"      H2O molar concentration, mmol/m3'
		PRINT *,'   "c"      CO2 molar concentration, mmol/m3'
		PRINT *,'   "a"      NH3 molar concentration, mmol/m3'
		PRINT *,'   "m"      CH4 molar concentration, mmol/m3'
		PRINT *,'   "temp"   Air temperature, °C'
		PRINT *,'   "hrel"   Relative humidity, %'
		PRINT *
		PRINT *,'(enclosing " are mandatory)'
		PRINT *
		PRINT *,"Output contains within-hour time stamp, wind components, sonic temperature,"
		PRINT *,"and all analog data whose channels exist and are converted. All in standard"
		PRINT *,"SonicLib table form."
		PRINT *
		PRINT *,"This is open source software, under MIT license"
		PRINT *
		STOP
	END IF
	IF(COMMAND_ARGUMENT_COUNT() == 3) THEN
		CALL GET_COMMAND_ARGUMENT(1, sInputFile)
		CALL GET_COMMAND_ARGUMENT(2, sCvtFile)
		CALL GET_COMMAND_ARGUMENT(3, sOutputFile)
	ELSE
		CALL GET_COMMAND_ARGUMENT(1, sInputFile)
		sCvtFile = ""
		CALL GET_COMMAND_ARGUMENT(2, sOutputFile)
	END IF
	
	! Get conversion file
	lConvertAnalog = sCvtFile /= " "
	IF(lConvertAnalog) THEN
	
		OPEN(10, FILE=sCvtFile, STATUS="OLD", ACTION="READ", IOSTAT=iRetCode)
		IF(iRetCode /= 0) THEN
			PRINT *,"mfc2:: warning: Conversion file not found, converting only sonic quadruples"
			lConvertAnalog = .FALSE.
		ELSE
			iNumCvtSpecs = 0
			DO
				READ(10, *, IOSTAT=iRetCode) iChannel, rMultiplier, rOffset, sVarName
				IF(iRetCode /= 0) EXIT
				iNumCvtSpecs = iNumCvtSpecs + 1
			END DO
			IF(iNumCvtSpecs <= 0) THEN
				PRINT *,"mfc2:: warning: Conversion file is empty, converting only sonic quadruples"
				lConvertAnalog = .FALSE.
			ELSE
				ALLOCATE(ivChannel(iNumCvtSpecs), rvMultiplier(iNumCvtSpecs), rvOffset(iNumCvtSpecs), svVarName(iNumCvtSpecs))
				REWIND(10)
				DO iCvtSpec = 1, iNumCvtSpecs
					READ(10, *) ivChannel(iCvtSpec), rvMultiplier(iCvtSpec), rvOffset(iCvtSpec), svVarName(iCvtSpec)
				END DO
			END IF
		END IF
		CLOSE(10)
		
		! Check no two channel to coincide, and all quantities to be known
		lTwoEquals = .FALSE.
loop:	DO i = 1, iNumCvtSpecs-1
			DO j = i+1, iNumCvtSpecs
				IF(ivChannel(i) == ivChannel(j)) THEN
					lTwoEquals = .TRUE.
					EXIT loop
				END IF
			END DO
		END DO loop
		IF(lTwoEquals) THEN
			PRINT *,"mfc2:: warning: Two identical channels in conversion file, converting only sonic quadruples"
			lConvertAnalog = .FALSE.
		END IF
		DO i = 1, iNumCvtSpecs
			lQuantityFound = .TRUE.
			DO j = 1, SIZE(KNOWN_ANALOG)
				IF(svVarName(i) == KNOWN_ANALOG(j)) THEN
					lQuantityFound = .TRUE.
					EXIT
				END IF
			END DO
			IF(.NOT.lQuantityFound) THEN
				PRINT *,"mfc2:: warning: Quantity ", TRIM(svVarName(i)), " at channel ", ivChannel(i), "not found"
				lConvertAnalog = .FALSE.
			END IF
		END DO
		
	END IF

	! Count sonic quadruples in file, and use this number to reserve workspace;
	! if the number of data is insufficient do not perform the conversion.
	OPEN(10, FILE=sInputFile, STATUS="OLD", ACTION="READ", ACCESS="STREAM", IOSTAT=iRetCode)
	IF(iRetCode /= 0) THEN
		PRINT *,"mfc2:: error: Input file not opened"
		STOP
	END IF
	iNumData = 0
	DO
		iRetCode = GetRecord(10, iSecond, iRecordType, ivValues)
		IF(iRetCode /= 0) EXIT
		IF(iRecordType == 1) iNumData = iNumData + 1
	END DO
	IF(iNumData < 100) THEN
		PRINT *,"mfc2:: error: Insufficient data in input file"
		STOP
	END IF
	ALLOCATE(imRecord(iNumData, NUM_COLS), rvTimeStamp(iNumData), STAT=iRetCode)
	IF(iRetCode /= 0) THEN
		PRINT *,"mfc2:: error: Impossible to reserve workspace in internal memory"
		STOP
	END IF
	imRecord = -99999
	REWIND(10)
	
	! Get actual data
	iData = 0
	DO
	
		! Get next data line
		iRetCode = GetRecord(10, iSecond, iRecordType, ivValues)
		IF(iRetCode /= 0) EXIT
		
		! Parse line data contents, and place result in the appropriate columns of data matrix
		SELECT CASE(iRecordType)
		CASE(1)
			iData = iData + 1
			rvTimeStamp(iData) = iSecond
			imRecord(iData,1) = ivValues(2)
			imRecord(iData,2) = ivValues(1)
			imRecord(iData,3) = ivValues(3)
			imRecord(iData,4) = ivValues(4)
		CASE(2)
			IF(iData > 0) THEN
				imRecord(iData,5) = ivValues(1)
				imRecord(iData,6) = ivValues(2)
				imRecord(iData,7) = ivValues(3)
				imRecord(iData,8) = ivValues(4)
			END IF
		CASE(3)
			IF(iData > 0) THEN
				imRecord(iData,9)  = ivValues(1)
				imRecord(iData,10) = ivValues(2)
				imRecord(iData,11) = ivValues(3)
				imRecord(iData,12) = ivValues(4)
			END IF
		END SELECT
		
	END DO
	CLOSE(10)
	
	! Identify which columns contain data, and get their index in vector "ivColumnIndex".
	! Their number is in "iNonEmptyCols"
	iNonEmptyCols = 0
	DO iColumn = 1, NUM_COLS
		IF(COUNT(imRecord(:,iColumn) > -99999) > 0) THEN
			iNonEmptyCols = iNonEmptyCols + 1
			ivColumnIndex(iNonEmptyCols) = iColumn
			! PRINT *,"Col.", iColumn, "   Valid data: ", COUNT(imRecord(:,iColumn) > -99999)
		END IF
	END DO
	
	! Are sonic quadruples represented? If not, it makes no sense to go on.
	IF(iNonEmptyCols < 4) THEN
		PRINT *,"mfc2:: error: At least one column in sonic quadruple missing"
		STOP
	END IF
	IF(COUNT(ivColumnIndex(1:iNonEmptyCols) <= 4) < 4) THEN
		PRINT *,"mfc2:: error: At least one column in sonic quadruple missing"
		STOP
	END IF
	
	! Identify non-empty channels subject to conversion
	iNumUsedChannels = 0
	DO iSonicData = 1, 4
		DO iColumn = 1, iNonEmptyCols
			IF(iSonicData == ivColumnIndex(iColumn)) THEN
				iNumUsedChannels = iNumUsedChannels + 1
				ivUsedChannels(iNumUsedChannels)   = ivColumnIndex(iColumn)
				rvUsedMultiplier(iNumUsedChannels) = 0.01
				rvUsedOffset(iNumUsedChannels)     = 0.
				svUsedVarName(iNumUsedChannels)    = KNOWN_ANALOG(iSonicData)
				EXIT
			END IF
		END DO
	END DO
	IF(lConvertAnalog) THEN
		DO iCvtSpec = 1, iNumCvtSpecs
			DO iColumn = 1, iNonEmptyCols
				IF(ivChannel(iCvtSpec) == (ivColumnIndex(iColumn)-4)) THEN
					iNumUsedChannels = iNumUsedChannels + 1
					ivUsedChannels(iNumUsedChannels)   = ivColumnIndex(iColumn)
					rvUsedMultiplier(iNumUsedChannels) = rvMultiplier(iCvtSpec)
					rvUsedOffset(iNumUsedChannels)     = rvOffset(iCvtSpec)
					svUsedVarName(iNumUsedChannels)    = svVarName(iCvtSpec)
					EXIT
				END IF
			END DO
		END DO
	END IF
	IF(lConvertAnalog .AND. iNumUsedChannels <= 4) THEN
		PRINT *,"mfc2:: warning: None of the analog channels to convert exists in data set"
		lConvertAnalog = .FALSE.
	END IF
	
	! Write data, limiting attention to columns containing at least one valid data
	! and for which a conversion line has been specified in conversion spec file.
	OPEN(10, FILE=sOutputFile, STATUS="UNKNOWN", ACTION="WRITE")
	WRITE(sBuffer, "(a,14(',',a))") "time.stamp", svUsedVarName(1:iNumUsedChannels)
	iPosLastChar = LEN_TRIM(sBuffer)
	IF(sBuffer(iPosLastChar:iPosLastChar) == ",") sBuffer(iPosLastChar:iPosLastChar) = " "
	WRITE(10, "(a)") TRIM(sBuffer)
	DO iData = 1, iNumData
		DO iChannel = 1, iNumUsedChannels
			IF(imRecord(iData,ivUsedChannels(iChannel)) >= -9990) THEN
				IF(ivUsedChannels(iChannel) <= 4) THEN
					rvRecord(iChannel) = imRecord(iData,ivUsedChannels(iChannel)) / 100.
				ELSE
					rvRecord(iChannel) = rvUsedMultiplier(iChannel) * imRecord(iData,ivUsedChannels(iChannel)) &
									   + rvUsedOffset(iChannel)
				END IF
			ELSE
				rvRecord(iChannel) = -9999.9
			END IF
		END DO
		WRITE(sBuffer, "(1x,f7.2,14(',',f9.3))") rvTimeStamp(iData), rvRecord(1:iNumUsedChannels)
		iPosLastChar = LEN_TRIM(sBuffer)
		IF(sBuffer(iPosLastChar:iPosLastChar) == ",") sBuffer(iPosLastChar:iPosLastChar) = " "
		WRITE(10, "(a)") TRIM(sBuffer)
	END DO
	CLOSE(10)
	
	! Leave
	DEALLOCATE(imRecord, rvTimeStamp)
	IF(lConvertAnalog) DEALLOCATE(ivChannel, rvMultiplier, rvOffset, svVarName)
	
CONTAINS

	FUNCTION GetRecord(iLUN, iTimeStamp, iRecordType, ivValues) RESULT(iRetCode)
	
		! Routine arguments
		INTEGER, INTENT(IN)						:: iLUN
		INTEGER, INTENT(OUT)					:: iTimeStamp
		INTEGER, INTENT(OUT)					:: iRecordType
		INTEGER(2), DIMENSION(4), INTENT(OUT)	:: ivValues
		INTEGER									:: iRetCode
		
		! Locals
		INTEGER		:: iErrCode
		INTEGER(2)	:: iTimeStamp2

		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Get record in raw form
		READ(iLUN, IOSTAT=iErrCode) iTimeStamp2, ivValues
		IF(iErrCode /= 0 .OR. iTimeStamp2 < 0) THEN
			iRetCode = 1
			RETURN
		END IF
		iRecordType = iTimeStamp2 / 5000 + 1
		iTimeStamp  = MOD(iTimeStamp2, 5000)
		
	END FUNCTION GetRecord
	
END PROGRAM MFC2_Adapter
