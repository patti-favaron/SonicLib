! mfc1 - Meteoflux Core V1 adapter to SonicLib standard format
!
! Copyright 2012 by Servizi Territorio srl
!                   All rights reserved
!
!	This is Public Domain Software, released as part of the open source
!	project "SonicLib".
!
!	Users may use, copy, modify and tweak the software as they like, in
!	full freedom. In case, Servizi Territorio srl is glad you, the User,
!	let us know having found this piece of software interesting/useful.
!
!	We kindly suggest, but do not require, you cite Servizi Territorio srl
!	if this program is used as an instrument to process data used for a
!	scientific paper or report.
!
!	You may contact the author, Mauri Favaron, at the following mail address:
!
!		mafavaron@mac.com
!
!	Of course Servizi Territorio srl does assume no responsibility
!	about the program suitability to users need.
!
PROGRAM MFC1_Adapter

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
	INTEGER												:: iV, iU, iW, iT
	INTEGER												:: iAnalogA, iAnalogB, iAnalogC, iAnalogD
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
		PRINT *, "MFC1 - Meteoflux Core V1 adapter for SonicLib"
		PRINT *
		PRINT *,"Usage:"
		PRINT *
		PRINT *,"  ./mfc1 <InputFile> [<CvtFile>] <OutputFile>"
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
		PRINT *,"This is open source software, under license ..."
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
			PRINT *,"mfc1:: warning: Conversion file not found, converting only sonic quadruples"
			lConvertAnalog = .FALSE.
		ELSE
			iNumCvtSpecs = 0
			DO
				READ(10, *, IOSTAT=iRetCode) iChannel, rMultiplier, rOffset, sVarName
				IF(iRetCode /= 0) EXIT
				iNumCvtSpecs = iNumCvtSpecs + 1
			END DO
			IF(iNumCvtSpecs <= 0) THEN
				PRINT *,"mfc1:: warning: Conversion file is empty, converting only sonic quadruples"
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
			PRINT *,"mfc1:: warning: Two identical channels in conversion file, converting only sonic quadruples"
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
				PRINT *,"mfc1:: warning: Quantity ", TRIM(svVarName(i)), " at channel ", ivChannel(i), "not found"
				lConvertAnalog = .FALSE.
			END IF
		END DO
		
	END IF

	! Count sonic quadruples in file, and use this number to reserve workspace;
	! if the number of data is insufficient do not perform the conversion.
	OPEN(10, FILE=sInputFile, STATUS="OLD", ACTION="READ", IOSTAT=iRetCode)
	IF(iRetCode /= 0) THEN
		PRINT *,"mfc1:: error: Input file not opened"
		STOP
	END IF
	iNumData = 0
	DO
		READ(10, *, IOSTAT=iRetCode) sBuffer, rHiResTime, iSecond
		IF(iRetCode /= 0) EXIT
		IF(sBuffer(3:3) == 'x') iNumData = iNumData + 1
	END DO
	IF(iNumData < 100) THEN
		PRINT *,"mfc1:: error: Insufficient data in input file"
		STOP
	END IF
	ALLOCATE(imRecord(iNumData, NUM_COLS), rvTimeStamp(iNumData), STAT=iRetCode)
	IF(iRetCode /= 0) THEN
		PRINT *,"mfc1:: error: Impossible to reserve workspace in internal memory"
		STOP
	END IF
	imRecord = -99999
	REWIND(10)
	
	! Get actual data
	iData = 0
	DO
	
		! Get next data line
		READ(10, *, IOSTAT=iRetCode) sBuffer, rHiResTime, iSecond
		IF(iRetCode /= 0) EXIT
		
		! Parse line data contents, and place result in the appropriate columns of data matrix
		IF(sBuffer(3:3) == 'x') THEN
			iData = iData + 1
			READ(sBuffer, "(1x,4(4x,i6))", IOSTAT=iRetCode) iV, iU, iW, iT
			IF(iRetCode /= 0) THEN
				iU = -99999
				iV = -99999
				iW = -99999
				iT = -99999
			END IF
			rvTimeStamp(iData) = iSecond
			imRecord(iData,1) = iU
			imRecord(iData,2) = iV
			imRecord(iData,3) = iW
			imRecord(iData,4) = iT
		ELSEIF(sBuffer(3:4) == 'a0' .OR. sBuffer(3:4) == 'e1') THEN
			IF(iData > 0) THEN
				READ(sBuffer, "(1x,4(4x,i6))", IOSTAT=iRetCode) iAnalogA, iAnalogB, iAnalogC, iAnalogD
				IF(iRetCode /= 0) THEN
					iAnalogA = -99999
					iAnalogB = -99999
					iAnalogC = -99999
					iAnalogD = -99999
				END IF
				imRecord(iData,5) = iAnalogA
				imRecord(iData,6) = iAnalogB
				imRecord(iData,7) = iAnalogC
				imRecord(iData,8) = iAnalogD
			END IF
		ELSEIF(sBuffer(3:4) == 'a4' .OR. sBuffer(3:4) == 'e5') THEN
			IF(iData > 0) THEN
				READ(sBuffer, "(1x,4(4x,i6))", IOSTAT=iRetCode) iAnalogA, iAnalogB, iAnalogC, iAnalogD
				IF(iRetCode /= 0) THEN
					iAnalogA = -99999
					iAnalogB = -99999
					iAnalogC = -99999
					iAnalogD = -99999
				END IF
				imRecord(iData,9)  = iAnalogA
				imRecord(iData,10) = iAnalogB
				imRecord(iData,11) = iAnalogC
				imRecord(iData,12) = iAnalogD
			END IF
		ELSEIF(sBuffer(3:3) == 'c') THEN
			IF(iData > 0) THEN
				READ(sBuffer, "(1x,4(4x,i6))", IOSTAT=iRetCode) iAnalogA, iAnalogB
				IF(iRetCode /= 0) THEN
					iAnalogA = -99999
					iAnalogB = -99999
				END IF
				imRecord(iData,13) = iAnalogA
				imRecord(iData,14) = iAnalogB
			END IF
		END IF
		
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
		PRINT *,"mfc1:: error: At least one column in sonic quadruple missing"
		STOP
	END IF
	IF(COUNT(ivColumnIndex(1:iNonEmptyCols) <= 4) < 4) THEN
		PRINT *,"mfc1:: error: At least one column in sonic quadruple missing"
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
		PRINT *,"mfc1:: warning: None of the analog channels to convert exists in data set"
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
	
END PROGRAM MFC1_Adapter
