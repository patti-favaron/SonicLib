! readslt.f90 - Program to read an SLT data file, and ancode it to
!               SonicLib raw data file.
!
! Author: M.Favaron
!
! Credit: W.Eugster, and his "readslt.R" file. His implementation is followed carefully.
!

PROGRAM ReadSlt

	USE Calendar

	IMPLICIT NONE
	
	! Locals
	CHARACTER(LEN=256)							:: sInputPrefix
	CHARACTER(LEN=256)							:: sOutputPath
	CHARACTER(LEN=20)							:: sDateTime
	CHARACTER(LEN=8)							:: sMode
	CHARACTER(LEN=256)							:: sConvert
	INTEGER										:: iNumInputFiles
	CHARACTER(LEN=256), DIMENSION(60)			:: svInputFile
	CHARACTER(LEN=256)							:: sInputFile
	CHARACTER(LEN=256)							:: sOutputFile
	INTEGER										:: iRetCode
	CHARACTER(1)								:: cRecSize	! 1 byte, whose numeric value contains the record length
	INTEGER										:: iRecSize ! The actual byte value, treated as unsigned
	CHARACTER(1), DIMENSION(:), ALLOCATABLE		:: cvHeader
	INTEGER, DIMENSION(:), ALLOCATABLE			:: ivHeader
	INTEGER(2), DIMENSION(:,:), ALLOCATABLE		:: imData
	INTEGER(4), DIMENSION(13)					:: ivValues
	REAL, DIMENSION(:), ALLOCATABLE				:: rvU, rvV, rvW, rvTv
	REAL, DIMENSION(:,:), ALLOCATABLE			:: rmAnalog
	INTEGER										:: iFileSize
	INTEGER										:: iNumRecords
	INTEGER										:: i
	INTEGER										:: iYear, iMonth, iDay, iHour, iMinute, iSecond
	INTEGER										:: iFirstSecondInYear
	INTEGER										:: iCurTime
	CHARACTER(LEN=4), DIMENSION(:), ALLOCATABLE	:: svName
	REAL, DIMENSION(:), ALLOCATABLE				:: rvMultiplier
	REAL, DIMENSION(:), ALLOCATABLE				:: rvOffset
	CHARACTER(LEN=4)							:: sName
	REAL										:: rMultiplier
	REAL										:: rOffset
	INTEGER										:: iNumSpecs
	INTEGER										:: iColumn
	CHARACTER(LEN=4), DIMENSION(6), PARAMETER	:: LEGAL_NAMES = [ &
		'q   ', 'c   ', 'a   ', 'm   ', 'temp', 'hrel' &
	]
	INTEGER										:: iPos
	CHARACTER(LEN=512)							:: sBuffer
	INTEGER										:: iYearDate, iMonthDate, iDayDate
	INTEGER										:: iCurrentYear, iCurrentDate
	INTEGER										:: iDayNum
	INTEGER										:: iHourDate, iMinute0
	INTEGER										:: iFile
	LOGICAL										:: lIsFile
	LOGICAL										:: lIsFirstFile = .TRUE.
	
	! Get parameters
	IF(COMMAND_ARGUMENT_COUNT() /= 5) THEN
		PRINT *,'readslt - Program to read SLT files as prof. Eugster''s "readslt.R"'
		PRINT *,'          Output is written in SonicLib raw standard format.'
		PRINT *
		PRINT *,'Usage:'
		PRINT *
		PRINT *,'  readslt <InputPrefix> <DateTime> <Mode> <Convert> <OutputPath>'
		PRINT *
		PRINT *,'where'
		PRINT *
		PRINT *,'  <DateTime> ::= "YYYY-MM-DD HH:00:00" (quote characters, ", mandatory)'
		PRINT *
		PRINT *,'  <Mode> ::= "eddymeas" | "eddysol"'
		PRINT *
		PRINT *,'and'
		PRINT *
		PRINT *,'  <Convert> is the name of a file containing specification conversions'
		PRINT *,'            for fifth to last columns (containing analog data).'
		PRINT *
		PRINT *,'            Each conversion is a line with three fields:'
		PRINT *
		PRINT *,'            - A variable name, according to SonicLib conventions'
		PRINT *,'              (that is, "q", "c", "a", "m", "temp", "hrel"); double'
		PRINT *,'              quote characters are mandatory.'
		PRINT *
		PRINT *,'            - A multiplier.'
		PRINT *
		PRINT *,'            - An offset.'
		PRINT *
		PRINT *,'            Example:'
		PRINT *
		PRINT *,'              "q" 0.03   0.0'
		PRINT *,'              "c" 0.0024 0.0'
		PRINT *
		PRINT *,'Copyright 2012 by the SonicLib team.'
		PRINT *
		PRINT *,'This program, part of the SonicLib project, is public-domain.'
		PRINT *
		STOP
	END IF
	CALL GET_COMMAND_ARGUMENT(1, sInputPrefix)
	CALL GET_COMMAND_ARGUMENT(2, sDateTime)
	CALL GET_COMMAND_ARGUMENT(3, sMode)
	CALL GET_COMMAND_ARGUMENT(4, sConvert)
	CALL GET_COMMAND_ARGUMENT(5, sOutputPath)
	
	! Get conversion data and store them for later use
	OPEN(10, FILE=sConvert, STATUS='OLD', ACTION='READ', IOSTAT=iRetCode)
	IF(iRetCode /= 0) THEN
		PRINT *,'readslt:: error: Conversion file not opened - Check name'
		STOP
	END IF
	iNumSpecs = 0
	DO
		READ(10, *, IOSTAT=iRetCode) sName, rMultiplier, rOffset
		IF(iRetCode /= 0) EXIT
		iPos = 0
		DO i = 1, SIZE(LEGAL_NAMES)
			IF(sName == LEGAL_NAMES(i)) THEN
				iPos = i
				EXIT
			END IF
		END DO
		IF(iPos == 0) THEN
			PRINT *,'readslt:: error: A variable name (',sName,') is not a SonicLib legal name'
			STOP
		END IF
		iNumSpecs = iNumSpecs + 1
	END DO
	REWIND(10)
	ALLOCATE(svName(iNumSpecs), rvMultiplier(iNumSpecs), rvOffset(iNumSpecs))
	DO i=1, iNumSpecs
		READ(10, *) svName(i), rvMultiplier(i), rvOffset(i)
	END DO
	CLOSE(10)
	
	! Locate input files in current hour, by generating all possible and counting how many exist
	READ(sDateTime, "(i4,3(1x,i2))", IOSTAT=iRetCode) iYearDate, iMonthDate, iDayDate, iHourDate
	IF(iRetCode /= 0) THEN
		PRINT *,'readslt:: error: Invalid date'
		STOP
	END IF
	iNumInputFiles = 0
	CALL PackTime(iCurrentDate, iYearDate, iMonthDate, iDayDate, 0, 0, 0)
	CALL PackTime(iCurrentYear, iYearDate, 1, 1, 0, 0, 0)
	iDayNum = (iCurrentDate - iCurrentYear)/(24*3600) + 1
	DO iMinute0 = 0, 59
		CALL UnpackTime(iCurrentDate + 3600*iHourDate + 60*iMinute0, iYear, iMonth, iDay, iHour, iMinute, iSecond)
		IF(sMode == "eddymeas") THEN
			WRITE(sInputFile, "(a,i3.3,2i2.2,'.slt')") &
				TRIM(sInputPrefix), iDayNum, iHour, iMinute
		ELSEIF(sMode == "eddysol") THEN
			WRITE(sInputFile, "(a,i4.4,i3.3,2i2.2,'.SLT')") &
				TRIM(sInputPrefix), iYear, iDayNum, iHour, iMinute
		END IF
		INQUIRE(FILE=sInputFile, EXIST=lIsFile)
		IF(lIsFile) THEN
			iNumInputFiles = iNumInputFiles + 1
			svInputFile(iNumInputFiles) = sInputFile
		END IF
	END DO
	! Post-condition: 'iNumInputFiles' contains the count, 'svInputFile' the actual names
	
	! Main loop: Iterate through files in current hour sequentially, and append their data
	! to a unique SonicLib raw data file
	WRITE(sOutputFile, "(a,'/',i4.4,2i2.2,'.',i2.2,'.csv')") TRIM(sOutputPath), iYearDate, iMonthDate, iDayDate, iHourDate
	OPEN(11, FILE=sOutputFile, STATUS='UNKNOWN', ACTION='WRITE')
	DO iFile = 1, iNumInputFiles
	
		print *,"Processing ",TRIM(svInputFile(iFile))
	
		! Access input files in binary mode
		OPEN(10, FILE=svInputFile(iFile), STATUS='OLD', ACCESS='STREAM', CONVERT='LITTLE_ENDIAN', ACTION='READ', IOSTAT=iRetCode)
		IF(iRetCode /= 0) THEN
			PRINT *,'readslt:: error: Input file not opened: check file name and read permissions'
			STOP
		END IF
		
		! Get SLT file header
		READ(10, IOSTAT=iRetCode) cRecSize
		IF(iRetCode /= 0) THEN
			PRINT *,'readslt:: error: Invalid input file'
			STOP
		END IF
		iRecSize = ICHAR(cRecSize)	! In SLT files count includes the record size byte: this property will be used later
		IF(iRecSize <= 0) THEN
			PRINT *,'readslt:: error: No records in input file'
			STOP
		END IF
		IF(iRecSize/2 /= iNumSpecs+4) THEN
			PRINT *,'readslt:: error: The conversion file contains a number of specs (',iNumSpecs,')'
			PRINT *,'                 which is different from the number of columns minus 4 (',iRecSize/2-4,')'
			STOP
		END IF
		print *,'-1-'
		ALLOCATE(cvHeader(iRecSize-1), ivHeader(iRecSize-1))
		print *,'-2-'
		READ(10, IOSTAT=iRetCode) cvHeader
		ivHeader = ICHAR(cvHeader)
		
		! Compose time stamp, depending on file type
		print *,'-3-'
		IF(sMode == "eddymeas") THEN
			iYear   = 100*ivHeader(4) + ivHeader(5)
			iMonth  = ivHeader(3)
			iDay    = ivHeader(2)
			iHour   = ivHeader(6)
			iMinute = ivHeader(7)
			iSecond = 0
		ELSEIF(sMode == "eddysoft") THEN
			! In this case the header does not contain the entire time stamp
			! and part of it must be inferred from file name. This conforms
			! to the following pattern:
			!
			!   CYYYYDDDHHMM.slt
			!
			! where C is a character, YYYY the year, DDD the day number within year,
			! HH the hour, and MM the minute. Of course more files may be present
			! related to the same hour, and provisions should be taken to assemble
			! them all in a single raw SonicLib file.
			IF(LEN_TRIM(sInputFile) < 16) THEN
				PRINT *,'readslt:: error: Input file name does not appear to correspond to a valid SLT'
				STOP
			END IF
			READ(sInputFile((LEN_TRIM(sInputFile)-14):), "(i4,i3,2i2)", IOSTAT=iRetCode) iYear, iDayNum, iHour, iMinute
			IF(iRetCode /= 0) THEN
				PRINT *,'readslt:: error: Input file name does not conform to SLT standard'
				STOP
			END IF
			CALL PackTime(iFirstSecondInYear, iYear, 1, 1, 0, 0, 0)
			iCurTime = iFirstSecondInYear + 3600 * (24 * (iDayNum - 1) + iHour) + 60 * iMinute
		ELSE
			PRINT *,'error:: readslt: Mode not supported. Valid modes'
			PRINT *,'                 are "eddymeas" and "eddysoft".'
			STOP
		END IF
		
		! Compute file size and use this to infer the number of data records
		print *,'-4-'
		CALL FSTAT(10, ivValues)
		iFileSize = ivValues(8)
		iNumRecords = FLOOR(iFileSize / FLOAT(iRecSize + 1))
		ALLOCATE( &
			imData(iNumRecords,iRecSize/2), &
			rvU(iNumRecords), &
			rvV(iNumRecords), &
			rvW(iNumRecords), &
			rvTv(iNumRecords), &
			rmAnalog(iNumRecords, iRecSize/2 - 4) &
		)
		
		! Read data set
		print *,'-5-'
		DO i = 1, iNumRecords
			READ(10,IOSTAT=iRetCode) imData(i, :)
		END DO
		CLOSE(10)
		
		! Perform basic conversions
		print *,'-6-'
		rvU = imData(:,1) / 100.0
		rvV = imData(:,2) / 100.0
		rvW = imData(:,3) / 100.0
		IF(sMode == "eddymeas") THEN
			rvTv = (imData(:,4)/50.)**2 / (1.402*287.64)
		ELSE
			rvTv = imData(:,4)/100.
		END IF
		rvTv = rvTv - 273.15	! Convert K to °C, according to SonicLib convention
		
		! Convert analog data
		print *,'-7-'
		DO iColumn = 5, iRecSize/2
			rmAnalog(:,iColumn-4) = rvMultiplier(iColumn-4)*imData(:,iColumn) + rvOffset(iColumn-4)
		END DO
		
		! Write output header, if the file read is the first one.
		! WARNING: This all makes sense if the files within current hour share
		!          the same contents, something not directly verified here.
		!          Checking for differences in configuration would be an
		!          interesting improvement area.
		print *,'-8-'
		IF(lIsFirstFile) THEN
			WRITE(sBuffer, "('u, v, w, t',6(',',a))") (svName(i), i=1,SIZE(svName))
			IF(sBuffer(LEN_TRIM(sBuffer):LEN_TRIM(sBuffer)) == ',') sBuffer(LEN_TRIM(sBuffer):LEN_TRIM(sBuffer)) = ' '
			WRITE(11,"(a)") TRIM(sBuffer)
			lIsFirstFile = .FALSE.
		END IF
		
		! Write the actual records
		print *,'-9-'
		DO i = 1, iNumRecords
			WRITE(sBuffer,"(10(f8.2,','))") rvU(i), rvV(i), rvW(i), rvTv(i), rmAnalog(i,:)
			IF(sBuffer(LEN_TRIM(sBuffer):LEN_TRIM(sBuffer)) == ',') sBuffer(LEN_TRIM(sBuffer):LEN_TRIM(sBuffer)) = ' '
			WRITE(11,"(a)") TRIM(sBuffer)
		END DO
		
		! Clean workspace, and prepare to next hour
		DEALLOCATE(imData,rvU,rvV,rvW,rvTv,rmAnalog)
		DEALLOCATE(cvHeader, ivHeader)
		
	END DO
 	CLOSE(11)
	
	! Leave
	DEALLOCATE(svName, rvMultiplier, rvOffset)

END PROGRAM ReadSlt
