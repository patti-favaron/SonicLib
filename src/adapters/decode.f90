! decode - Transform data from old formats to Meteoflux Core.
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
PROGRAM decode

	USE SonicDecode
	
	IMPLICIT NONE
	
	! Locals
	CHARACTER(LEN=256)	:: sInputFile
	CHARACTER(LEN=256)	:: sOutputFile
	CHARACTER(LEN=8)	:: sBuffer
	INTEGER				:: iRetCode
	INTEGER				:: iFileType
	INTEGER				:: iNumData
	REAL, DIMENSION(72000)		:: rvTime
	INTEGER, DIMENSION(72000)	:: ivU
	INTEGER, DIMENSION(72000)	:: ivV
	INTEGER, DIMENSION(72000)	:: ivW
	INTEGER, DIMENSION(72000)	:: ivT
	INTEGER, DIMENSION(72000)	:: ivQ
	INTEGER, DIMENSION(72000)	:: ivC
	INTEGER, DIMENSION(72000)	:: ivTemp
	INTEGER, DIMENSION(72000)	:: ivUrel
	
	! Get input parameters
	IF(COMMAND_ARGUMENT_COUNT() /= 3) THEN
		PRINT *,'decode - Transform old data to Meteoflux Core raw form'
		PRINT *
		PRINT *,'Usage:'
		PRINT *
		PRINT *,'  decode <InputFile> <FileType> <OutputFile>'
		PRINT *
		PRINT *,'where'
		PRINT *
		PRINT *,'  FileType ::= 1 | # Meteoflux 3.x'
		PRINT *,'               2 | # Labview-based daq, 32 bit data ("Andrea''s long")'
		PRINT *,'               3   # Labview-based daq, 16 bit data ("Andrea''s short")'
		PRINT *
		PRINT *,'Copyright 2012 by Servizi Territorio srl'
		PRINT *,'                  All rights reserved'
		PRINT *
		STOP
	END IF
	CALL GET_COMMAND_ARGUMENT(1, sInputFile)
	CALL GET_COMMAND_ARGUMENT(2, sBuffer)
	READ(sBuffer, *, IOSTAT=iRetCode) iFileType
	IF(iRetCode /= 0 .OR. iFileType < 1 .OR. iFileType > 3) THEN
		PRINT *,'decode:: error: Invalid file type'
		STOP
	END IF
	CALL GET_COMMAND_ARGUMENT(3, sOutputFile)
	
	! Process file
	iRetCode = ReadUltrasonicData( &
		10, sInputFile, sOutputFile, iFileType, &
		rvTime, &
		ivU, ivV, ivW, ivT, ivQ, ivC, ivTemp, ivUrel, &
		iNumData &
	)
	IF(iRetCode /= 0) THEN
		PRINT *,'decode:: error: Data not read'
		STOP
	END IF
	PRINT *,'Data read: ',iNumData

END PROGRAM decode
