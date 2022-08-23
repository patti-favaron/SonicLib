! decode - Transform data from old formats to Meteoflux Core.
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
		PRINT *,'Copyright 2012 by Patrizia Favaron'
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
