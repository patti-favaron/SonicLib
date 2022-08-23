! ToUsaSim - Program converting from "RAW" CSV Ameriflux form to
!            binary, for use by Arduino's UsaSim program, used to
! simulate an USA-1 or uSonic-3 stream.
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

PROGRAM ToUsaSim

	IMPLICIT NONE

	! Locals
	CHARACTER(LEN=256)	:: sSourceFile
	CHARACTER(LEN=256)	:: sDestinationFile
	INTEGER				:: iRetCode
	REAL				:: u, v, w, t, h2o, co2
	INTEGER(2)			:: iU, iV, iW, iT, iH2O, iCO2
	
	! Get parameters
	IF(COMMAND_ARGUMENT_COUNT() /= 2) THEN
		PRINT *,"ToUsaSim - Program generating binary file for UsaSim"
		PRINT *
		PRINT *,"Usage:"
		PRINT *
		PRINT *,"  ./ToUsaSim <InputFile> <SourceFile>"
		PRINT *
		PRINT *,"Copyright 2015 by Servizi Territorio srl"
		PRINT *,"                  All rights reserved"
		PRINT *
		PRINT *,"Written by: Patrizia Favaron"
		PRINT *,""
		STOP 1
	END IF
	CALL GET_COMMAND_ARGUMENT(1, sSourceFile)
	CALL GET_COMMAND_ARGUMENT(2, sDestinationFile)
	
	! Connect files, and perform conversion one line at time
	OPEN(10, FILE=sSourceFile, STATUS='OLD', ACTION='READ', IOSTAT=iRetCode)
	IF(iRetCode /= 0) THEN
		PRINT *,"ToUsaSim:: error: Input file not opened"
		STOP 2
	END IF
	OPEN(11, FILE=sDestinationFile, STATUS='UNKNOWN', ACTION='WRITE', ACCESS='STREAM')
	DO
		READ(10, *, IOSTAT=iRetCode) w, u, v, t, h2o, co2
		IF(iRetCode /= 0) EXIT
		iU   = INT(u   *  100.)
		iV   = INT(v   *  100.)
		iW   = INT(w   *  100.)
		iT   = INT(t   *  100.)
		iH2O = INT(h2o * 1000.)
		iCO2 = INT(co2 * 1000.)
		WRITE(11) iU, iV, iW, iT, iH2O, iCO2
	END DO
	CLOSE(11)
	CLOSE(10)

END PROGRAM ToUsaSim
