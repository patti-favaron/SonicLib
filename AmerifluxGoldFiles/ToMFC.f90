!------------------------------------------------------------------
!
! ToMFC - Program converting from "RAW" CSV Ameriflux form to
!         binary MeteoFlux Core V2, for eddy covariance program testing.
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

PROGRAM ToMFC

	IMPLICIT NONE

	! Locals
	CHARACTER(LEN=4)	:: sDayNumber
	INTEGER				:: iRetCode
	INTEGER				:: iDayNumber
	INTEGER				:: iHour
	INTEGER				:: iHalfHour
	CHARACTER(LEN=256)	:: sSourceFile
	CHARACTER(LEN=256)	:: sDestinationFile
	REAL				:: u, v, w, t, h2o, co2
	INTEGER(2)			:: iU, iV, iW, iT, iH2O, iCO2
	INTEGER				:: iDataNum
	INTEGER(2)			:: iTimeStamp
	INTEGER(2)			:: iZero = 0
	
	! Get parameters
	IF(COMMAND_ARGUMENT_COUNT() /= 1) THEN
		PRINT *,"ToUsaSim - Program generating binary file for UsaSim"
		PRINT *
		PRINT *,"Usage:"
		PRINT *
		PRINT *,"  ./ToUsaSim <DayNum>"
		PRINT *
		PRINT *,"where <DayNum> ::= 104 | 181"
		PRINT *
		PRINT *,"Copyright 2015 by Università degli Studi di Milano"
		PRINT *,"                  All rights reserved"
		PRINT *
		PRINT *,"Written by: Patrizia Favaron"
		PRINT *,""
		STOP 1
	END IF
	CALL GET_COMMAND_ARGUMENT(1, sDayNumber)
	READ(sDayNumber, *, IOSTAT=iRetCode) iDayNumber
	IF(iRetCode /= 0) THEN
		PRINT *,"ToMFC:: error: Invalid day number"
		STOP 2
	END IF
	IF(iDayNumber /= 104 .AND. iDayNumber /= 181) THEN
		PRINT *,"ToMFC:: error: Day number is not 104 or 181"
		STOP 3
	END IF

	! Main loop: iterate over hours in day
	DO iHour = 0, 23
	
		! Identify output file and create it
		IF(iDayNumber == 104) THEN
			WRITE(sDestinationFile, "('20150414.',i2.2,'R')") iHour
		ELSE
			WRITE(sDestinationFile, "('20150630.',i2.2,'R')") iHour
		END IF
		OPEN(11, FILE=sDestinationFile, STATUS='UNKNOWN', ACTION='WRITE', ACCESS='STREAM')
		
		! Loop over the two halves of an hour
		DO iHalfHour = 0, 30, 30
		
			! Get input file, convert it to MFC V2 form and write in encoded form
			WRITE(sSourceFile, "('G',i3.3,i2.2,i2.2,'.RAW')") iDayNumber, iHour, iHalfHour
			OPEN(10, FILE=sSourceFile, STATUS='OLD', ACTION='READ')
			iDataNum = 0
			DO
				READ(10, *, IOSTAT=iRetCode) w, u, v, t, h2o, co2
				IF(iRetCode /= 0) EXIT
				iTimeStamp = iDataNum / 10
				iU   = INT(u   *  100.)
				iV   = INT(v   *  100.)
				iW   = INT(w   *  100.)
				iT   = INT(t   *  100.)
				iH2O = INT(h2o * 1000.)
				iCO2 = INT(co2 * 1000.)
				WRITE(11) iTimeStamp, iU, iV, iW, iT
				WRITE(11) iTimeStamp + 5000, iH2O, iCO2, iZero, iZero
				iDataNum = iDataNum + 1
			END DO
			CLOSE(10)
			
		END DO
		
		CLOSE(11)
		
	END DO
	
END PROGRAM ToMFC
