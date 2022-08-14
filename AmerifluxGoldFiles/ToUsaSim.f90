! ToUsaSim - Program converting from "RAW" CSV Ameriflux form to
!            binary, for use by Arduino's UsaSim program, used to
! simulate an USA-1 or uSonic-3 stream.
!
! Copyright 2015 by Servizi Territorio srl
!                   All rights reserved.
!
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
		PRINT *,"Written by: Mauri Favaron"
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
