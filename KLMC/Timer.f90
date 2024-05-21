MODULE TIMER

USE FORMAT
USE Config, ONLY : stderr, stdout, stdsee, OUTPUT_LEVEL

CONTAINS

  FUNCTION WTIME ( )

    IMPLICIT NONE

    INTEGER ( KIND = 4 ) CLOCK_MAX
    INTEGER ( KIND = 4 ) CLOCK_RATE
    INTEGER ( KIND = 4 ) CLOCK_READING
    REAL    ( KIND = 8 ) WTIME

    CALL SYSTEM_CLOCK ( CLOCK_READING, CLOCK_RATE, CLOCK_MAX )

    WTIME = REAL ( CLOCK_READING, KIND = 8 ) &
          / REAL ( CLOCK_RATE, KIND = 8 )

    RETURN
  END FUNCTION WTIME

  !==========================================================================================!
  ! A function to return a month's name
  !==========================================================================================!
  CHARACTER(LEN=4) FUNCTION MONTHNAME(m)
    INTEGER, INTENT(IN) :: m

    IF (m == 1) THEN
      WRITE(MONTHNAME, *) "Jan"
    ELSEIF (m == 2)  THEN
      WRITE(MONTHNAME, *) "Feb"
    ELSEIF (m == 3)  THEN
      WRITE(MONTHNAME, *) "Mar"
    ELSEIF (m == 4)  THEN
      WRITE(MONTHNAME, *) "Apr"
    ELSEIF (m == 5)  THEN
      WRITE(MONTHNAME, *) "May"
    ELSEIF (m == 6)  THEN
      WRITE(MONTHNAME, *) "Jun"
    ELSEIF (m == 7)  THEN
      WRITE(MONTHNAME, *) "Jul"
    ELSEIF (m == 8)  THEN
      WRITE(MONTHNAME, *) "Aug"
    ELSEIF (m == 8)  THEN
      WRITE(MONTHNAME, *) "Sep"
    ELSEIF (m == 10) THEN
      WRITE(MONTHNAME, *) "Oct"
    ELSEIF (m == 11) THEN
      WRITE(MONTHNAME, *) "Nov"
    ELSEIF (m == 12) THEN
      WRITE(MONTHNAME, *) "Dec"
    ELSE
      WRITE(MONTHNAME, *) "xxx"
    ENDIF

  END FUNCTION MONTHNAME

  !==========================================================================================!
  ! A function to return a day of the week according to a date
  !==========================================================================================!
  CHARACTER(LEN=4) FUNCTION DAYOFWEEK(d, m, y)
    INTEGER, INTENT(IN) :: d, m, y
    INTEGER :: j, k, mm, yy, dayNo

    mm=m
    yy=y
    IF(mm .le. 2) THEN
      mm = mm + 12
      yy = yy - 1
    END IF

    j = yy / 100
    k = MOD(yy, 100)

    dayNo = MOD(d + ((mm+1)*26)/10 + k + k/4 + j/4 + 5*j, 7)

    IF (dayNo == 1)  THEN
      WRITE(DAYOFWEEK, *) "Sun"

    ELSEIF (dayNo == 2) THEN
      WRITE(DAYOFWEEK, *) "Mon"

    ELSEIF (dayNo == 3) THEN
      WRITE(DAYOFWEEK, *) "Tue"

    ELSEIF (dayNo == 4) THEN
      WRITE(DAYOFWEEK, *) "Wed"

    ELSEIF (dayNo == 5) THEN
      WRITE(DAYOFWEEK, *) "Thu"

    ELSEIF (dayNo == 6) THEN
      WRITE(DAYOFWEEK, *) "Fri"

    ELSE
      WRITE(DAYOFWEEK, *) "Sat"
    ENDIF

    RETURN

  END FUNCTION DAYOFWEEK

  !==========================================================================================!
  ! A function to return a time stamp in the CAR format
  !==========================================================================================!
  CHARACTER(LEN=30) FUNCTION CARTIMESTAMP()

    INTEGER*4 TODAY(3), NOW(3)

    CALL IDATE(TODAY)   ! TODAY(1)=day,  (2)=month,  (3)=year
    CALL ITIME(NOW)     ! NOW(1)  =hour, (2)=minute, (3)=second

    WRITE(CARTIMESTAMP, 1000 )  TRIM(DAYOFWEEK(TODAY(1), TODAY(2), TODAY(3))), &
      TRIM(MONTHNAME(TODAY(2))), TODAY(1), NOW, TODAY(3)

    1000 FORMAT (a4, a4, i3.2, i3.2, ':', i2.2, ':', i2.2, i5.4)

    RETURN
  END FUNCTION CARTIMESTAMP

  !==========================================================================================!
  ! A function to return a time stamp
  !==========================================================================================!
  CHARACTER(LEN=24) FUNCTION TIMESTAMPX()

    INTEGER*4 TODAY(3), NOW(3)

    CALL IDATE(TODAY)   ! TODAY(1)=day,  (2)=month,  (3)=year
    CALL ITIME(NOW)     ! NOW(1)  =hour, (2)=minute, (3)=second

    WRITE(TIMESTAMPX, 1001) TODAY(1), TODAY(2), TODAY(3), NOW

    1001 FORMAT (i2.2, '/', i2.2, '/', i4.4, i3.2, ':', i2.2, ':', i2.2)

    RETURN
  END FUNCTION TIMESTAMPX

  !==========================================================================================!
  ! A routine to print a log message with a timestamp to the stderr
  !==========================================================================================!
  SUBROUTINE printErr(routine, message, logLevel, level)
    CHARACTER(LEN=*), INTENT(IN) :: routine, message
    INTEGER, INTENT(IN) :: logLevel
    INTEGER, INTENT(IN), OPTIONAL :: level

    CALL printToStream(stderr, routine, message, logLevel, level)

  END SUBROUTINE printErr

  !==========================================================================================!
  ! A routine to print a log message with a timestamp to the stdsee
  !==========================================================================================!
  SUBROUTINE printLog(routine, message, logLevel, level)
    CHARACTER(LEN=*), INTENT(IN) :: routine, message
    INTEGER, INTENT(IN) :: logLevel
    INTEGER, INTENT(IN), OPTIONAL :: level

    CALL printToStream(stdsee, routine, message, logLevel, level)

  END SUBROUTINE printLog

  !==========================================================================================!
  ! A routine to print a log message with a timestamp to the stdout
  !==========================================================================================!
  SUBROUTINE printOut(routine, message, logLevel, level)
    CHARACTER(LEN=*), INTENT(IN) :: routine, message
    INTEGER, INTENT(IN) :: logLevel
    INTEGER, INTENT(IN), OPTIONAL :: level

    CALL printToStream(stdout, routine, message, logLevel, level)

  END SUBROUTINE printOut

  !==========================================================================================!
  ! A routine to print a log messages to all output streams
  !==========================================================================================!
  SUBROUTINE print2All(routine, message, logLevel, level)
    CHARACTER(LEN=*), INTENT(IN) :: routine, message
    INTEGER, INTENT(IN) :: logLevel
    INTEGER, INTENT(IN), OPTIONAL :: level

    CALL printToStream(1, routine, message, logLevel, level)
    CALL printToStream(stdsee, routine, message, logLevel, level)
    CALL printToStream(stderr, routine, message, logLevel, level)

  END SUBROUTINE print2All

  !==========================================================================================!
  ! A routine to print an log message with a timestamp to a specific output stream
  !==========================================================================================!

  SUBROUTINE printToStream(outStream, routine, message, logLevel, level)
    INTEGER, INTENT(IN) :: outStream
    CHARACTER(LEN=*), INTENT(IN) :: routine, message
    INTEGER, INTENT(IN) :: logLevel
    INTEGER, INTENT(IN), OPTIONAL :: level

    CHARACTER(LEN=30) :: timeStamp

    timeStamp = TIMESTAMPX()

    IF (logLevel .LE. OUTPUT_LEVEL) THEN
      WRITE(outStream, *) TRIM(timeStamp) // " >> " // TRIM(routine) // " >> " // TRIM(message)
    ENDIF

  END SUBROUTINE printToStream

END MODULE TIMER
