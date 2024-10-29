MODULE Format
    IMPLICIT NONE
    INTEGER :: max_width=79
    INTEGER :: N_CHANNEL=11
CONTAINS

! subroutines:
! selectChannel findChannel
! printEmptyLine printLine printBanner
! printBeginBanner printBannerLine printEndBanner
! printHeaderLine printHeader printHeaderCentred printStringColumns 

SUBROUTINE selectChannel(n)
    INTEGER, INTENT(IN) :: n
    write(*,*)'now changing to channel ',n
    N_CHANNEL=n
END SUBROUTINE selectChannel

SUBROUTINE findChannel(n)
    INTEGER, INTENT(OUT) :: n
    n=N_CHANNEL
END SUBROUTINE findChannel

SUBROUTINE printEmptyLine()
    WRITE(N_CHANNEL,*)
END SUBROUTINE printEmptyLine

SUBROUTINE printLine(str1)
    CHARACTER(LEN=*), INTENT(IN) :: str1
    WRITE(N_CHANNEL,*)TRIM(str1)
END SUBROUTINE printLine

SUBROUTINE printBanner(str1,str2)
    CHARACTER(LEN=*), INTENT(IN) :: str1
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: str2
    IF (PRESENT(str2).AND.TRIM(str2)=='continue') THEN
    ELSE
      CALL printEmptyLine
    ENDIF
    CALL printBeginBanner
    CALL printHeaderCentred(str1)
    CALL printEndBanner
    IF (PRESENT(str2).AND.(TRIM(str2)=='start'.OR.TRIM(str2)=='continue')) THEN
    ELSE
      CALL printEmptyLine
    ENDIF
END SUBROUTINE printBanner

SUBROUTINE printBeginBanner
    CALL printHeaderLine
    CALL printBannerLine('****')
    CALL printBannerLine('**')
END SUBROUTINE printBeginBanner

SUBROUTINE printBannerLine(str)
    CHARACTER(LEN=*), INTENT(IN) :: str
    CHARACTER(LEN=max_width) :: out
    INTEGER :: str_length=0, offset=0, str_offset=1, str_until=1
    str_length = LEN_TRIM(str)
    out = ''
    IF (str_length>0) THEN
      out(1:str_length) = str
      out(max_width+1-str_length:max_width) = str
    ENDIF
    WRITE(N_CHANNEL,*) out
END SUBROUTINE printBannerLine

SUBROUTINE printEndBanner
    CALL printBannerLine('**')
    CALL printBannerLine('****')
    CALL printHeaderLine
END SUBROUTINE printEndBanner

SUBROUTINE printHeaderLine()
    CHARACTER(LEN=max_width) :: out
    out = REPEAT('*',max_width)
    WRITE(N_CHANNEL,*) out
END SUBROUTINE printHeaderLine

SUBROUTINE printHeader(str)
    CHARACTER(LEN=*), INTENT(IN) :: str
    CHARACTER(LEN=max_width) :: out
    out = ''
    out(3:max_width-2) = str
    out(1:1) = '*'
    out(max_width:max_width) = '*'
    WRITE(N_CHANNEL,*) out
END SUBROUTINE printHeader

SUBROUTINE printHeaderCentred(str)
    CHARACTER(LEN=*), INTENT(IN) :: str
    CHARACTER(LEN=max_width) :: out
    INTEGER :: str_length=0, offset=0, str_offset=1, str_until=1
    str_length = LEN_TRIM(str)
    str_offset = 1
    str_until = 1

    DO
        out = ''
        IF(LEN_TRIM(str(str_offset:str_length))+4 <= max_width) THEN
            str_until = str_length
        ELSE
            str_until = INDEX(str(str_offset:max_width-4), ' ', .TRUE.)
            IF(str_until == 0) str_until = str_offset + max_width - 5
        END IF
        offset = (max_width - LEN_TRIM(str(str_offset:str_until))) / 2 + 1
        out(offset:max_width) = str(str_offset:str_until)
        out(1:1) = '*'
        out(max_width:max_width) = '*'
        WRITE(N_CHANNEL,*) out
        str_offset = str_until + 1
        IF(str_offset > str_length) THEN
            EXIT
        END IF
    END DO
END SUBROUTINE

SUBROUTINE printStringColumns(str1,str2)
    CHARACTER(LEN=*), INTENT(IN) :: str1,str2
    CHARACTER(LEN=35) :: left_str
    CHARACTER(LEN=35) :: right_str
    INTEGER :: str_length
    left_str(1:35)=' '
    right_str(1:35)=' '
    str_length = LEN_TRIM(str1)
    IF (str_length > 35) str_length=35
    IF (str_length > 0) left_str(36-str_length:35)=TRIM(str1)
    str_length = LEN_TRIM(str2)
    IF (str_length > 35) str_length=35
    IF (str_length > 0) right_str(1:str_length)=TRIM(str2)
    WRITE(N_CHANNEL,*)'*  '//left_str(1:35)//'   '//right_str(1:35)//'  *'
END SUBROUTINE

  !==========================================================================================!
  ! A routine to convert a string to a lower case formatting
  !==========================================================================================!

  SUBROUTINE toLowerCase(str, strLowerCase)
    CHARACTER(*), INTENT(in) :: str
    CHARACTER(*), INTENT(out) :: strLowerCase
    INTEGER :: i

    strLowerCase = str

    DO i = 1, len(str)
      SELECT CASE(str(i:i))
        CASE("A":"Z")
          strLowerCase(i:i) = achar(iachar(str(i:i))+32)
      END SELECT
    END DO

    RETURN

  END SUBROUTINE toLowerCase

  !==========================================================================================!
  ! A routine to convert an integer number to a string
  !==========================================================================================!

  CHARACTER(LEN=20) FUNCTION intToStr(number)
    INTEGER, INTENT(IN)     :: number

    WRITE (intToStr, *) number

  END FUNCTION intToStr

  !==========================================================================================!
  ! A routine to convert an integer number to a string
  !==========================================================================================!

  INTEGER FUNCTION strToInt(strExpr)
    CHARACTER(LEN=*), INTENT(IN)     :: strExpr

    READ (strExpr, *) strToInt

  END FUNCTION strToInt

END MODULE Format
