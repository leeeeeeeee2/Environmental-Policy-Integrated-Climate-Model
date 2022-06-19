      SUBROUTINE SWTN
!     EPIC1102
      USE PARM
      DO J=1,NBSL
          ISL=LID(J)
          IF(Z(ISL)>=.15)GO TO 1
      END DO
      ISL=LID(NBSL)
    1 XX=LOG10(S15(ISL))
      X1=3.1761-1.6576*(LOG10(ST(ISL))-XX)/(LOG10(FC(ISL))-XX)
      IF(X1<4.)THEN
          WTN=MAX(5.,10.**X1)
      ELSE
          WTN=10000.
      END IF    
      RETURN
      END