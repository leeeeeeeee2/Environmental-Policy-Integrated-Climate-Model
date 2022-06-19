      SUBROUTINE PSTEV
!     EPIC1102
!     THIS SUBPROGRAM ESTIMATES UPWARD NO3 MOVEMENT CAUSED BY SOIL EVAPO
      USE PARM
      IF(NEV==1)RETURN
      SUM=0.
      DO J=NEV,2,-1
          ISL=LID(J)
          IF(WNO3(ISL)<.001)CYCLE
          SUM=SUM+SEV(ISL)
          IF(SUM<=0.)CYCLE
          XX=SUM*WNO3(ISL)/(ST(ISL)+SUM)
          SSFNO3=SSFNO3+XX
          WNO3(ISL)=WNO3(ISL)-XX
          SUM=0.
      END DO
      WNO3(LD1)=WNO3(LD1)+SSFNO3
      RETURN
      END