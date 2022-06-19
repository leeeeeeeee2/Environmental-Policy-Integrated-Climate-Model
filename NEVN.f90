      SUBROUTINE NEVN
!     EPIC1102
!     THIS SUBPROGRAM ESTIMATES UPWARD NO3 MOVEMENT CAUSED BY SOIL EVAPO
      USE PARM
      IF(NEV==1)RETURN
      TOT=0.
      DO J=NEV,2,-1
          ISL=LID(J)
          X1=WNO3(ISL)-.001*WT(ISL)*PRMT(27)
          IF(X1<=.01)CYCLE
	      X2=1.-EXP(-PRMT(62)*SEV(ISL)/(STFR(ISL)*PO(ISL)))
	      XX=X1*X2
          TOT=TOT+XX
          WNO3(ISL)=WNO3(ISL)-XX
      END DO
      WNO3(LD1)=WNO3(LD1)+TOT
      RETURN
      END