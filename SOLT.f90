      SUBROUTINE SOLT
!     EPIC1102
!     THIS SUBPROGRAM ESTIMATES DAILY AVEAGE TEMPERATURE AT THE CENTER
!     OF EACH SOIL LAYER.
      USE PARM
      DATA ITMP/1/
      DATA XLAG/.8/
      XLG1=1.-XLAG
      F=ABD/(ABD+686.*EXP(-5.63*ABD))
      DP=1.+2.5*F
      WW=.356-.144*ABD
      B=LOG(.5/DP)
      WC=.001*SW/(WW*Z(LID(NBSL)))
      F=EXP(B*((1.-WC)/(1.+WC))**2)
      DD=F*DP
      IF(ITMP==0)THEN
          X4=.5*AMPX
          XZ=.5*(TMX-TMN)*ST0/PRMT(94)
          X2=TX
          X3=(1.-BCV)*X2+BCV*STMP(LID(2))
          TG=.5*(X2+X3)
          DST0=AVT+X4*COS((IDA-200)/PIT)
          X1=TX-DST0
          DO J=1,NBSL
              ISL=LID(J)
              X7=PRMT(95)*Z(ISL)/DD
              X5=EXP(-X7)
              STMP(ISL)=(AVT+X4*COS((IDA-200)/PIT-X7)*X5)+X1*X5
          END DO
      ELSE        
          XZ=.5*(TMX-TMN)*ST0/PRMT(94)
          X2=TX+XZ
          X3=(1.-BCV)*X2+BCV*STMP(LID(2))
          DST0=.5*(X2+X3)
          ZZ=2.*DD
          XX=0.
          X1=AVT-DST0
          DO J=1,NBSL
              ISL=LID(J)
              ZD=(XX+Z(ISL))/ZZ
              F=ZD/(ZD+EXP(-.8669-2.0775*ZD))
              STMP(ISL)=XLAG*STMP(ISL)+XLG1*(F*X1+DST0)
              SEV(ISL)=0.
              XX=Z(ISL)
          END DO
      END IF
      RETURN
      END