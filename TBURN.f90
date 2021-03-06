      SUBROUTINE TBURN
!     EPIC1102
!     THIS SUBPROGRAM BURNS ALL STANDING AND FLAT CROP RESIDUE.
      USE PARM
      X2=1.-PRMT(49)
      ADD=0.
      SUM=0.
      DO J=1,LC
          RTO=MIN(.99,STL(J)/(DM(J)+1.E-10))
          X1=PRMT(49)*STL(J)
          X4=420.*X1
          VAR(99)=VAR(99)-X4
          SMM(99,MO)=SMM(99,MO)-X4
          DM(J)=DM(J)-X1
          STL(J)=STL(J)-X1
          X3=PRMT(49)*UN1(J)*RTO
          UN1(J)=UN1(J)-X3
          X5=STD(J)*X2
          STD(J)=STD(J)-X5
          ADD=ADD+420.*X5
          X1=PRMT(49)*STDN(J)
          STDN(J)=STDN(J)-X1
          SUM=SUM+X1+X3
      END DO
      WLS(LD1)=WLS(LD1)*X2
      WLM(LD1)=WLM(LD1)*X2
      X1=PRMT(49)*WLSN(LD1)
      WLSN(LD1)=WLSN(LD1)-X1
      X3=PRMT(49)*WLMN(LD1)
      WLMN(LD1)=WLMN(LD1)-X3
      SUM=SUM+X1+X3
      WLSL(LD1)=WLSL(LD1)*X2
      X1=PRMT(49)*WLSC(LD1)
      WLSC(LD1)=WLSC(LD1)-X1
      X3=PRMT(49)*WLMC(LD1)
      WLMC(LD1)=WLMC(LD1)-X3
      WLSLC(LD1)=WLSLC(LD1)*X2
      WLSLNC(LD1)=WLSC(LD1)-WLSLC(LD1)
      ADD=ADD+X1+X3
      SMM(97,MO)=SMM(97,MO)+ADD
      VAR(97)=ADD
      SMM(98,MO)=SMM(98,MO)+SUM
      VAR(98)=SUM
      RSD(LD1)=.001*(WLS(LD1)+WLM(LD1))
      STDO=0.
      STDON=0.
      X1=STDP*PRMT(49)
      FOP(LD1)=FOP(LD1)+X1+STDOP
      STDP=STDP-X1
      STDOP=0.
      RETURN
      END