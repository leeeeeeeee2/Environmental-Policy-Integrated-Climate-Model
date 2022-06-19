      SUBROUTINE PSTAPP
!     EPIC1102
!     THIS SUBPROGRAM APPLIES PESTICIDES TO CROP CANOPY & SOIL
      USE PARM
      KP=LPC(IRO,KT)
      XX=PSTR(IRO,KT)*HE(JT1)
      SMMP(1,KP,MO)=SMMP(1,KP,MO)+XX
      TPAC(JJK,KP)=TPAC(JJK,KP)+XX
      VARP(1,KP)=XX
      X1=PCST(KP)*PSTR(IRO,KT)
      COST=COST+X1
      SMM(96,MO)=SMM(96,MO)+PCEM(KP)
      IF(KFL(20)>0)WRITE(KW(20),1)IYR,MO,KDA,PSTN(KP),KDC(JJK),KDP&
      (KP),IHC(JT1),NBE(JT1),NBT(JT1),X1,X1,PSTR(IRO,KT)
	  PSTS=PSTS-PSTE(IRO,KT)*PRMT(37)
      IF(NOP>0)WRITE(KW(1),6)IYR,MO,KDA,PSTN(KP),PSTR(IRO,KT),HE(JT1)&
      ,PSTE(IRO,KT),PSTS,X1
      IF(TLD(JT1)<1.E-10)THEN
          X1=XX*FGC
          PFOL(KP)=PFOL(KP)+X1
          PSTZ(KP,LD1)=PSTZ(KP,LD1)+XX-X1
          RETURN
      ELSE
          DO K=1,NBSL
              ISL=LID(K)
              IF(TLD(JT1)<=Z(ISL))EXIT
          END DO
          PSTZ(KP,ISL)=PSTZ(KP,ISL)+XX
      END IF
      RETURN
    1 FORMAT(1X,3I4,2X,A16,I6,2X,4I4,F10.2,10X,3F10.2)
    6 FORMAT(1X,3I4,2X,A8,2X,'APPL RATE = ',F5.1,'kg/ha',2X,'APPL EFF &
      = ',F6.2,2X,'KILL EFF = ',F6.2,2X,'PST IDX = ',E12.4,2X,'COST=',&
      F7.0,'$/ha')
      END