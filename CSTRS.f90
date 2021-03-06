      SUBROUTINE CSTRS
!     EPIC1102
!     THIS SUBPROGRAM ESTIMATES PLANT STRESS FACTORS CAUSED BY LIMITED
!     N, P, AIR, AND WATER AND DETERMINES THE ACTIVE CONSTRAINT
!     (MINIMUM STRESS FACTOR--N, P, WATER, OR TEMPERATURE).  CALLS
!     NFIX AND NFERT (AUTOMATIC FERTILIZER OPTION).
      USE PARM
      DIMENSION JFS(7)
      DATA JFS/13,14,15,16,17,18,19/
      J3=5
      VARC(17,JJK)=REG(JJK)
      IF(EP(JJK)>0.)THEN
          IF(RZSW>0.)THEN
              WS=100.*RZSW/PAW
              WS=WS/(WS+EXP(SCRP(11,1)-SCRP(11,2)*WS))
          ELSE
              WS=0.
          END IF
          WS=(1.-PRMT(35))*WS+PRMT(35)*SU/(EP(JJK)+1.E-10)
      END IF
      VARC(13,JJK)=WS
      WFX=0.
      IF(IDC(JJK)==NDC(1).OR.IDC(JJK)==NDC(2).OR.IDC(JJK)==NDC(3)&
      .OR.IDC(JJK)==NDC(10))CALL NFIX
      CALL NAJN(UN,WNO3,WNH3,UNO3,SUN,1.)
      X1=SUN/(UNO3+1.E-10)
      UNO3=SUN+WFX
      UN1(JJK)=UN1(JJK)+UNO3
      IF(UPP>SUP)CALL NAJP(UP,AP,UPP,SUP,1.)
      X2=MIN(1.,SUP/(UPP+1.E-10))
      UPP=SUP
      UP1(JJK)=UP1(JJK)+UPP
      !CALL NAJN(UK,SOLK,UPK,SUK,1.,0)
      X3=SUK/(UPK+1.E-10)
      UPK=SUK
      !UK1(JJK)=UK1(JJK)+UPK
      CALL NUTS(UN1(JJK),UN2(JJK),SN)
      SN=MAX(X1,SN)
      VARC(14,JJK)=SN
      CALL NUTS(UP1(JJK),UP2(JJK),SP)
      SP=MAX(X2,SP)
      VARC(15,JJK)=SP
!     CALL NUTS(UK1(JJK),UK2(JJK),SK)
!     SK=MAX(X3,SK)
!     VARC(16,JJK)=SK
      VARC(18,JJK)=SAT
      X1=.15625*TSRZ/SWRZ
      XX=X1-STX(2,JJK)
      IF(XX>0.)THEN
          SSLT=MAX(0.,1.-STX(1,JJK)*XX)
      ELSE
          SSLT=1.
      END IF
      VARC(19,JJK)=SSLT
      DO
          CALL CFRG(6,J3,SAT,REG(JJK),0.,JRT)
          IF(JRT>0)EXIT
          IF(ICG==0)CALL CFRG(1,J3,WS,REG(JJK),0.,JRT)
          IF(JRT>0)EXIT
          CALL CFRG(3,J3,SP,REG(JJK),0.,JRT)
          IF(JRT>0)EXIT
    !     CALL CFRG(4,J3,SK,REG(JJK),0.,JRT)
    !     IF(JRT>0)EXIT
          CALL CFRG(7,J3,SSLT,REG(JJK),0.,JRT)
          IF(JRT>0)EXIT
          ZZ=REG(JJK)
          CALL CFRG(2,J3,SN,ZZ,REG(JJK),JRT)
          IF(JRT==0)EXIT
          REG(JJK)=ZZ
          IF(IDC(JJK)==NDC(1).OR.IDC(JJK)==NDC(2).OR.IDC(JJK)==NDC(3)&
          .OR.IDC(JJK)==NDC(10))EXIT
          IF(BFT>SN.AND.NFA>=IFA)CALL NFERT(3,IAUF)
          EXIT
      END DO    
      XX=1.-REG(JJK)
      N1=NCP(JJK)
      SFMO(J3,JJK)=SFMO(J3,JJK)+XX
      SF(J3,N1,JJK)=SF(J3,N1,JJK)+XX
      CGSF(J3,JJK)=XX
      BLYN(JJK)=BLYN(JJK)+1.
      J1=JFS(J3)
      SMMC(J1,JJK,MO)=SMMC(J1,JJK,MO)+XX
      VARC(J1,JJK)=REG(JJK)
      SMMC(20,JJK,MO)=SMMC(20,JJK,MO)+XX
      VARC(20,JJK)=REG(JJK)
      RETURN
      END