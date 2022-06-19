      SUBROUTINE CRGBD(RGS)
!     EPIC1102
!     THIS SUBPROGRAM CALCULATES ROOT GROWTH STRESSES CAUSED BY
!     TEMPERATURE,  ALUMINUM TOXICITY, AND SOIL STRENGTH AND DETERMINES
!     THE ACTIVE CONSTRAINT ON ROOT GROWTH (THE MINIMUM STRESS FACTOR).
      USE PARM
      RGS=1.
      IF(PRMT(2)>1.99)RETURN
      II=3
      XX=STMP(ISL)/TOPC(JJK)
      IF(XX<=0.)THEN
          RGS=0.
          GO TO 4
      END IF
      IF(XX<1.)RGS=SIN(1.5708*XX)
      A0=10.+(ALT(JJK)-1.)*20.
      IF(ALS(ISL)>A0)THEN
          F=(100.-ALS(ISL))/(100.-A0)
          CALL CFRG(2,II,F,RGS,.1,JRT)
          IF(JRT>0)GO TO 4
      END IF
      CALL SBDSC(BDP(ISL),PRMT(2),F,ISL,3)
      XX=ROK(ISL)
      F=F*(1.-XX/(XX+EXP(SCRP(1,1)-SCRP(1,2)*XX)))
      CALL CFRG(1,II,F,RGS,.1,JRT)
    4 STDA(II,JJK)=STDA(II,JJK)+(1.-RGS)/NBSL
      IF(ISL==LID(LRD))RGSM=RGS
      RETURN
      END