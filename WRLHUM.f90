      SUBROUTINE WRLHUM
!     EPIC1102
      USE PARM
      Q1=RHM-1.
      UPLM=RHM-Q1*EXP(Q1)
      BLM=RHM*(1.-EXP(-RHM))
      RHD=ATRI(BLM,RHM,UPLM,7)
      RETURN
      END