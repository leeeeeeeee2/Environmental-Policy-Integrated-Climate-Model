      SUBROUTINE WNSPD
!     EPIC1102
!     THIS SUBPROGRAM SIMULATES MEAN DAILY WIND SPEED @ 10 M HEIGHT
      USE PARM
      V6=AUNIF(IDG(5))
      U10=UAVM(MO)*(-LOG(V6))**UXP
      RETURN
      END