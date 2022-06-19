      SUBROUTINE HLGB(Q,EV,O,RG,VLGE,VLGB,WW,KW,MSO)
!     EPIC1102
!     THIS SUBPROGRAM CHECKS THE LAGOON WATER BALANCE AT THE END OF A
!     SIMULATION.
      DIMENSION KW(MSO+3)
      WRITE(KW(1),'(T10,A)')'LAGOON WATER BALANCE'
      DF=VLGB+Q-EV-O-VLGE-RG+WW
      PER=200.*DF/(VLGB+VLGE)
      WRITE(KW(1),3)DF,VLGB,Q,EV,O,VLGE,RG,WW
      VLGB=VLGE
      RETURN
    3 FORMAT(8E16.6)
      END