      SUBROUTINE WSOLRA
!     EPIC1102
      USE PARM
      RX=RAMX-RM
      SRAD=RM+WX(3)*RX/4.
      IF(SRAD<=0.)SRAD=.05*RAMX
      RETURN
      END