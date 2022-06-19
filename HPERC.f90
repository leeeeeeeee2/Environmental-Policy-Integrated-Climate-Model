      SUBROUTINE HPERC
!     EPIC1102
!     THIS SUBPROGRAM COMPUTES PERCOLATION AND LATERAL SUBSURFACE FLOW
!     FROM A SOIL LAYER WHEN FIELD CAPACITY IS EXCEEDED.
      USE PARM
      SEP=0.
      SST=0.
      AVW=ST(ISL)-FC(ISL)
      IF(AVW<1.E-5)RETURN
      X1=24./(PO(ISL)-FC(ISL))
      X2=HCL(ISL)*X1
      ZZ=X1*SATC(ISL)
      XZ=X2+ZZ
      IF(XZ>20.)THEN
          X3=AVW
      ELSE
          X3=AVW*(1.-EXP(-XZ))
      END IF
      SEP=X3/(1.+X2/ZZ)
      SST=X3-SEP
      IF(ISL/=IDR)SST=SST*RFTT
      RETURN
      END