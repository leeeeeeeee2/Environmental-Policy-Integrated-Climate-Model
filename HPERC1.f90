      SUBROUTINE HPERC1
!     EPIC1102
!     THIS SUBPROGRAM COMPUTES PERCOLATION AND LATERAL SUBSURFACE FLOW
!     FROM A SOIL LAYER BY PASSING 4 mm SLUGS THROUGH AS A FUNCTION OF 
!     HYDRAULIC CONDUCTIVITY.
      USE PARM
      SEP=0.
      SST=0.
      ICW=0
      AVW=ST(ISL)-FC(ISL)
      IF(AVW>0.)THEN
          POFC=PO(ISL)-FC(ISL)
          X1=24./POFC
          DO WHILE(AVW>.01) 
              X5=MIN(AVW/POFC,1.)
              X4=MAX(1.E-5,X5**PRMT(93))
              H=SATC(ISL)*X4
              X2=X1*HCL(ISL)*X4
              ZZ=X1*H
              XZ=X2+ZZ
              XX=MIN(4.,AVW)
              IF(XZ>20.)THEN
                  X3=XX
              ELSE
                  X3=XX*(1.-EXP(-XZ))
              END IF
              X6=X3/(1.+X2/ZZ)
              SEP=SEP+X6
              SST=SST+X3-X6
              AVW=AVW-4.
              IF(ISL/=IDR)SST=SST*RFTT
              ICW=ICW+1
              IF(ICW>50)EXIT
          END DO 
      END IF    
      RETURN
      END
