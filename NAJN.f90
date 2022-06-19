      SUBROUTINE NAJN(UU,XNO3,XNH3,DMD,SUPL,AJF)
!     EPIC1102
!     THIS SUBPROGRAM COMPUTES ACTUAL N PLANT UPTAKE FROM EACH
!     LAYER (UPTAKE = MINIMUM OF PLANT DEMAND AND SOIL SUPPLY).
      USE PARM
      DIMENSION UU(15),XNO3(15),XNH3(15)
      X2=DMD/(SUPL+1.E-20)
      AD1=0.
      IF(X2<1..AND.DMD>0.)THEN
          DO J=1,LRD
              K=LID(J)
              UU(K)=UU(K)*X2
              AD1=AD1+UU(K)
          END DO
          SUPL=AD1
      ELSE
          AD1=0.
          X2=AJF*(DMD-SUPL)
          X21=X2
          DO J=1,LRD
              K=LID(J)
              XX=UU(K)+X2
              X3=.001*PRMT(27)*WT(K)
              X1=PRMT(91)*(XNO3(K)-X3)+P91*XNH3(K)
              IF(XX<X1)EXIT
              IF(X1>0.)THEN
                  X2=X2-X1+UU(K)
                  UU(K)=X1
                  AD1=AD1+UU(K)
              ELSE
                  UU(K)=0.
              END IF
          END DO
          IF(J>LRD)THEN
              SUPL=AD1
          ELSE
              UU(K)=XX
              SUPL=SUPL+X21
          END IF
      END IF
      AD1=0.
      AD2=0.
      DO J=1,LRD
          K=LID(J)
          X3=.001*PRMT(27)*WT(K)
          XX=P91*XNH3(K)
          RTO=XX/(PRMT(91)*(XNO3(K)-X3)+XX)
          X1=RTO*UU(K)
          AD1=AD1+X1
          X2=UU(K)-X1
          AD2=AD2+X2
          XNO3(K)=XNO3(K)-X2
          XNH3(K)=XNH3(K)-X1
      END DO
      SMM(108,MO)=SMM(108,MO)+AD2
      VAR(108)=AD2
      SMM(109,MO)=SMM(109,MO)+AD1
      VAR(109)=AD1    
      RETURN
    1 FORMAT(5X,'!!!!!',3I4,10E16.6)  
      END