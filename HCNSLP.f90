      SUBROUTINE HCNSLP(CNII,X1)
!     EPIC1102
!     THIS SUBPROGRAM ADJUSTS THE 2 CONDITION SCS RUNOFF CURVE
!     NUMBER FOR WATERSHED SLOPE AND COMPUTES CN1 AND CN3.
      USE PARM
      C2=100.-CNII
      CN3=CNII*EXP(.006729*C2)
      X1=(CN3-CNII)*UPSQ+CNII
      C2=100.-X1
      CN1=MAX(.4*X1,X1-20.*C2/(C2+EXP(2.533-.0636*C2)))
      SMX=254.*(100./CN1-1.)
      CN3=X1*EXP(.006729*C2)
      S3=254.*(100./CN3-1.)
      S2=254.*(100./X1-1.)
      SUM=0.
      TOT=0.
      DO J=1,NBSL
          ISL=LID(J)
          IF(Z(ISL)>1.)GO TO 2
          SUM=SUM+FC(ISL)-S15(ISL)
          TOT=TOT+PO(ISL)-S15(ISL)
      END DO
      GO TO 3
    2 L1=LID(J-1)
      RTO=(1.-Z(L1))/(Z(ISL)-Z(L1))
      SUM=SUM+RTO*(FC(ISL)-S15(ISL))
      TOT=TOT+RTO*(PO(ISL)-S15(ISL))
    3 N1=100.+SCRP(30,2)*(TOT/SUM-1.)+.5
      SCRP(4,1)=1.-S2/SMX+SCRP(30,1)
      SCRP(4,2)=1.-S3/SMX+N1
      Z1=ASPLT(SCRP(4,1))
      Z2=ASPLT(SCRP(4,2))
      CALL ASCRV(SCRP(4,1),SCRP(4,2),Z1,Z2) 
      RETURN
      END