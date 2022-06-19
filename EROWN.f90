      FUNCTION EROWN(Y1)
!     EPIC1102
!     THIS SUBPROGRAM COMPUTES POTENTIAL WIND EROSION RATE FOR BARE
!     SOIL GIVEN WIND SPEED
      USE PARM
      DU10=U10*Y1
      IF(DU10>U10MX(MO))U10MX(MO)=DU10
      USTR=.0408*DU10
      X1=USTR*USTR-USTW
      IF(X1<0.)THEN
          EROWN=0.
      ELSE
          YWR=.255*X1**1.5
          EROWN=YWR*ALG
      END IF
      RETURN
      END