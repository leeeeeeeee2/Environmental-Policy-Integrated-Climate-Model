      SUBROUTINE SPOFC(I)
!     EPIC1102
      USE PARM
      X1=.95*PO(I)
      IF(FC(I)>X1)THEN
          X2=FC(I)-S15(I)
          FC(I)=X1
          S15(I)=FC(I)-X2
          IF(S15(I)<=0.)S15(I)=.01*FC(I)
      END IF    
      RETURN
      END