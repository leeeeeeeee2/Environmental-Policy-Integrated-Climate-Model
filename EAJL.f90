      FUNCTION EAJL(X,Y)
!     EPIC1102
!     THIS SUBPROGRAM IS CALLED BY ESLOS TO CALCULATE THE AMOUNT OF
!     MATERIAL ADDED TO THE TOP LAYER AND REMOVED FROM THE SECOND LAYER.
      EAJL=X*Y
      X=X-EAJL
      RETURN
      END