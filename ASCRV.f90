      SUBROUTINE ASCRV(X1,X2,X3,X4)
!     EPIC1102
!     THIS SUBPROGRAM COMPUTES S CURVE PARMS GIVEN 2 (X,Y) POINTS.
      USE PARM
      XX=LOG(X3/X1-X3)
      X2=(XX-LOG(X4/X2-X4))/(X4-X3)
      X1=XX+X3*X2
      RETURN
      END