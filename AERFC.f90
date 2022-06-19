      FUNCTION AERFC(XX)
!     EPIC1102
!     THIS SUBPROGRAM COMPUTES POINTS ON ERROR FUNCTION OF NORMAL
!     DISTRIBUTION.
      DATA C1/.19684/,C2/.115194/,C3/.00034/,C4/.019527/
      X=ABS(1.4142*XX)
      ERF=1.-(1.+C1*X+C2*X*X+C3*X**3+C4*X**4)**(-4)
      IF(XX<0.)ERF=-ERF
      AERFC=1.-ERF
      RETURN
      END