      SUBROUTINE HFURD
!     EPIC1102
!     THIS SUBPROGRAM COMPUTES THE STORAGE VOLUME OF FURROW DIKES GIVEN
!     DIKE INTERVAL AND HEIGHT, RIDGE HEIGHT, AND SLOPE.
      USE PARM
      DH=DHT*.001
      H2=2.*DH
      X1=.001*DKHL
      TW=RGIN-X1
      BW=MAX(TW-4.*X1,.1*TW)
      DI=DKIN-X1
      D2=DH*(1.-2.*UPS)
      D3=DH-UPS*(DI-H2)
      X1=(TW-BW)/DH
      TW2=BW+D2*X1
      TW3=BW+D3*X1
      A2=.5*D2*(TW2+BW)
      A3=.5*D3*(TW3+BW)
      XX=DH/UPS
      ZZ=DI-H2
      X1=FDSF/(RGIN*DKIN)
      IF(XX>ZZ)THEN
          DV=X1*(A2*DH+.5*(A2+A3)*(DI-4.*DH)+A3*D3)
      ELSE
          DV=X1*A2*(DH+.5*(XX-H2))
      END IF
      RETURN
      END