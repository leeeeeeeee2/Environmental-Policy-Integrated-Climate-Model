      SUBROUTINE AINTRI(X,Y,N1,N2)
!     EPIC1102
!     THIS SUBPROGRAM INTERPOLATES SOIL PROPERTIES FROM LAYERS WITH UN
!     EQUAL THICKNESS TO LAYERS OF EQUAL THICKNESS USED IN DIFFERENTIAL
!     EQUATIONS OF GAS DIFFUSION.
      USE PARM
      DIMENSION X(15),Y(N2)
      ZZ=0.
      Z1=0.
      TOT=0.
      Y=0.
      J=1
      DO K=1,N1
          L=LID(K)
          DO 
              IF(ZC(J)>Z(L))EXIT
              Y(J)=TOT+X(L)*(ZC(J)-ZZ)/(Z(L)-Z1)
              ZZ=ZC(J)
              J=J+1
              IF(J>N2)RETURN
              TOT=0.
          END DO
          TOT=TOT+X(L)*(Z(L)-ZZ)/(Z(L)-Z1)
          IF(J>N2)EXIT
          Z1=Z(L) 
          ZZ=Z(L)
      END DO
      I=MIN(J,N2)
      Y(I)=MAX(Y(I),TOT)
      AD1=0.
      DO I=1,N1
          AD1=AD1+X(I)
      END DO
      AD2=0.
      DO I=1,N2
          AD2=AD2+Y(I)
      END DO
      IF(ABS((AD1-AD2)/(AD1+1.E-20))>.01)THEN
          WRITE(KW(1),'(A,3I4,A,2E13.5)')'!!!!!',IYR,MO,KDA,'AINTRI ERR',AD1,AD2
      END IF    
      RETURN
      END