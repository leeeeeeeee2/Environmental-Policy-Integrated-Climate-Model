      SUBROUTINE SWN1530
!     THIS SUBPROGRAM COMPUTES THE SOIL WATER CONTENT(m/m) AND NO3 
!     CONCENTRATION(G/M^3) OF A SOIL AT 0.15 AND 0.3 M DEPTHS. 
      USE PARM
      SW15=0.
	  SW30=0.
	  SNN15=0.
	  SNN30=0.
	  SNA15=0.
	  SNA30=0.
	  WT15=0.
	  WT30=0.
	  DO I=1,NBSL
	      L=LID(I)
	      IF(Z(L)>.15)GO TO 2
	      SW15=SW15+ST(L)
	      SNN15=SNN15+WNO3(L)
	      SNA15=SNA15+WNH3(L)
	      WT15=WT15+WT(L)
	      L1=L
	  END DO
      GO TO 5
    2 RTO=(.15-Z(L1))/(Z(L)-Z(L1))
      X1=RTO*ST(L)
      SW15=SW15+X1
	  Y1=RTO*WNO3(L)
      SNN15=SNN15+Y1
	  Y2=RTO*WNH3(L)
	  SNA15=SNA15+Y2
	  W1=RTO*WT(L)
	  WT15=WT15+W1
	  DO J=1,NBSL
	      L=LID(J)
	      IF(Z(L)>.3)GO TO 4
	      SW30=SW30+ST(L)
	      SNN30=SNN30+WNO3(L)
	      SNA30=SNA30+WNH3(L)
	      WT30=WT30+WT(L)
	      L1=L
	  END DO
      GO TO 6
    4 RTO=(.3-Z(L1))/(Z(L)-Z(L1))
      SW30=SW30+RTO*ST(L)
      SNN30=SNN30+RTO*WNO3(L)
	  SNA30=SNA30+RTO*WNH3(L)
      WT30=WT30+RTO*WT(L)
    6 SW30=SW30/300.
      SNN30=1000.*SNN30/WT30
	  SNA30=1000.*SNA30/WT30
    5 SW15=SW15/150.
	  SNN15=1000.*SNN15/WT15
	  SNA15=1000.*SNA15/WT15
      RETURN
	  END 