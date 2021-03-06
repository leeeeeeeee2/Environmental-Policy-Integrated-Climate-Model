      SUBROUTINE THVRT(YY,X3,X1,X6,X7,N1)
!     EPIC1102
!     THIS SUBPROGRAM HARVESTS ROOT CROPS.
      USE PARM
      JD=JJK
      X1=ORHI(JT1)
      X3=RW(JJK)
      XX=DM(JJK)
      XZ=X1*X3
      CALL PESTF
      X6=PSTF(JJK)
      TPSF(N1,JJK)=TPSF(N1,JJK)+X6
      NPSF(N1,JJK)=NPSF(N1,JJK)+1
      YY=XZ*HE(JT1)*X6
      X12=UN1(JJK)/XX
      X13=UP1(JJK)/XX
      X8=UK1(JJK)/XX
      X9=.9*STL(JJK)
      X10=X12*X9
	  AD1=X10
      CALL NCNSTD(X9,X10,0)
	  U2=X13*X9 
	  AD2=U2
	  FOP(LD1)=FOP(LD1)+U2
      SOLK(LD1)=SOLK(LD1)+UK1(JJK)
      YLN=YY*X12
      YLP=YY*X13
      YLC=.42*YY
      YLK=YY*X8
      XX=X1*(1.-HE(JT1))
	  RW(JJK)=0.
	  DO J=1,LRD
          ISL=LID(J)
          X11=RWT(ISL,JJK)*XX
	      RWT(ISL,JJK)=RWT(ISL,JJK)*(1.-X1)
	      X10=X11*X12
	      AD1=AD1+X10
          CALL NCNSTD(X11,X10,1)
          U2=X11*X13
	      AD2=AD2+U2 
          FOP(ISL)=FOP(ISL)+U2
          RW(JJK)=RW(JJK)+RWT(ISL,JJK)
      END DO
      YLD(JJK)=YY
      YLD1(N1,JJK)=YLD1(N1,JJK)+YY
      YLNF(N1,JJK)=YLNF(N1,JJK)+YLN
      YLPF(N1,JJK)=YLPF(N1,JJK)+YLP
      YLCF(N1,JJK)=YLCF(N1,JJK)+YLC
      YLKF(N1,JJK)=YLKF(N1,JJK)+YLK
      X7=.001*X12
	  STL(JJK)=STL(JJK)-X9
	  DM(JJK)=RW(JJK)+STL(JJK)
      UN1(JJK)=UN1(JJK)-YLN-AD1
      UP1(JJK)=UP1(JJK)-YLP-AD2
      !UK1(JJK)=UK1(JJK)-YLK
	  HU(JJK)=HU(JJK)*.1
      SLAI(JJK)=.001
      RETURN
      END