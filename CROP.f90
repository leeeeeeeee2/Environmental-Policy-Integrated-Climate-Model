      SUBROUTINE CROP
!     EPIC1102
!     THIS SUBPROGRAM PREDICTS DAILY POTENTIAL GROWTH OF TOTAL PLANT
!     BIOMASS AND ROOTS.
      USE PARM
      FNPP(X)=DMLA(JJK)*X/(X+EXP(PPCF(1,JJK)-PPCF(2,JJK)*X))
      XX=.685-.209*ROSP
      IF(IGO>1)THEN
          SUM=0.
          DO I=1,LC
              J=KG(I)
              IF(J==0)CYCLE
              SUM=SUM+PPL0(J)
          END DO    
          XLA1=FNPP(SUM)
          RTO=XLA1/XLAI(JJK)
          SLA1=RTO*SLAI(JJK)
          TLI=1.-EXP(-XX*SLA1)
          XXC=-LOG(1.-TLI/REAL(IGO))/SLAI(JJK)
      ELSE    
          XXC=XX    
      END IF    
      IF(IEVI==0)THEN
          X1=1.-EXP(-XXC*SLAI(JJK))
	  ELSE
	      X1=EVI
	  END IF
      PAR=.0005*SRAD*X1
	  X2=PAR*AJWA(JJK)
      IF(ICG==0)THEN
          X1=MAX(VPD-1.,-.5)
          XX=WA(JJK)-WAVP(JJK)*X1
          DDM(JJK)=XX*X2
	      IF(DDM(JJK)<1.E-10)DDM(JJK)=0.
      ELSE
          X2=X2*WA(JJK)
      	  VPDX=.67*(ASVP(TMX+273)-RHD*ASVP(TX+273.))
          X3=.01*WUB(JJK)*VPDX**(-.5)
	      X4=SU*X3
          DDM(JJK)=MIN(X2,X4)		
	  END IF
      DM(JJK)=DM(JJK)+DDM(JJK)
      RETURN
      END