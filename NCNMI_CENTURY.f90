      SUBROUTINE NCNMI_CENTURY(Z5,CS)
!     EPIC0810
!     THIS SUBPROGRAM SIMULATES MINERALIZATION AND IMMOBILIZATION OF N
!     AND C USING EQUATIONS TAKEN FROM CENTURY.
      USE PARM
      XZ=WNO3(ISL)+WNH3(ISL)
!     AD1=WLSN(ISL)+WLMN(ISL)+WBMN(ISL)+WHSN(ISL)+WHPN(ISL)+XZ
      RLR=MIN(.8,WLSL(ISL)/(WLS(ISL)+1.E-5))
	  RHS=PRMT(47)
      RHP=PRMT(48)
      XHN=WHSN(ISL)+WHPN(ISL)
      AD1=WLSC(ISL)+WLMC(ISL)+WBMC(ISL)+WHSC(ISL)+WHPC(ISL)
	  !IF(IOX>0)THEN
	      Y1=.1*WOC(ISL)/WT(ISL)
	      Y2=2.11+.0375*CLA(ISL)
	      Y3=100.*Y1/Y2
	      OXK=Y3/(Y3+EXP(SCRP(24,1)-SCRP(24,2)*Y3))
	  !ELSE
          OXZ=1.-PRMT(53)*Z5/(Z5+EXP(SCRP(20,1)-SCRP(20,2)*Z5))
      !END IF
      OX=PRMT(30)*OXK+(1.-PRMT(30))*OXZ
      IF(BD(ISL)>BDP(ISL))THEN
	      X1=MIN(20.,EXP(PRMT(52)*(BD(ISL)-BDP(ISL))))
	  ELSE
	      X1=1.
	  END IF
      SMS(4,ISL)=SMS(4,ISL)+X1
      CS=MIN(10.,SQRT(CDG(ISL)*SUT(ISL))*PRMT(20)*OX*X1)
      SMS(11,ISL)=SMS(11,ISL)+1.
      SMS(1,ISL)=SMS(1,ISL)+SUT(ISL)
      APCO2=.55
      ASCO2=.60
      IF(ISL==LD1)THEN
          IF(IDC(JD)==NDC(1))THEN
              CS=1.
              RLM=.2
              RLS=.2
          ELSE
              CS=CS*PRMT(51)
              RLM=.0405
              RLS=.0107
          END IF
          ABCO2=.55
          A1CO2=.55
          RBM=.0164
          HPNC=.1
          XBM=1.
    !     COMPUTE N/C RATIOS
          X1=.1*(WLMN(LD1)+WLSN(LD1))/(RSD(LD1)+1.E-5)
          IF(X1>2.)THEN
              BMNC=.1
          ELSE
              IF(X1>.01)THEN
                  BMNC=1./(20.05-5.0251*X1)
              ELSE
                  BMNC=.05
              END IF    
          END IF    
          HSNC=BMNC/(5.*BMNC+1.)
      ELSE
          ABCO2=.17+.0068*SAN(ISL)
          A1CO2=.55
          RBM=.02
          RLM=.0507
          RLS=.0132
          XBM=.25+.0075*SAN(ISL)
          X1=1000.*(WNH3(ISL)+WNO3(ISL))/WT(ISL)
          IF(X1>7.15)THEN
              BMNC=.33
              HSNC=.083
              HPNC=.143
          ELSE
              BMNC=1./(15.-1.678*X1)
              HSNC=1./(20.-1.119*X1)
              HPNC=1./(10.-.42*X1)
          END IF
      END IF    
      ABP=.003+.00032*CLA(ISL)
      SMS(3,ISL)=SMS(3,ISL)+CS
      ASP=MAX(.001,PRMT(45)-.00009*CLA(ISL))
!     POTENTIAL TRANSFORMATIONS STRUCTURAL LITTER
      X1=RLS*CS*EXP(-3.*RLR)
      TLSCP=X1*WLSC(ISL)
      TLSLCP=TLSCP*RLR
      TLSLNCP=TLSCP*(1.-RLR)
      TLSNP=X1*WLSN(ISL)
!     POTENTIAL TRANSFORMATIONS METABOLIC LITTER
      X1=RLM*CS
      TLMCP=WLMC(ISL)*X1
      TLMNP=WLMN(ISL)*X1
!     POTENTIAL TRANSFORMATIONS MICROBIAL BIOMASS
      X1=RBM*CS*XBM
      TBMCP=WBMC(ISL)*X1
      TBMNP=WBMN(ISL)*X1
!     POTENTIAL TRANSFORMATIONS SLOW HUMUS
      X1=RHS*CS
      THSCP=WHSC(ISL)*X1
      THSNP=WHSN(ISL)*X1
!     POTENTIAL TRANSFORMATIONS PASSIVE HUMUS
      X1=CS*RHP
      THPCP=WHPC(ISL)*X1
      THPNP=WHPN(ISL)*X1
!     ESTIMATE N DEMAND
      A1=1.-A1CO2
      ASX=1.-ASCO2-ASP
      APX=1.-APCO2
      PN1=TLSLNCP*A1*BMNC
      PN2=.7*TLSLCP*HSNC
      PN3=TLMCP*A1*BMNC
      PN5=TBMCP*ABP*HPNC
      PN6=TBMCP*(1.-ABP-ABCO2)*HSNC
      PN7=THSCP*ASX*BMNC
      PN8=THSCP*ASP*HPNC
      PN9=THPCP*APX*BMNC
!     COMPARE SUPPLY AND DEMAND FOR N
      SUM=0.
      CPN1=0.
      CPN2=0.
      CPN3=0.
      CPN4=0.
      CPN5=0.
      X1=PN1+PN2
      IF(TLSNP<X1)THEN
          CPN1=X1-TLSNP
      ELSE
          SUM=SUM+TLSNP-X1
      END IF
      IF(TLMNP<PN3)THEN
          CPN2=PN3-TLMNP
      ELSE
          SUM=SUM+TLMNP-PN3
      END IF
      X1=PN5+PN6
      IF(TBMNP<X1)THEN
          CPN3=X1-TBMNP
      ELSE
          SUM=SUM+TBMNP-X1
      END IF      
      X1=PN7+PN8
      IF(THSNP<X1)THEN
          CPN4=X1-THSNP
      ELSE
          SUM=SUM+THSNP-X1
      END IF
      IF(THPNP<PN9)THEN
          CPN5=PN9-THPNP
      ELSE
          SUM=SUM+THPNP-PN9
      END IF
!     WNH3(ISL)=WNH3(ISL)+SUM
      WMIN=MAX(1.E-5,WNO3(ISL)+SUM)
      DMDN=CPN1+CPN2+CPN3+CPN4+CPN5
      X3=1.
!     REDUCE DEMAND IF SUPPLY LIMITS
      IF(WMIN<DMDN)X3=WMIN/DMDN
      SMS(5,ISL)=SMS(5,ISL)+X3
!     ACTUAL TRANSFORMATIONS
      TLSCA=TLSCP*X3
      TLSLCA=TLSLCP*X3
      TLSLNCA=TLSLNCP*X3
      TLSNA=TLSNP*X3
      TLMCA=TLMCP*X3
      TLMNA=TLMNP*X3
      TBMCA=TBMCP*X3
      TBMNA=TBMNP*X3
      THSCA=THSCP*X3
      THSNA=THSNP*X3
      THPCA=THPCP*X3
      THPNA=THPNP*X3
!     DMDN=DMDN*X3
      SGMN=SGMN+SUM
      RNMN(ISL)=SUM-DMDN
!     UPDATE
      IF(RNMN(ISL)>0.)THEN
          X1=PRMT(96)*RNMN(ISL)
          WNH3(ISL)=WNH3(ISL)+X1
  	      WNO3(ISL)=WNO3(ISL)+RNMN(ISL)-X1
      ELSE
	      X1=WNO3(ISL)+RNMN(ISL)
	      IF(X1>0.)THEN
              WNO3(ISL)=X1
	      ELSE
              RNMN(ISL)=-WNO3(ISL)
              WNO3(ISL)=1.E-10    
          END IF   
      END IF    
      DF1=TLSNA
      DF2=TLMNA
	  SNMN=SNMN+RNMN(ISL)
	  SMS(9,ISL)=SMS(9,ISL)+RNMN(ISL)
      WLSC(ISL)=MAX(1.E-10,WLSC(ISL)-TLSCA)
      WLSLC(ISL)=MAX(1.E-10,WLSLC(ISL)-TLSLCA)
      WLSLNC(ISL)=MAX(1.E-10,WLSC(ISL)-WLSLC(ISL))
      WLMC(ISL)=MAX(1.E-10,WLMC(ISL)-TLMCA)
      WLM(ISL)=MAX(1.E-10,WLM(ISL)-TLMCA/.42)
      WLSL(ISL)=MAX(1.E-10,WLSL(ISL)-TLSLCA/.42)
      WLS(ISL)=MAX(1.E-10,WLS(ISL)-TLSCA/.42)
      X3=APX*THPCA+ASX*THSCA+A1*(TLMCA+TLSLNCA)
      WBMC(ISL)=MAX(1.E-10,WBMC(ISL)-TBMCA+X3)
      DF3=TBMNA-BMNC*X3
      X1=.7*TLSLCA+TBMCA*(1.-ABP-ABCO2)
      WHSC(ISL)=MAX(1.E-5,WHSC(ISL)-THSCA+X1)
      DF4=THSNA-HSNC*X1
      X1=THSCA*ASP+TBMCA*ABP
      WHPC(ISL)=MAX(1.E-5,WHPC(ISL)-THPCA+X1)
      DF5=THPNA-HPNC*X1
      DF6=XZ-WNO3(ISL)-WNH3(ISL)
      SMS(10,ISL)=SMS(10,ISL)-DF6
      ADD=DF1+DF2+DF3+DF4+DF5+DF6
      ADF1=ABS(DF1)
      ADF2=ABS(DF2)
      ADF3=ABS(DF3)
      ADF4=ABS(DF4)
      ADF5=ABS(DF5)
      TOT=ADF1+ADF2+ADF3+ADF4+ADF5
      XX=ADD/(TOT+1.E-10)
      WLSN(ISL)=MAX(.001,WLSN(ISL)-DF1+XX*ADF1)
      WLMN(ISL)=MAX(.001,WLMN(ISL)-DF2+XX*ADF2)
      WBMN(ISL)=WBMN(ISL)-DF3+XX*ADF3
      WHSN(ISL)=WHSN(ISL)-DF4+XX*ADF4
      WHPN(ISL)=WHPN(ISL)-DF5+XX*ADF5
      !RSPC(ISL)=MAX(0.,.3*TLSLCA+A1CO2*(TLSLNCA+TLMCA)+ABCO2*TBMCA+&
      !ASCO2*THSCA+APCO2*THPCA)
      AD2=WLSC(ISL)+WLMC(ISL)+WBMC(ISL)+WHSC(ISL)+WHPC(ISL)
      RSPC(ISL)=MAX(0.,AD1-AD2)
      SMM(74,MO)=SMM(74,MO)+RSPC(ISL)
      SMS(8,ISL)=SMS(8,ISL)+RSPC(ISL)
      TRSP=TRSP+RSPC(ISL)      
      VAR(74)=VAR(74)+RSPC(ISL)
      RSD(ISL)=.001*(WLS(ISL)+WLM(ISL))
      DHN(ISL)=XHN-WHSN(ISL)-WHPN(ISL)
!     XZ=WNO3(ISL)+WNH3(ISL)
!     AD2=WLSN(ISL)+WLMN(ISL)+WBMN(ISL)+WHSN(ISL)+WHPN(ISL)+XZ
!     DF=AD2-AD1
!     IF(ABS(DF)>.001)WRITE(KW(1),1)IY,MO,KDA,AD1,AD2,DF
!   1 FORMAT(1X,'NCNMI',3I4,3E16.6)      
      RETURN
      END