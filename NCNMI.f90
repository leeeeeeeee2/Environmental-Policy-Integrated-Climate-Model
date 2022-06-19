      SUBROUTINE NCNMI(Z5,CS,RTOE)
!     EPIC1102
!     THIS SUBPROGRAM SIMULATES MINERALIZATION AND IMMOBILIZATION OF N
!     AND C USING POOLS FOLLOWING CENTURY (IZAURRALDE ET AL. 2006) 
!     & C/N OF MICROBIAL BIOMASS FOLLOWING PHOENIX (MCGILL ET AL. 1981)
      USE PARM
      DATA SDF/0./
      ! WNO2 (ISL) added by WBM 2012-11-05
      XZ=WNO3(ISL)+WNO2(ISL)+WNH3(ISL)
      !AD1=WLSN(ISL)+WLMN(ISL)+WBMN(ISL)+WHSN(ISL)+WHPN(ISL)+XZ
      RLR=WLSL(ISL)/(WLS(ISL)+1.E-5)
      IF(RLR>.8)THEN
          RLR=.8
      ELSE
          IF(RLR<.1)RLR=.1
      END IF    
	  AD1=WBMC(ISL)+WHPC(ISL)+WHSC(ISL)+WLMC(ISL)+WLSC(ISL)
      XHN=WHSN(ISL)+WHPN(ISL)
	  IF(BD(ISL)>BDP(ISL))THEN
	      X1=MIN(20.,EXP(PRMT(52)*(BD(ISL)-BDP(ISL))))
	  ELSE
	      X1=1.
	  END IF
      SMS(4,ISL)=SMS(4,ISL)+X1
      OX=1.-PRMT(53)*Z5/(Z5+EXP(SCRP(20,1)-SCRP(20,2)*Z5))
      SELECT CASE(IOX)
          CASE(1)
	          Y1=.1*WOC(ISL)/WT(ISL)
	          Y2=2.11+.0375*CLA(ISL)
	          Y3=100.*Y1/Y2
	          OX=Y3/(Y3+EXP(SCRP(24,1)-SCRP(24,2)*Y3))
          CASE(2)
              IF(CGO2(LD1)>0.)OX=CGO2(ISL)/CGO2(LD1)
      END SELECT
      IF(IDN>2)OX=OX*RTOE   
      CS=MIN(10.,SQRT(CDG(ISL)*SUT(ISL))*PRMT(20)*OX*X1)
      SMS(11,ISL)=SMS(11,ISL)+1.
      SMS(1,ISL)=SMS(1,ISL)+SUT(ISL)
      APCO2=.55
      ASCO2=.60
      IF(ISL==LD1)THEN
          IF(IDC(JJK)==NDC(1))THEN
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
      ELSE
          ABCO2=.17+.0068*SAN(ISL)
          A1CO2=.55
          RBM=.02
          RLM=.0507
          RLS=.0132
          XBM=.25+.0075*SAN(ISL)
          HPNC=WHPN(ISL)/WHPC(ISL) 
      END IF
      BMNC=WBMN(ISL)/WBMC(ISL)
      HSNC=WHSN(ISL)/WHSC(ISL)
      XLMNC=WLMN(ISL)/WLMC(ISL)
      XLSNC=WLSN(ISL)/WLSC(ISL)
      ABP=.003+.00032*CLA(ISL)
      SMS(3,ISL)=SMS(3,ISL)+CS
      ASP=MAX(.001,PRMT(45)-.00009*CLA(ISL))
      A1=1.-A1CO2
      ASX=1.-ASCO2-ASP
      APX=1.-APCO2
      ! TRANSFORMATION OF STRUCTURAL LITTER WBM 2012-11-14
      ! CR = scaling factor for decomposition (0 - 1) based BMNC and &
      ! ratio of NC-substrate/Yield.
      CRX=1.-(CRUNC-BMNC)/(CRUNC-CRLNC)
      CR=0.
      IF(BMNC>CRLNC.AND.BMNC<CRUNC)CR=CRX
      CR0=CR
      X2=1.-RLR
      IF((WLSN(ISL)/(WLSC(ISL)*X2))/A1>BMNC.OR.BMNC>CRUNC)CR=1.
      TLSCA=RLS*CS*CR*EXP(-3.*RLR)*WLSC(ISL)
      TLSLNCA=TLSCA*X2
      TLSLCA=TLSCA*RLR
      TLSNA=TLSCA*XLSNC
      ! TRANSFORMATIONS METABOLIC LITTER; SIX LINES BELOW ADDED WBM 2012-11-14
      CR=CR0
      IF(XLMNC/A1>BMNC.OR.BMNC>CRUNC)CR=1.
      TLMCA=WLMC(ISL)*RLM*CS*CR
      TLMNA=TLMCA*XLMNC
      ! TRANSFORMATIONS MICROBIAL BIOMASS; NEXT SIX LINES ADDED WBM 2012-11-14
      CR=CR0
      IF(BMNC/(1.-ABCO2)>BMNC.OR.BMNC>CRUNC)CR=1.
      TBMCA=WBMC(ISL)*RBM*CS*CR*XBM
      TBMNA=TBMCA*BMNC
      ! TRANSFORMATION OF SLOW HUMUS; NEXT SIX LINES ADDED WBM 2012-11-14
      CR=CR0
      IF(HSNC/(1.-ASCO2)>BMNC.OR.BMNC>CRUNC)CR=1.
      THSCA=WHSC(ISL)*RHS*CS*CR
      THSNA=THSCA*HSNC
      ! TRANSFORMATIONS OF PASSIVE HUMUS; NEXT SIX LINES ADDED WBM 2012-11-14
      IF(ISL/=LD1)THEN
          CR=CR0
          IF(HPNC/APX>BMNC.OR.BMNC>CRUNC)CR=1.
          THPCA=WHPC(ISL)*CS*CR*RHP
          THPNA=THPCA*HPNC
          ! MINERALIZATION AND IMMOBILIZATION (32 LINES) ADDED WBM 2012-11-14
          ! CA = AMMONIFICATION RATE SCALING FACTOR BASED ON BMNC (0 - 1)
          ! CU = IMMOPBILIZATION (UPTAKE) RATE SCALING FACTOR BASED ON BMNC (0 - 1)
          ! WKA = SPECIFIC BASE RATE FOR AMMONIFICATION (d-1)
          ! WNCMIN = BMNC AT WHICH IMMOBILIZATION IS A MAXIMUM; BMNC AT WHICH AMMONIFICATION CEASES
          ! WNCMAX = BMNC AT WHICH IMMOBILIZATION CEASES; BMNC AT WHICH AMMONIFICATION IS A MAXIMUM
          ! VMU = MAXIMUM RATE OF UPTAKE OF N DURING IMMOBILIZATION (gN (gC-1) d-1)
          ! WKMNH3 = HALF SATURATION CONSTANT FOR AMMONIA IMMOBILIZATION (mg N L-1)
          ! WKMNO2 = HALF SATURATION CONSTANT FOR NITRITE IMMOBILIZATION (mg N L-1)
          ! WKMNO3 = HALF SATURATION CONSTANT FOR NITRATE IMMOBILIZATION (mg N L-1)
      END IF
      XSF=(BMNC-WNCMIN)/(WNCMAX-WNCMIN)
      CA=0.
      IF(BMNC>WNCMIN.AND.BMNC<WNCMAX)CA=XSF
      IF(BMNC>WNCMAX)CA=1.
      CU=.001
      IF(BMNC>WNCMIN.AND.BMNC<WNCMAX)CU=1.-XSF
      IF(BMNC<WNCMIN)CU=1.
      ! AMMONIFICATION (N MINERLIZATION) PRODUCRT OF CA*CS*WKA*BMN
      SUP=MAX(0.,CA*CS*WKA*WBMN(ISL))
      ! MICROBIAL UPTAKE OF N (N IMMOBILIZATION) MICHAELIS-MENTEN EXPRESION &
      ! DRIVEN BY MICROBIAL CARBON AND REGUALTED BY CU
      XU=CS*CU*VMU
      XDENOM=.01*ST(ISL)
      DMDNMAX=XU*WBMC(ISL)
      XWNH3=WNH3(ISL)/XDENOM
      XWNO2=WNO2(ISL)/XDENOM
      XWNO3=WNO3(ISL)/XDENOM
      DMDNH3MX=MIN(WNH3(ISL),XWNH3*DMDNMAX/(WKMNH3+XWNH3))
      DMDNO2MX=MIN(WNO2(ISL),XWNO2*DMDNMAX/(WKMNO2+XWNO2))
      DMDNO3MX=MIN(WNO3(ISL),XWNO3*DMDNMAX/(WKMNO3+XWNO3))
      DMDN=DMDNH3MX+DMDNO2MX+DMDNO3MX
      XX=1.
      IF(DMDN>DMDNMAX)XX=DMDNMAX/DMDN
      TUPH3=DMDNH3MX*XX
      TUPO2=DMDNO2MX*XX
      TUPO3=DMDNO3MX*XX
      SGMN=SGMN+SUP
      XNIMO=-TUPH3-TUPO2-TUPO3
      RNMN(ISL)=SUP+XNIMO
      SMNIM=SMNIM+XNIMO
      WNO3(ISL)=MAX(1.E-10,WNO3(ISL)-TUPO3)
      WNO2(ISL)=MAX(1.E-10,WNO2(ISL)-TUPO2)
      WNH3(ISL)=MAX(1.E-10,WNH3(ISL)-TUPH3+SUP)
      SNMN=SNMN+RNMN(ISL)
      SMS(9,ISL)=SMS(9,ISL)+RNMN(ISL)
      IF(TLSCA>WLSC(ISL))TLSCA=WLSC(ISL)
	  WLSC(ISL)=WLSC(ISL)-TLSCA
      IF(TLSLCA>WLSLC(ISL))TLSLCA=WLSLC(ISL)
	  WLSLC(ISL)=WLSLC(ISL)-TLSLCA
      WLSLNC(ISL)=WLSC(ISL)-WLSLC(ISL)
      TLMCA=MIN(WLMC(ISL),TLMCA)
      IF(WLM(ISL)>0.)THEN
          RTO=MAX(.42,WLMC(ISL)/WLM(ISL))
          WLM(ISL)=WLM(ISL)-TLMCA/RTO
          WLMC(ISL)=MAX(.01,WLMC(ISL)-TLMCA)
      END IF
      WLSL(ISL)=WLSL(ISL)-TLSLCA/.42
      WLS(ISL)=WLSC(ISL)/.42
      IF(ISL==LD1)THEN 
          X3=ASX*THSCA+A1*(TLMCA+TLSLNCA)
          X1=.7*TLSLCA+TBMCA*(1.-ABCO2)
          WBMN(ISL)=WBMN(ISL)-TBMNA+THSNA*(1.-ASP)+TLMNA+TLSNA-RNMN(ISL)
          !RSPC(ISL)=.3*TLSLCA+A1CO2*(TLSLNCA+TLMCA)+ABCO2*TBMCA+ASCO2*THSCA
      ELSE
          X3=APX*THPCA+ASX*THSCA+A1*(TLMCA+TLSLNCA)
          X1=.7*TLSLCA+TBMCA*(1.-ABP-ABCO2)
          X2=THSCA*ASP+TBMCA*ABP
          WHPC(ISL)=WHPC(ISL)-THPCA+X2
          WBMN(ISL)=WBMN(ISL)-TBMNA+THPNA+THSNA*(1.-ASP)+TLMNA+TLSNA-RNMN(ISL)
          WHPN(ISL)=WHPN(ISL)-THPNA+TBMNA*ABP+THSNA*ASP
          !RSPC(ISL)=.3*TLSLCA+A1CO2*(TLSLNCA+TLMCA)+ABCO2*TBMCA+ASCO2*THSCA+&
          !APCO2*THPCA
      END IF        
      WBMC(ISL)=WBMC(ISL)-TBMCA+X3
      WHSC(ISL)=WHSC(ISL)-THSCA+X1
      AD2=WBMC(ISL)+WHPC(ISL)+WHSC(ISL)+WLMC(ISL)+WLSC(ISL)
      RSPC(ISL)=AD1-AD2
      WHSN(ISL)=WHSN(ISL)-THSNA+TBMNA*(1.-ABP)
      WLSN(ISL)=WLSN(ISL)-TLSNA
      WLMN(ISL)=WLMN(ISL)-TLMNA
      SMM(74,MO)=SMM(74,MO)+RSPC(ISL)
      VAR(74)=VAR(74)+RSPC(ISL)
      SMS(8,ISL)=SMS(8,ISL)+RSPC(ISL)
      TRSP=TRSP+RSPC(ISL)      
      RSD(ISL)=.001*(WLS(ISL)+WLM(ISL))
      DHN(ISL)=XHN-WHSN(ISL)-WHPN(ISL)
      DF=AD1-AD2-RSPC(ISL)
      SDF=SDF+DF
      IF(ABS(DF)>.01)WRITE(KW(1),3)IY,MO,KDA,ISL,AD1,AD2,RSPC(ISL),DF,SDF
      !XZ=WNO3(ISL)+WNO2(ISL)+WNH3(ISL)
      !AD2=WLSN(ISL)+WLMN(ISL)+WBMN(ISL)+WHSN(ISL)+WHPN(ISL)+XZ
      !DF=AD2-AD1
      !SDF=SDF+DF
      !IF(ABS(DF)>.01)WRITE(KW(1),3)IY,MO,KDA,ISL,AD1,AD2,DF,SDF
    3 FORMAT(1X,'NCNMI',4I4,10E16.6)      
      RETURN
      END