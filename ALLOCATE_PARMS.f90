      SUBROUTINE ALLOCATE_PARMS                                                      
!     THIS SUBROUTINE ALLOCATES ARRAY SIZES                                          
      USE PARM                                                                       
      MFT=20                                                                          
	  MSL=15
	  MSC=31                                                                              
      NSM=114                                                                              
      MPS=40                                                                         
	  MNC=30                                                                              
      MRO=150                                                                        
      MNT=100                                                                        
      NGF=MSO+5                                                                      
	  ALLOCATE(CPNM(MNC),FTNM(MFT),HED(NSM),PSTN(MPS),TIL(MNT),ASHZ(MSL))                           
      ALLOCATE(IAP(MNC),IHU(MNC),IYH(MNC),JE(MNC),JP(MNC),JPL(MNC),&
      KDC(MNC),KG(MNC),KOMP(MNT),LID(MSL+1),LORG(MSL),NBC(MRO),NBE(MNT),&
      NBT(MNT),NCP(MNC),NCR(MNC),NHU(MNC),NHV(MNC),NPST(MRO),NTL(MRO),&
      NYLN(MNC),ICUS(MNT),IHC(MNT),IHT(MNT),KDF(MFT),KDP(MPS),KFL(NGF))                        
      ALLOCATE(ITL(MRO,MNT),LFT(MRO,MNT),LT(MRO,MNT),LYR(MRO,MNT),NGZ&               
      (MRO,MNT),LPC(MRO,MNT),JH(MRO,MNT),LY(MRO,MNC),NBCX(MRO,MNC),&
      NPT(6,MSL),IGMD(10,MNC),IHVD(10,MNC),IPLD(10,MNC),NPSF(10,MNC))
      ALLOCATE(ACO2C(MSC),AFP(MSC),AN2OC(MSC),AO2C(MSC),CGCO2(MSC),CGN2O&
      (MSC),CGO2(MSC),CLCO2(MSC),CLN2O(MSC),CLO2(MSC),DCO2GEN(MSC),DN2G&
      (MSC),DN2OG(MSC),DO2CONS(MSC),DPRC(MSC),DPRN(MSC),DPRO(MSC),DRWX&
      (MSC),EAR(MSC),FC(MSC),HKPC(MSC),HKPN(MSC),HKPO(MSC),&
      RSPC(MSC),RWTZ(MSC),S15(MSC),SMEA(MSC),SMES(MSC),SOT(MSC),&
      SSFCO2(MSC),SSFN2O(MSC),SSFO2(MSC),TPOR(MSC),VCO2(MSC),VGA(MSC),&
      VGN(MSC),VN2O(MSC),VO2(MSC),VFC(MSC),VWC(MSC),VWP(MSC),WBMC(MSC),&
      WCO2G(MSC),WCO2L(MSC),WN2O(MSC),WN2OG(MSC),WN2OL(MSC),WNO2(MSC),&
      WNH3(MSC),WNO3(MSC),WO2G(MSC),WO2L(MSC),XN2O(MSC),ZC(MSC))    
      ALLOCATE(ALS(MSL),AP(MSL),BD(MSL),BDD(MSL),BDM(MSL),BDP(MSL),&
      BMIX(MSL),BPC(MSL),CAC(MSL),CBNF(MSL),CDG(MSL),CEC(MSL),CEM(MSL),&
      CLA(MSL),CNDS(MSL),CN3F(MSL),CPRH(MSL),CPRV(MSL),DHN(MSL),&
      ECND(MSL),EQKE(MSL),EQKS(MSL),EXCK(MSL),FE26(MSL),FIXK(MSL),&
      FOP(MSL),H2OF(MSL),HCL(MSL),HK(MSL),PH(MSL),OP(MSL),PKRZ(MSL),&
      PMN(MSL),PO(MSL),PSP(MSL),RNMN(MSL),ROK(MSL),RSD(MSL),SAN(MSL),&
      SATC(MSL),SEV(MSL),SIL(MSL),SLTP(MSL),SMB(MSL),SMSL(MSL),SOLK(MSL))                
      ALLOCATE(SSF(MSL),ST(MSL),STFR(MSL),STMP(MSL),SUT(MSL),U(MSL),&
      UK(MSL),UN(MSL),UP(MSL),VNO2(MSL),VNO3(MSL),WBMN(MSL),WDN(MSL),&
      WHPC(MSL),WHPN(MSL),WHSC(MSL),WHSN(MSL),WLM(MSL),WLMC(MSL),&
      WLMN(MSL),WLS(MSL),WLSC(MSL),WLSL(MSL),WLSLC(MSL),WLSLNC(MSL),&
      WLSN(MSL),WNO3F(MSL),WOC(MSL),WON(MSL),WP(MSL),WSLT(MSL),WT(MSL),Z(MSL))                  
      ALLOCATE(ANA(MNC),AWC(MNC),CAF(MNC),CKY(MNC),CNY(MNC),CPY(MNC),&
      CSTS(MNC),FLF0(MNC),FLSL(MNC),FNMX(MNC),HUF(MNC),PST(MNC),&
      PSTM(MNC),RDF(MNC),SDW(MNC),TOPC(MNC),TFTN(MNC),TFTP(MNC),WCY(MNC))                                                  
      ALLOCATE(ACET(MNC),AJHI(MNC),AJWA(MNC),ALT(MNC),BLYN(MNC),CCEM&                
      (MNC),CPHT(MNC),DDM(MNC),DLAI(MNC),DM(MNC),DMLA(MNC),DMLX(MNC),DM1&             
      (MNC),EP(MNC),FLT(MNC),FTO(MNC),GMHU(MNC),GSI(MNC),HI(MNC),HMX&                
      (MNC),HU(MNC),HUI(MNC),PPL0(MNC),PRYG(MNC),PRYF(MNC),PSTF(MNC),&               
      RBMD(MNC),RD(MNC),RDMX(MNC),REG(MNC),RLAD(MNC),RW(MNC),RWX(MNC),&              
      SLAI(MNC),STD(MNC),STDN(MNC),STL(MNC),STL0(MNC),SWH(MNC),SWP(MNC),&
      TCAW(MNC),TCQV(MNC),TCRF(MNC),TCSO(MNC),TCST(MNC),TDM(MNC))                                                 
      ALLOCATE(TETG(MNC),TFTK(MNC),TBSC(MNC),THU(MNC),TIRL(MNC),TRA(MNC)&            
      ,TRD(MNC),TVAL(MNC),TVIR(MNC),TYLC(MNC),TYLK(MNC),TYLN(MNC),TYLP&
      (MNC),TYL1(MNC),TYL2(MNC),UK1(MNC),UK2(MNC),ULYN(MNC),UNA(MNC),UN1&
      (MNC),UN2(MNC),UNMX(MNC),UP1(MNC),UP2(MNC),UPMX(MNC),VPD2(MNC),&
      VPTH(MNC),WA(MNC),WAVP(MNC),WCHT(MNC),WLV(MNC),WSYF(MNC),WUB(MNC),&
      XDLA0(MNC),XDLAI(MNC),XLAI(MNC),XMTU(MNC),YLD(MNC))                                                                     
      ALLOCATE(COOP(MNT),COTL(MNT),DKH(MNT),DKI(MNT),EFM(MNT),EMX(MNT)&              
      ,FPOP(MNT),FRCP(MNT),FULU(MNT),HE(MNT),HMO(MNT),ORHI(MNT),RHT(MNT)&            
      ,RIN(MNT),RR(MNT),TCEM(MNT),TLD(MNT))                                          
      ALLOCATE(FCEM(MFT),FCST(MFT),FK(MFT),FN(MFT),FNH3(MFT),FNO(MFT),&              
      FOC(MFT),FP(MFT),FPO(MFT),FSLT(MFT))                                           
      ALLOCATE(GWPS(MPS),PCEM(MPS),PCST(MPS),PFOL(MPS),PHLF(MPS),PHLS(MPS),PKOC&               
      (MPS),PLCH(MPS),PSOL(MPS),PWOF(MPS),RSPS(MPS),SSPS(MPS))                                 
      ALLOCATE(SM(NSM),SMY(NSM),VAR(NSM))                                            
      ALLOCATE(DLAP(2,MNC),FRST(2,MNC),PPCF(2,MNC),PPLP(2,MNC),RWPC(2,&              
      MNC),STX(2,MNC),WAC2(2,MNC))                                                   
      ALLOCATE(PSTE(MRO,MNT),PSTR(MRO,MNT),PVQ(MPS,90),PVY(MPS,90),SMM&              
      (NSM,12),PHU(MNC,150),POP(MNC,150),PPLA(MNC,150),PSTZ(MPS,MSL),TPAC&              
      (MNC,MPS),SMAP(10,MPS),SMYP(10,MPS),VARP(12,MPS),SMS(11,MSL),SPQ(5&            
      ,MPS),SPQC(5,MPS),SPY(5,MPS),SOIL(20,MSL),STDA(4,MNC),BK(4,MNC),BN&            
      (4,MNC),BP(4,MNC),BLG(3,MNC),BWD(3,MNC))                                       
      ALLOCATE(VARC(20,MNC),RWT(MSC,MNC),RWTX(MSL,MNC),FRTK(10,MNC),&                
      FRTN(10,MNC),FRTP(10,MNC),TPSF(10,MNC),CAW(10,MNC),CQV(10,MNC),CRF&            
      (10,MNC),CSOF(10,MNC),CSTF(10,MNC),DMF(10,MNC),ETG(10,MNC),HIF(10,&            
      MNC),RWF(10,MNC),VIL(10,MNC),VIR(10,MNC),YLCF(10,MNC),YLD1(10,MNC),&
      YLD2(10,MNC),YLKF(10,MNC),YLNF(10,MNC),YLPF(10,MNC),SOL(23,MSL),&
      TSFC(7,MNC),CGSF(7,MNC),SFMO(7,MNC))                                                                  
      ALLOCATE(CFRT(MRO,MNT),CND(MRO,MNT),HUSC(MRO,MNT),HWC(MRO,MNT),&
      QIR(MRO,MNT),RSTK(MRO,MNT),TIR(MRO,MNT),TLMA(MRO,MNT),VIRR(MRO,MNT),&
      WFA(MRO,MNT))                                       
      ALLOCATE(APQ(5,MPS,MYR),APQC(5,MPS,MYR),APY(5,MPS,MYR),AQB(5,MPS&              
      ,MYR),AYB(5,MPS,MYR),SMMP(12,MPS,12),SMMC(20,MNC,12),SF(7,10,MNC))             
      END                                                                            
