      SUBROUTINE SPLA(I,I1,K,L,RTO)
!     EPIC1102
      USE PARM
      IF(L>0)Z(K)=(Z(I)+Z(I1))*.5
      PSP(K)=PSP(I)
      BD(K)=BD(I)
      BDM(K)=BDM(I)
      CLA(K)=CLA(I)
      SIL(K)=SIL(I)
      SAN(K)=SAN(I)
      ROK(K)=ROK(I)
      SATC(K)=SATC(I)
      HCL(K)=HCL(I)
      STFR(K)=STFR(I)
      PH(K)=PH(I)
      STMP(K)=STMP(I)
      BDD(K)=BDD(I)
      BPC(K)=BPC(I)
      BDP(K)=BDP(I)
      SMB(K)=SMB(I)
      CEC(K)=CEC(I)
      CGO2(K)=CGO2(I)
      CGCO2(K)=CGCO2(I)
      CGN2O(K)=CGN2O(I)
      VGA(K)=VGA(I)
      VGN(K)=VGN(I)
      IF(ISW==1.OR.ISW==3.OR.ISW==5)CEM(K)=CEM(I)
      CAC(K)=CAC(I)
      ALS(K)=ALS(I)
      ECND(K)=ECND(I)
      WT(K)=EAJL(WT(I),RTO)
      WNO3(K)=EAJL(WNO3(I),RTO)
      WP(K)=EAJL(WP(I),RTO)
      WHPN(K)=EAJL(WHPN(I),RTO)
      WHSN(K)=EAJL(WHSN(I),RTO)
      WBMN(K)=EAJL(WBMN(I),RTO)
      WLSN(K)=EAJL(WLSN(I),RTO)
      WLMN(K)=EAJL(WLMN(I),RTO)
      WHPC(K)=EAJL(WHPC(I),RTO)
      WHSC(K)=EAJL(WHSC(I),RTO)
      WBMC(K)=EAJL(WBMC(I),RTO)
      WLS(K)=EAJL(WLS(I),RTO)
      WLM(K)=EAJL(WLM(I),RTO)
      WLSL(K)=EAJL(WLSL(I),RTO)
      WLSC(K)=.42*WLS(K)
      WLMC(K)=.42*WLM(K)
      WLSLC(K)=.42*WLSL(K)
      WLSLNC(K)=WLSC(K)-WLSLC(K)
      RSD(K)=.001*(WLS(K)+WLM(K))
      WLSC(I)=.42*WLS(I)
      WLMC(I)=.42*WLM(I)
      WLSLC(I)=.42*WLSL(I)
      WLSLNC(I)=WLSC(I)-WLSLC(I)
      RSD(I)=.001*(WLS(I)+WLM(I))
      WOC(I)=WBMC(I)+WHPC(I)+WHSC(I)+WLMC(I)+WLSC(I)
      WOC(K)=WBMC(K)+WHPC(K)+WHSC(K)+WLMC(K)+WLSC(K)
      WON(I)=WBMN(I)+WHPN(I)+WHSN(I)+WLMN(I)+WLSN(I)
      WON(K)=WBMN(K)+WHPN(K)+WHSN(K)+WLMN(K)+WLSN(K)
      AP(K)=EAJL(AP(I),RTO)
      PMN(K)=EAJL(PMN(I),RTO)
      FOP(K)=EAJL(FOP(I),RTO)
      OP(K)=EAJL(OP(I),RTO)
      SOLK(K)=EAJL(SOLK(I),RTO)
      EXCK(K)=EAJL(EXCK(I),RTO)
      FIXK(K)=EAJL(FIXK(I),RTO)
      EQKS(K)=EAJL(EQKS(I),RTO)
      EQKE(K)=EAJL(EQKE(I),RTO)
      WSLT(K)=EAJL(WSLT(I),RTO)
      S15(K)=EAJL(S15(I),RTO)
      FC(K)=EAJL(FC(I),RTO)
      PO(K)=EAJL(PO(I),RTO)
      ST(K)=EAJL(ST(I),RTO)
      RETURN
      END