      SUBROUTINE SOLIO(YTP,L)
!     EPIC1102
!     THIS SUBPROGRAM OUTPUTS THE SOIL TABLE
      USE PARM
	  DIMENSION YTP(16)
      WRITE(KW(L),'(1X,A,1X,I4)')'YR=',IY
      WRITE(KW(L),2)(SID(LORG(LID(J))),J=1,NBSL),SID(16)
      WRITE(KW(L),1)'DEPTH(m)',(Z(LID(I)),I=1,NBSL)
      WRITE(KW(L),4)'POROSITY(m/m)',(SOIL(8,LID(I)),I=1,NBSL),YTP(4)
      WRITE(KW(L),4)'FC SW(m/m)',(SOIL(9,LID(I)),I=1,NBSL),YTP(1)
      WRITE(KW(L),4)'WP SW(m/m)',(SOIL(20,LID(I)),I=1,NBSL),YTP(3)
      WRITE(KW(L),4)'SW(m/m)',(SOIL(12,LID(I)),I=1,NBSL),YTP(2)
      WRITE(KW(L),3)'SAT COND(mm/h)',(SATC(LID(I)),I=1,NBSL)
      WRITE(KW(L),3)'H SC(mm/h)',(HCL(LID(I)),I=1,NBSL)
      WRITE(KW(L),1)'BD 33kpa(t/m3)',(BD(LID(I)),I=1,NBSL),YTP(5)
      WRITE(KW(L),1)'BD DRY(t/m3)',(SOIL(13,LID(I)),I=1,NBSL)
      WRITE(KW(L),5)'SAND(%)',(SAN(LID(I)),I=1,NBSL)
      WRITE(KW(L),5)'SILT(%)',(SIL(LID(I)),I=1,NBSL)
      WRITE(KW(L),5)'CLAY(%)',(CLA(LID(I)),I=1,NBSL)
      WRITE(KW(L),5)'ROCK(%)',(ROK(LID(I)),I=1,NBSL)
      WRITE(KW(L),5)'PH',(PH(LID(I)),I=1,NBSL)
      WRITE(KW(L),5)'SM BS(cmol/kg)',(SMB(LID(I)),I=1,NBSL)
      WRITE(KW(L),5)'CEC(cmol/kg)',(CEC(LID(I)),I=1,NBSL)
      WRITE(KW(L),5)'AL SAT(%)',(ALS(LID(I)),I=1,NBSL)
      WRITE(KW(L),5)'CACO3(%)',(CAC(LID(I)),I=1,NBSL)
      WRITE(KW(L),6)'LAB P(g/t)',(SOIL(1,LID(I)),I=1,NBSL),TAP
      WRITE(KW(L),3)'P SORP RTO',(PSP(LID(I)),I=1,NBSL)
      WRITE(KW(L),6)'MN P AC(g/t)',(SOIL(2,LID(I)),I=1,NBSL),TMP
      WRITE(KW(L),6)'MN P ST(g/t)',(SOIL(3,LID(I)),I=1,NBSL),TOP
      WRITE(KW(L),6)'ORG P(g/t)',(SOIL(4,LID(I)),I=1,NBSL),TP
      WRITE(KW(L),6)'NO3(g/t)',(SOIL(5,LID(I)),I=1,NBSL),ZNO3
      WRITE(KW(L),6)'SOLK(g/t)',(SOIL(14,LID(I)),I=1,NBSL),TSK
      WRITE(KW(L),6)'EXCK(g/t)',(SOIL(15,LID(I)),I=1,NBSL),TEK
      WRITE(KW(L),6)'FIXK(g/t)',(SOIL(16,LID(I)),I=1,NBSL),TFK
      WRITE(KW(L),6)'ORG N(g/t)',(SOIL(6,LID(I)),I=1,NBSL),TWN
      X1=.001*TOC
      WRITE(KW(L),3)'ORG C(%)',(SOIL(7,LID(I)),I=1,NBSL),X1
      WRITE(KW(L),3)'CROP RSD(t/ha)',(RSD(LID(I)),I=1,NBSL),TRSD
      WRITE(KW(L),5)'WLS(kg/ha)',(WLS(LID(I)),I=1,NBSL),ZLS
      WRITE(KW(L),5)'WLM(kg/ha)',(WLM(LID(I)),I=1,NBSL),ZLM
      WRITE(KW(L),5)'WLSL(kg/ha)',(WLSL(LID(I)),I=1,NBSL),ZLSL
      WRITE(KW(L),5)'WLSC(kg/ha)',(WLSC(LID(I)),I=1,NBSL),ZLSC
      WRITE(KW(L),5)'WLMC(kg/ha)',(WLMC(LID(I)),I=1,NBSL),ZLMC
      WRITE(KW(L),5)'WLSLC(kg/ha)',(WLSLC(LID(I)),I=1,NBSL),ZLSLC
      WRITE(KW(L),5)'WLSLNC(kg/ha)',(WLSLNC(LID(I)),I=1,NBSL),ZLSLNC
      WRITE(KW(L),5)'WBMC(kg/ha)',(WBMC(LID(I)),I=1,NBSL),ZBMC
      WRITE(KW(L),5)'WHSC(kg/ha)',(WHSC(LID(I)),I=1,NBSL),ZHSC
      WRITE(KW(L),5)'WHPC(kg/ha)',(WHPC(LID(I)),I=1,NBSL),ZHPC
      X1=.001*TOC
      WRITE(KW(L),5)'WOC(kg/ha)',(WOC(LID(I)),I=1,NBSL),X1
      WRITE(KW(L),5)'WLSN(kg/ha)',(WLSN(LID(I)),I=1,NBSL),ZLSN
      WRITE(KW(L),5)'WLMN(kg/ha)',(WLMN(LID(I)),I=1,NBSL),ZLMN
      WRITE(KW(L),5)'WBMN(kg/ha)',(WBMN(LID(I)),I=1,NBSL),ZBMN
      WRITE(KW(L),5)'WHSN(kg/ha)',(WHSN(LID(I)),I=1,NBSL),ZHSN
      WRITE(KW(L),5)'WHPN(kg/ha)',(WHPN(LID(I)),I=1,NBSL),ZHPN
      WRITE(KW(L),6)'WON(kg/ha)',(WON(LID(I)),I=1,NBSL),TWN
      WRITE(KW(L),3)'ECND(mmho/cm)',(ECND(LID(I)),I=1,NBSL)
      WRITE(KW(L),6)'WSLT(kg/ha)',(WSLT(LID(I)),I=1,NBSL),TSLT
      WRITE(KW(L),4)'STFR',(STFR(LID(I)),I=1,NBSL)
      WRITE(KW(L),4)'CGO2(kg/ha)',(CGO2(LID(I)),I=1,NBSL)
      WRITE(KW(L),4)'CGCO2(kg/ha)',(CGCO2(LID(I)),I=1,NBSL)
      WRITE(KW(L),4)'CGN2O(kg/ha)',(CGN2O(LID(I)),I=1,NBSL)
      RETURN
    1 FORMAT(4X,A15,1X,16F12.5)  
    2 FORMAT(T52,'SOIL LAYER NO'/T22,16(4X,A4,4X))
    3 FORMAT(4X,A15,1X,16F12.2)
    4 FORMAT(4X,A15,1X,16F12.3)
    5 FORMAT(4X,A15,1X,16F12.1)
    6 FORMAT(4X,A15,1X,16F12.0)
      END