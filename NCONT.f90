      SUBROUTINE NCONT
      ! EPIC1102
      USE PARM
      CHARACTER(1)::ABL
      DATA ABL/'C'/
      DATA SMC,SMN/0.,0./
      TEK=0.
      TFK=0.
      TSK=0.
      ZN2O=0.
      ZN2OG=0.
      ZN2OL=0.
      ZNO2=0.
      ZNO3=0.
      ZNH3=0.
      TAP=0.
      TP=0.
      TMP=0.
      TRSD=0.
      TFOP=0.
      TWN=0.
      TOP=0.
      ZLS=0.
      ZLM=0.
      ZLSL=0.
      ZLSC=0.
      ZLMC=0.
      ZLSLC=0.
      ZLSLNC=0.
      ZBMC=0.
      ZHSC=0.
      ZHPC=0.
      ZLSN=0.
      ZLMN=0.
      ZBMN=0.
      ZHSN=0.
      ZHPN=0.
      TNOR=0.
      DO J=1,NBSL
          LL=LID(J)
          ZN2O=ZN2O+WN2O(LL)
          ZNO2=ZNO2+WNO2(LL)
          ZN2OG=ZN2OG+WN2OG(LL)
          ZN2OL=ZN2OL+WN2OL(LL)
          ZNO3=ZNO3+WNO3(LL)
          ZNH3=ZNH3+WNH3(LL)
          TAP=TAP+AP(LL)
          TOP=TOP+OP(LL)
          TMP=TMP+PMN(LL)
          TP=TP+WP(LL)
          TRSD=TRSD+RSD(LL)
          SW=SW+ST(LL)
          TFOP=TFOP+FOP(LL)
          TEK=TEK+EXCK(LL)
          TFK=TFK+FIXK(LL)
          TSK=TSK+SOLK(LL)
          ZLS=ZLS+WLS(LL)
          ZLM=ZLM+WLM(LL)
          ZLSL=ZLSL+WLSL(LL)
          ZLSC=ZLSC+WLSC(LL)
          ZLMC=ZLMC+WLMC(LL)
          ZLSLC=ZLSLC+WLSLC(LL)
          ZLSLNC=ZLSLNC+WLSLNC(LL)
          ZBMC=ZBMC+WBMC(LL)
          ZHSC=ZHSC+WHSC(LL)
          ZHPC=ZHPC+WHPC(LL)
          WOC(LL)=WBMC(LL)+WHPC(LL)+WHSC(LL)+WLMC(LL)+WLSC(LL)
          ZLSN=ZLSN+WLSN(LL)
          ZLMN=ZLMN+WLMN(LL)
          ZBMN=ZBMN+WBMN(LL)
          ZHSN=ZHSN+WHSN(LL)
          ZHPN=ZHPN+WHPN(LL)
          WON(LL)=WBMN(LL)+WHPN(LL)+WHSN(LL)+WLMN(LL)+WLSN(LL)
      END DO
      TWN=ZLSN+ZLMN+ZBMN+ZHSN+ZHPN
      TOC=ZLSC+ZLMC+ZBMC+ZHSC+ZHPC
      IF(ABL=='P')THEN
          ADD=0.
          DO I=1,LC
              ADD=ADD+UP1(I)
          END DO
          FTP=TAP+TOP+TMP+TP+TFOP+STDP+STDOP+ADD
          BAL=BTP-YP-QAP-VAP-YLP+VAR(62)+VAR(63)-FTP
          SM4=SM4+BAL
          IF(ABS(BAL)>.001)THEN
              WRITE(KW(1),6)IY,MO1,KDA,BTP,YP,QAP,VAP,VAR(62),VAR(63),YLP,&
              FTP,BAL,SM4
              WRITE(KW(1),5)IY,MO1,KDA,TAP,TOP,TMP,TP,TFOP,STDP,STDOP,ADD
          END IF
          BTP=SUM
          YLP=0.
      END IF
      IF(ABL=='C')THEN
          AD1=0.
          AD2=0.
          DO I=1,LC
              AD1=AD1+420.*STD(I)
              AD2=AD2+420.*DM(I)
          END DO
          TYC1=1000.*(TYC-TYC1)
          STDOC=420.*STDO
          FTC=TOC+AD1+AD2+STDOC
          BAL=BTCX-VAR(77)-VAR(75)-VAR(76)-VAR(74)+VAR(99)-VAR(97)+&
          VAR(65)-TYC1-FTC
          SMC=SMC+BAL
          PER=100.*BAL/FTC
          IF(ABS(BAL)>.01)THEN
              WRITE(KW(1),1)IY,MO1,KDA,BTCX,VAR(77),VAR(75),VAR(76),VAR(74),&
              VAR(99),VAR(65),VAR(97),TYC1,FTC,BAL,SMC,RFV,PER
              WRITE(KW(1),2)ZLSC,ZLMC,ZBMC,ZHSC,ZHPC,TOC,AD1,AD2,STDOC,SM99
          END IF
          BTCX=FTC
          TYC1=TYC
      END IF
      IF(ABL=='N')THEN
          ADD=0.
          TOT=0.
          DO I=1,LC
              ADD=ADD+UN1(I)
              TOT=TOT+STDN(I)
          END DO
          TYN1=TYN-TYN1
          SUM=ZN2OG+ZN2OL+ZNO2+ZNO3+ZNH3+TWN+TOT+STDON+ADD
          X1=VAR(49)
          IF(IDN>2)X1=VAR(89)
          BAL=BTNX+RNO3-VAR(43)-VAR(44)-VAR(45)-VAR(46)-X1-VAR(52)+VAR(59)+&
          VAR(60)+VAR(61)+DFX-VAR(98)-VAR(102)-VAR(103)-VAR(104)-VAR(105)-&
          VAR(106)-VAR(107)-TYN1-SUM+VAR(101)
	      SMN=SMN+BAL
	      IF(ABS(BAL)>.001)THEN
              WRITE(KW(1),3)IY,MO1,KDA,BTNX,RNO3,VAR(43),VAR(44),VAR(45),VAR(46),&
              X1,VAR(52),VAR(59),VAR(60),VAR(61),DFX,VAR(98),VAR(106),VAR(105),&
              VAR(102),VAR(107),VAR(104),VAR(103),TYN1,SUM,BAL,SMN
              WRITE(KW(1),4)ZN2OG,ZN2OL,ZN2O,ZNO2,ZNO3,ZNH3,ZLSN,ZLMN,ZBMN,&
              ZHSN,ZHPN,TOT,STDON,ADD,TWN
          END IF
          BTNX=SUM
          TYN1=TYN
      END IF
      ! X1=UK1(1)+UK1(2)
      ! SUM=TSK+TEK+TFK+STDK+STDOK+X1
      ! BAL=BTK-YK-QK-SSK-PRKK-CYLK+FSK-SUM
      ! WRITE(KW(1),125)IY,MO1,KDA,TSK,TEK,TFK,STDK,STDOK,X1,SUM
      ! WRITE(KW(1),125)IY,MO1,KDA,QK,YK,SSK,PRKK,FSK,CYLK,BAL
      ! BTK=SUM
      RETURN
    1 FORMAT(4X,'C XBAL ',3I4,2X,'BTOT=',E13.6,2X,'Y   =',E13.6,2X,'PRK =',&
      E13.6,2X,'Q   =',E13.6/5X,'RSPC=',E13.6,2X,'NPPC=',E13.6,2X,'FOC =',&
      E13.6,2X,'BURN=',E13.6,2X,'YLD =',E13.6/5X,'FTOT=',E13.6,2X,'BAL =',&
      E13.6,2X,'SMBL=',E13.6,2X,'RFV =',E13.6,2X,'PER =',E13.6)
    2 FORMAT(5X,'CONTENTS',T26,'ZLSC=',E13.6,2X,'ZLMC=',E13.6,2X,'ZBMC=',&
      E13.6,2X,'ZHSC=',E13.6/5X,'ZHPC=',E13.6,2X,'TOC =',E13.6,2X,'STD =',&
      E13.6,2X,'BIOM=',E13.6,2X,'STDO=',E13.6/5X,'SPPC',E13.6/)
    3 FORMAT(/4X,'N XBAL ',3I4,2X,'BTOT=',E13.6,2X,'PCP =',E13.6,2X,'Y   =',&
      E13.6,2X,'QNO3=',E13.6/5X,'SNO3=',E13.6,2X,'VNO3=',E13.6,2X,'DNN2=',&
      E13.6,2X,'VOL =',E13.6,2X,'FORG=',E13.6/5X,'FNO3=',E13.6,2X,'FNH3=',&
      E13.6,2X,'FIX =',E13.6,2X,'BURN=',E13.6,2X,'QNO2=',E13.6/5X,'VNO2=',&
      E13.6,2X,'SNO2=',E13.6,2X,'QN2O=',E13.6,2X,'VN2O=',E13.6,2X,'SN2O=',&
      E13.6/5X,'YLD =',E13.6,2X,'FTOT=',E13.6,2X,'BAL =',E13.6,2X,'SMBL=',&
      E13.6)
    4 FORMAT(/5X,'CONTENTS',T26,'N2OG=',E13.6,2X,'N2OL=',E13.6,2X,'N2O =',&
      E13.6,2X,'NO2 =',E13.6/5X,'NO3 =',E13.6,2X,'NH3 =',E13.6,2X,'LSN =',&
      E13.6,2X,'LMN =',E13.6,2X,'BMN =',E13.6/5X,'HSN =',E13.6,2X,'HPN =',&
      E13.6,2X,'STDN=',E13.6,2X,'SDON=',E13.6,2X,'UN1 =',E13.6/5X,'ORGN=',&
      E13.6/)                  
    5 FORMAT(/4X,'P XBAL ',3I4,2X,'BTOT=',E13.6,2X,'Y   =',E13.6,2X,'Q   =',&
      E13.6,2X,'PRK =',E13.6/5X,'FORG=',E13.6,2X,'FMN =',E13.6,2X,'YLD =',&
      E13.6,2X,'FTOT=',E13.6,2X,'BAL =',E13.6,2X,'SMBL=',E13.6)
    6 FORMAT(/5X,'CONTENTS',T26,'TAP =',E13.6,2X,'TOP =',E13.6,2X,'TP  =',&
      E13.6,2X,'TFOP=',E13.6/5X,'STDP=',E13.6,2X,'SDOP=',E13.6,2X,'UP  =',&
      E13.6)                  
      END