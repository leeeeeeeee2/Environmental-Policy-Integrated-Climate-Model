      SUBROUTINE CGROW(JRT)
!     EPIC1102
!     THIS SUBPROGRAM CALCUALTES LEAF AREA INDEX, HEAT UNITS, ROOT DEPTH
!     AND TEMPERATURE STRESS FOR THE.
      USE PARM
      DIMENSION SLA0(12)
      JRT=0
      X1=DM(JJK)+1.E-10
      CPR=UP1(JJK)/X1
      CNR=UN1(JJK)/X1
      CKR=MAX(1.E-5,UK1(JJK)/X1)
      AJWA(JJK)=1.
      XPHU=PHU(JJK,IHU(JJK))
      X4=TOPC(JJK)-TBSC(JJK)
      TGX=TX-TBSC(JJK)
      HU(JJK)=HU(JJK)+MAX(0.,TGX)
      IF(JDA==JDHU.AND.IDC(JJK)/=NDC(7).AND.IDC(JJK)/=NDC(8).AND.IDC&
      (JJK)/=NDC(10).AND.IDC(JJK)/=NDC(4))THEN
          HU(JJK)=XPHU*PRMT(19)
          PSTS=MIN(0.,PSTS)
          IPST=0
      END IF
      HUI(JJK)=HU(JJK)/XPHU
      IF(HU(JJK)>XPHU)THEN
          WCYD=MAX(WCY(JJK),WCYD-EO*.002)
          IF(IDC(JJK)==NDC(3).OR.IDC(JJK)==NDC(6))THEN
              HU(JJK)=0.
              JRT=2
          ELSE
              IF(IDC(JJK)==NDC(7).OR.IDC(JJK)==NDC(8).OR.IDC(JJK)==NDC(10))THEN
                  JRT=0
                  HU(JJK)=.9*HU(JJK)
              ELSE
                  JRT=1
              END IF
          END IF
          RETURN
      END IF
      F2=HUI(JJK)/(HUI(JJK)+EXP(DLAP(1,JJK)-DLAP(2,JJK)*HUI(JJK)))
	  IF(IDC(JJK)==NDC(7).OR.IDC(JJK)==NDC(8).OR.IDC(JJK)==NDC(10))THEN
          X1=HSM/AHSM
	      F1=X1/(X1+EXP(DLAP(1,JJK)-DLAP(2,JJK)*X1))
	      F=F1
	      XLAI(JJK)=MAX(.1,DMLX(JJK)*HUI(JJK)/(HUI(JJK)+EXP(DLAP(1,JJK)-&
          DLAP(2,JJK)*HUI(JJK))))
	  ELSE
	      F=F2 
	  END IF
	  IF(IDC(JJK)==NDC(8).OR.IDC(JJK)==NDC(7).OR.IDC(JJK)==NDC(10))THEN
	      F3=SQRT(HUI(JJK)+1.E-10)
	  ELSE    
	      F3=SQRT(F2+1.E-10)
	  END IF
	  FF=F-WLV(JJK)
      XX=FF*XLAI(JJK)
      X2=1.
      SLAX=0.
	  X3=(SLAI(JJK)+.001)*CPHT(JJK)
      IF(IGO==1)THEN
          SUM=X3
          TOT=STL(JJK)
          ADD=SLAI(JJK)
      ELSE    
          SUM=0.
          TOT=0.
          ADD=0.
          DO I=1,IGO
              K1=JE(I)
              IF(K1>MNC)CYCLE
              IF(SLAI(K1)>SLAX)SLAX=SLAI(K1)
              SUM=SUM+SLAI(K1)*CPHT(JJK)
              TOT=TOT+STL(K1)
              ADD=ADD+SLAI(K1)
          END DO
          IF(SLAX>2.)X2=X3/SUM
      END IF
      IF(XX>0.)THEN
          X1=XX*X2*(1.+HR1)**PRMT(70)
	      IF(IDC(JJK)/=NDC(7).AND.IDC(JJK)/=NDC(8).AND.IDC(JJK)/=NDC&
          (10))X1=X1*SQRT(REG(JJK))*SHRL
          SLAI(JJK)=MIN(XLAI(JJK),SLAI(JJK)+X1)
      END IF
      WLV(JJK)=F
      RTO=TGX/X4
      IF(TGX>0..AND.RTO<2.)THEN	
	      REG(JJK)=SIN(1.5707*RTO)
	  ELSE
          REG(JJK)=0.
      END IF
      IF(SLAI(JJK)<.001)THEN
          SLAI(JJK)=.001
      ELSE
          !CPHT(JJK)=MAX(CPHT(JJK),HMX(JJK)*F3)
          FF=MAX(0.,F3-WCHT(JJK))
          CPHT(JJK)=MIN(HMX(JJK),CPHT(JJK)+FF*HMX(JJK))
          IF(HUI(JJK)>XDLAI(JJK).AND.HRLT>WDRM.AND.XDLA0(JJK)>0.)THEN
              XX=(1.-HUI(JJK))/XDLA0(JJK)
              IF(XX>1.E-5)THEN
                  XX=LOG10(XX)
              ELSE
                  XX=-5.
              END IF
              IF(IDC(JJK)/=NDC(7).AND.IDC(JJK)/=NDC(8).AND.IDC(JJK)/=NDC(10)&
              )THEN
                  RTO=RLAD(JJK)*XX
                  IF(RTO<-10.)RTO=-10.
                  X1=SLA0(JJK)*10.**RTO
                  IF(X1<SLAI(JJK))THEN
                      DLAX=SLAI(JJK)-X1
                      SLAI(JJK)=X1
                      SLAI(JJK)=MAX(.001,X1)
                      X2=DLAX*STL(JJK)*FLSL(JJK)
                      XZ=MIN(UN1(JJK),1000.*CNY(JJK)*X2)
                      XY=MIN(UP1(JJK),1000.*CPY(JJK)*X2)
                      W1=1000.*CKY(JJK)*X2
                      IF(X2>0.)CALL NCNSTD(X2,XZ,0)
                      FOP(LD1)=FOP(LD1)+XY
                      UN1(JJK)=UN1(JJK)-XZ
                      UP1(JJK)=UP1(JJK)-XY
                  END IF    
              END IF
              RTO=RBMD(JJK)*XX
              IF(RTO<-10.)RTO=-10.
              AJWA(JJK)=10.**RTO
          ELSE
              XDLA0(JJK)=1.-XDLAI(JJK)
              SLA0(JJK)=SLAI(JJK)
          END IF              
      END IF
      WCHT(JJK)=F3
      XX=MAX(CPHT(JJK),RD(JJK),2.5*RDMX(JJK)*HUI(JJK))
      RD(JJK)=MIN(RDMX(JJK),Z(LID(NBSL)),XX)
      FGC=ADD/(ADD+EXP(SCRP(23,1)-SCRP(23,2)*ADD))
      FGSL=TOT/(TOT+EXP(SCRP(26,1)-SCRP(26,2)*TOT)) 
      CLG=BLG(3,JJK)*HUI(JJK)/(HUI(JJK)+EXP(BLG(1,JJK)-BLG(2,JJK)*&
      HUI(JJK)))
      SHRL=1.
      FHR=0.
      IF(HRLT+1.E-5>WDRM)THEN
          SRA=SRA+SRAD
          GSVP=GSVP+VPD
      ELSE
          SHRL=0.
          FHR=1.-HRLT/WDRM
      END IF
      !IF(IDC(JJK)/=NDC(7))THEN
          IF(TMN>-1.)THEN
              IF(SHRL>0.)GO TO 15
              F=FHR
          ELSE
              XX=ABS(TMN)
              F=XX/(XX+EXP(FRST(1,JJK)-FRST(2,JJK)*XX))
              F=MAX(F,FHR)
          END IF
          IF(STL(JJK)>0..AND.STL(JJK)<DM(JJK).AND.IDC(JJK)/=NDC(7).AND.IDC(JJK)/=NDC(8).AND.IDC(JJK)/=NDC(10))THEN
              STL(JJK)=MAX(0.,DM(JJK)-RW(JJK))
              XX=F*STL(JJK)
              STL(JJK)=STL(JJK)-XX
              DM(JJK)=RW(JJK)+STL(JJK)
              STD(JJK)=STD(JJK)+XX
              STDL=STDL+CLG*XX
              XY=XX*CNR
              XZ=XX*CPR
              XW=XX*CKR
              XUN=UN1(JJK)
              IF(XUN-XY<.01)XY=XUN-.01
              XUP=UP1(JJK)
              IF(XUP-XZ<.01)XZ=XUP-.01
              STDK=STDK+XW
              STDN(JJK)=STDN(JJK)+XY
              STDP=STDP+XZ
              !UK1(JJK)=UK1(JJK)-XW
              UN1(JJK)=XUN-XY
              UP1(JJK)=XUP-XZ
          END IF
          IF(IDC(JJK)==NDC(7))F=F*F
          SLAI(JJK)=MAX(.001,SLAI(JJK)*(1.-F))
      !END IF
   15 IF(REG(JJK)>0.)RETURN
      N1=NCP(JJK)
      SF(5,N1,JJK)=SF(5,N1,JJK)+1.
      SFMO(5,JJK)=SFMO(5,JJK)+1.
      CGSF(5,JJK)=1.
      BLYN(JJK)=BLYN(JJK)+1.
      SMMC(17,JJK,MO)=SMMC(17,JJK,MO)+1.
      VARC(17,JJK)=0.
      JRT=1
      RETURN
      END