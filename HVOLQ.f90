      SUBROUTINE HVOLQ(IVR)
!     EPIC1102
!     THIS SUBPROGRAM PREDICTS DAILY RUNOFF VOLUME AND PEAK RUNOFF RATE
!     GIVEN DAILY PRECIPITATION AND SNOW MELT.
      USE PARM
      IF(IVR==0)THEN
          SUM=0.
          ADD=0.
          IF(LUN==35)THEN
              SCN=25400./CN0-254.
              CN=CN0
          ELSE
              II=NVCN+1
              SELECT CASE(II)
                  CASE(1)
                      XX=0.
                      DO JJ=1,NBSL
                          J=LID(JJ)
                          IF(Z(J)>1.)EXIT
                          ZZ=(Z(J)-XX)/Z(J)
                          SUM=SUM+(ST(J)-S15(J))*ZZ/(FC(J)-S15(J))
                          ADD=ADD+ZZ
                          XX=Z(J)
                      END DO
                      IF(JJ<=NBSL)THEN
                          ZZ=1.-XX
                          SUM=SUM+(ST(J)-S15(J))*ZZ/(FC(J)-S15(J))
                          ADD=ADD+ZZ
                      END IF
                      SUM=SUM/ADD
                      IF(SUM>0.)THEN
                          SUM=SUM*100.
                          SCN=SMX*(1.-SUM/(SUM+EXP(SCRP(4,1)-SCRP(4,2)*SUM)))
                      ELSE
                          SCN=SMX*(1.-SUM)**2
                      END IF
                      SCN=PRMT(81)*SCN
                  CASE(2)
                      DO JJ=1,NBSL
                          L=LID(JJ)
                          IF(Z(L)>1.)EXIT
                          SUM=SUM+ST(L)-S15(L)
                          ADD=ADD+FC(L)-S15(L)
                          L1=L
                      END DO
                      IF(JJ<=NBSL)THEN
                          RTO=(1.-Z(L1))/(Z(L)-Z(L1))
                          SUM=SUM+(ST(L)-S15(L))*RTO
                          ADD=ADD+(FC(L)-S15(L))*RTO
                      END IF
                      SUM=SUM/ADD
                      IF(SUM>0.)THEN
                          SUM=SUM*100.
                          SCN=SMX*(1.-SUM/(SUM+EXP(SCRP(4,1)-SCRP(4,2)*SUM)))
                      ELSE
                          SCN=SMX*(1.-SUM)**2
                      END IF
                  CASE(3)
                      DO JJ=1,NBSL
                          ISL=LID(JJ)
                          SUM=SUM+ST(ISL)-S15(ISL)
                          ADD=ADD+FC(ISL)-S15(ISL)
                          IF(Z(ISL)>1.)EXIT
                      END DO
                      SUM=SUM/ADD
                      IF(SUM<0.)THEN
                          SCN=SMX*(1.-SUM)**2
                      ELSE
                          RTO=MIN(.98,SUM)
                          SCN=SMX*(1.-RTO)
                      END IF
                  CASE(4)
                      SCN=25400./CN0-254.
                      CN=CN0
                  CASE(5)
                      SCN=MAX(3.,SCI)
              END SELECT
              IF(II/=4)THEN
                  SCN=(SCN-SMX)*EXP(PRMT(75)*(1.-RSD(LD1)))+SMX  
                  IF(STMP(LID(2))<-1.)SCN=SCN*PRMT(65)
                  IF(ICV>0)SCN=SCN*SQRT(1.-FCV) 
                  CN=25400./(SCN+254.)
                  IF(ISCN==0)THEN
                      UPLM=MIN(99.5,CN+5.)
                      BLM=MAX(1.,CN-5.)
                      CN=ATRI(BLM,CN,UPLM,8)
                  END IF
                  SCN=25400./CN-254.
                  IF(SCN<3.)THEN
                      SCN=3. 
                      CN=25400./(SCN+254.)
                  END IF
              END IF
          END IF    
          BB=.2*SCN
          TOT=100.
          DO I=1,9
              TOT=TOT-5.
              IF(CN>TOT)EXIT
          END DO
          CNDS(I)=CNDS(I)+1.
          RTO=MIN(1.,SCN/SMX)
          CRKF=PRMT(17)*RFV*RTO
          RFV=RFV-CRKF
          IF(RWO>0.)THEN
              SELECT CASE(INFL)
                  CASE(1)
                      X1=RWO-BB
                      IF(X1>0.)THEN
                          QD=X1*X1/(RWO+.8*SCN)
                      ELSE
                          QD=0.
                      END IF
                  CASE(2,3)
                      CALL HREXP
                  CASE(4)
                      CALL HRUNF
              END SELECT
          END IF    
      ELSE
          QD=EFI*RWO
      END IF    
      IF(IFD>0.AND.QD>0.)THEN
          IF(DHT>.01.AND.RGIN>.01)THEN
              CALL HFURD
	          X1=MAX(0.,ST(LD1)-PO(LD1))
              IF(NOP>0)WRITE(KW(1),17)IYR,MO,KDA,DHT,RHTT,QD,DV,X1,XHSM
              IF(QD<=DV-X1)THEN
                  QD=0.
              ELSE
                  DHT=0.
                  IF(IDRL==0.AND.CPHT(JJK)<1.)DHT=DKHL    
              END IF
          END IF
      END IF
      IF(ITYP==0)THEN
          X2=QD/DUR
          IF(X2>1.)THEN
              X2=X2**.25
          ELSE
              X2=1.
          END IF   
          X4=MIN(UPSL/360.,TCS/X2)
          TC=X4+TCC/X2
          ALTC=1.-EXP(-TC*PR)
	      X1=ALTC*QD
          QP=X1/TC
	      QPR=X1/X4
      ELSE
          TC=TCC+TCS/SQRT(RWO)
          QP=MIN(.95,BB/RWO)
          CALL HTR55
      END IF    
      IF(KFL(4)>0)WRITE(KW(4),27)IYR,MO,KDA,CN,RWO,QD,TC,QP,DUR,ALTC,AL5
      RETURN
   27 FORMAT(1X,3I4,9F10.2)
   17 FORMAT(1X,3I4,12X,'DHT=',F5.0,'MM',2X,'RHTT=',F5.0,'MM',2X,'Q='&
      ,F5.1,'MM',2X,'DV=',F5.1,'MM',2X,'ST=',F5.1,'MM',2X,'HUSC=',F6.2)
      END
