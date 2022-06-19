      SUBROUTINE NITVOL(Z5)
!     EPIC1102
!     THIS SUBPROGRAM SIMULATES THE TRANSFORMATION FROM NH3 TO NO3, AND
!     THE VOLATILIZATION OF NH3 USING MODIFIED METHODS OF REDDY AND OF
!     THE CERES MODEL
      USE PARM
!     AD1=WNO3(ISL)+WNH3(ISL)
      X1=.41*(STMP(ISL)-5.)
      IF(X1<=0.)RETURN
      IF(ISL==LD1)THEN
          FAF=.335+.16*LOG(U10+.2)
          AKAV=X1*FAF
      ELSE
          FCEC=MAX(.3,1.-.038*CEC(ISL))
          FZ=1.-Z5/(Z5+EXP(SCRP(12,1)-SCRP(12,2)*Z5))
          AKAV=X1*FCEC*FZ
      END IF
      IF(PH(ISL)>7.)THEN
          IF(PH(ISL)>7.4)THEN
              FPH=5.367-.599*PH(ISL)
          ELSE
              FPH=1.
          END IF
      ELSE
          FPH=.307*PH(ISL)-1.269
      END IF
      AKAN=X1*SUT(ISL)*FPH
      AKAV=AKAV*SUT(ISL)
      XX=AKAV+AKAN
      IF(XX<1.E-5)RETURN
      F=MIN(PRMT(64),1.-EXP(-XX))
      X2=F*WNH3(ISL)
      AVOL=X2*PRMT(57)
      SVOL=SVOL+AVOL
      IF(NTV==0)THEN
          RNIT=X2-AVOL
          WNH3(ISL)=WNH3(ISL)-AVOL-RNIT
          WNO3(ISL)=WNO3(ISL)+RNIT
          SNIT=SNIT+RNIT
      ELSE
          WNH3(ISL)=WNH3(ISL)-AVOL
          F=MIN(1.,PRMT(64)*AKAN)
          GNO2=F*WNH3(ISL)
          WNH3(ISL)=WNH3(ISL)-GNO2
          WNO2(ISL)=WNO2(ISL)+GNO2
          IF(PH(ISL)>5.5)THEN
              IF(PH(ISL)>7.2)THEN
                  FPH=4.367-.5324*PH(ISL)
              ELSE
                  FPH=1.
              END IF
          ELSE
              FPH=.307*PH(ISL)-1.269
          END IF
          AKAN=X1*SUT(ISL)*FPH
          F=MIN(1.,PRMT(64)*AKAN)
          GNO3=F*WNO2(ISL)
          WNO2(ISL)=WNO2(ISL)-GNO3
          WNO3(ISL)=WNO3(ISL)+GNO3
          SNIT=SNIT+GNO2
      END IF    
      !XZ=WNO3(ISL)+WNH3(ISL)
      !AD2=XZ+AVOL
      !DF=AD2-AD1
      !IF(ABS(DF)>.001)WRITE(KW(1),1)IY,MO,KDA,AD1,AD2,DF
      !1 FORMAT(1X,'NITVOL',3I4,3E16.6)      
      RETURN
      END