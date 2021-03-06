      SUBROUTINE HREXP
!     EPIC1102
!     THIS SUBPROGRAM DISTRIBUTES DAILY RAINFALL EXPONENTIALLY &
!     FURNISHES THE GREEN & AMPT SUBPROGRAM RAIN INCREMENTS OF EQUAL
!     VOLUME = DRFV
      USE PARM
      DATA DRFV/2./
      PT=0.
      UPLM=.95
      QMN=.25
      BLM=.05
      R1=ATRI(BLM,QMN,UPLM,8)
      RTP=R1*RFV
      XK1=R1/4.605
      XK2=XK1*(1.-R1)/R1
      DUR=RFV/(REP*(XK1+XK2))
      X1=REP*DUR
      XKP1=XK1*X1
      XKP2=XK2*X1
      RX=0.
      PT=0.
      WRITE(KW(4),1)IYR,MO,KDA,SCN,XK1,XK2,RFV,RTP,REP,DUR,R1
      DO WHILE(PT<RTP)
          PT=PT+DRFV
          RX=REP*(1.-(RTP-PT)/XKP1)
          CALL HGASP(DRFV,PT,Q1,RX)
      END DO
      A=RTP-PT+DRFV
      PT=RTP
      RX=REP
      CALL HGASP(A,PT,Q1,RX)
      DO WHILE(PT<RFV)
          PT=PT+DRFV
          RX=REP*(1.-(PT-RTP)/XKP2)
          CALL HGASP(DRFV,PT,Q1,RX)
      END DO
      RX=0.
      A=RFV-PT
      PT=RFV
      CALL HGASP(A,PT,Q1,RX)
      RETURN
    1 FORMAT(1X,3I4,9F10.2)
      END