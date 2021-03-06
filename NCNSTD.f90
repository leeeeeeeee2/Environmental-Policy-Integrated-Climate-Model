      SUBROUTINE NCNSTD(X1,X2,LCD)
!     THIS SUBPROGRAM REMOVES C AND N FROM STANDING RESIDUE AND ADDS IT
!     TO THE TOP SOIL LAYER AS A RESULT OF A TILLAGE OPERATION.
      USE PARM
      IF(LCD>0)THEN
          JSL=ISL
          BX1=.1
      ELSE
          JSL=LD1
          BX1=.05
      END IF
      !XZ=WNO3(JSL)+WNH3(JSL)
      !AD1=WLSN(JSL)+WLMN(JSL)+WBMN(JSL)+WHSN(JSL)+WHPN(JSL)+XZ+X2
      CNRTO=(WLSC(JSL)+WLMC(JSL))/(WLSN(JSL)+WLMN(JSL)+1.E-5)
      IF(CNRTO>=10.)THEN
          Y1=BX1*WNO3(JSL)
          Y2=BX1*WNH3(JSL)
          X2=X2+Y1+Y2
          WNO3(JSL)=WNO3(JSL)-Y1
          WNH3(JSL)=WNH3(JSL)-Y2
      END IF    
      RLN=1000.*STDL/(STDN(JJK)+1.E-5)
      RLR=STDL/(STD(JJK)+1.E-5)
      IF(RLR>.8)THEN
          RLR=.8
      ELSE
          IF(RLR<.1)RLR=.1
      END IF        
      X7=1000.*X1
      C7=.42*X7
      SMS(7,JSL)=SMS(7,JSL)+C7
      SMM(73,MO)=SMM(73,MO)+C7
      VAR(73)=VAR(73)+C7
      RCN=C7/(X2+1.E-5)
      X8=.85-.018*RLN
      IF(X8<.01)THEN
          X8=.01
      ELSE
          IF(X8>.7)X8=.7
      END IF
      XX=X7*X8
      WLM(JSL)=WLM(JSL)+XX
      XZ=X7-XX
      WLS(JSL)=WLS(JSL)+XZ
      WLSL(JSL)=WLSL(JSL)+XZ*RLR
      X6=X2
	  SMM(86,MO)=SMM(86,MO)+X6
      XY=.42*XZ
      WLSC(JSL)=WLSC(JSL)+XY
      WLSLC(JSL)=WLSLC(JSL)+XY*RLR
      WLSLNC(JSL)=WLSC(JSL)-WLSLC(JSL)
      X3=MIN(X6,XY/150.)
	  WLSN(JSL)=WLSN(JSL)+X3
      WLMC(JSL)=WLMC(JSL)+.42*XX
      WLMN(JSL)=WLMN(JSL)+X6-X3
      RSD(JSL)=.001*(WLS(JSL)+WLM(JSL))
      !XZ=WNO3(JSL)+WNH3(JSL)
      !AD2=WLSN(JSL)+WLMN(JSL)+WBMN(JSL)+WHSN(JSL)+WHPN(JSL)+XZ
      !DF=AD2-AD1
      !IF(ABS(DF)>.001)WRITE(KW(1),1)IY,MO,KDA,AD1,AD2,DF
    !1 FORMAT(1X,'NCNSTD',3I4,3E16.6)      
      RETURN
      END