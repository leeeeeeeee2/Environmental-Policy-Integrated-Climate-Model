      SUBROUTINE HRFEI(IVR)
!     EPIC1102
!     THIS SUBPROGRAM ESTIMATES THE USLE RAINFALL ENERGY FACTOR, GIVEN
!     DAILY RAINFALL.
      USE PARM
      DATA ALMN/.02083/
      IF(IVR==0)THEN
          AJP=1.-EXP(-125./(RFV+5.))
          AL5=ATRI(ALMN,WI(NWI,MO),AJP,4)
          X1=-2.*LOG(1.-AL5)
          PR=2.*RFV*AL5+.001
          IF(REP<1.E-5)REP=X1*RFV+.001
          DUR=MIN(24.,4.605/X1)
      ELSE
          REP=RFV/24.
          DUR=24.
          PR=REP
          X1=PR
      END IF    
      EI=MAX(0.,RFV*(12.1+8.9*(LOG10(REP)-.4343))*PR/1000.)
      PR=X1
      RETURN
      END