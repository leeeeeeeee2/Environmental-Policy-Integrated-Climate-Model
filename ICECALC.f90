      SUBROUTINE ICECALC(IFW,SOILTEMP,VOLWC,TOTPOR)
      ! CORRECT VOLWC FOR FREEZING WHEN TEMPERATURE IS BELOW 0C.  ICE
      ! FRACTION IS ASSUMED TO BE A LINEAR RAMP BETWEEN 0C AND -8C.  WE
      ! SET A FLOOR OF 1.0E-8 ON THE RESIDUAL WATER FRACTION IN ORDER TO
      ! PREVENT DIVIDE-BY-ZERO PROBLEMS AND TO ENSURE THAT THIS FUNCTION
      ! IS INVERTIBLE.
      USE PARM
      DIMENSION SOILTEMP(NBCL),VOLWC(NBCL),TOTPOR(NBCL)
      DO J=1,NBCL
          IF(SOILTEMP(J)<0.)THEN
              ! SET A FLOOR ON FAC TO AVOID POSSIBLE DIVIDE-BY-ZERO AND TO ENSURE RELATION IS INVERTIBLE
              FAC=MAX(1.+.125*SOILTEMP(J),1.0E-8)
          ELSE
              FAC=1.
          END IF
          IF(IFW==0)THEN
              TOTPOR(J)=TOTPOR(J)-(1.-FAC)*VOLWC(J) ! REDUCE TOTAL POROSITY BY THE VOLUME OF THE ICE COMPONENT
              VOLWC(J)=VOLWC(J)*FAC
          ELSE
              VOLWC(J)=VOLWC(J)/FAC
              TOTPOR(J)=TOTPOR(J)+(1.0-FAC)*VOLWC(J) ! ADD THE ICE BACK TO THE TOTAL
          END IF
      END DO
      END