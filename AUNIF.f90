      FUNCTION AUNIF(J)
!     EPIC1102
      USE PARM
      K=IX(J)/127773
      IX(J)=16807*(IX(J)-K*127773)-K*2836
      IF(IX(J)<0)IX(J)=IX(J)+2147483647
      AUNIF=IX(J)*4.656612875D-10
      RETURN
      END