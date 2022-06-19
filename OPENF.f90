      SUBROUTINE OPENF
!     EPIC1102
!     THIS SUBPROGRAM OPENS FILES.
      USE PARM
      CHARACTER(4)::AXT
	  DIMENSION AXT(32)
	  DATA AXT/".OUT",".ACM",".SUM",".DHY",".DPS",".MFS",".MPS",".ANN",&
      ".SOT",".DTP",".MCM",".DCS",".SCO",".ACN",".DCN",".SCN",".DGN",&
      ".DWT",".ACY",".ACO",".DSL",".MWC",".ABR",".ATG",".MSW",".APS",&
      ".DWC",".DHS",".DGZ",".DNC",".ASL",".DDN"/
	  DO I=1,MSO-1
	      IF(AXT(I)/="    ".AND.KFL(I)>0)OPEN(KW(I),FILE=ASTN//AXT(I))
      END DO
      RETURN
      END