      SUBROUTINE AICL
!     EPIC1102
!     THIS SUBPROGRAM COMPUTES THE DAY OF THE MONTH, GIVEN THE MONTH AND
!     THE DAY OF THE YEAR.
      USE PARM
      KDA=JDA-NC(MO)
      IF(MO>2)KDA=KDA+NYD
	  RETURN
      END