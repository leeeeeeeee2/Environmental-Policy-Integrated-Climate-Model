      SUBROUTINE HGASP(A,PT,Q1,RX)
!     EPIC1102
!     THIS SUBPROGRAM SOLVES THE GREEN & AMPT INFILTRATION EQ ASSUMING
!     F1 IS INCREMENTED BY TOTAL RAIN DURING DT
      USE PARM
      F1=PT-QD
      X1=SATK
      IF(STMP(LID(2))<-1.)X1=.01*X1
      ZI=X1*(SCN/F1+1.)
      IF(RX>ZI)THEN
          Q1=A*(RX-ZI)/RX
      ELSE
          Q1=0.
      END IF
      QD=QD+Q1
      RETURN
      END