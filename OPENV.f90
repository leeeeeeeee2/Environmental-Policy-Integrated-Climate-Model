      SUBROUTINE OPENV(NUM,FNAM,ID,NMSO)
!     EPIC1102      
!     VERIFIES THE EXISTENCE OF A FILE BEFORE OPENING IT
      USE PARM
      CHARACTER(80)::FNAM,ADIR
	  CHARACTER(120)::FNM
	  LOGICAL::XMIS
	  JRT=0
	  SELECT CASE(ID)
	      CASE(1)	
	          ADIR='C:\WEATDATA\'
	      CASE(2)
	          ADIR='C:\SITE\'
	      CASE(3)
	          ADIR='C:\SOIL\'
	      CASE(4)
	          ADIR='C:\OPSC\'
	      CASE DEFAULT
              FNM=FNAM
              GO TO 1
	  END SELECT
	  FNM=ADJUSTR(ADIR)//ADJUSTL(FNAM)
	  FNM=ADJUSTL(FNM)	
    1 INQUIRE(FILE=FNM,EXIST=XMIS)
	  IF(XMIS==.TRUE.)THEN
	      OPEN(NUM,FILE=FNM)
	  ELSE
	      IF(IBAT==0)THEN
              WRITE(*,'(/A/)')'File '//TRIM(FNM)//' IS MISSING.'
              PAUSE
          ELSE
              WRITE(NMSO,'(A,A8,1X,A96,A)')' !!!!! ',ASTN,TRIM(FNM),&
              ' IS MISSING.'
              PAUSE
          END IF
	  END IF	
	  RETURN
      END