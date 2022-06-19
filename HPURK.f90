      SUBROUTINE HPURK
!     EPIC1102
!     THIS SUBPROGRAM IS THE MASTER PERCOLATION COMPONENT.  IT
!     MANAGES THE ROUTING PROCESS
      USE PARM
      DIMENSION STPT(5)
      DATA STPT/.9,.75,.5,.25,.1/
      ADD=0.
      SEP=RFV-QD
      DO KK=1,NBSL
          ISL=LID(KK)
          ST(ISL)=ST(ISL)+SEP
          IF(WTBL<=Z(ISL))THEN
              SSF(ISL)=0.
              PKRZ(ISL)=0.
              SEP=0.
          ELSE
              IF(IPRK==0)THEN
                  CALL HPERC
              ELSE    
                  CALL HPERC1
              END IF    
              ST(ISL)=ST(ISL)-SEP-SST
              SSF(ISL)=SST
              IF(ISL==IDR)THEN
                  SMM(18,MO)=SMM(18,MO)+SST
                  VAR(18)=SST
              END IF
              PKRZ(ISL)=SEP
              ADD=ADD+SST
          END IF    
      END DO
      SST=ADD
      L1=LD1
      DO K=NBSL,2,-1
          ISL=LID(K)
          L1=LID(K-1)
          XX=ST(ISL)-FC(ISL)
          IF(XX>0.)THEN
              X1=VGN(L1)/(VGN(L1)-1.)
              X2=ST(L1)-S15(L1)
              IF(X2>0.)THEN
                  RTO=(PO(L1)-S15(L1))/X2
                  X3=RTO**X1-1.
                  IF(X3>0.)THEN
                      T1=(X3/VGA(L1)**VGN(L1))**(1./VGN(L1))
                      T1=T1/10.19
                  ELSE
                      T1=1.
                  END IF
              ELSE
                  T1=1500.
              END IF    
              ZH=10.*(Z(ISL)-Z(L1))
              X1=VGN(ISL)/(VGN(ISL)-1.)
              X2=ST(ISL)-S15(ISL)
              IF(X2>0.)THEN
                  RTO=(PO(ISL)-S15(ISL))/X2
                  X3=RTO**X1-1.
                  IF(X3>0.)THEN
                      T2=(X3/VGA(ISL)**VGN(ISL))**(1./VGN(ISL))
                      T2=T2/10.19
                  ELSE
                      T2=1.
                  END IF
              ELSE
                  T2=1500.
              END IF    
              T2=T2+ZH
              IF(T1<T2)CYCLE
              X1=XX*MIN(.1,(T1-T2)/(T1+T2))
              SLTP(ISL)=SLTP(ISL)+X1
              ST(L1)=ST(L1)+X1
              PKRZ(L1)=PKRZ(L1)-X1
              ST(ISL)=ST(ISL)-X1
          END IF    
      END DO
      DO K=1,NBSL
          ISL=LID(K)
          IF(ST(ISL)>FC(ISL))THEN
              POFC=PO(ISL)-FC(ISL)
              RTO=(ST(ISL)-FC(ISL))/POFC
              DO J=1,5
                  IF(RTO>STPT(J))EXIT
              END DO
              NPT(J,ISL)=NPT(J,ISL)+1
          END IF    
      END DO
      IF(PKRZ(L1)<0.)PKRZ(L1)=0.
      RETURN
      END