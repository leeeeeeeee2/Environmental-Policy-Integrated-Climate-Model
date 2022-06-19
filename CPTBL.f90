      SUBROUTINE CPTBL
!     EPIC1102
!     THIS SUBPROGRAM READS CROP TABLE TO DETERMINE CROP PARAMETERS
      USE PARM
      CHARACTER(12)::ADUM
      DIMENSION XTP(58)
      IF(NSTP>0)GO TO 174
      IF(LC==0)GO TO 174
      DO L=1,LC
          IF(KDC(L)==JX(6))GO TO 179
      END DO
  174 LC=LC+1
      KDC1(JX(6))=LC
      KDC(LC)=JX(6)
      XMTU(LC)=JX(7)
!     READ CROP DATA
!     CPNM = NAMES OF CROPS IN CROP PARAMETER TABLE
!  1  WA   = BIOMASS/ENERGY (kg/ha/MJ)(FOR CO2 = 330 ppm
!  2  HI   = HARVEST INDEX (CROP YIELD/ABOVE GROUND BIOMASS)
!  3  TOPC = OPTIMAL TEMP FOR PLANT GROWTH (C)
!  4  TBSC = MIN TEMP FOR PLANT GROWTH (C)
!  5  DMLA = MAX LEAF AREA INDEX
!  6  DLAI = FRACTION OF GROWING SEASON WHEN LEAF AREA STARTS DECLINING
!  7  DLAP(= LAI DEVELOPMENT PARMS--NUMBERS BEFORE DECIMAL = %
!  8   1,2) OF GROWING SEASON .  NUMBERS AFTER DDECIMAL = FRACTION OF
!            DMLA AT GIVEN %.
!  9  RLAD = LAI DECLINE RATE FACTOR.
! 10  RBMD = WA  DECLINE RATE FACTOR.
! 11  ALT  = ALUMINUM TOLERANCE INDEX(1-5)1=SENSITIVE, 5=TOLERANT
! 12  GSI  = MAX STOMATAL CONDUCTANCE (DROUGTH TOLERANT PLANTS HAVE
!            LOW VALUES.)
! 13  CAF  = CRITICAL AERATION FACTOR (SW/POR > CAF CAUSES AIR STRESS)
! 14  SDW  = SEEDING RATE (kg/ha)
! 15  HMX  = MAX CROP HEIGHT (m)
! 16  RDMX = MAX ROOT DEPTH (m)
! 17  WAC2 = NUMBER BEFORE DECIMAL = CO2 CONC IN FUTURE ATMOSPHERE
!            (ppm).  NUMBER AFTER DECIMAL = RESULTING WA VALUE.
! 18  CNY  = N FRACTION OF YIELD (KG/KG)
! 19  CPY  = P FRACTION OF YIELD (KG/KG)
! 20  CKY  = K FRACTION OF YIELD (KG/KG)
! 21  WSYF = LOWER LIMIT OF HARVEST INDEX
! 22  PST  = PEST(INSECTS,WEEDS,AND DISEASE)FACTOR (0-1)
! 23  CSTS = SEED COST ($/KG)
! 24  PRYG = PRICE FOR SEED YIELD ($/t)
! 25  PRYF = PRICE FOR FORAGE YIELD ($/t)
! 26  WCY  = FRACTION WATER IN YIELD.
!27-29BN   = N FRACTION IN PLANT WHEN GROWTH IS 0.,.5,1.0
!30-32BP   = P FRACTION IN PLANT WHEN GROWTH IS 0.,.5,1.
!33-35BK   = K FRACTION IN PLANT WHEN GROWTH IS 0.,.5,1.
!36-38BW   = WIND EROSION FACTORS FOR STANDING LIVE, STANDING DEAD,
!            AND FLAT RESIDUE
! 39  IDC  = CROP ID NUMBERS. USED TO PLACE CROPS IN CATEGORIES
!            (1 FOR WARM SEASON ANNUAL LEGUME
!             2 FOR COLD SEASON ANNUAL LEGUME
!             3 FOR PERENNIAL LEGUME
!             4 FOR WARM SEASON ANNUAL
!             5 FOR COLD SEASON ANNUAL
!             6 FOR PERENNIAL
!             7 FOR EVERGREEN TREES
!             8 FOR DECIDUOUS TREES
!             9 FOR COTTON
!            10 FOR DECIDUOUS TREES (LEGUME)
! 40/ FRST(= FROST DAMAGE PARMS--NUMBERS BEFORE DECIMAL = MIN TEMP(deg C
! 41   1,2) NUMBERS AFTER DECIMAL = FRACTION YLD LOST WHEN GIVEN MIN TE
!            IS EXPERIENCED.
! 42  WAVP = PARM RELATING VAPOR PRESSURE DEFFICIT TO WA
! 43  VPTH = THRESHOLD VPD (kpa)(F=1.
! 44  VPD2 = NUMBER BEFORE DECIMAL = VPD VALUE (kpa).  NUMBER AFTER
!            DECIMAL = F2 < 1.
! 45  RWPC1= ROOT WEIGHT/BIOMASS PARTITIONING COEF
! 46  RWPC2= ROOT WEIGHT/BIOMASS PARTITIONING COEF
! 47  GMHU = HEAT UNITS REQUIRED FOR GERMINATION
! 48  PPLP1= PLANT POP PARM--NUMBER BEFORE DECIMAL = # PLANTS
!            NO AFTER DEC = FRACTION OF MAX LAI
! 49  PPLP2= 2ND POINT ON PLANT POP-LAI CURVE. PPLP1<PPLP2--PLANTS/M2
!                                              PPLP1>PPLP2--PLANTS/HA
! 50  STX1 = YLD DECREASE/SALINITY INCREASE ((t/ha)/(MMHO/CM))
! 51  STX2 = SALINITY THRESHOLD (MMHO/CM)
! 52  BLG1 = LIGNIN FRACTION IN PLANT AT .5 MATURITY
! 53  BLG2 = LIGNIN FRACTION IN PLANT AT MATURITY
! 54  WUB  = WATER USE CONVERSION TO BIOMASS(T/MM)
! 55  FTO  = FRACTION TURNOUT (COTTON LINT/STRIPPER YLD)
! 56  FLT  = FRACTION LINT (COTTON LINT/PICKER YLD)
! 57  CCEM = CARBON EMISSION/SEED WEIGHT(KG/KG)
! 58  FLSL = FRACTION OF LEAF WEIGHT TO STL WEIGHT AT DLAI
      READ(KR(4),1)ADUM
	  READ(KR(4),1)ADUM
	  J2=-1
	  DO WHILE(J2/=JX(6))
          READ(KR(4),333,IOSTAT=NFL)J2,CPNM(LC),(XTP(L),L=1,58)
          IF(NFL/=0)THEN
              IF(IBAT==0)THEN
                  WRITE(*,*)'CROP NO = ',JX(6),' NOT IN CROP LIST FILE'
                  PAUSE
              ELSE
	              WRITE(KW(MSO),'(A,A8,A,I4,A)')' !!!!! ',ASTN,&
	              &'CROP NO = ',JX(6),' NOT IN CROP LIST FILE'
	          END IF
              PAUSE	              
	      END IF       
      END DO
      WA(LC)=XTP(1)
      HI(LC)=XTP(2)
      TOPC(LC)=XTP(3)
      TBSC(LC)=XTP(4)
      DMLA(LC)=XTP(5)
      DLAI(LC)=XTP(6)
      DLAP(1,LC)=XTP(7)
      DLAP(2,LC)=XTP(8)
      RLAD(LC)=XTP(9)
      RBMD(LC)=XTP(10)
      ALT(LC)=XTP(11)
      GSI(LC)=XTP(12)
      CAF(LC)=XTP(13)
      SDW(LC)=XTP(14)
      HMX(LC)=XTP(15)
      RDMX(LC)=XTP(16)
      WAC2(2,LC)=XTP(17)
      CNY(LC)=XTP(18)
      CPY(LC)=XTP(19)
      CKY(LC)=XTP(20)
      WSYF(LC)=XTP(21)
      PST(LC)=XTP(22)
      CSTS(LC)=XTP(23)
      PRYG(LC)=XTP(24)
      PRYF(LC)=XTP(25)
      WCY(LC)=XTP(26)
      BN(1,LC)=XTP(27)
      BN(2,LC)=XTP(28)
      BN(3,LC)=XTP(29)
      BP(1,LC)=XTP(30)
      BP(2,LC)=XTP(31)
      BP(3,LC)=XTP(32)
      BK(1,LC)=XTP(33)
      BK(2,LC)=XTP(34)
      BK(3,LC)=XTP(35)
      BWD(1,LC)=XTP(36)
      BWD(2,LC)=XTP(37)
      BWD(3,LC)=XTP(38)
      IDC(LC)=XTP(39)
      FRST(1,LC)=XTP(40)
      FRST(2,LC)=XTP(41)
      WAVP(LC)=XTP(42)
      VPTH(LC)=XTP(43)
      VPD2(LC)=XTP(44)
      RWPC(1,LC)=XTP(45)
      RWPC(2,LC)=XTP(46)
      GMHU(LC)=XTP(47)
      PPLP(1,LC)=XTP(48)
      PPLP(2,LC)=XTP(49)
      STX(1,LC)=XTP(50)
      STX(2,LC)=XTP(51)
      BLG(1,LC)=XTP(52)
      BLG(2,LC)=XTP(53)
      WUB(LC)=XTP(54)
      FTO(LC)=XTP(55)
      FLT(LC)=XTP(56)
      CCEM(LC)=XTP(57)
      FLSL(LC)=XTP(58)
      REWIND KR(4)
  179 JJK=KDC1(JX(6))
      IHU(JJK)=IHU(JJK)+1
      PHU(JJK,IHU(JJK))=OPV(1)
      IF(XMTU(JJK)>0)PHU(JJK,IHU(JJK))=CAHU(1,365,TBSC(JJK),0)*XMTU(JJK)
      Y1=PPLP(1,JJK)
      Y2=PPLP(2,JJK)
      IF(Y2>Y1)THEN
          X4=Y1
          X5=Y2
      ELSE
          X4=Y2
          X5=Y1
      END IF   
      X1=ASPLT(X4)
      X2=ASPLT(X5)
	  CALL ASCRV(X4,X5,X1,X2)
      PPCF(1,JJK)=X4
      PPCF(2,JJK)=X5
	  IF(OPV(5)>0.)THEN
	      X3=OPV(5)
	  ELSE
	      G1=X2
	      DO IT=1,10
     	      Z1=EXP(X4-X5*G1)
	          Z2=G1+Z1
              FU=G1/Z2-.9
              IF(ABS(FU)<1.E-5)EXIT
	          DFDU=Z1*(1.+X5*G1)/(Z2*Z2)
	          G1=G1-FU/DFDU
	      END DO
          IF(IT>10)WRITE(KW(1),5)
          X3=G1
      END IF
      PPLA(JJK,IHU(JJK))=DMLA(JJK)*X3/(X3+EXP(X4-X5*X3))
      POP(JJK,IHU(JJK))=X3
      IF(OPV(6)>0.)FMX=OPV(6)
      FNMX(JJK)=FMX
      RETURN
    1 FORMAT(A12)
    5 FORMAT(5X,'!!!!! PLANT POP DID NOT CONVERGE')
  333 FORMAT(1X,I4,1X,A4,100F8.0)
      END