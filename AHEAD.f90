      SUBROUTINE AHEAD
!     THIS SUBPROGRAM SETS HEADERS AND INITIALIZES GLOBAL VARIABLES
      USE PARM
      HEDC=(/" HUI"," LAI","  RD","  RW","BIOM","PLTC"," STL","CPHT",&
      " STD"," UN1"," UP1"," UK1","  WS","  NS","  PS","  KS","  TS",&
      "  AS","SALT"," REG"/)
      HEDP=(/"PAPL","PSRO","PLCH","PSSF","PSED","PDGF","PDGS","PFOL",&
      "PSOL","PDRN"/)
      HEDS=(/"TNH3","TNO3","PLAB","TSOK","SNOA","RZSW","WTBL","GWST",&
      "STDO"," RSD","OCPD"," TOC","  LS","  LM"," LSL"," LSC"," LMC",&
      "LSLC","LSNC"," BMC"," HSC"," HPC"," LSN"," LMN"," BMN"," HSN",&
      " HPN"," TWN","SALT","TNO2"/)
      SID=(/"   1","   2","   3","   4","   5","   6","  7","   8",&
      "   9","  10","  11","  12","  13","  14","  15"," TOT"/)
      SLOD=(/"ALFISOLS ","MOLLISOLS","ULTISOLS "/)
      NFS=40;NJC=4;NKA=100;NKD=40;NKS=40;NKYA=40
      NC=(/0,31,60,91,121,152,182,213,244,274,305,335,366/)
      NDC=(/1,2,3,4,5,6,7,8,9,10/)
	  NHC=(/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,&
      23,24,25,26,27/)
      KR=(/11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,&
      31,32,33,34,35,36,37,38,39,40/)
      PSZ=(/200.,10.,2.,30.,500./)
      PI2=6.283185;PIT=58.13;CLT=57.296
      WCS=(/25.4,50.8,76.2/)
	  RETURN
      END