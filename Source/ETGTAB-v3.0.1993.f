      PROGRAM ETGTAB
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C     Program ETGTAB, version 3.0 930821 FORTRAN 77.                   C
C                                                                      C
C     === Version for MS-DOS using LAHEY-F77 FORTRAN compiler    ===   C
C                                                                      C
C     === To run this program under UNIX, you have to modify     ===   C
C         routine GEOEXT.                                              C
C                                                                      C
C     The program ETGTAB computes model tides using different tidal    C
C     potential developments (DOODSON 1921 with 378 waves, CARTWRIGHT- C
C     TAYLER-EDDEN 1973 with 505 waves, TAMURA 1987 with 1200 waves,   C
C     or BUELLESFELD 1985 with 665 waves) for a number of different    C
C     tidal components using observed or estimated (e.g. by a body     C
C     tide and ocean tide model) tidal parameters.                     C
C                                                                      C
C     Disc file description:                                           C
C     ----------------------                                           C
C                                                                      C
C     ETCPOT.DAT:  formatted unit, on which the tidal potential        C
C                  development has to be stored before the execution   C
C                  of program ETGTAB.                                  C
C     ETGTAB.INP:  formatted unit, on which the input parameters       C
C                  have to be stored before the execution of program   C
C                  ETGTAB.                                             C
C     ETGTAB.PRN:  formatted print file.                               C
C     ETGTAB.OUT:  formatted output file in ETERNA 3.0 format.         C
C                                                                      C
C     Used routines:                                                   C
C     --------------                                                   C
C                                                                      C
C     ETASTE: computes astronomical elements.                          C
C     ETGCOF: computes geodetic coefficients.                          C
C     ETGREG: computes GREGORIAN date.                                 C
C     ETJULD: computes JULIAN date.                                    C
C     ETLOVE: computes elastic parameters from WAHR-DEHANT model.      C
C     ETMUTC: computes difference TDT - UTC.                           C
C     ETPOTA: computes amplitudes, frequencies and phases of tidal     C
C             waves.                                                   C
C     GEOEXT: computes JOBTIME.                                        C
C                                                                      C
C     Numerical accuracy:                                              C
C     -------------------                                              C
C                                                                      C
C     The program has been tested on IBM-At compatible computers under C
C     MS-DOS operating system with different compilers using DOUBLE    C
C     PRECISION for all variables (15 digits) and on a SUN SPARC2      C
C     under UNIX operating system with SUN F77 compiler, and gave      C
C     identical results to 0.001 nm/s**2.                              C
C                                                                      C
C     Execution time:                                                  C
C     ---------------                                                  C
C                                                                      C
C     The CPU execution time depends mainly on the number of waves of  C
C     the choosen tidal potential development, and the number of model C
C     tide values to be computed. For TAMURA 1987 tidal potential      C
C     development with 1200 tidal waves and 744 model tide values, the C
C     CPU execution time is about                                      C
C                                                                      C
C         7.36 s on IBM-AT compatible 486DX2 66 Mhz.                   C
C                                                                      C
C     Program creation:  730623 by H.-G. Wenzel,                       C
C                        Geodaetisches Institut,                       C
C                        Universitaet Karlsruhe,                       C
C                        Englerstr. 7,                                 C
C                        D-76128 KARLSRUHE,                            C
C                        Germany.                                      C
C                        Tel.: 0721-6082307,                           C
C                        FAX:  0721-694552.                            C
C                        e-mail: wenzel@gik.bau-verm.uni-karlsruhe.de  C
C     Last modification: 930821 by H.-G. Wenzel.                       C
C**********************************************************************C
      IMPLICIT DOUBLE PRECISION (D)
      CHARACTER CUNIT(11)*8,CTEX(10,10)*8,COMPON(11)*24,CVERS*10
C    1 C88*8,C99*9
      DIMENSION DGI(24)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     The following dimension statements are concerning the number of  C
C     wavegroups to be used, which is restricted to  85 in the current C
C     program version (parameter MAXWG).                               C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      PARAMETER (MAXWG=85)
      DIMENSION NA(MAXWG),NE(MAXWG),INA(MAXWG),INE(MAXWG),
     1 DG0(MAXWG),DPHI0(MAXWG)
      CHARACTER CNSY(MAXWG)*8
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     The following dimension statements are concerning the number of  C
C     waves of the tidal potential development, which is 1214 in the   C
C     current program version (parameter MAXNW).                       C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      PARAMETER (MAXNW=1214)
      DIMENSION DTHAM(MAXNW),DTHPH(MAXNW),DTHFR(MAXNW),DBODY(MAXNW)
      DIMENSION NRW(MAXNW)
      COMMON /UNITS/ CUNIT,IC2
      COMMON /CONST/ DPI,DPI2,DRAD,DRO
      DATA IUN4/10/,IUN5/11/,IUN6/12/,IUN13/13/,IUN14/14/
      DATA CVERS/'3.0 180219'/
C      DATA C88/'88888888'/
C      DATA C99/'999999999'/
      DATA COMPON/'Potential  ' ,'Gravity                 ',
     1'Tilt                    ','Vertical displcement    ',
     3'Horizontal displacement ','Vertical strain         ',
     5'Horizontal strain       ','Aereal strain           ',
     7'Shear  strain           ','Volume strain           ',
     9'Ocean tide              '/
      OPEN(IUN4,FILE='ETCPOT.DAT')
      OPEN(IUN5,FILE='ETGTAB.INP')
      OPEN(IUN6,FILE='ETGTAB.PRN')
      OPEN(IUN13,FILE='ETGTAB.OUT',FORM='FORMATTED')
      WRITE(IUN6,7002) CVERS
      WRITE(*,7002) CVERS
      CALL GEOEXT(IUN6,DEXTIM)
      READ(IUN5,7008) DLAT
      READ(IUN5,7008) DLON
      READ(IUN5,7008) DH
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     DLAT: ellipsoidal latitude in degree, referring to Geodetic      C
C           Reference System 1980.                                     C
C     DLON: ellipsoidal longitude in degree, referring to Geodetic     C
C           Reference System 1980, positive east of Greenwhich.        C
C     DH:   ellipsoidal height in meter, referring to Geodetic         C
C           Reference System 1980.                                     C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      READ(IUN5,7008) DGRAV
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     DGRAV: gravity in m/s**2. If unknown, put DGRAV to zero.         C
C            For DGRAV is less 1 m/s**2, the observerd gravity will be C
C            replaced by normal gravity referring to GRS80 Reference   C
C            System. DGRAV is only valid for tidal tilt.               C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      READ(IUN5,7008) DAZ
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     DAZ: azimuth in degree clockwise from north direction, (omly     C
C          valid for tidat tilt and horizontal strain).                C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      READ(IUN5,7006) IC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Read Earth tide component IC.                                    C
C        IC=-1: tidal potential, geodetic coefficients in m**2/s**2.   C
C        IC= 0: vertical tidal acceleration (gravity tide), geodetic   C
C               coefficients in nm/s**2 (positive down).               C
C        IC= 1: horizontal tidal acceleration (tidal tilt) in azimuth  C
C               DAZ, geodetic coefficients in mas = arc sec/1000.      C
C        IC= 2: vertical tidal displacement, geodetic coefficients in  C
C               mm.                                                    C
C        IC= 3: horizontal tidal displacement in azimuth DAZ, geodetic C
C               coefficients in mm.                                    C
C        IC= 4: vertical tidal strain, geodetic coefficients in 10**-9 C
C               = nstr.                                                C
C        IC= 5: horizontal tidal strain in azimuth DAZ, geodetic       C
C               coefficients in 10**-9 = nstr.                         C
C        IC= 6: areal tidal strain, geodetic coefficients  in 10**-9   C
C               = nstr.                                                C
C        IC= 7: shear tidal strain, geodetic coefficients  in 10**-9   C
C               = nstr.                                                C
C        IC= 8: volume tidal strain, geodetic coefficients in 10**-9   C
C               = nstr.                                                C
C        IC= 9: ocean tides, geodetic coefficients in millimeter.      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      READ(IUN5,7006) IPRINT
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     IPRINT: printout parameter for tidal potential development.      C
C        IPRINT = 0: tidal potential development will not be printed.  C
C        IPRINT = 1: geodetic coefficients and astronomical elements   C
C                    will be printed only.                             C
C        IPRINT = 2: geodetic coefficients, astronomical elements, and C
C                    tidal potential development will be printed.      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      READ(IUN5,7007) ITY,ITM,ITD,ITH
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Initial epoch:                                                   C 
C     ITY...  year  of start epoch of model tide computation.          C
C     ITM...  month of start epoch of tidal computation.               C
C     ITD...  day   of start epoch of tidal computation.               C
C     ITH...  hour  of start epoch of tidal computation (UTC).         C
C             Example: 27th of september 1984, 10 o'clock UTC is       C
C             ITY = 1984, ITM = 9, ITD = 27, ITH = 10.                 C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      READ(IUN5,7006) IH
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     IH...   Time span in hours, for which the model tides will be    C
C             computed.                                                C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      READ(IUN5,7006) IDTSEC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     IDTSEC: Time interval between the model tide values in secs.     C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DDTH=DBLE(IDTSEC)/3600.D0                                   
      DTH=DBLE(ITH)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     IMODEL..tidal potential development to be used.                  C
C             IMODEL=0: DOODSON 1921 development with 378 waves.       C
C             IMODEL=1: CARTWRIGHT-TAYLER-EDDEN 1973 development with  C   
C                                                          505 waves.  C
C             IMODEL=2: TAMURA  1987     development with 1200 waves.  C
C             IMODEL=3: BUELLESFELD 1985 development with  665 waves.  C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      READ(IUN5,7006) IMODEL
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Read parameter IRIGID = 1 for rigid   Earth model tides,         C
C                    IRIGID = 0 for elastic Earth model tides.         C
C     Attention: IRIGID=1 should only be used for comparison with      C
C                model tides computed from ephemeris programs. For     C
C                read world predictions, use always IRIGID=0.          C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      READ(IUN5,7006) IRIGID
      IF(IRIGID.EQ.1) WRITE(IUN6,7009)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Compute amplitudes, frequencies and phases of the tidal waves.   C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      CALL ETPOTA(IUN4,IUN6,IUN14,IPRINT,IMODEL,DLAT,DLON,DH,DGRAV,DAZ,
     1 IC,ITY,ITM,ITD,DTH,DDT0,MAXNW,NRW,DTHAM,DTHPH,DTHFR,DBODY,NW)
      IC2=IC+2
      CLOSE(UNIT=IUN4)
      ITH=DTH
      WRITE(IUN13,7018) CVERS,DLAT,DLON,DH,DGRAV,DAZ,IC,ITY,ITM,ITD,ITH,
     1 IMODEL,IRIGID
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Read alphanumeric comment on 10 records:                         C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      WRITE(IUN6,7010) CVERS
      DO 900 I=1,10
      READ(IUN5,7011)   (CTEX(I,J),J=1,10)
      WRITE(IUN6,7012)  (CTEX(I,J),J=1,10)
      WRITE(IUN13,7011) (CTEX(I,J),J=1,10)
  900 CONTINUE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Read wave groups and observed tidal parameters:                  C
C     DG0:     amplitude factor.                                       C
C     DPHI0... phase lead in degree.                                   C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      READ(IUN5,7005)   NGR
      WRITE(IUN13,7005) NGR
      WRITE(IUN6,7013)
      DO 910 IG=1,NGR
      READ(IUN5,7014)   NA(IG),NE(IG),CNSY(IG),DG0(IG),DPHI0(IG)
      WRITE(IUN13,7014) NA(IG),NE(IG),CNSY(IG),DG0(IG),DPHI0(IG)
      DO 930 IW=1,NW
      IF(NRW(IW).LT.NA(IG)) GOTO 930
      INA(IG)=IW
      GOTO 940
  930 CONTINUE
  940 CONTINUE
      INE(IG)=NW
      DO 950 IW=1,NW
      IF(NRW(IW).LE.NE(IG)) GOTO 950
      INE(IG)=IW-1
      GOTO 960
  950 CONTINUE
  960 CONTINUE
      NAK=INA(IG)
      NEK=INE(IG)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Search for main wave of the group:                               C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DAM=0.D0
      DO 970 IW=NAK,NEK
      IF(IRIGID.EQ.1) DBODY(IW)=1.D0
      IF(DTHAM(IW).LT.DAM) GOTO 970
      DAM=DTHAM(IW)
      DBOD=DBODY(IW)
  970 CONTINUE
      WRITE(IUN6,7015) IG,INA(IG),INE(IG),DG0(IG),DPHI0(IG),CNSY(IG),
     1 DAM,DBOD
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Compute observed amplitude. Constituents of different degree are C
C     scaled by WAHR-DEHANT body tide amplitude factors:               C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DO 920 IW=NAK,NEK
      DTHAM(IW)=DTHAM(IW)*DG0(IG)*DBODY(IW)/DBOD
      DTHPH(IW)=DTHPH(IW)+DPHI0(IG)*DRAD
C      WRITE(IUN6,*) IW,DTHAM(IW),DTHPH(IW)
  920 CONTINUE
  910 CONTINUE
      CALL GEOEXT(IUN6,DEXTIM)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Print hourly model tides with format 6F10.3:                     C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 1000 WRITE(IUN6,7016) CVERS,COMPON(IC2),CUNIT(IC2)
      WRITE(*,7016)    CVERS,COMPON(IC2),CUNIT(IC2)
      WRITE(IUN13,7019)
C      WRITE(IUN13,7020) 0.D0
      ITMIN=0
      ITSEC=0
      DDAT=DBLE(IH)/DDTH
      NDAT=DDAT
      DO 1010 I=1,NDAT,6
      DO 1020 J=1,6
      DT=DBLE(I+J-2)*DDTH
      DDC=0.D0
      DO 1030 IG=1,NGR
      DO 1040 IW=INA(IG),INE(IG)
      DDC=DDC+DTHAM(IW)*DCOS(DTHPH(IW)+DT*DTHFR(IW))
 1040 CONTINUE
 1030 CONTINUE
      DGI(J)=DDC
 1020 CONTINUE
      ITH=DTH
      WRITE(IUN6,7017) ITY,ITM,ITD,ITH,ITMIN,ITSEC,(DGI(J),J=1,6)
      WRITE(   *,7017) ITY,ITM,ITD,ITH,ITMIN,ITSEC,(DGI(J),J=1,6)
C
      DO 1050 J=1,6
      WRITE(IUN13,7017) ITY,ITM,ITD,ITH,ITMIN,ITSEC,DGI(J)
      ITSEC=ITSEC+IDTSEC
      IF(ITSEC.LT.60) GOTO 1052
      IK=ITSEC/60
      ITSEC=ITSEC-60*IK
      ITMIN=ITMIN+IK
 1052 CONTINUE
      IF(ITMIN.LT.60) GOTO 1054
      ITMIN=ITMIN-60
      ITH=ITH+1
 1054 CONTINUE
 1050 CONTINUE
 1060 DTH=DTH+6.D0*DDTH
      IF(DTH.GT.23.0D0) CALL ETGREG(ITY,ITM,ITD,DTH)
 1010 CONTINUE
C      WRITE(IUN13,7001) C99
C      WRITE(IUN13,7001) C88
      WRITE(IUN6,7030) 
      WRITE(*,7030)
      CALL GEOEXT(IUN6,DEXTIM)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Format statements:                                               C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 7001 FORMAT(A9)
 7002 FORMAT(' Program ETGTAB, version ',A10,' FORTRAN 77.'//
     1' Computation of model tides from tidal potential development'
     2 //)
 7005 FORMAT(2I5)
 7006 FORMAT(40X,I10)
 7007 FORMAT(45X,4I5)
 7008 FORMAT(40X,F10.3)
 7009 FORMAT(//' ***** Parameter IRIGID = 1 has been input. '/
     1' ***** IRIGID =1 should only be used for comparison with model'/
     2' ***** tides computed from ephemeris programs. For real world '/
     3' ***** computations, use always IRIGID=0.'/)
 7010 FORMAT(' Program ETGTAB, version ',A8,'FORTRAN 77'//)
 7011 FORMAT(10A8)
 7012 FORMAT(10A8)
 7013 FORMAT(///' Wave groups and observed tidal parameters'//
     1'  no. from   to ampl.fac. phase lead        ampl.  WD body '/
     2'                              [deg]'/)
 7014 FORMAT(2I5,1X,A8,2F10.4)
 7015 FORMAT(3I5,2F10.4,1X,A8,2F10.4)
 7016 FORMAT(////' Program ETGTAB, version ',A8,' FORTRAN 77'//
     1' Component ',A24,' IN ',1X,A8//4X,'Date in UT'//)
 7017 FORMAT(1X,I4,2(1X,I2.2),3(1X,I2.2),6F10.3)
 7018 FORMAT(' Model tides from program ETGTAB, version',A10,' F77.'/
     1'C*************************************************************'/
     2'ELLIPSOIDAL LATITUDE  (DEGREE)          ',F10.4/
     3'ELLIPSOIDAL LONGITUDE (DEGREE EAST)     ',F10.4/
     4'ELLIPSOIDAL HEIGHT    (METER)           ',F10.3/
     5'GRAVITY (M/S**2, < 1.0 IF UNKNOWN)      ',F10.4/
     6'AZIMUTH FROM NORTH (DEGREE)             ',F10.4/
     7'EARTH TIDE COMPONENT                    ',I10/
     8'PRINT OUT OF TIDAL WAVES (1=YES)                 0'/
     9'SEARCH FOR DATA ERRORS (1=YES) AND LIMIT         0   100.000'/
     *'INITIAL EPOCH (YYYY,MM,DD,HH)           ',5X,4I5/
     1'NUMERICIAL LOW PASS FILTER (0...5)               2'/
     2'PRINT OBSERVATIONS (1=YES)                       0'/
     3'PRINT LOWPASS  FILTERED OBSERV. (1=YES)          0'/
     4'TIDAL POTENTIAL DEVELOPMENT (0...2)     ',I10/
     5'RIGID EARTH AMPLITUDE FACTORS (1=YES)   ',I10/
     6'HANN WINDOW FOR ADJUSTMENT (1=YES)               0'/
     7'QUICKLOOK ADJUSTMENT (1=YES)                     0')
 7019 FORMAT(/,'      MODL         1.0000    1.0000     0.000         0'
     1,' ETGTAB',/)
 7020 FORMAT(15X,F10.3)
 7021 FORMAT(I4,2I02,1X,3I02,6F10.3)
 7022 FORMAT(1X,I4,1X,E15.6)
 7030 FORMAT(///' ***** Program ETGTAB finished execution.'/)
      PAUSE
      END
C
      BLOCK DATA 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C     BLOCK DATA for program ETGTAB, version 930819 FTN77.             C
C                                                                      C
C     Routine creation:  930401 by H.-G. Wenzel.                       C
C                        Geodaetisches Institut,                       C
C                        Universitaet Karlsruhe,                       C
C                        Englerstr. 7,                                 C
C                        D-76128 KARLSRUHE 1,                          C
C                        Germany.                                      C
C                        Tel: 0049-721-6082307,                        C
C                        FAX: 0049-721-694552.                         C
C                        e-mail: wenzel@gik.bau-verm.uni-karlsruhe.de  C
C     Last modification: 930703 by H.-G. Wenzel.                       C
C**********************************************************************C
      IMPLICIT DOUBLE PRECISION (D)
      CHARACTER CUNIT(11)*8
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     COMMON /CONST/:                                                  C
C     DPI...        3.1415....                                         C
C     DPI2...       2.D0*DPI                                           C
C     DRAD...       DPI/180.D0                                         C
C     DRO...        180.D0/DPI                                         C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      COMMON /CONST/ DPI,DPI2,DRAD,DRO
      COMMON /UNITS/ CUNIT,IC2
      DATA DPI/3.141592653589793D0/,DPI2/6.283185307179586D0/,
     1 DRAD/1.745329251994330D-02/,DRO/57.295779513082320D0/
      DATA CUNIT/'(m/s)**2','nm/s**2 ',' mas    ',' mm     ',' mm     ',
     1' nstr   ',' nstr   ',' nstr   ',' nstr   ',' nstr   ',' mm     '/
      END
C
      SUBROUTINE ETASTE(IUN6,IPRINT,IMODEL,DLON,ITY,ITM,ITD,DTH,DAS,
     1 DASP,DDT0)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C     Routine ETASTE, version 930531 FORTRAN 77.                       C
C                                                                      C
C     The routine ETASTE computes the astronomical elements for        C
C     a specific epoch, given in UTC. This routine compiles under      C
C     operation system UNIX using the SUN-FORTRAN compiler and under   C
C     operation system MS-DOS using the MS 5.0 FORTRAN compiler.       C
C                                                                      C
C     Reference: TAMURA, Y. 1987: A Harmonic Development of the Tide-  C
C                generating Potential. Bulletin d'Informations Marees  C
C                Terrestres no. 99, 6813-6855, Bruxelles 1987.         C
C                                                                      C
C                WENZEL, H.-G. 1976: Zur Genauigkeit von gravimetri-   C
C                schen Erdgezeitenbeobachtunngen. Wissenschaftliche    C
C                Arbeiten der Lehrtsuehle fuer Geodaesie, Photogramme- C
C                trie und Kartographie an der Technischen Universitaet C
C                Hannover NR. 67, Hannover 1976.                       C
C                                                                      C
C     All variables with D as first character are double precision.    C
C                                                                      C
C     Input parameter description:                                     C
C     ----------------------------                                     C
C                                                                      C
C     IUN6...      unit number of formatted printout file.             C
C     IPRINT...    printout parameter. For IPRINT=0, nothing will      C
C                  be printed on unit IUN6.                            C
C     IMODEL...    parameter for the used tidal potential model.       C
C                  IMODEL = 0: DOODSON 1921     tidal potential.       C
C                  IMODEL = 1: CTED 1973        tidal potential.       C
C                  IMODEL = 2: TAMURA 1987      tidal potential.       C
C                  IMODEL = 3: BUELLESFELD 1985 tidal potential.       C
C     DLON...      ellipsoidal longitude of the station referring to   C
C                  geodetic reference system GRS80 in degree,          C
C                  positive east of Greenwhich.                        C
C     ITY...       integer year  of the epoch (e.g. 1988).             C
C     ITM...       integer month of the epoch (e.g. January = 1).      C
C     ITD...       integer day   of the epoch.                         C
C     DTH...       hour          of the epoch in UTC.                  C
C                                                                      C
C     Output parameter description:                                    C
C     -----------------------------                                    C
C                                                                      C
C     DAS(1)...    mean local Moontime in degree.                      C
C     DAS(2)...    mean longitude of the Moon in degree.               C
C     DAS(3)...    mean longitude of the Sun  in degree.               C
C     DAS(4)...    mean longitude of the perigee of the Moon's orbit   C
C                  in degree.                                          C
C     DAS(5)...    negative mean longitude of the ascending node of    C
C                  the Moon's orbit in degree.                         C
C     DAS(6)...    mean longitude of the perigee of the Suns's orbit   C
C                  in degree.                                          C
C     DAS(7)...    argument of Jupiter's opposition in degree (for     C
C                  TAMURA's 1987 tidal potential development).         C
C     DAS(8)...    argument of Venus's conjunction in degree (for      C
C                  TAMURA's 1987 tidal potential development).         C
C     DASP(1...8): time derivatives of the corresponding variables     C
C                  DAS in degree per hour.                             C
C     DDT0...      difference TDT minus UTC at initial epoch in sec.   C
C                                                                      C
C     Used routines:                                                   C
C     --------------                                                   C
C                                                                      C
C     ETEXTI: computes jobtime.                                        C
C     ETJULD: computes Julian date.                                    C
C                                                                      C
C     Numerical accuracy:                                              C
C     -------------------                                              C
C                                                                      C
C     The routine has been tested on IBM Pc with 15 digits accuracy    C
C     in double precision using different compilers.                   C
C                                                                      C
C     Execution time:                                                  C
C     ---------------                                                  C
C                                                                      C
C     The execution time is about 0.00017 s per call of routine ETASTE C
C     on ICM Pc 80486 DX2 with 66 MHz speed.                           C
C                                                                      C
C     Routine creation:  880130 BY H.-G.WENZEL.                        C
C                        Geodaetisches Institut,                       C
C                        Universitaet Karlsruhe,                       C
C                        Englerstr. 7,                                 C
C                        D-76128 KARLSRUHE 1,                          C
C                        Germany.                                      C
C                        Tel.: 0721-6082307,                           C
C                        FAX:  0721-694552.                            C
C                        e-mail: wenzel@gik.bau-verm.uni-karlsruhe.de  C
C     Last modification: 930531 by H.-G. Wenzel.                       C
C**********************************************************************C
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT INTEGER*4 (I-N)
      DOUBLE PRECISION DAS(8),DASP(8)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     COMMON /CONST/:                                                  C
C     DPI...        3.1415....                                         C
C     DPI2...       2.D0*DPI                                           C
C     DRAD...       DPI/180.D0                                         C
C     DRO...        180.D0/DPI                                         C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      COMMON /CONST/ DPI,DPI2,DRAD,DRO
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Compute Julian date for epoch ITY, ITM, ITD, DTH.                C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      CALL ETJULD(IUN6,ITY,ITM,ITD,DTH,DTUJD)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Compute Universal Time epoch DTUT in Julian centuries referring  C
C     to 1. January 1900.                                              C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DTUT=(DTUJD-2415020.0D0)/36525.0D0
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Compute Universal Time epoch DTUT20 in Julian Centuries          C
C     referring to 1. January 2000 12 h.                               C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DTUT20=(DTUJD-2451545.0D0)/36525.0D0
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Compute Terrestrial Dynamical Time epoch DT in Julian Centuries  C
C     referring to 1. January 1990 and DTDT20 referring to 1 January   C
C     2000 12 h.                                                       C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      CALL ETMUTC(IUN6,IPRINT,DTUJD,DDT0)
      DT=DTUT+DDT0/3155760000.0D0
      DTDT20=DTUT20+DDT0/3155760000.0D0
      IF(IPRINT.GT.0) WRITE(IUN6,7001) ITY,ITM,ITD,DTH,DTUJD,DDT0
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Compute astronomical elements for initial epoch from             C
C     NEWCOMB's formula for the Sun and from BROWN's formulas for the  C
C     Moon (see Astronomical Ephemeris, Explanatory Supplement).       C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DT2=DT*DT
      DT3=DT2*DT
      IF(IMODEL.EQ.2) GOTO 100
      DAS(2)=270.434164D0 +481267.8831417D0*DT-0.0011333D0*DT2
     1 +0.0000019D0*DT3
      DAS(3)=279.696678D0 +36000.768925D0*DT +0.0003025D0*DT2
      DAS(4)=334.329556D0 +4069.0340333D0*DT -0.010325D0*DT2
     1 -0.0000125D0*DT3
      DAS(5)=100.816725D0 +1934.1420083D0*DT -0.0020778D0*DT2
     1 -0.0000022D0*DT3
      DAS(6)=281.220833D0 +1.719175D0*DT +0.0004528D0*DT2
     1 +0.0000033D0*DT3
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Compute Jupiter's and Venus's arguments from TAMURA's 1987       C
C     formulas.                                                        C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DAS(7)=248.1D0+32964.47D0*DTDT20
      DAS(8)= 81.5D0+22518.44D0*DTDT20
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Compute speeds in degree per hour:                               C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DASP(2)=0.54901652195037D0 -2.58575D-9*DT +6.46D-12*DT2
      DASP(3)=0.04106863897444D0 +6.902D-10*DT
      DASP(4)=0.00464183667960D0 -2.355692D-8*DT-4.278D-11*DT2
      DASP(5)=0.00220641342494D0 -4.74054D-9*DT -7.60D-12*DT2
      DASP(6)=0.00000196118526D0 +1.03303D-9*DT +1.141D-11*DT2
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Compute Jupiter's and Venus's speed from TAMURA's 1987           C
C     formulas.                                                        C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DASP(7)=32964.47D0/(24.D0*36525.D0)
      DASP(8)=22518.44D0/(24.D0*36525.D0)
      DASP(1)=DASP(3)-DASP(2)+15.0D0
      DO 10 I=2,8
      DAS(I)=DMOD(DAS(I),360.0D0)
      IF(DAS(I).LT.0.D0) DAS(I)=DAS(I)+360.D0
   10 CONTINUE
      DAS(1)=DAS(3)-DAS(2)+DLON+DTH*15.0D0
      IF(DAS(1).LT.0.D0) DAS(1)=DAS(1)+360.0D0
      GOTO 200
  100 CONTINUE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Compute astronomical elements from TAMURA's 1987 formulas:       C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DT2=DTUT20*DTUT20
      DT3=DT2*DTUT20
      DAL=280.4606184D0 + 36000.7700536D0*DTUT20 + 0.00038793D0*DT2
     1 -0.0000000258D0*DT3
      DALP=(36000.7700536D0 +2.0D0*0.00038793D0*DTUT20
     1 -3.0D0*0.0000000258D0*DT2)/(24.0D0*36525.D0)
      DT2=DTDT20**2
      DS=218.316656D0+481267.881342D0*DTDT20-0.001330D0*DT2
      DSP=(481267.881342D0-2.0D0*0.001330D0*DTDT20)/(24.D0*36525.0D0)
      DH=280.466449D0+36000.769822D0*DTDT20+0.0003036D0*DT2
      DHP=(36000.769822D0+2.0D0*0.0003036D0*DTDT20)/(24.D0*36525.0D0)
      DDS=0.0040D0*DCOS((29.D0+133.0D0*DTDT20)*DRAD)
      DDSP=(-0.0040D0*133.0D0*DRAD*DSIN((29.D0+133.0D0*DTDT20)*DRAD))/
     1 (24.0D0*36525.0D0)
      DDH=0.0018D0*DCOS((159.D0+19.D0*DTDT20)*DRAD)
      DDHP=(-0.0018D0*19.0D0*DRAD*DSIN((159.D0+19.D0*DTDT20)*DRAD))/
     1 (24.0D0*36525.0D0)
      DAS(1)=DAL-DS+DLON+DTH*15.0D0
      DAS(2)=DS+DDS
      DAS(3)=DH+DDH
      DAS(4)=83.353243D0 + 4069.013711D0*DTDT20 -0.010324D0*DT2
      DAS(5)=234.955444D0 +1934.136185D0*DTDT20 -0.002076D0*DT2
      DAS(6)=282.937348D0 + 1.719533D0*DTDT20 +0.0004597D0*DT2
      DAS(7)=248.1D0+32964.47D0*DTDT20
      DAS(8)= 81.5D0+22518.44D0*DTDT20
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Compute the speeds in degree per hour:                           C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DASP(1)=DALP-DSP+15.0D0
      DASP(2)=DSP+DDSP
      DASP(3)=DHP+DDHP
      DASP(4)=(4069.013711D0-2.0D0*0.010324D0*DTDT20)/(24.0D0*36525.0D0)
      DASP(5)=(1934.136185D0-2.0D0*0.002076D0*DTDT20)/(24.0D0*36525.0D0)
      DASP(6)=(1.719533D0+2.0D0*0.0004597D0*DTDT20)/(24.0D0*36525.0D0)
      DASP(7)=32964.47D0/(24.D0*36525.D0)
      DASP(8)=22518.44D0/(24.D0*36525.D0)
      DO 110 I=1,8
      DAS(I)=DMOD(DAS(I),360.0D0)
      IF(DAS(I).LT.0.D0) DAS(I)=DAS(I)+360.0D0
  110 CONTINUE
  200 CONTINUE
      IF(IPRINT.EQ.0) RETURN
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Print astronomical elements:                                     C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IF(IMODEL.NE.2) WRITE(IUN6,7003) (DAS(K),DASP(K),K=1,8)
      IF(IMODEL.EQ.2) WRITE(IUN6,7004) (DAS(K),DASP(K),K=1,8)
 5000 CONTINUE
      WRITE(IUN6,7030)
      WRITE(*,7030)
      RETURN
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Format statements:                                               C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 7001 FORMAT(' Routine ETASTE, version 930531 FTN 77.'//
     1' Astronomic elements for initial epoch ',I4,2I3,F3.0,' UTC'/
     2' Julian date                   : ',F15.4/
     3' Correction to UTC to give TDT : ',F15.3,' s'/)
 7003 FORMAT(' BROWNs formulas for the Moon, NEWCOMBs formulas for the',
     1' Sun'/' TAMURAs formulas for Jupiter and Venus'//
     2' TAU',F20.11,' deg  TAU.',F20.11,' deg/hour'/
     3' S  ',F20.11,' deg  S.  ',F20.11,' deg/hour'/
     4' H  ',F20.11,' deg  H.  ',F20.11,' deg/hour'/
     5' P  ',F20.11,' deg  P.  ',F20.11,' deg/hour'/
     6' N  ',F20.11,' deg  N.  ',F20.11,' deg/hour'/
     7' P1 ',F20.11,' deg  P1. ',F20.11,' deg/hour'/
     8' JU ',F20.11,' deg  JU. ',F20.11,' deg/hour'/
     9' VE ',F20.11,' deg  VE. ',F20.11,' deg/hour'/)
 7004 FORMAT(' TAMURAs 1987 formulas are used.'//
     1' F1 ',F20.11,' deg  F1. ',F20.11,' deg/hour'/
     2' F2 ',F20.11,' deg  F2. ',F20.11,' deg/hour'/
     3' F3 ',F20.11,' deg  F3. ',F20.11,' deg/hour'/
     4' F4 ',F20.11,' deg  F4. ',F20.11,' deg/hour'/
     5' F5 ',F20.11,' deg  F5. ',F20.11,' deg/hour'/
     6' F6 ',F20.11,' deg  F6. ',F20.11,' deg/hour'/
     7' F7 ',F20.11,' deg  F7. ',F20.11,' deg/hour'/
     8' F8 ',F20.11,' deg  F8. ',F20.11,' deg/hour'/)
 7030 FORMAT(///' ***** Routine ETASTE finished the execution.'/)
      END
C
      SUBROUTINE ETGCOF(IUN6,IPRINT,DLAT,DLON,DH,DGRAV,DAZ,IC,DGK,DPK)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C     Routine ETGCOF, version 930706 FORTRAN 77.                       C
C                                                                      C
C     The routine ETGCOF computes the geodetic coefficients for        C
C     the tidal potential developments, DOODSON's normalization.       C
C                                                                      C
C     All variables with D as first character are DOUBLE PRECISION.    C
C                                                                      C
C     Input parameter description:                                     C
C     ----------------------------                                     C
C                                                                      C
C     IUN6...      formatted line printer unit.                        C
C     DLAT...      ellipsoidal latitude in degree, referring to        C
C                  geodetic reference system GRS80.                    C
C     DLON...      ellipsoidal longitude in degree, referring to       C
C                  geodetic reference system GRS80, positiv east of    C
C                  Greenwhich.                                         C
C     DH...        ellipsoidal height in meter, referring to geodetic  C
C                  reference system GRS80.                             C
C     DGRAV...     gravity in m/s**2. If DGRAV less than  9.50 m/s**2, C
C                  DGRAV will be overwritten by normal gravity         C
C                  referring to geodetic reference system 1980.        C
C     DAZ...       azimuth in degree from north direction counted      C
C                  clockwise (necessary for tidal tilt only).          C
C     IC...        Earth tide component to be computed.                C
C                  IC=-1: tidal potential, geodetic coefficients       C
C                         in m**2/s**2.                                C
C                  IC= 0: vertical tidal acceleration (gravity tide),  C
C                         geodetic coefficients in nm/s**2 (positive   C
C                         down).                                       C
C                  IC= 1: horizontal tidal acceleration (tidal tilt)   C
C                         in azimuth DAZ, geodetic coefficients in     C
C                         mas = arc sec/1000.                          C
C                  IC= 2: vertical tidal displacement, geodetic        C
C                         coefficients in mm.                          C
C                  IC= 3: horizontal tidal displacement in azimuth     C
C                         DAZ, geodetic coefficients in mm.            C
C                  IC= 4: vertical tidal strain, geodetic coefficients C
C                         in 10**-9 = nstr.                            C
C                  IC= 5: horizontal tidal strain in azimuth DAZ,      C
C                         geodetic coefficients in 10**-9 = nstr.      C
C                  IC= 6: areal tidal strain, geodetic coefficients    C
C                         in 10**-9 = nstr.                            C
C                  IC= 7: shear tidal strain, geodetic coefficients    C
C                         in 10**-9 = nstr.                            C
C                  IC= 8: volume tidal strain, geodetic coefficients   C
C                         in 10**-9 = nstr.                            C
C                  IC= 9: ocean tides, geodetic coefficients in        C
C                         millimeter.                                  C
C                                                                      C
C     Output parameter description:                                    C
C     -----------------------------                                    C
C                                                                      C
C     DGK...       array (1...12) of geodetic coefficients.            C
C     DGK(1)...    geodetic coefficient for degree 2 and order 0.      C
C     DGK(2)...    geodetic coefficient for degree 2 and order 1.      C
C     DGK(3)...    geodetic coefficient for degree 2 and order 2.      C
C     DGK(4)...    geodetic coefficient for degree 3 and order 0.      C
C     DGK(5)...    geodetic coefficient for degree 3 and order 1.      C
C     DGK(6)...    geodetic coefficient for degree 3 and order 2.      C
C     DGK(7)...    geodetic coefficient for degree 3 and order 3.      C
C     DGK(8)...    geodetic coefficient for degree 4 and order 0.      C
C     DGK(9)...    geodetic coefficient for degree 4 and order 1.      C
C     DGK(10)...   geodetic coefficient for degree 4 and order 2.      C
C     DGK(11)...   geodetic coefficient for degree 4 and order 3.      C
C     DGK(12)...   geodetic coefficient for degree 4 and order 4.      C
C                                                                      C
C     DPK...       array (1...12) of phases in degree.                 C
C     DPK(1)...    phase in degree for      degree 2 and order 0.      C
C     DPK(2)...    phase in degree for      degree 2 and order 1.      C
C     DPK(3)...    phase in degree for      degree 2 and order 2.      C
C     DPK(4)...    phase in degree for      degree 3 and order 0.      C
C     DPK(5)...    phase in degree for      degree 3 and order 1.      C
C     DPK(6)...    phase in degree for      degree 3 and order 2.      C
C     DPK(7)...    phase in degree for      degree 3 and order 3.      C
C     DPK(8)...    phase in degree for      degree 4 and order 0.      C
C     DPK(9)...    phase in degree for      degree 4 and order 1.      C
C     DPK(10)..    phase in degree for      degree 4 and order 2.      C
C     DPK(11)..    phase in degree for      degree 4 and order 3.      C
C     DPK(12)..    phase in degree for      degree 4 and order 4.      C
C                                                                      C
C     Used routines:                                                   C
C     --------------                                                   C
C                                                                      C
C     ETLOVE: computes latitude dependent elastic parameters.          C
C                                                                      C
C     Numerical accuracy:                                              C
C     -------------------                                              C
C                                                                      C
C     The routine has been tested under operation system MS-DOS and    C
C     UNIX in double precision (8 byte words = 15 digits) using        C
C     different compilers.                                             C
C                                                                      C
C     References:                                                      C
C                                                                      C
C     WILHELM, H. and W. ZUERN 1984: Tidal forcing field.              C
C           In: LANDOLT-BOERNSTEIN, Zahlenwerte und Funktionen aus     C
C           Naturwissenschaften und Technik, New series, group V,      C
C           Vol. 2, Geophysics of the Solid Earth, the Moon and the    C
C           Planets, Berlin 1984.                                      C
C                                                                      C
C     ZUERN, W. and  H. WILHELM 1984: Tides of the solid Earth.        C
C           In: LANDOLT-BOERNSTEIN, Zahlenwerte und Funktionen aus     C
C           Naturwissenschaften und Technik, New series, group V, Vol. C
C           2, Geophysics of the Solid Earth, the Moon and the Planets,C
C           Berlin 1984.                                               C
C                                                                      C
C     Routine creation:  880129 by H.-G. Wenzel,                       C
C                        Geodaetisches Institut,                       C
C                        Universitaet Karlsruhe,                       C
C                        Englerstr. 7,                                 C
C                        D-76128 KARLSRUHE 1,                          C
C                        Germany.                                      C
C                        Tel.: 0721-6082307,                           C
C                        FAX:  0721-694552.                            C
C                        e-mail: wenzel@gik.bau-verm.uni-karlsruhe.de  C
C     Last modification: 930706 by H.-G. Wenzel.                       C
C**********************************************************************C
      IMPLICIT DOUBLE PRECISION (D)
      CHARACTER CUNIT(11)*8
      DIMENSION DGK(12),DPK(12),DGX(12),DGY(12),DGZ(12)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     COMMON /LOVE/ contains gravimeter factors, LOVE-numbers, SHIDA-  C
C     numbers and tilt factors for degree 2...4 at latitude DLAT:      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DIMENSION DGLAT(12),DHLAT(12),DKLAT(12),DLLAT(12),DTLAT(12)
      COMMON /LOVE/ DOM0,DOMR,DGLAT,DGR,DHLAT,DHR,DKLAT,DKR,DLLAT,DLR,
     1 DTLAT,DTR
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     COMMON /CONST/:                                                  C
C     DPI...        3.1415....                                         C
C     DPI2...       2.D0*DPI                                           C
C     DRAD...       DPI/180.D0                                         C
C     DRO...        180.D0/DPI                                         C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      COMMON /CONST/ DPI,DPI2,DRAD,DRO
      COMMON /UNITS/ CUNIT,IC2
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Definition of parameters of Geodetic Reference System 1980.      C
C     DEA  is major semi axis in meter.                                C
C     DEE  is square of first excentricity (without dimension).        C
C     DEGM is geocentric gravitational constant in m*3/s**2.           C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DATA DEA/6378137.00D0/,DEE/6.69438002290D-3/,DEGM1/398600.5D0/
      DEGM=DEGM1*1.D9
      IF(IPRINT.GT.0) WRITE(IUN6,7000) DEA,DEE,DEGM1
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     DCLAT is cos and DSLAT is sin of ellipsoidal latitude.           C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DCLAT=DCOS(DLAT*DRAD)
      DSLAT=DSIN(DLAT*DRAD)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Compute normal gravity in m/s**2:                                C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IF(DGRAV.LT.9.50D0) DGRAV=9.78032677D0*(1.D0+0.001931851353D0*
     1 DSLAT**2)/DSQRT(1.D0-DEE*DSLAT**2)-0.3086D-5*DH
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Compute ellipsoidal curvature radius DN in meter.               C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DN=DEA/DSQRT(1.D0-DEE*DSLAT**2)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Compute geocentric latitude DPSI in degree:                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DPSI=DRO*DATAN(((DN*(1.D0-DEE)+DH)*DSLAT)/((DN+DH)*DCLAT))
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Compute geocentric radius DR1 in meter:                          C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DR1=DSQRT((DN+DH)**2*DCLAT**2+(DN*(1.D0-DEE)+DH)**2*DSLAT**2)
      IF(IPRINT.GT.0) WRITE(IUN6,7001) DLAT,DPSI,DLON,DH,DGRAV,DR1,IC,
     1 DAZ
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Definition of astronomical parameters I.A.U. 1984.               C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DPAR=3422.448D0
      DMAS=1.D0/0.01230002D0
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Compute DOODSON's constant DDC in m**2/s**2:                     C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DR0=DEA*(1.D0-DEE/6.D0-5.D0*DEE**2/72.D0-DEE**3*55.D0/1296.D0)
      DDC=DR0**2*0.75D0*DEGM/(DEA**3*DMAS)
      DDC=DDC*(DPAR*DRAD/3600.D0)**3
      DF=DRO*3.600D-3/DGRAV
      IF(IPRINT.GT.0) WRITE(IUN6,7002) DPAR,DMAS,DDC,DF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Compute latitude dependent elastic parameters from WAHR-DEHANT-  C
C     ZSCHAU model:                                                    C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      CALL ETLOVE(IUN6,IPRINT,DLAT,DH)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     DCPSI is cos and DSPSI is sin of geocentric latitude.            C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DCPSI=DCOS(DPSI*DRAD)
      DSPSI=DSIN(DPSI*DRAD)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Compute geodetic coefficients for tidal potential, stored        C
C     provisional in array DGK.                                        C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DC2=DDC*(DR1/DR0)**2
      DC3=DDC*(DR1/DR0)**3
      DC4=DDC*(DR1/DR0)**4
      DGK(1) =DC2*0.5D0*(1.D0-3.D0*DSPSI**2)
      DGK(2) =DC2*2.D0*DSPSI*DCPSI
      DGK(3) =DC2*DCPSI**2
      DGK(4) =DC3*1.118033989D0*DSPSI*(3.D0-5.D0*DSPSI**2)
      DGK(5) =DC3*0.726184378D0*DCPSI*(1.D0-5.D0*DSPSI**2)
      DGK(6) =DC3*2.598076212D0*DSPSI*DCPSI**2
      DGK(7) =DC3*DCPSI**3
      DGK(8) =DC4*0.125000000D0*(3.D0-30.D0*DSPSI**2+35.D0*DSPSI**4)
      DGK(9) =DC4*0.473473091D0*2.D0*DSPSI*DCPSI*(3.D0-7.D0*DSPSI**2)
      DGK(10)=DC4*0.777777778D0*DCPSI**2*(1.D0-7.D0*DSPSI**2)
      DGK(11)=DC4*3.079201436D0*DSPSI*DCPSI**3
      DGK(12)=DC4*DCPSI**4
      DO 10 I=1,12
   10 DPK(I)=0.D0
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Compute geodetic coefficients for tidal acceleration vector      C
C     orientated to sperical coordinate system, stored in DGX (north), C
C     DGY (east), DGZ (radially upwards), dimensions are nm/s**2.      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DC2=DDC/DR0*(DR1/DR0)*1.D9
      DC3=DDC/DR0*(DR1/DR0)**2*1.D9
      DC4=DDC/DR0*(DR1/DR0)**3*1.D9
      DGX(1) =-DC2*3.D0*DSPSI*DCPSI
      DGX(2) = DC2*2.D0*(DCPSI**2-DSPSI**2)
      DGX(3) =-DC2*2.D0*DSPSI*DCPSI
      DGX(4) = DC3*1.118033989D0*DCPSI*(3.D0-15.D0*DSPSI**2)
      DGX(5) = DC3*0.726184378D0*DSPSI*(4.D0-15.D0*DCPSI**2)
      DGX(6) = DC3*2.598076212D0*DCPSI*(1.D0-3.D0*DSPSI**2)
      DGX(7) =-DC3*3.D0*DSPSI*DCPSI**2
      DGX(8) =-DC4*0.125000000D0*DSPSI*DCPSI*(60.D0-140.D0*DSPSI**2)
      DGX(9) = DC4*0.473473091D0*(6.D0-54.D0*DSPSI**2+56.D0*DSPSI**4)
      DGX(10)=-DC4*0.777777778D0*DCPSI*(16.D0*DSPSI-28.D0*DSPSI**3)
      DGX(11)= DC4*3.079201436D0*DCPSI**2*(4.D0*DCPSI**2-3.D0)
      DGX(12)=-DC4*4.D0*DCPSI**3*DSPSI
C
      DGY(1) = 0.D0
      DGY(2) = 2.D0*DC2*DSPSI
      DGY(3) = 2.D0*DC2*DCPSI
      DGY(4) = 0.D0
      DGY(5) = DC3*0.726184378D0*(1.D0-5.D0*DSPSI**2)
      DGY(6) = 2.D0*DC3*2.598076212D0*DSPSI*DCPSI
      DGY(7) = 3.D0*DC3*DCPSI**2
      DGY(8) = 0.D0
      DGY(9) = 2.D0*DC4*0.473473091D0*DSPSI*(3.D0-7.D0*DSPSI**2)
      DGY(10)= 2.D0*DC4*0.777777778D0*DCPSI*(1.D0-7.D0*DSPSI**2)
      DGY(11)= 3.D0*DC4*3.079201436D0*DSPSI*DCPSI**2
      DGY(12)= 4.D0*DC4*DCPSI**3
C
      DGZ(1) = DC2*(1.D0-3.D0*DSPSI**2)
      DGZ(2) = 4.D0*DC2*DSPSI*DCPSI
      DGZ(3) = 2.D0*DC2*DCPSI**2
      DGZ(4) = 3.D0*DC3*1.118033989D0*DSPSI*(3.D0-5.D0*DSPSI**2)
      DGZ(5) = 3.D0*DC3*0.726184378D0*DCPSI*(1.D0-5.D0*DSPSI**2)
      DGZ(6) = 3.D0*DC3*2.598076212D0*DSPSI*DCPSI**2
      DGZ(7) = 3.D0*DC3*DCPSI**3
      DGZ(8) = 4.D0*DC4*0.125000000D0*(3.D0-30.D0*DSPSI**2+35.D0*
     1 DSPSI**4)
      DGZ(9) = 8.D0*DC4*0.473473091D0*DSPSI*DCPSI*(3.D0-7.D0*DSPSI**2)
      DGZ(10)= 4.D0*DC4*0.777777778D0*DCPSI**2*(1.D0-7.D0*DSPSI**2)
      DGZ(11)= 4.D0*DC4*3.079201436D0*DSPSI*DCPSI**3
      DGZ(12)= 4.D0*DC4*DCPSI**4
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Compute geodetic coefficients for tidal acceleration vector      C
C     orientated to ellipsoidal coordinate system stored in            C
C     DGX (north), DGY (east) and DGZ (upwards), all in nm/s**2.       C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DCDLAT=DCLAT*DCPSI+DSLAT*DSPSI
      DSDLAT=DSLAT*DCPSI-DCLAT*DSPSI
      DO 50 I=1,12
      DUMMY =DCDLAT*DGX(I)-DSDLAT*DGZ(I)
      DGZ(I)=DSDLAT*DGX(I)+DCDLAT*DGZ(I)
      DGX(I)=DUMMY
   50 CONTINUE
      IC2=IC+2
      DCAZ=DCOS(DAZ*DRAD)
      DSAZ=DSIN(DAZ*DRAD)
      GOTO(100,200,300,400,500,600,700,800,900,1000,1100),IC2
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     IC=-1, compute geodetic coefficients for tidal potential.        C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
  100 CONTINUE
      GOTO 2000
  200 CONTINUE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     IC=0, compute geodetic coefficients for vertical component       C
C           (gravity tide).                                            C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DO 210 I=1,12
      DGK(I)=DGZ(I)
  210 DPK(I)=180.0D0
      GOTO 2000
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     IC=1, compute geodetic coefficients for horizontal component     C
C           (tidal tilt) in azimuth DAZ, in mas.                       C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
  300 CONTINUE
      DO 310 I=1,12
      DGK(I)=DSQRT((DGX(I)*DCAZ)**2+(DGY(I)*DSAZ)**2)*DF
      DPK(I)=0.D0
      IF(DGX(I)*DCAZ.EQ.0.D0.AND.DGY(I)*DSAZ.EQ.0.D0) GOTO 310
      DPK(I)=DRO*DATAN2(DGY(I)*DSAZ,DGX(I)*DCAZ)
  310 CONTINUE
      GOTO 2000
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     IC=2, compute geodetic coefficients for vertical displacement    C
C           in mm.                                                     C
C     Attention: this component has never been tested !!!              C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
  400 CONTINUE
      DFAK=1.D3/DGRAV
      DO 410 I=1,12
      DGK(I)=DGK(I)*DHLAT(I)*DFAK
  410 DPK(I)=0.0D0
      WRITE(IUN6,*) '*****The component',IC,' has never been tested !!!'
      WRITE(*,*)    '*****The component',IC,' has never been tested !!!'
      GOTO 2000
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     IC=3, compute geodetic coefficients for horizontal displacement  C
C           in azimuth DAZ in mm.                                      C
C     Attention: this component has never been tested !!!              C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
  500 CONTINUE
      DFAK=1.D3*DR1/DGRAV
      DO 510 I=1,12
      DGK(I)=DSQRT((DGX(I)*DCAZ)**2+(DGY(I)*DSAZ)**2)*DLLAT(I)*DFAK
      DPK(I)=0.D0
      IF(DGX(I)*DCAZ.EQ.0.D0.AND.DGY(I)*DSAZ.EQ.0.D0) GOTO 510
      DPK(I)=DRO*DATAN2(DGY(I)*DSAZ,DGX(I)*DCAZ)
  510 CONTINUE
      WRITE(IUN6,*) '*****The component',IC,' has never been tested !!!'
      WRITE(*,*)    '*****The component',IC,' has never been tested !!!'
      GOTO 2000
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     IC=4, compute geodetic coefficients for vertical strain at the   C
C           Earth's deformed surface in 10**-9 units = nstr.           C
C           We use a spherical approximation for the vertical strain,  C
C           i.e. eps(rr) , and a POISSON ratio of 0.25 (see ZUERN and  C
C           WILHELM 1984, p. 282).                                     C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
  600 CONTINUE
      DPOISS=0.25D0 
      DFAK=1.D9*DPOISS/(DPOISS-1.D0)
      DO 610 I=1,3
  610 DGK(I)=DGK(I)*DFAK*(2.D0*DHLAT(I)-2.D0*3.D0*DLLAT(I))/(DGRAV*DR1)
      DO 620 I=4,7
  620 DGK(I)=DGK(I)*DFAK*(2.D0*DHLAT(I)-3.D0*4.D0*DLLAT(I))/(DGRAV*DR1)
      DO 630 I=8,12
  630 DGK(I)=DGK(I)*DFAK*(2.D0*DHLAT(I)-4.D0*5.D0*DLLAT(I))/(DGRAV*DR1)
      DO 640 I=1,12
  640 DPK(I)=0.0D0
      GOTO 2000
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     IC=5, compute geodetic coefficients for horizontal strain        C
C           in azimuth DAZ, in 10**-9 units.                           C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
  700 CONTINUE
      DTHETA=(90.D0-DPSI)*DRAD
      DAZR=(DAZ+180.D0)*DRAD
      DCAZ =DCOS(DAZR)
      DSAZ =DSIN(DAZR)
      DSAZ2=DSIN(2.D0*DAZR)
      DCSTS=-0.5D0*DSIN(2.D0*DAZR)
      DCT=DSPSI
      DST=DCPSI
      DCT2=DCT*DCT
      DST2=DST*DST
      DCC2=DCOS(2.D0*DPSI*DRAD)
      DC2T=-DCC2
      DCOTT =1.D0/DTAN(DTHETA)
      DCOTT2=1.D0/DTAN(2.D0*DTHETA)
      DFAK=1.D9/(DR1*DGRAV)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Real part is stored in DGX, imaginary part is stored in DGY.     C
C     Formulas were given by Dr. W. Zuern, BFO Schiltach (personal     C
C     communication) and tested against horizontal strain computed     C
C     (with lower precision) by program ETIDEL.                        C
C     Results agreed to 0.3 % and 0.1 degree for most of the waves,    C
C     except for 2N2 and L2 (deviation of 3 %).                        C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DGX(1)=(DHLAT(1)-(6.D0*DLLAT(1)*DC2T)/(3.D0*DCT2-1.D0))*DCAZ**2
     1      +(DHLAT(1)-(6.D0*DLLAT(1)*DCT2)/(3.D0*DCT2-1.D0))*DSAZ**2
      DGY(1)=0.D0
      DGX(2)=(DHLAT(2)-4.D0*DLLAT(2))*DCAZ**2+(DHLAT(2)-DLLAT(2)/DST2
     1 +2.D0*DLLAT(2)*DCOTT*DCOTT2)*DSAZ**2
      DGY(2)=2.D0*DLLAT(2)*(2.D0*DCOTT2-DCOTT)*DCSTS/DST
      DGX(3)=(DHLAT(3)+2.D0*DLLAT(3)*(DCOTT*DCOTT-1.D0))*DCAZ**2
     1 +(DHLAT(3)-4.D0*DLLAT(3)/DST2+2.D0*DLLAT(3)*DCOTT*DCOTT)*DSAZ**2
      DGY(3)=4.D0*DLLAT(3)*DCOTT*DCSTS/DST
      DGX(4)=(DHLAT(4)+DLLAT(4)*(33.D0-45.D0*DCT2)/(5.D0*DCT2-3.D0))*
     1 DCAZ**2+(DHLAT(4)-DLLAT(4)*(1.D0+10.D0*DCT2/(5.D0*DCT2-3.D0)))*
     2 DSAZ**2
      DGY(4)=0.D0
      DGX(5)=(DHLAT(5)-DLLAT(5)*(1.D0+10.D0*(1.D0-4.D0*DCT2)/
     1 (1.D0-5.D0*DCT2)))*DCAZ**2+(DHLAT(5)+DLLAT(5)*
     2 (DCOTT*DCOTT-1.D0/DST2-10.D0*DCT2/(5.D0*DCT2-1.D0)))*DSAZ**2
      DGY(5)=-20.D0*DLLAT(5)*DCT*DCSTS/(5.D0*DCT2-1.D0)
      DGX(6)=(DHLAT(6)+DLLAT(6)*(2.D0*DCOTT*DCOTT-7.D0))*DCAZ**2
     1 +(DHLAT(6)+DLLAT(6)*(2.D0*DCOTT*DCOTT-1.D0-4.D0/DST2))*DSAZ**2
      DGY(6)=-4.D0*DLLAT(6)*(DCOTT-1.D0/DCOTT)*DCSTS/DST
      DGX(7)=(DHLAT(7)+DLLAT(7)*(6.D0*DCOTT*DCOTT-3.D0))*DCAZ**2
     1 +(DHLAT(7)+DLLAT(7)*(3.D0*DCOTT*DCOTT-9.D0/DST2))*DSAZ**2
      DGY(7)=12.D0*DLLAT(7)*DCOTT*DCSTS/DST
      DGX(8)=(DHLAT(8)-4.D0*DLLAT(8)*(4.D0-3.D0*(5.D0*DCT2-1.D0)/
     1 (35.D0*DCT2*DCT2-30.D0*DCT2+3.D0)))*DCAZ**2+
     2 (DHLAT(8)-4.D0*DLLAT(8)*(1.D0+3.D0*(5.D0*DCT2-1.D0)/
     3 (35.D0*DCT2*DCT2-30.D0*DCT2+3.D0)))*DSAZ**2
      DGY(8)=0.D0
      DGX(9)=  (DHLAT(9)-2.D0*DLLAT(9)*(8.D0-3.D0/(7.D0*DCT2-3.D0)))*
     1 DCAZ**2+(DHLAT(9)-2.D0*DLLAT(9)*(2.D0+3.D0/(7.D0*DCT2-3.D0)))*
     2 DSAZ**2
      DGY(9)=DLLAT(9)*3.D0/DCT*(1.D0+2.D0/(7.D0*DCT2-3.D0))*DSAZ2
      DGX(10)=(DHLAT(10)-4.D0*DLLAT(10)*(4.D0+3.D0*DCT2/
     1 (7.D0*DCT2**2-8.D0*DCT2+1.D0)))*DCAZ**2
     2       +(DHLAT(10)-4.D0*DLLAT(10)*(1.D0-3.D0*DCT2/
     2 (7.D0*DCT2**2-8.D0*DCT2+1.D0)))*DSAZ**2
      DGY(10)=-DLLAT(10)*6.D0*DCT/DST**2*(1.D0-4.D0/(7.D0*DCT2-1.D0))*
     1 DSAZ2 
      DGX(11)=(DHLAT(11)-2.D0*DLLAT(11)*(8.D0-3.D0/DST2))*DCAZ**2
     1       +(DHLAT(11)-2.D0*DLLAT(11)*(2.D0+3.D0/DST2))*DSAZ**2
      DGY(11)= DLLAT(11)*3.D0/DCT*(3.D0-2.D0/DST2)*DSAZ2
      DGX(12)=(DHLAT(12)-4.D0*DLLAT(12)*(4.D0-3.D0/DST2))*DCAZ**2
     1       +(DHLAT(12)-4.D0*DLLAT(12)*(1.D0+3.D0/DST2))*DSAZ**2
      DGY(12)= DLLAT(12)*12.D0*DCT/DST2*DSAZ2
      DO 710 I=1,12
      DGK(I)=DGK(I)*DSQRT(DGX(I)**2+DGY(I)**2)*DFAK
  710 DPK(I)=DPK(I)+DATAN2(DGY(I),DGX(I))*DRO
      GOTO 2000
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     IC=6, compute geodetic coefficients for areal strain             C
C           in 10**-9 units = nstr.                                    C
C           We use a spherical approximation for the aereal strain,    C
C           i.e. eps(t,t) + eps(l,l), (see ZUERN and WILHELM 1984,     C
C           p. 282).                                                   C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
  800 CONTINUE
      DO 810 I=1,3
  810 DGK(I)=DGK(I)*(2.D0*DHLAT(I)-2.D0*3.D0*DLLAT(I))/(DGRAV*DR1)*1.D9
      DO 820 I=4,7
  820 DGK(I)=DGK(I)*(2.D0*DHLAT(I)-3.D0*4.D0*DLLAT(I))/(DGRAV*DR1)*1.D9
      DO 830 I=8,12
  830 DGK(I)=DGK(I)*(2.D0*DHLAT(I)-4.D0*5.D0*DLLAT(I))/(DGRAV*DR1)*1.D9
      DO 840 I=1,12
  840 DPK(I)=0.0D0
      GOTO 2000
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     IC=7, compute geodetic coefficients for shear tidal strain       C
C           at the Earth's deformed surface in 10**-9 units = nstr.    C
C           We use a spherical approximation, i.e. eps(t,l)            C
C     Attention: this component has never been tested !!!!             C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
  900 CONTINUE
      DTHETA=(90.D0-DPSI)*DRAD
      DAZR=(DAZ+180.D0)*DRAD
      DCAZ =DCOS(DAZR)
      DSAZ =DSIN(DAZR)
      DSAZ2=DSIN(2.D0*DAZR)
      DCSTS=-0.5D0*DSIN(2.D0*DAZR)
      DCT=DSPSI
      DST=DCPSI
      DCT2=DCT*DCT
      DST2=DST*DST
      DCC2=DCOS(2.D0*DPSI*DRAD)
      DC2T=-DCC2
      DCOTT =1.D0/DTAN(DTHETA)
      DCOTT2=1.D0/DTAN(2.D0*DTHETA)
      DFAK=1.D9/(DR1*DGRAV)
      DGY(1)=0.D0
      DGY(2)=2.D0*DLLAT(2)*(2.D0*DCOTT2-DCOTT)*DCSTS/DST
      DGY(3)=4.D0*DLLAT(3)*DCOTT*DCSTS/DST
      DGY(4)=0.D0
      DGY(5)=-20.D0*DLLAT(5)*DCT*DCSTS/(5.D0*DCT2-1.D0)
      DGY(6)=-4.D0*DLLAT(6)*(DCOTT-1.D0/DCOTT)*DCSTS/DST
      DGY(7)=12.D0*DLLAT(7)*DCOTT*DCSTS/DST
      DGY(8)=0.D0
      DGY(9)=DLLAT(9)*3.D0/DCT*(1.D0+2.D0/(7.D0*DCT2-3.D0))*DSAZ2
      DGY(10)=-DLLAT(10)*6.D0*DCT/DST**2*(1.D0-4.D0/(7.D0*DCT2-1.D0))*
     1 DSAZ2 
      DGY(11)=DLLAT(11)*3.D0/DCT*(3.D0-2.D0/DST2)*DSAZ2
      DGY(12)=DLLAT(12)*12.D0*DCT/DST2*DSAZ2
      DO 910 I=1,12
      DGK(I)=DGK(I)*DGY(I)*DFAK
  910 DPK(I)=0.D0
      WRITE(IUN6,*) ' ***** The shear strain has never been tested !!!'
      WRITE(*,*)    ' ***** The shear strain has never been tested !!!'
      GOTO 2000
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     IC=8, compute geodetic coefficients for volume strain            C
C           at the Earth's deformed surface in 10**-9 units = nstr.    C
C           We use a spherical approximation, i.e. eps(t,t)+eps(l,l).  C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 1000 CONTINUE
      DPOISS=0.25D0 
      DFAK=1.D9*(1.D0-2.D0*DPOISS)/(1.D0-DPOISS)
      DO 1010 I=1,3
 1010 DGK(I)=DGK(I)*DFAK*(2.D0*DHLAT(I)-2.D0*3.D0*DLLAT(I))/(DGRAV*DR1)
      DO 1020 I=4,7
 1020 DGK(I)=DGK(I)*DFAK*(2.D0*DHLAT(I)-3.D0*4.D0*DLLAT(I))/(DGRAV*DR1)
      DO 1030 I=8,12
 1030 DGK(I)=DGK(I)*DFAK*(2.D0*DHLAT(I)-4.D0*5.D0*DLLAT(I))/(DGRAV*DR1)
      DO 1040 I=1,12
 1040 DPK(I)=0.0D0
      GOTO 2000
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     IC=9, compute geodetic coefficients for ocean tides in mm:       C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 1100 CONTINUE
      DFAK=1.D3/DGRAV
      DO 1110 I=1,12
      DGK(I)=DGK(I)*DFAK
 1110 DPK(I)=0.0D0
 2000 CONTINUE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Common loop for all components.                                  C
C     For negative geodetic coefficients, use absolute value           C
C     and add 180 deg to the phase.                                    C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC  
      DO 2010 I=1,12
      IF(DGK(I).GE.0.D0) GOTO 2010
      DGK(I)=-DGK(I)
      DPK(I)=DPK(I)+180.D0
      IF(DPK(I).LT.0.D0) DPK(I)=DPK(I)+360.D0
 2010 CONTINUE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Print geodetic coefficients:                                     C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IF(IPRINT.EQ.0) RETURN
      WRITE(IUN6,7003) IC,DAZ,(DGK(I),CUNIT(IC2),DPK(I),I=1,12)
 5000 CONTINUE
      IF(IPRINT.GT.0) WRITE(IUN6,7005)
      RETURN
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Format statements:                                               C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 7000 FORMAT(' Routine ETGCOF, version 930706 FTN 77.'//
     1' Computation of geodetic coefficients'//
     3' Parameters of Geodetic Reference System 1980:'/
     4' Major semi axis                  ',F12.0,'  m'/
     5' 1. excentricity                  ',F12.8/
     6' Geocentric gravitational constant',F12.1,' 10**9 m**3/s**2'/)
 7001 FORMAT(' Station paremeters:'//
     1' Latitude                       ',F12.6,' deg'/
     2' Geocentric latitude            ',F12.6,' deg'/
     3' Longitude                      ',F12.6,' deg'/
     4' Height                         ',F12.3,' m'/
     5' Gravity                        ',F12.6,' m/s**2'/
     6' Geocentric radius              ',F12.3,' m'/
     7' Component of observations      ',I12/
     8' Azimuth from north direction   ',F12.6,' deg'//)
 7002 FORMAT(' Astronomic constants International Astronomic Union',
     1' 1984 m.kg.s-system'/
     2' Moons mean sine parallax  ',F12.6/
     3' Mass relation Earth/Moon  ',F12.6//
     4' DOODSONs constant         ',F12.6,' m**2/s**2'/
     6' F                         ',F12.6,' mas/(nm/s**2)'//)
 7003 FORMAT(//' Geodetic coefficients and phases for component',I4/
     1' azimuth:',F12.6,' degree'//
     2' GC 2,0',F14.5,2X,A8,2X,F14.6,' deg'/
     3' GC 2,1',F14.5,2X,A8,2X,F14.6,' deg'/
     4' GC 2,2',F14.5,2X,A8,2X,F14.6,' deg'/
     5' GC 3,0',F14.5,2X,A8,2X,F14.6,' deg'/
     6' GC 3,1',F14.5,2X,A8,2X,F14.6,' deg'/
     7' GC 3,2',F14.5,2X,A8,2X,F14.6,' deg'/
     8' GC 3,3',F14.5,2X,A8,2X,F14.6,' deg'/
     9' GC 4,0',F14.5,2X,A8,2X,F14.6,' deg'/
     *' GC 4,1',F14.5,2X,A8,2X,F14.6,' deg'/
     1' GC 4,2',F14.5,2X,A8,2X,F14.6,' deg'/
     2' GC 4,3',F14.5,2X,A8,2X,F14.6,' deg'/
     3' GC 4,4',F14.5,2X,A8,2X,F14.6,' deg'//)
 7005 FORMAT(///' ***** Routine ETGCOF finished the execution.'/)
      END
C
      SUBROUTINE ETGREG(ITY,ITM,ITD,DTH)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C     Routine ETGREG, version 930819 FORTRAN 77.                       C
C                                                                      C
C     The routine ETGREG computes the GREGORIAN date from year,        C
C     month, day and hour, where the hour may exceed 24.               C
C                                                                      C
C     Input/output parameter description:                              C
C     -----------------------------------                              C
C                                                                      C
C     All following parameters are input and output parameters,        C
C     which measn that the parameters may be changed during            C
C     the execution of routine ETGREI.                                 C
C                                                                      C
C     ITY...       year  in INTEGER form, E.G. 1971                    C
C     ITM...       month in INTEGER form, E.G. 1 = January.            C
C     ITD...       day   in INTEGER form.                              C
C     DTH...       hour  in DOUBLE PRECISION (UTC). DTH may exceed 24. C
C                                                                      C
C     Routine creation:  710523 by H.-G. Wenzel.                       C
C                        Geodaetisches Institut,                       C
C                        Universitaet Karlsruhe,                       C
C                        Englerstr. 7,                                 C
C                        D-76128 KARLSRUHE 1,                          C
C                        Germany.                                      C
C                        Tel: 0049-721-6082307,                        C
C                        FAX: 0049-721-694552.                         C
C                        e-mail: wenzel@gik.bau-verm.uni-karlsruhe.de  C
C     Last modification: 930819 by H.-G. Wenzel.                       C
C**********************************************************************C
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT INTEGER*4 (I-N)
      INTEGER*2 ID1(12),ID2(12)
      SAVE ID1,ID2
      DATA ID1/31,28,31,30,31,30,31,31,30,31,30,31/
      DATA ID2/31,29,31,30,31,30,31,31,30,31,30,31/
      IH=DTH/24
      DTH=DTH-24.D0*DBLE(IH)
      ITD=ITD+IH
   50 L=ITY/4
      L=4*L
      IF(L.EQ.ITY) GOTO 300
  100 IF(ITD.LE.ID1(ITM)) GOTO 555
      ITD=ITD-ID1(ITM)
      ITM=ITM+1
      IF(ITM.EQ.13) GOTO 200
      GOTO 100
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Next year:                                                       C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
  200 ITM=1
      ITY=ITY+1
      GOTO 50
  300 IF(ITD.LE.ID2(ITM)) GOTO 555
      ITD=ITD-ID2(ITM)
      ITM=ITM+1
      IF(ITM.EQ.13) GOTO 200
      GOTO 300
  555 RETURN
      END
C
      SUBROUTINE ETJULD(IUN6,ITY,ITM,ITD,DTH,DJULD)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C     Routine ETJULD, version 930503 FORTRAN 77.                       C
C                                                                      C
C     The routine ETJULD computes the Julian date from year,           C
C     month, days and hours.                                           C
C                                                                      C
C     Input parameter description:                                     C
C     ----------------------------                                     C
C                                                                      C
C     IUN6...      formatted printer unit.                             C
C     ITY...       year  in integer form, e.g. 1971                    C
C     ITM...       month in INTEGER form, e.g. 1 = JANUARY.            C
C     ITD...       day   in integer form.                              C
C     DTH...       double precision hour  in UTC.                      C
C                                                                      C
C     Output parameter description:                                    C
C     -----------------------------                                    C
C                                                                      C
C     DJULD...     double precision Julian date, e.g. for              C
C                  01. January  1988, 0.00 H UTC is DJULD=2447161.500  C
C                  01. February 1988, 0.00 H UTC is DJULD=2447192.500  C
C                  29. February 1988, 0.00 H UTC is DJULD=2447220.500  C
C                  01. March    1988, 0.00 H UTC is DJULD=2447221.500  C
C                                                                      C
C     Execution time:                                                  C
C     ---------------                                                  C
C                                                                      C
C     About 0.0062 sec CPU time for 1000 calls of routine ETJULD on    C
C     80486 DX2 66 Mhz processor under operation system MS-DOS using   C
C     the LAHEY FORTRAN-compiler.                                      C
C                                                                      C
C     Numerical accuracy:                                              C
C     -------------------                                              C
C                                                                      C
C     The result under operation system MS-DOS is accurate to          C
C     0.000 035 s which is equivalent to 4*E-11 days.                  C
C                                                                      C
C     Program creation:  710523 by H.-G. Wenzel,                       C
C                        Geodaetisches Institut,                       C
C                        Universitaet Karlsruhe,                       C
C                        Englerstr. 7,                                 C
C                        D-76128 KARLSRUHE 1,                          C
C                        Germany.                                      C
C                        Tel.: 0721-6082301.                           C
C                        FAX:  0721-694552.                            C
C                        e-mail: wenzel@gik.bau-verm.uni-karlsruhe.de  C
C     Last Modification: 930503 by H.-G.Wenzel.                        C
C**********************************************************************C
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT INTEGER*4 (I-N)
      INTEGER*2 ITDY(12)
      SAVE ITDY
      DATA ITDY/0,31,59,90,120,151,181,212,243,273,304,334/
      DJULD=2415019.5D0
      ITYR=ITY-1900
      ILEAP=ITYR/4
      ITA=ITYR*365+ILEAP
      IF(ITM.LT.1) GOTO 5000
      IF(ITM.GT.12) GOTO 5010
      DJULD=DJULD+DBLE(ITA+ITDY(ITM)+ITD)+DTH/24.D0
      ITYL=ILEAP*4
      IF(ITYR.NE.ITYL) RETURN
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Leap year:                                                       C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IF(ITM.GT.2) RETURN
      DJULD=DJULD-1.D0
      RETURN
 5000 WRITE(IUN6,7050) ITY,ITM,ITD,DTH
      WRITE(*,7050)    ITY,ITM,ITD,DTH
      STOP
 5010 WRITE(IUN6,7051) ITY,ITM,ITD,DTH
      WRITE(*,7051)    ITY,ITM,ITD,DTH
      STOP
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Format statements:                                               C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 7050 FORMAT(/' *****Error in routine ETJULD, version 930503 FTN77.'/
     1' *****Month is less 1:',2X,3I4,F12.3/
     2' *****Routine ETJULD stops the execution.'/)
 7051 FORMAT(/' *****Error in routine ETJULD, version 930503 FTN77.'/
     1' *****Month is greater 12:',2X,3I4,F12.3/
     2' *****Routine ETJULD stops the execution.'/)
      END
C
      SUBROUTINE ETLOVE(IUN6,IPRINT,DLAT,DELV)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C     Routine ETLOVE, version 930706 FORTRAN 77.                       C
C                                                                      C
C     The routine computes latitude dependent LOVE-numbers DH, DK,     C
C     SHIDA-numbers DL, gravimeter factors DG and tilt factors DT      C
C     using the WAHR-DEHANT-ZSCHAU model.                              C
C                                                                      C
C     Body tide amplitude factors for WAHR-DEHANT-ZSCHAU model.        C 
C     The NDFW resonance is approximated by                            C
C                                                                      C 
C     G(RES) = GLAT - GR*(DOM - DOM0)/(DOMR - DOM).                    C
C                                                                      C
C     similar equations hold for the other parameters.                 C
C                                                                      C 
C     Gravimetric amplitude factors, LOVE numbers h and k for degree   C
C     0...3 have been taken from DEHANT 1987, Table 7, 8 and 9         C
C     for an elliptical, uniformly rotating, oceanless Earth with      C
C     liquid outer core and inelastic mantle (PREM Earth model with    C
C     inelastic mantle from ZSCHAU) and for the fourth degree from     C
C     DEHANT et. al 1989, Table 6). The resonance factors GR have      C
C     been computed to fit the difference between body tide amplitude  C
C     factors at O1 and PSI1 from DEHANT 1987, PREM model with         C
C     elastic mantle (Table 1...3). The NDFW resonance frequency is    C
C     15.073729 degree per hour = 1.004915267 CPD UT, taken from       C
C     WAHR 1981 (because it is not given in DEHANT's papers).          C
C                                                                      C
C     Input parameter description:                                     C
C     ----------------------------                                     C
C                                                                      C
C     IUN6...      formatted line printer unit.                        C
C     IPRINT...    printout parameter. For IPRINT=1, the computed      C
C                  LOVE- and SHIDA- number s will be printed.          C
C     DLAT...      ellipsoidal latitude in degree.                     C
C     DELV...      ellipsoidal height in meter.                        C
C                                                                      C
C     Description of COMMON /LOVE/:                                    C
C     -----------------------------                                    C
C                                                                      C
C     DOM0...      frequency of O1 in degree per hour.                 C
C     DOMR...      frequency of the FCN eigenfrequency in degree per   C
C                  hour.                                               C
C     DGLAT...     array(1..12) containing the gravimetric factors at  C
C                  latitude DLAT.                                      C
C     DGR...       resonance factor for gravimetric factors.           C
C     DHLAT        array(1..12) containing the LOVE-numbers h at       C
C                  latitude DLAT.                                      C
C     DHR...       resonance factor for the LOVE-number h(2,1).        C
C     DKLAT        array(1..12) containing the LOVE-numbers k at       C
C                  latitude DLAT.                                      C
C     DKR...       resonance factor for the LOVE-number k(2,1).        C
C     DLLAT...     array(1..12) containing the SHIDA-numbers l at      C
C                  latitude DLAT.                                      C
C     DLR...       resonance factor for the SHIDA-number l(2,1).       C 
C     DTLAT...     array(1..12) containing the tilt factors at         C
C                  latitude DLAT.                                      C
C                                                                      C 
C     Reference:                                                       C
C     ----------                                                       C
C                                                                      C
C                                                                      C
C     DEHANT, V. 1987: Tidal Parameters for an Inelastic Earth.        C
C           Physics of the Earth and Planetary Interiors, 49, 97-116,  C
C           1987.                                                      C
C                                                                      C
C     WAHR, J.M. 1981: Body tides on an elliptical, rotating, elastic  C
C           and oceanless earth. Geophysical Journal of the Royal      C
C           astronomical Society, vol. 64, 677-703, 1981.              C
C                                                                      C
C     ZSCHAU, J. and R. WANG 1987: Imperfect elasticity in the Earth's C
C           mantle. Implications for Earth tides and long period       C
C           deformations. Proceedings of the 9th International Sym-    C
C           posium on Earth Tides, New York 1987, pp. 605-629, editor  C
C           J.T. KUO, Schweizerbartsche Verlagsbuchhandlung, Stuttgart C
C           1987.                                                      C
C                                                                      C
C                                                                      C
C     Routine creation:  930703 by H.-G. Wenzel.                       C
C                        Geodaetisches Institut,                       C
C                        Universitaet Karlsruhe,                       C
C                        Englerstr. 7,                                 C
C                        D-76128 KARLSRUHE 1,                          C
C                        Germany.                                      C
C                        Tel: 0049-721-6082307,                        C
C                        FAX: 0049-721-694552.                         C
C                        e-mail: wenzel@gik.bau-verm.uni-karlsruhe.de  C
C     Last modification: 930706 by H.-G. Wenzel.                       C
C**********************************************************************C
      IMPLICIT DOUBLE PRECISION (D)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     The following DIMENSION statement is concerning the elastic      C
C     Earth model for the different degree and order constituents.     C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DOUBLE PRECISION DG0(12),DGP(12),DGM(12)
      DOUBLE PRECISION DH0(12),DHP(12),DHM(12)
      DOUBLE PRECISION DK0(12),DKP(12),DKM(12)
      DOUBLE PRECISION DL0(12),DLP(12),DLM(12)
      DOUBLE PRECISION DLATP(12),DLATM(12)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     COMMON /LOVE/ contains gravimeter factors, LOVE-numbers, SHIDA-  C
C     numbers and tilt factors for degree 2...4 at latitude DLAT:      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DIMENSION DGLAT(12),DHLAT(12),DKLAT(12),DLLAT(12),DTLAT(12)
      COMMON /LOVE/ DOM0,DOMR,DGLAT,DGR,DHLAT,DHR,DKLAT,DKR,DLLAT,DLR,
     1 DTLAT,DTR
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     COMMON /CONST/:                                                  C
C     DPI...        3.1415....                                         C
C     DPI2...       2.D0*DPI                                           C
C     DRAD...       DPI/180.D0                                         C
C     DRO...        180.D0/DPI                                         C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      COMMON /CONST/ DPI,DPI2,DRAD,DRO
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     The following DATA statements are concerning the elastic         C
C     Earth model for the different degree and order constituents.     C
C     The latitude dependency is not given for all constituents in     C
C     the WAHR-DEHANT-ZSCHAU model !!!!!!                              C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DATA DG0/1.1576D0,1.1542D0,1.1600D0,1.0728D0,1.0728D0,1.0728D0,
     1 1.0728D0,1.0363D0,1.0363D0,1.0363D0,1.0363D0,1.0363D0/
      DATA DGP/-0.0016D0,-0.0018D0,-0.0010D0,0.D0,0.D0,0.D0,-0.0010D0,
     1 0.D0,0.D0,0.D0,0.D0,-0.000315D0/
      DATA DGM/0.0054D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,
     1 0.D0,0.D0/
      DATA DH0/0.6165D0,0.6069D0,0.6133D0,0.2946D0,0.2946D0,0.2946D0,
     1 0.2946D0,0.1807D0,0.1807D0,0.1807D0,0.1807D0,0.1807D0/
      DATA DHP/0.0007D0,0.0007D0,0.0005D0,0.D0,0.D0,0.D0,0.0003D0,
     1 0.D0,0.D0,0.D0,0.D0,0.00015D0/
      DATA DHM/0.0018D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,
     1 0.D0,0.D0/
      DATA DK0/0.3068D0,0.3009D0,0.3034D0,0.0942D0,0.0942D0,0.0942D0,
     1 0.0942D0,0.0427D0,0.0427D0,0.0427D0,0.0427D0,0.0427D0/
      DATA DKP/0.0015D0,0.0014D0,0.0009D0,0.D0,0.D0,0.D0,0.0007D0,
     1 0.D0,0.D0,0.D0,0.D0,0.00066D0/
      DATA DKM/-0.0004D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,
     1 0.D0,0.D0/
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     SHIDA-numbers:                                                   C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DATA DL0/ 0.0840D0,0.0841D0,0.0852D0,0.0149D0,0.0149D0,0.0149D0,
     1 0.0149D0,0.0100D0,0.0100D0,0.0100D0,0.0100D0,0.0100D0/
      DATA DLP/-0.002D0,-0.002D0,-0.001D0,0.0000D0,0.0000D0,0.0000D0,
     1 0.0000D0,0.0000D0,0.0000D0,0.0000D0,0.0000D0,0.0000D0/
      DATA DLM/ 0.0000D0,0.0000D0,0.0000D0,0.0000D0,0.0000D0,0.0000D0,
     1 0.0000D0,0.0000D0,0.0000D0,0.0000D0,0.0000D0,0.0000D0/
      DATA DLATP/0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,
     1 0.D0,0.D0/
      DATA DLATM/0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,
     1 0.D0,0.D0/
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Definition of parameters of Geodetic Reference System 1980.      C
C     DEA  is major semi axis in meter.                                C
C     DEE  is square of first excentricity (without dimnension).       C
C     DEGM is geocentric gravitational constant in m*3/s**2.           C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DATA DEA/6378137.00D0/,DEE/6.69438002290D-3/
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Define resonance frequency and resonance factors:                C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DOMR=15.073729D0
      DOM0=13.943036D0
      DGR =-0.000625D0
      DHR =-0.002505D0
      DKR =-0.001261D0
      DLR =0.0000781D0
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     DCLAT is cos and DSLAT is sin of ellipsoidal latitude.           C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DCLAT=DCOS(DLAT*DRAD)
      DSLAT=DSIN(DLAT*DRAD)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Compute ellipsoidal curvature radius DN in meter.               C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DN=DEA/DSQRT(1.D0-DEE*DSLAT**2)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Compute geocentric latitude DPSI in degree:                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DPSI=DRO*DATAN(((DN*(1.D0-DEE)+DELV)*DSLAT)/((DN+DELV)*DCLAT))
      DTHET=90.D0-DPSI
      DCT=DCOS(DTHET*DRAD)
      DCT2=DCT*DCT
      DLATP(1)=0.335410D0*(35.D0*DCT2*DCT2-30.D0*DCT2+3.D0)/
     1 (3.D0*DCT2-1.D0)
      DLATM(1) =0.894427D0/(3.D0*DCT2-1.D0)
      DLATP(2) =0.612372D0*(7.D0*DCT2-3.D0)
      DLATP(3) =0.866025D0*(7.D0*DCT2-1.D0)
      DLATP(7) =0.829156D0*(9.D0*DCT2-1.D0)
      DLATP(12)=0.806226D0*(11.D0*DCT2-1.D0)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Compute latitude dependent gravimeter factors DG:                C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DO 110 I=1,12
  110 DGLAT(I)=DG0(I)+DGP(I)*DLATP(I)+DGM(I)*DLATM(I)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Compute latitude dependent LOVE-numbers DH (for vertical         C
C     displacement):                                                   C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DO 120 I=1,12
  120 DHLAT(I)=DH0(I)+DHP(I)*DLATP(I)+DHM(I)*DLATM(I)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Compute latitude dependent LOVE-numbers DK:                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DO 130 I=1,12
  130 DKLAT(I)=DK0(I)+DKP(I)*DLATP(I)+DKM(I)*DLATM(I)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Compute latitude dependent SHIDA-numbers DL:                     C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DO 140 I=1,12
  140 DLLAT(I)=DL0(I)+DLP(I)*DLATP(I)+DLM(I)*DLATM(I)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Compute latitude dependent tilt factors DT:                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DO 150 I=1,12
      DTLAT(I)=1.D0+DK0(I)-DH0(I)+DLATP(I)*(DKP(I)-DHP(I))+
     1 DLATM(I)*(DKM(I)-DHM(I))
  150 CONTINUE
      DTR=DKR-DHR
      IF(IPRINT.EQ.0) RETURN
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Print out of parameters:                                         C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      WRITE(IUN6,7001) DOM0,DOMR,DGR,DHR,DKR,DLR,DTR
      I=0
      WRITE(IUN6,7002) DLAT
      DO 300 L=2,4
      WRITE(IUN6,7004)
      DO 300 M=0,L 
      I=I+1
      WRITE(IUN6,7003)  L,M,DGLAT(I),DHLAT(I),DKLAT(I),DLLAT(I),DTLAT(I)
  300 CONTINUE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Format statements:                                               C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 7001 FORMAT(/' Routine ETLOVE, version 930706 FTN77.'/
     1' Latitude dependent parameters for an elliptical, rotating,'/
     2' inelastic and oceanless Earth from WAHR-DEHANT-ZSCHAU model.'//
     3'    frequency of wave O1:',F10.6,' deg per hour'/
     4'    resonance frequency :',F10.6,' deg per hour'//
     5'    resonance factor for G:',F10.6/
     6'    resonance factor for h:',F10.6/
     7'    resonance factor for k:',F10.6/
     8'    resonance factor for l:',F10.6/
     9'    resonance factor for T:',F10.6/)
 7002 FORMAT(//' Latitude dependent elastic parameters'//
     1' ellipsoidal latitude:',F10.4,' deg'//
     2' G    is gravimetric factor delta'/
     3' h    is LOVE-number  h'/
     4' k    is LOVE-number  k'/
     5' l    is SHIDA-number l'/
     6' T    is tilt factor gamma'//
     7' degree  order         G         h         k         l',
     8'         T')
 7003 FORMAT(2I7,5F10.6)
 7004 FORMAT(' ')
      RETURN
      END
C
      SUBROUTINE ETMUTC(IUN6,IPRINT,DTUJD,DDT)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C     Routine ETMUTC, version 930712 FORTRAN 77.                       C
C                                                                      C
C     All variables with D as first character are DOUBLE PRECISION.    C
C                                                                      C
C     Input parameter description:                                     C
C     ----------------------------                                     C
C                                                                      C
C     IUN6...      formatted printer unit.                             C
C     IPRINT...    printout parameter. For IPRINT=0, nothing will be   C
C                  written on unit IUN6. For IPRINT=2, the tables DTX, C
C                  DTJULD and DTY will be printed for the first call   C
C                  of routine ETMUTC.                                  C
C     DTUJD...     Julian date of epoch (Universal time).              C
C                                                                      C
C     Output parameter description:                                    C
C     -----------------------------                                    C
C                                                                      C
C     DDT...       difference ET - UTC   resp. TDT - UTC in seconds    C
C                  from 1955.5 until 1992.5 . For epochs less 1955.5,  C
C                  DDT is set to 31.59 s. For epochs exceeding 1993.5, C
C                  DDT is set to 60.184 s. ET  is Ephemeris Time.      C
C                  TDT is Terrestrial Dynamical Time.                  C
C                  UTC is Universal Time Coordinated, as broadcasted byC
C                  radio or GPS satellites.                            C
C                                                                      C
C     The table  DTAB has to be extended, when new data are available. C
C     Change parameter NTAB and DIMENSIONS !!!                         C
C                                                                      C
C     Routine creation:  W. Zuern, BFO Schiltach, Heubach 206,         C
C                        D-7620 WOLFACH (in program RIGTID).           C
C     Last modification: 930712 by H.-G. Wenzel.                       C
C                        Geodaetisches Institut,                       C
C                        Universitaet Karlsruhe,                       C
C                        Englerstr. 7,                                 C
C                        D-76128 KARLSRUHE 1,                          C
C                        Germany.                                      C
C                        Tel.: 0721-6082307,                           C
C                        FAX:  0721-694552.                            C
C                        e-mail: wenzel@gik.bau-verm.uni-karlsruhe.de  C
C**********************************************************************C
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT INTEGER*4 (I-N)
      DOUBLE PRECISION DTAB(3,81)
      DOUBLE PRECISION DTAB1(3,20),DTAB2(3,20),DTAB3(3,20),DTAB4(3,20),
     1 DTAB5(3,1)
      EQUIVALENCE (DTAB(1,1), DTAB1(1,1)),(DTAB(1,21),DTAB2(1,1)),
     1            (DTAB(1,41),DTAB3(1,1)),(DTAB(1,61),DTAB4(1,1)),
     2            (DTAB(1,81),DTAB5(1,1))
      SAVE NTAB,IWARN,ITAB,DTAB
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     DTAB(1,I) is the year (in decimal form),                         C
C     DTAB(2,I) is the JULIAN date (in days), and                      C
C     DTAB(3,I) is the difference ET minus UTC, or TDT minus UTC       C
C     in sec, as taken from the Astronomical Ephemeris or the Bulletin C
C     of the International Earth Rotation Service.                     C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DATA DTAB1/ 1955.50000D0,   2435289.50000D0,   31.590D0,
     1            1956.50000D0,   2435655.50000D0,   32.060D0,
     2            1957.50000D0,   2436020.50000D0,   31.820D0,
     3            1958.50000D0,   2436385.50000D0,   32.690D0,
     4            1959.50000D0,   2436750.50000D0,   33.050D0,
     5            1960.50000D0,   2437116.50000D0,   33.160D0,
     6            1961.50000D0,   2437481.50000D0,   33.590D0,
     7            1962.00000D0,   2437665.50000D0,   34.032D0,
     8            1962.50000D0,   2437846.50000D0,   34.235D0,
     9            1963.00000D0,   2438030.50000D0,   34.441D0,
     *            1963.50000D0,   2438211.50000D0,   34.644D0,
     1            1964.00000D0,   2438395.50000D0,   34.950D0,
     2            1964.50000D0,   2438577.50000D0,   35.286D0,
     3            1965.00000D0,   2438761.50000D0,   35.725D0,
     4            1965.50000D0,   2438942.50000D0,   36.160D0,
     5            1966.00000D0,   2439126.50000D0,   36.498D0,
     6            1966.50000D0,   2439307.50000D0,   36.968D0,
     7            1967.00000D0,   2439491.50000D0,   37.444D0,
     8            1967.50000D0,   2439672.50000D0,   37.913D0,
     9            1968.00000D0,   2439856.50000D0,   38.390D0/
      DATA DTAB2/ 1968.25000D0,   2439947.50000D0,   38.526D0,
     1            1968.50000D0,   2440038.50000D0,   38.760D0,
     2            1968.75000D0,   2440130.50000D0,   39.000D0,
     3            1969.00000D0,   2440222.50000D0,   39.238D0,
     4            1969.25000D0,   2440312.50000D0,   39.472D0,
     5            1969.50000D0,   2440403.50000D0,   39.707D0,
     6            1969.75000D0,   2440495.50000D0,   39.946D0,
     7            1970.00000D0,   2440587.50000D0,   40.185D0,
     8            1970.25000D0,   2440677.50000D0,   40.420D0,
     9            1970.50000D0,   2440768.50000D0,   40.654D0,
     *            1970.75000D0,   2440860.50000D0,   40.892D0,
     1            1971.00000D0,   2440952.50000D0,   41.131D0,
     2            1971.08500D0,   2440983.50000D0,   41.211D0,
     3            1971.16200D0,   2441011.50000D0,   41.284D0,
     4            1971.24700D0,   2441042.50000D0,   41.364D0,
     5            1971.32900D0,   2441072.50000D0,   41.442D0,
     6            1971.41400D0,   2441103.50000D0,   41.522D0,
     7            1971.49600D0,   2441133.50000D0,   41.600D0,
     8            1971.58100D0,   2441164.50000D0,   41.680D0,
     9            1971.66600D0,   2441195.50000D0,   41.761D0/
      DATA DTAB3/ 1971.74800D0,   2441225.50000D0,   41.838D0,
     1            1971.83300D0,   2441256.50000D0,   41.919D0,
     2            1971.91500D0,   2441286.50000D0,   41.996D0,
     3            1971.99999D0,   2441317.49999D0,   42.184D0,
     4            1972.00000D0,   2441317.50000D0,   42.184D0,
     5            1972.49999D0,   2441499.49999D0,   42.184D0,
     6            1972.50000D0,   2441499.50000D0,   43.184D0,
     7            1972.99999D0,   2441683.49999D0,   43.184D0,
     8            1973.00000D0,   2441683.50000D0,   44.184D0,
     9            1973.99999D0,   2442048.49999D0,   44.184D0,
     *            1974.00000D0,   2442048.50000D0,   45.184D0,
     1            1974.99999D0,   2442413.49999D0,   45.184D0,
     2            1975.00000D0,   2442413.50000D0,   46.184D0,
     3            1975.99999D0,   2442778.49999D0,   46.184D0,
     4            1976.00000D0,   2442778.50000D0,   47.184D0,
     5            1976.99999D0,   2443144.49999D0,   47.184D0,
     6            1977.00000D0,   2443144.50000D0,   48.184D0,
     7            1977.99999D0,   2443509.49999D0,   48.184D0,
     8            1978.00000D0,   2443509.50000D0,   49.184D0,
     9            1978.99999D0,   2443874.49999D0,   49.184D0/
      DATA DTAB4/ 1979.00000D0,   2443874.50000D0,   50.184D0,
     1            1979.99999D0,   2444239.49999D0,   50.184D0,
     2            1980.00000D0,   2444239.50000D0,   51.184D0,
     3            1981.49999D0,   2444786.49999D0,   51.184D0,
     4            1981.50000D0,   2444786.50000D0,   52.184D0,
     5            1982.49999D0,   2445151.49999D0,   52.184D0,
     6            1982.50000D0,   2445151.50000D0,   53.184D0,
     7            1983.49999D0,   2445516.49999D0,   53.184D0,
     8            1983.50000D0,   2445516.50000D0,   54.184D0,
     9            1985.49999D0,   2446247.49999D0,   54.184D0,
     *            1985.50000D0,   2446247.50000D0,   55.184D0,
     1            1987.99999D0,   2447161.49999D0,   55.184D0,
     2            1988.00000D0,   2447161.50000D0,   56.184D0,
     3            1989.99999D0,   2447892.49999D0,   56.184D0,
     4            1990.00000D0,   2447892.50000D0,   57.184D0,
     5            1990.99999D0,   2448257.49999D0,   57.184D0,
     6            1991.00000D0,   2448257.50000D0,   58.184D0,
     7            1992.49999D0,   2448804.49999D0,   58.184D0,
     8            1992.50000D0,   2448804.50000D0,   59.184D0,
     9            1993.49999D0,   2449169.49999D0,   59.184D0/
      DATA DTAB5/ 1993.50000D0,   2449169.50000D0,   60.184D0/
      DATA NTAB/81/,IWARN/1/,ITAB/1/,N/1/
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Print table for the first call of ETMUTC.                        C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IF(IPRINT.NE.2) GOTO 10
      IF(ITAB.EQ.0) GOTO 10
      WRITE(IUN6,7001)
      DO 1 I=1,NTAB
      WRITE(IUN6,7002) I,DTAB(1,I),DTAB(2,I),DTAB(3,I)
    1 CONTINUE
      ITAB=0
   10 CONTINUE
      IF(DTUJD.LT.DTAB(2,NTAB)) GOTO 11
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     DTUJD exceeds last tabulated epoch DTAB(NTAB,3).                 C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
   12 DDT=DTAB(3,NTAB)
      IF(IWARN.EQ.1) WRITE(IUN6,7051) DTAB(1,NTAB)
      IF(IWARN.EQ.1) WRITE(*,7051)    DTAB(1,NTAB)
      IWARN=0
      RETURN
   11 DO  20  I=1,NTAB
      IF(DTUJD-DTAB(2,I))   21,22,20
   22 DDT=DTAB(3,I)
      RETURN
   21 N=I-1
      GOTO  23
   20 CONTINUE
   23 DDT=(DTAB(3,N+1)*(DTUJD-DTAB(2,N))-DTAB(3,N)*(DTUJD-DTAB(2,N+1)))/
     1 (DTAB(2,N+1)-DTAB(2,N))
      RETURN
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Format statements:                                               C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 7001 FORMAT(//' Routine ETMUTC, version 930712 FTN 77.'//
     1' List of tables:'//
     2'       No.           Juld            DTX       DTY'//)
 7002 FORMAT(I10,2F15.5,F10.3)
 7051 FORMAT(/' ***** Warning from routine ETMUTC, version 930712.'/
     1' ***** Epoch exceeds the last tabulated value:',F10.5/
     2' ***** DDT of last tabulated epoch is used.'/
     3' ***** Please try to update tabels in routine ETMUTC.'/)
      END
C
      SUBROUTINE ETPOTA(IUN4,IUN6,IUN14,IPRINT,IMODEL,DLAT,DLON,DH,
     1 DGRAV,DAZ,IC,ITY,ITM,ITD,DTH,DDT0,MAXNW,NRW,DTHAM,DTHPH,DTHFR,
     2 DBODY,NW)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C     Routine ETPOTA, version 930710 FORTRAN 77.                       C
C                                                                      C
C     The routine ETPOTA computes amplitudes, phases, frequencies and  C
C     body tide amplitude factors for a number of different Earth tide C
C     components using three different tidal potential developments.   C
C                                                                      C
C     Attention: This routine has finally not been tested for vertical C
C                and horizontal displacements and for shear tidal      C
C                strain !!!!                                           C
C                                                                      C
C     All variables with D as first character are DOUBLE PRECISION.    C
C                                                                      C
C     Input parameter description:                                     C
C     ----------------------------                                     C
C                                                                      C
C     IUN4...      formatted unit, on which the tidal potential        C
C                  development has to be stored before the execution   C
C                  of routine ETPOTA  (e.g. file ETCPOT.DAT).          C
C     IUN6...      formatted line printer unit.                        C
C     IUN14...     unformatted copy of IUN4. This unit will be opened  C
C                  as file ETCPOT.UFT during the execution of routine  C
C                  ETPOTA with STATUS=OLD if it exists and with        C
C                  STATUS=NEW, if it does not exist. If the STATUS is  C
C                  NEW, ETCPOT.DAT will be established during the      C
C                  execution of routine ETPOTA.                        C
C     IPRINT...    printout parameter.                                 C
C                  for IPRINT = 0, nothing will be printed.            C
C                  for IPRINT = 1, a short list will be printed.       C
C                  for IPRINT = 2, a long list will be printed         C
C                  (including the tidal potential development).        C
C     IMODEL...    parameter for selecting the tidal potential         C
C                  development.                                        C
C                  IMODEL = 0: DOODSON 1921 tidal potential develop-   C
C                              ment with 378 waves.                    C
C                  IMODEL = 1: CARTWRIGHT-TAYLOR-EDDEN 1973 tidal      C
C                              potential development with 505 waves.   C
C                  IMODEL = 2: TAMURA 1987 tidal potential develop-    C
C                              ment with 1200 waves.                   C
C                  IMODEL = 3: BUELLESFELD 1985 tidal potential        C
C                              development with 656 waves.             C
C     DLAT...      ellipsoidal latitude  referring to Geodetic         C
C                  Reference System 1980 in degree.                    C
C     DLON...      ellipsoidal longitude referring to Geodetic         C
C                  Reference System 1980 in degree, positive east of   C
C                  Greenwhich.                                         C
C     DH...        ellipsoidal height referring to Geodetic Reference  C
C                  System 1980 in meter.                               C
C     DGRAV...     gravity in m/s**2. If the gravity is input below    C
C                  1 m/s**2, the gravity will be replaced by the       C
C                  computed normal gravity for reference system GRS80. C
C     DAZ...       azimuth in degree from north direction (only valid  C
C                  for tidal tilt, horizontal displacement, and        C
C                  horizontal strain).                                 C
C     IC...        Earth tide component to be computed.                C
C                  IC=-1: tidal potential in m**2/s**2.                C
C                  IC= 0: vertical tidal acceleration (gravity tide),  C
C                         in nm/s**2 (positive downwards).             C
C                  IC= 1: horizontal tidal acceleration (tidal tilt)   C
C                         in azimuth DAZ in mas = arc sec/1000.        C
C                  IC= 2: vertical tidal displacement, geodetic        C
C                         coefficients in mm (positive upwards).       C
C                  IC= 3: horizontal tidal displacement in azimuth     C
C                         DAZ in mm.                                   C
C                  IC= 4: vertical tidal strain in 10**-9 = nstr.      C
C                  IC= 5: horizontal tidal strain in azimuth DAZ       C
C                         in 10**-9 = nstr.                            C
C                  IC= 6: areal  tidal strain in 10**-9 = nstr.        C
C                  IC= 7: shear  tidal strain in 10**-9 = nstr.        C
C                  IC= 8: volume tidal strain in 10**-9 = nstr.        C
C                  IC= 9: ocean tides, geodetic coefficients in        C
C                         millimeter.                                  C
C     IR...   printout parameter for the tidal potential development.  C
C             IR= 0: no printout of the tidal potential development.   C
C             IR= 1: tidal potential development and the development   C
C                    of the specific tidal component will be printed.  C
C     ITY...       year  of initial epoch of tidal development.        C
C     ITM...       month of initial epoch of tidal development.        C
C     ITD...       day   of initial epoch of tidal development.        C
C     DTH...       hour  of initial epoch of tidal development (UTC).  C
C                  Example: September 27th 1984, 10 o'clock UTC is     C
C                  ITY = 1984, ITM = 9, ITD = 27, DTH = 10.D0          C
C     MAXNW...     maximum number of waves, used for DIMENSION of      C
C                  arrays NRW, DTHAM, DTHPH, DTHFR, DBODY. Although    C
C                  the TAMURA 1987 potential development contains 1200 C
C                  waves only, the CTED 1973 and the BUELLESFELD 1985  C
C                  tidal potential developments contain a few waves    C
C                  with small amplitude, which do not exist in the     C
C                  TAMURA 1987 potential development. Thus, the total  C
C                  maximum number of waves MAXNW of file ETCPOT.DAT is C
C                  1214.                                               C     
C                                                                      C
C     Output parameter description:                                    C
C     -----------------------------                                    C
C                                                                      C
C     DDT0...      difference TDT - UTC at initial epoch in sec.       C
C     NRW...       array(1...MAXNW) of wave numbers.                   C
C     DTHAM...     array (1...MAXNW) of tidal amplitudes given in      C
C                  units of CUNIT(IC2).                                C
C     DTHPH...     array (1...MAXNW) of tidal phases in radians at     C
C                  initial epoch.                                      C
C     DTHFR...     array (1...MAXNW) of tidal frequencies in radian    C
C                  per hour.                                           C
C     DBODY...     array (1...MAXNW) of bady tide amplitude factors    C
C                  for tidal gravity and tidal tilt. In order to       C
C                  compute the body tide, the amplitudes DTHAM have to C
C                  be multiplied by DBODY. In case of IRIGID=1, all    C
C                  body tide amplitude factors are set to 1.0000       C
C     NW...        number of defined tidal waves.                      C
C                                                                      C
C     Used routines:                                                   C
C     --------------                                                   C
C                                                                      C
C     ETASTE: computes astronomical elements.                          C
C     ETGCOF: computes geodetic coefficients.                          C
C     ETJULD: computes Julian date.                                    C
C     ETLOVE: computes latitude dependent elastic parameters (called   C
C             ETGCOF).                                                 C
C     ETMUTC: computes the difference TDT minus UTC (called by ETASTE).C
C     GEOEXT: computes jobtime.                                        C
C                                                                      C
C     Numerical accuracy:                                              C
C     -------------------                                              C
C                                                                      C
C     The routine has been tested under operation systems UNIX and     C
C     MS-DOS with 15 digits in DOUBLE PRECISION.                       C
C                                                                      C
C     Routine creation:  880427 by H.-G. Wenzel.                       C
C                        Geodaetisches Institut,                       C
C                        Universitaet Karlsruhe,                       C
C                        Englerstr. 7,                                 C
C                        D-76128 KARLSRUHE 1,                          C
C                        Germany.                                      C
C                        Tel: 0049-721-6082307,                        C
C                        FAX: 0049-721-694552.                         C
C                        e-mail: wenzel@gik.bau-verm.uni-karlsruhe.de  C
C     Last modification: 930706 by H.-G. Wenzel.                       C
C**********************************************************************C
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT INTEGER*4 (I-N)
      LOGICAL*1 LEX14
      CHARACTER CHEAD(8)*10,CENDH*10,CUNIT(11)*8,CWN*4,CMODEL(4)*13
      INTEGER*2 NS(8)
      DOUBLE PRECISION DX(3),DHH(4)
      DOUBLE PRECISION DAS(8),DASP(8),DGK(12),DPK(12)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     The following DIMENSION statement is concerning the number of    C
C     waves of the tidal potential development, which is MAXNW.        C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      INTEGER*4 NRW(MAXNW)
      DOUBLE PRECISION DTHAM(MAXNW),DTHPH(MAXNW),DTHFR(MAXNW),
     1 DBODY(MAXNW)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     The following DIMENSION statement is concerning the elastic      C
C     Earth model for the different degree and order constituents.     C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DOUBLE PRECISION DELTA(12)
      COMMON /UNITS/ CUNIT,IC2
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     COMMON /CONST/:                                                  C
C     DPI...        3.1415....                                         C
C     DPI2...       2.D0*DPI                                           C
C     DRAD...       DPI/180.D0                                         C
C     DRO...        180.D0/DPI                                         C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      COMMON /CONST/ DPI,DPI2,DRAD,DRO
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     COMMON /LOVE/ contains gravimeter factors, LOVE-numbers, SHIDA-  C
C     numbers and tilt factors for degree 2...4 at latitude DLAT:      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DIMENSION DGLAT(12),DHLAT(12),DKLAT(12),DLLAT(12),DTLAT(12)
      COMMON /LOVE/ DOM0,DOMR,DGLAT,DGR,DHLAT,DHR,DKLAT,DKR,DLLAT,DLR,
     1 DTLAT,DTR
      DATA CENDH/'C*********'/
      DATA CMODEL/'DOODSON 1921 ','CTED 1973    ','TAMURA 1987  ',
     1            'BUELLESF.1985'/
      IF(IPRINT.EQ.0) GOTO 10
      WRITE(IUN6,7001) CMODEL(IMODEL+1)
      WRITE(*,7001)    CMODEL(IMODEL+1)
   10 CONTINUE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Test, whether there exist already unformatted file ETCPOT.UFT:   C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      OPEN(UNIT=IUN14,FILE='ETCPOT.UFT',FORM='UNFORMATTED',STATUS='OLD',
     1 ERR=11)
      LEX14=.TRUE.
      REWIND IUN14
      GOTO 12
   11 OPEN(UNIT=IUN14,FILE='ETCPOT.UFT',FORM='UNFORMATTED',STATUS='NEW')
      LEX14=.FALSE.
      REWIND IUN4
   12 CONTINUE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Compute geodetic coefficients and body tide amplitude factors    C
C     for the WAHR-DEHANT-ZSCHAU model. The NDFW resonance is          C
C     approximated by                                                  C
C                                                                      C 
C     G0 - GR*(DOM - DOM0)/(DOMR - DOM),                               C
C                                                                      C
C     similar equations hold for the other components.                 C
C                                                                      C
C     Gravimetric amplitude factors, LOVE numbers h and k for zero to  C
C     third degree tidal potential have been taken from DEHANT 1987,   C
C     table 7, 8 and 9 for elliptical, uniformly rotating, oceanless   C
C     Earth with liquid outer core and inelastic mantle (PREM Earth    C
C     model with inelastic mantle from ZSCHAU) and for the fourth      C
C     degree from DEHANT et al. 1989, table 6). The resonance factors  C
C     GR have been computed to fit the difference between body tide    C
C     amplitude factors at waves O1 and PSI1 from DEHANT 1987, PREM    C
C     model with elastic mantle (table 1...3). The NDFW resonance      C
C     frequency is 15.073729 degree per hour  = 1.004915267 CPD UT,    C
C     taken from WAHR 1981 (because it is not given in any of DEHANT's C
C     papers).                                                         C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      CALL ETGCOF(IUN6,IPRINT,DLAT,DLON,DH,DGRAV,DAZ,IC,DGK,DPK)
      IC2=IC+2
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Define default body tide amplitude factors for components        C
C     IC=2...9.                                                        C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DO 50 I=1,12
  50  DELTA(I)=1.D0
      DELTAR=0.D0
      GOTO (100,200,300),IC2
      GOTO 1000
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     IC=-1, compute body tide amplitude factors for tidal potential:  C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
  100 CONTINUE
      DO 110 I=1,12
  110 DELTA(I)=DKLAT(I)
      DELTAR=DKR
      GOTO 1000
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     IC=0, compute body tide amplitude factors for vertical component C
C     (gravity tides):                                                 C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
  200 CONTINUE
      DO 210 I=1,12
  210 DELTA(I)=DGLAT(I)
      DELTAR=DGR
      GOTO 1000
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     IC=1: compute body tide amplitude factors for horizontal         C
C     component (tidal tilt):                                          C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
  300 CONTINUE
      DO 310 I=1,12
  310 DELTA(I)=DTLAT(I)
      DELTAR=DKR-DHR
 1000 CONTINUE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     compute JULIAN date for initial epoch ITY,ITM,ITD,DTH:           C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      CALL ETJULD(IUN6,ITY,ITM,ITD,DTH,DT)
      DT2000=(DT-2451544.D0)/36525.0D0
      DT=(DT-2415020.0D0)/36525.0D0
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Compute astronomical elements for initial epoch:                 C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      CALL ETASTE(IUN6,IPRINT,IMODEL,DLON,ITY,ITM,ITD,DTH,DAS,DASP,
     1 DDT0)
      IC2=IC+2
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Interpolation factors for CTED 1973 potential coefficients at    C
C     start epoch by linear least squares interpolation (see WENZEL    C
C     1976).                                                           C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DX(1)=0.550376D0-1.173312D0*DT
      DX(2)=0.306592D0+0.144564D0*DT
      DX(3)=0.143033D0+1.028749D0*DT
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Read file header of tidal potential file on unit IUN4:           C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IF(LEX14) READ(IUN14) (CHEAD(I),I=1,8)
      IF(.NOT.LEX14) READ(IUN4,7028)  (CHEAD(I),I=1,8)
      IF(.NOT.LEX14) WRITE(IUN14) (CHEAD(I),I=1,8)
      WRITE(IUN6,7029) (CHEAD(I),I=1,8)
      WRITE(*,7029)    (CHEAD(I),I=1,8)
 1100 IF(LEX14) READ(IUN14) (CHEAD(I),I=1,8)
      IF(.NOT.LEX14) READ(IUN4,7028)  (CHEAD(I),I=1,8)
      IF(.NOT.LEX14) WRITE(IUN14) (CHEAD(I),I=1,8)
      IF(IPRINT.GT.1) WRITE(IUN6,7029) (CHEAD(I),I=1,8)
      IF(IPRINT.GT.1) WRITE(*,7029)    (CHEAD(I),I=1,8)
      IF(CHEAD(1).NE.CENDH) GOTO 1100
      IF(LEX14) READ(IUN14) NW
      IF(.NOT.LEX14) READ(IUN4,7005) NW
      IF(.NOT.LEX14) WRITE(IUN14) NW
      IF(NW.GT.MAXNW) GOTO 5000
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Compute tidal development for the specific component from tidal  C
C     potential development:                                           C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DSH1=0.D0
      DSH2=0.D0
      DSH3=0.D0
      DSHD=0.D0
      DSHT=0.D0
      IW=1
      NAMPL=0
 1110 CONTINUE
 1120 CONTINUE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     NRT is the wave number of TAMURA's 1987 tidal potential          C
C     development, NRC is the wave number of CARTWRIGHT-TAYLER-EDDEN   C
C     1973 tidal potential development.                                C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IF(LEX14) READ(IUN14,END=2000) NRI,K,(NS(J),J=1,8),NP,
     1 (DHH(J),J=1,3),CWN,DHD,DHT,DHTD,DHB
      IF(.NOT.LEX14) READ(IUN4,7006,END=2000) NRI,K,(NS(J),J=1,8),NP,
     1 (DHH(J),J=1,3),CWN,DHD,DHT,DHTD,DHB
      IF(.NOT.LEX14) WRITE(IUN14) NRI,K,(NS(J),J=1,8),NP,
     1 (DHH(J),J=1,3),CWN,DHD,DHT,DHTD,DHB
      DSHT=DSHT+DHT
      J=IW-1
C     IF(MOD(J,51).EQ.0.AND.IPRINT.EQ.2) WRITE(IUN6,7007) ITY,ITM,ITD,
C    1 DTH,IC,NW,CUNIT(IC2)
      DHH(4)=0.D0
      DSH1=DSH1+DHH(1)
      DSH2=DSH2+DHH(2)
      DSH3=DSH3+DHH(3)
      DSHD=DSHD+DHD
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     CTED 1973 tidal potential development is used:                   C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DO 1130 J=1,3
 1130 DHH(4)=DHH(4)+DHH(J)*DX(J)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     DOODSON's 1921 tidal potential development is used:              C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IF(IMODEL.EQ.0) DHH(4)=DHD
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     TAMURA's 1987 tidal potential development is used:               C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IF(IMODEL.EQ.2) DHH(4)=DHT+DHTD*DT2000
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     BUELLESFELD's 1985 tidal potential development is used:          C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IF(IMODEL.EQ.3) DHH(4)=DHB
      IF(DABS(DHH(4)).LT.1.D-6) GOTO 1120
      NAMPL=NAMPL+1
      DC2=0.D0
      DC3=0.D0
      DO 1140 J=1,8
      DC2=DC2+NS(J)*DAS(J)
 1140 DC3=DC3+NS(J)*DASP(J)
      J=(K+1)*K/2-2+NS(1)
      DC2=DC2+DPK(J)+NP*90.D0
      DC1=DHH(4)*DGK(J)
      DBODY(IW)=DELTA(J)
      IF(J.EQ.2) DBODY(IW)=DELTA(J)+DELTAR*(DC3-DOM0)/(DOMR-DC3)
      IF(DC1.GE.0.D0) GOTO 1160
      DC1=-DC1
      DC2=DC2-180.D0
 1160 DC2=DMOD(DC2,360.D0)
      IF(DC2.GE.0.D0) GOTO 1170
      DC2=DC2+360.D0
      GOTO 1160
 1170 CONTINUE
      DTHAM(IW)=DC1
      DTHPH(IW)=DC2*DRAD
      DTHFR(IW)=DC3*DRAD
      NRW(IW)=NRI
C     IF(IPRINT.EQ.2) WRITE(IUN6,7008) NRI,K,(NS(J),J=1,8),NP,
C    1 (DHH(J),J=1,4),DC1,DC2,DC3,CWN,DHD,DHT,DHTD
      IF(IPRINT.EQ.2) WRITE(IUN6,7011) DC1,DC2,DC3,CWN,DBODY(IW)
      IF(IPRINT.EQ.2) WRITE(*,7011)    DC1,DC2,DC3,CWN,DBODY(IW)
      IW=IW+1
      GOTO 1110
 2000 CONTINUE
      NW=IW-1
      IF(IPRINT.EQ.0) RETURN
      WRITE(IUN6,7009) DSH1,DSH2,DSH3,DSHD,DSHT
      WRITE(*,7009)    DSH1,DSH2,DSH3,DSHD,DSHT
      WRITE(IUN6,7010) NW,NAMPL
      WRITE(*,7010)    NW,NAMPL
      WRITE(IUN6,7030)
      WRITE(*,7030)
      RETURN
 5000 CONTINUE
      WRITE(IUN6,7050) NW,MAXNW
      WRITE(*,7050)    NW,MAXNW
      STOP
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Format statements:                                               C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 7001 FORMAT(' Routine ETPOTA, version 930710 FORTRAN 77.'//
     1' Tidal component development from tidal potential development.'//
     2 1X,A13,' tidal potential development is used.'/)
 7005 FORMAT(2I5)
 7006 FORMAT(I4,9X,10I2,3F7.5,1X,A4,F7.5,F8.6,F9.6,F8.6)
 7007 FORMAT(' Routine ETPOTA, version 930710 FORTRAN 77'//
     1' Tidal potential and tidal component development'/
     2' Initial epoch',I6,2(1H.,I2),F5.2/
     3' Component: ',I4/' Number of waves: ',I4//
     3' no. argum.  argum.nrs. NP  H1870  H1924',
     4'  H1960   H(T)   ampl.  phase   frequency'/
     5 55X,A6,' [deg]  [deg/h]'/)
 7008 FORMAT(1X,I4,10I2,4F7.5,F8.4,F9.4,F12.8,1X,A4/F7.5,F8.6,F9.6)
 7009 FORMAT(//' Sum of potential coefficients :'/5F10.6)
 7010 FORMAT(//' Number of waves is         :',I6/
     1         ' Number of amplitudes > 0 is:',I6/ )
 7011 FORMAT(3F10.5,2X,A6,2X,F10.6)
 7028 FORMAT(8A10)
 7029 FORMAT(1X,8A10)
 7030 FORMAT(///' ***** Routine ETPOTA finished execution.'/)
 7050 FORMAT(/' ***** Error in routine ETPOTA, version 930710.'/
     1' ***** The current number of waves:',I5,' exceeds the maximum',
     2' number of waves:',I5/
     3' ***** Routine ETPOTA stops the execution.'/)
      END
C
      SUBROUTINE GEOEXT(IUN6,DEXTIM)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C     Routine GEOEXT, version 930418 FORTRAN 77.                       C
C                                                                      C
C     === MS-DOS version for MS-FORTRAN 5.0 compiler ==========        C
C                                                                      C
C     The routine GEOEXT computes the actual job time and writes       C
C     the actual execution time on printer output unit IUN6.           C
C     For the first call of routine GEOEXT, the actual jobtime will    C
C     be computed (in secs since midnight) and stored. For the next    C
C     call(s) of routine GEOEXT, the actual jobtime will be computed   C
C     and the execution time (actual jobtime minus jobtime of the      C
C     first call of routine GEOEXT) will be printed.                   C
C                                                                      C
C     Input parameter description:                                     C
C     ----------------------------                                     C
C                                                                      C
C     IUN6...      formatted printer unit.                             C
C                                                                      C
C     Output parameter description:                                    C
C     -----------------------------                                    C
C                                                                      C
C     DEXTIM...    actual jobtime in seconds (time elapsed from the    C
C                  first call of routine GEOEXT to the actual call of  C
C                  routine GEOEXT.), double precision.                 C
C                                                                      C
C     used routines:                                                   C
C     --------------                                                   C
C                                                                      C
C     Subroutine GETTIM for WATFOR87 or IBM PROFESSIONAL FORTRAN       C
C     compiler.                                                        C
C                                                                      C
C     Program creation:  790830 by H.-G. Wenzel,                       C
C                        Geodaetisches Institut,                       C
C                        Universitaet Karlsruhe,                       C
C                        Englerstr. 7,                                 C
C                        D-7500 KARLSRUHE 1,                           C
C                        Germany.                                      C
C                        Tel.: 0721-6082301.                           C
C                        FAX:  0721-694552.                            C
C                        e-mail: wenzel@gik.bau-verm.uni-karlsruhe.de  C
C     Last Modification: 930418 by H.-G.Wenzel.                        C
C**********************************************************************C
      IMPLICIT DOUBLE PRECISION (D)
C MSFOR:      INTEGER*2 IH,IM,IS,IS100
      REAL TIME1, TIME2
      SAVE IZ1,IZ2,DTIME1
      DATA  IZ1/0/,IZ2/0/
      IF(IZ1-IZ2) 6003,6001,6003
 6001 CONTINUE
      IZ1=10000
C MSFOR:      CALL GETTIM(IH,IM,IS,IS100)
C MSFOR:      DTIME1=DBLE(IS+IM*60+IH*3600)+0.01*FLOAT(IS100)
C LAHEY:      CALL TIMER(ITS100)
C LAHEY:      DTIME1=DBLE(ITS100)/100.
C UNIX:       DTIME1=DBLE(SECNDS(RDUMMY))
C G77:
      CALL CPU_TIME(TIME1)
      DTIME1=DBLE(TIME1)
      WRITE(IUN6,7001)
      WRITE(*,7001)
      DEXTIM=0.D0
      RETURN
 6003 CONTINUE
C MSFOR:      CALL GETTIM(IH,IM,IS,IS100)
C MSFOR:      DTIME2=DBLE(IS+IM*60+IH*3600)+0.01*FLOAT(IS100)
C LAHEY:      CALL TIMER(ITS100)
C LAHEY:      DTIME2=DBLE(ITS100)/100.
C UNIX:       DTIME2=DBLE(SECNDS(RDUMMY))
C G77:
      CALL CPU_TIME(TIME2)
      DTIME2=DBLE(TIME2)
      DEXTIM=DTIME2-DTIME1
      WRITE(IUN6,7002) DEXTIM
      WRITE(*,7002) DEXTIM
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Format statements:                                               C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 7001 FORMAT(' First call of routine GEOEXT, version 930418 FTN77.')
 7002 FORMAT(/' Routine GEOEXT. Execution time=',F10.3,' sec'/)
      RETURN
      END
