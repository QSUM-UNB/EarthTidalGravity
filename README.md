# EarthTidalGravity
A python wrapper for ETGTAB (Earth Tidal Gravity TABble) a program to predict Earth tidal gravity accelerations and other tidal components.

The program ETGTAB computes model tides using different tidal potential developments (DOODSON 1921 with 378 waves, CARTWRIGHT-TAYLER-EDDEN 1973 with 505 waves, TAMURA 1987 with 1200 waves, or BUELLESFELD 1985 with 665 waves) for a number of different tidal components using observed or estimated (e.g. by a body tide and ocean tide model) tidal parameters.

The program ETGTAB can be used for the computation of Earth tides with 1 hour or 5 minute time intervals for one specific station in order to generate a table of Earth tide values (tidal potential, accelerations, tilts). There can be used three different tidal potential developments (DOODSON 1921, CARTWRIGHT-TAYLER-EDDEN 1973, TAMURA 1987) as well as observed tidal parameters. The program is written mainly in FORTRAN 90 (ANSI-standard) except for the routines to compute actual time used within subroutine GEOEXT.

## Fortran Source File Descriptions:
- ETGTAB-v3.0-2024.f: 
- ETCPOT.DAT:  formatted unit, in which the tidal potential development must be stored before execution.
- ETGTAB.INP:  formatted unit, in which the input parameters must be stored before execution.
- ETGTAB.PRN:  formatted print file.
- ETGTAB.OUT:  formatted output file in ETERNA 3.0 format.

## Fortran subroutines:
The program consists of the following subroutines
- ETASTE: computes the astronomical elements.
- ETGCOF: computes the geodetic coefficients.
- ETGREG: computes the Gregorian date of the next hour.
- ETJULD: computes the Julian date.
- ETMUTC: computes the difference between UTC and TDT.
- ETLOVE: computes elastic parameters from WAHR-DEHANT model.
- ETPOTA: computes tidal amplitudes, frequencies and phases from the tidal potential development.
- GEOEXT: computes and print the execution time.

The input parameters for program ETGTAB are read from formatted file ETGTAB.INP. The tidal potential will be read either from formatted file ETCPOT.DAT or from unformatted file ETCPOT.UFT.  

The subroutine ETPOTA computes three arrays containing amplitudes, phases and frequencies for a rigid Earth model, which are transferred to the main program.
Additionally,  an  array  containing amplitude factors for an elastic Earth from WAHR-DEHANT model are computed.
Subroutine ETPOTA calls itself in other subroutines as ETASTE, ETGCOF, ETJULD, ETLOVE,  ETMUTC. 
      
Thus, an implementation of ETPOTA into other programs (as e.g. Earth tide analysis programs) should be possible.
In fact, ETPOTA is used within Earth tide analysis program ETERNA.

ETGTAB has been superseeded by PREDICT - a module in the ETERNA software package.
It allows one to compute the tidal response of an elastic Earth anywhere on its surface.
ETGTAB and PREDICT were originally written in FORTRAN77 by the late Prof. H.-G Wenzel, Geodetic Institute, Karlsruhe University.


