# EarthTidalGravity
A python wrapper for ETGTAB (Earth Tidal Gravity TABble) -- a program to predict Earth tidal gravity accelerations and other tidal components.

The program ETGTAB computes model tides using different tidal potential developments (DOODSON 1921 with 378 waves, CARTWRIGHT-TAYLER-EDDEN 1973 with 505 waves, TAMURA 1987 with 1200 waves, or BUELLESFELD 1985 with 665 waves) for a number of different tidal components using observed or estimated (e.g. by a body tide and ocean tide model) tidal parameters. ETGTAB can be used for the computation of Earth tides with 1 hour or 5 minute time intervals for one specific station in order to generate a table of Earth tide values (tidal potential, accelerations, tilts).

Note that ETGTAB has been superseeded by PREDICT - a module in the ETERNA software package.
ETGTAB and PREDICT were originally written in FORTRAN77 by the late Prof. H.-G Wenzel, Geodetic Institute, Karlsruhe University.
