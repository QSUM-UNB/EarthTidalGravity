      File: ETGTAB.TXT, Status 1996.10.11

      Prof. Dr.-Ing.habil Hans-Georg Wenzel
      Geod—tisches Institut
      Universit—t Karlsruhe
      Englerstr. 7
      D-76128 KARLSRUHE 
      Tel.: 0049-721-6082307
      e-mail: wenzel@gik.bau-verm.uni-karlsruhe.de 


      Program ETGTAB version 3.0:
      ---------------------------

      Remarks for the installation, test of installation and use of  program
      ETGTAB  on  an  IBM-AT  under  operation  system  MS-DOS 3.3  upwards.
      There is given the file ETGTAB.EXE  on  the  floppy,  which  has  been 
      compiled with Lahe LF90 2.00 FORTRAN compiler and is executable on  an 
      80386/387, 80486 and PENTIUM processor. The program ETGTAB can be com-
      piled under UNIX operating system, but the subroutine GEOEXT has to be
      modified properly (see comments in the routine).

      General Remarks:
      ----------------

      The program ETGTAB can be used for the computation of Earth tides with
      1 hour or 5 minute time interval for one specific  station in order to
      generate a table of Earth tide values (tidal potential, accelerations,
      tilts). There can be used three different tidal potential developments
      (DOODSON 1921, CARTWRIGHT-TAYLER-EDDEN 1973, TAMURA 1987)   as well as
      observed tidal parameters. The program is written mainly in FORTRAN 90
      (ANSI-standard)  except  for  the routines to compute actual time used 
      within subroutine  GEOEXT.  The  program  consists  of  the  following 
      subroutines

      routine ETASTE: computes the astronomical elements.
      routine ETGCOF: computes the geodetic coefficients.
      routine ETGREG: computes the Gregorian date of the next hour.
      routine ETJULD: computes the Julian date.
      routine ETMUTC: computes the difference between UTC and TDT.
      routine ETLOVE: computes elastic parameters from WAHR-DEHANT model.
      routine ETPOTA: computes tidal amplitudes, frequencies and 
                      phases from the tidal potential development.
      routine GEOEXT: computes and print the execution time.

      The input parameters for program ETGTAB are read from  formatted  file
      ETGTAB.INP. The tidal potential will be  read  either  from  formatted
      file ETCPOT.DAT or  from  unformatted  file  ETCPOT.UFT.  

      The subroutine ETPOTA computes  three  arrays  containing  amplitudes,
      phases and frequencies for a  rigid Earth model, which are transferred 
      to  the  main  program.  Additionally,  an  array  containing amplitude 
      factors for an elastic Earth  from  WAHR-DEHANT  model  are  computed. 
      Subroutine  ETPOTA  calls itself in other subroutines as ETASTE, ETGCOF, 
      ETJULD, ETLOVE,  ETMUTC. Thus, an implementation  of ETPOTA into other
      programs (as e.g. Earth tide analysis programs)  should  be  possible.
      In fact,  ETPOTA is used within Earth tide analysis program ETERNA. 


     Implementation of ETGTAB on an IBM-AT :
     =======================================

     1) Open a subdirectory, in which the  complete  contents  of the floppy
        disc will be copied later on, by typing and entering 

                      mkdir etgtab

      2) Change the actual directory to subdirectory  etgtab  by typing  and
         entering

                      cd etgtab

      3) Copy the complete floppy disc from  disc  drive  (assumed to be a:)
         into the subdirectory etgtab by typing and entering

                      copy a:*.* C:

      4) Decompress file ETGTAB.ARC by entering

                      arc x etgtab.arc *.*

      5) Decompress file PLOTDATA.ARC by entering

                      arc x plotdata.arc *.*  

      6) If you use an 80386/387, 80486 or PENTIUM processor,  execute  the
         program ETGTAB  compiled  with  LAHEY  LF90  compiler  version  by 
         entering

                      etgtab

      7) You will receive on the screen an output of the  program  test  run
         as  well  as  after  the  end  of  the  execution (about 4 minutes, 
         depending  on  the speed  of your computer) on the file ETGTAB.PRN,
         the print file of the test run on your  computer.  Print  the  file 
         ETGTAB.PRN by entering

                      copy etgtab.prn prn

      8) The file ETGTAB.PRN on the floppy is the reference  print  file  of
         the test run on my computer (a IBM-AT 486 DX2 66 Mhz);  print  this 
         file by entering

                      copy a:etgtab.prn prn

         and compare both listings.  They should be identical except for the
         computation  time and  job  date.  If the listings agree,  you have 
         successfully  installed ETGTAB  on  your  computer. If the listings 
         differ significantly, you can try  to  compile  the  program  again
         using your compiler (see section 7).

      Using the compiled program ETGTAB (File ETGTAB.EXE):
      ----------------------------------------------------

      The file ETCPOT.DAT contains the  tidal potential developments used by 
      ETGTAB. File ETCPOT.UFT is a unformatted copy of file  ETCPOT.DAT.  Do 
      not ever change file ETCPOT.DAT; this  would  only  be  necessary  for 
      implementing an additional tidal potential development.

      The file ETGTAB.INP contains the input parameters  used by  ETGTAB  as 
      e.g.   latitude,  longitude  of  the  station,  starting  time,  tidal 
      parameters (e.g. adjusted by program ETERNA)  etc. The contents of the 
      file is self describing.  A  description of these  parameters is given
      in  the  source  code  file  ETGTAB.FOR.  You  should  edit  the  file 
      ETGTAB.INP  for  your application and execute your tidal prediction by
      entering

                      etgtab

      The computed model  tide  values  are  given  in  compressed format (6 
      values per record) on file ETGTAB.PRN and in ETERNA 3.0  input  format
      on file ETGTAB.OUT. The data given in file ETGTAB.OUT may be plotted
      using program PLOTDATA given on the floppy.

      Good luck when using ETGTAB !!!!
      
      The program ETGTAB has been extensively tested by comparing predicted
      tides from other e.g. ephemeris programs. If you detect errors in the
      program or mysterious behavior  of  the  program, please report these
      errors to me.

      Hans-Georg Wenzel.

######end of current description