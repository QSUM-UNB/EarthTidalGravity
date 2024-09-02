#####################################################################
## Filename:	ETGTAB_Class.py
## Author:		B. Barrett
## Description: Python wrapper for the ETGTAB program to predict Earth tidal gravity parameters
## Version:		1.0.0
## Last Mod:	02/09/2024
#####################################################################

import datetime as dt
import json
import logging
import matplotlib.pyplot as plt
import matplotlib as mpl
import numpy as np
import os
import pandas as pd
import pytz
import subprocess
import time

from pathlib import Path
from matplotlib.dates import DateFormatter, HourLocator

#################################################################

## Set default plot options
plt.style.use('default')
mpl.rcParams['figure.figsize'] = [7., 5.]
mpl.rcParams['figure.dpi'] = 150
mpl.rcParams['savefig.dpi'] = 150
mpl.rcParams['font.size'] = 12
mpl.rcParams['legend.borderpad'] = 0.4
mpl.rcParams['legend.handlelength'] = 1.5
mpl.rcParams['legend.handletextpad'] = 0.5
mpl.rcParams['legend.fontsize'] = 10

## Configure logger
logging.basicConfig(
	level = logging.INFO,
	format = '%(asctime)s %(levelname)s::%(module)s::%(message)s',
	datefmt = '%Y-%m-%d %H:%M:%S'
	)

#################################################################

class EarthTidalGravity:
	#############################################################
	## Class for Earth tidal gravity model
	#############################################################

	def __init__(self, OutputFile, Location={}, TidePars={}):
		"""Initialize class attributes.
		ARGUMENTS:
		\t OutputFile (str) - Name of file in which to store model output from ETGTAB.
		\t Location (dict) - Parameters defining location at which to compute tides.
		\t TidePars (dict) - Earth tide gravity parameters to compute.
		"""

		## Path variables
		self.WorkDir    = os.path.dirname(os.path.realpath(__file__))
		self.SourceDir  = 'Source'							## Name of folder within WorkDir containing ETGTAB source files
		self.OutputDir  = 'Output'							## Name of folder within WorkDir to store ETGTAB output
		self.OutputFile = os.path.splitext(OutputFile)[0]	## Name of file in which to store ETGTAB output
		# self.OutputExt  = os.path.splitext(OutputFile)[1]	## Output file extension (if any)

		# self.ETGTAB_EXE = 'ETGTAB-v3.0.1993.exe'			## Name of ETGTAB executable (1993 version)
		# self.ETGTAB_EXE = 'ETGTAB-v3.0.2018.exe'			## Name of ETGTAB executable (2018 version)
		self.ETGTAB_EXE = 'ETGTAB-v3.0.2024.exe'			## Name of ETGTAB executable (2024 version)
		self.ETGTAB_INP = 'ETGTAB.INP'						## Name of ETGTAB input file. ETGTAB_EXE will look for this file in its directory.
		self.ETGTAB_OUT = 'ETGTAB.OUT'						## Name of ETGTAB output file. ETGTAB_EXE will create this file in its directory.

		self.SourcePath = os.path.join(self.WorkDir, self.SourceDir)	## Location of Source files
		self.OutputPath = os.path.join(self.WorkDir, self.OutputDir)	## Location of Output files

		self.ETGTAB_EXEFilePath    = os.path.join(self.SourcePath, self.ETGTAB_EXE)
		self.ETGTAB_InputFilePath  = os.path.join(self.SourcePath, self.ETGTAB_INP)
		self.ETGTAB_OutputFilePath = os.path.join(self.SourcePath, self.ETGTAB_OUT)

		if Location and TidePars:
			self.Export_Model_Parameters(Location, TidePars)
		else:
			Location, TidePars = self.Import_Model_Parameters()

		## Location-dependent parameters
		self.Latitude 	= Location['Latitude']				## Latitude  (deg North) ## Ellipsoidal latitude referring to Geodetic Reference System 1980.
		self.Longitude	= Location['Longitude']				## Longitude (deg East) ## Ellipsoidal longitude referring to Geodetic Reference System 1980.
		self.Elevation	= Location['Elevation']				## Elevation above sea level (m) ## Ellipsoidal elevation referring to Geodetic Reference System 1980.
		self.Gravity	= Location['Gravity']				## Local gravity (m/s^2) (only used for tidal tilt computations) ## Setting to zero will tell ETGTAB to use the GRS80 reference value.
		self.Timezone	= Location['Timezone']				## Local timezone (see pytz.common_timezones for complete list)
		self.City		= Location['City']					## City nearest coordinates

		self.tz 		= pytz.timezone(self.Timezone) 		## Local timezone in tzinfo format 

		self.TidalDF 	= pd.DataFrame([])
		self.FirstRun	= True

		self.Azimuth 	= TidePars['Azimuth']				## Azimuthal angle clockwise from North [deg].
															## Only used for tidal tilt and horizontal strain computations.
		self.TidalComps = TidePars['TidalComps'] 			## List of tidal components to compute. Possible values:

		self.nTC 		= len(self.TidalComps)				## Number of components to compute.
		self.ICs		= [0 for iTC in range(self.nTC)]	## List of tidal component indices for ETGTAB.
		self.yLabels    = ['' for iTC in range(self.nTC)]	## List of y-axis labels for use with plots.
		self.yUnits     = ['' for iTC in range(self.nTC)]	## List of y-axs units for use with plots.

		for iTC in range(self.nTC):
			self.Parse_Tidal_Components(iTC)

		self.PrintLevel = TidePars['PrintLevel'] ## Tidal data print level. Possible values: 'None', 'Some', 'All'
		if self.PrintLevel == 'None': ## IPRINT = 0: tidal potential development will not be printed.
			self.IPRINT = 0
		elif self.PrintLevel == 'Some': ## IPRINT = 1: geodetic coefficients and astronomical elements will be printed only.
			self.IPRINT = 1
		elif self.PrintLevel == 'All': ## IPRINT = 2: geodetic coefficients, astronomical elements, and tidal potential development will be printed
			self.IPRINT = 2
		else:
			logging.warning('__init__::Print level {} not recognized...'.format(self.PrintLevel))
			logging.warning('__init__::Setting to default: "None".')
			self.PrintLevel = 'None'
			self.IPRINT = 0

		self.TimeStart  = TidePars['TimeStart'] ## Start date and time for tidal computation in UTC: [YYYY, MM, DD, HH]
		self.TimeSpan   = TidePars['TimeSpan'] ## Time span for tidal computation [hrs]
		self.TimeStep   = TidePars['TimeStep'] ## Time step for tidal computation [s] (only 300 or 3600 s allowed?)

		if abs(self.TimeStep - 300) > 0. and abs(self.TimeStep - 300) < abs(self.TimeStep - 3600):
			logging.warning('__init__::Time step {} s invalid...'.format(self.TimeStep))
			logging.warning('__init__::Setting to closest allowed value: 300 s')
			self.TimeStep = 300
		elif abs(self.TimeStep - 3600.) > 0. and abs(self.TimeStep - 3600) < abs(self.TimeStep - 300):
			logging.warning('__init__::Time step {} s invalid...'.format(self.TimeStep))
			logging.warning('__init__::Setting to closest allowed value: 3600 s')
			self.TimeStep = 3600

		self.TidalModel = TidePars['TidalModel'] ## Tidal potential model to be used. Possible values:
		## 'Doodson1921', 'CTE1973', 'Tamura1987', 'Buellesfeld1985'
		if self.TidalModel == 'Doodson1921': ## IMODEL = 0: DOODSON 1921 model with 378 waves
			self.IMODEL = 0
		elif self.TidalModel == 'CTE1973': ## IMODEL = 1: CARTWRIGHT-TAYLER-EDDEN 1973 model with 505 waves
			self.IMODEL = 1
		elif self.TidalModel == 'Tamura1987': ## IMODEL = 2: TAMURA 1987 model with 1200 waves
			self.IMODEL = 2
		elif self.TidalModel == 'Buellesfeld1985': ## IMODEL = 3: BUELLESFELD 1985 model with 665 waves
			self.IMODEL = 3
		else:
			logging.warning('__init__::Tidal model {} not recognized...'.format(self.TidalModel))
			logging.warning('__init__::Setting to default: "Tamura1987".')
			self.TidalModel = 'Tamura1987'
			self.IMODEL = 2

		self.EarthModel = TidePars['EarthModel'] ## Earth model to be used. Possible values: 'Elastic', 'Rigid'
		## Attention: 'Rigid' should only be used for comparison with model tides computed from ephemeris programs.
		## Attention: For read world predictions, always use 'Elastic'
		if self.EarthModel == 'Elastic': ## IRIGID = 0 for elastic Earth model tides
			self.IRIGID = 0
		elif self.EarthModel == 'Rigid': ## IRIGID = 1 for rigid Earth model tides
			self.IRIGID = 1
		else:
			logging.warning('__init__::Earth model {} not recognized...'.format(self.EarthModel))
			logging.warning('__init__::Setting to default: "Elastic".')
			self.TidalModel = 'Elastic'
			self.IRIGID = 0

	#################### End of self.__init__() #####################
	#################################################################

	def Export_Model_Parameters(self, Location, TidePars):
		"""Export model parameters to files."""

		path = os.path.join(self.OutputPath, self.OutputFile+'_Location.json')
		strg = json.dumps(Location, indent=4)

		with open(path, 'w') as f:
			for s in strg:
				f.write(s)

		path = os.path.join(self.OutputPath, self.OutputFile+'_TidePars.json')
		strg = json.dumps(TidePars, indent=4)

		with open(path, 'w') as f:
			for s in strg:
				f.write(s)

	############### End of Export_Model_Parameters() ################
	#################################################################

	def Import_Model_Parameters(self):
		"""Import model parameters from files."""

		path = os.path.join(self.OutputPath, self.OutputFile+'_Location.json')

		if os.path.exists(path):
			with open(path, 'r') as f:
				Location = json.load(f)
		else:
			logging.error('Import_Model_Parameters::Location file not found: {}'.format(path))
			logging.error('Import_Model_Parameters::Aborting...')
			exit()

		path = os.path.join(self.OutputPath, self.OutputFile+'_TidePars.json')

		if os.path.exists(path):
			with open(path, 'r') as f:
				TidePars = json.load(f)
		else:
			logging.error('Import_Model_Parameters::TidePars file not found: {}'.format(path))
			logging.error('Import_Model_Parameters::Aborting...')
			exit()

		return Location, TidePars
	
	############### End of Import_Model_Parameters() ################
	#################################################################

	def Parse_Tidal_Components(self, iTC):
		"""Parse current tidal component from list and assign ETGTAB index, plot labels, and units."""

		TidalComp = self.TidalComps[iTC] ## Tidal component to output.
		## Possible values:
		## 'Tidal_Potential', 'Vertical_Acceleration', 'Horizontal_Acceleration', 'Vertical_Displacement', 'Horizontal_Displacement',
		## 'Vertical_Strain', 'Horizontal_Strain', 'Areal_Strain', 'Shear_Strain', 'Volume_Strain', 'Ocean_Tides'
		if TidalComp == 'Tidal_Potential': ## IC =-1: tidal potential, geodetic coefficients in m**2/s**2.
			self.ICs[iTC] = -1
			self.yLabels[iTC] = r'$U_{\rm tide}$'
			self.yUnits[iTC] = r'(m$^2$/s$^2$)'
		elif TidalComp == 'Vertical_Acceleration': ## IC = 0: vertical tidal acceleration (gravity tide), geodetic coefficients in nm/s**2 (positive down).
			self.ICs[iTC] = 0
			self.yLabels[iTC] = r'$a_z$'
			self.yUnits[iTC] = r'(nm/s$^2$)'
		elif TidalComp == 'Horizontal_Acceleration': ## IC = 1: horizontal tidal acceleration (tidal tilt) in azimuth DAZ, geodetic coefficients in milli-arcsec
			self.ICs[iTC] = 1
			self.yLabels[iTC] = r'$\theta_x$'
			self.yUnits[iTC] = '(marcsec)'
		elif TidalComp == 'Vertical_Displacement': ## IC = 2: vertical tidal displacement, geodetic coefficients in mm.
			self.ICs[iTC] = 2
			self.yLabels[iTC] = r'$\Delta z$'
			self.yUnits[iTC] = '(mm)'
		elif TidalComp == 'Horizontal_Displacement': ## IC = 3: horizontal tidal displacement in azimuth DAZ, geodetic coefficients in mm.
			self.ICs[iTC] = 3
			self.yLabels[iTC] = r'$\Delta x$'
			self.yUnits[iTC] = '(mm)'
		elif TidalComp == 'Vertical_Strain': ## IC = 4: vertical tidal strain, geodetic coefficients in 10**-9 = nstr.
			self.ICs[iTC] = 4
			self.yLabels[iTC] = r'$S_z$'
			self.yUnits[iTC] = '(nstr)'
		elif TidalComp == 'Horizontal_Strain': ## IC = 5: horizontal tidal strain in azimuth DAZ, geodetic coefficients in 10**-9 = nstr.
			self.ICs[iTC] = 5
			self.yLabels[iTC] = r'$S_x$'
			self.yUnits[iTC] = '(nstr)'
		elif	TidalComp == 'Areal_Strain': ## IC = 6: areal tidal strain, geodetic coefficients  in 10**-9 = nstr.
			self.ICs[iTC] = 6
			self.yLabels[iTC] = r'$S_{\rm areal}$'
			self.yUnits[iTC] = '(nstr)'
		elif TidalComp == 'Shear_Strain': ## IC = 7: shear tidal strain, geodetic coefficients  in 10**-9 = nstr.
			self.ICs[iTC] = 7
			self.yLabels[iTC] = r'$S_{\rm shear}$'
			self.yUnits[iTC] = '(nstr)'
		elif TidalComp == 'Volume_Strain': ## IC = 8: volume tidal strain, geodetic coefficients in 10**-9 = nstr.
			self.ICs[iTC] = 8
			self.yLabels[iTC] = r'$S_{\rm volume}$'
			self.yUnits[iTC] = '(nstr)'
		elif TidalComp == 'Ocean_Tides': ## IC = 9: ocean tides, geodetic coefficients in mm.
			self.ICs[iTC] = 9
			self.yLabels[iTC] = r'$\Delta z_{\rm ocean}$'
			self.yUnits[iTC] = '(mm)'
		else:
			logging.warning('__init__::Tidal component {} not recognized...'.format(TidalComp))
			logging.warning('__init__::Setting to default: "Vertical_Acceleration"')
			TidalComp = 'Vertical_Acceleration'
			self.ICs[iTC] = 0
			self.yLabels[iTC] = r'$a_z$'
			self.yUnits[iTC] = r'(nm/s$^2$)'

	############# End of self.Parse_Tidal_Components() ##############
	#################################################################

	def Write_Input_File(self, iTC):
		"""Write input parameters to file."""

		##===========================================================
		## Sample input for ETGTAB:
		##===========================================================
		## Ellipsoidal latitude  in degree:            44.804
		## Ellipsoidal longitude in degree:            -0.605
		## Ellipsoidal height in meter:                21.000
		## Gravity in m/s**2:                           9.806
		## Azimuth in degree clockwise from north:      0.000
		## Earth tide component (-1...9):                   0
		## Printout of tidal potential (0...2):             0 
		## Initial epoch(YYMMDDHH):                      2019   12   05   18
		## Number of hours:                                38
		## Time interval in secs:                         300
		## Tidal potential (0...3):                         2
		## Tides for a rigid Earth (IRIGID=1):              0
		##===========================================================

		with open(self.ETGTAB_InputFilePath, 'w') as f:
			f.write('Ellipsoidal latitude  in degree:            {:5.3f}\n'.format(self.Latitude))
			f.write('Ellipsoidal longitude in degree:            {:5.3f}\n'.format(self.Longitude))
			f.write('Ellipsoidal height in meter:                {:5.3f}\n'.format(self.Elevation))
			f.write('Gravity in m/s**2:                           {:4.3f}\n'.format(self.Gravity))
			f.write('Azimuth in degree clockwise from north:      {:4.3f}\n'.format(self.Azimuth))
			f.write('Earth tide component (-1...9):                  {: 2d}\n'.format(self.ICs[iTC]))
			f.write('Printout of tidal potential (0...2):             {:d}\n'.format(self.IPRINT)) 
			f.write('Initial epoch(YYMMDDHH):                      {:04d}   {:02d}   {:02d}   {:02d}\n'.format(*self.TimeStart))
			f.write('Number of hours:                              {:4d}\n'.format(self.TimeSpan))
			f.write('Time interval in secs:                        {:4d}\n'.format(self.TimeStep))
			f.write('Tidal potential (0...3):                         {:d}\n'.format(self.IMODEL))
			f.write('Tides for a rigid Earth (IRIGID=1):              {:d}\n'.format(self.IRIGID))
			f.write(' Gravimetric tides for Black Forest Observatory\n')
			f.write(' Parameters from LaCoste Romberg G249F\n')
			f.write('  120 days (8.12.1988 - 11.4.1989)\n')
			f.write('\n\n\n\n\n\n\n')
			f.write('14\n')
			f.write('    1  285 LONG       1.1500    0.0000\n')
			f.write('  286  428 Q1         1.1429   -0.3299\n')
			f.write('  429  488 O1         1.1464    0.0791\n')
			f.write('  489  537 M1         1.1537    0.0583\n')
			f.write('  538  592 P1K1       1.1349    0.3174\n')
			f.write('  593  634 J1         1.1537   -0.0078\n')
			f.write('  635  739 OO1        1.1538   -0.0531\n')
			f.write('  740  839 2N2        1.1560    2.5999\n')
			f.write('  840  890 N2         1.1761    2.5446\n')
			f.write('  891  947 M2         1.1840    2.0816\n')
			f.write('  948  987 L2         1.1906    0.1735\n')
			f.write('  988 1121 S2K2       1.1865    0.6230\n')
			f.write(' 1122 1204 M3         1.0584   -0.0814\n')
			f.write(' 1205 1214 M4         0.5777   92.2440\n')
			f.write(' \n')

	################# End of self.Write_Input_File() ################
	#################################################################

	def Run_ETGTAB(self, iTC, ReExecute=True):
		"""Execute ETGTAB tidal gravity anomaly software."""

		if ReExecute:
			self.Write_Input_File(iTC)

			if os.path.exists(self.ETGTAB_OutputFilePath):
				## Delete pre-existing output file to avoid unexpected behaviour
				outpath = Path(self.ETGTAB_OutputFilePath)
				outpath.unlink()

			subprocess.run(self.ETGTAB_EXEFilePath, cwd=self.SourcePath, check=True)
			# subprocess.check_call(self.ETGTAB_EXEFilePath, cwd=self.SourcePath)

		self.Read_Raw_Output_File(iTC)

	################### End of self.Run_ETGTAB() ####################
	#################################################################

	def Read_Raw_Output_File(self, iTC):
		"""Read raw ETGTAB output into a dataframe."""

		df = pd.read_csv(self.ETGTAB_OutputFilePath, sep='\s+', header=None, engine='python', skiprows=46, on_bad_lines='skip')

		TidalComp = self.TidalComps[iTC]
		df.columns = ['Y', 'M', 'D', 'H', 'm', 's', TidalComp]
		nRows = df.shape[0]

		if self.FirstRun:
			self.FirstRun = False
			self.TidalDF['DateTime_UTC'] = [dt.datetime(df['Y'].iloc[r], df['M'].iloc[r], df['D'].iloc[r], df['H'].iloc[r], df['m'].iloc[r], df['s'].iloc[r], tzinfo=pytz.UTC) for r in range(nRows)]
			self.TidalDF['DateTime_TZ'] = [self.TidalDF['DateTime_UTC'].iloc[r].astimezone(tz=self.tz) for r in range(nRows)]
			# self.TidalDF['Timestamp_UTC']  = np.array([self.TidalDF['DateTime_UTC'].iloc[r].timestamp() for r in range(nRows)])
			# self.TidalDF['Timestamp_TZ']  = np.array([self.TidalDF['DateTime_TZ'].iloc[r].replace(tzinfo=None).timestamp() for r in range(nRows)])

		self.TidalDF[TidalComp] = df[TidalComp].copy()

	############## End of self.Read_Raw_Output_File() ###############
	#################################################################

	def Move_Raw_Output_File(self, iTC):
		"""Move and rename raw ETGTAB output file into 'Output' directory."""

		## Path object for 'ETGTAB.OUT' file 
		rawPath = Path(self.ETGTAB_OutputFilePath)

		## Strings representing new output paths
		newPath = os.path.join(self.OutputPath, self.OutputFile+'_'+self.TidalComps[iTC]+'.txt')

		try:
			## Wait for other processes to complete
			time.sleep(1)
			## Rename 'ETGTAB.OUT' file and move to 'Output' directory
			rawPath = rawPath.replace(newPath)
		except Exception as e:
			logging.error(f'Move_Raw_Output_File::Error moving {self.ETGTAB_OUT} to {newPath}:')
			logging.error(f'Move_Raw_Output_File::{e}')
			logging.error('Move_Raw_Output_File::Aborting...')
			exit()

	############## End of self.Move_Raw_Output_File() ###############
	#################################################################

	def Delete_Raw_Output_Files(self):
		"""Delete raw ETGTAB output files from 'Output' directory."""

		pass

	############# End of self.Delete_Raw_Output_Files() #############
	#################################################################

	def Write_CSV_File(self):
		"""Write dataframe to CSV file."""

		## Output path for CSV file
		path = os.path.join(self.OutputPath, self.OutputFile+'.csv')

		self.TidalDF.to_csv(path, sep='\t', header=True, index=False, float_format='%.6E')

	################# End of self.Write_CSV_File() ##################
	#################################################################

	def Read_CSV_File(self):
		"""Write dataframe to CSV file."""

		## Output path for CSV file
		path = os.path.join(self.OutputPath, self.OutputFile+'.csv')

		self.TidalDF = pd.read_csv(path, sep='\t', header=0, index_col=False, parse_dates=[0,1])

		## For use with Pandas version >= 2.0.0 (untested)
		# self.TidalDF = pd.read_csv(path, sep='\t', header=0, index_col=False, parse_dates=[0,1], date_format='%Y-%m-%d %H:%M:%S.%f')

	################# End of self.Read_CSV_File() ##################
	#################################################################

	def Generate_Model_Output(self, Keep_Raw_Output=False):
		"""Generate Earth tidal gravity model output in the form of raw text files and a CSV file in the Output folder."""

		for iTC in range(self.nTC):
			self.Run_ETGTAB(iTC, ReExecute=True)
			if Keep_Raw_Output:
				self.Move_Raw_Output_File(iTC)
			time.sleep(0.5)

		self.Write_CSV_File()

	############# End of self.Generate_Model_Output() ###############
	#################################################################

	def Plot_Tidal_Component(self, iTC, ax, color, linestyle, marker, x=None, y=None, ylabel=None):
		"""Plot output from the ETGTAB computation.
		The default function is to plot the tidal component corresponding to index iTC.
		ARGUMENTS:
		  iTC (int)       - Index of tidal component to plot.
		  ax (fig axis)   - Figure axis to store plot.
		  color (str)     - Color of plot.
		  linestyle (str) - Line style of plot.
		  marker (str)    - Marker style of plot.
		  x (list)        - List of datetimes where tidal component is computed.
		  y (np.array)    - Array containing tidal component values.
		  ylabel (str)    - y-axis label.
		"""

		TidalComp = self.TidalComps[iTC]

		if x is None and y is None:
			x = list(self.TidalDF['DateTime_TZ'])
			y = self.TidalDF[TidalComp].to_numpy()

		if TidalComp == 'Horizontal_Acceleration':
			y = y*(np.pi/180.)/3600. ## Convert from m-arcsec to mrad
			self.yUnits[iTC] = '(mrad)'

		if ylabel is None:
			ylabel = self.yLabels[iTC]+' '+self.yUnits[iTC]

		ax.plot(x, y, color=color, linestyle=linestyle, marker=marker)

		td = (x[-1] - x[0])/dt.timedelta(days=1)
		if td <= 3.5:
			ax.xaxis.set_minor_locator(HourLocator(range(0, 25, 3), tz=self.tz))
			ax.xaxis.set_major_locator(HourLocator(range(0, 25, 6), tz=self.tz))
		elif td <= 7:
			ax.xaxis.set_minor_locator(HourLocator(range(0, 25, 6), tz=self.tz))
			ax.xaxis.set_major_locator(HourLocator(range(0, 25, 12), tz=self.tz))

		ax.xaxis.set_major_formatter(DateFormatter('%b %d %H:%M', tz=self.tz))

		for label in ax.get_xticklabels(which='major'):
			label.set_ha('right')
			label.set_rotation(30)

		ax.set_ylabel(ylabel)

		return ax

	################## End of Plot_Tidal_Component() ################
	#################################################################

	def Plot_All_Tidal_Components(self, colors, linestyle, marker, autotitle=False):
		"""Plot all Earth tidal model components stored in data frame.
		ARGUMENTS:
		colors (list)    - List of plot colors to use for each tidal component.
		linestyle (str)  - Line style of plot.
		marker (str)     - Marker style of plot.
		autotitle (bool) - Flag to auto-generate plot title.
		"""

		if self.nTC <= 2:
			nrows = self.nTC
			ncols = 1
		elif self.nTC == 3:
			nrows = 3
			ncols = 1
		elif self.nTC == 4:
			nrows = 2
			ncols = 2
		elif self.nTC <= 6:
			nrows = 3
			ncols = 2
		elif self.nTC <= 8:
			nrows = 4
			ncols = 2
		elif self.nTC == 9:
			nrows = 3
			ncols = 3
		elif self.nTC == 10:
			nrows = 5
			ncols = 2

		fig, axs = plt.subplots(nrows=nrows, ncols=ncols, figsize=(ncols*6,nrows*3.5), sharex=True, constrained_layout=True)

		if self.nTC == 1:
			axs = [axs]
		else:
			axs = axs.flatten()

		if autotitle:
			if self.nTC == 1:
				axs[0].set_title('{} in {} (tz = {})'.format(self.TidalComps[0], self.City, self.Timezone), ha='center', fontsize=12)
			else:
				fig.suptitle('ETG Model in {} (tz = {})'.format(self.City, self.Timezone), ha='center', fontsize=12)

		for iTC in range(self.nTC):
			axs[iTC] = self.Plot_Tidal_Component(iTC, axs[iTC], colors[iTC], linestyle, marker)
			axs[iTC].grid()

		plt.show()

	############### End of Plot_All_Tidal_Components() ##############
	#################################################################

#####################################################################
################# End of class EarthTidalGravity ####################
#####################################################################

if __name__ == '__main__':
	pass

