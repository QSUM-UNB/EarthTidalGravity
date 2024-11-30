#####################################################################
## Filename:	Utilities.py
## Author:		B. Barrett
## Description: Utility functions for Earth tidal gravity program.
## Version:		1.1.0
## Last Mod:	30/11/2024
#####################################################################

import datetime as dt
import matplotlib.pyplot as plt
import numpy as np
import pytz

from scipy.interpolate import interp1d

if __name__ == '__main__':
	from Earth_Tidal_Gravity import EarthTidalGravity
else:
	from .Earth_Tidal_Gravity import EarthTidalGravity

#################################################################
#################################################################

def Time_Series(Prefix, Location, TidePars):
	"""Generate time series of tidal components at one location and plot the result.
	ARGUMENTS:
	  Prefix   (str)  - File prefix for storing ETGTAB output.
 	  Location (dict) - Contains location parameters.
	  TidePars (dict) - Contains ETGTAB model parameters.
	"""

	model = EarthTidalGravity(Prefix, Location, TidePars)
	model.Generate_Model_Output()
	model.Plot_All_Tidal_Components(colors=['crimson', 'royalblue', 'forestgreen'], linestyle='-', marker='', autotitle=True)

##################### End of Time_Series() ######################
#################################################################

def Compare_Locations(Prefix1, Location1, Prefix2, Location2, TidePars):
	"""Compare time series of ETGTAB output at two locations.
	CSV files with Prefix1 and Prefix2 should must exist in the Output folder.
	The code loads model data from these CSV files and plots the results for comparison.
	ARGUMENTS:
	  Prefix   (str)  - File prefix for storing ETGTAB output.
	  Location (dict) - Contains location parameters.
	  TidePars (dict) - Contains ETGTAB model parameters.
	"""

	model1 = EarthTidalGravity(Prefix1, Location=Location1, TidePars=TidePars)
	model1.Read_CSV_File()

	model2 = EarthTidalGravity(Prefix2, Location=Location2, TidePars=TidePars)
	model2.Read_CSV_File()

	nrows = 2
	ncols = model1.nTC

	_, axs = plt.subplots(nrows=nrows, ncols=ncols, figsize=(ncols*8,nrows*2.5), squeeze=False, sharex=True, constrained_layout=True)

	# axs = axs.flatten()

	x  = list(model1.TidalDF['DateTime_TZ'])

	for iTC in range(ncols):
		TC = model1.TidalComps[iTC]
		yLabel = model1.yLabels[iTC]
		yUnit = model1.yUnits[iTC]
		y1 = model1.TidalDF[TC].to_numpy()
		y2 = model2.TidalDF[TC].to_numpy()

		axs[0,iTC] = model1.Plot_Tidal_Component(iTC, axs[0,iTC], color='crimson', linestyle='-', marker='', x=x, y=y1, ylabel=yLabel+' '+yUnit)
		axs[0,iTC] = model1.Plot_Tidal_Component(iTC, axs[0,iTC], color='royalblue', linestyle='-', marker='', x=x, y=y2, ylabel=yLabel+' '+yUnit)
		axs[1,iTC] = model1.Plot_Tidal_Component(iTC, axs[1,iTC], color='forestgreen', linestyle='-', marker='', x=x, y=y2-y1, ylabel='Difference '+yUnit)

	for ax in axs.flatten():
		ax.grid()

	plt.show()

################## End of Compare_Locations() ###################
#################################################################

def Interpolate(Location, tList, ReExecute=False):
	"""Compute tidal gravity anomaly at specific times given by tList.
	Here, ETGTAB is used to compute the veritical tidal gravity component on a grid with 300 s resolution.
	Then an interpolant is used to predict values between these grid points at the times in tList.
	ARGUMENTS:
	  Location  (dict) - Contains location parameters at which to compute tidal model.
  	  tList     (list) - List of ordered timestamps (in seconds since epoch 01/01/1970) at which to compute tidal component.
	  ReExecute (bool) - Flag for recomputing tides for new times or location.
	"""

	## Reference time is the start time [s] rounded down to the nearest hour - 1
	t0       = np.floor(tList[0]/3600. - 1.)*3600.
	dt0_UTC  = dt.datetime.fromtimestamp(t0, tz=pytz.UTC)
	## Time span [hrs] is the requested span rounded up to the nearest hour + 1
	timeSpan = int(np.ceil((tList[-1] - tList[0])/3600.)) + 1

	TidePars = {
		'Azimuth':		0.,						## [deg]
		'TidalComps':	['Vertical_Acceleration'], ## List of tidal components to compute.
		'PrintLevel':	'None',					## 'None', 'Some', 'All'
		'TimeStart':	[dt0_UTC.year, dt0_UTC.month, dt0_UTC.day, dt0_UTC.hour], ## [YYYY,MM,DD,HH] in UTC timezone
		'TimeSpan':		timeSpan, 				## [hrs]
		'TimeStep':		300, 					## [s] (300 or 3600 only)
		'TidalModel':	'Tamura1987',			## 'Doodson1921', 'CTE1973', 'Tamura1987', 'Buellesfeld1985'
		'EarthModel':	'Elastic'				## 'Rigid', 'Elastic'
	}

	model = EarthTidalGravity('Temp', Location, TidePars)
	model.Run_ETGTAB(0, ReExecute=ReExecute)
	model.Write_CSV_File()

	TidalComp = model.TidalComps[0]

	nTide = model.TidalDF[TidalComp].shape[0]

	## Times relative to reference time at which to compute tides
	tTide = np.array([(model.TidalDF['DateTime_UTC'].iloc[i] - dt0_UTC).total_seconds() for i in range(nTide)])
	gTide = model.TidalDF[TidalComp].to_numpy() ## nm/s^2, positive downward
	fTide = interp1d(tTide, gTide, kind='quadratic')
	gTide = fTide(tList - tList[0])

	return gTide

##################### End of Interpolate() ######################
#################################################################

def Interpolate_Example(Location):
	"""Example implementation of ETG_Interpolate."""

	tz = pytz.timezone(Location['Timezone'])

	dt_now = dt.datetime.now().astimezone(tz)
	tStart = dt_now.timestamp()
	tStop  = (dt_now + dt.timedelta(days=1, hours=0, minutes=0, seconds=0)).timestamp()
	nData  = 1001
	tStep  = (tStop - tStart)/(nData - 1)

	print('dt_now =',dt_now)
	print('tStart =',tStart)
	print('tStop  =',tStop)
	print('tStep  =',tStep)

	tList = np.linspace(tStart, tStop, num=nData, endpoint=True)
	dtList = [dt.datetime.fromtimestamp(tList[i], tz=pytz.timezone('UTC')) for i in range(nData)]
	gTide = Interpolate(Location, tList, ReExecute=True)

	_, axs = plt.subplots(nrows=1, ncols=1, figsize=(8,3), sharex=True, constrained_layout=True)

	axs.plot((tList - tList[0])/3600., gTide, color='black', linestyle='', marker='.')
	axs.set_xlabel('Time (hrs) - {:%Y-%m-%d %H:%M} ({})'.format(dtList[0].astimezone(tz), tz))
	axs.set_ylabel(r'$a_z$  (nm/s$^2$)')
	axs.grid()
	plt.show()

################# End of Interpolate_Example() ##################
#################################################################

if __name__ == '__main__':
	
	Location1 = {
		'Latitude':  	45.9500,				## Latitude  (deg North)
		'Longitude': 	66.6411,				## Longitude (deg East)
		'Elevation': 	45.,					## Elevation above sea level (m)
		'Gravity': 		9.8067624,				## Local gravity (m/s^2)
		'Timezone':		'Canada/Atlantic',		## Local timezone (e.g. 'Europe/Paris', 'Canada/Atlantic', 'UTC', see pytz.common_timezones)
		'City':			'Fredericton, NB',		## City description
	}

	## Allowed tidal components:
	## 'Tidal_Potential', 'Vertical_Acceleration', 'Horizontal_Acceleration', 'Vertical_Displacement', 'Horizontal_Displacement',
	## 'Vertical_Strain', 'Horizontal_Strain', 'Areal_Strain', 'Shear_Strain', 'Volume_Strain', 'Ocean_Tides'
	TidePars = {
		'Azimuth':		0.,						## [deg]
		'TidalComps':	['Vertical_Acceleration', 'Vertical_Displacement'], ## List of tidal components to compute.
		'PrintLevel':	'None',					## 'None', 'Some', 'All'
		'TimeStart':	[2024, 8, 1, 0],		## [YYYY,MM,DD,HH] in UTC timezone
		'TimeSpan':		24*31, 					## [hrs]
		'TimeStep':		3600, 					## [s] (300 or 3600 only)
		'TidalModel':	'Tamura1987',			## 'Doodson1921', 'CTE1973', 'Tamura1987', 'Buellesfeld1985'
		'EarthModel':	'Elastic'				## 'Rigid', 'Elastic'
	}

	Time_Series('UNB_FR', Location1, TidePars)
