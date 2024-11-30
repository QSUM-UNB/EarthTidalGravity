#####################################################################
## Filename:	Main.py
## Author:		B. Barrett
## Description: Python wrapper for the ETGTAB Fortran program to predict Earth tidal gravity components.
## Version:		1.1.0
## Last Mod:	30/11/2024
#####################################################################

import datetime as dt
import matplotlib.pyplot as plt
import numpy as np
import pytz

from scipy.interpolate import interp1d

from ETGTAB import *

etg   = Earth_Tidal_Gravity
utils = Utilities

def Main():
	"""Main entry point for ETGTAB."""

	Location1 = {
		'Latitude':  	45.9500,				## Latitude  (deg North)
		'Longitude': 	66.6411,				## Longitude (deg East)
		'Elevation': 	45.,					## Elevation above sea level (m)
		'Gravity': 		9.8067624,				## Local gravity (m/s^2)
		'Timezone':		'Canada/Atlantic',		## Local timezone (e.g. 'Europe/Paris', 'Canada/Atlantic', 'UTC', see pytz.common_timezones)
		'City':			'Fredericton, NB',		## City description
	}

	Location2 = {
		'Latitude':  	45.3051,				## Latitude  (deg North)
		'Longitude': 	66.0855,				## Longitude (deg East)
		'Elevation': 	81.,					## Elevation above sea level (m)
		'Gravity': 		9.8064168,				## Local gravity (m/s^2)
		'Timezone':		'Canada/Atlantic',		## Local timezone (print pytz.common_timezones for examples)
		'City':			'St. John, NB',			## City description
	}

	Location3 = {
		'Latitude':  	43.7733,				## Latitude  (deg North)
		'Longitude': 	79.5064,				## Longitude (deg East)
		'Elevation': 	198.,					## Elevation above sea level (m)
		'Gravity': 		9.8041740,				## Local gravity (m/s^2)
		'Timezone':		'Canada/Atlantic',		## Local timezone (print pytz.common_timezones for examples)
		'City':			'Toronto, ON',			## City description
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

	# utils.Time_Series('UNB_FR', Location1, TidePars)
	# utils.Time_Series('UNB_SJ', Location2, TidePars)
	# utils.Time_Series('YorkU', Location3, TidePars)

	# utils.Compare_Locations('UNB_FR', Location1, 'UNB_SJ', Location2, TidePars)
	utils.Compare_Locations('UNB_FR', Location1, 'YorkU', Location3, TidePars)

	# utils.Interpolate_Example(Location1)

if __name__ == '__main__':
	Main()
