# EarthTidalGravity
A python wrapper for ETGTAB (Earth Tidal Gravity TABle) - a program to predict Earth tidal gravity accelerations and other tidal components.

ETGTAB computes Earth tide components (tidal potential, acceleration, tilt, displacements, strains, etc.) at any location for time intervals of 5 or 60 minutes. Tidal components can be computed using different tidal potential developments (DOODSON 1921 with 378 waves, CARTWRIGHT-TAYLER-EDDEN 1973 with 505 waves, TAMURA 1987 with 1200 waves, or BUELLESFELD 1985 with 665 waves) for a rigid or elastic Earth.

Note that ETGTAB has been superseeded by PREDICT - a module in the ETERNA software package (version 3.4).
ETGTAB and PREDICT were originally written in FORTRAN by the late Prof. H.-G. Wenzel, Geodetic Institute, Karlsruhe University.

## Basic usage

- The user specifies location parameters (coordinates, local gravity, timezone, etc) and tide model parameters (tide components, time intervals, potential model, etc). The code separates these parameters into dictionaries `Location` and `TidePars` to allow for simple comparisons between locations or tide models. A basic example is shown below:

```Python
from ETGTAB import *

etg   = Earth_Tidal_Gravity
utils = Utilities

Location = {
  'Latitude':  45.9500,           ## Latitude  (deg North)
  'Longitude': 66.6411,           ## Longitude (deg East)
  'Elevation': 45.,               ## Elevation above sea level (m)
  'Gravity':   9.8067624,         ## Local gravity (m/s^2)
  'Timezone':  'Canada/Atlantic', ## Local timezone (e.g. 'Europe/Paris', 'Canada/Atlantic', 'UTC', see pytz.common_timezones)
  'City':      'Fredericton, NB', ## City description
}

TidePars = {
  'Azimuth':    0.,              ## [deg]
  'TidalComps': ['Vertical_Acceleration', 'Vertical_Displacement'], ## List of tidal components to compute.
  'PrintLevel': 'None',          ## 'None', 'Some', 'All'
  'TimeStart':  [2024, 8, 1, 0], ## [YYYY,MM,DD,HH] in UTC timezone
  'TimeSpan':   24*31,           ## [hrs]
  'TimeStep':   3600,            ## [s] (300 or 3600 only)
  'TidalModel': 'Tamura1987',    ## 'Doodson1921', 'CTE1973', 'Tamura1987', 'Buellesfeld1985'
  'EarthModel': 'Elastic'        ## 'Rigid', 'Elastic'
}

utils.Time_Series('Example', Location, TidePars)
```

- Possible tidal components are: 'Tidal_Potential', 'Vertical_Acceleration', 'Horizontal_Acceleration', 'Vertical_Displacement', 'Horizontal_Displacement', 'Vertical_Strain', 'Horizontal_Strain', 'Areal_Strain', 'Shear_Strain', 'Volume_Strain', 'Ocean_Tides'

 - This code produces the following plot:
![Example ETG time series in Fredericton, NB for August 1-31 2024](/Figures/ETG_Example_TimeSeries.jpeg)

- In the folder `Output`, this code produces a CSV file called `Example.csv` containing all requested tidal components, along with copies of the dictionaries `Location` and `TidePars` in json format called `Example_Location.json` and `Example_TidePars.json`.

## File Descriptions

- `Main.py`: main the entry point for generating model output.

- `ETGTAB/Earth_Tidal_Gravity.py`: contains the Python wrapper for ETGTAB, sets default plot options, configures the logger, and defines the class `EarthTidalGravity`. The class has the following methods:
  - `__init__()`: Initializes class attributes.
  - `Export_Model_Parameters()`: Exports model parameters to files.
  - `Import_Model_Parameters()`: Import model parameters from files.
  - `Parse_Tidal_Components()`: Parses current tidal component from list and assigns ETGTAB index, plot labels, and units.
  - `Write_Input_File()`: Write ETGTAB input parameters to a formatted file.
  - `Run_ETGTAB()`: Python wrapper for ETGTAB. This method overwrites the input file, deletes any existing raw output file, and runs the ETGTAB executable as a subprocess. It then reads the new raw output file.
  - `Read_Raw_Output_File()`: Read raw ETGTAB output into a dataframe.
  - `Move_Raw_Output_File()`: Move and rename raw ETGTAB output file into 'Output' directory.
  - `Write_CSV_File()`: Write dataframe to CSV file.
  - `Read_CSV_File()`: Read CSV file into dataframe.
  - `Generate_Model_Output()`: Generate Earth tidal gravity model output in the form of raw text files and a CSV file in the Output folder.
  - `Plot_Tidal_Component()`: Plot output from the ETGTAB computation.
  - `Plot_All_Tidal_Components()`: Plot all Earth tidal model components stored in data frame.

- `ETGTAB/Utilities.py`: Utilities and helper functions that manipulate Earth tidal gravity data.
  - `TimeSeries()`: Generate time series of tidal components at one location and plot the result.
  - `Compare_Locations()`: Compare time series of tidal component at two locations.
  - `Interpolate()`: Compute tidal gravity anomaly at times specified by a list of timestamps. Here, ETGTAB is used to compute the veritical tidal gravity on a grid with 300 s resolution. Then an interpolant is used to predict values between these grid points at times in the list.
  - `Interpolate_Example()`: Example implementation of ETG_Interpolate().
