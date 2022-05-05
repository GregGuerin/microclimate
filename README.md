# microclimate
R functions to calculate bioclimatic variables by processing raw climate observations from microclimate transects 

# 

This package can be used by cloning as a folder to your desktop and starting an R project in that directory, or by package installation from github. The package imports functions from the packages stringr and dplyr (recommended to install these first) and can be installed as follows:
```
library(devtools)
install_github("GregGuerin/microclimate")
library(microclimate)
``` 

Input data are temperature observations from sensors installed along microclimate transects from the edge to interior of a patch of vegetation at distances of 0, 2, 5, 10, 20, 40 & 80 m. Data from each logger is a text file and location along the transect is assumed to be coded as site-transectNumber-Distance, e.g., KA-1-0 (Site 'KA', transect 1, 0 m from edge). If present (not in sample data), soil sensors are denoted with an S (e.g., KA-1-0S) and control observations outside of the vegetation are denoted as, e.g., KA-1-X. Note raw data format will vary between logging devices so see example data for currently used input data.


To run the code with the example data (which must be cloned/downloaded to your desktop):

## Step 1
Compile data from all sensors into one object and load sun times data
```
my.sensor.data <- read_sensor_data("./Sample_data/KA*") #input file path to set of .txt files with logged climate measurements
```
The 'my.sensor.data' object returned above contains all the data from the .txt files in the folder together in one data frame with the 'Sensor' field denoting which file/sensor it came from.


## Step 2
Load external sun rise and set times and format for analysis

Load sun rise/set times data: data can be downloaded from Geoscience Australia [https://geodesyapps.ga.gov.au/sunrise] or relevant local source elsewhere, and formatted in excel (e.g. to combine data from different years to cover a climate logging period); be careful to download the data in the correct timezone, in this case ACDT. Beware of daylight savings - e.g. if data are in ACDT, make sure logged data are too.
```
rise_set <- read.csv("./Sample_data/rise_set_combined.csv") #e.g.
```
Copyright: Based on Sunrise, Sunset & Twilight Times, https://geodesyapps.ga.gov.au/sunriseby Geoscience Australia which is © Commonwealth of Australia and is provided under a Creative Commons Attribution 4.0 International Licence and is subject to the disclaimer of warranties in section 5 of that licence.

Now run formatting function to calculate 1 hour before and after sunset and sunrise
```
rise_set <- format_sun_times(rise_set)
```

## Step 3
Process raw data to include sun times, code as day or night, add season etc
```
my.sensor.data <- format_sensor_data(my.sensor.data, rise_set) 
```

## Step 4
Iterate through sensor and day to get daily climate values for min / max / mean / standard deviation / range and then calculate mean values per night/day for month, season and 'year' (year at this state simply meaning all the data/dates)
```
temp_means <- calculate_means(my.sensor.data)
```
Returns a list of data frames each giving night/day max/mean/min/range/sd temps, respectively for $Day, $Month, $Season and $Year 

