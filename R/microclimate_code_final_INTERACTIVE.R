###############################################################
#Step 1 - compile data from all sensors into one object and load sun times data

my.sensor.data <- read_sensor_data("./Sample_data/KA*") #input file path to set of .txt files with logged climate measurements

#the 'my.sensor.data' object returned above contains all the data from the .txt files in the folder together in one data frame with the 'Sensor' field denoting which file/sensor it came from.

##################################
#Step 2 - load external sun rise and set times and format for analysis
#Load sun rise/set times data::

#data can be downloaded from Geoscience Australia [https://geodesyapps.ga.gov.au/sunrise] or relevant local source elsewhere, and formatted in excel (e.g. to combine data from different years to cover a climate logging period); be careful to download the data in the correct timezone, in this case ACDT
#beware of daylight savings - e.g. if data are in ACDT, make sure logged data are too

rise_set <- read.csv("./Sample_data/rise_set_combined.csv") #e.g.
#Copyright: Based on Sunrise, Sunset & Twilight Times, https://geodesyapps.ga.gov.au/sunriseby Geoscience Australia which is © Commonwealth of Australia and is provided under a Creative Commons Attribution 4.0 International Licence and is subject to the disclaimer of warranties in section 5 of that licence.


#run formatting function to calculate 1 hour before and after sunset and sunrise
rise_set <- format_sun_times(rise_set)

###############################################################
#Step 3 - process the raw data to include sun times, code as day or night, add season etc

my.sensor.data <- format_sensor_data(my.sensor.data, rise_set) 

###############################################################
#Step 4 - iterate through sensor and day to get daily climate values for min / max / mean / standard deviation / range and then calculate mean values per night/day for month, season and 'year' (year being all the data)

temp_means <- calculate_means(my.sensor.data) #returns night/day mean max/mean/min/range/sd/CV temps for winter & summer

#################