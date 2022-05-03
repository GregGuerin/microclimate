###############################################################
#Step 1 - compile data from all sensors into one object and load sun times data
template <- read_sensor_data("/KA/KA*")

#the 'template' object returned above should have all the data from the .txt files in the folder together in one data frame with the 'Sensor' field denoting which file/sensor it came from.


##################################

#Load sun rise/set times data::
#data are downloaded from Geoscience Australia or relevant source elsewhere, and formatted in excel; be careful to download the data in the correct timezone, in this case ACDT
#beware of daylight savings - e.g. if data are in ACDT, make sure logged data are too

rise_set <- read.csv("rise_set_times.csv") #e.g.

###############################################################
#Step 2 - process the raw data to include sun times, code as day or night, add season etc

template <- format_sensor_data(template, rise_set) 

###############################################################
#Step 3 - iterate through sensor and day to get daily climate values for min / max / mean / standard deviation / range and then calculate mean values per night/day for month, season and 'year' (year being all the data)

temp_means <- calculate_means(template) #returns night/day mean max/mean/min/range/sd/CV temps for winter & summer

#################