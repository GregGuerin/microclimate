#######################################################

#Microclimate sensor data functions - source this code: > source('microclimate_functions.R')


#######################################################


#Function 1


#######################################################

read_sensor_data <- function(sensor_data_folder_path) {

#this function loads in the raw individual data files and merges them together
	
	if(class(sensor_data_folder_path) != "character") {stop("File path must be in character format.")}

file_list <- Sys.glob(sensor_data_folder_path) #call a list of file paths with the same prefix or wildcard/file extension

#open files from the above list, and append the data to a single R object

#loop through entire list of paths/files and append to the template:
for (i in 1:length(file_list)) { #iterate through the number of files to read, so i starts at one and goes up to the length of the list... 
  
###raw data files have special characters in the header, so here is a work around:
working_header <- as.character(stringr::str_split_fixed(readLines(file_list[i])[1], ",", 6)) #get a vector of column headers from the current file (readlines can handle the characters)

working_header[c(3,5)] <- c("Celsius(C)", "dewpoint(C)") #manually force new names with no special characters
  
  working_file <- read.csv(file=file_list[i], col.names=working_header) #on each iteration, read in the next file on the list, add my new header row with no special characters
  
  working_file$Sensor <- colnames(working_file)[1] #create a new column where each value is the header of column 1 (site/sensor ID or whatever)
  colnames(working_file)[1] <- "Observation_number" #set the name of that column to something standard instead of a sensor ID
  
  if(i == 1) { #first iteration only
    template <- working_file #assign that to 'template', which we will add to subsequently
  } #close i == 1
  
  if(i > 1) { #for all iterations after the first
    template <- rbind(template, working_file) #we can add the latest file as rows to the template because they have the same columns
  } #close if(i >1)
  
  cat("Dataset", i, "added \n") #print out a message at the end of the iteration so you feel like something is occurring
  
} #close for loop
	
	return(template)
	
} #end read_sensor_data function

#####################################################


#Function 2


####################################################
#define function to assign day or night
assignDayNight <- function(datevec, riseplusvec, riseminvec, setminvec, setplusvec, count_vector) {
  cat(count_vector)
  if(datevec > riseplusvec && datevec < setminvec) {
    cat("Day", "\n")
    return("Day")
  } else {
    if(datevec > setplusvec || datevec < riseminvec) {
      cat("Night", "\n")
      return("Night")
    } else {
      cat("Neither night nor day", "\n")
      return("Twilight_sunrise")
    }} #end if else
} #end assignDayNight function

###################################################


#Function 3


###################################################
#function to assign a date to a season, used below
assign_season <- function(x) {
  return(c(rep("Summer", 2), rep("Autumn", 3), rep("Winter", 3), rep("Spring", 3), "Summer")[which(c(0:11) %in% x)])
}

####################################################


#Function 4


####################################################

format_sensor_data <- function(template, rise_set, time_zone="ACDT", verbose=TRUE) {
	
  season <- NULL
  
#this function formats the merged data (especially date and time) for analysis (e.g. season) and adds in the sun times to assign night day etc
	
#Create new columns "Day" & "TimeNew"
date_time <- stringr::str_split_fixed(template$Time," ", 2) #creates new matrix object with 2 cols derived from the existing "Time" col
colnames(date_time) <- c("Day", "TimeNew") #rename the cols
template <- cbind(template, date_time) #bind the new cols to the template object

#######
#add sunrise and sunset data to the template object

#convert all Day values in both data frames (sensor/sun times) to the same format
rise_set$Day <- as.Date(rise_set$Day,format = "%d/%m/%Y")
template$Day <- as.Date(template$Day, format = "%d/%m/%Y")

#merge rise_set data frame with 'template' based on Day:
template <- dplyr::inner_join(template, rise_set, by="Day")

#change the time formats
#first need to merge the date and times in the template document into a single field
#create new field (date_time) merging Day and TimeNew
template$date_time <- paste(template$Day, template$TimeNew, sep=" ")
template$date_time <- strptime(template$date_time, format = "%Y-%m-%d %H:%M:%OS")
#ALTERNATIVELY: use the existing "Time" col which is date and time merged?

#make all the rise and set fields (times) include the date
#and convert them to POSIXt class
template$rise_plus1time <- paste(template$Day, template$rise_plus1time, sep = " ")
template$rise_plus1time <- sub("NA", "00", template$rise_plus1time)
template$rise_plus1time <- strptime(template$rise_plus1time, format = "%Y-%m-%d %H:%M")

template$rise_minus1time <- paste(template$Day, template$rise_minus1time, sep = " ")
template$rise_minus1time <- sub("NA", "00", template$rise_minus1time)
template$rise_minus1time <- strptime(template$rise_minus1time, format = "%Y-%m-%d %H:%M")

template$set_plus1time <- paste(template$Day, template$set_plus1time, sep = " ")
template$set_plus1time <- sub("NA", "00", template$set_plus1time)
template$set_plus1time <- strptime(template$set_plus1time, format = "%Y-%m-%d %H:%M")

template$set_minus1time <- paste(template$Day, template$set_minus1time, sep = " ")
template$set_minus1time <- sub("NA", "00", template$set_minus1time)
template$set_minus1time <- strptime(template$set_minus1time, format = "%Y-%m-%d %H:%M")

#rename cols to make shorter
colnames(template)[colnames(template) == "rise_minus1time"] <- "rise_minus1"
colnames(template)[colnames(template) == "rise_plus1time"] <- "rise_plus1"
colnames(template)[colnames(template) == "set_minus1time"] <- "set_minus1"
colnames(template)[colnames(template) == "set_plus1time"] <- "set_plus1"

#give each observation a unique code
template$ID <- seq_along(template[,1])


######
#Assign day or night to each observation

#template['day_night'] <- NA #create new EMPTY/na column for day/night to go into

#Note, could add this to earlier when these were first defined as times (R class), but here adding 'tz' to each to standardise time zone format so > < operations work consistently
template$date_time <- strptime(template$date_time, format = "%Y-%m-%d %H:%M:%OS", tz=time_zone)
template$rise_plus1 <- strptime(template$rise_plus1, format = "%Y-%m-%d %H:%M:%OS", tz=time_zone)
template$rise_minus1 <- strptime(template$rise_minus1, format = "%Y-%m-%d %H:%M:%OS", tz=time_zone)
template$set_minus1 <- strptime(template$set_minus1, format = "%Y-%m-%d %H:%M:%OS", tz=time_zone)
template$set_plus1 <- strptime(template$set_plus1, format = "%Y-%m-%d %H:%M:%OS", tz=time_zone)


#apply function to all observations
template$day_night <- suppressWarnings(mapply(assignDayNight, template$date_time, template$rise_plus1, template$rise_minus1, template$set_minus1, template$set_plus1, 1:length(template$date_time)))

###
#add unique ID for days and nights NOTING that these ARE NOT defined by dates only as the defined night periods are over two dates.
template$day_night_unique <- rep(NA, nrow(template)) #create unique ID for days and nights (and twilight/dawn) noting that each 'unit' is numbered consecutively regardless of type: Day1, Twilight_sunrise2, Night3, and so on - the key thing is that each has a unique ID for when you filter to days/nights and calculate things by each day/night
##NB. the final row throws an error at present just because there is no "row + 1"...
print(head(template))
n <- 0
day_no <- 1
for(i in 1:nrow(template)) {
  n <- n + 1
  template$day_night_unique[n] <- paste0(template[n, "day_night"], day_no)
  if(verbose) {cat("Row", n, " completed", "\n")}
  if(n != nrow(template)) {
  	if(template$day_night[n] != template$day_night[n+1]) {
    day_no <- day_no + 1
  } #end if not the last row
  } #end if change in day_night
} #end for i in 1:nrow

#can now filter on day/night
#e.g., 
# xx <- subset(template, day_night=="Day")
# for(i in unique(xx$day_night_unique)) {...some calculation}

########################
#Create a "season" column and assign each observation as either "Summer", "Winter" etc.
#use the $mon slot of the date_time field to assign the correct season

template$season <- unlist(lapply(template$date_time$mon, assign_season))

####
#Correct different seasons applied to same unique night at season boundaries (e.g. night of 30.11 - 1/12)
season_unique <- template[-which(duplicated(template$day_night_unique)),c("day_night_unique", "season")] #unique season entry per unique day/night

template <- dplyr::inner_join(subset(template, select=-c(season)), season_unique, by="day_night_unique") #merge back the unique season data to main table 

#####################
#ADD TRANSECT INFO
#add transect distance and type of sensor for raw data too: 'template':
template$transect <- unlist(lapply(strsplit(template$Sensor,".", fixed=T), function(x) paste0(x[1], x[2])))
#
#
template$distance <- gsub('\\D','', lapply(strsplit(template$Sensor,".", fixed=T), function(x) paste0(x[3])))
#
template$type <- gsub('\\d','', lapply(strsplit(template$Sensor,".", fixed=T), function(x) paste0(x[3])))
#
template$type[template$type == ""] <- "A"

##########
#Calculate absolute humidity from RH and temp and add as a column to the data:

#load absolute humidity calculator function from threadr package on git:
source('https://raw.githubusercontent.com/skgrange/threadr/master/R/absolute_humidity.R')

template$Humidity..ah. <- absolute_humidity(template$Celsius.C., template$Humidity..rh.)
#####


##########
return(template)

} #end format_sensor_data function

#################################################


#Function 5


################################################

calculate_means <- function(template) {

day_night <- NULL
  
#temp calcs per sensor, season, day/night and unique day period (i.e. daily diurnal max...)
#DAILY VALUES by day/night
maxByday <- aggregate(template$Celsius.C., by=list(template$Sensor, template$season, template$day_night, template$day_night_unique), FUN=max)
maxByday <- maxByday[do.call(order, maxByday), ]
names(maxByday) <- c("Sensor", "season", "day_night", "day_night_unique", "maxT")
######merge day_night_unique to get the actual day/date, whereas season and year don't need this step
maxByday$Day <- template$Day[match(maxByday$day_night_unique, template$day_night_unique)]


#mean max by sensor, season and day/night (i.e., mean of daily maxima in these categories)
#SEASONAL VALUES by day/night
maxByday2 <- aggregate(maxByday$maxT, by=list(maxByday$Sensor, maxByday$season, maxByday$day_night), FUN=mean)
maxByday2 <- maxByday2[do.call(order, maxByday2), ]
names(maxByday2) <- c("Sensor", "season", "day_night", "maxT")

#mean max by sensor and day/night(i.e., overall 'ANNUAL' mean but not strictly so since the data may be more or less than a year...)
maxByday3 <- aggregate(maxByday$maxT, by=list(maxByday$Sensor, maxByday$day_night), FUN=mean)
maxByday3 <- maxByday3[do.call(order, maxByday3), ]
names(maxByday3) <- c("Sensor", "day_night", "maxT")

#mean max by sensor, day/night and month (MONTHLY MEANs)
maxByday4 <- aggregate(maxByday$maxT, by=list(maxByday$Sensor, strptime(maxByday$Day, format = "%Y-%m-%d")$mon, maxByday$day_night), FUN=mean)
maxByday4 <- maxByday4[do.call(order, maxByday4), ]
names(maxByday4) <- c("Sensor", "month", "day_night", "maxT")

#####
###add sensor details and use maxT as the template for adding other variables
#add transect
maxByday2$transect <- unlist(lapply(strsplit(maxByday2$Sensor,".", fixed=T), function(x) paste0(x[1], x[2])))
#add distance from edge
maxByday2$distance <- unlist(gsub('\\D','', lapply(strsplit(maxByday2$Sensor,".", fixed=T), function(x) paste0(x[3]))))
#add type A/S/X air soil control
maxByday2$type <- gsub('\\d','', lapply(strsplit(maxByday2$Sensor,".", fixed=T), function(x) paste0(x[3])))
#
maxByday2$type[maxByday2$type == ""] <- "A"
######
###add sensor details and use maxT as the template for adding other variables
#add transect
maxByday3$transect <- unlist(lapply(strsplit(maxByday3$Sensor,".", fixed=T), function(x) paste0(x[1], x[2])))
#add distance from edge
maxByday3$distance <- unlist(gsub('\\D','', lapply(strsplit(maxByday3$Sensor,".", fixed=T), function(x) paste0(x[3]))))
#add type A/S/X air soil control
maxByday3$type <- gsub('\\d','', lapply(strsplit(maxByday3$Sensor,".", fixed=T), function(x) paste0(x[3])))
#
maxByday3$type[maxByday3$type == ""] <- "A"
##########add sensor details and use maxT as the template for adding other variables
#add transect
maxByday$transect <- unlist(lapply(strsplit(maxByday$Sensor,".", fixed=T), function(x) paste0(x[1], x[2])))
#add distance from edge
maxByday$distance <- unlist(gsub('\\D','', lapply(strsplit(maxByday$Sensor,".", fixed=T), function(x) paste0(x[3]))))
#add type A/S/X air soil control
maxByday$type <- gsub('\\d','', lapply(strsplit(maxByday$Sensor,".", fixed=T), function(x) paste0(x[3])))
#
maxByday$type[maxByday$type == ""] <- "A"
#
###add sensor details and use maxT as the template for adding other variables
#add transect
maxByday4$transect <- unlist(lapply(strsplit(maxByday4$Sensor,".", fixed=T), function(x) paste0(x[1], x[2])))
#add distance from edge
maxByday4$distance <- unlist(gsub('\\D','', lapply(strsplit(maxByday4$Sensor,".", fixed=T), function(x) paste0(x[3]))))
#add type A/S/X air soil control
maxByday4$type <- gsub('\\d','', lapply(strsplit(maxByday4$Sensor,".", fixed=T), function(x) paste0(x[3])))
#
maxByday4$type[maxByday4$type == ""] <- "A"

######################more responses:
#############
#minT
minBy <- aggregate(template$Celsius.C., by=list(template$Sensor, template$season, template$day_night, template$day_night_unique), FUN=min)
minBy <- minBy[do.call(order, minBy), ] #re-order more sensibly
names(minBy) <- c("Sensor", "season", "day_night", "day_night_unique", "minT") #daily minima
minBy$Day <- template$Day[match(minBy$day_night_unique, template$day_night_unique)]

#mean minima by category
minBy2 <- aggregate(minBy$minT, by=list(minBy$Sensor, minBy$season, minBy$day_night), FUN=mean)
minBy2 <- minBy2[do.call(order, minBy2), ]
names(minBy2) <- c("Sensor", "season", "day_night", "minT")

#mean min by sensor and day/night(i.e., overall 'annual' mean but not strictly so since the data may be more or less than a year...)
minBy3 <- aggregate(minBy$minT, by=list(minBy$Sensor, minBy$day_night), FUN=mean)
minBy3 <- minBy3[do.call(order, minBy3), ]
names(minBy3) <- c("Sensor", "day_night", "minT")
#
minBy4 <- aggregate(minBy$minT, by=list(minBy$Sensor, strptime(minBy$Day, format = "%Y-%m-%d")$mon, minBy$day_night), FUN=mean)
minBy4 <- minBy4[do.call(order, minBy4), ]
names(minBy4) <- c("Sensor", "month", "day_night", "minT")

#add to first dfs for convenience
maxByday$minT <- minBy$minT
maxByday2$minT <- minBy2$minT
maxByday3$minT <- minBy3$minT
maxByday4$minT <- minBy4$minT

#meanT
meanBy <- aggregate(template$Celsius.C., by=list(template$Sensor, template$season, template$day_night, template$day_night_unique), FUN=mean)
meanBy <- meanBy[do.call(order, meanBy), ]
names(meanBy) <- c("Sensor", "season", "day_night", "day_night_unique", "meanT")
meanBy$Day <- template$Day[match(meanBy$day_night_unique, template$day_night_unique)]

#
meanBy2 <- aggregate(meanBy$meanT, by=list(meanBy$Sensor, meanBy$season, meanBy$day_night), FUN=mean)
meanBy2 <- meanBy2[do.call(order, meanBy2), ]
names(meanBy2) <- c("Sensor", "season", "day_night", "meanT")
#
meanBy3 <- aggregate(meanBy$meanT, by=list(meanBy$Sensor, meanBy$day_night), FUN=mean)
meanBy3 <- meanBy3[do.call(order, meanBy3), ]
names(meanBy3) <- c("Sensor", "day_night", "meanT")
#
meanBy4 <- aggregate(meanBy$meanT, by=list(meanBy$Sensor, strptime(meanBy$Day, format = "%Y-%m-%d")$mon, meanBy$day_night), FUN=mean)
meanBy4 <- meanBy4[do.call(order, meanBy4), ]
names(meanBy4) <- c("Sensor", "month", "day_night", "meanT")

#
maxByday$meanT <- meanBy$meanT
maxByday2$meanT <- meanBy2$meanT
maxByday3$meanT <- meanBy3$meanT
maxByday4$meanT <- meanBy4$meanT


#sdT
sdBy <- aggregate(template$Celsius.C., by=list(template$Sensor, template$season, template$day_night, template$day_night_unique), FUN=sd)
sdBy <- sdBy[do.call(order, sdBy), ]
names(sdBy) <- c("Sensor", "season", "day_night", "day_night_unique", "sdT")
sdBy$Day <- template$Day[match(sdBy$day_night_unique, template$day_night_unique)]
#
sdBy2 <- aggregate(sdBy$sdT, by=list(sdBy$Sensor, sdBy$season, sdBy$day_night), FUN=function(x) {return(mean(na.omit(x)))})
sdBy2 <- sdBy2[do.call(order, sdBy2), ]
names(sdBy2) <- c("Sensor", "season", "day_night", "sdT")
#
sdBy3 <- aggregate(sdBy$sdT, by=list(sdBy$Sensor, sdBy$day_night), FUN=function(x) {return(mean(na.omit(x)))})
sdBy3 <- sdBy3[do.call(order, sdBy3), ]
names(sdBy3) <- c("Sensor", "day_night", "sdT")
#
sdBy4 <- aggregate(sdBy$sdT, by=list(sdBy$Sensor, strptime(sdBy$Day, format = "%Y-%m-%d")$mon, sdBy$day_night), FUN=function(x) {return(mean(na.omit(x)))})
sdBy4 <- sdBy4[do.call(order, sdBy4), ]
names(sdBy4) <- c("Sensor", "month", "day_night", "sdT")
#
maxByday$sdT <- sdBy$sdT
maxByday2$sdT <- sdBy2$sdT
maxByday3$sdT <- sdBy3$sdT
maxByday4$sdT <- sdBy4$sdT

#
# #cvT - coefficient of variation
# cvBy <- aggregate(template$Celsius.C., by=list(template$Sensor, template$season, template$day_night, template$day_night_unique), FUN=function(x) {return(sd(x)/mean(x))})
# cvBy <- cvBy[do.call(order, cvBy), ]
# names(cvBy) <- c("Sensor", "season", "day_night", "day_night_unique", "cvT")
# cvBy$Day <- template$Day[match(cvBy$day_night_unique, template$day_night_unique)]
# #
# cvBy2 <- aggregate(cvBy$cvT, by=list(cvBy$Sensor, cvBy$season, cvBy$day_night), FUN=function(x) {return(mean(na.omit(x)))})
# cvBy2 <- cvBy2[do.call(order, cvBy2), ]
# names(cvBy2) <- c("Sensor", "season", "day_night", "cvT")
# #
# cvBy3 <- aggregate(cvBy$cvT, by=list(cvBy$Sensor, cvBy$day_night), FUN=function(x) {return(mean(na.omit(x)))})
# cvBy3 <- cvBy3[do.call(order, cvBy3), ]
# names(cvBy3) <- c("Sensor", "day_night", "cvT")
# #
# cvBy4 <- aggregate(cvBy$cvT, by=list(cvBy$Sensor, strptime(cvBy$Day, format = "%Y-%m-%d")$mon, cvBy$day_night), FUN=function(x) {return(mean(na.omit(x)))})
# cvBy4 <- cvBy4[do.call(order, cvBy4), ]
# names(cvBy4) <- c("Sensor", "month", "day_night", "cvT")
# #
# maxByday$cvT <- cvBy$cvT
# maxByday2$cvT <- cvBy2$cvT
# maxByday3$cvT <- cvBy3$cvT
# maxByday4$cvT <- cvBy4$cvT


#RANGET (Note range and sd are quite correlated and both represent variation, CV is less correlated)
rangeBy <- aggregate(template$Celsius.C., by=list(template$Sensor, template$season, template$day_night, template$day_night_unique), FUN=function(x) abs(diff(range(x))))
rangeBy <- rangeBy[do.call(order, rangeBy), ]
names(rangeBy) <- c("Sensor", "season", "day_night", "day_night_unique", "rangeT")
rangeBy$Day <- template$Day[match(rangeBy$day_night_unique, template$day_night_unique)]

#
rangeBy2 <- aggregate(rangeBy$rangeT, by=list(rangeBy$Sensor, rangeBy$season, rangeBy$day_night), FUN=mean)
rangeBy2 <- rangeBy2[do.call(order, rangeBy2), ]
names(rangeBy2) <- c("Sensor", "season", "day_night", "rangeT")
#
rangeBy3 <- aggregate(rangeBy$rangeT, by=list(rangeBy$Sensor, rangeBy$day_night), FUN=mean)
rangeBy3 <- rangeBy3[do.call(order, rangeBy3), ]
names(rangeBy3) <- c("Sensor", "day_night", "rangeT")
#
rangeBy4 <- aggregate(rangeBy$rangeT, by=list(rangeBy$Sensor, strptime(rangeBy$Day, format = "%Y-%m-%d")$mon, rangeBy$day_night), FUN=mean)
rangeBy4 <- rangeBy4[do.call(order, rangeBy4), ]
names(rangeBy4) <- c("Sensor", "month", "day_night", "rangeT")
#
maxByday$rangeT <- rangeBy$rangeT
maxByday2$rangeT <- rangeBy2$rangeT
maxByday3$rangeT <- rangeBy3$rangeT
maxByday4$rangeT <- rangeBy4$rangeT

########################
#tidy up tables

###
#daily
maxByday <- subset(maxByday, day_night != "Twilight_sunrise") 
maxByday <- maxByday[with(maxByday, order(Sensor, Day, day_night)),]
maxByday <- maxByday[,c("Sensor", "Day", "season", "day_night", "day_night_unique", "transect", "distance", "type", "maxT",  "minT",  "meanT", "sdT", "rangeT")]
#
#month
maxByday4 <- subset(maxByday4, day_night != "Twilight_sunrise") 
maxByday4 <- maxByday4[with(maxByday4, order(Sensor, month, day_night)),]
maxByday4 <- maxByday4[,c("Sensor", "month", "day_night", "transect", "distance", "type", "maxT",  "minT",  "meanT", "sdT", "rangeT")]
maxByday4$month <- month.name[maxByday4$month+1]
#
#season
maxByday2 <- subset(maxByday2, day_night != "Twilight_sunrise") 
maxByday2 <- maxByday2[with(maxByday2, order(Sensor, season, day_night)),]
maxByday2 <- maxByday2[,c("Sensor", "season", "day_night", "transect", "distance", "type", "maxT",  "minT",  "meanT", "sdT", "rangeT")]
#
#year
maxByday3 <- subset(maxByday3, day_night != "Twilight_sunrise") 
maxByday3 <- maxByday3[with(maxByday3, order(Sensor, day_night)),]
maxByday3 <- maxByday3[,c("Sensor", "day_night", "transect", "distance", "type", "maxT",  "minT",  "meanT", "sdT", "rangeT")]
######################
#return result
result <- list()
result$Season <- maxByday2
result$Day <- maxByday
result$Year <- maxByday3
result$Month <- maxByday4

return(result)

} #end calculate means function

##################################################

#Function 6

##################################################

calculate_humidity <- function(template) {

day_night <- NULL
  
#HUMIDITY calcs per sensor, season, day/night and unique day period (i.e. daily diurnal max...)
#DAILY VALUES by day/night
maxByday <- aggregate(template$Humidity..ah., by=list(template$Sensor, template$season, template$day_night, template$day_night_unique), FUN=max)
maxByday <- maxByday[do.call(order, maxByday), ]
names(maxByday) <- c("Sensor", "season", "day_night", "day_night_unique", "maxAH")
######merge day_night_unique to get the actual day/date, whereas season and year don't need this step
maxByday$Day <- template$Day[match(maxByday$day_night_unique, template$day_night_unique)]


#mean max by sensor, season and day/night (i.e., mean of daily maxima in these categories)
#SEASONAL VALUES by day/night
maxByday2 <- aggregate(maxByday$maxAH, by=list(maxByday$Sensor, maxByday$season, maxByday$day_night), FUN=mean)
maxByday2 <- maxByday2[do.call(order, maxByday2), ]
names(maxByday2) <- c("Sensor", "season", "day_night", "maxAH")

#mean max by sensor and day/night(i.e., overall 'ANNUAL' mean but not strictly so since the data may be more or less than a year...)
maxByday3 <- aggregate(maxByday$maxAH, by=list(maxByday$Sensor, maxByday$day_night), FUN=mean)
maxByday3 <- maxByday3[do.call(order, maxByday3), ]
names(maxByday3) <- c("Sensor", "day_night", "maxAH")

#mean max by sensor, day/night and month (MONTHLY MEANs)
maxByday4 <- aggregate(maxByday$maxAH, by=list(maxByday$Sensor, strptime(maxByday$Day, format = "%Y-%m-%d")$mon, maxByday$day_night), FUN=mean)
maxByday4 <- maxByday4[do.call(order, maxByday4), ]
names(maxByday4) <- c("Sensor", "month", "day_night", "maxAH")

#####
###add sensor details and use maxAH as the template for adding other variables
#add transect
maxByday2$transect <- unlist(lapply(strsplit(maxByday2$Sensor,".", fixed=T), function(x) paste0(x[1], x[2])))
#add distance from edge
maxByday2$distance <- unlist(gsub('\\D','', lapply(strsplit(maxByday2$Sensor,".", fixed=T), function(x) paste0(x[3]))))
#add type A/S/X air soil control
maxByday2$type <- gsub('\\d','', lapply(strsplit(maxByday2$Sensor,".", fixed=T), function(x) paste0(x[3])))
#
maxByday2$type[maxByday2$type == ""] <- "A"
######
###add sensor details and use maxT as the template for adding other variables
#add transect
maxByday3$transect <- unlist(lapply(strsplit(maxByday3$Sensor,".", fixed=T), function(x) paste0(x[1], x[2])))
#add distance from edge
maxByday3$distance <- unlist(gsub('\\D','', lapply(strsplit(maxByday3$Sensor,".", fixed=T), function(x) paste0(x[3]))))
#add type A/S/X air soil control
maxByday3$type <- gsub('\\d','', lapply(strsplit(maxByday3$Sensor,".", fixed=T), function(x) paste0(x[3])))
#
maxByday3$type[maxByday3$type == ""] <- "A"
##########add sensor details and use maxT as the template for adding other variables
#add transect
maxByday$transect <- unlist(lapply(strsplit(maxByday$Sensor,".", fixed=T), function(x) paste0(x[1], x[2])))
#add distance from edge
maxByday$distance <- unlist(gsub('\\D','', lapply(strsplit(maxByday$Sensor,".", fixed=T), function(x) paste0(x[3]))))
#add type A/S/X air soil control
maxByday$type <- gsub('\\d','', lapply(strsplit(maxByday$Sensor,".", fixed=T), function(x) paste0(x[3])))
#
maxByday$type[maxByday$type == ""] <- "A"
#
###add sensor details and use maxAH as the template for adding other variables
#add transect
maxByday4$transect <- unlist(lapply(strsplit(maxByday4$Sensor,".", fixed=T), function(x) paste0(x[1], x[2])))
#add distance from edge
maxByday4$distance <- unlist(gsub('\\D','', lapply(strsplit(maxByday4$Sensor,".", fixed=T), function(x) paste0(x[3]))))
#add type A/S/X air soil control
maxByday4$type <- gsub('\\d','', lapply(strsplit(maxByday4$Sensor,".", fixed=T), function(x) paste0(x[3])))
#
maxByday4$type[maxByday4$type == ""] <- "A"

######################more responses:
#############
#minAH
minBy <- aggregate(template$Humidity..ah., by=list(template$Sensor, template$season, template$day_night, template$day_night_unique), FUN=min)
minBy <- minBy[do.call(order, minBy), ] #re-order more sensibly
names(minBy) <- c("Sensor", "season", "day_night", "day_night_unique", "minAH") #daily minima
minBy$Day <- template$Day[match(minBy$day_night_unique, template$day_night_unique)]

#mean minima by category
minBy2 <- aggregate(minBy$minAH, by=list(minBy$Sensor, minBy$season, minBy$day_night), FUN=mean)
minBy2 <- minBy2[do.call(order, minBy2), ]
names(minBy2) <- c("Sensor", "season", "day_night", "minAH")

#mean min by sensor and day/night(i.e., overall 'annual' mean but not strictly so since the data may be more or less than a year...)
minBy3 <- aggregate(minBy$minAH, by=list(minBy$Sensor, minBy$day_night), FUN=mean)
minBy3 <- minBy3[do.call(order, minBy3), ]
names(minBy3) <- c("Sensor", "day_night", "minAH")
#
minBy4 <- aggregate(minBy$minAH, by=list(minBy$Sensor, strptime(minBy$Day, format = "%Y-%m-%d")$mon, minBy$day_night), FUN=mean)
minBy4 <- minBy4[do.call(order, minBy4), ]
names(minBy4) <- c("Sensor", "month", "day_night", "minAH")

#add to first dfs for convenience
maxByday$minAH <- minBy$minAH
maxByday2$minAH <- minBy2$minAH
maxByday3$minAH <- minBy3$minAH
maxByday4$minAH <- minBy4$minAH

#meanT
meanBy <- aggregate(template$Humidity..ah., by=list(template$Sensor, template$season, template$day_night, template$day_night_unique), FUN=mean)
meanBy <- meanBy[do.call(order, meanBy), ]
names(meanBy) <- c("Sensor", "season", "day_night", "day_night_unique", "meanAH")
meanBy$Day <- template$Day[match(meanBy$day_night_unique, template$day_night_unique)]

#
meanBy2 <- aggregate(meanBy$meanAH, by=list(meanBy$Sensor, meanBy$season, meanBy$day_night), FUN=mean)
meanBy2 <- meanBy2[do.call(order, meanBy2), ]
names(meanBy2) <- c("Sensor", "season", "day_night", "meanAH")
#
meanBy3 <- aggregate(meanBy$meanAH, by=list(meanBy$Sensor, meanBy$day_night), FUN=mean)
meanBy3 <- meanBy3[do.call(order, meanBy3), ]
names(meanBy3) <- c("Sensor", "day_night", "meanAH")
#
meanBy4 <- aggregate(meanBy$meanAH, by=list(meanBy$Sensor, strptime(meanBy$Day, format = "%Y-%m-%d")$mon, meanBy$day_night), FUN=mean)
meanBy4 <- meanBy4[do.call(order, meanBy4), ]
names(meanBy4) <- c("Sensor", "month", "day_night", "meanAH")

#
maxByday$meanAH <- meanBy$meanAH
maxByday2$meanAH <- meanBy2$meanAH
maxByday3$meanAH <- meanBy3$meanAH
maxByday4$meanAH <- meanBy4$meanAH


#sdT
sdBy <- aggregate(template$Humidity..ah., by=list(template$Sensor, template$season, template$day_night, template$day_night_unique), FUN=sd)
sdBy <- sdBy[do.call(order, sdBy), ]
names(sdBy) <- c("Sensor", "season", "day_night", "day_night_unique", "sdAH")
sdBy$Day <- template$Day[match(sdBy$day_night_unique, template$day_night_unique)]
#
sdBy2 <- aggregate(sdBy$sdAH, by=list(sdBy$Sensor, sdBy$season, sdBy$day_night), FUN=function(x) {return(mean(na.omit(x)))})
sdBy2 <- sdBy2[do.call(order, sdBy2), ]
names(sdBy2) <- c("Sensor", "season", "day_night", "sdAH")
#
sdBy3 <- aggregate(sdBy$sdAH, by=list(sdBy$Sensor, sdBy$day_night), FUN=function(x) {return(mean(na.omit(x)))})
sdBy3 <- sdBy3[do.call(order, sdBy3), ]
names(sdBy3) <- c("Sensor", "day_night", "sdAH")
#
sdBy4 <- aggregate(sdBy$sdAH, by=list(sdBy$Sensor, strptime(sdBy$Day, format = "%Y-%m-%d")$mon, sdBy$day_night), FUN=function(x) {return(mean(na.omit(x)))})
sdBy4 <- sdBy4[do.call(order, sdBy4), ]
names(sdBy4) <- c("Sensor", "month", "day_night", "sdAH")
#
maxByday$sdAH <- sdBy$sdAH
maxByday2$sdAH <- sdBy2$sdAH
maxByday3$sdAH <- sdBy3$sdAH
maxByday4$sdAH <- sdBy4$sdAH


#min/max RANGE of Humidity (Note range and sd are quite correlated and both represent variation, CV is less correlated)
rangeBy <- aggregate(template$Humidity..ah., by=list(template$Sensor, template$season, template$day_night, template$day_night_unique), FUN=function(x) abs(diff(range(x))))
rangeBy <- rangeBy[do.call(order, rangeBy), ]
names(rangeBy) <- c("Sensor", "season", "day_night", "day_night_unique", "rangeAH")
rangeBy$Day <- template$Day[match(rangeBy$day_night_unique, template$day_night_unique)]

#
rangeBy2 <- aggregate(rangeBy$rangeAH, by=list(rangeBy$Sensor, rangeBy$season, rangeBy$day_night), FUN=mean)
rangeBy2 <- rangeBy2[do.call(order, rangeBy2), ]
names(rangeBy2) <- c("Sensor", "season", "day_night", "rangeAH")
#
rangeBy3 <- aggregate(rangeBy$rangeAH, by=list(rangeBy$Sensor, rangeBy$day_night), FUN=mean)
rangeBy3 <- rangeBy3[do.call(order, rangeBy3), ]
names(rangeBy3) <- c("Sensor", "day_night", "rangeAH")
#
rangeBy4 <- aggregate(rangeBy$rangeAH, by=list(rangeBy$Sensor, strptime(rangeBy$Day, format = "%Y-%m-%d")$mon, rangeBy$day_night), FUN=mean)
rangeBy4 <- rangeBy4[do.call(order, rangeBy4), ]
names(rangeBy4) <- c("Sensor", "month", "day_night", "rangeAH")
#
maxByday$rangeAH <- rangeBy$rangeAH
maxByday2$rangeAH <- rangeBy2$rangeAH
maxByday3$rangeAH <- rangeBy3$rangeAH
maxByday4$rangeAH <- rangeBy4$rangeAH

#####################Add more responses for RELATIVE humidity and add to the master tables:
#############
#minRH
minBy <- aggregate(template$Humidity..rh., by=list(template$Sensor, template$season, template$day_night, template$day_night_unique), FUN=min)
minBy <- minBy[do.call(order, minBy), ] #re-order more sensibly
names(minBy) <- c("Sensor", "season", "day_night", "day_night_unique", "minRH") #daily minima
minBy$Day <- template$Day[match(minBy$day_night_unique, template$day_night_unique)]

#mean minima by category
minBy2 <- aggregate(minBy$minRH, by=list(minBy$Sensor, minBy$season, minBy$day_night), FUN=mean)
minBy2 <- minBy2[do.call(order, minBy2), ]
names(minBy2) <- c("Sensor", "season", "day_night", "minRH")

#mean min by sensor and day/night(i.e., overall 'annual' mean but not strictly so since the data may be more or less than a year...)
minBy3 <- aggregate(minBy$minRH, by=list(minBy$Sensor, minBy$day_night), FUN=mean)
minBy3 <- minBy3[do.call(order, minBy3), ]
names(minBy3) <- c("Sensor", "day_night", "minRH")
#
minBy4 <- aggregate(minBy$minRH, by=list(minBy$Sensor, strptime(minBy$Day, format = "%Y-%m-%d")$mon, minBy$day_night), FUN=mean)
minBy4 <- minBy4[do.call(order, minBy4), ]
names(minBy4) <- c("Sensor", "month", "day_night", "minRH")

#add to first dfs for convenience
maxByday$minRH <- minBy$minRH
maxByday2$minRH <- minBy2$minRH
maxByday3$minRH <- minBy3$minRH
maxByday4$minRH <- minBy4$minRH
########################
#############
#meanRH
meanBy <- aggregate(template$Humidity..rh., by=list(template$Sensor, template$season, template$day_night, template$day_night_unique), FUN=mean)
meanBy <- meanBy[do.call(order, meanBy), ] #re-order more sensibly
names(meanBy) <- c("Sensor", "season", "day_night", "day_night_unique", "meanRH") #daily minima
meanBy$Day <- template$Day[match(meanBy$day_night_unique, template$day_night_unique)]

#mean by category
meanBy2 <- aggregate(meanBy$meanRH, by=list(meanBy$Sensor, meanBy$season, meanBy$day_night), FUN=mean)
meanBy2 <- meanBy2[do.call(order, meanBy2), ]
names(meanBy2) <- c("Sensor", "season", "day_night", "meanRH")

#mean by sensor and day/night(i.e., overall 'annual' mean but not strictly so since the data may be more or less than a year...)
meanBy3 <- aggregate(meanBy$meanRH, by=list(meanBy$Sensor, meanBy$day_night), FUN=mean)
meanBy3 <- meanBy3[do.call(order, meanBy3), ]
names(meanBy3) <- c("Sensor", "day_night", "meanRH")
#
meanBy4 <- aggregate(meanBy$meanRH, by=list(meanBy$Sensor, strptime(meanBy$Day, format = "%Y-%m-%d")$mon, meanBy$day_night), FUN=mean)
meanBy4 <- meanBy4[do.call(order, meanBy4), ]
names(meanBy4) <- c("Sensor", "month", "day_night", "meanRH")

#add to first dfs for convenience
maxByday$meanRH <- meanBy$meanRH
maxByday2$meanRH <- meanBy2$meanRH
maxByday3$meanRH <- meanBy3$meanRH
maxByday4$meanRH <- meanBy4$meanRH
########################
#############
#maxRH
maxBy <- aggregate(template$Humidity..rh., by=list(template$Sensor, template$season, template$day_night, template$day_night_unique), FUN=max)
maxBy <- maxBy[do.call(order, maxBy), ] #re-order more sensibly
names(maxBy) <- c("Sensor", "season", "day_night", "day_night_unique", "maxRH") #daily minima
maxBy$Day <- template$Day[match(maxBy$day_night_unique, template$day_night_unique)]

#max by category
maxBy2 <- aggregate(maxBy$maxRH, by=list(maxBy$Sensor, maxBy$season, maxBy$day_night), FUN=mean)
maxBy2 <- maxBy2[do.call(order, maxBy2), ]
names(maxBy2) <- c("Sensor", "season", "day_night", "maxRH")

#max by sensor and day/night(i.e., overall 'annual' mean but not strictly so since the data may be more or less than a year...)
maxBy3 <- aggregate(maxBy$maxRH, by=list(maxBy$Sensor, maxBy$day_night), FUN=mean)
maxBy3 <- maxBy3[do.call(order, maxBy3), ]
names(maxBy3) <- c("Sensor", "day_night", "maxRH")
#
maxBy4 <- aggregate(maxBy$maxRH, by=list(maxBy$Sensor, strptime(maxBy$Day, format = "%Y-%m-%d")$mon, maxBy$day_night), FUN=mean)
maxBy4 <- maxBy4[do.call(order, maxBy4), ]
names(maxBy4) <- c("Sensor", "month", "day_night", "maxRH")

#add to first dfs for convenience
maxByday$maxRH <- maxBy$maxRH
maxByday2$maxRH <- maxBy2$maxRH
maxByday3$maxRH <- maxBy3$maxRH
maxByday4$maxRH <- maxBy4$maxRH
########################
#sdT
sdBy <- aggregate(template$Humidity..rh., by=list(template$Sensor, template$season, template$day_night, template$day_night_unique), FUN=sd)
sdBy <- sdBy[do.call(order, sdBy), ]
names(sdBy) <- c("Sensor", "season", "day_night", "day_night_unique", "sdRH")
sdBy$Day <- template$Day[match(sdBy$day_night_unique, template$day_night_unique)]
#
sdBy2 <- aggregate(sdBy$sdRH, by=list(sdBy$Sensor, sdBy$season, sdBy$day_night), FUN=function(x) {return(mean(na.omit(x)))})
sdBy2 <- sdBy2[do.call(order, sdBy2), ]
names(sdBy2) <- c("Sensor", "season", "day_night", "sdRH")
#
sdBy3 <- aggregate(sdBy$sdRH, by=list(sdBy$Sensor, sdBy$day_night), FUN=function(x) {return(mean(na.omit(x)))})
sdBy3 <- sdBy3[do.call(order, sdBy3), ]
names(sdBy3) <- c("Sensor", "day_night", "sdRH")
#
sdBy4 <- aggregate(sdBy$sdRH, by=list(sdBy$Sensor, strptime(sdBy$Day, format = "%Y-%m-%d")$mon, sdBy$day_night), FUN=function(x) {return(mean(na.omit(x)))})
sdBy4 <- sdBy4[do.call(order, sdBy4), ]
names(sdBy4) <- c("Sensor", "month", "day_night", "sdRH")
#
maxByday$sdRH <- sdBy$sdRH
maxByday2$sdRH <- sdBy2$sdRH
maxByday3$sdRH <- sdBy3$sdRH
maxByday4$sdRH <- sdBy4$sdRH


#min/max RANGE of RELATIVE Humidity (Note range and sd are quite correlated and both represent variation, CV is less correlated)
rangeBy <- aggregate(template$Humidity..rh., by=list(template$Sensor, template$season, template$day_night, template$day_night_unique), FUN=function(x) abs(diff(range(x))))
rangeBy <- rangeBy[do.call(order, rangeBy), ]
names(rangeBy) <- c("Sensor", "season", "day_night", "day_night_unique", "rangeRH")
rangeBy$Day <- template$Day[match(rangeBy$day_night_unique, template$day_night_unique)]

#
rangeBy2 <- aggregate(rangeBy$rangeRH, by=list(rangeBy$Sensor, rangeBy$season, rangeBy$day_night), FUN=mean)
rangeBy2 <- rangeBy2[do.call(order, rangeBy2), ]
names(rangeBy2) <- c("Sensor", "season", "day_night", "rangeRH")
#
rangeBy3 <- aggregate(rangeBy$rangeRH, by=list(rangeBy$Sensor, rangeBy$day_night), FUN=mean)
rangeBy3 <- rangeBy3[do.call(order, rangeBy3), ]
names(rangeBy3) <- c("Sensor", "day_night", "rangeRH")
#
rangeBy4 <- aggregate(rangeBy$rangeRH, by=list(rangeBy$Sensor, strptime(rangeBy$Day, format = "%Y-%m-%d")$mon, rangeBy$day_night), FUN=mean)
rangeBy4 <- rangeBy4[do.call(order, rangeBy4), ]
names(rangeBy4) <- c("Sensor", "month", "day_night", "rangeRH")
#
maxByday$rangeRH <- rangeBy$rangeRH
maxByday2$rangeRH <- rangeBy2$rangeRH
maxByday3$rangeRH <- rangeBy3$rangeRH
maxByday4$rangeRH <- rangeBy4$rangeRH


########################
#tidy up tables

###
#daily
maxByday <- subset(maxByday, day_night != "Twilight_sunrise") 
maxByday <- maxByday[with(maxByday, order(Sensor, Day, day_night)),]
maxByday <- maxByday[,c("Sensor", "Day", "season", "day_night", "day_night_unique", "transect", "distance", "type", "maxAH",  "minAH",  "meanAH", "sdAH", "rangeAH", "maxRH",  "minRH",  "meanRH", "sdRH", "rangeRH")]
#
#month
maxByday4 <- subset(maxByday4, day_night != "Twilight_sunrise") 
maxByday4 <- maxByday4[with(maxByday4, order(Sensor, month, day_night)),]
maxByday4 <- maxByday4[,c("Sensor", "month", "day_night", "transect", "distance", "type", "maxAH",  "minAH",  "meanAH", "sdAH", "rangeAH", "maxRH",  "minRH",  "meanRH", "sdRH", "rangeRH")]
maxByday4$month <- month.name[maxByday4$month+1]
#
#season
maxByday2 <- subset(maxByday2, day_night != "Twilight_sunrise") 
maxByday2 <- maxByday2[with(maxByday2, order(Sensor, season, day_night)),]
maxByday2 <- maxByday2[,c("Sensor", "season", "day_night", "transect", "distance", "type", "maxAH",  "minAH",  "meanAH", "sdAH", "rangeAH", "maxRH",  "minRH",  "meanRH", "sdRH", "rangeRH")]
#
#year
maxByday3 <- subset(maxByday3, day_night != "Twilight_sunrise") 
maxByday3 <- maxByday3[with(maxByday3, order(Sensor, day_night)),]
maxByday3 <- maxByday3[,c("Sensor", "day_night", "transect", "distance", "type", "maxAH",  "minAH",  "meanAH", "sdAH", "rangeAH", "maxRH",  "minRH",  "meanRH", "sdRH", "rangeRH")]
######################
#return result
result <- list()
result$Season <- maxByday2
result$Day <- maxByday
result$Year <- maxByday3
result$Month <- maxByday4

return(result)

} #end calculate humidity function


#################################################

#Function 7


##################################################

#add times for an hour before and after sun rise and set in order to calculate variables later
format_sun_times <- function(rise_set) {
  rise_set$rise_decimal <- rise_set$rise/100
  rise_set$set_decimal <- rise_set$set/100
  
  rise_set$rise_plus1time <- rise_set$rise_decimal + 1
  rise_set$set_plus1time <- rise_set$set_decimal + 1
  rise_set$rise_minus1time <- rise_set$rise_decimal - 1
  rise_set$set_minus1time <- rise_set$set_decimal - 1
  
#character format
  rise_set$rise_plus1time <- unlist(lapply(strsplit(as.character(rise_set$rise_plus1time), "\\."), function(x) {return(paste(x[1],x[2], sep=":"))}))
  rise_set$set_plus1time <- unlist(lapply(strsplit(as.character(rise_set$set_plus1time), "\\."), function(x) {return(paste(x[1],x[2], sep=":"))}))
  rise_set$rise_minus1time <- unlist(lapply(strsplit(as.character(rise_set$rise_minus1time), "\\."), function(x) {return(paste(x[1],x[2], sep=":"))}))
  rise_set$set_minus1time <- unlist(lapply(strsplit(as.character(rise_set$set_minus1time), "\\."), function(x) {return(paste(x[1],x[2], sep=":"))}))
  
  return(rise_set)
} #end format_sun_times function

##################################################


####################################################