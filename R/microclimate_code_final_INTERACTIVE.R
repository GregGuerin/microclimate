###############################################################

#load functions
source('microclimate_functions.R')

#required packages
library(stringr)
library(dplyr)

###############################################################
#Step 1 - compile data from all sensors into one object and load sun times data
template <- read_sensor_data("~/Box/Stringybark_dieback/Students_projects_dieback/microclimate_R/KA/KA*")

#the 'template' object returned above should have all the data from the .txt files in the folder together in one data frame with the 'Sensor' field denoting which file/sensor it came from.

##################################
###Just for this particular dataset:::
#remove sensor Ka.3.5S as it stops measuring after 3 days
template <- subset(template, Sensor != "Ka.3.5S")


#... check data and add any cleaning steps needed for each case
#remove unwanted cols if desired, e.g.


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
#formal data processing ends...

#write tables to csv if desired, e.g.
write.csv(temp_means$Day, file="KA_Daily.csv") #write the daily temp variables to file
#etc...
write.csv(temp_means$Year, file="KA_Year.csv")
write.csv(temp_means$Month, file="KA_Month.csv")
write.csv(temp_means$Season, file="KA_Season.csv")

########################################
#Step 4 analyse patterns, these are exploratory examples only

###
#a slight 'fudge' so we can plot distance numerically: here we assign a distance of -1 to the outside control sensors
temp_means$Season[which(temp_means$Season$distance==""),"distance"] <- -1
temp_means$Season$distance <- as.numeric(temp_means$Season$distance)
###

#max temp with distance (max temp, summer, air sensor, day)
plot(maxT ~ distance, subset(temp_means$Season, season=="Summer" & type=="A" & day_night=="Day"))
abline(lm(maxT ~ distance, subset(temp_means$Season, season=="Summer" & type=="A" & day_night=="Day")))


boxplot(maxT ~ distance, subset(temp_means$Season, season=="Summer")) #where '-1' is 'x', the outside control sensor

boxplot(minT ~ distance, subset(temp_means$Season, season=="Winter" & type=="A")) # only air sensors, no control sensor, e.g.

boxplot(maxT ~ distance, subset(temp_means$Season, season=="Summer" & type=="S")) #soil sensors summer

boxplot(minT ~ distance, subset(temp_means$Season, season=="Winter" & type=="S" & day_night=="Day")) #soil sensors in winter day - strong pattern - colder minT with distance, 0.47 r-squared with lm. Note no relationship for air sensors. NOTE single values if just one transect included in the raw data

boxplot(minT ~ distance, subset(temp_means$Season, season=="Winter" & (type=="S" | type=="XS") & day_night=="Day")) # soil control colder in winter and warmer in summer

#NOTE R-squared of 0.70 for night time winter minima and soil sensors!!! Bizarre that it is colder the further from the edge though - you'd expect the opposite?
summary(lm(minT ~ distance, subset(temp_means$Season, season=="Winter" & type=="S" & day_night=="Night")))

#also with control soil sensor added in:
boxplot(minT ~ distance, subset(temp_means$Season, season=="Winter" & (type=="S" | type=="XS") & day_night=="Night")) #min night temp for soil is way lower in the open, but decreases from highest at edge to colder in interior!


#e.g., max daytime summer air temperature for transect Ka1 only
plot(maxT ~ distance, subset(temp_means$Season, season=="Summer" & type=="A" & transect=="Ka1" & day_night=="Day"))

#add the control sensor
plot(maxT ~ distance, subset(temp_means$Season, season=="Summer" & (type=="A" | type=="X") & transect=="Ka1" & day_night=="Day"))

#much hotter summer day max soil T outside:
boxplot(maxT ~ distance, subset(temp_means$Season, season=="Summer" & (type=="S" | type=="XS") & day_night=="Day"))

#somewhat hotter summer air T outside
boxplot(maxT ~ distance, subset(temp_means$Season, season=="Summer" & (type=="A" | type=="X") & day_night=="Day"))

#############################
#Daily values, e.g.
boxplot(maxT ~ distance, subset(temp_means$Day, season=="Summer" & (type=="A" | type=="X") & day_night=="Day"))

#'Yearly' values (max temp, air)
boxplot(maxT ~ distance, subset(temp_means$Year, (type=="A" | type=="X") ))

#Monthly temp range values for January, soil
boxplot(rangeT ~ distance, subset(temp_means$Month, type=="S" & month=="January")) #January


###################
#technically, though, we may need to recognise non-independence of transect points, e.g.,  first few sensors are spatially close, suggesting grouping structure through random effects, e.g.:

####WILL NOT WORK WITH EXAMPLE DATA FROM KA.1 single transect!!

library(lme4)

mod0 <- lmer(minT ~ (1|transect), subset(temp_means$Season, season=="Winter" & type=="A" & day_night=="Day"))

mod1 <- lmer(minT ~ distance + (1|transect), subset(temp_means$Season, season=="Winter" & type=="A" & day_night=="Day"))

#car::Anova(mod1)
anova(mod0, mod1)

###

mod0 <- lmer(minT ~ (1|transect), subset(temp_means$Season, season=="Winter" & (type=="A" | type=="X") & day_night=="Night"))

mod1 <- lmer(minT ~ as.factor(distance) + (1|transect), subset(temp_means$Season, season=="Winter" & (type=="A" | type=="X") & day_night=="Night"))

#car::Anova(mod1)
anova(mod0, mod1)