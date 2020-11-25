# PACKAGES

# need to have Program Mark installed separately
# RMark uses Mark as a platform
# install and load RMark package
# install.packages("RMark")
# cd /usr/local/bin
# ln -s ~/Applications/mark mark
# ln -s ~/Applications/PhotoScapeX PhotoScapeX

MarkViewer="open -a PhotoScapeX"
#MarkPath="/Applications"
library(RMark)

# install and load the Hmisc package to make some simple graphs
# need this package to print the figures for the real parameter estimates
# install.packages("Hmisc") # installs a lot of other packages
library(Hmisc)

# install.packages("dplyr") #install package
library(dplyr)  #needed for View function to see nice tables of data

# INPUT FILE

# set working directory 
#setwd("C:/Users/brett.sandercock/Downloads") 

# scrub to restart as needed
#rm(list=ls())

# input file
# lapwing has some extra variables with the dates converted to day of year (1 to 365)
# renamed fate to fatecode
nest = read.csv("Data/RMark.csv", header=TRUE, sep=",")
nest = nest[nest$fatecode%in%c(0,1),]
summary(as.factor(nest$fatecode))  
nrow(nest)

# rename some case-sensitive variables
names(nest)[names(nest) == 'nest'] <- 'NestID'
names(nest)[names(nest) == 'year'] <- 'Year'

# View full file with all records
#View(nest)

# PROCESSING

# clean up the fate variable
# fatecode has four values: 0, 1, 2, 5
nest$Fate[nest$fatecode==1] = 0 # successful
nest$Fate[nest$fatecode==0] = 1 # depredated
table(nest$Fate)

# adjust last_ok and last_control for logger data
nest$last_control_day[nest$logger_fate%in%"yes"] = nest$end_expo_day[nest$logger_fate%in%"yes"]
nest$last_ok_day[nest$logger_fate%in%"yes"] = nest$end_expo_day[nest$logger_fate%in%"yes"]
# subset variables

nest = select(nest, NestID, Year, first_egg_day, start_expo_day, last_ok_day, end_expo_day, last_control_day, Fate)

# create three variables needed for dates
nest$FirstFound = nest$start_expo_day
#nest$LastPresent = 0
#nest$LastChecked = 0
# successful nests
index <- nest$Fate==0
nest$LastPresent[index] = nest$end_expo_day[index]
nest$LastChecked[index] = nest$end_expo_day[index]
# unsuccessful nests
index <- nest$Fate==1
nest$LastPresent[index] = nest$last_ok_day[index]
nest$LastChecked[index] = nest$last_control_day[index]
# some failed nests have same value for ok_day and expo_day
# Program Mark will choke on this so adding a day
index = (nest$Fate==1 & nest$LastChecked==nest$LastPresent)
nest$LastChecked[index] =nest$LastChecked[index]+1

# offset is the day before first day of nesting season
# rescale all days so that day 1 is first day of season
(offset = min(nest$FirstFound)-1)
nest$FirstFound = nest$FirstFound - offset
nest$LastPresent = nest$LastPresent - offset
nest$LastChecked = nest$LastChecked - offset
nest$first_egg_day = nest$first_egg_day - offset

# nest age on day of the season, can be negative
nest$NestAge = (nest$FirstFound - nest$first_egg_day) - (nest$FirstFound - 1)

# subset variables
nest = select(nest, NestID, Year, FirstFound, LastPresent, LastChecked, Fate, NestAge)

# screen for errors in date coding

# look at first and last occasions
# needs to be 1 for dataset
(min = min(nest$FirstFound))  # 1
# this gives maximum count for number of occasions
(occ = max(nest$LastChecked))  # 205

# check input for errors
nest$Error="No"
nest$Error[(nest$LastPresent-nest$FirstFound)<0] <- "Yes"
nest$Error[(nest$LastChecked-nest$LastPresent)<0] <- "Yes"
nest$Diff = nest$LastChecked-nest$LastPresent
nest$Error[nest$Fate==1 & nest$Diff<=0] <- "Yes"
nest$Error[nest$Fate==0 & nest$Diff!=0] <- "Yes"
# look at nests flagged with errors
(Error <- subset(nest, Error=="Yes"))
# drop errors if present
nest <- nest[which(nest$Error=='No'), ]


# SPECIFY NEST SURVIVAL MODELS FOR PROGRAM MARK

# Treat categorical variables as factors
nest$Year=factor(nest$Year)

# Create a function with a set of competing models for daily survival rates (DSR)

run.nest=function()
{

# DSR is constant
Dot=mark(nest,nocc=occ,model="Nest",
model.parameters=list(S=list(formula=~1)))
return(collect.models() )
}

# run the candidate models by sending the data to Program Mark
# next line runs the models above and takes a minute or two before you get output
nest.results=run.nest() 

# look at the model results and the ranking of candidate models
# the numbers at left are index numbers, and used to pull estimates for some of the output below
nest.results # print model selection table to screen
nest.results$Dot$results$real$estimate
1-nest.results$Dot$results$real$estimate

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Examine table of model selection results #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
options(width=100) # set page width to 100 characters
sink("results.table.txt") # capture screen output to file
print.marklist(nest.results) # send output to file
sink() # return output to screen
system("SublimeText results.table.txt",invisible=FALSE) # view results in notepad

# cleanup temporary Mark files
cleanup(lx = NULL, ask = FALSE, prefix = "mark")
cleanup(ask = FALSE, prefix = "mark")
cleanup(ask=F)


# PARAMETER ESTIMATES FROM THE DIFFERENT MODELS

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Examine output for constant DSR model #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
nest.results$Dot$results$real # view the estimates of Daily Survival Rate

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Examine output for Year model #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
nest.results$Year$results$real # view the estimates of Daily Survival Rate
# plot the data
plot(nest.results$Year$results$real$estimate)
with(nest.results$Year$results$real,errbar(1:3,estimate[1:3],lcl[1:3],ucl[1:3], ylim=c(0.85,1),xaxt="n", xlab="Year",ylab="DSR"))
axis(1, at=1:3, labels=c('2018', '2019', '2020'))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Plot output for Time Trend model #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
nest.results$TimeTrend$results$real # view the estimates of Daily Survival Rate
plot(nest.results$TimeTrend$results$real$estimate, type="l", col="blue", lwd=2, xlab="Day of season", ylab="DSR", ylim=c(0.85,1))
lines(nest.results$TimeTrend$results$real$lcl, type="l", col="red", lwd=2, lty=2)
lines(nest.results$TimeTrend$results$real$ucl, type="l", col="red", lwd=2, lty=2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Plot output for Year + Trend model #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
nest.results$YearPlusTimeTrend$results$real # view the estimates of Daily Survival Rate
plot(nest.results$YearPlusTimeTrend$results$real[1:204,]$estimate, type="l", col="blue", lwd=2, xlab="Day of season", ylab="DSR", ylim=c(0.85,1))
lines(nest.results$YearPlusTimeTrend$results$real[1:204,]$lcl, type="l", lty="dashed", col="blue", lwd=1)
lines(nest.results$YearPlusTimeTrend$results$real[1:204,]$ucl, type="l", lty="dashed", col="blue", lwd=1)
lines(nest.results$YearPlusTimeTrend$results$real[205:408,]$estimate, type="l", col="red", lwd=2)
lines(nest.results$YearPlusTimeTrend$results$real[205:408,]$lcl, type="l", lty="dashed", col="red", lwd=1)
lines(nest.results$YearPlusTimeTrend$results$real[205:408,]$ucl, type="l", lty="dashed", col="red", lwd=1)
lines(nest.results$YearPlusTimeTrend$results$real[409:612,]$estimate, type="l", col="green", lwd=2)
lines(nest.results$YearPlusTimeTrend$results$real[409:612,]$lcl, type="l", lty="dashed", col="green", lwd=1)
lines(nest.results$YearPlusTimeTrend$results$real[409:612,]$ucl, type="l", lty="dashed", col="green", lwd=1)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Plot output for Year * Trend model #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
nest.results$YearByTimeTrend$results$real # view the estimates of Daily Survival Rate
plot(nest.results$YearByTimeTrend$results$real[1:204,]$estimate, type="l", col="blue", lwd=2, xlab="Day of season", ylab="DSR", ylim=c(0.85,1))
lines(nest.results$YearByTimeTrend$results$real[1:204,]$lcl, type="l", lty="dashed", col="blue", lwd=1)
lines(nest.results$YearByTimeTrend$results$real[1:204,]$ucl, type="l", lty="dashed", col="blue", lwd=1)
lines(nest.results$YearByTimeTrend$results$real[205:408,]$estimate, type="l", col="red", lwd=2)
lines(nest.results$YearByTimeTrend$results$real[205:408,]$lcl, type="l", lty="dashed", col="red", lwd=1)
lines(nest.results$YearByTimeTrend$results$real[205:408,]$ucl, type="l", lty="dashed", col="red", lwd=1)
lines(nest.results$YearByTimeTrend$results$real[409:612,]$estimate, type="l", col="green", lwd=2)
lines(nest.results$YearByTimeTrend$results$real[409:612,]$lcl, type="l", lty="dashed", col="green", lwd=1)
lines(nest.results$YearByTimeTrend$results$real[409:612,]$ucl, type="l", lty="dashed", col="green", lwd=1)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Plot output for Time Quad model #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
nest.results$TimeQuad$results$real # view the estimates of Daily Survival Rate
plot(nest.results$TimeQuad$results$real$estimate, type="l", col="blue", lwd=2, xlab="Day of season", ylab="DSR", ylim=c(0.85,1))
lines(nest.results$TimeQuad$results$real$lcl, type="l", col="red", lwd=2, lty=2)
lines(nest.results$TimeQuad$results$real$ucl, type="l", col="red", lwd=2, lty=2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Plot output for Year + Time Quad model #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
nest.results$YearPlusTimeQuad$results$real # view the estimates of Daily Survival Rate
plot(nest.results$YearPlusTimeQuad$results$real[1:204,]$estimate, type="l", col="blue", lwd=2, xlab="Day of season", ylab="DSR", ylim=c(0.85,1))
lines(nest.results$YearPlusTimeQuad$results$real[1:204,]$lcl, type="l", lty="dashed", col="blue", lwd=1)
lines(nest.results$YearPlusTimeQuad$results$real[1:204,]$ucl, type="l", lty="dashed", col="blue", lwd=1)
lines(nest.results$YearPlusTimeQuad$results$real[205:408,]$estimate, type="l", col="red", lwd=2)
lines(nest.results$YearPlusTimeQuad$results$real[205:408,]$lcl, type="l", lty="dashed", col="red", lwd=1)
lines(nest.results$YearPlusTimeQuad$results$real[205:408,]$ucl, type="l", lty="dashed", col="red", lwd=1)
lines(nest.results$YearPlusTimeQuad$results$real[409:612,]$estimate, type="l", col="green", lwd=2)
lines(nest.results$YearPlusTimeQuad$results$real[409:612,]$lcl, type="l", lty="dashed", col="green", lwd=1)
lines(nest.results$YearPlusTimeQuad$results$real[409:612,]$ucl, type="l", lty="dashed", col="green", lwd=1)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Plot output for Year * Time Quad model #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
nest.results$YearByTimeQuad$results$real # view the estimates of Daily Survival Rate
plot(nest.results$YearByTimeQuad$results$real[1:204,]$estimate, type="l", col="blue", lwd=2, xlab="Day of season", ylab="DSR", ylim=c(0.85,1))
lines(nest.results$YearByTimeQuad$results$real[1:204,]$lcl, type="l", lty="dashed", col="blue", lwd=1)
lines(nest.results$YearByTimeQuad$results$real[1:204,]$ucl, type="l", lty="dashed", col="blue", lwd=1)
lines(nest.results$YearByTimeQuad$results$real[205:408,]$estimate, type="l", col="red", lwd=2)
lines(nest.results$YearByTimeQuad$results$real[205:408,]$lcl, type="l", lty="dashed", col="red", lwd=1)
lines(nest.results$YearByTimeQuad$results$real[205:408,]$ucl, type="l", lty="dashed", col="red", lwd=1)
lines(nest.results$YearByTimeQuad$results$real[409:612,]$estimate, type="l", col="green", lwd=2)
lines(nest.results$YearByTimeQuad$results$real[409:612,]$lcl, type="l", lty="dashed", col="green", lwd=1)
lines(nest.results$YearByTimeQuad$results$real[409:612,]$ucl, type="l", lty="dashed", col="green", lwd=1)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Plot output for NestAge model #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# DID NOT RUN BECAUSE OF NA VALUES FOR NESTAGE

# Assuming that duration of incubation is about 27 days
nestAge.values=1+(0:30)*(26)/30
# check index number in nest.results
PredReal = covariate.predictions(nest.results[[5]], data=data.frame(nestAge=nestAge.values),indices=c(1))
plot(PredReal$estimates$covdata, PredReal$estimates$estimate, type="l", col="blue", lwd=2, xlab="nestAge", ylab="DSR", ylim=c(0.8,1))
lines(PredReal$estimates$covdata, PredReal$estimates$lcl, type="l", col="red", lwd=2, lty=2)
lines(PredReal$estimates$covdata, PredReal$estimates$ucl, type="l", col="red", lwd=2, lty=2)
