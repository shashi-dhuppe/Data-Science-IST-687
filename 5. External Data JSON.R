# Homework 5 - Submitted by Shashikant R. Dhuppe on October 4 2018
# Portions of this code came from Introduction to Data Science
# but the comments are all original.

# Step A: Load the data

# 1)	Read in the following JSON dataset
# http://data.maryland.gov/api/views/pdvh-tf2u/rows.json?accessType=DOWNLOAD

install.packages("jsonlite") ##installing the package:jsonlite
library(jsonlite) #Including or accessing the package:jsonlite
url<-"http://data.maryland.gov/api/views/pdvh-tf2u/rows.json?accessType=DOWNLOAD" #storing the url in a variable

install.packages("curl") ##installing the package:curl
JSONresult<-fromJSON(url)##Including or accessing the package:curl

json_data<- sapply(JSONresult, rbind) #binding the meta data and data and converting it to list

JSONdf<-data.frame(json_data) #converting list to data frame

# Step B: Clean the data

# 2)	Remove the first 8 columns

# Removing the first 9 columns instead of 8 because the generated data frame contains an additional column
dfClean<-JSONdf[,-c(1:9)] 

# 3)	Then, to make it easier to work with, name the rest of the columns as follows:

# Renaming the columns
colnames(dfClean) <- c("CASE_NUMBER","BARRACK","ACC_DATE","ACC_TIME","ACC_TIME_CODE","DAY_OF_WEEK","ROAD","INTERSECT_ROAD","DIST_FROM_INTERSECT","DIST_DIRECTION","CITY_NAME","COUNTY_CODE","COUNTY_NAME","VEHICLE_COUNT","PROP_DEST","INJURY","COLLISION_WITH_1","COLLISION_WITH_2")

dfClean<-as.data.frame(apply(dfClean,2,function(x)gsub('\\s+','',x))) #Removing all blank spaces by substituting it with no character

# Step D: Explore the data - using the dataframe you created

install.packages("sqldf") ##installing the package:sqldf
library(sqldf) ##Including or accessing the package:sqldf

# 4)	What was the total number of accidents with injuries?

## Using an sql query to fetch number of accidents by giving condition that it should have an injury
sqldf("select count(*) AS 'no of accidents with injuries' FROM dfClean where INJURY='YES'")

# 5)	How many accidents happened on Sunday?

## Using an sql query to fetch number of accidents by giving condition that it should have occured on Sunday
sqldf("select count(*) AS 'No of accidents on Sunday' FROM dfClean where DAY_OF_WEEK='SUNDAY'")

# 6)	How many injuries occurred each day of the week?

## Using an sql query to fetch number of accidents by giving condition that it should have an injury
## and grouping it using GROUP BY clause
sqldf("select DAY_OF_WEEK, count(*) AS 'No of accidents with Injuries' FROM dfClean where INJURY='YES' GROUP BY DAY_OF_WEEK")

# Step D: Explore the data - using dplyr

install.packages("dplyr")  ##installing the package:dplyr
library(dplyr) ##Including or accessing the package:dplyr

# Then answer the following questions:
# 7)	What was the total number of accidents with injuries?

install.packages("RCurl") ##installing the package:RCurl
library(RCurl) ##Including or accessing the package:RCurl

df.GroupByInjuries<-group_by(dfClean,INJURY) #Grouping with the help of INJURY
accidents<-summarize(df.GroupByInjuries,Injuries=n()) #Summarizing the grouped data
accidentsWithInjuries<-accidents[accidents$INJURY=="YES",] #Filtering the results containing injuries
accidentsWithInjuries<-na.exclude(accidentsWithInjuries) #Excluding the NAs in the data frame
accidentsWithInjuries

# 8)	How many accidents happened on Sunday?

df.GroupBydays<-group_by(dfClean, DAY_OF_WEEK) #Grouping with the help of Days of the week
accidentsOnAllDays<-summarize(df.GroupBydays, NoOfAccidents=n()) #Summarizing the grouped results
accidentsOnSunday<-accidentsOnAllDays[accidentsOnAllDays$DAY_OF_WEEK=="SUNDAY",] #Finding the accidents on Sunday
accidentsOnSunday #Printing the results

# 9)	How many injuries occurred each day of the week?

df.GroupBydays.Injuries<-group_by(dfClean,DAY_OF_WEEK,INJURY) #Grouping with the help of Days of the week and Injury
accidents<-summarize(df.GroupBydays.Injuries, InjuriesEachDay=n()) #Summarizing the grouped result
accidentsWithInjuries<-accidents[accidents$INJURY=="YES",] #Filtering the results containing injuries
accidentsWithInjuries<-na.exclude(accidentsWithInjuries) #Excluding the NAs in the data frame
accidentsWithInjuries[,c(1,3)] #Printing the results

# 10)	 In a block comment, explain if you find doing the analysis with the dataframe directly
# , or using dplyr easier

## In my opnion, I found analysis using dplyr was much easier.
## The functions are powerful and one can use them easily and as convienently as possible

# Step D: Explore the distribution of the number of vehicles in accidents

# 11)	What is the distribution of the number of vehicles in accidents on Friday?
#   (use a histogram and quantile)

df.GroupBydays<-group_by(dfClean, DAY_OF_WEEK, VEHICLE_COUNT) #Grouping with the help of Days of the week and Vehicle count
accidentsOnDay<-summarize(df.GroupBydays, accidents.count=n()) #Summarizing the grouped results
accidentsOnOnlyFriday<-accidentsOnDay[accidentsOnDay$DAY_OF_WEEK=="FRIDAY",] #Filtering the results containing Friday

accidentsOnOnlyFridayN<-accidentsOnOnlyFriday$accidents.count #Storing the accidents count in a vector

hist(accidentsOnOnlyFridayN , #Plotting the histogram
        main="Number of Vehicles in accidents on Friday", #Setting the heading
        xlab = "Number of Accidents",  #Naming y-axis
        ylab = "Number of Vehicles", #Naming x-axis
        axes = F)
axis(2) #Keeping the y-axis default
axis(1,at=seq(0,1500,by=300), labels = seq(0,1500,300)) #Changing the x-axis intervals and keeping it 300

# 12)	How does this distribution compare with the distribution of the 
#number of vehicles in accidents on Sunday?

accidentsOnOnlySunday<-accidentsOnDay[accidentsOnDay$DAY_OF_WEEK=="SUNDAY",] #Filtering the results containing Friday
accidentsOnOnlySundayN<-accidentsOnOnlySunday$accidents.count #Storing the accidents count in a vector

hist(accidentsOnOnlySundayN, #Plotting the histogram
     main="Number of Vehicles in accidents on Sunday", #Setting the heading
     xlab = "Number of Accidents",  #Naming y-axis
     ylab = "Number of Vehicles", #Naming x-axis
     axes = F)
axis(2) #Keeping the y-axis default
axis(1,at=seq(0,1500,by=300), labels = seq(0,1500,300))#Changing the x-axis intervals and keeping it 300

## This distribution(Sunday) is has more spaces or breaks as comapred to Friday's which shows us and it may be
## evenly spread out. One can also hypothesize the accidents took place more on Friday than on Sunday by looking at
## the distribution

