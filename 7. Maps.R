# Homework 7 - Submitted by Shashikant Rajeshwar Dhuppe on October 17, 2018
# Portions of this code came from Introduction to Data Science
# but the comments are all original.

# Step A: Load and Merge datasets
# 1)	Read in the census dataset and the USArrests and merge them 
# (just like HW6)

dfstates<-read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2017/state/asrh/scprc-est2017-18+pop-res.csv", 
                                      header= TRUE, sep="," , stringsAsFactors = FALSE) #Reading the dataset
                   
clean_data<-function(){ #Defining a function
  df<-dfstates[-c(1:4)] #Deleting the first four irrelevant rows
  pr<-which(df$NAME=="Puerto Rico Commonwealth") #finding the index of Puerto Rico
  us<-which(df$NAME=="United States") #Finding the index of summary of united states
  df<-df[-c(pr,us),] #Deleting the rows of Puerto Rico and summary of united states
  
  #renaming the columns with desired name
  
  colnames(df)[colnames(df)=="NAME"]<-"stateName"
  colnames(df)[colnames(df)=="POPESTIMATE2017"]<-"population"
  colnames(df)[colnames(df)=="POPEST18PLUS2017"]<-"popOver18"
  colnames(df)[colnames(df)=="PCNT_POPEST18PLUS"]<-"percentOver18"
  return(df)
}

dfstatesCleaned<-clean_data() #Using the function and storing the value in another data frame

arrests<-USArrests #Assigning inbuilt data frame value to local variable

arrests$stateName<-row.names(arrests) #Creating a new column which contains row names of data frame
mergedDf<-merge(dfstatesCleaned, arrests) #Merging the two data frames on the basis of same column name
#As the both data frame have same column names, we needn't specify them.

# 2)	Add the area of each state, and the center of each state, to the merged dataframe, 
#     using the 'stateName', 'state.center' and 'state.name' vectors

stateArea<-state.area #Copying the value of inbuilt function - area of all states
stateCenter<-state.center #Copying the value of inbuilt function - centers x & y of all states
stateName<-state.name #Copying the value of inbuilt function - Name of all states

stateDf<-data.frame(stateArea,stateCenter,stateName) #Creating a data frame of the generated vectors

mergedDf<-merge(mergedDf,stateDf,by='stateName') #Merging the earlier made dataset with above entites

# Step B: Generate a color coded map
# 3)	Create a color coded map, based on the area of the state 

install.packages("ggplot2") #Installing the package: ggplot2
library(ggplot2) #Using the package: ggplot2

install.packages("ggmap") #Installing the package: ggmap
library(ggmap) #using the package: ggmap

options(scipen = 5) #Converting scientific notation to common language

mergedDf$stateName<-tolower(mergedDf$stateName) #Converting names of all the states to lower case
us<-map_data("state") #Finding the standard map of USA

ggplot(mergedDf, aes(map_id=stateName)) +
  geom_map(map=us,aes(fill=stateArea), color="black") +
  expand_limits(x=us$long,y=us$lat) +
  coord_map() +
  ggtitle("Color coded, state based map of USA")

# building a map based on the states identity
# and then filling it color wise according to population of each state
# and finally using coord_map function to keep it properly in place

# Step C: Create a color shaded map of the U.S. based on the Murder rate for each state 

# 4) Repeat step B, but color code the map based on the murder rate of each state.

ggplot(mergedDf, aes(map_id=stateName)) +
  geom_map(map=us,aes(fill=Murder), color="black") +
  expand_limits(x=us$long,y=us$lat) +
  coord_map() +
  ggtitle("Color coded, Murder Rate based map of USA")

# Repeating the step B, but instead of taking area, we are taking Murder Rate to differentiate the states

# 5) Show the population as a circle per state (the larger the population, the larger the circle), 
# using the location defined by the center of each state

ggplot(mergedDf, aes(map_id=stateName)) +
  geom_map(map=us,fill="red", color="black") +
  geom_point(aes(x=mergedDf$x, y= mergedDf$y, size=population),color="white") +
  expand_limits(x=us$long,y=us$lat) +
  coord_map() +
  ggtitle("Population as circle per state")

# Using the map made in previous steps, for adding circles according to population, we are taking help of geom_point
# Bulding a geometric point for each state and defining the size of each point by the relative population of every state

# Step D: Zoom the map

# 6)	Repeat step C, but only show the states in the north east
# Hint: get the lat and lon of new york city
# Hint: set the xlim and ylim to NYC +/- 10

latlon.ny<-geocode(source = "dsk"," NYC, ny") # Getting the latitutde and longititude of New York City

ggplot(mergedDf, aes(map_id=stateName)) +
  geom_map(map=us,aes(fill=Murder), color="black") +
  expand_limits(x=us$long,y=us$lat) +
  xlim(latlon.ny$lon-10, latlon.ny$lon+10) + 
  ylim(latlon.ny$lat-10,latlon.ny$lat+10)+
  coord_map() +
  ggtitle("Color coded, Murder Rate based map of northeast USA")

# Using the map made for Murder Rate of entire USA, restricting the x and y limits to the coordinates of NYC generated earlier
# Here, we are taking care of fluctuations of +/-10

ggplot(mergedDf, aes(map_id=stateName)) +
  geom_map(map=us,fill="red", color="black") +
  geom_point(aes(x=mergedDf$x, y= mergedDf$y, size=population),color="white") +
  expand_limits(x=us$long,y=us$lat) +
  xlim(latlon.ny$lon-10, latlon.ny$lon+10) + 
  ylim(latlon.ny$lat-10,latlon.ny$lat+10)+
  coord_map() +
  ggtitle("Population as circle per state of northeast states")

# Applying similar concept and concentrating on circlular portion for northeastern states.
