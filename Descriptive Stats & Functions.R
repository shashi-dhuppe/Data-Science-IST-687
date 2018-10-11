##################################################################
# Homework 3 - Submitted by Shashikant Rajeshwar Dhuppe on September 19, 2018
# Portions of this code came from Introduction to Data Science
# but the comments are all original.

# Step A: Use read.csv( ) and url( ) to read a CSV file form the web into a data frame

# 1.	Use R code to read directly from a URL on the web. Store the dataset into a new dataframe, called dfStates. Use stringsAsFactors=FALSE. 
# The URL is: https://www2.census.gov/programs-surveys/popest/datasets/2010-2017/state/asrh/scprc-est2017-18+pop-res.csv

dfstates<-read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2017/state/asrh/scprc-est2017-18+pop-res.csv", 
                   header= TRUE, sep="," , stringsAsFactors = FALSE)

# Step B: Clean the dataframe
# 2.	Use View( ), head( ), and tail( ) to examine the data frame. 

View(dfstates) #viewing the entire database. It opens a new file with a tabular struture
head(dfstates) #displaying the first six(default) instances
tail(dfstates) #displaying the last six(default) instances

# 3.	Remove unneeded columns and rows by using the minus sign in the rows or columns of the [ , ] accessor.

dfstatescleaned<-dfstates[-c(1:4)] #deleting the columns using indices. first four columns contain irrelevant data

# 4.	Remove the last Row (for Puerto Rico)

dfstatescleaned<-dfstatescleaned[-c(which(dfstatescleaned$NAME=="Puerto Rico Commonwealth")),] #Deleting the row of Puerto Rico

# 5.	Make sure there are exactly 51 rows (one per state + the district of Columbia).  
dfstatescleaned<-dfstatescleaned[-c(which(dfstatescleaned$NAME=="United States")),] #Deleting the summary for the United States

# 6.	Make sure there are precisely 4 columns, with the following names:
#   stateName, population, popOver18, percentOver18. 

#Renaming the column names with desired names

colnames(dfstatescleaned)[colnames(dfstatescleaned)=="NAME"]<-"stateName" 
colnames(dfstatescleaned)[colnames(dfstatescleaned)=="POPESTIMATE2017"]<-"population"
colnames(dfstatescleaned)[colnames(dfstatescleaned)=="POPEST18PLUS2017"]<-"popOver18"
colnames(dfstatescleaned)[colnames(dfstatescleaned)=="PCNT_POPEST18PLUS"]<-"percentOver18"

# Step C: Create a Function
# 7.	Create a function that takes no parameters and returns the clean dataframe created in step 6 above.
 
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

print(clean_data())

# Step D: Explore the dataframe

# 8.	Calculate the average population of the states

averageOfPopulation<-mean(dfstatescleaned$population) #Calculating the average of entire population
print(averageOfPopulation) #Displaying the average population

averageOfPopulationOver18<-mean(dfstatescleaned$popOver18) #Calculating the average of population over 18 years
print(averageOfPopulationOver18) #Displaying the average population over 18

# 9.	Find the state with the highest population  (use which.max)

maxOfPopulation<-which.max(dfstatescleaned$population) #finding the index of the state with max population
print(dfstatescleaned[maxOfPopulation,1]) #Printing the state using index calcualted above

# 10.	Create a histogram of the state populations, what do you observe?

hist(dfstatescleaned$population, #Creating a histogram of cleaned data frame
     main = "Histogram of state populations", #Renaming the header of histogram
     xlab = "Population" #Renaming the x-axis
     ) #creating histogram of population

# It's assymetrical
# It's very concentrated towards left and negligible towards right

# 11.	Sort the data frame by population

sortdf<-dfstatescleaned[order(dfstatescleaned$population),] #sorting the data frame in ascending order and storing into new data frame

# 12.	Show the 10 states with the lowest populations

print(sortdf[1:10,1,drop=FALSE]) #displaying the 10 states with lowest population

# 13.	Use barplot( ) to create a plot of each of the population from the sorted dataframe.  What do you observe?

barplot(sortdf$population) 
# It's assymetrical and shows increasing graph towards right
# It shows an increasing graph because we have sorted it in ascending order
# If we would have done descending order, it would have showed us decreasing graph

barplot(sortdf$popOver18) 

#It's assymetrical and shows increasing graph towards right
# Similarly, this data also shows increasing graph.
