# Homework 6 - Submitted by SHashikant Rajeshwar Dhuppe on October 10, 2018
# Portions of this code came from Introduction to Data Science
# but the comments are all original.

# Step A: Load and Merge datasets

# 1)	Read in the census dataset (using the function created in HW 3)

# dfstates<-read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2017/state/asrh/scprc-est2017-18+pop-res.csv", 
#                   header= TRUE, sep="," , stringsAsFactors = FALSE) #Reading the dataset
# This was working earlier but it's not working while I was submitting
# Therefore, I used the data in the folder

dfstates<-read.csv("states.csv") #using the downloaded data because URL data wasn't working

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

# 2)	Copy the USArrests dataset into a local variable (similar to HW 2)

arrests<-USArrests #Assigning inbuilt data frame value to local variable

# 3)	Create a merged dataframe -- with the attributes from both dataframes
# Hint: get the state names from the USArests dataframe with the rownames 

arrests$stateName<-row.names(arrests) #Creating a new column which contains row names of data frame
mergedDf<-merge(dfstatesCleaned, arrests) #Merging the two data frames on the basis of same column name
#As the both data frame have same column names, we needn't specify them.

# Step 2: Explore the Data - Understanding distributions

# 4)	Create a histogram using GGPLOT for the population and a different histogram for the murder rate

install.packages("ggplot2") #installing the package: ggplot2

library(ggplot2) #Including the package ggplot2

options(scipen = 5) #prevent scientific notation, so that the data can be read easily

# Creating histogram using ggplot2 for Population
# using binwidth to focus more closely

ggplot(data = mergedDf, aes(x=mergedDf$population)) + 
  geom_histogram(binwidth = 1000000, color='black',fill='green') + 
  ggtitle("Histogram of Population") +
  scale_x_continuous(name="Population") +
  scale_y_continuous(name="Frequency")

# Creating histogram using ggplot2 for Murder Rate
# using binwidth to focus more closely

ggplot(data = mergedDf, aes(x=mergedDf$Murder)) + 
  geom_histogram(binwidth = 0.5, color='black', fill='red') +
  ggtitle("Histogram of Murder Rate") +
  scale_x_continuous(name="Murder Rate") +
  scale_y_continuous(name="Frequency")

# 5)	Create a boxplot for the population, and a different boxplot for the murder rate

# Creating Boxplot using ggplot2 for Population
# using x=1 for factor

ggplot(data = mergedDf, aes(x=1,y=mergedDf$population)) + 
  geom_boxplot(fill="green") +
  ggtitle("BoxPlot of Population") +
  scale_y_continuous(name="Population")

# Creating Boxplot using ggplot2 for Murder Rate
# using x=1 for factor

ggplot(data = mergedDf, aes(x=1,y=mergedDf$Murder)) +
  geom_boxplot(fill="red") +
  ggtitle("BoxPlot of Murder Rate") +
  scale_y_continuous(name="Murder Rate")

# 6)	Create a block comment explaining which visualization (boxplot or histogram) you thought was more helpful (explain why)

## The main purpose of the visualizatio is to know more about data
## Box plot provides stats such as 
## Minimum value, Second quartile, Median value, Third quartile and Maximum value
## Whereas histogram doesn't provide it readily
## That's why I found boxplot to be more helpful

# Step 2: Which State had the Most Murders - bar charts

# 7)	Calculate the number of murders per state
# Hint: use the population and the murder rate in your new dataframe

mergedDf$noOfMurders<-(mergedDf$population/100000)*mergedDf$Murder #Calculating Murders
cat("Number of Murders Per State\n")
mergedDf[,c(1,9)] #Murders Per State of all the states

# 8)	Generate a bar chart, with the number of murders per state
# Hint: use the geom_col function

# Creating Bar Chart using ggplot2 for number of murders per state

ggplot(data= mergedDf ,aes(x=stateName,y=noOfMurders))+ 
  geom_col( fill="red", color="black") +
  scale_y_continuous(name="Number of Murders") +
  ggtitle("Bar Chart of Number of Murders per state")

#The x-axis names will appear cluttered as we didn't do any corrections

# 9)	Generate a bar chart, with the number of murders per state. Rotate text (on the X axis), so we can see x labels
# Hint: use theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Creating Bar Chart using ggplot2 for number of murders per state
# This time x-axis text will be rotated in order to look legible

ggplot(data=mergedDf , aes(x=stateName, y=noOfMurders)) +
  geom_bar(stat = "identity", fill="blue", color="black") +
  scale_y_continuous(name="Number of Murders") +
  ggtitle("Bar Chart of number of Murders Per State") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# 10)	Generate a new bar chart, the same as in the previous step, but also sort the x-axis by the murder rate

sortedmergeddf <- mergedDf[order(mergedDf$Murder ),] #Sorting on the basis of Murder Rate

# Creating Bar Chart using ggplot2 for Murder Rate after sorting

ggplot(data= sortedmergeddf, aes(x=stateName, y=mergedDf$noOfMurders )) +
  geom_bar(stat = "identity", fill="orange", color="white") +
  scale_y_continuous(name="Number of Murders") +
  ggtitle("Bar Chart of Murder Rate") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# 11)	Generate a third bar chart, the same as the previous step, but also showing percentOver18 as the color of the bar

# Creating Bar Chart using ggplot2 for Murder Rate along with percentOver18

ggplot(data=sortedmergeddf, aes(x=stateName, y=mergedDf$Murder,fill=percentOver18)) +
  geom_bar(stat = "identity", position = "dodge",color="black") +
  scale_y_continuous(name="Number of Murders") +
  ggtitle("Bar Chart of Murder Rate along with percent over 18") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Step 2: Explore Murders - scatter chart

# 12)	Generate a scatter plot - have population on the X axis, the percent over 18 on the y axis, and the size & color represent the Murder rate

ggplot(data=sortedmergeddf,aes(x=mergedDf$population,y=mergedDf$percentOver18)) +
  geom_point(aes(size=mergedDf$Murder,color=mergedDf$Murder)) +
  scale_y_continuous(name="Percentage Over 18") +
  scale_x_continuous(name="Population")+
  ggtitle("Scatter Plot") +
  scale_color_gradient(low="red",high="pink")

