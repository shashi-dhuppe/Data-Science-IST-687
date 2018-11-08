# Homework 9 - Submitted by Shashikant Rajeshwar Dhuppe on November 7, 2018
# Portions of this code came from Introduction to Data Science
# but the comments are all original.

# Part A: Explore Data Set
# 1)	Load the dataset: hotelSurveyBarriot.json (similar to HW8, but a different dataset)

setwd("C:/Users/SHASHIKANT/Desktop/R") #Setting the directory where JSON file is located

install.packages("jsonlite") # Installing the package: jsonlite
library(jsonlite) # Including the package: jsonlite

JSONresult<-fromJSON("hotelSurveyBarriot.json") #accessing the JSON file and converting it to list

json_data<- sapply(JSONresult, rbind) #binding the meta data and data and converting it to list

JSONdf<-data.frame(json_data) #converting list to data frame

# 2)	Name the dataframe hotelSurvey

hotelSurvey<-JSONdf #Assigning to dataframe hotelSurvey

# Part B: Explore Data Set

# 1)	Ensure hotelSurvey is a dataframe, and look at the structure via the str() command

# converting respective fields to numeric
hotelSurvey[1:3]<-lapply(hotelSurvey[1:3], as.numeric)
hotelSurvey[5:6]<-lapply(hotelSurvey[5:6], as.numeric)
hotelSurvey[8:10]<-lapply(hotelSurvey[8:10], as.numeric)

print(str(hotelSurvey)) #Printing the structure of the data frame

# 2)	Map each numeric attribute to a category  - 
# Since we want to create rules, we should convert the attributes that have a numeric range into buckets (ex. low or high)

# We will do the folowing process to all the 8 numeric attributes which are
# a. Customer Satisfaction b. Hotel Size c. CheckInStatus d.Hotel Clean
# e. Hotel Friendly f.Guest Age g. Length of Stay h. When Booked Trip
#
# First we will use the median() to decide the center section of dataset
# Secondly, we will store the respective column to one variable(dummy)
# Then we will map all the values to Average
# After that, we will map all the values below median to Low
# Then, we will map all the values above median to High
# Finally, we will transfer the dummy variable values to particular column
# As we will be doint it for multiple times, we will be creating a function to do our work

# Defining a function
map<-function(vec){
  m=median(vec)
  dummy<-replicate(length(vec),"Average")
  dummy[vec > m]<-"High"
  dummy[vec < m]<-"Low"
  vec<-dummy
  return(vec)
}

# a. Mapping the attributes for Overall Customer Satisfaction

hotelSurvey$overallCustSat<-map(hotelSurvey$overallCustSat)

# b. Mapping the attributes for Hotel Size

hotelSurvey$hotelSize<-map(hotelSurvey$hotelSize)

# c. Mapping the attributes for CheckInStatus

hotelSurvey$checkInSat<-map(hotelSurvey$checkInSat)

# d. Mapping the attributes for Hotel Clean

hotelSurvey$hotelClean<-map(hotelSurvey$hotelClean)

# e. Mapping the attributes for Hotel Friendliness

hotelSurvey$hotelFriendly<-map(hotelSurvey$hotelFriendly)

# f. Mapping the attributes for Guest Age

hotelSurvey$guestAge<-map(hotelSurvey$guestAge)

# g. Mapping the attributes for Length of Stay

hotelSurvey$lengthOfStay<-map(hotelSurvey$lengthOfStay)

# h. Mapping the attributes for When Booked Trip

hotelSurvey$whenBookedTrip<-map(hotelSurvey$whenBookedTrip)

# converting respective fields to factors
hotelSurvey[1:3]<-lapply(hotelSurvey[1:3], as.factor)
hotelSurvey[5:6]<-lapply(hotelSurvey[5:6], as.factor)
hotelSurvey[8:10]<-lapply(hotelSurvey[8:10], as.factor)

summary(hotelSurvey[1:10]) #Checking whether each numerical has been categorized

# 3)	Count the people in each category of for the age and friendliness attributes

Guest_Age<-hotelSurvey$guestAge # Storing the value in a variable
print(table(Guest_Age)) # Checking the fields for the Guest Age

Hotel_Friendly<-hotelSurvey$hotelFriendly # Storing the value in a variable
print(table(Hotel_Friendly)) #Checking the fields for the Friendliness

# 4)	Express the results of problem 3 as percentages by sending the results of the table( ) command into the prop.table( ) command

print(prop.table(table(Guest_Age))) # Expressing the number of low,average and high of Guest Age as percentages

print(prop.table(table(Hotel_Friendly))) # Expressing the number of low,average and high of Hotel Friendliness as percentages
 
# 5)	Show a "contingency table" of percentages for the age and the overall satisfaction variables together. 
# Write a block comment about what you see.

Overall_Customer_Satisfaction<-hotelSurvey$overallCustSat #Storing it in different variable as to differentiate purposes
ct<-table(hotelSurvey$guestAge,Overall_Customer_Satisfaction) # Preparing contigency table for guest age and overall customer satisfaction
print(prop.table(ct)) # Preparing contigency table as percentages

# Part C: Coerce the data frame into transactions
# 6)	Install and library two packages: arules and arulesViz.

install.packages("arules") # Installing the package: arules
library(arules) # Including the library: arules

install.packages("arulesViz") # Installing the package: arulesViz
library(arulesViz) # Including the library: arulesViz
 
# 7)	Coerce the hotelSurvey data frame into a sparse transactions matrix using:
#   hotelSurveyX <- as(hotelSurvey,"transactions")

hotelSurveyX<- as(hotelSurvey,"transactions") #Converting the dataset to set of transactions

# 8)	Use the inspect( ), itemFrequency( ), and itemFrequencyPlot( ) commands to explore the contents of hotelSurveyX.

# HotelSurveyX being an huge list, we will inspect the first 10 rows to see the gist of HotelSurveyX
inspect(hotelSurveyX[1:10,]) # Using inspect to see the transactions

# HotelSurveyx being an huge list, there are too many categorical values.
# Columns such as StateName and Free Text doesn't contribute to the analysis
# So while counting frequency we have excluded that

itemFrequency(hotelSurveyX[,c(1:9,61:77)]) #using itemFrequency to check frequency of only desired columns

#Similar thought process will be applied to itemFrequencyplot

itemFrequencyPlot(hotelSurveyX[,c(1:9,61:77)]) # Using itemFrequcyplot to show graphical visualisatioon

# Part C: Use arules to discover patterns
# Support is the proportion of times that a particular set of items occurs relative to the whole dataset. 
# Confidence is proportion of times that the consequent occurs when the antecedent is present. 
# See the review on the next page.  

# 9)	Run the apriori command to try and predict happy customers (as defined by their overall satisfaction being high - above 7).

# We have chosen RHS as Overall satisfaction as high, because we want to predict happy customers
# minlen is set to 2, as to avoid any empty rules
rules<-apriori(hotelSurveyX,parameter = list(support=0.12,confidence=0.5,minlen=2),appearance = list(rhs=c("overallCustSat=High")))

# 10)	Once you have a reasonable number of rules, use inspect( ) to view the ruleset.

inspect(rules) # Checking what rules have been identified

# 11)	 If you had to provide two rules to the hotel owner

# (in terms of what helps drive high overall customer satisfaction, what would those two rules be?  
# Use a block comment to explain your answer.

plot(rules) # Plotting the rules to see the significance of rules

# Here we see that there are very few rules who have value of lift greater than 2

goodrules<-rules[quality(rules)$lift>2] # Choosing those rules from above ruleset which has lift value greater than 2

inspect(goodrules) # Checking what rules have been identified

# If I had to provide two rules to the hotel owner
#
# Based on the analysis, these will be my two rules
# 1. {hotelFriendly=High,lengthOfStay=Average,whenBookedTrip=High} 
#  It says, if HotelFriendliness is High, Length of stay is Average and When Booked Trip is High, Overall Customer Satisfaction is High
#
# {hotelFriendly=Low,guestAge=Low,lengthOfStay=Average,whenBookedTrip=Low} => {overallCustSat=High} 0.1299  0.9488678  2.039699 1299
#  It says, if HotelFriendliness is High, Guest Age is High, Length of stay is Average and When Booked Trip is High, Overall Customer Satisfaction is High
# 
# A good rule is defined by its lift. 
# So, I also have considered Lift to be deciding factor amongst which rules are strong.
# By using Inspect command, we came to know the lift values of many rules
# This helped me in determing which rules can be termed as good rules
#
