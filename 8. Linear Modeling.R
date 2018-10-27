# Homework 8 - Submitted by Shashikant Rajeshwar Dhuppe on October 24, 2018
# Portions of this code came from Introduction to Data Science
# but the comments are all original.

# specify the packages of interest
packages=c("RCurl","gdata","jsonlite","ggplot2")

# use this function to check if each package is on the local machine
# if a package is installed, it will be loaded
# if any are not, the missing package(s) will be installed and loaded
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

options(scipen = 999) # Converting scientific notation to normal numbers

# Part A: Load and condition the data
# 
# 1. The data is available on blackboard (hotelSurveySherison), as a JSON file. 
# Hint: Don't forget to use setwd() to make sure that R is looking in the right folder for your text file.

setwd("C:/Users/SHASHIKANT/Desktop/R") #Setting the directory where JSON file is located

JSONresult<-fromJSON("hotelSurveySherison.json") #accessing the JSON file and converting it to list

json_data<- sapply(JSONresult, rbind) #binding the meta data and data and converting it to list

JSONdf<-data.frame(json_data) #converting list to data frame

# 2. Use the str command to make sure you can see the following attributes

# overallCustSat: Overall customer satisfaction (based on survey response)
# hotelSize: The size of the hotel (number of rooms in the hotel)
# gender: gender of the customer
# guestAge: age of the guest (in years)
# lengthOfStay: nights at the hotel
# whenBookedTrip: number of days before the trip the room was reserved
# checkInSat: The customer;s survey response to their checkin experience
# hotelState: The state (location) of the hotel - in the united states
# hotelClean: The customer's survey response on the cleanliness of the hotel
# hotelFriendly: The customer's survey response on the friendliness of the staff
# freeText: free form customer comments

print(str(JSONdf)) #Printing the structure of thee data frame

# Part B: Explore the data

# 1. Create bivariate plots for each of the attributes.
 
# Your code should produce nine separate plots. 
# Make sure the Y-axis and X-axis are labeled. 
# Keeping in mind that the overall customer satisfaction is the outcome (or dependent) variable, 
# which axis should it go on in your plots? 
# Hint: use the jitter command, so you can see all the surveys (something such as) jitter(hotelSurvey$checkInSat)

# Using jitter to just add random noise to a data frame column of numeric values

JSONdf$overallCustSat<- jitter(as.numeric(JSONdf$overallCustSat))
JSONdf$hotelSize<-jitter(as.numeric(JSONdf$hotelSize))
JSONdf$checkInSat<-jitter(as.numeric(JSONdf$checkInSat))
JSONdf$hotelClean<-jitter(as.numeric(JSONdf$hotelClean))
JSONdf$hotelFriendly<-jitter(as.numeric(JSONdf$hotelFriendly))
JSONdf$guestAge<-jitter(as.numeric(JSONdf$guestAge))
JSONdf$lengthOfStay<-jitter(as.numeric(JSONdf$lengthOfStay))
JSONdf$whenBookedTrip<-jitter(as.numeric(JSONdf$whenBookedTrip))

str(JSONdf) #Seeing the resulting structure 

# Plotting the bivariate plot of each attribute with Overall Customer Satisfaction

# 1. with Hotel Size

ggplot(data=JSONdf, aes(x=hotelSize, y=overallCustSat)) +
  geom_point(size=4,color="red") +
  ggtitle(" Hotel Size vs overall Customer Satisfaction") +
  scale_x_discrete(" Hotel Size") +
  scale_y_continuous("Overall Customer Satisfaction")

# 2. with Check In Status

ggplot(data=JSONdf, aes(x=checkInSat , y=overallCustSat)) +
  geom_point(size=2,color="green") +
  ggtitle(" Check In Status vs overall Customer Satisfaction") +
  scale_x_discrete(" Check In Status") +
  scale_y_continuous("Overall Customer Satisfaction")

# 3. with Hotel Clean

ggplot(data=JSONdf, aes(x=hotelClean , y=overallCustSat)) +
  geom_point(size=2,color="yellow") +
  ggtitle(" Hotel Clean vs overall Customer Satisfaction") +
  scale_x_discrete(" Hotel Clean") +
  scale_y_continuous("Overall Customer Satisfaction")

# 4. Hotel Friendliness

ggplot(data=JSONdf, aes(x=hotelFriendly , y=overallCustSat)) +
  geom_point(size=2,color="pink") +
  ggtitle(" Hotel Friendliness vs overall Customer Satisfaction") +
  scale_x_discrete(" Hotel Friendliness") +
  scale_y_continuous("Overall Customer Satisfaction")

# 5. with Guest Age

ggplot(data=JSONdf, aes(x=guestAge , y=overallCustSat)) +
  geom_point(size=2,color="light blue") +
  ggtitle(" Guest Age vs overall Customer Satisfaction") +
  scale_x_discrete(" Guest Age ") +
  scale_y_continuous("Overall Customer Satisfaction")

# 6. with Length of Stay

ggplot(data=JSONdf, aes(x=lengthOfStay , y=overallCustSat)) +
  geom_point(size=2,color="blue") +
  ggtitle(" Length of stay vs overall Customer Satisfaction") +
  scale_x_discrete(" Length of stay ") +
  scale_y_continuous("Overall Customer Satisfaction")

# 7. with when booked trip 

ggplot(data=JSONdf, aes(x=whenBookedTrip , y=overallCustSat)) +
  geom_point(size=2,color="light green") +
  ggtitle(" When Booked Trip vs overall Customer Satisfaction") +
  scale_x_discrete(" When Booked Trip ") +
  scale_y_continuous("Overall Customer Satisfaction")

# 8. with gender

ggplot(data=JSONdf, aes(x=gender , y=overallCustSat)) +
  geom_point(size=2,color="light green") +
  ggtitle(" Gender vs overall Customer Satisfaction") +
  scale_x_discrete(" Gender ") +
  scale_y_continuous("Overall Customer Satisfaction")

# 9. with hotelStatte

ggplot(data=JSONdf, aes(x=hotelState , y=overallCustSat)) +
  geom_point(size=2,color="light green") +
  ggtitle(" Hotel State vs overall Customer Satisfaction") +
  scale_x_discrete(" Hotel State ") +
  scale_y_continuous("Overall Customer Satisfaction") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

# 2. What do you observe from the plots? Note via a block comment.

# We observe that data is not fully scatterred neither fully dense.
# Its clutterred in groups
# and those groups are scatterred over the area
# Although the plots differ in how they are scattered and grouped
# We can also see there is no specific pattern visible to naked eye.

# Part B: Generate a linear model
 
# 3. Next, create one regression model predicting the overall customer satisfaction from the other variables 
# (but not the freeText response). 
# Refer to page 202 in the text for syntax and explanations of lm( ). 
# Make sure to include all predictors in one model - NOT different models each with one predictor.

JSONdf<-JSONdf[,-11] #Deleting the column of free text because it doesn't contribute in the overall customer satisfaction

str(JSONdf) # Seeing the structure after removing the column of free text

# Appling model considering all the numeric predictors which might affect overall customer satisfaction

model<-lm(formula = overallCustSat~hotelSize + checkInSat + hotelClean + hotelFriendly + guestAge + lengthOfStay,data = JSONdf)

summary(model) # Seeing the summary to determine which predictors contribute at what level

# 4. Report the R-Squared in a comment.

# Which of the predictors are statistically significant in the model? 
# In a comment, report the coefficients (AKA slopes or B-weights) for each predictor that is statistically significant.

# The variables 1. CheckInStat 2. HotelClean 3. HotelFriendly 4.guestofStay 5. lengthofstay are statistically significant
# the slopes of each are
# 1. checkInstat: 0.0000091900
# 2. hotelClean: < 0.0000000115
# 3. HotelFriendly: < 0.0000000000000002
# 4. guestAge: < 0.0000000000000002
# 5. lengthofstay: < 0.0000000000000002
 
# 5. Write a block comment that explains in a narrative your overall interpretation of the model. 
# Make sure to refer to each variable (one dependent and three independent) by a descriptive name (i.e., not X1, X2, etc.).

# Based on the summary of earlier regression model, the three predictors hotelFriendly, guestAge and lengthOfStay
# are of equal value.
# This says that all these three predictors contribute to change in overall customer rating
# As they are of equal value, we can't directly pinpoint which of them affects the most
# Thus, this model gave preety good idea on significant predictors and helped to narrow down from 9 predictors to top 3

# Part C: Generate a different linear model
 
# 6. Next, create a different regression model predicting the overall customer satisfaction 
# from the one variable you think is best.

# Based on the summary of earlier regression model, the three predictors hotelFriendly, guestAge and lengthOfStay
# are of equal value.
# In my opinion, I think hotelFriendly is most significant

# Creating a linear model of overall customer satisfaction and hotelFriendly
modelOnlyOneVariable<-lm(formula = overallCustSat~hotelFriendly, data = JSONdf)

# Plotting the points of overall customer satisfaction and hotelFriendly
plot(JSONdf$overallCustSat,JSONdf$hotelFriendly)

# Generating line based on the model created above
abline(modelOnlyOneVariable)

# 7. Write a block comment comparing the two lm models.

# The first model provided us with the information on which predictors are more significant and which are less
# while the second one helped us to generate model on one predictor which we think contributes the most
# Thus, although both generate model, the purpose at which they are used differs and so their usage.

# One more thing to be noted is, we can't build a clear line for first plot due to use of all the predictors
# but we can plot for second model as only one predictor (which is hotelFriendly) is used.