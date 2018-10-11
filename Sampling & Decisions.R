# Homework 4 - Submitted by Shashikant R. Dhuppe on September 26, 2018
# Portions of this code came from Introduction to Data Science
# but the comments are all original.

# Part A: Write a function to reveal the distribution of a vector of numeric values

# 1.	Create a new function 'printVecInfo' and have it take one numeric vector as its input argument.

printVecInfo<-function(vec){ #Defining a function which takes one numeric vector
}

# 2.	Make the function print the following information for the vector supplied in the argument:

# a.	Mean

printVecInfo<-function(vec){ #Defining a function which takes one numeric vector
  cat(mean(vec),"\n") #Calculating Mean
}

# b.	Median

printVecInfo<-function(vec){ #Defining a function which takes one numeric vector
  cat(mean(vec),"\n") #Calculating Mean
  cat(median(vec),"\n") #Calculating Median
}

# c.	Min & Max

printVecInfo<-function(vec){ #Defining a function which takes one numeric vector
  cat(mean(vec),"\n") #Calculating Mean
  cat(median(vec),"\n") #Calculating Median
  cat(max(vec)," ",min(vec),"\n") #Calculating Maximum and Minimum
}

# d.	Standard deviation

printVecInfo<-function(vec){ #Defining a function which takes one numeric vector
  cat(mean(vec),"\n") #Calculating Mean
  cat(median(vec),"\n") #Calculating Median
  cat(max(vec)," ",min(vec),"\n") #Calculating Maximum and Minimum
  cat(sd(vec),"\n") #Calculating standard deviation
}

# e.	0.05 and 0.95 quantiles (Use the quantile( ) function) 

printVecInfo<-function(vec){ #Defining a function which takes one numeric vector
  cat(mean(vec),"\n") #Calculating Mean
  cat(median(vec),"\n") #Calculating Median
  cat(max(vec)," ",min(vec),"\n") #Calculating Maximum and Minimum
  cat(sd(vec),"\n") #Calculating standard deviation
  print(quantile(vec,probs = c(0.05,0.95))) #Calculating Quantiles for 5% and 95%
}

# 3.	Test the function with this vector: testVector <- 1:10. Results should look something like this:

testVector<-c(1:10) #an example vector to test our functions

#   [1] 5.5
# [1] 10
# [1] 1
# [1] 3.02765
# 5%  95% 
#   1.45 9.55

printVecInfo(testVector) #Using the defined function on test vector

# 4.	 Add labels to each element of the function's output.

printVecInfo<-function(vec){
  
  #labelling the respective functions using concatinating function
  
  cat("Mean:",mean(vec),"\n") 
  cat("Median: ",median(vec),"\n")
  cat("Min:",min(vec),"\nMax:",max(vec),"\n")
  cat("SD:",sd(vec),"\n")
  cat("Quantiles:\n")
  print(quantile(vec,probs = c(0.05,0.95)))
}

printVecInfo(testVector) #Displaying all values along with their labels

# Part B: Read the census dataset

# 5.	Read in the Census dataset

#Reading the url of given Census dataset

dfstates<-read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2017/state/asrh/scprc-est2017-18+pop-res.csv", header= TRUE, sep="," , stringsAsFactors = FALSE)

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

dfStatesCleaned<-clean_data() #storing cleaned dataset into a different vector

# Part C: Sample from the state population data frame
# 6.	Sample 20 observations from states$population 
#and use printVecInfo( ) to display the characteristics of the resulting sample, 
#and then display the results as a histogram.

sampled_df<-sample( dfStatesCleaned$population ,size = 20,replace = TRUE) #sampling 20 observations

printVecInfo(sampled_df) #Displaying the results of sampled vector

hist(sampled_df) #Histogram of sampled vector
 
# 7.	Repeat step 6 two more times. 
#Each time that you create a sample, run the resulting vector through printVecInfo( ) and create a histogram. 

#Running the sampling second time

sampled_df<-sample( dfStatesCleaned$population ,size = 20,replace = TRUE)

printVecInfo(sampled_df)

hist(sampled_df)

#Running the sampling third time

sampled_df<-sample( dfStatesCleaned$population ,size = 20,replace = TRUE)

printVecInfo(sampled_df)

hist(sampled_df)

# 8.	Using a block comment, explain in a comment why each result is different.

## The sample function runs on probability of finding each occurence in the given set n times.
## So, when the size is 20, it will take one random sample out of population, 20 times, without replacing
## so each time, we used sample function, it will show different stats as each time probability changes

# Part D: Replicate the sampling

# 9.	Use the replicate function, to replicate the sampling (described in step 6 above). 
#Replicate the sampling 2000 times, 
#then use printVecInfo( ) to display the characteristics of the resulting replicated sample, 
#and then display the results as a histogram.

replicaed_df<-replicate(2000,sample( dfStatesCleaned$population ,size = 51,replace = TRUE)) 
#Replicating the sample 2000 times with the size of 51

printVecInfo(replicaed_df) #Displaying the results of replicated sample

hist(replicaed_df) #Histogram of replicated sample
 
# 10.	Repeat step 9 two more times. Each time that you create the replicated sample, run the resulting vector through printVecInfo( ) and create a histogram. 

#Replicating for second time

replicaed_df<-replicate(2000,sample( dfStatesCleaned$population ,size = 51,replace = TRUE))

printVecInfo(replicaed_df)

hist(replicaed_df)

#Replicating for third time

replicaed_df<-replicate(2000,sample( dfStatesCleaned$population ,size = 51,replace = TRUE))

printVecInfo(replicaed_df)

hist(replicaed_df)

# 11.	 Using a block comment,  
#explain why the histograms generated in Part C are different than the histograms generated in Part D

## Replicate function runs the sample function with given number of times
## So, as each time replicate function runs, it runs the inside sample function with n times
## The reason for each sample being different is,
## The sample function runs on probability of finding each occurence in the given set n times.
## So, when the size is 20, it will take one random sample out of population, 20 times, without replacing the taken out item
## so each time, we used sample function, it will show different stats as each time probability changes
