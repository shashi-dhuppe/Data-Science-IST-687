##################################################################
# Homework 1 - Submitted by Shashikant Rajeshwar Dhuppe on September 5, 2018
# Portions of this code came from Introduction to Data Science
# but the comments are all original.

#Step A: Create a Vector

#1)	Define a vector 'grades', which contains the numbers 4.0, 3.3 and 3.7 (i.e., three numbers in the vector 'grades'). 

grades<-c(4.0,3.3,3.7) #Creating a vector and concatinating it with numeric values

#2)	Define a vector 'courseName', which contain the strings "Bio", "Math", "History".

courseName<-c('Bio','Math','History') #Creating a vector and concatinating it with textual values

#3)	Define a variable 'BetterThanB', that is equal to 3

BetterThanB<-3 #Declaring a variable and assigning a value 3

#Step B: Calculating statistics using R  

#4)	Compute the average of the grades vector with the mean() function

average=mean(grades) #Calculating average using inbuilt function mean()

#5)	Calculate the number of observations in the grades vector with the length() function, and store the result in the 
#variable 'total.length'

total.length<-length(grades) #Calculating average using inbuilt function mean()

#6)	Output the value of 'total.length'

print(total.length) #Printing the value of total.length calculated in the earlier step

#7)	Calculate the sum of  'grades' with the sum() function, store the result in 'total'.

total<-sum(grades) #Calculating sum using inbuilt function sum()

#8)	Recompute the average of all the grades by combining questions 5 and 7

averageReCompute<-total/total.length #Calculating the average by dividing the sum of elements by total number of elements

#Step C: Using the max/min functions in R

#9)	Compute the max grades, store the result in 'maxG'

maxG<-max(grades) #Calculating maximum using inbuilt function max()

#10)	Compute the min grades, store the results in 'minG'

minG<-min(grades) #Calculating mainimum using inbuilt function min()

#Step D: Vector Math

#11) Create a new vector called betterGrades, which is the grades + 0.3 (each grade improved each grade by  0.3 points)

betterGrades<-grades+0.3 #adding a numerical value to every element of given vector

#12)	 Compute the average of betterGrades

averageBetterGrades<-mean(betterGrades)

#Step E: Using Conditional if statements
 
#13)	 Test if maxG is greater than 3.5 (output "yes" or "no")

if(maxG>3.5) "yes" else "no" #Using conditional statement to check whether maxG is greater or 3.5

#14)	 Test if minG is greater than the variable 'BetterThanB'' (output "yes" or "no")

if(minG>BetterThanB) "yes" else "no" #Using conditional statement to check whether minG is greater or BetterThanB

#Step F: Accessing an element in a vector

#12)  Output the name of the second class, in the 'courseName' vector

print(courseName[2]) #accessing the value of an element of vector by using its index.
