# Step A: Initialize an 'arrests' dataframe

# 1)	Copy USArrests into a new variable (called 'arrests')

arrests<-USArrests #assigning value from inbuilt dataset

# Step B: Explore the assault rate

# 2)	Write a comment: Is a higher or lower assault rate best?

#Lower assualt rate is best

# 3)	Which state has the best assault rate? 

minAssault<- min(arrests$Assault) #finding minimum in the given column

print(row.names(subset(arrests, arrests$Assault == minAssault ))) #Printing only the name of the state

# Step C: Explore the murder rate

# 4)	Which state has the highest murder rate?

maxMurder<- max(arrests$Murder) #Finding maximum in the given column

MurderState<-subset(arrests, arrests$Murder == maxMurder) #reducing the data frame to given condition
print(MurderState[1,1, drop=FALSE]) #printing state along with its murder rate

# 5)	Create a sorted dataframe, based on descending murder rate

arrestsSorted<- arrests[order(-arrests$Murder),] #sorting in descending rate

# 6)	Show the 10 states with the highest murder rate

print(arrestsSorted[1:10,]) #printing the only 10 states

# 7)	What is the value of the 20'th row, third column (in the sorted dataframe)? Use R code (not visual inspection)

print(arrestsSorted[20,3]) #refrencing the given value

# Step D: Which state is the least safe? Explain your logic

# 8)	Write the R code to determine your answer

arrestsN<-arrests

arrestsN$Murder<-(arrests$Murder - min(arrests$Murder))/(max(arrests$Murder)-min(arrests$Murder)) #normalising
arrestsN$Assault<-(arrests$Assault - min(arrests$Assault))/(max(arrests$Assault)-min(arrests$Assault)) #normalising
arrestsN$Rape<-(arrests$Rape - min(arrests$Rape))/(max(arrests$Rape)-min(arrests$Rape))#normalising

arrestsN$Lsafeindex<-arrestsN$Murder + arrestsN$Assault + arrestsN$Rape #adding these metrics

arrestsN<-arrestsN[order(-arrestsN$Lsafeindex),] #creating a new column and assigning all the values

print(row.names(arrestsN[1,])) #printing the name of the state

# 9)	Write a comment to explain your logic

# I have changed the range to include 0 to 1. By using the value, (given value - min value)/(max value - min value)
# So, using these metrics, I have brought these into same metric.
# Then, by adding these we get least safe index
# Descending the data frame by least safe index gives the state with least safe index.