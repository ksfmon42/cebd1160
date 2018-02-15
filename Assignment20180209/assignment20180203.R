getwd()
setwd("D:/Dated/BigDataCEBD1160/20180203/Assignments-20180209")
list.files()
install.packages("tidyverse") # To install dplyr
library(tidyverse) 
test_data <- read.csv("test.csv", sep=",")
train_data <- read.csv("train.csv", sep=",")
test_data <- as_data_frame(test_data)  # Use as_data_frame to ensure data type
train_data <- as_data_frame(train_data)
class(test_data) # Ensure type
class(train_data)
print(test_data) # Inspect data quickly
print(train_data)
View(test_data)  # Nicer view of the data
View(train_data)

# From this point I'm just going work on train_data, as I'm not sure what test_data is for since it misses the "Survived" column 

train_data<-distinct(train_data) # Remove duplicate rows based on entire row
train_data<-distinct(train_data,Passengerid) # Remove rows based on duplicate of passenger ID
count(filter(train_data, Survived!=0 & Survived!=1)) # Check for any values besides 0 or 1
count(filter(train_data, Survived==0)) # Total number of non survivers
count(filter(train_data, Survived==1)) # Total number of survivers
count(train_data) # Total number of passengers
summarise(train_data, survival=mean(Survived,na.rm=TRUE)) # Survival rate
traindata <- select(train_data, -c(Name,Ticket,Cabin))  # Simplify the data by removing unuseful columns
any(is.na(traindata$PassengerId))
names(traindata)[sapply(traindata, anyNA)] # Determine which columns have missing data, as it turns out Age is the only column.
summary(traindata)  # Global summary of all columns
by_sex <- group_by(traindata, Sex)
by_age <- group_by(traindata, Age)
by_sibsp <- group_by(traindata, SibSp)
by_parch <- group_by(traindata, Parch)
summarise(by_sex,mean(Survived))  # Suvival rate among each gender
summarise(by_sibsp, mean(Survived))
summarise(by_parch, mean(Survived))
survival_rate_by_age <- summarise(by_age, mean(Survived))
ggplot(data=survival_rate_by_age, mapping = aes(x=Age)) + geom_freqpoly(binwidth=3)
surviver_data=filter(traindata, Survived==1)
nonsurviver_data=filter(traindata, Survived==0)
View(nonsurviver_data)
plot(surviver_data$Age, surviver_data$Sibsp)
plot(nonsurviver_data$Age, nonsurviver_data$Sibsp)

# Plotting a single scatter plot for both survivers and non-survivers bewteen 2 variables 
plot(traindata$Age, traindata$Parch, col=traindata$Survived+2, pch=traindata$Survived+2) 

