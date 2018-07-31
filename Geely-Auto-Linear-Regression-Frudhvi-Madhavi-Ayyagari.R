#-----------------------------------------------------------------------------
# Course 3 - Predictive Analytics I | Module 2 - Linear Regression Assignment
# Individual Assignment - Linear Regression - Geely Auto
# Name: Frudhvi Madhavi Ayyagari | E- madhavi.a07@gmail.com 
#-----------------------------------------------------------------------------

rm(list = ls()) 

#-----------------------------------------------------------------------------
# STEP 1: IMPORT THE CAR PRICE DATA SET TO R 
#-----------------------------------------------------------------------------

#Set working directory to source file location
#setwd("D:/Course 3/Linear Regression/Assignment") 

# Loading the required libraries

library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(DT)
library(tidyr)
library(scales)
#install MASS ans Car packages

library(MASS)
library(car)


CarPrice_Assignment <- read.csv("CarPrice_Assignment.csv", header = TRUE, sep = ",", stringsAsFactors = F)


#-----------------------------------------------------------------------------
# STEP 1: DATA CLEANING AND EDA
#-----------------------------------------------------------------------------

# Checking for duplicate Car Ids
duplicated(CarPrice_Assignment$car_ID)
sum(duplicated(CarPrice_Assignment$car_ID)) 
# ZERO duplicate IDs

# Checking for missing values in the data set
sum(is.na(CarPrice_Assignment)) 
# NO missing values

# Checking for blank values in the data set
sapply(CarPrice_Assignment, function(x) length(which(x == ""))) 
# ZERO blank values

# Check structure
str(CarPrice_Assignment) 

# Now seperating Car company and model from Car Name
CarPrice_Assignment$car_company <- lapply(strsplit(as.character(CarPrice_Assignment$CarName), "\\ "), "[", 1)
CarPrice_Assignment$car_model <- lapply(strsplit(as.character(CarPrice_Assignment$CarName), "\\ "), "[", 2)
str(CarPrice_Assignment)

# Convert to char to factor and lower case values
CarPrice_Assignment$CarName <- factor(tolower(CarPrice_Assignment$CarName))
CarPrice_Assignment$car_company <- factor(tolower(CarPrice_Assignment$car_company))
CarPrice_Assignment$car_model <- factor(tolower(CarPrice_Assignment$car_model))
CarPrice_Assignment$fueltype <- factor(tolower(CarPrice_Assignment$fueltype))
CarPrice_Assignment$aspiration <- factor(tolower(CarPrice_Assignment$aspiration))
CarPrice_Assignment$doornumber <- factor(tolower(CarPrice_Assignment$doornumber))
CarPrice_Assignment$carbody <- factor(tolower(CarPrice_Assignment$carbody))
CarPrice_Assignment$drivewheel <- factor(tolower(CarPrice_Assignment$drivewheel))
CarPrice_Assignment$enginelocation <- factor(tolower(CarPrice_Assignment$enginelocation))
CarPrice_Assignment$enginetype <- factor(tolower(CarPrice_Assignment$enginetype))
CarPrice_Assignment$cylindernumber <- factor(tolower(CarPrice_Assignment$cylindernumber))
CarPrice_Assignment$fuelsystem <- factor(tolower(CarPrice_Assignment$fuelsystem))

str(CarPrice_Assignment) 

# Setting header names to lower case
setnames(CarPrice_Assignment, tolower(names(CarPrice_Assignment[1:28])))
str(CarPrice_Assignment)

#check carcompany & car model
summary(CarPrice_Assignment$car_company)
CarPrice_Assignment$car_company <- replace(CarPrice_Assignment$car_company, CPA$car_company=="maxda", "mazda")
CarPrice_Assignment$car_company <- replace(CarPrice_Assignment$car_company, CPA$car_company=="porcshce", "porsche")
CarPrice_Assignment$car_company <- replace(CarPrice_Assignment$car_company, CPA$car_company=="vokswagen", "volkswagen")
CarPrice_Assignment$enginetype <- replace(CarPrice_Assignment$enginetype, CarPrice_Assignment$enginetype=="dohcv", "dohc")


# Removing the carname column
CPA <- subset( CarPrice_Assignment, select = -3)

# Check for missing values once again
sum(is.na(CPA)) # 2 values
CPA <- CPA [-which(is.na(CPA$car_model)), ]

# Final cleaned dataset to be used for analysis
CPA
str(CPA)

#-----------------------------------------------------------------------------
# STEP 2: DUMMY VARIABLE CREATION
#-----------------------------------------------------------------------------

str(CPA$fueltype)
summary(factor(CPA$fueltype))
levels(CPA$fueltype)<-c(1,0)
# Now store the numeric values in the same variable
CPA$fueltype<- as.numeric(levels(CPA$fueltype))[CPA$fueltype]
# Check the summary of fuel type variable
summary(CPA$fueltype)

#-------------------------------------

str(CPA$aspiration)
summary(factor(CPA$aspiration))
levels(CPA$aspiration)<-c(1,0)
# Now store the numeric values in the same variable
CPA$aspiration<- as.numeric(levels(CPA$aspiration))[CPA$aspiration]

#-------------------------------------

str(CPA$doornumber)
summary(factor(CPA$doornumber))
levels(CPA$doornumber)<-c(1,0)
# Now store the numeric values in the same variable
CPA$doornumber<- as.numeric(levels(CPA$doornumber))[CPA$doornumber]

#-------------------------------------

str(CPA$enginelocation)
summary(factor(CPA$enginelocation))
levels(CPA$enginelocation)<-c(1,0)
# Now store the numeric values in the same variable
CPA$enginelocation<- as.numeric(levels(CPA$enginelocation))[CPA$enginelocation]

#-------------------------------------
# DUMMY FOR VARIABLES WITH MULTIPLE LEVELS
#-------------------------------------


str(CPA$carbody)
summary(factor(CPA$carbody))
# Now car body has 5 levels 
#Converting into dummies . 
dummy_1 <- data.frame(model.matrix( ~carbody, data = CPA))
#check the dummy_1 data frame.
View(dummy_1)
#intercept column to be removed 
dummy_1 <- dummy_1[,-1]
# Combine the dummy variables to the main data set, after removing the original categorical variable column
CPA <- cbind(CPA[,-6], dummy_1)
View(CPA)

#-------------------------------------

str(CPA$drivewheel)
summary(factor(CPA$drivewheel))
# Now drivewheel has 3 levels 
#Converting into dummies . 
dummy_1 <- data.frame(model.matrix( ~drivewheel, data = CPA))
#check the dummy_1 data frame.
#intercept column to be removed 
dummy_1 <- dummy_1[,-1]
# Combine the dummy variables to the main data set, after removing the original categorical variable column
CPA <- cbind(CPA[,-6], dummy_1)

#-------------------------------------

str(CPA$enginetype)
summary(factor(CPA$enginetype))
# Now engine type has multi level 
#Converting into dummies . 
dummy_1 <- data.frame(model.matrix( ~enginetype, data = CPA))
#check the dummy_1 data frame.
#intercept column to be removed 
dummy_1 <- dummy_1[,-1]
# Combine the dummy variables to the main data set, after removing the original categorical variable column
CPA <- cbind(CPA[,-12], dummy_1)

#-------------------------------------

str(CPA$cylindernumber)
summary(factor(CPA$cylindernumber))
# Now cylindernumber has 7 levels 
#Converting into dummies . 
dummy_1 <- data.frame(model.matrix( ~cylindernumber, data = CPA))
#check the dummy_1 data frame.
#intercept column to be removed 
dummy_1 <- dummy_1[,-1]
# Combine the dummy variables to the main data set, after removing the original categorical variable column
CPA <- cbind(CPA[,-12], dummy_1)

#-------------------------------------

str(CPA$fuelsystem)
summary(factor(CPA$fuelsystem))
# Now fuel system has 8 levels 
#Converting into dummies . 
dummy_1 <- data.frame(model.matrix( ~fuelsystem, data = CPA))
#check the dummy_1 data frame.
#intercept column to be removed 
dummy_1 <- dummy_1[,-1]
# Combine the dummy variables to the main data set, after removing the original categorical variable column
CPA <- cbind(CPA[,-13], dummy_1)

#-------------------------------------

str(CPA$car_company)
summary(factor(CPA$car_company))
# Now car company has 27 levels 
#Converting into dummies . 
dummy_1 <- data.frame(model.matrix( ~car_company, data = CPA))
#check the dummy_1 data frame.
#intercept column to be removed 
dummy_1 <- dummy_1[,-1]
# Combine the dummy variables to the main data set, after removing the original categorical variable column
CPA <- cbind(CPA[,-21], dummy_1)

#-------------------------------------

str(CPA$car_model)
summary(factor(CPA$car_model))
# Now car model has 114 levels 
#Converting into dummies . 
dummy_1 <- data.frame(model.matrix( ~car_model, data = CPA))
#check the dummy_1 data frame.
#intercept column to be removed 
dummy_1 <- dummy_1[,-1]
# Combine the dummy variables to the main data set, after removing the original categorical variable column
CPA <- cbind(CPA[,-21], dummy_1)

View(CPA)

#-----------------------------------------------------------------------------
# STEP 3: MODEL BUILDING AND EVALUATION
#-----------------------------------------------------------------------------

# Divide into training and test data set
#set the seed to 100, let's run it 
set.seed(100)

# randomly generate row indices for dataset
trainindices= sample(1:nrow(CPA), 0.7*nrow(CPA))
# generate the data set
train = CPA[trainindices,]

#Similarly store the rest of the observations into an object "test".
test = CPA[-trainindices,]

train
test

#Execute the first model_1 multilinear model in the training set. 
model_1 <- lm(price~., data = train)

alias( lm(price~., data = train) )

# remove insignificant variables using stepAIC
step <- stepAIC(model_1,direction = "both")
step

corrs = cor(model_1)
View(corrs)

# Pass the model_1 in the vif function
vif(model_1)

# Check if the correlation matrix givessome insight.
corrs = cor(step)
View(corrs)

# Look at summary of the model again to see the P values
summary(model_1)
