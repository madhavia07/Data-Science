#### Clear the console ####
rm(list = ls()) 

#### Course 4 | Module 3 - SVM - Handwritten Digit Recognition Assignment ####
#### Individual Assignment - 1
#### Name: Frudhvi Madhavi Ayyagari | E- madhavi.a07@gmail.com ####

#### Set working directory to the source file location #### 

#### 1. Business Understanding ####
# A classic problem in the field of pattern recognition is that of handwritten digit recognition. 
# Suppose that you have an image of a digit submitted by a user via a scanner, a tablet, or other digital devices.
# The goal is to develop a model that can correctly identify the digit (between 0-9) written in an image. 

#### 2. Data Understanding ####
# Data given is in two file ones as train dataset and other test data set 
# First columns of the data set being the labels (0-9)

#### 3. Data Preparation ####

# Loading Neccessary libraries
library(kernlab)
library(readr)
library(caret)
library(caTools)
library(dplyr)
library(ggplot2)
library(gridExtra)

#### EDA for Train Data ####

# Loading Train Data
mnist_train_data <- read.csv("mnist_train.csv", header=FALSE) # No header data given

#Understanding Dimensions
dim(mnist_train_data)  # 60000 obs. of 785 variables

#Structure of the dataset
str(mnist_train_data) # All are integers

#Exploring the data
summary(mnist_train_data)

#Checking missing value
sapply(mnist_train_data, function(x) sum(is.na(x)))  # No missing values

#Making our Label to factor
mnist_train_data$V1<-factor(mnist_train_data$V1)

#### EDA for Test Data ####

#Loading test Data
mnist_test_data <- read.csv("mnist_test.csv", header=FALSE)

#Understanding Dimensions
dim(mnist_test_data) # 10000 obs. of 785 variables
#Structure of the dataset
str(mnist_test_data)
#Exploring the data
summary(mnist_test_data)
#Checking missing value
sapply(mnist_test_data, function(x) sum(is.na(x)))  # No missing values
#Making our target class to factor
mnist_test_data$V1<-factor(mnist_test_data$V1)

mnistTest <- mnist_test_data

#### Check the distrubution of Labels ####
ggplot(mnist_train_data,aes(x = factor(V1),fill=factor(V1)))+geom_bar()
# Labels - V1(0-9) are proportionate and and between 0-9

ggplot(mnist_test_data,aes(x = factor(V1),fill=factor(V1)))+geom_bar()
# Labels - V1(0-9) are proportionate and and between 0-9 and similar to train data

#### Data analysis for train data#### 
mnistTrain<- mnist_train_data
dim(mnistTrain)

#### 4. Mean analysis ####
# let us create a new feature called Mean by taking mean of each row and
# Plot the Mean of each label (V1 to V9)
mnistTrain$mean <- apply(mnistTrain[,-1], 1, mean) 

MeanofLabels <- aggregate (mnistTrain$mean, by = list(mnistTrain$V1), FUN = mean)

plot <- ggplot(data=MeanofLabels, aes(x=Group.1, y = x)) +
  geom_bar(stat="identity")
plot + scale_x_discrete(limits=0:9) + xlab("Label") + 
  ylab("Weighted Mean")


#Let us plot the intensity distribution of all labels 
L0 <- qplot(subset(mnistTrain, V1 ==0)$mean, binwidth = .75, 
            xlab = "Mean Histogram for 0")

L1<- qplot(subset(mnistTrain, V1 ==1)$mean, binwidth = .75,
           xlab = "Mean Histogram for 1")

L2 <- qplot(subset(mnistTrain, V1 ==2)$mean, binwidth = .75,
            xlab = "Mean Histogram for 2")

L3 <- qplot(subset(mnistTrain, V1 ==3)$mean, binwidth = .75,
            xlab = "Mean Histogram for 3")

L4 <- qplot(subset(mnistTrain, V1 ==4)$mean, binwidth = .75, 
            xlab = "Mean Histogram for 4")

L5 <- qplot(subset(mnistTrain, V1 ==5)$mean, binwidth = .75,
            xlab = "Mean Histogram for 5")

L6 <- qplot(subset(mnistTrain, V1 ==6)$mean, binwidth = .75,
            xlab = "Mean Histogram for 6")

L7 <- qplot(subset(mnistTrain, V1 ==7)$mean, binwidth = .75,
            xlab = "Mean Histogram for 7")

L8 <- qplot(subset(mnistTrain, V1 ==8)$mean, binwidth = .75,
            xlab = "Mean Histogram for 8")

L9 <- qplot(subset(mnistTrain, V1 ==9)$mean, binwidth = .75,
            xlab = "Mean Histogram for 9")

grid.arrange(L0,L1 ,L2, L3,L4,L5,L6,L7,L8,L9)

print('Combined histograms of all the labels(0-9) imply varied data points and thus wee need to check for RBF model but first lets check for linear and proceed further with confirmation.')


####  5. Model building ####
mnistTrain$mean<-NULL

####  5.1 Linear SVM Model ####
#Using Linear Kernel 
Model_linear <- ksvm(V1~ ., data = mnistTrain, kernel = "vanilladot")
Eval_linear<- predict(Model_linear, mnistTest)

####  Confusion matrix - Linear Kernel #### 
confusionMatrix(Eval_linear,mnistTest$V1)

#### 5.2 RBF Model ####
#Using RBF Kernel
Model_RBF <- ksvm(V1~ ., data = mnistTrain, scale = FALSE, kernel = "rbfdot")
Eval_RBF<- predict(Model_RBF, mnistTest)

####  Confusion matrix - RBF Kernel #### 
confusionMatrix(Eval_RBF,mnistTest$V1)

#### Continuing with RBF model for Hyperparameter tuning and CV as the accuracy percentage is higher than linear model. ####
print('Continuing with RBF model for Hyperparameter tuning and CV as the accuracy percentage is higher than linear model.')

#### 6. Hyperparameter tuning and Cross Validation ####

#### Defining train control for Cross validation ####
trainControl <- trainControl(method="cv", number=5)

metric <- "Accuracy" # Evaluation metric is Accuracy

#Expand.grid functions takes set of hyperparameters, that we shall pass to our model.
set.seed(7)
grid <- expand.grid(.sigma=c(0.015 ,0.025, 0.05), .C=c(0.1,0.5,1,2) )

#### Performing 5-fold cross validation for RBF SVM ####
fit.svm <- train(V1~., data=mnistTrain, method="svmRadial", metric=metric, 
                 tuneGrid=grid, trControl=trainControl)

print(fit.svm)


#### Constructing final model with sigma(gamma) = 0.05 and Cost(c) =0.1 ####
Final_model <- ksvm(V1~ ., data = mnistTrain, scale = FALSE, gamma=0.05,cost=0.1 ,kernel = "rbfdot")
Eval_Final_model<- predict(Final_model, mnistTest)

confusionMatrix(Eval_Final_model,mnistTest$V1)

#### Conclusion ####
# After tuning the final suitable model chosen is an RBF model with gamma=0.05 and C=0.1 and 92.2 % accuracy
print('After tuning the final suitable model chosen is an RBF model with gamma=0.05 and C=0.1 and 92.2 % accuracy')

#### END OF ASSIGNMENT ####
