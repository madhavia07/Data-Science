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
library(readr)
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

#### Check the distrubution of Labels ####
ggplot(mnist_train_data,aes(x = factor(V1),fill=factor(V1)))+geom_bar()
# Labels - V1(0-9) are proportionate and and between 0-9

ggplot(mnist_test_data,aes(x = factor(V1),fill=factor(V1)))+geom_bar()
# Labels - V1(0-9) are proportionate and and between 0-9 and similar to train data

#### Data analysis for train data#### 
mnist_train_data_1 = mnist_train_data[,-1]
dim(mnist_train_data_1)

#### With 785 Variables analysis is going to be timetaking and heavy, so we need to reduce the variables ####
var_obj = nearZeroVar(mnist_train_data_1,saveMetrics=TRUE, allowParallel = T)
var_obj$zeroVar
table(var_obj$zeroVar)##### 67 variables are available with Zero Variance
mnist_train_data_1 = mnist_train_data_1[ ,var_obj$zeroVar==FALSE]   
dim(mnist_train_data_1) # Reduced data set has 717 variables

#### Pre process data using caret library ####
preProcess_values <- preProcess(mnist_train_data_1, method = c("center", "scale"))
preProcess_values #Pre-processing: - centered (717) - ignored (0) - scaled (717)

mnist_train_data_trans <- predict(preProcess_values, mnist_train_data_1)
mnist_train_data_trans 

#### Add the label column back to data
mnist_train_data_trans$V1 = mnist_train_data$V1 # Now this is used for SVM analysis


#### 3.2 Sample the train data into smaller chunks for easy execution i.e 30% of the data ####
set.seed(1)
train.indices = sample(1:nrow(mnist_train_data_trans), 0.3*nrow(mnist_train_data_trans))
train = mnist_train_data_trans[train.indices, ]
train.indices2 = sample(1:nrow(mnist_test_data), 0.3*nrow(mnist_test_data))
test = mnist_test_data[train.indices2, ]
#### 4. Model Building ####
# Started with Linear and Polynomila then RBF model

####  4.1 Linear SVM Model ####
#Using Linear Kernel
Model_linear_mnist <- ksvm(V1~ ., data = train, scale = FALSE, kernel = "vanilladot")
Eval_linear_mnist <- predict(Model_linear_mnist, test)
#confusion matrix - Linear Kernel
confusionMatrix(Eval_linear_mnist,test$V1)
#Accuracy - 82.37 %
############ 4.1.1 Hyperparameter tuning and Cross Validation for Linear SVM Model #####################
trainControl <- trainControl(method="cv", number=5)
# Evaluation metric is Accuracy
metric <- "Accuracy"
#Expand.grid functions takes set of hyperparameters, that we shall pass to our model.
set.seed(1)
# making a grid of C values. 
grid <- expand.grid(C=seq(1, 5, by=1))
#### Performing 5-fold cross validation for Linear SVM ####
fit.svm <- train(V1~., data=train, method="svmLinear", metric=metric, 
                 tuneGrid=grid, trControl=trainControl) 
# Print Linear SVM fit
print(fit.svm)
# Best tune at C=1, 
# Accuracy - 91.18%
# Plotting fit.svm results for Linear SVM
plot(fit.svm)

####  4.2  Polynomial SVM Model ####

#### Sample the train data into smaller chunks for easy execution i.e 10% of the data ####
set.seed(1)
train.indices = sample(1:nrow(mnist_train_data_trans), 0.1*nrow(mnist_train_data_trans))
train = mnist_train_data_trans[train.indices, ]
train.indices2 = sample(1:nrow(mnist_test_data), 0.1*nrow(mnist_test_data))
test = mnist_test_data[train.indices2, ]

#Using Polynomial Kernel
Model_Poly_mnist <- ksvm(V1~ ., data = train, scale = FALSE, kernel = "rbfdot")
Eval_Poly_mnist<- predict(Model_Poly_mnist, test)
#confusion matrix - Polynomial Kernel
confusionMatrix(Eval_Poly_mnist,test$V1)
############ 4.3.1 Hyperparameter tuning and Cross Validation for Polynomial SVM Model #####################
trainControl <- trainControl(method="cv", number=5)
# Evaluation metric is Accuracy
metric <- "Accuracy"
#Expand.grid functions takes set of hyperparameters, that we shall pass to our model.
set.seed(1)
grid <- expand.grid(.degree= 2, .scale=c(0.1,0.5,1,2), .C=c(0.1,0.5,1,2) )
#### Performing 5-fold cross validation for Polynomial SVM ####
fit.svm <- train(V1~., data=train, method="svmPoly", metric=metric, 
                 tuneGrid=grid, trControl=trainControl)
# Print Polynomial SVM fit
print(fit.svm)
# Sigma & C plot for Polynomial SVM
plot(fit.svm)

####  4.3  Radial Basis Function SVM Model ####

#### Sample the train data into smaller chunks for easy execution i.e 10% of the data ####
set.seed(1)
train.indices = sample(1:nrow(mnist_train_data_trans), 0.1*nrow(mnist_train_data_trans))
train = mnist_train_data_trans[train.indices, ]
train.indices2 = sample(1:nrow(mnist_test_data), 0.1*nrow(mnist_test_data))
test = mnist_test_data[train.indices2, ]

#Using Radial Basis Function Kernel
#Using RBF Kernel
Model_RBF_mnist <- ksvm(V1~ ., data = train, scale = FALSE, kernel = "rbfdot")
Eval_RBF_mnist<- predict(Model_RBF_mnist, test)
#confusion matrix - RBF Kernel
confusionMatrix(Eval_RBF_mnist,test$V1) 
############ 4.3.1 Hyperparameter tuning and Cross Validation for RBF SVM Model #####################
trainControl <- trainControl(method="cv", number=5)
# Evaluation metric is Accuracy
metric <- "Accuracy"
#Expand.grid functions takes set of hyperparameters, that we shall pass to our model.
set.seed(1)
grid <- expand.grid(.degree= 2, .scale=c(0.1,0.5,1,2), .C=c(0.1,0.5,1,2) )
#### Performing 5-fold cross validation for RBF SVM ####
fit.svm <- train(V1~., data=train, method="svmRadial", metric=metric, 
                 tuneGrid=grid, trControl=trainControl)
# Print RBF SVM fit
print(fit.svm)
# Sigma & C plot for RBF SVM
plot(fit.svm)

#### After comparing the 3 SVM models RBF is the suitable model.

#### END OF ASSIGNMENT ####
