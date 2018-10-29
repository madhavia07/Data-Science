#### Domain Elective | Module 4 ####
#### Recommendation System Assignment ####
#### Madhavi Ayyagari | Roll No: DDA1720282  ####

#### Clear the console ####
rm(list = ls()) 
# Set working directory

#Install Libraries
library(recommenderlab)
library(dplyr)
library(ggplot2)

#### Loading data and data cleaning ####
beer_data <- read.csv("beer_data.csv", header=TRUE) 
summary(beer_data)

# Removing rows where Reviewer name is empty
nrow(beer_data[(is.na(beer_data$review_profilename) | beer_data$review_profilename==""), ]) # There are 100 rows with no reviewer name
beer_data<-beer_data[!(beer_data$review_profilename==""), ]
nrow(beer_data[(is.na(beer_data$review_profilename) | beer_data$review_profilename==""), ]) # We have removed the rows with empty reviewer name

str(beer_data)

# Removing rows having both beer & user duplicated
# Reference using Dplyr (https://stackoverflow.com/questions/22959635/remove-duplicated-rows-using-dplyr)

beer_data[!(duplicated(beer_data[c("beer_beerid","review_profilename")]) | duplicated(beer_data[c("beer_beerid","review_profilename")], fromLast = TRUE)), ] %>% nrow()
# 473055
beer_data %>% distinct(beer_beerid,review_profilename,.keep_all = TRUE) %>% nrow()
# 474462
beer_data <- distinct(beer_data,beer_beerid,review_profilename,.keep_all = TRUE)

#### 1. Data preparation ####
#### 1.1 Choose only those beers that have at least N number of reviews. ####
#        Figure out an appropriate value of N using EDA; this may not have one correct answer, 
#        but you shouldn't choose beers having extremely low number of ratings 

# No. of Distinct reviewers 
beer_data %>% group_by(review_profilename) %>% summarise(total_user_reviews=n()) %>% nrow() # 22497

# Distinct beers & total reviews they received
beer_reviews_count <- beer_data %>% group_by(beer_beerid) %>% summarise(total_beer_reviews=n())
beer_reviews_count[with(beer_reviews_count, order(total_beer_reviews,decreasing = TRUE)), ]
summary(beer_reviews_count)

# Finding ideal value of N, by choosing beers with more no. of reviews
qplot(beer_reviews_count$total_beer_reviews, geom = "histogram", binwidth = 50, xlab="Total Reviews")

# No. of beers with only 1 reviews
beer_reviews_count %>% subset(total_beer_reviews==1) %>% dim() # 18080
# No. of beers with only 2 reviews
beer_reviews_count %>% subset(total_beer_reviews==2) %>% dim()

# Lets draw the matrix for the reviews 
review_frequency<-beer_reviews_count %>% group_by(total_beer_reviews) %>% summarise(review_occurance=n())
summary(review_frequency)

# Filtering by beers having atleast 50 reviews & users having reviewed atleast 30 beers as there are more distinct beers(40308)
beer_reviews_count_subset<-subset(beer_reviews_count,beer_reviews_count$total_beer_reviews>=50)
ggplot(beer_reviews_count_subset,aes(x=total_beer_reviews)) + geom_bar()

# Lets also filter beer dataset based on users who have atleast reviewd 30 beers each
user_reviews_count<- beer_data %>% group_by(review_profilename) %>% summarise(total_user_reviews=n())
user_reviews_count_subset<-subset(user_reviews_count,user_reviews_count$total_user_reviews>=30)
ggplot(user_reviews_count_subset,aes(x=total_user_reviews)) + geom_bar()

# Now lets filter original data by these beer and user ids

selected_beers<-merge(beer_data,beer_reviews_count_subset,by.x="beer_beerid",by.y="beer_beerid")
selected_beers<-merge(selected_beers,user_reviews_count_subset,by.x="review_profilename",by.y="review_profilename")
summary(selected_beers)

#### 1.2 Convert this data frame to a "realratingMatrix" before you build your collaborative filtering models ####
beer_rating_matrix <- as(selected_beers, "realRatingMatrix")
class(beer_rating_matrix)

# get some informtaion
dimnames(beer_rating_matrix)
rowCounts(beer_rating_matrix)
colCounts(beer_rating_matrix)
rowMeans(beer_rating_matrix)

# coerce the matrix to a dataframe
beer_rating_df <- as(beer_rating_matrix, "data.frame")
str(beer_rating_df)

#### 2. Data Exploration ####
#### 2.1 Determine how similar the first ten users are with each other and visualise it ####

similarity_10users <- similarity(beer_rating_matrix[1:10,],method = "cosine",which = "users")
# Similarity matrix for users
(as.matrix(similarity_10users))
# Visualise similarity matrix for users
image(as.matrix(similarity_10users), main = "User similarity")

#### 2.2 Compute and visualise the similarity between the first 10 beers 
similarity_10users2 <- similarity(beer_rating_matrix[,1:10],method = "cosine",which = "items")
# Similarity matrix for users
(as.matrix(similarity_10users2))
# Visualise similarity matrix - For the first 10 beers
image(as.matrix(similarity_10users2), main = "User similarity")

#### 2.3 What are the unique values of ratings?  #### 
beer_rating_df %>% group_by(rating) %>% summarise(rating_frequency=n()) %>% nrow()  # 9 unique ratings
beer_rating_df %>% group_by(rating) %>% summarise(rating_frequency=n()) # Here is the rating chart
# Most given ratings are 4.0 & 4.5 and least given ratings are 1.0 & 1.5

#### 2.4 Visualise the rating values and notice:  #### 

#### 2.4.1 The average beer ratings ####
avg_ratings_beer<-beer_rating_df %>% group_by(item) %>% summarise(average_rating=mean(rating))
summary(avg_ratings_beer)
ggplot(avg_ratings_beer,aes(x=average_rating)) + geom_histogram() + labs(x="Average Rating", y="Number of Beers") + scale_x_discrete(limits=1:5)


#### 2.4.2 The average user ratings ####
avg_ratings_user<-beer_rating_df %>% group_by(user) %>% summarise(average_rating=mean(rating))
summary(avg_ratings_beer)
ggplot(avg_ratings_user,aes(x=average_rating)) + geom_histogram() + labs(x="Average Rating", y="Number of Users")

#### 2.4.3 The average number of ratings given to the beers ####
avg_reviews_beer<-selected_beers %>% group_by(beer_beerid) %>% summarise(average_reviews=mean(total_beer_reviews))
ggplot(avg_reviews_beer,aes(x=average_reviews)) + geom_histogram() + labs(x="Average Rating", y="Number of Beers")

#### 2.4.4 The average number of ratings given by the users ####
avg_reviews_user<-selected_beers %>% group_by(review_profilename) %>% summarise(average_reviews=mean(total_user_reviews))
ggplot(avg_reviews_user,aes(x=average_reviews)) + geom_histogram()

#### 3 Recommendation Models ####

#List of models available
recommender_models <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
names(recommender_models)# 9 types of models

#description of recommendation system algorithms/models used
lapply(recommender_models, "[[", "description")

#This gives you different types of recommendation models
#In this case study , let's compare user based and item based
#collaborative filtering

#checking the parameters of these two models
recommender_models$IBCF_realRatingMatrix$parameters

#### 3.1 Divide your data into training and testing datasets ####
# Experiment with 'split' and 'cross-validation' evaluation schemes

scheme_split <- evaluationScheme(beer_rating_matrix, method = "split", 
                                 train = .75,
                                 k = 1, 
                                 given = -1, 
                                 goodRating = 4)
scheme_split

scheme_CV <- evaluationScheme(beer_rating_matrix, method = "cross-validation", 
                              k = 5, 
                              given = -1, 
                              goodRating = 4)
scheme_CV

#### 3.2 Build IBCF and UBCF models ####
algorithms <- list(
  "user-based CF" = list(name="UBCF", param=list(normalize = "Z-score",
                                                 method="Cosine",
                                                 nn=30)),
  "item-based CF" = list(name="IBCF", param=list(normalize = "Z-score"
  ))
)

#### 3.3 Compare the performance of the two models and suggest the one that should be deployed ####

# run algorithms
results_split <- evaluate(scheme_split, algorithms, n=c(1, 3, 5, 10, 15, 20))
class(results_split)

results_CV <- evaluate(scheme_CV, algorithms, n=c(1, 3, 5, 10, 15, 20))
class(results_CV)

#### 3.4 Plot the ROC curves for UBCF and IBCF and compare them ####
plot(results_split, annotate = 1:4, legend="topleft")
plot(results_CV, annotate = 1:4, legend="topleft")

# UBCF performs better than IBCF for both schemes.

#### Give the names of the top 5 beers that you would recommend to the users "cokes", "genog" & "giblet" ####
r <- Recommender(beer_rating_matrix, method = "UBCF")
r

#cokes
recommmed_cokes <- predict(r, beer_rating_matrix['cokes'], n=5)
as(recommmed_cokes, "list") # Will recommend these Beer Ids to Coke

#genog
recommmed_genog <- predict(r, beer_rating_matrix['genog'], n=5)
as(recommmed_genog, "list") # Will recommend these Beer Ids to Coke

#giblet 
recommmed_giblet <- predict(r, beer_rating_matrix['giblet'], n=5)
as(recommmed_giblet, "list") # Will recommend these Beer Ids to Coke


#### -------------------- END OF ASSIGNMENT -------------------- #### 