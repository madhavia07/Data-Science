
#-----------------------------------------------------------------------------
# Module 2 - Programming in R
# Individual Assignment 1 - Problem Statement (Bollywood)
# Name: Frudhvi Madhavi Ayyagari | E- madhavi.a07@gmail.com | M- 7730899695
#-----------------------------------------------------------------------------

#	Import the Bollywood data set in Rstudio in a variable named bollywood

  bollywood <- read.csv("bollywood.csv")
  View(bollywood)

#	When you import a data set, R stores character vectors as factors (by default)
# You can check the structure of the data frame by using str()
  str(bollywood)

# You can change the attribute 'Movie' from factor to character type using the given command
  bollywood$Movie <- as.character(bollywood$Movie)

#Check the type
  typeof(bollywood$Movie)
#-----------------------------------------------------------------------------

#Q1.
#	Access the last 10 movies (from the bottom of the Bollywood data frame) using column bollywood$Movie
# Store the names of those movies in last_10 vector (in the same order)
     
  last_10 <- tail(bollywood$Movie,10)
  last_10

#--------------------------------END OF Q1------------------------------------
  	  
#Q2.
#	Find out the total number of  missing values (NA) in the bollywood data frame.
# Store the result in na_bollywood vector
     
  na_bollywood <- sum(is.na(bollywood)) 
	na_bollywood  

#--------------------------------END OF Q2------------------------------------
	
#Q3
#	Write the command to find out which movie tops the list in terms of Total Collections
# Store the movie name in variable named top_movie
 
  top_movie <- bollywood[which.max(bollywood$Tcollection),1]
  top_movie

#--------------------------------END OF Q3------------------------------------
  
#Q4
#	Write the command to find out which movie comes second on the list in terms of Total Collections
# Store the movie name in variable named top_2_movie

  Sorted_Bollywood_byTcollection <- bollywood[order(-bollywood$Tcollection),]
  top_2_movie <- Sorted_Bollywood_byTcollection[2,1]
  top_2_movie
	  
# Now let's find out the movies shot by Shahrukh, Akshay and Amitabh separately.
# subset() function is used for that. The code has already been written for you. 
	
	shahrukh <- subset(bollywood, Lead == "Shahrukh")
	akshay <- subset(bollywood, Lead == "Akshay")
	amitabh <- subset(bollywood, Lead  == "Amitabh")
	
# You can view what the above data frames look like

	shahrukh
	akshay
	amitabh

#--------------------------------END OF Q4------------------------------------
		   
#Q5
#	What is the total collection of Shahrukh, Akshay and Amitabh movies individually?
# You can use	a column named 'Tcollection' for this 
 
	shahrukh_collection <- sum(shahrukh$Tcollection)
  shahrukh_collection
  
  akshay_collection <- sum(akshay$Tcollection) 
  akshay_collection
  
	amitabh_collection <- sum(amitabh$Tcollection)
  amitabh_collection
  
#--------------------------------END OF Q5------------------------------------
	
#Q6  
# Write command/s to find out how many movies are in Flop, Average, Hit and Superhit categories in the entire Bollywood data set.
  Superhit <- length(which(bollywood$Verdict == "Super Hit"))
  Superhit
  
  Hit <- length(which(bollywood$Verdict == "Hit"))
  Hit
  
  Average <- length(which(bollywood$Verdict == "Average"))
  Average
  
  Flop <- length(which(bollywood$Verdict == "Flop"))
  Flop

#--------------------------------END OF Q6------------------------------------

#You can use SAPPLY function if you want to apply a function specific columns in a data frame 
#You can write a command to find the maximum value of Ocollection, Wcollection, Fwcollecion and Tcollection using sapply

#Q7 
# Write a command to find the names of the movies which have the maximum Ocollection, Wcollection, Fwcollecion & Tcollection
# Store the names of 4 movies in same sequence in movie_result vector

  c(bollywood[which.max(bollywood$Ocollection),1], bollywood[which.max(bollywood$Wcollection),1],bollywood[which.max(bollywood$Fwcollection),1],bollywood[which.max(bollywood$Tcollection),1])
	names(movie_result) <- c("Maximum_Ocollection_movie","Maximum_Wcollection_movie","Maximum_Fwcollection_movie","Maximum_Tcollection_movie")
  movie_result

  #--------------------------------END OF ASSIGNMENT------------------------------------	

   
    


    
    
    