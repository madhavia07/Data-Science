## Checkpoints - Part 1

## Loading the companies dataframe -
## Set the working directory
companies <- read.delim("companies.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
str(companies)

## Loading the rounds2 dataframe - 
rounds2 <- read.csv("rounds2.csv", header = TRUE, stringsAsFactors = FALSE)
str(rounds2)

## Table 1.1
## How many unique companies are present in rounds2?

rounds2$company_permalink <- tolower(rounds2$company_permalink)

length(unique(rounds2$company_permalink))

## How many unique companies are present in companies?

length(unique(tolower(companies$permalink)))

## In the companies data frame, which column can be used as the unique key for each company? Write the name of the column.

companies$permalink

## Are there any companies in the rounds2 file which are not present in companies? Answer yes or no: Y/N

companies$permalink <- tolower(companies$permalink)

master_frame <- merge(rounds2, companies, by.x = "company_permalink", by.y = "permalink")

## As number of rows in the master_frame is same as rounds2 dataframe, we can conclude that No all the companies
## present in rounds2, are also present in companies. The number of rows after merge is same as rounds2.

master_frame <- merge(rounds2, companies, by.x = "company_permalink", by.y = "permalink")

## Merge the two data frames so that all variables (columns) in the companies frame are added to the rounds2 data frame. Name the merged frame master_frame. 
##How many observations are present in master_frame?

master_frame <- merge(rounds2, companies, by.x = "company_permalink", by.y = "permalink")

## Checkpoint 2
## Table 2.1 
## Calculate the average investment amount for each of the four funding types (venture, angel, seed, and private equity)

## Load the package dplyr

library(dplyr)

grouping_funding_type <- group_by(filter(master_frame, (master_frame$funding_round_type =="venture" |
                                                           master_frame$funding_round_type == "angel" |
                                                          master_frame$funding_round_type == "seed" |
                                                          master_frame$funding_round_type == "private_equity")), funding_round_type)

avg_funding <- setNames(summarise(grouping_funding_type
                 ,mean(raised_amount_usd, na.rm = TRUE)), c("funding_type", "avg_funds"))

## Considering that Spark Funds wants to invest between 5 to 15 million USD per investment round, which investment type is the most suitable for them?

filter(avg_funding, (avg_funding$avg_funds >= 5000000.00 & avg_funding$avg_funds <= 15000000.00))


## Checkpoint 3

 venture_master <- filter(master_frame, (funding_round_type == "venture" & country_code !="")) 
 
 group_country_funds <- group_by(venture_master, country_code)
 
 top_9 <- top_n(arrange(setNames(summarise(group_country_funds, sum(raised_amount_usd, na.rm = TRUE)), c("country_code", "total_raised_funds")), desc(total_raised_funds)), 9)
 
 
 ## Checkpoint 4
 
 mappings <- read.csv("mapping.csv", stringsAsFactors = FALSE)
 
 mappings <- gather(mappings, sector, value, 2:10 )
 mappings <- mappings[!(mappings$value == 0), ]
 mappings <- mappings[, -3]
 mappings <- filter(mappings, mappings$sector != "Blanks")

 library(tidyr) 
 library(dplyr)
 
 write.csv(mappings, file = "final_mapping.csv")
 ?write.csv
 
sector_master <- separate(master_frame, category_list, into =c("primary_sector", "secondary_sector"), sep = "\\|")

mappings$category_list <- tolower(mappings$category_list)
sector_master$primary_sector <- tolower(sector_master$primary_sector)

master_frame <- merge(sector_master, mappings, by.x = "primary_sector", by.y = "category_list" )

country1_usa <- filter(master_frame, country_code == "USA" & funding_round_type == "venture" & raised_amount_usd >= 5000000 & raised_amount_usd <= 15000000)
country2_gbr <- filter(master_frame, country_code == "GBR" & funding_round_type == "venture" & raised_amount_usd >= 5000000 & raised_amount_usd <= 15000000)
country3_ind <- filter(master_frame, country_code == "IND" & funding_round_type == "venture" & raised_amount_usd >= 5000000 & raised_amount_usd <= 15000000)


## Checkpoint 5 

## Total number of Investments (count)

nrow(country1_usa)
nrow(country2_gbr)
nrow(country3_ind)
 
## Total amount of Investments (USD)

summarise(country1_usa, sum(raised_amount_usd, na.rm = TRUE))
summarise(country2_gbr, sum(raised_amount_usd, na.rm = TRUE))
summarise(country3_ind, sum(raised_amount_usd, na.rm = TRUE))

## Top 3 sector name, number of investment wise

 top_n(arrange(setNames(data.frame(table(country1_usa$sector)), c("Sector", "number_of_investments")), desc(number_of_investments)), 3)
 top_n(arrange(setNames(data.frame(table(country2_gbr$sector)), c("Sector", "number_of_investments")), desc(number_of_investments)), 3)
 top_n(arrange(setNames(data.frame(table(country3_ind$sector)), c("Sector", "number_of_investments")), desc(number_of_investments)), 3)
 
## For point 3 (top sector count-wise), which company received the highest investment?

 highest_investment_usa_others <- top_n(arrange(summarise(group_by(filter(country1_usa, sector == "Others"), name), total_investment = sum(raised_amount_usd, na.rm = TRUE)), desc(total_investment)), 1)
 highest_investment_gbr_others <- top_n(arrange(summarise(group_by(filter(country2_gbr, sector == "Others"), name), total_investment = sum(raised_amount_usd, na.rm = TRUE)), desc(total_investment)), 1)
 highest_investment_ind_others <- top_n(arrange(summarise(group_by(filter(country3_ind, sector == "Others"), name), total_investment = sum(raised_amount_usd, na.rm = TRUE)), desc(total_investment)), 1)
 
## For point 4 (second best sector count-wise), which company received the highest investment?
 
 highest_investment_usa_social <- top_n(arrange(summarise(group_by(filter(country1_usa, sector == "Social..Finance..Analytics..Advertising"), name), total_investment = sum(raised_amount_usd, na.rm = TRUE)), desc(total_investment)), 1)
 highest_investment_gbr_social <- top_n(arrange(summarise(group_by(filter(country2_gbr, sector == "Social..Finance..Analytics..Advertising"), name), total_investment = sum(raised_amount_usd, na.rm = TRUE)), desc(total_investment)), 1)
 highest_investment_ind_social <- top_n(arrange(summarise(group_by(filter(country3_ind, sector == "Social..Finance..Analytics..Advertising"), name), total_investment = sum(raised_amount_usd, na.rm = TRUE)), desc(total_investment)), 1)
 
 
 ## Export master_frame dataframe
 
 write.csv(master_frame, "masterframe.csv")
 

