library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(lubridate)
library(scales)
library(moments)
library(corrplot)
options(scipen = 999.99)
## Changed the format to number in the csv file for annual_inc field before loading it

## Set the working directory 
## load the loan.csv file

loans <- read.csv("loan.csv", stringsAsFactors = FALSE)

## Identifying the Consumer attributes and Loan Attributes and other Attributes

Consumer_attributes <- c("member_id",
                         "emp_title",
                         "emp_length",
                         "home_ownership",
                         "annual_inc",
                         "verification_status",
                         "url",
                         "desc",
                         "purpose",
                         "title",
                         "zip_code",
                         "addr_state",
                         "application_type")

loan_attributes <- c("id",
                     "loan_amnt",
                     "funded_amnt",
                     "funded_amnt_inv",
                     "term",
                     "int_rate",
                     "installment",
                     "grade",
                     "sub_grade",
                     "issue_d",
                     "loan_status",
                     "dti")

Other_attributes <- c("pymnt_plan",
                      "delinq_2yrs",
                      "earliest_cr_line",
                      "inq_last_6mths",
                    "mths_since_last_delinq",
                     "mths_since_last_record",
                     "open_acc",
                    "pub_rec",
                    "revol_bal",
                     "revol_util",
                    "total_acc",
                     "initial_list_status",
                      "out_prncp",
                      "out_prncp_inv",
                      "total_pymnt",
                      "total_pymnt_inv",
                      "total_rec_prncp",
                      "total_rec_int",
                      "total_rec_late_fee",
                      "recoveries",
                      "collection_recovery_fee",
                      "last_pymnt_d",
                      "last_pymnt_amnt",
                      "next_pymnt_d",
                      "last_credit_pull_d",
                      "collections_12_mths_ex_med",
                      "mths_since_last_major_derog",
                      "policy_code",
                      "annual_inc_joint",
                      "dti_joint",
                      "verification_status_joint",
                      "acc_now_delinq",
                      "tot_coll_amt",
                      "tot_cur_bal",
                      "open_acc_6m",
                      "open_il_6m",
                      "open_il_12m",
                      "open_il_24m",
                      "mths_since_rcnt_il",
                      "total_bal_il",
                      "il_util",
                      "open_rv_12m",
                      "open_rv_24m",
                      "max_bal_bc",
                      "all_util",
                      "total_rev_hi_lim",
                      "inq_fi",
                      "total_cu_tl",
                      "inq_last_12m",
                      "acc_open_past_24mths",
                      "avg_cur_bal",
                      "bc_open_to_buy",
                      "bc_util",
                      "chargeoff_within_12_mths",
                      "delinq_amnt",
                      "mo_sin_old_il_acct",
                      "mo_sin_old_rev_tl_op",
                      "mo_sin_rcnt_rev_tl_op",
                      "mo_sin_rcnt_tl",
                      "mort_acc",
                      "mths_since_recent_bc",
                      "mths_since_recent_bc_dlq",
                      "mths_since_recent_inq",
                      "mths_since_recent_revol_delinq",
                      "num_accts_ever_120_pd",
                      "num_actv_bc_tl",
                      "num_actv_rev_tl",
                      "num_bc_sats",
                      "num_bc_tl",
                      "num_il_tl",
                      "num_op_rev_tl",
                      "num_rev_accts",
                      "num_rev_tl_bal_gt_0",
                      "num_sats",
                      "num_tl_120dpd_2m",
                      "num_tl_30dpd",
                      "num_tl_90g_dpd_24m",
                      "num_tl_op_past_12m",
                      "pct_tl_nvr_dlq",
                      "percent_bc_gt_75",
                      "pub_rec_bankruptcies",
                      "tax_liens",
                      "tot_hi_cred_lim",
                      "total_bal_ex_mort",
                      "total_bc_limit",
                      "total_il_high_credit_limit"
                      )

## removing the other attributes from loan dataframe

loan_new <- loans[, !(colnames(loans) %in% Other_attributes)]

## View structure of loan_new dataframe

str(loan_new)

## Cleaning and Preparing data for analysis
## Convert issue_d to date format

loan_new$issue_d <- str_replace_all(paste("1-",loan_new$issue_d), " ", "")

loan_new$issue_d <- as.Date(loan_new$issue_d, "%d-%b-%y")

## Change the format of term to factor format

loan_new$term <- as.factor(loan_new$term)

## Remove the percent sign in int_rate and convert it to numeric format

loan_new$int_rate <- as.numeric(str_replace_all(loan_new$int_rate, "%", ""))

## Deriving new column int_rate_category 

range(loan_new$int_rate)

int_rate_category <- cut(loan_new$int_rate, breaks = c(5, 11, 18, 25), labels =  c("Low", "Medium","High"), include.lowest = TRUE)

loan_new <- cbind(loan_new, int_rate_category)

## We need to look at the data with "Fully Paid" and "Charged off" loan status and exclude 
## "Current" loan status as we cannot make out if these will be fully paid or charged off going forward
## So lets filter out the data

loan_data <- filter(loan_new, loan_new$loan_status %in% c("Fully Paid", "Charged Off"))

## Filter data with fully paid loan status

loan_data_paid <- filter(loan_data, loan_status == "Fully Paid")

## Filter data with charged off loan status

loan_data_default <- filter(loan_data, loan_status == "Charged Off")

loans$loan_status <- factor(loans$loan_status)

## Univariate Analysis -

quantile(loan_data$annual_inc, 0.99)

ggplot(loan_data,aes(x=annual_inc)) + geom_histogram(binwidth = 1000, fill = "red") + ylim(0, 1000) + xlim(0,250000)

summary(loan_data)

#Creating Categorical column for the annual income by dividing those into following ranges
loan_data$income_range <- cut(loan_data$annual_inc, breaks = c(0, 50000, 100000, 150000, 200000, 250000, 6000000), labels = c("Below 50000", "50000-100000", "100000-150000", "150000-200000", "200000-250000", "Above 250000"), include.lowest = TRUE)

ggplot(loan_data, aes(x = income_range)) + geom_histogram(stat = "count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## By looking at the graph most have annual income in range of 50000 - 100000 and less than 50000.


## Now lets analyze loan amount

## Summary for loan_amnt

summary(loan_data$loan_amnt)

## Histogram for loan_amnt

quantile(loan_data$loan_amnt, 0.99)
ggplot(loan_data,aes(x=loan_amnt))+geom_histogram()

## Deriving a loan_amnt_range
range(loan_data$loan_amnt)
boxplot(loan_data$loan_amnt)

loan_data$loan_amnt_range <- cut(loan_data$loan_amnt, breaks = c(0, 5000, 10000, 20000, 30000, 35000), labels = c("<5k", "5k-10k", "10k-20k", "20k-30k", ">30k"), include.lowest = TRUE)

## Plot the loan_amnt_range

ggplot(loan_data,aes(x=loan_amnt_range))+geom_histogram(stat = "count")

## The most number of loan_amount ranges from 5000-20000

## 
## Lets analyze funded amount

summary(loan_data$funded_amnt)

quantile(loan_data$funded_amnt, 0.99)

ggplot(loan_data,aes(x=funded_amnt))+geom_histogram()

boxplot(loan_data$funded_amnt)

range(loan_data$funded_amnt)

loan_data$fnd_range <- cut(loan_data$funded_amnt, breaks = c(0, 5000, 10000, 20000, 30000, 35000), labels = c("<5k", "5k-10k", "10k-20k", "20k-30k", ">30k"), include.lowest = TRUE)

ggplot(loan_data,aes(x=fnd_range))+geom_histogram(stat = "count")

## maximun times funds amount range from 5000 - 20000.

## Now Lets analyze of int_rate_category 

## Lets find the count in each categories

table(factor(loan_data$int_rate_category))

## Lets plot a bar chart for the same

ggplot(loan_data,aes(x=int_rate_category)) + geom_bar()

## maximum times the interest rate is <18%

## Let's analyze grades

table(loan_data$grade)

ggplot(loan_data, aes(x = grade)) + geom_histogram(stat = "count")

## maximum times grade is A & B

## Let's analyze term now

table(loan_data$term)

ggplot(loan_data, aes(x = term)) + geom_bar()

## Most times, term is chosen as 36 months

## Now let's analyze the purpose of loan

arrange(data.frame(table(loan_data$purpose)), desc(Freq))

ggplot(loan_data, aes(x = purpose)) + geom_bar() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

## By looking at the graph, top four purposes are credit card, debt consolidation, home improvement, others.

## Lets analyze home ownership

ggplot(loan_data, aes(x = home_ownership)) + geom_bar()

## maximum number of loan applicants have have either rented house or mortgage their property.

## Let's analyze emp_length

ggplot(loan_data, aes(x = emp_length)) + geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Let's analyze the year of loan issue 
## Let's derive a column for year

loan_data$year <- format(loan_data$issue_d, "%Y")

ggplot(loan_data, aes(x = year)) + geom_bar()

## We can clearly see that most number of loans applied in the year 2011.

## Let's analyze DTI ratio

quantile(loan_data$dti)

ggplot(loan_data, aes(x = dti)) + geom_histogram(binwidth = 1)

skewness(loan_data$dti) 

## - 0.02684138 implies that it is very slightly skewed on the left

kurtosis(loan_data$dti)

## 2.143657 implies that the tail is slight heavy on its right.

## Let's derive a column with dti_range

range(loan_data$dti)

loan_data$dti_range <- cut(loan_data$dti, breaks = c(0, 10, 20, 30), labels = c("<10", "10-20", ">20"), include.lowest = TRUE)

ggplot(loan_data, aes(x = dti_range)) + geom_bar()

## Mostly the dti ratio is between 10 - 20 or <10

## Let's analyze funded_amnt_inv

ggplot(loan_data, aes(x = funded_amnt_inv)) + geom_histogram(binwidth = 1000)

range(loan_data$funded_amnt_inv)

summary(loan_data$funded_amnt_inv)

## Let's derive the funded_inv_range

loan_data$funded_inv_range <- cut(loan_data$funded_amnt_inv, breaks = c(0, 5000, 10000, 20000, 30000, 35000), labels = c("<5k", "5k-10k", "10k-20k", "20k-30k", ">30k"), include.lowest = TRUE)

ggplot(loan_data, aes(x = funded_inv_range)) + geom_bar()

## Maximum applicants funds investment is < 20000.

## Now lets see the correlation between all numeric variables

## Let's remove fields which has no significance

rem_data <- c("member_id","id","policy_code", "url","desc","emp_title","zip_code","addr_state","title")

loan_data <- loan_data[,!(colnames(loan_data) %in% rem_data)]

## Now convertng all the character variables to factor

loan_data[sapply(loan_data, is.character)] <- lapply(loan_data[sapply(loan_data, is.character)], 
                                           as.factor)

## Let's remove the factor data now

continuous_data <- loan_data[,(colnames(loan_data) %in% (names(loan_data)[sapply(loan_data, class) != "factor"]))]

str(continuous_data)

## Also, remove issue_d field from continuous_data

continuous_data <- continuous_data[ , -7]
str(continuous_data)

## Let's see the correlation matrix 

cor_mat <- cor(continuous_data)

corrplot(cor_mat, method="number")

## The observation is obvious funded amount is positively correlatec to loan amount.
## funded amount, funded amount investment and installments are dependent on each other.

## Now analysing all the factors in loan_data with loan_status

## Annual income v/s Loan status

ggplot(loan_data, aes(x = income_range, fill = loan_status)) + geom_bar(position = "fill") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## By looking at the plot we can clearly say that people below 50000 income tends to defualt more and as annual income increases people defualting decreases

## Interest rate v/s loan status

ggplot(loan_data, aes(x = int_rate_category, fill = loan_status)) + geom_bar(position = "fill")

## By looking at the graph we can say that a high interest rate has more number of defaulters.

## Loan amount v/s  loan_status

ggplot(loan_data, aes(x = loan_amnt_range, fill = loan_status)) + geom_bar(position = "fill")

## From this graph we can say that more the loan amount more you are likely to defualt

## DTI ratio v/s loan status

ggplot(loan_data, aes(x = dti_range, fill = loan_status)) + geom_bar(position = "fill")

## We see that DTI ratio above 20 have more defaulters.

## Year v/s loan status

ggplot(loan_data, aes(x = year, fill = loan_status)) + geom_bar(position = "fill")

## here we can see that 2007 & 2011 had more defaulters

## Home ownership v/s loan status

ggplot(loan_data, aes(x = home_ownership, fill = loan_status)) + geom_bar(position = "fill")

## here we can clearly see that mortgage, own, and rent have similar amount of defualters with other having more tha them

 
## Grade v/s loan status

ggplot(loan_data, aes(x = grade, fill = loan_status)) + geom_bar(position = "fill")

## here you can clearly see as the grade goes from A to G, by G being last no of defaulters increases

## Sub grade vs loan status
ggplot(loan_data, aes(x = sub_grade, fill = loan_status)) + geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## the trend remains same for sub grades as well with F5 consisting more defaulters

## Purpose vs loan status
ggplot(loan_data, aes(x = purpose, fill = loan_status)) + geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## we can clearly see that even though no of borrowers n debt consolidation & credit card are more, defualting people are more who take loans for small businesses

## Let's see grade and interest rate are correlated

ggplot(loan_data, aes(x = int_rate_category, fill = grade )) + geom_bar()

## As there is a change in grade from A to G, the interest rate increases.

## With univariate analysis and Bivariate analysis, we can say that following are the driving factors
## Annual Income
## DTI
## loan amount
## Grade
## Sub grade
## Home ownership
## Interest rate

## Multivariate analysis

## Lets see if the above factors are driving factors in the major purposes.
## Considering credit card & debt_consolidation

loan_data_final <- loan_data[loan_data$purpose %in% c("credit_card", "debt_consolidation"), ]

## Grade

ggplot(loan_data_final, aes(x = grade, fill = loan_status)) + geom_bar(position = "fill") + facet_grid( ~purpose)

## we can see that grade E & F are commonly affecting. 


## Year

##ggplot(loan_data_final, aes(x = year, fill = loan_status)) + geom_bar(position = "fill") + facet_grid( ~purpose)

## We can see similar patterns in both types of purposes, Hence we can say Loan issued year is a driving factor.

## Home ownership

##ggplot(loan_data_final, aes(x = home_ownership, fill = loan_status)) + geom_bar(position = "fill") + facet_grid( ~purpose)

## We can see similar patterns in both types of purposes, Hence we can say home ownership is a driving factor.

## loan amount

ggplot(loan_data_final, aes(x = loan_amnt_range, fill = loan_status)) + geom_bar(position = "fill") + facet_grid( ~purpose)

## We can see that people with more loan amount tends to defualt more

## Interest rate

ggplot(loan_data_final, aes(x = int_rate_category, fill = loan_status)) + geom_bar(position = "fill") + facet_grid( ~purpose)

## We can see similar patterns in both types of purposes, Hence we can say interest rate is a driving factor.


## checking how loan amount and intrest rate affecting the loan status
ggplot(loan_data, aes(x = loan_amnt_range, fill = loan_status)) + geom_bar(position = "fill") + facet_grid(int_rate_category ~ .)
## so from this we can conclude that more the loan amount, more the intrest rate more likely to defualt. But pattern accross loan range remains same for constant int rate category

## checking how loan amount and term affecting the loan status 
ggplot(loan_data, aes(x = loan_amnt_range, fill = loan_status)) + geom_bar(position = "fill") + facet_grid(term ~ .)
## from here you can see that people with 60 motnhs term and more loan amount tend to defualt more. But pattern accross loan range remains constant for the each term individually.
