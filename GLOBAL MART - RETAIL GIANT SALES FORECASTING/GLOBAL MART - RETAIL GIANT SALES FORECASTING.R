#------Course 4 | Predictive Analytics II | Retail Giant Sales Forecasting----------
#  Madhavi Ayyagari 
#——————————————————————————————————————————————————————

#-------Clear work space----------
rm(list = ls()) 
#-------Set Working Directory---------
#Session > Set woring directory >Choose file

#-------Load Libraries----------
library(ggplot2)
library(graphics)
library(forecast)
library(tseries)

#-------1. Data Preparation and cleaning---------
# We have a data of the online store GLOBAL MART which has worldwide customers and deals in these major product categories:
# Consumer, Corporate & home office

# Let us import the Global store sales data  
globalstore <- read.csv("Global Superstore.csv", stringsAsFactors = F)

# Now lets treat NA Values in this data
sapply(globalstore, function(x) sum(is.na(x)))

# So we can see that there are NA values for the Postal code (41296 values) of other markets except US.
# Lets us check the structure of the data
str(globalstore)

#Now change the data type for performing analysis

# Order.Date from Char to Date
globalstore$Order.Date<-as.Date(globalstore$Order.Date,"%d-%m-%Y")
# Ship.Date from char to Date
globalstore$Ship.Date<-as.Date(globalstore$Ship.Date,"%d-%m-%Y")
# Postal.Code from int to factor
globalstore$Postal.Code<-as.factor(globalstore$Postal.Code)

#As per the business understanding the we have to subset the data into 21 buckets based on 7 markets and 3 product categories

# Products categories : Consumer, Corporate & Home Office
levels(globalstore$Segment) 
# Market segments : Africa, APAC, Canada, EMEA, EU, LATAM, US
levels(globalstore$Market)  

#Lets create a cummulative sales data table based on the 3 attributes to focus Sales, Quantity & Profit

TotalProfit<-aggregate(globalstore$Profit, by=list(globalstore$Market,globalstore$Segment), FUN=sum)
names(TotalProfit)<-list("Market","Segment","TotalProfit")

AvgSales<-aggregate(globalstore$Sales, by=list(globalstore$Market,globalstore$Segment), FUN=mean)
names(AvgSales)<-list("Market","Segment","AvgSalesAmount")

TotalSales<-aggregate(globalstore$Sales, by=list(globalstore$Market,globalstore$Segment), FUN=sum)
names(TotalSales)<-list("Market","Segment","TotalSalesAmount")

SalesQuantity<-aggregate(globalstore$Sales, by=list(globalstore$Market,globalstore$Segment), FUN=length)
names(SalesQuantity)<-list("Market","Segment","SalesQuantity")

gsSalesSummary<-data.frame(TotalProfit,AvgSales,TotalSales,SalesQuantity)
gsSalesSummary<-gsSalesSummary[,c(1,2,3,6,9,12)]

# Adding Profit as a percentage of Sales amount to the dataframe
gsSalesSummary$ProfitPercent<-(gsSalesSummary$TotalProfit/gsSalesSummary$TotalSalesAmount)*100

# Products categories : Consumer, Corporate & Home Office
levels(globalstore$Segment) 
# Market segments : Africa, APAC, Canada, EMEA, EU, LATAM, US
levels(globalstore$Market)  

#Lets create a monthly sales data table based on the 3 attributes to focus Sales, Quantity & Profit

MonthlyTotalProfit<-aggregate(globalstore$Profit, by=list(globalstore$Market,globalstore$Segment,format(as.Date(globalstore$Order.Date), "%Y%m")), FUN=sum)
names(MonthlyTotalProfit)<-list("Market","Segment","OrderDate", "MonthlyTotalProfit")

MonthlyAvgSales<-aggregate(globalstore$Sales, by=list(globalstore$Market,globalstore$Segment,format(as.Date(globalstore$Order.Date), "%Y%m")), FUN=mean)
names(MonthlyAvgSales)<-list("Market","Segment","OrderDate","MonthlyAvgSalesAmount")

MonthlyTotalSales<-aggregate(globalstore$Sales, by=list(globalstore$Market,globalstore$Segment,format(as.Date(globalstore$Order.Date), "%Y%m")), FUN=sum)
names(MonthlyTotalSales)<-list("Market","Segment","OrderDate","MonthlyTotalSalesAmount")

MonthlySalesQuantity<-aggregate(globalstore$Sales, by=list(globalstore$Market,globalstore$Segment,format(as.Date(globalstore$Order.Date), "%Y%m")), FUN=length)
names(MonthlySalesQuantity)<-list("Market","Segment","OrderDate","MonthlySalesQuantity")

gsSalesMonthlySummary<-data.frame(MonthlyTotalProfit,MonthlyAvgSales,MonthlyTotalSales,MonthlySalesQuantity)
gsSalesMonthlySummary<-gsSalesMonthlySummary[,c(1,2,3,4,8,12,16)]

#Now lets calculate the profit percentage for the monthly sales data
# Profit percentage =  Amount of Profit by Amount of Sales multiplied by Quantity of sale for that month

gsSalesMonthlySummary$MonthlyProfitPercent<-(gsSalesMonthlySummary$MonthlyTotalProfit/gsSalesMonthlySummary$MonthlyTotalSalesAmount)*100
#gsSalesMonthlySummary<-gsSalesMonthlySummary[order(gsSalesMonthlySummary$TotalMonthlySales,decreasing=TRUE),]

#Now we need to find the coefficient of variation of the profit for the market combinations
# Coefficient of variation (CV) = Standard deviation (SD) / Mean

#Lets find the Mean of monthly profit percentage for the 21 market combinations
Mean_gsMonthlyProfitPercentage<-aggregate(gsSalesMonthlySummary$MonthlyProfitPercent, by=list(gsSalesMonthlySummary$Market,gsSalesMonthlySummary$Segment), FUN=mean)
names(Mean_gsMonthlyProfitPercentage)<-list("Market","Segment","Mean_gsMonthlyProfitPercentage")

#Lets find the SD of monthly profit percentage for the 21 market combinations
SD_gsMonthlyProfitPercentage<-aggregate(gsSalesMonthlySummary$MonthlyProfitPercent, by=list(gsSalesMonthlySummary$Market,gsSalesMonthlySummary$Segment), FUN=sd)
names(SD_gsMonthlyProfitPercentage)<-list("Market","Segment","SD_gsMonthlyProfitPercentage")

#Adding SD and Mean Values to the gsSalesSummary and to calculate and agggregated value
gsSalesSummary<-data.frame(gsSalesSummary,Mean_gsMonthlyProfitPercentage,SD_gsMonthlyProfitPercentage)
gsSalesSummary<-gsSalesSummary[,c(1,2,3,4,5,6,7,10,13)]

#Lets now calculate CV and add the column to the gsSalesSummary
gsSalesSummary$CV_gsMonthlyProfitPercentage<-gsSalesSummary$SD_gsMonthlyProfitPercentage/gsSalesSummary$Mean_gsMonthlyProfitPercentage

#Order on Total profit to find the most profitable and consistent Market Segment 
gsSalesSummary<-gsSalesSummary[order(gsSalesSummary$TotalProfit,decreasing=TRUE),]

# write.csv(gsSalesSummary, "D:/gsSales.csv")

# Plotting Market Segment Vs. Total Profit

# Market and Segment generating most profit
gsProfitPlot1<-ggplot(gsSalesSummary,aes(x=gsSalesSummary$Market,y=gsSalesSummary$TotalProfit,fill=gsSalesSummary$Segment))
gsProfitPlot1+geom_bar(stat ="identity",position="dodge")+xlab("Market")+ylab("Profit")+ggtitle("Total Profit Vs Market Segment")

# Market and Segment having most profit margin
gsProfitPlot2<-ggplot(gsSalesSummary,aes(x=gsSalesSummary$Market,y=gsSalesSummary$ProfitPercent,fill=gsSalesSummary$Segment))
gsProfitPlot2+geom_bar(stat="identity",position="dodge")+xlab("Market")+ylab("Profit %age")+ggtitle("Profit percentage Vs Market Segment")

# Market and Segment having most profit margin
gsProfitPlot3<-ggplot(gsSalesSummary,aes(x=gsSalesSummary$Market,y=gsSalesSummary$CV_gsMonthlyProfitPercentage,fill=gsSalesSummary$Segment))
gsProfitPlot3+geom_bar(stat="identity",position="dodge")+xlab("Market")+ylab("CV Monthly Profit")+ggtitle("CV Monthly profit Vs Market Segment")

#---------Graph Analysis-----------
#Top 10 Market segments based on the total maximum profits 
#1 APAC	Consumer
#2 EU	Consumer
#3 US	Consumer
#4 APAC	Corporate
#5 EU	Corporate
#6 LATAM	Consumer
#7 US	Corporate
#8 APAC	Home Office
#9 EU	Home Office
#10 US	Home Office


#Among the above the top 10 Market segments further selected based on maximum profit percentage
#1 APAC	Consumer
#2 EU	Consumer
#3 US	Consumer
#4 APAC	Corporate
#5 EU	Corporate
#6 LATAM	Consumer
#7 US	Corporate
#8 APAC	Home Office
#9 EU	Home Office
#10 US	Home Office

#Among the above the top 10 Market segments further selected based on the low CV
#1 APAC	Consumer
#2 EU	Consumer
#3 US	Consumer
#4 APAC	Corporate
#5 EU	Corporate
#6 LATAM	Consumer
#7 US	Corporate
#8 APAC	Home Office
#9 EU	Home Office
#10 US	Home Office

#-------Result of Step 1: Therefore the 2 most profitable segments which have to be considered for Model building are: ------
#1 APAC Consumer
#2 EU Consumer

#-------2. Model Building---------
#Smoothen data before performing classical decomposition
#We have perform classical decomposition and Auto Arima for forecasting 

#------- APAC Consumer---------
APACConsumerSalesSummary<-subset(gsSalesMonthlySummary,(gsSalesMonthlySummary$Market=="APAC")&(gsSalesMonthlySummary$Segment=="Consumer"))
APACConsumerSalesSummary<-APACConsumerSalesSummary[order(APACConsumerSalesSummary$OrderDate),]
APACConsumerSalesSummary$NumOfTheMonth<-c(1:nrow(APACConsumerSalesSummary))

#Preparing data for TS forecasting
APACConsumerProfit<-APACConsumerSalesSummary[,c(9,4)]
APACConsumerQuantity<-APACConsumerSalesSummary[,c(9,7)]

#-------- 2.1.1 APAC CONSUMER PROFIT MODELLEING ----------
#Separating test data
RowCount<-nrow(APACConsumerSalesSummary)
APACConsumerProfit_Test<-APACConsumerProfit[(RowCount-10):RowCount,] # Test data
xlab<-c(1)
ylab<-c(2)

#-------- Plotting timeseries data for Profit--------------------------
APACConsumerProfit_Timeser<-ts(APACConsumerProfit[,ylab[1]])

#------ 2.1.2 Decompose timeseries to see the components with frequency = 5 ---------
APACConsumerProfit_Timeser_Decompose<-ts(APACConsumerProfit[,ylab[1]],frequency=5)
APACConsumerProfit_Timeser_Decompose <- decompose(APACConsumerProfit_Timeser_Decompose)
plot(APACConsumerProfit_Timeser_Decompose)

# Decomposotion showed that:
# 1. Trend in a high wavelength sine curve
# 2. Seasonality is a low wavelength sine curve

# Plot the timeseries
plot(APACConsumerProfit_Timeser)

# Smoothening the curve 
w <-1
APACConsumerProfit_Timeser_Smooth <- stats::filter(APACConsumerProfit_Timeser, 
                                                   filter=rep(1/(2*w+1),(2*w+1)), 
                                                   method='convolution', sides=2)



diff <- APACConsumerProfit_Timeser_Smooth[w+2] - APACConsumerProfit_Timeser_Smooth[w+1]

for (i in seq(w,1,-1)) {
  APACConsumerProfit_Timeser_Smooth[i] <- APACConsumerProfit_Timeser_Smooth[i+1] - diff
}
n <- length(APACConsumerProfit_Timeser)
n
timeseries <- APACConsumerProfit[[xlab[1]]]
timeseries_test <- APACConsumerProfit_Test[[xlab[1]]]

diff <- APACConsumerProfit_Timeser_Smooth[n-w] - APACConsumerProfit_Timeser_Smooth[n-w-1]

for (i in seq(n-w+1, n)) {
  APACConsumerProfit_Timeser_Smooth[i] <- APACConsumerProfit_Timeser_Smooth[i-1] + diff
}
lines(APACConsumerProfit_Timeser_Smooth, col="red", lwd=2)

APACConsumerProfit_Timeser_SmoothHdf <- as.data.frame(cbind(timeseries, as.vector(APACConsumerProfit_Timeser_Smooth)))
colnames(APACConsumerProfit_Timeser_SmoothHdf) <- c('NumOfTheMonth', 'Profit')
APACConsumerProfit_Timeser_SmoothHdf$NumOfTheMonth<-as.numeric(APACConsumerProfit_Timeser_SmoothHdf$NumOfTheMonth)
APACConsumerProfit_Timeser_SmoothHdf$Profit<-as.numeric(APACConsumerProfit_Timeser_SmoothHdf$Profit)

str(APACConsumerProfit_Timeser_SmoothHdf)

str(APACConsumerProfit)

#----------2.1.3 APAC Consumer model-------------
lmfit <- lm(APACConsumerProfit_Timeser_SmoothHdf$Profit ~ sin(0.5*APACConsumerProfit_Timeser_SmoothHdf$NumOfTheMonth) *
              poly(APACConsumerProfit_Timeser_SmoothHdf$NumOfTheMonth,2) 
            + cos(0.5*APACConsumerProfit_Timeser_SmoothHdf$NumOfTheMonth) * 
              poly(APACConsumerProfit_Timeser_SmoothHdf$NumOfTheMonth,2)
            + sin(0.05*APACConsumerProfit_Timeser_SmoothHdf$NumOfTheMonth)*
              APACConsumerProfit_Timeser_SmoothHdf$NumOfTheMonth, 
            data=APACConsumerProfit_Timeser_SmoothHdf)
summary(lmfit)
accuracy(lmfit)


trendProfit <- predict(lmfit, data.frame(x=timeseries))

lines(timeseries, trendProfit, col="blue", lwd=2)

#--------2.1.4 Auto Arima APAC COnsumer -------------
Data_1 <- APACConsumerProfit
nrow(Data_1)

#Converting dataframe to time series
indata <- Data_1$MonthlyTotalProfit[1:48]
timeser <- ts(indata)
plot(timeser)

#Building final model in R
autoarimafit<- auto.arima(timeser)
in_data_pred<-fitted(autoarimafit)

plot(autoarimafit$x, col="red")
lines(in_data_pred, col="blue")

#--------2.1.5 Evaluating the model using MAPE-------
fcast_auto_arima <- predict(autoarimafit, n.ahead = 6)
outdata <- Data_1$MonthlyTotalProfit[43:48]
outdata
MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata)[5]
MAPE_auto_arima #51.81 Accuarcy

#--------2.5 Forecasting APAC Consumer Profit------------------
#---Forecast using normal method-------
APACConsumerProfitForecast <- forecast(APACConsumerProfit_Timeser)
APACConsumerProfitForecast
plot(APACConsumerProfitForecast)

#---Forecast using holts method-------
APACConsumerProfitForecast6months <- forecast::holt(APACConsumerProfit_Timeser,h=6)
APACConsumerProfitForecast6months
plot(APACConsumerProfitForecast6months)

# Comparing with Test data
accuracy(APACConsumerProfitForecast6months)

#-----Using Forecast function to calculate next six month profit-----------
forecast(APACConsumerProfit_Timeser,h=6)
forecast(autoarimafit,h=6)

#-------- 2.2.1 APAC CONSUMER Quantity MODELLEING ----------
#Separating test data
RowCount<-nrow(APACConsumerSalesSummary)
APACConsumerQuantity_Test<-APACConsumerQuantity[(RowCount-10):RowCount,] # Test data
xlab<-c(1)
ylab<-c(2)

#-------- Plotting timeseries data for Quantity--------------------------
APACConsumerQuantity_Timeser<-ts(APACConsumerQuantity[,ylab[1]])

#------ 2.2.2 Decompose timeseries to see the components with frequency = 5 ---------
APACConsumerQuantity_Timeser_Decompose<-ts(APACConsumerQuantity[,ylab[1]],frequency=5)
APACConsumerQuantity_Timeser_Decompose <- decompose(APACConsumerQuantity_Timeser_Decompose)
plot(APACConsumerQuantity_Timeser_Decompose)

# Decomposotion showed that:
# 1. Trend in a high wavelength sine curve
# 2. Seasonality is a low wavelength sine curve

# Plot the timeseries
plot(APACConsumerQuantity_Timeser)

# Smoothening the curve 
w <-1
APACConsumerQuantity_Timeser_Smooth <- stats::filter(APACConsumerQuantity_Timeser, 
                                                     filter=rep(1/(2*w+1),(2*w+1)), 
                                                     method='convolution', sides=2)



diff <- APACConsumerQuantity_Timeser_Smooth[w+2] - APACConsumerQuantity_Timeser_Smooth[w+1]

for (i in seq(w,1,-1)) {
  APACConsumerQuantity_Timeser_Smooth[i] <- APACConsumerQuantity_Timeser_Smooth[i+1] - diff
}
n <- length(APACConsumerQuantity_Timeser)
n
timeseries <- APACConsumerQuantity[[xlab[1]]]
timeseries_test <- APACConsumerQuantity_Test[[xlab[1]]]

diff <- APACConsumerQuantity_Timeser_Smooth[n-w] - APACConsumerQuantity_Timeser_Smooth[n-w-1]

for (i in seq(n-w+1, n)) {
  APACConsumerQuantity_Timeser_Smooth[i] <- APACConsumerQuantity_Timeser_Smooth[i-1] + diff
}
lines(APACConsumerQuantity_Timeser_Smooth, col="red", lwd=2)

APACConsumerQuantity_Timeser_SmoothHdf <- as.data.frame(cbind(timeseries, as.vector(APACConsumerQuantity_Timeser_Smooth)))
colnames(APACConsumerQuantity_Timeser_SmoothHdf) <- c('NumOfTheMonth', 'Quantity')
APACConsumerQuantity_Timeser_SmoothHdf$NumOfTheMonth<-as.numeric(APACConsumerQuantity_Timeser_SmoothHdf$NumOfTheMonth)
APACConsumerQuantity_Timeser_SmoothHdf$Quantity<-as.numeric(APACConsumerQuantity_Timeser_SmoothHdf$Quantity)

str(APACConsumerQuantity_Timeser_SmoothHdf)

str(APACConsumerQuantity)

#----------2.2.3 APAC Consumer model-------------
lmfit <- lm(APACConsumerQuantity_Timeser_SmoothHdf$Quantity ~ sin(0.5*APACConsumerQuantity_Timeser_SmoothHdf$NumOfTheMonth) *
              poly(APACConsumerQuantity_Timeser_SmoothHdf$NumOfTheMonth,2) 
            + cos(0.5*APACConsumerQuantity_Timeser_SmoothHdf$NumOfTheMonth) * 
              poly(APACConsumerQuantity_Timeser_SmoothHdf$NumOfTheMonth,2)
            + sin(0.05*APACConsumerQuantity_Timeser_SmoothHdf$NumOfTheMonth)*
              APACConsumerQuantity_Timeser_SmoothHdf$NumOfTheMonth, 
            data=APACConsumerQuantity_Timeser_SmoothHdf)
summary(lmfit)
accuracy(lmfit)


trendQuantity <- predict(lmfit, data.frame(x=timeseries))

lines(timeseries, trendQuantity, col="blue", lwd=2)

#--------2.2.4 Auto Arima APAC COnsumer -------------
Data_1 <- APACConsumerQuantity
nrow(Data_1)

#Converting dataframe to time series
indata <- Data_1$MonthlyTotalQuantity[1:48]
timeser <- ts(indata)
plot(timeser)

#Building final model in R
autoarimafit<- auto.arima(timeser)
in_data_pred<-fitted(autoarimafit)

plot(autoarimafit$x, col="red")
lines(in_data_pred, col="blue")

#--------2.2.5 Evaluating the model using MAPE-------
fcast_auto_arima <- predict(autoarimafit, n.ahead = 6)
outdata <- Data_1$MonthlyTotalQuantity[43:48]
outdata
MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata)[5]
MAPE_auto_arima #51.81 Accuarcy

#--------2.5 Forecasting APAC Consumer Quantity------------------
#---Forecast using normal method-------
APACConsumerQuantityForecast <- forecast(APACConsumerQuantity_Timeser)
APACConsumerQuantityForecast
plot(APACConsumerQuantityForecast)

#---Forecast using holts method-------
APACConsumerQuantityForecast6months <- forecast::holt(APACConsumerQuantity_Timeser,h=6)
APACConsumerQuantityForecast6months
plot(APACConsumerQuantityForecast6months)

# Comparing with Test data
accuracy(APACConsumerQuantityForecast6months)

#-----Using Forecast function to calculate next six month Quantity-----------
forecast(APACConsumerQuantity_Timeser,h=6)
forecast(autoarimafit,h=6)


#-------3.1 EU Consumer---------
EUConsumerSalesSummary<-subset(gsSalesMonthlySummary,(gsSalesMonthlySummary$Market=="EU")&(gsSalesMonthlySummary$Segment=="Consumer"))
EUConsumerSalesSummary<-EUConsumerSalesSummary[order(EUConsumerSalesSummary$OrderDate),]
EUConsumerSalesSummary$NumOfTheMonth<-c(1:nrow(EUConsumerSalesSummary))

#Preparing data for TS forecasting
EUConsumerProfit<-EUConsumerSalesSummary[,c(9,4)]
EUConsumerQuantity<-EUConsumerSalesSummary[,c(9,7)]

#-------- 3.1.1 EU CONSUMER PROFIT MODELLEING ----------
#Separating test data
RowCount<-nrow(EUConsumerSalesSummary)
EUConsumerProfit_Test<-EUConsumerProfit[(RowCount-10):RowCount,] # Test data
xlab<-c(1)
ylab<-c(2)

#-------- Plotting timeseries data for Profit--------------------------
EUConsumerProfit_Timeser<-ts(EUConsumerProfit[,ylab[1]])

#------ 3.1.2 Decompose timeseries to see the components with frequency = 5 ---------
EUConsumerProfit_Timeser_Decompose<-ts(EUConsumerProfit[,ylab[1]],frequency=5)
EUConsumerProfit_Timeser_Decompose <- decompose(EUConsumerProfit_Timeser_Decompose)
plot(EUConsumerProfit_Timeser_Decompose)

# Decomposotion showed that:
# 1. Trend in a high wavelength sine curve
# 3. Seasonality is a low wavelength sine curve

# Plot the timeseries
plot(EUConsumerProfit_Timeser)

# Smoothening the curve 
w <-1
EUConsumerProfit_Timeser_Smooth <- stats::filter(EUConsumerProfit_Timeser, 
                                                 filter=rep(1/(2*w+1),(2*w+1)), 
                                                 method='convolution', sides=2)



diff <- EUConsumerProfit_Timeser_Smooth[w+2] - EUConsumerProfit_Timeser_Smooth[w+1]

for (i in seq(w,1,-1)) {
  EUConsumerProfit_Timeser_Smooth[i] <- EUConsumerProfit_Timeser_Smooth[i+1] - diff
}
n <- length(EUConsumerProfit_Timeser)
n
timeseries <- EUConsumerProfit[[xlab[1]]]
timeseries_test <- EUConsumerProfit_Test[[xlab[1]]]

diff <- EUConsumerProfit_Timeser_Smooth[n-w] - EUConsumerProfit_Timeser_Smooth[n-w-1]

for (i in seq(n-w+1, n)) {
  EUConsumerProfit_Timeser_Smooth[i] <- EUConsumerProfit_Timeser_Smooth[i-1] + diff
}
lines(EUConsumerProfit_Timeser_Smooth, col="red", lwd=2)

EUConsumerProfit_Timeser_SmoothHdf <- as.data.frame(cbind(timeseries, as.vector(EUConsumerProfit_Timeser_Smooth)))
colnames(EUConsumerProfit_Timeser_SmoothHdf) <- c('NumOfTheMonth', 'Profit')
EUConsumerProfit_Timeser_SmoothHdf$NumOfTheMonth<-as.numeric(EUConsumerProfit_Timeser_SmoothHdf$NumOfTheMonth)
EUConsumerProfit_Timeser_SmoothHdf$Profit<-as.numeric(EUConsumerProfit_Timeser_SmoothHdf$Profit)

str(EUConsumerProfit_Timeser_SmoothHdf)

str(EUConsumerProfit)

#----------3.1.3 EU Consumer model-------------
lmfit <- lm(EUConsumerProfit_Timeser_SmoothHdf$Profit ~ sin(0.5*EUConsumerProfit_Timeser_SmoothHdf$NumOfTheMonth) *
              poly(EUConsumerProfit_Timeser_SmoothHdf$NumOfTheMonth,2) 
            + cos(0.5*EUConsumerProfit_Timeser_SmoothHdf$NumOfTheMonth) * 
              poly(EUConsumerProfit_Timeser_SmoothHdf$NumOfTheMonth,2)
            + sin(0.05*EUConsumerProfit_Timeser_SmoothHdf$NumOfTheMonth)*
              EUConsumerProfit_Timeser_SmoothHdf$NumOfTheMonth, 
            data=EUConsumerProfit_Timeser_SmoothHdf)
summary(lmfit)
accuracy(lmfit)


trendProfit <- predict(lmfit, data.frame(x=timeseries))

lines(timeseries, trendProfit, col="blue", lwd=2)

#--------3.1.4 Auto Arima EU COnsumer -------------
Data_1 <- EUConsumerProfit
nrow(Data_1)

#Converting dataframe to time series
indata <- Data_1$MonthlyTotalProfit[1:48]
timeser <- ts(indata)
plot(timeser)

#Building final model in R
autoarimafit<- auto.arima(timeser)
in_data_pred<-fitted(autoarimafit)

plot(autoarimafit$x, col="red")
lines(in_data_pred, col="blue")

#--------3.1.5 Evaluating the model using MAPE-------
fcast_auto_arima <- predict(autoarimafit, n.ahead = 6)
outdata <- Data_1$MonthlyTotalProfit[43:48]
outdata
MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata)[5]
MAPE_auto_arima #51.81 Accuarcy

#--------3.5 Forecasting EU Consumer Profit------------------
#---Forecast using normal method-------
EUConsumerProfitForecast <- forecast(EUConsumerProfit_Timeser)
EUConsumerProfitForecast
plot(EUConsumerProfitForecast)

#---Forecast using holts method-------
EUConsumerProfitForecast6months <- forecast::holt(EUConsumerProfit_Timeser,h=6)
EUConsumerProfitForecast6months
plot(EUConsumerProfitForecast6months)

# Comparing with Test data
accuracy(EUConsumerProfitForecast6months)

#-----Using Forecast function to calculate next six month profit-----------
forecast(EUConsumerProfit_Timeser,h=6)
forecast(autoarimafit,h=6)

#-------3.1 EU Consumer---------
EUConsumerSalesSummary<-subset(gsSalesMonthlySummary,(gsSalesMonthlySummary$Market=="EU")&(gsSalesMonthlySummary$Segment=="Consumer"))
EUConsumerSalesSummary<-EUConsumerSalesSummary[order(EUConsumerSalesSummary$OrderDate),]
EUConsumerSalesSummary$NumOfTheMonth<-c(1:nrow(EUConsumerSalesSummary))

#Preparing data for TS forecasting
EUConsumerProfit<-EUConsumerSalesSummary[,c(9,4)]
EUConsumerQuantity<-EUConsumerSalesSummary[,c(9,7)]

#-------- 3.3.1 EU CONSUMER Quantity MODELLEING ----------
#Separating test data
RowCount<-nrow(EUConsumerSalesSummary)
EUConsumerQuantity_Test<-EUConsumerQuantity[(RowCount-10):RowCount,] # Test data
xlab<-c(1)
ylab<-c(2)

#-------- Plotting timeseries data for Quantity--------------------------
EUConsumerQuantity_Timeser<-ts(EUConsumerQuantity[,ylab[1]])

#------ 3.3.2 Decompose timeseries to see the components with frequency = 5 ---------
EUConsumerQuantity_Timeser_Decompose<-ts(EUConsumerQuantity[,ylab[1]],frequency=5)
EUConsumerQuantity_Timeser_Decompose <- decompose(EUConsumerQuantity_Timeser_Decompose)
plot(EUConsumerQuantity_Timeser_Decompose)

# Decomposotion showed that:
# 1. Trend in a high wavelength sine curve
# 3. Seasonality is a low wavelength sine curve

# Plot the timeseries
plot(EUConsumerQuantity_Timeser)

# Smoothening the curve 
w <-1
EUConsumerQuantity_Timeser_Smooth <- stats::filter(EUConsumerQuantity_Timeser, 
                                                   filter=rep(1/(2*w+1),(2*w+1)), 
                                                   method='convolution', sides=2)



diff <- EUConsumerQuantity_Timeser_Smooth[w+2] - EUConsumerQuantity_Timeser_Smooth[w+1]

for (i in seq(w,1,-1)) {
  EUConsumerQuantity_Timeser_Smooth[i] <- EUConsumerQuantity_Timeser_Smooth[i+1] - diff
}
n <- length(EUConsumerQuantity_Timeser)
n
timeseries <- EUConsumerQuantity[[xlab[1]]]
timeseries_test <- EUConsumerQuantity_Test[[xlab[1]]]

diff <- EUConsumerQuantity_Timeser_Smooth[n-w] - EUConsumerQuantity_Timeser_Smooth[n-w-1]

for (i in seq(n-w+1, n)) {
  EUConsumerQuantity_Timeser_Smooth[i] <- EUConsumerQuantity_Timeser_Smooth[i-1] + diff
}
lines(EUConsumerQuantity_Timeser_Smooth, col="red", lwd=2)

EUConsumerQuantity_Timeser_SmoothHdf <- as.data.frame(cbind(timeseries, as.vector(EUConsumerQuantity_Timeser_Smooth)))
colnames(EUConsumerQuantity_Timeser_SmoothHdf) <- c('NumOfTheMonth', 'Quantity')
EUConsumerQuantity_Timeser_SmoothHdf$NumOfTheMonth<-as.numeric(EUConsumerQuantity_Timeser_SmoothHdf$NumOfTheMonth)
EUConsumerQuantity_Timeser_SmoothHdf$Quantity<-as.numeric(EUConsumerQuantity_Timeser_SmoothHdf$Quantity)

str(EUConsumerQuantity_Timeser_SmoothHdf)

str(EUConsumerQuantity)

#----------3.3.3 EU Consumer model-------------
lmfit <- lm(EUConsumerQuantity_Timeser_SmoothHdf$Quantity ~ sin(0.5*EUConsumerQuantity_Timeser_SmoothHdf$NumOfTheMonth) *
              poly(EUConsumerQuantity_Timeser_SmoothHdf$NumOfTheMonth,2) 
            + cos(0.5*EUConsumerQuantity_Timeser_SmoothHdf$NumOfTheMonth) * 
              poly(EUConsumerQuantity_Timeser_SmoothHdf$NumOfTheMonth,2)
            + sin(0.05*EUConsumerQuantity_Timeser_SmoothHdf$NumOfTheMonth)*
              EUConsumerQuantity_Timeser_SmoothHdf$NumOfTheMonth, 
            data=EUConsumerQuantity_Timeser_SmoothHdf)
summary(lmfit)
accuracy(lmfit)


trendQuantity <- predict(lmfit, data.frame(x=timeseries))

lines(timeseries, trendQuantity, col="blue", lwd=2)

#--------3.3.4 Auto Arima EU COnsumer -------------
Data_1 <- EUConsumerQuantity
nrow(Data_1)

#Converting dataframe to time series
indata <- Data_1$MonthlyTotalQuantity[1:48]
timeser <- ts(indata)
plot(timeser)

#Building final model in R
autoarimafit<- auto.arima(timeser)
in_data_pred<-fitted(autoarimafit)

plot(autoarimafit$x, col="red")
lines(in_data_pred, col="blue")

#--------3.3.5 Evaluating the model using MAPE-------
fcast_auto_arima <- predict(autoarimafit, n.ahead = 6)
outdata <- Data_1$MonthlyTotalQuantity[43:48]
outdata
MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata)[5]
MAPE_auto_arima #51.81 Accuarcy

#--------3.5 Forecasting EU Consumer Quantity------------------
#---Forecast using normal method-------
EUConsumerQuantityForecast <- forecast(EUConsumerQuantity_Timeser)
EUConsumerQuantityForecast
plot(EUConsumerQuantityForecast)

#---Forecast using holts method-------
EUConsumerQuantityForecast6months <- forecast::holt(EUConsumerQuantity_Timeser,h=6)
EUConsumerQuantityForecast6months
plot(EUConsumerQuantityForecast6months)

# Comparing with Test data
accuracy(EUConsumerQuantityForecast6months)

#-----Using Forecast function to calculate next six month Quantity-----------
forecast(EUConsumerQuantity_Timeser,h=6)
forecast(autoarimafit,h=6)
