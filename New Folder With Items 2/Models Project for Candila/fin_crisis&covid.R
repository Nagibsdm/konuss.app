
setwd("~/Downloads")

library("xts")
library("highfrequency")
library("tseries")
library("fBasics")
library("rugarch") 
library("maxLik")
library("rumidas")
library("dccmidas")
library("fpp2")
library("ggplot2")
library("rqmidas")
library("GGally")
library("GAS")
library("xtable")


##################################################
#              GLOBAL FINANCIAL CRISIS           #
##################################################

ssmi_fin_crisis <- read.csv("fin_crisis.csv",
                 header = T, 		
                 sep = ",", 			
                 dec = ".",
                 na.strings="null")

head(ssmi_fin_crisis)
dim(ssmi_fin_crisis)
tail(ssmi_fin_crisis)



ssmi_fin_crisis<-ssmi_fin_crisis[complete.cases(ssmi_fin_crisis),]
dim(ssmi_fin_crisis)

class(ssmi_fin_crisis)

##### Transform to Time series data the data set of the index 

Date_ssmi_fin_crisis <- strptime(ssmi_fin_crisis[,1], "%Y-%m-%d",tz="GMT")
ssmi_fin_crisis_ts <- as.xts(ssmi_fin_crisis[,2:ncol(ssmi_fin_crisis)], Date_ssmi_fin_crisis)

head(ssmi_fin_crisis_ts)

class(ssmi_fin_crisis)
class(ssmi_fin_crisis_ts)

dim(ssmi_fin_crisis_ts)
range(time(ssmi_fin_crisis_ts))

##### Plot closing prices 

plot(ssmi_fin_crisis_ts$Adj.Close, main="SSMI Financial Crisis", col="black")


###### Let us obtain the close-to-close daily log-returns

r_t_fin_crisis <- makeReturns(ssmi_fin_crisis_ts$Adj.Close)
N <- length(r_t_fin_crisis) 
dim(r_t_fin_crisis)     

##### Plot of r_t, adding different name and color 

plot(r_t_fin_crisis, main="SSMI Financial Crisis", col="black")

##### Let's now calculate the Stylized facts that we learn in class 

skewness(r_t_fin_crisis)
kurtosis(r_t_fin_crisis)

summary(r_t_fin_crisis)

Box.test(r_t_fin_crisis,type="Ljung-Box",lag=5) 
Box.test(r_t_fin_crisis,type="Box-Pierce",lag=5)

Box.test(r_t_fin_crisis^2,type="Ljung-Box", lag=5)
Box.test(r_t_fin_crisis^2,type="Box-Pierce",lag=5)

jarque.bera.test(r_t_fin_crisis)

adf.test(r_t_fin_crisis)


##################################################
#                    COVID-19                    #
##################################################

ssmi_covid <- read.csv("covid.csv",
                       header = T, 		
                       sep = ",", 			
                       dec = ".",
                       na.strings="null")

head(ssmi_covid)
dim(ssmi_covid)
tail(ssmi_covid)



ssmi_covid<-ssmi_covid[complete.cases(ssmi_covid),]
dim(ssmi_covid)

class(ssmi_covid)

##### Transform to Time series data the data set of the index 

Date_ssmi_covid <- strptime(ssmi_covid[,1], "%Y-%m-%d",tz="GMT")
ssmi_covid_ts <- as.xts(ssmi_covid[,2:ncol(ssmi_covid)], Date_ssmi_covid)

head(ssmi_covid_ts)

class(ssmi_covid)
class(ssmi_covid_ts)

dim(ssmi_covid_ts)
range(time(ssmi_covid_ts))

##### Plot closing prices 

plot(ssmi_covid_ts$Adj.Close, main="SSMI COVID-19", col="black")


###### Let us obtain the close-to-close daily log-returns

r_t_covid <- makeReturns(ssmi_covid_ts$Adj.Close)
N <- length(r_t_covid) 
dim(r_t_covid)     

##### Plot of r_t, adding different name and color 

plot(r_t_covid, main="SSMI COVID-19", col="black")

##### Let's now calculate the Stylized facts that we learn in class 

skewness(r_t_covid)
kurtosis(r_t_covid)

summary(r_t_covid)

Box.test(r_t_covid,type="Ljung-Box",lag=5) 
Box.test(r_t_covid,type="Box-Pierce",lag=5)

Box.test(r_t_covid^2,type="Ljung-Box", lag=5)
Box.test(r_t_covid^2,type="Box-Pierce",lag=5)

jarque.bera.test(r_t_covid)

adf.test(r_t_covid)
