setwd("~/Downloads")

# Import covid_oxfordmanrealizedvolatilityindices.2 dataset before running 

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

fin_crisis <- read.csv("fin_crisis.csv",
                       header = T, 		
                       sep = ",", 			
                       dec = ".",
                       na.strings="null")

head(fin_crisis)
dim(fin_crisis)
tail(fin_crisis)



fin_crisis<-fin_crisis[complete.cases(fin_crisis),]
dim(fin_crisis)

class(fin_crisis)

##### Transform to Time series data the data set of the index 

Date_fin_crisis <- strptime(fin_crisis[,1], "%Y-%m-%d",tz="GMT")
fin_crisis_ts <- as.xts(fin_crisis[,2:ncol(fin_crisis)], Date_fin_crisis)

head(fin_crisis_ts)

class(fin_crisis)
class(fin_crisis_ts)

dim(fin_crisis_ts)
range(time(fin_crisis_ts))

##### Plot closing prices 

plot(fin_crisis_ts$Adj.Close, main="Financial Global Crisis", col="black")

###### We can also obtain the open-to-close daily log-returns 

#log(bitcoin_i$Close) - log(bitcoin_i$Open)


###### Let us obtain the close-to-close daily log-returns

r_t <- makeReturns(fin_crisis_ts$Adj.Close)
N <- length(r_t) 
dim(r_t)      #WHAT IS THE DIFFERENCE BETWEEN LENGHT() AND DIM() ? 

##### Overnight Returns

fin_crisis_ts$OVR <- 0

fin_crisis_ts$OVR<-log(fin_crisis_ts$Open/(lag(fin_crisis_ts$Close))) #Overnight Returns  #WHY DO WE USE LOG OR LAG?
fin_crisis_ts$OVR[1]<-0  #WITH THIS WE ESTABLISHED THAT THE FIRST VALUE ITS ZERO

head(fin_crisis_ts) #As we see, now we have a table for the OVR values

##### Plot of r_t, adding different name and color 

plot(r_t, main="Financial Global Crisis", col="black")

boxplot(r_t,col="red",main="fin_crisis")

## WHAT IS THE DIFFERENCE BETWEEN USING OR NOT COREDATA 

boxplot(coredata(r_t),col="red",main="fin_crisis")


##########################################


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

covid <- read.csv("covid.csv",
                  header = T, 		
                  sep = ",", 			
                  dec = ".",
                  na.strings="null")

head(covid)
dim(covid)
tail(covid)



covid<-covid[complete.cases(covid),]
dim(covid)

class(covid)

##### Transform to Time series data the data set of the index 

Date_covid <- strptime(covid[,1], "%Y-%m-%d",tz="GMT")
covid_ts <- as.xts(covid[,2:ncol(covid)], Date_covid)

head(covid_ts)

class(covid)
class(covid_ts)

dim(covid_ts)
range(time(covid_ts))

##### Plot closing prices 

plot(covid_ts$Adj.Close, main="COVID-19", col="black")

###### We can also obtain the open-to-close daily log-returns 

#log(bitcoin_i$Close) - log(bitcoin_i$Open)


###### Let us obtain the close-to-close daily log-returns

r_t <- makeReturns(covid_ts$Adj.Close)
N <- length(r_t) 
dim(r_t)      #WHAT IS THE DIFFERENCE BETWEEN LENGHT() AND DIM() ? 

##### Overnight Returns

covid_ts$OVR <- 0

covid_ts$OVR<-log(covid_ts$Open/(lag(covid_ts$Close))) #Overnight Returns  #WHY DO WE USE LOG OR LAG?
covid_ts$OVR[1]<-0  #WITH THIS WE ESTABLISHED THAT THE FIRST VALUE ITS ZERO

head(covid_ts) #As we see, now we have a table for the OVR values

##### Plot of r_t, adding different name and color 

plot(r_t, main="COVID-19", col="black")

boxplot(r_t,col="red",main="covid")

## WHAT IS THE DIFFERENCE BETWEEN USING OR NOT COREDATA 

boxplot(coredata(r_t),col="red",main="covid")