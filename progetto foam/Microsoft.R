
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

## Import the data set to R

msft <- read.csv("MSFT.csv",
                      header = T, 		
                      sep = ",", 			
                      dec = ".",
                      na.strings="null")

##### Transform to Time series data the data set of the index 

Date_msft <- strptime(msft[,1], "%Y-%m-%d",tz="GMT")
msft_ts <- as.xts(msft[,2:ncol(msft)], Date_msft)

head(msft_ts)

class(msft)
class(msft_ts)

dim(msft_ts)
range(time(msft_ts))

msft<-msft[complete.cases(msft),] 


##### Plot closing prices 

plot(msft_ts$Adj.Close, main="MSFT", col="black")


###### Let us obtain the close-to-close daily log-returns

r_t <- makeReturns(msft_ts$Adj.Close)
N <- length(r_t) 
dim(r_t)     

##### Plot of r_t, adding different name and color 

plot(r_t, main="MSFT", col="black")

boxplot(coredata(r_t),col="red",main="MSFT")

##### ACF and PACF

#par(mfrow=c(2,1))   ### For showing two plots

acf(coredata(r_t), main="Returns")
pacf(coredata(r_t), main="Returns")

##### ACF on squared returns 

acf(coredata(r_t^2), main="Squared Returns")
pacf(coredata(r_t^2), main="Squared Returns")

# Absolute returns

acf(coredata(abs(r_t)), main="Absolute Returns")
pacf(coredata(abs(r_t)), main="Absolute Returns")

##### Let's now calculate the Stylized facts that we learn in class 

skewness(r_t)
kurtosis(r_t)

qqnorm(r_t)
qqline(r_t, col = "red")

summary(r_t)

##################################################################

hist(r_t, freq = FALSE, ylim = c(0,60), breaks = 40)
curve(dnorm(x, mean=mean(r_t), sd=sd(r_t)), add=TRUE, col="red")

# lag = 5

Box.test(r_t,type="Ljung-Box",lag=5)  
Box.test(r_t,type="Box-Pierce",lag=5)

Box.test(r_t^2,type="Ljung-Box", lag=5)
Box.test(r_t^2,type="Box-Pierce",lag=5)

# lag = 50

Box.test(r_t,type="Ljung-Box",lag=50)  
Box.test(r_t,type="Box-Pierce",lag=50)

Box.test(r_t^2,type="Ljung-Box", lag=50)
Box.test(r_t^2,type="Box-Pierce",lag=50)

# lag = 100

Box.test(r_t,type="Ljung-Box",lag=100)  
Box.test(r_t,type="Box-Pierce",lag=100)

Box.test(r_t^2,type="Ljung-Box", lag=100)
Box.test(r_t^2,type="Box-Pierce",lag=100)

# lag = 200

Box.test(r_t,type="Ljung-Box",lag=200)  
Box.test(r_t,type="Box-Pierce",lag=200)

Box.test(r_t^2,type="Ljung-Box", lag=200)
Box.test(r_t^2,type="Box-Pierce",lag=200)


jarque.bera.test(r_t)

adf.test(r_t)


##############################

#ESTIMATION OF GARCH MODELS 

##############################


##################################################
#               Parametric methods               #
################################################## 


r_t_est<-r_t['2012/2022']
dim(r_t_est)

##################################################

tau <- 0.05

################################
### Estimate sGARCH (1,1) - norm 

spec_garch_11_norm <- ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)), 
                                 mean.model=list(armaOrder=c(0,0), include.mean=FALSE),  
                                 distribution.model="norm")

fit_garch_11_norm<-ugarchfit(spec=spec_garch_11_norm, data=r_t_est)

### estimated coefficients

fit_garch_11_norm@fit$matcoef
round(fit_garch_11_norm@fit$matcoef,3)
round(coef(fit_garch_11_norm),3)

### conditional volatility

cond_vol_garch_11_norm<-fit_garch_11_norm@fit$sigma

##
VaR_garch_11_norm <- qnorm(tau)*fit_garch_11_norm@fit$sigma
ES_garch_11_norm <- -fit_garch_11_norm@fit$sigma*dnorm(qnorm(tau))/tau

### short evaluation:

sum(ifelse(r_t_est<VaR_garch_11_norm,1,0))/N

################################
#########################
### gjrGARCH (1,1) - norm 

spec_gjr_garch_11_norm <- ugarchspec(variance.model=list(model="gjrGARCH", garchOrder=c(1,1)), 
                                     mean.model=list(armaOrder=c(0,0), include.mean=FALSE),  
                                     distribution.model="norm")

fit_gjr_garch_11_norm<-ugarchfit(spec=spec_gjr_garch_11_norm, data=r_t_est)

### estimated coefficients

fit_gjr_garch_11_norm@fit$matcoef
round(fit_gjr_garch_11_norm@fit$matcoef,3)
round(coef(fit_gjr_garch_11_norm),3)

### conditional volatility

cond_vol_gjr_garch_11_norm<-fit_gjr_garch_11_norm@fit$sigma

##
VaR_gjr_garch_11_norm <- qnorm(tau)*fit_gjr_garch_11_norm@fit$sigma
ES_gjr_garch_11_norm <- -fit_gjr_garch_11_norm@fit$sigma*dnorm(qnorm(tau))/tau

### short evaluation:

sum(ifelse(r_t_est<VaR_gjr_garch_11_norm,1,0))/N

#######################
### eGARCH (1,1) - norm 

spec_e_garch_11_norm <- ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(1,1)), 
                                   mean.model=list(armaOrder=c(0,0), include.mean=FALSE),  
                                   distribution.model="norm")

fit_e_garch_11_norm<-ugarchfit(spec=spec_e_garch_11_norm, data=r_t_est)

### estimated coefficients

fit_e_garch_11_norm@fit$matcoef
round(fit_e_garch_11_norm@fit$matcoef,3)
round(coef(fit_e_garch_11_norm),3)

### conditional volatility

cond_vol_e_garch_11_norm<-fit_e_garch_11_norm@fit$sigma

###
VaR_e_garch_11_norm  <- qnorm(tau)*fit_e_garch_11_norm@fit$sigma
ES_e_garch_11_norm  <- -fit_e_garch_11_norm@fit$sigma*dnorm(qnorm(tau))/tau

### short evaluation:

sum(ifelse(r_t_est<VaR_e_garch_11_norm,1,0))/N 

#######################
### csGARCH (1,1) - norm 

spec_cs_garch_11_norm <- ugarchspec(variance.model=list(model="csGARCH", garchOrder=c(1,1)), 
                                    mean.model=list(armaOrder=c(0,0), include.mean=FALSE),  
                                    distribution.model="norm")

fit_cs_garch_11_norm<-ugarchfit(spec=spec_cs_garch_11_norm, data=r_t_est)

### estimated coefficients

fit_cs_garch_11_norm@fit$matcoef
round(fit_cs_garch_11_norm@fit$matcoef,3)
round(coef(fit_cs_garch_11_norm),3)

### conditional volatility

cond_vol_cs_garch_11_norm<-fit_cs_garch_11_norm@fit$sigma

###
VaR_cs_garch_11_norm <- qnorm(tau)*fit_cs_garch_11_norm@fit$sigma
ES_cs_garch_11_norm  <- -fit_cs_garch_11_norm@fit$sigma*dnorm(qnorm(tau))/tau

### short evaluation:

sum(ifelse(r_t_est<VaR_cs_garch_11_norm ,1,0))/N


################# Student


################################
### sGARCH (1,1) - std

spec_garch_11_std <- ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)), 
                                mean.model=list(armaOrder=c(0,0), include.mean=FALSE),  
                                distribution.model="std")

fit_garch_11_std<-ugarchfit(spec=spec_garch_11_std, data=r_t_est)

### estimated coefficients

fit_garch_11_std@fit$matcoef
round(fit_garch_11_std@fit$matcoef,3)
round(coef(fit_garch_11_std),3)

### conditional volatility

cond_vol_garch_11_std<-fit_garch_11_std@fit$sigma

########################################

df<-as.numeric(fit_garch_11_std@fit$coef['shape'])

correc<-((df-2)/df)^0.5

VaR_garch_11_std<-qt(tau,df)*fit_garch_11_std@fit$sigma*correc

## first term:
first_term<-(dt(qt(tau,df),df)/tau)

## second term:
second_term<- (df+(qt(tau,df))^2)/(df-1)

ES_garch_11_std<- -fit_garch_11_std@fit$sigma*first_term*second_term*correc

### short evaluation:

sum(ifelse(r_t_est<VaR_garch_11_std,1,0))/N


########################
### gjrGARCH (1,1) - std

spec_gjr_garch_11_std <- ugarchspec(variance.model=list(model="gjrGARCH", garchOrder=c(1,1)), 
                                    mean.model=list(armaOrder=c(0,0), include.mean=FALSE),  
                                    distribution.model="std")

fit_gjr_garch_11_std<-ugarchfit(spec=spec_gjr_garch_11_std, data=r_t_est)

### estimated coefficients

fit_gjr_garch_11_std@fit$matcoef
round(fit_gjr_garch_11_std@fit$matcoef,3)
round(coef(fit_gjr_garch_11_std),3)


### conditional volatility

cond_vol_gjr_garch_11_std<-fit_gjr_garch_11_std@fit$sigma


########################################

df<-as.numeric(fit_gjr_garch_11_std@fit$coef['shape'])

correc<-((df-2)/df)^0.5

VaR_gjr_garch_11_std<-qt(tau,df)*fit_gjr_garch_11_std@fit$sigma*correc

## first term:
first_term<-(dt(qt(tau,df),df)/tau)

## second term:
second_term<- (df+(qt(tau,df))^2)/(df-1)

ES_gjr_garch_11_std<- -fit_gjr_garch_11_std@fit$sigma*first_term*second_term*correc

### short evaluation: 

sum(ifelse(r_t_est<VaR_gjr_garch_11_std,1,0))/N


######################
### eGARCH (1,1) - std 

spec_e_garch_11_std <- ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(1,1)), 
                                  mean.model=list(armaOrder=c(0,0), include.mean=FALSE),  
                                  distribution.model="std")

fit_e_garch_11_std <- ugarchfit(spec=spec_e_garch_11_std, data=r_t_est)

### estimated coefficients

fit_e_garch_11_std@fit$matcoef
round(fit_e_garch_11_std@fit$matcoef,3)
round(coef(fit_e_garch_11_std),3)

### conditional volatility

cond_vol_e_garch_11_std<-fit_e_garch_11_std@fit$sigma

########################################

df<-as.numeric(fit_e_garch_11_std@fit$coef['shape'])

correc<-((df-2)/df)^0.5

VaR_e_garch_11_std<-qt(tau,df)*fit_e_garch_11_std@fit$sigma*correc

## first term:
first_term<-(dt(qt(tau,df),df)/tau)

## second term:
second_term<- (df+(qt(tau,df))^2)/(df-1)

ES_e_garch_11_std<- -fit_e_garch_11_std@fit$sigma*first_term*second_term*correc

### short evaluation: 

sum(ifelse(r_t_est<VaR_e_garch_11_std,1,0))/N

#######################
### csGARCH (1,1) - std

spec_cs_garch_11_std <- ugarchspec(variance.model=list(model="csGARCH", garchOrder=c(1,1)), 
                                   mean.model=list(armaOrder=c(0,0), include.mean=FALSE),  
                                   distribution.model="std")

fit_cs_garch_11_std <- ugarchfit(spec=spec_cs_garch_11_std, data=r_t_est)

### estimated coefficients

fit_cs_garch_11_std@fit$matcoef
round(fit_cs_garch_11_std@fit$matcoef,3)
round(coef(fit_cs_garch_11_std),3)

### conditional volatility

cond_vol_cs_garch_11_std<-fit_cs_garch_11_std@fit$sigma


########################################

df<-as.numeric(fit_cs_garch_11_std@fit$coef['shape'])

correc<-((df-2)/df)^0.5

VaR_cs_garch_11_std<-qt(tau,df)*fit_cs_garch_11_std@fit$sigma*correc

## first term:
first_term<-(dt(qt(tau,df),df)/tau)

## second term:
second_term<- (df+(qt(tau,df))^2)/(df-1)

ES_cs_garch_11_std<- -fit_cs_garch_11_std@fit$sigma*first_term*second_term*correc

### short evaluation: 

sum(ifelse(r_t_est<VaR_cs_garch_11_std,1,0))/N



##############################################################################
##############################################################################
##############################################################################

VaR_full<-cbind(VaR_garch_11_norm, VaR_gjr_garch_11_norm, VaR_e_garch_11_norm, VaR_cs_garch_11_norm,
                VaR_garch_11_std, VaR_gjr_garch_11_std, VaR_e_garch_11_std, VaR_cs_garch_11_std)



ES_full<-cbind(ES_garch_11_norm, ES_gjr_garch_11_norm, ES_e_garch_11_norm, ES_cs_garch_11_norm,
               ES_garch_11_std, ES_gjr_garch_11_std, ES_e_garch_11_std, ES_cs_garch_11_std)


##########################################################
##########################################################
##########################################################
##########################################################

############################################
############################################ BACKTESTING
############################################ VaR

M<-ncol(VaR_full) #number of models

col_res<-matrix(rep(NA,4*M),ncol=4)

for (i in 1:M){
  col_res[i,1]<-as.numeric(BacktestVaR(r_t_est, VaR_full[,i], tau)$AE)
  col_res[i,2]<-as.numeric(BacktestVaR(r_t_est, VaR_full[,i], tau)$LRuc[2])
  col_res[i,3]<-as.numeric(BacktestVaR(r_t_est, VaR_full[,i], tau)$LRcc[2])
  col_res[i,4]<-as.numeric(BacktestVaR(r_t_est, VaR_full[,i], tau, Lags = 4)$DQ[2])
}

rownames(col_res)<-c("sGARCH N",
                     "gjrGARCH N",
                     "eGARCH N",
                     "csGARCH N",
                     "sGARCH Std",
                     "gjrGARCH Std",
                     "eGARCH Std",
                     "csGARCH Std")

# 8 models


colnames(col_res)<-c("AE","UC","CC","DQ")

round(col_res,3)

xtable(col_res,type="latex",digits=3)


############################################
############################################ BACKTESTING
############################################ ES

col_res1<-matrix(rep(NA,3*M),ncol=3)

for (i in 1:M){
  RES<-ESTest(actual=r_t_est, ES=ES_full[,i],VaR=VaR_full[,i],boot=TRUE, n.boot = 2000)
  col_res1[i,1]<-as.numeric(RES$expected.exceed)
  col_res1[i,2]<-as.numeric(RES$actual.exceed)
  col_res1[i,3]<-as.numeric(RES$p.value)
}


rownames(col_res1)<-c("sGARCH N",
                      "gjrGARCH N",
                      "eGARCH N",
                      "csGARCH N",
                      "sGARCH Std",
                      "gjrGARCH Std",
                      "eGARCH Std",
                      "csGARCH Std")

# 30 models

colnames(col_res1)<-c("Expected Exc.","Actual Exc.","P-value")

round(col_res1,3)

xtable(col_res1,type="latex",digits=3)

#############################################
############################################# MCS
#############################################

loss_M1<-QL(r_t_est,VaR_full[,1],tau)
loss_M2<-QL(r_t_est,VaR_full[,2],tau)
loss_M3<-QL(r_t_est,VaR_full[,3],tau)
loss_M4<-QL(r_t_est,VaR_full[,4],tau)
loss_M5<-QL(r_t_est,VaR_full[,5],tau)
loss_M6<-QL(r_t_est,VaR_full[,6],tau)
loss_M7<-QL(r_t_est,VaR_full[,7],tau)
loss_M8<-QL(r_t_est,VaR_full[,8],tau)



db_loss<-cbind(
  loss_M1,
  loss_M2,
  loss_M3,
  loss_M4,
  loss_M5,
  loss_M6,
  loss_M7,
  loss_M8)

summary(db_loss)
colnames(db_loss)<-rownames(col_res) 


############################################# MCS 

########## 

alpha<-0.25
B<-1000

MCS_est<-mcsTest(coredata(db_loss), 
                 alpha = alpha, 
                 nboot = B, 
                 nblock = 30,
                 boot = c("block"))


col_means<-colMeans(coredata(db_loss))

dim(db_loss)

MCS_est$includedSQ

col_mcs<-cbind(
  round(col_means*100,3),
  ifelse(1:ncol(db_loss) %in% MCS_est$includedSQ,1,0)
)

row_mcs_f<-cbind(paste(
  ifelse(col_mcs[,2]==1,"cellcolor{gray!75}",""),
  col_mcs[,1],sep="")
)

rownames(row_mcs_f)<-colnames(db_loss)

xtable(row_mcs_f,type="latex",digits=3)

############################################
############################################ MCS
############################################ VaR + ES

loss_M1<-FZLoss(r_t_est,VaR_full[,1],ES_full[,1],tau)
loss_M2<-FZLoss(r_t_est,VaR_full[,2],ES_full[,2],tau)
loss_M3<-FZLoss(r_t_est,VaR_full[,3],ES_full[,3],tau)
loss_M4<-FZLoss(r_t_est,VaR_full[,4],ES_full[,4],tau)
loss_M5<-FZLoss(r_t_est,VaR_full[,5],ES_full[,5],tau)
loss_M6<-FZLoss(r_t_est,VaR_full[,6],ES_full[,6],tau)
loss_M7<-FZLoss(r_t_est,VaR_full[,7],ES_full[,7],tau)
loss_M8<-FZLoss(r_t_est,VaR_full[,8],ES_full[,8],tau)



db_loss<-cbind(
  loss_M1,
  loss_M2,
  loss_M3,
  loss_M4,
  loss_M5,
  loss_M6,
  loss_M7,
  loss_M8)

colnames(db_loss)<-rownames(col_res1)[1:8]


############################################# MCS 

alpha<-0.25
B<-1000

MCS_est<-mcsTest(coredata(db_loss), 
                 alpha = alpha, 
                 nboot = B, 
                 nblock = 30,
                 boot = c("block"))

col_means<-colMeans(coredata(db_loss))

dim(db_loss)

MCS_est$includedSQ

col_mcs<-cbind(
  round(col_means,3),
  ifelse(1:ncol(db_loss) %in% MCS_est$includedSQ,1,0)
)

row_mcs_f<-cbind(paste(
  ifelse(col_mcs[,2]==1,"cellcolor{gray!75}",""),
  col_mcs[,1],sep="")
)

rownames(row_mcs_f)<-colnames(db_loss)

xtable(row_mcs_f,type="latex",digits=3)

