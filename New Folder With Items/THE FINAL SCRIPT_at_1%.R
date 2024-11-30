
setwd("~/Desktop")

library("xts")
library("highfrequency")
library("tseries")
library("fBasics")
library("rugarch") 
library("maxLik")
library("rumidas")
library("fpp2")
library("ggplot2")
library("rqmidas")
library("GGally")
library("GAS")
library("xtable")

ssmi <- read.csv("SSMI.csv",
                 header = T, 		
                 sep = ",", 			
                 dec = ".",
                 na.strings="null")

head(ssmi)
dim(ssmi)
tail(ssmi)

ssmi<-ssmi[complete.cases(ssmi),]
dim(ssmi)

class(ssmi)

##### Transform to Time series data the data set of the index 

Date_ssmi <- strptime(ssmi[,1], "%Y-%m-%d",tz="GMT")
ssmi_ts <- as.xts(ssmi[,2:ncol(ssmi)], Date_ssmi)

head(ssmi_ts)

class(ssmi)
class(ssmi_ts)

dim(ssmi_ts)
range(time(ssmi_ts))

##### Plot closing prices 

plot(ssmi_ts$Adj.Close, main= "SSMI", col= "black")

###### Let us obtain the close-to-close daily log-returns

r_t <- makeReturns(ssmi_ts$Adj.Close)
N <- length(r_t) 
dim(r_t)    

##### Plot of r_t, adding different name and color 

plot(r_t, main="SSMI", col="black")
boxplot(coredata(r_t),col="red",main="SSMI")

##### ACF and PACF

par(mfrow=c(2,1))   ### For showing two plots

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

summary(ssmi_ts)

##################################################################

hist(r_t, freq = FALSE, ylim = c(0,60), breaks = 40) 
curve(dnorm(x, mean=mean(r_t), sd=sd(r_t)), add=TRUE, col="red")

Box.test(r_t,type="Ljung-Box",lag=5)  
Box.test(r_t,type="Box-Pierce",lag=5)

Box.test(r_t^2,type="Ljung-Box", lag=5)
Box.test(r_t^2,type="Box-Pierce",lag=5)

jarque.bera.test(r_t)

adf.test(r_t)

adf.test(swiss_real_gdp_ts)
adf.test(swiss_bonds_ts)
adf.test(swiss_unemployment_ts)


##############################

#ESTIMATION OF GARCH MODELS 

##############################


##################################################
#               Parametric methods               #
################################################## 


r_t_est<-r_t['2000/2020']
dim(r_t_est)

##################################################

tau <- 0.01

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


###############################################################
###############################################################

source(file="functions.R")

##### Unemployment

swiss_unemployment <- read.csv("unemployment.csv",
                               header = T, 		
                               sep = ",", 			
                               dec = ".",
                               na.strings="null")

##### Transform to Time series data 

Date_swiss_unemployment <- strptime(swiss_unemployment[,1], "%Y-%m-%d",tz="GMT")
swiss_unemployment_ts <- as.xts(swiss_unemployment[,2:ncol(swiss_unemployment)], Date_swiss_unemployment)

swiss_unemployment_ts

range(time(swiss_unemployment_ts))
range(time(ssmi_ts))

##################### plot of unemployment 

plot(swiss_unemployment_ts)

##################### let us obtain the close-to-close daily log-returns

r_t <- makeReturns(ssmi_ts$Adj.Close)

##################### let us focus only on the period  2000/2020

r_t_est<-r_t['2000/2020']
N<-length(r_t_est)

##################### transformation of nai_i into a matrix

K<-3 # number of lagged realizations entering the long-run equation

################################################################################

RV_quarterly_sum <- apply.quarterly(swiss_unemployment_ts,sum)

RV_quarterly <- as.xts(coredata(RV_quarterly_sum),seq(as.Date("1999-01-01"), 
                                                      by = "quarter", length.out = length(RV_quarterly_sum)))

unemployment_mv <- mv_into_mat(r_t_est['2000/2020'],diff(RV_quarterly),K=K,type="quarterly")

##################################################
################################################## VaR and ES settings
##################################################

#########################################################################################
##################### estimate a GARCH-MIDAS, without skew parameter, normal distribution

unemployment_fit_gm_norm <- ugmfit(model="GM",skew="NO",distribution="norm",
                                   daily_ret=r_t_est,mv_m=unemployment_mv,K=K)
unemployment_fit_gm_norm
summary.rumidas(unemployment_fit_gm_norm)
names(unemployment_fit_gm_norm)
unemployment_fit_gm_norm$inf_criteria

unemployment_M1_vol<-unemployment_fit_gm_norm$est_vol_in_s

###
unemployment_VaR_gm_norm<-qnorm(tau)*unemployment_fit_gm_norm$est_vol_in_s
unemployment_ES_gm_norm <- -unemployment_fit_gm_norm$est_vol_in_s*dnorm(qnorm(tau))/tau

### short evaluation:

sum(ifelse(r_t_est<unemployment_VaR_gm_norm,1,0))/N


##################### estimate a GARCH-MIDAS, with skew parameter, normal distribution

unemployment_fit_gm_s_norm <- ugmfit(model="GM",skew="YES",distribution="norm",
                                     daily_ret=r_t_est,mv_m=unemployment_mv,K=K)  
unemployment_fit_gm_s_norm
summary.rumidas(unemployment_fit_gm_s_norm)
names(unemployment_fit_gm_s_norm)
unemployment_fit_gm_s_norm$inf_criteria

unemployment_M2_vol<-unemployment_fit_gm_s_norm$est_vol_in_s

###
unemployment_VaR_gm_s_norm<-qnorm(tau)*unemployment_fit_gm_s_norm$est_vol_in_s
unemployment_ES_gm_s_norm<- -unemployment_fit_gm_s_norm$est_vol_in_s*dnorm(qnorm(tau))/tau

### short evaluation:

sum(ifelse(r_t_est<unemployment_VaR_gm_s_norm,1,0))/N

##################### estimate a GARCH-MIDAS, without skew parameter, student distribution

unemployment_fit_gm_std <- ugmfit(model="GM",skew="NO",distribution="std",
                                  daily_ret=r_t_est,mv_m=unemployment_mv,K=K)
unemployment_fit_gm_std
summary.rumidas(unemployment_fit_gm_std)
names(unemployment_fit_gm_std)
unemployment_fit_gm_std$inf_criteria

unemployment_M3_vol<-unemployment_fit_gm_std$est_vol_in_s

###
df<-as.numeric(unemployment_fit_gm_std$rob_coef_mat[6,1])

correc<-((df-2)/df)^0.5

unemployment_VaR_gm_std<-qt(tau,df)*unemployment_fit_gm_std$est_vol_in_s*correc

## first term:
first_term<-(dt(qt(tau,df),df)/tau)

## second term:
second_term<- (df+(qt(tau,df))^2)/(df-1)

unemployment_ES_gm_std<- -unemployment_fit_gm_std$est_vol_in_s*first_term*second_term*correc

### short evaluation:

sum(ifelse(r_t_est<unemployment_VaR_gm_std,1,0))/N

##################### estimate a GARCH-MIDAS, with skew parameter, student distribution

unemployment_fit_gm_s_std <- ugmfit(model="GM",skew="YES",distribution="std",
                                    daily_ret=r_t_est,mv_m=unemployment_mv,K=K)
unemployment_fit_gm_s_std
summary.rumidas(unemployment_fit_gm_s_std)
names(unemployment_fit_gm_s_std)
unemployment_fit_gm_s_std$inf_criteria

unemployment_M4_vol<-unemployment_fit_gm_s_std$est_vol_in_s

###
df<-as.numeric(unemployment_fit_gm_s_std$rob_coef_mat[7,1])

correc<-((df-2)/df)^0.5

unemployment_VaR_gm_s_std<-qt(tau,df)*unemployment_fit_gm_s_std$est_vol_in_s*correc

## first term:
first_term<-(dt(qt(tau,df),df)/tau)

## second term:
second_term<- (df+(qt(tau,df))^2)/(df-1)

unemployment_ES_gm_s_std<- -unemployment_fit_gm_s_std$est_vol_in_s*first_term*second_term*correc

### short evaluation:

sum(ifelse(r_t_est<unemployment_VaR_gm_s_std,1,0))/N

###################################
########## DAGM MODELS ############

##################### estimate a DAGM, without skew parameter, normal distribution

unemployment_fit_dagm_norm <- ugmfit(model="DAGM",skew="NO",distribution="norm",
                                     daily_ret=r_t_est,mv_m=unemployment_mv,K=K) 
unemployment_fit_dagm_norm
summary.rumidas(unemployment_fit_dagm_norm)
names(unemployment_fit_dagm_norm)
unemployment_fit_dagm_norm$inf_criteria

unemployment_M5_vol<-unemployment_fit_dagm_norm$est_vol_in_s

###
unemployment_VaR_dagm_norm<-qnorm(tau)*unemployment_fit_dagm_norm$est_vol_in_s
unemployment_ES_dagm_norm<- -unemployment_fit_dagm_norm$est_vol_in_s*dnorm(qnorm(tau))/tau

### short evaluation:

sum(ifelse(r_t_est<unemployment_VaR_dagm_norm,1,0))/N

##################### estimate a DAGM, with skew parameter, normal distribution 

unemployment_fit_dagm_s_norm <- ugmfit(model="DAGM",skew="YES",distribution="norm",
                                       daily_ret=r_t_est,mv_m=unemployment_mv,K=K)
unemployment_fit_dagm_s_norm
summary.rumidas(unemployment_fit_dagm_s_norm)
names(unemployment_fit_dagm_s_norm)
unemployment_fit_dagm_s_norm$inf_criteria

unemployment_M6_vol<-unemployment_fit_dagm_s_norm$est_vol_in_s

###
unemployment_VaR_dagm_s_norm<-qnorm(tau)*unemployment_fit_dagm_s_norm$est_vol_in_s
unemployment_ES_dagm_s_norm<- -unemployment_fit_dagm_s_norm$est_vol_in_s*dnorm(qnorm(tau))/tau

### short evaluation:

sum(ifelse(r_t_est<unemployment_VaR_dagm_s_norm,1,0))/N


###################################################################

##### real_gdp

swiss_real_gdp <- read.csv("real_gdp.csv",
                           header = T, 		
                           sep = ",", 			
                           dec = ".",
                           na.strings="null")

##### Transform to Time series data 

Date_swiss_real_gdp <- strptime(swiss_real_gdp[,1], "%Y-%m-%d",tz="GMT")
swiss_real_gdp_ts <- as.xts(swiss_real_gdp[,2:ncol(swiss_real_gdp)], Date_swiss_real_gdp)

swiss_real_gdp_ts

range(time(swiss_real_gdp_ts))
range(time(ssmi_ts))

##################### plot of real gdp

plot(swiss_real_gdp_ts)

##################### let us obtain the close-to-close daily log-returns

r_t <- makeReturns(ssmi_ts$Adj.Close)

##################### let us focus only on the period  2000/2020

r_t_est<-r_t['2000/2020']
N<-length(r_t_est)

##################### transformation of nai_i into a matrix

K<-3 # number of lagged realizations entering the long-run equation

################################################################################

RV_quarterly_sum <- apply.quarterly(swiss_real_gdp_ts,sum)

RV_quarterly <- as.xts(coredata(RV_quarterly_sum),seq(as.Date("1999-01-01"), 
                                                      by = "quarter", length.out = length(RV_quarterly_sum)))

real_gdp_mv <- mv_into_mat(r_t_est['2000/2020'],diff(RV_quarterly),K=K,type="quarterly")

##################################################
################################################## VaR and ES settings
##################################################

#########################################################################################
##################### estimate a GARCH-MIDAS, without skew parameter, normal distribution

real_gdp_fit_gm_norm <- ugmfit(model="GM",skew="NO",distribution="norm",
                               daily_ret=r_t_est,mv_m=real_gdp_mv,K=K)
real_gdp_fit_gm_norm
summary.rumidas(real_gdp_fit_gm_norm)
names(real_gdp_fit_gm_norm)
real_gdp_fit_gm_norm$inf_criteria

real_gdp_M1_vol<-real_gdp_fit_gm_norm$est_vol_in_s

###
real_gdp_VaR_gm_norm<-qnorm(tau)*real_gdp_fit_gm_norm$est_vol_in_s
real_gdp_ES_gm_norm <- -real_gdp_fit_gm_norm$est_vol_in_s*dnorm(qnorm(tau))/tau

### short evaluation:

sum(ifelse(r_t_est<real_gdp_VaR_gm_norm,1,0))/N


##################### estimate a GARCH-MIDAS, with skew parameter, normal distribution

real_gdp_fit_gm_s_norm <- ugmfit(model="GM",skew="YES",distribution="norm",
                                 daily_ret=r_t_est,mv_m=real_gdp_mv,K=K)  
real_gdp_fit_gm_s_norm
summary.rumidas(real_gdp_fit_gm_s_norm)
names(real_gdp_fit_gm_s_norm)
real_gdp_fit_gm_s_norm$inf_criteria

real_gdp_M2_vol<-real_gdp_fit_gm_s_norm$est_vol_in_s

###
real_gdp_VaR_gm_s_norm<-qnorm(tau)*real_gdp_fit_gm_s_norm$est_vol_in_s
real_gdp_ES_gm_s_norm<- -real_gdp_fit_gm_s_norm$est_vol_in_s*dnorm(qnorm(tau))/tau

### short evaluation:

sum(ifelse(r_t_est<real_gdp_VaR_gm_s_norm,1,0))/N

##################### estimate a GARCH-MIDAS, without skew parameter, student distribution

real_gdp_fit_gm_std <- ugmfit(model="GM",skew="NO",distribution="std",
                              daily_ret=r_t_est,mv_m=real_gdp_mv,K=K)
real_gdp_fit_gm_std
summary.rumidas(real_gdp_fit_gm_std)
names(real_gdp_fit_gm_std)
real_gdp_fit_gm_std$inf_criteria

real_gdp_M3_vol<-real_gdp_fit_gm_std$est_vol_in_s

###
df<-as.numeric(real_gdp_fit_gm_std$rob_coef_mat[6,1])

correc<-((df-2)/df)^0.5

real_gdp_VaR_gm_std<-qt(tau,df)*real_gdp_fit_gm_std$est_vol_in_s*correc

## first term:
first_term<-(dt(qt(tau,df),df)/tau)

## second term:
second_term<- (df+(qt(tau,df))^2)/(df-1)

real_gdp_ES_gm_std<- -real_gdp_fit_gm_std$est_vol_in_s*first_term*second_term*correc

### short evaluation:

sum(ifelse(r_t_est<real_gdp_VaR_gm_std,1,0))/N

##################### estimate a GARCH-MIDAS, with skew parameter, student distribution

real_gdp_fit_gm_s_std <- ugmfit(model="GM",skew="YES",distribution="std",
                                daily_ret=r_t_est,mv_m=real_gdp_mv,K=K)
real_gdp_fit_gm_s_std
summary.rumidas(real_gdp_fit_gm_s_std)
names(real_gdp_fit_gm_s_std)
real_gdp_fit_gm_s_std$inf_criteria

real_gdp_M4_vol<-real_gdp_fit_gm_s_std$est_vol_in_s

###
df<-as.numeric(real_gdp_fit_gm_s_std$rob_coef_mat[7,1])

correc<-((df-2)/df)^0.5

real_gdp_VaR_gm_s_std<-qt(tau,df)*real_gdp_fit_gm_s_std$est_vol_in_s*correc

## first term:
first_term<-(dt(qt(tau,df),df)/tau)

## second term:
second_term<- (df+(qt(tau,df))^2)/(df-1)

real_gdp_ES_gm_s_std<- -real_gdp_fit_gm_s_std$est_vol_in_s*first_term*second_term*correc

### short evaluation:

sum(ifelse(r_t_est<real_gdp_VaR_gm_s_std,1,0))/N

###################################
########## DAGM MODELS ############

##################### estimate a DAGM, without skew parameter, normal distribution

real_gdp_fit_dagm_norm <- ugmfit(model="DAGM",skew="NO",distribution="norm",
                                 daily_ret=r_t_est,mv_m=real_gdp_mv,K=K) 
real_gdp_fit_dagm_norm
summary.rumidas(real_gdp_fit_dagm_norm)
names(real_gdp_fit_dagm_norm)
real_gdp_fit_dagm_norm$inf_criteria

real_gdp_M5_vol<-real_gdp_fit_dagm_norm$est_vol_in_s

###
real_gdp_VaR_dagm_norm<-qnorm(tau)*real_gdp_fit_dagm_norm$est_vol_in_s
real_gdp_ES_dagm_norm<- -real_gdp_fit_dagm_norm$est_vol_in_s*dnorm(qnorm(tau))/tau

### short evaluation:

sum(ifelse(r_t_est<real_gdp_VaR_dagm_norm,1,0))/N

##################### estimate a DAGM, with skew parameter, normal distribution ## Do not work

real_gdp_fit_dagm_s_norm <- ugmfit(model="DAGM",skew="YES",distribution="norm",
                                   daily_ret=r_t_est,mv_m=real_gdp_mv,K=K)
real_gdp_fit_dagm_s_norm
summary.rumidas(real_gdp_fit_dagm_s_norm)
names(real_gdp_fit_dagm_s_norm)
real_gdp_fit_dagm_s_norm$inf_criteria

real_gdp_M6_vol<-real_gdp_fit_dagm_s_norm$est_vol_in_s

###
real_gdp_VaR_dagm_s_norm<-qnorm(tau)*real_gdp_fit_dagm_s_norm$est_vol_in_s
real_gdp_ES_dagm_s_norm<- -real_gdp_fit_dagm_s_norm$est_vol_in_s*dnorm(qnorm(tau))/tau

### short evaluation:

sum(ifelse(r_t_est<real_gdp_VaR_dagm_s_norm,1,0))/N

##################################################################################
##### Bonds Yield 10 Years

swiss_bonds <- read.csv("bonds.csv",
                        header = T, 		
                        sep = ",", 			
                        dec = ".",
                        na.strings="null")

##### Transform to Time series data 

Date_swiss_bonds <- strptime(swiss_bonds[,1], "%Y-%m-%d",tz="GMT")
swiss_bonds_ts <- as.xts(swiss_bonds[,2:ncol(swiss_bonds)], Date_swiss_bonds)

swiss_bonds_ts

range(time(swiss_bonds_ts))
range(time(ssmi_ts))

##################### plot of Bonds

plot(swiss_bonds_ts)

##################### let us obtain the close-to-close daily log-returns

r_t <- makeReturns(ssmi_ts$Adj.Close)

##################### let us focus only on the period  2000/2020

r_t_est<-r_t['2000/2020']
N<-length(r_t_est)

##################### transformation of nai_i into a matrix

K<-3 # number of lagged realizations entering the long-run equation

################################################################################

RV_quarterly_sum <- apply.quarterly(swiss_bonds_ts,sum)

RV_quarterly <- as.xts(coredata(RV_quarterly_sum),seq(as.Date("1999-01-01"), 
                                                      by = "quarter", length.out = length(RV_quarterly_sum)))

bonds_mv <- mv_into_mat(r_t_est['2000/2020'],diff(RV_quarterly),K=K,type="quarterly")

##################################################
################################################## VaR and ES settings
##################################################

##################### estimate a GARCH-MIDAS, without skew parameter, normal distribution

bonds_fit_gm_norm <- ugmfit(model="GM",skew="NO",distribution="norm",
                            daily_ret=r_t_est,mv_m=bonds_mv,K=K)
bonds_fit_gm_norm
summary.rumidas(bonds_fit_gm_norm)
names(bonds_fit_gm_norm)
bonds_fit_gm_norm$inf_criteria

bonds_M1_vol<-bonds_fit_gm_norm$est_vol_in_s

###
bonds_VaR_gm_norm<-qnorm(tau)*bonds_fit_gm_norm$est_vol_in_s
bonds_ES_gm_norm <- -bonds_fit_gm_norm$est_vol_in_s*dnorm(qnorm(tau))/tau

### short evaluation:

sum(ifelse(r_t_est<bonds_VaR_gm_norm,1,0))/N


##################### estimate a GARCH-MIDAS, with skew parameter, normal distribution

bonds_fit_gm_s_norm <- ugmfit(model="GM",skew="YES",distribution="norm",
                              daily_ret=r_t_est,mv_m=bonds_mv,K=K)  
bonds_fit_gm_s_norm
summary.rumidas(bonds_fit_gm_s_norm)
names(bonds_fit_gm_s_norm)
bonds_fit_gm_s_norm$inf_criteria

bonds_M2_vol<-bonds_fit_gm_s_norm$est_vol_in_s

###
bonds_VaR_gm_s_norm<-qnorm(tau)*bonds_fit_gm_s_norm$est_vol_in_s
bonds_ES_gm_s_norm<- -bonds_fit_gm_s_norm$est_vol_in_s*dnorm(qnorm(tau))/tau

### short evaluation:

sum(ifelse(r_t_est<bonds_VaR_gm_s_norm,1,0))/N

##################### estimate a GARCH-MIDAS, without skew parameter, student distribution

bonds_fit_gm_std <- ugmfit(model="GM",skew="NO",distribution="std",
                           daily_ret=r_t_est,mv_m=bonds_mv,K=K)
bonds_fit_gm_std
summary.rumidas(bonds_fit_gm_std)
names(bonds_fit_gm_std)
bonds_fit_gm_std$inf_criteria

bonds_M3_vol<-bonds_fit_gm_std$est_vol_in_s

###
df<-as.numeric(bonds_fit_gm_std$rob_coef_mat[6,1])

correc<-((df-2)/df)^0.5

bonds_VaR_gm_std<-qt(tau,df)*bonds_fit_gm_std$est_vol_in_s*correc

## first term:
first_term<-(dt(qt(tau,df),df)/tau)

## second term:
second_term<- (df+(qt(tau,df))^2)/(df-1)

bonds_ES_gm_std<- -bonds_fit_gm_std$est_vol_in_s*first_term*second_term*correc

### short evaluation:

sum(ifelse(r_t_est<bonds_VaR_gm_std,1,0))/N


###################################
########## DAGM MODELS ############

##################### estimate a DAGM, without skew parameter, normal distribution

bonds_fit_dagm_norm <- ugmfit(model="DAGM",skew="NO",distribution="norm",
                              daily_ret=r_t_est,mv_m=bonds_mv,K=K) 
bonds_fit_dagm_norm
summary.rumidas(bonds_fit_dagm_norm)
names(bonds_fit_dagm_norm)
bonds_fit_dagm_norm$inf_criteria

bonds_M4_vol<-bonds_fit_dagm_norm$est_vol_in_s

###
bonds_VaR_dagm_norm<-qnorm(tau)*bonds_fit_dagm_norm$est_vol_in_s
bonds_ES_dagm_norm<- -bonds_fit_dagm_norm$est_vol_in_s*dnorm(qnorm(tau))/tau

### short evaluation:

sum(ifelse(r_t_est<bonds_VaR_dagm_norm,1,0))/N

##################### estimate a DAGM, with skew parameter, normal distribution 

bonds_fit_dagm_s_norm <- ugmfit(model="DAGM",skew="YES",distribution="norm",
                                daily_ret=r_t_est,mv_m=bonds_mv,K=K)
bonds_fit_dagm_s_norm
summary.rumidas(bonds_fit_dagm_s_norm)
names(bonds_fit_dagm_s_norm)
bonds_fit_dagm_s_norm$inf_criteria

bonds_M5_vol<-bonds_fit_dagm_s_norm$est_vol_in_s

###
bonds_VaR_dagm_s_norm<-qnorm(tau)*bonds_fit_dagm_s_norm$est_vol_in_s
bonds_ES_dagm_s_norm<- -bonds_fit_dagm_s_norm$est_vol_in_s*dnorm(qnorm(tau))/tau

### short evaluation:

sum(ifelse(r_t_est<bonds_VaR_dagm_s_norm,1,0))/N


########################################################################################
########################################################################################


##################################################
#               VaR and ES settings              #
##################################################


########################################
#  VaR and ES via non-parametric model #
########################################

w<-250 # length of rolling window

HS_250<-hs_f(r_t,w,tau,"2000","2020")

VaR_HS_250<-HS_250$VaR_HS
ES_HS_250<-HS_250$ES_HS

head(VaR_HS_250)
tail(VaR_HS_250)

sum(ifelse(r_t_est<VaR_HS_250,1,0))/N


##################################################
#            Semi-Parametric methods             #
################################################## 

## Indirect GARCH (With ES)

fit_caviar_ig_ES<-ucaviarfit(model="IG", tau, daily_ret=r_t_est, R = 100, B=2, ES="Yes")
fit_caviar_ig_ES
summary.rqmidas(fit_caviar_ig_ES)
VaR_IG_ES<-fit_caviar_ig_ES$VaR
ES_IG_ES<-fit_caviar_ig_ES$ES

sum(ifelse(r_t_est<VaR_IG_ES,1,0))/N


## SAV

fit_sav<-ucaviarfit(model="SAV", tau, daily_ret=r_t_est, R = 100, B=2, ES="Yes")
fit_sav
summary.rqmidas(fit_sav)
VaR_SAV<-fit_sav$VaR
ES_SAV<-fit_sav$ES

sum(ifelse(r_t_est<VaR_SAV,1,0))/N


## AS

fit_as<-ucaviarfit(model="AS", tau, daily_ret=r_t_est, R = 100, B=2, ES="Yes")
fit_as
summary.rqmidas(fit_as)
VaR_AS<-fit_as$VaR
ES_AS<-fit_as$ES
sum(ifelse(r_t_est<VaR_AS,1,0))/N


##################################### linear ARCH

fit_larch<-uqfit(model="lARCH", tau, daily_ret=r_t_est,q=6,ES="Yes")
fit_larch
summary.rqmidas(fit_larch)
VaR_larch<-fit_larch$VaR
ES_larch<-fit_larch$ES

#####################################################################################################################
#####################################################################################################################
#####################################################################################################################


VaR_full<-cbind(VaR_garch_11_norm, VaR_gjr_garch_11_norm, VaR_e_garch_11_norm, VaR_cs_garch_11_norm,
                VaR_garch_11_std, VaR_gjr_garch_11_std, VaR_e_garch_11_std, VaR_cs_garch_11_std,
                unemployment_VaR_gm_norm, unemployment_VaR_gm_s_norm, unemployment_VaR_gm_std,
                unemployment_VaR_gm_s_std, unemployment_VaR_dagm_norm,unemployment_VaR_dagm_s_norm,
                real_gdp_VaR_gm_norm, real_gdp_VaR_gm_s_norm,real_gdp_VaR_gm_std, 
                real_gdp_VaR_gm_s_std, real_gdp_VaR_dagm_norm, real_gdp_VaR_dagm_s_norm, 
                bonds_VaR_gm_norm, bonds_VaR_gm_s_norm, bonds_VaR_gm_std, 
                bonds_VaR_dagm_norm, bonds_VaR_dagm_s_norm,
                VaR_SAV,VaR_AS,VaR_IG_ES,VaR_larch, VaR_HS_250)



ES_full<-cbind(ES_garch_11_norm, ES_gjr_garch_11_norm, ES_e_garch_11_norm, ES_cs_garch_11_norm,
               ES_garch_11_std, ES_gjr_garch_11_std, ES_e_garch_11_std, ES_cs_garch_11_std,
               unemployment_ES_gm_norm,unemployment_ES_gm_s_norm, unemployment_ES_gm_std,
               unemployment_ES_gm_s_std, unemployment_ES_dagm_norm,unemployment_ES_dagm_s_norm, 
               real_gdp_ES_gm_norm,real_gdp_ES_gm_s_norm, real_gdp_ES_gm_std,
               real_gdp_ES_gm_s_std, real_gdp_ES_dagm_norm, real_gdp_ES_dagm_s_norm, 
               bonds_ES_gm_norm,bonds_ES_gm_s_norm, bonds_ES_gm_std,
               bonds_ES_dagm_norm, bonds_ES_dagm_s_norm, 
               ES_SAV,ES_AS,ES_IG_ES,ES_larch,ES_HS_250)


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
                     "csGARCH Std",
                     "GM N - U",
                     "GM-skew N - U",
                     "GM Std - U",
                     "GM-skew Std - U",
                     "DAGM N - U",
                     "DAGM-skew  N- U",
                     "GM N - Real GDP",
                     "GM-skew N - Real GDP",
                     "GM Std - Real GDP",
                     "GM-skew Std - Real GDP",
                     "DAGM N - Real GDP",
                     "DAGM N-skew - Real GDP",
                     "GM N - Bond Yields",
                     "GM-skew N - Bond Yields",
                     "GM Std - Bond Yields",
                     "DAGM N - Bond Yields",
                     "DAGM-skew N - Bond Yields",
                     "SAV",
                     "AS",
                     "IG",
                     "Lin. ARCH",
                     "HS (w=250)")




colnames(col_res)<-c("AE","UC","CC","DQ")

round(col_res,3)

xtable(col_res,type="latex",digits=3)

############################################
############################################ BACKTESTING
############################################ ES

col_res1<-matrix(rep(NA,3*M),ncol=3)

for (i in 1:M){
  RES<-ESTest(actual=r_t_est, ES=ES_full[,i],VaR=VaR_full[,i],boot=TRUE, n.boot = 2000, alpha = 0.01)
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
                      "csGARCH Std",
                      "GM N - U",
                      "GM-skew N - U",
                      "GM Std - U",
                      "GM-skew Std - U",
                      "DAGM N - U",
                      "DAGM-skew  N- U",
                      "GM N - Real GDP",
                      "GM-skew N - Real GDP",
                      "GM Std - Real GDP",
                      "GM-skew Std - Real GDP",
                      "DAGM N - Real GDP",
                      "DAGM N-skew - Real GDP",
                      "GM N - Bond Yields",
                      "GM-skew N - Bond Yields",
                      "GM Std - Bond Yields",
                      "DAGM N - Bond Yields",
                      "DAGM-skew N - Bond Yields",
                      "SAV",
                      "AS",
                      "IG",
                      "Lin. ARCH",
                      "HS (w=250)")


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
loss_M9<-QL(r_t_est,VaR_full[,9],tau)
loss_M10<-QL(r_t_est,VaR_full[,10],tau)
loss_M11<-QL(r_t_est,VaR_full[,11],tau)
loss_M12<-QL(r_t_est,VaR_full[,12],tau)
loss_M13<-QL(r_t_est,VaR_full[,13],tau)
loss_M14<-QL(r_t_est,VaR_full[,14],tau)
loss_M15<-QL(r_t_est,VaR_full[,15],tau)
loss_M16<-QL(r_t_est,VaR_full[,16],tau)
loss_M17<-QL(r_t_est,VaR_full[,17],tau)
loss_M18<-QL(r_t_est,VaR_full[,18],tau)
loss_M19<-QL(r_t_est,VaR_full[,19],tau)
loss_M20<-QL(r_t_est,VaR_full[,20],tau)
loss_M21<-QL(r_t_est,VaR_full[,21],tau)
loss_M22<-QL(r_t_est,VaR_full[,22],tau)
loss_M23<-QL(r_t_est,VaR_full[,23],tau)
loss_M24<-QL(r_t_est,VaR_full[,24],tau)
loss_M25<-QL(r_t_est,VaR_full[,25],tau)
loss_M26<-QL(r_t_est,VaR_full[,26],tau)
loss_M27<-QL(r_t_est,VaR_full[,27],tau)
loss_M28<-QL(r_t_est,VaR_full[,28],tau)
loss_M29<-QL(r_t_est,VaR_full[,29],tau)
loss_M30<-QL(r_t_est,VaR_full[,30],tau)


db_loss<-cbind(
  loss_M1,
  loss_M2,
  loss_M3,
  loss_M4,
  loss_M5,
  loss_M6,
  loss_M7,
  loss_M8,
  loss_M9,
  loss_M10,
  loss_M11,
  loss_M12,
  loss_M13,
  loss_M14,
  loss_M15,
  loss_M16,
  loss_M17,
  loss_M18,
  loss_M19,
  loss_M20,
  loss_M21,
  loss_M22,
  loss_M23,
  loss_M24,
  loss_M25,
  loss_M26,
  loss_M27,
  loss_M28,
  loss_M29,
  loss_M30)

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
loss_M9<-FZLoss(r_t_est,VaR_full[,9],ES_full[,9],tau)
loss_M10<-FZLoss(r_t_est,VaR_full[,10],ES_full[,10],tau)
loss_M11<-FZLoss(r_t_est,VaR_full[,11],ES_full[,11],tau)
loss_M12<-FZLoss(r_t_est,VaR_full[,12],ES_full[,12],tau)
loss_M13<-FZLoss(r_t_est,VaR_full[,13],ES_full[,13],tau)
loss_M14<-FZLoss(r_t_est,VaR_full[,14],ES_full[,14],tau)
loss_M15<-FZLoss(r_t_est,VaR_full[,15],ES_full[,15],tau)
loss_M16<-FZLoss(r_t_est,VaR_full[,16],ES_full[,16],tau)
loss_M17<-FZLoss(r_t_est,VaR_full[,17],ES_full[,17],tau)
loss_M18<-FZLoss(r_t_est,VaR_full[,18],ES_full[,18],tau)
loss_M19<-FZLoss(r_t_est,VaR_full[,19],ES_full[,19],tau)
loss_M20<-FZLoss(r_t_est,VaR_full[,20],ES_full[,20],tau)
loss_M21<-FZLoss(r_t_est,VaR_full[,21],ES_full[,21],tau)
loss_M22<-FZLoss(r_t_est,VaR_full[,22],ES_full[,22],tau)
loss_M23<-FZLoss(r_t_est,VaR_full[,23],ES_full[,23],tau)
loss_M24<-FZLoss(r_t_est,VaR_full[,24],ES_full[,24],tau)
loss_M25<-FZLoss(r_t_est,VaR_full[,25],ES_full[,25],tau)
loss_M26<-FZLoss(r_t_est,VaR_full[,26],ES_full[,26],tau)
loss_M27<-FZLoss(r_t_est,VaR_full[,27],ES_full[,27],tau)
loss_M28<-FZLoss(r_t_est,VaR_full[,28],ES_full[,28],tau)
loss_M29<-FZLoss(r_t_est,VaR_full[,29],ES_full[,29],tau)
loss_M30<-FZLoss(r_t_est,VaR_full[,30],ES_full[,30],tau)



db_loss<-cbind(
  loss_M1,
  loss_M2,
  loss_M3,
  loss_M4,
  loss_M5,
  loss_M6,
  loss_M7,
  loss_M8,
  loss_M9,
  loss_M10,
  loss_M11,
  loss_M12,
  loss_M13,
  loss_M14,
  loss_M15,
  loss_M16,
  loss_M17,
  loss_M18,
  loss_M19,
  loss_M20,
  loss_M21,
  loss_M22,
  loss_M23,
  loss_M24,
  loss_M25,
  loss_M26,
  loss_M27,
  loss_M28,
  loss_M29,
  loss_M30)

colnames(db_loss)<-rownames(col_res1)[1:30]

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


##########################################################################
##########################################################################
##########################################################################

##################################################
#                   PLOTS at 1%                  #
##################################################

end_x<-endpoints(r_t_est,on="years")
end_x<-end_x+c(rep(1,length(end_x)-1),0)


# sGARCH-norm VaR (1%),sGARCH-norm ES (1%)

pdf("VaR_garch_11_norm_SSMI.pdf",height=9,width=16)
par(mai=c(1,1,0.5,0.3))

plot(1:N,coredata(r_t_est),type="l",
     xaxt="n",ylim=range(r_t_est,ES_garch_11_norm),
     xlab="",ylab="SSMI: Returns, VaR and ES (1%)",
     cex.lab=2,font.lab=2,lty=1,col="black",lwd=2)

axis(side=1,
     end_x[1:(length(end_x))], 
     c(2000:2021),cex.axis = 1.1,xpd=T)

lines(1:N,VaR_garch_11_norm,col="red",type="l",lwd=2)
lines(1:N,ES_garch_11_norm,col="gray",type="l",lwd=2)

legend(
  "topright",
  c("SSMI daily ret.","sGARCH-norm VaR (1%)","sGARCH-norm ES (1%)"),
  lty=1,
  col=c("black","red","gray"),
  lwd=2)

grid(NA,NULL)

abline(v=end_x,h=NA,col="gray",lty=3)

dev.off()


# gjrGARCH-norm VaR (1%),gjrGARCH-norm ES (1%)

pdf("VaR_gjr_garch_11_norm_SSMI.pdf",height=9,width=16)
par(mai=c(1,1,0.5,0.3))

plot(1:N,coredata(r_t_est),type="l",
     xaxt="n",ylim=range(r_t_est,ES_gjr_garch_11_norm),
     xlab="",ylab="SSMI: Returns, VaR and ES (1%)",
     cex.lab=2,font.lab=2,lty=1,col="black",lwd=2)

axis(side=1, 
     end_x[1:(length(end_x))], 
     c(2000:2021),cex.axis = 1.1,xpd=T)

lines(1:N,VaR_gjr_garch_11_norm,col="red",type="l",lwd=2)
lines(1:N,ES_gjr_garch_11_norm,col="gray",type="l",lwd=2)

legend(
  "topright",
  c("SSMI daily ret.","gjrGARCH-norm VaR (1%)","gjrGARCH-norm ES (1%)"),
  lty=1,
  col=c("black","red","gray"),
  lwd=2)

grid(NA,NULL)

abline(v=end_x,h=NA,col="gray",lty=3)

dev.off()


# eGARCH-norm VaR (1%),eGARCH-norm ES (1%)

pdf("VaR_e_garch_11_norm_SSMI.pdf",height=9,width=16)
par(mai=c(1,1,0.5,0.3))

plot(1:N,coredata(r_t_est),type="l",
     xaxt="n",ylim=range(r_t_est,ES_e_garch_11_norm),
     xlab="",ylab="SSMI: Returns, VaR and ES (1%)",
     cex.lab=2,font.lab=2,lty=1,col="black",lwd=2)

axis(side=1, 
     end_x[1:(length(end_x))], 
     c(2000:2021),cex.axis = 1.1,xpd=T)

lines(1:N,VaR_e_garch_11_norm,col="red",type="l",lwd=2)
lines(1:N,ES_e_garch_11_norm,col="gray",type="l",lwd=2)

legend(
  "topright",
  c("SSMI daily ret.","eGARCH-norm VaR (1%)","eGARCH-norm ES (1%)"),
  lty=1,
  col=c("black","red","gray"),
  lwd=2)

grid(NA,NULL)

abline(v=end_x,h=NA,col="gray",lty=3)

dev.off()


# csGARCH-norm VaR (1%),csGARCH-norm ES (1%)

pdf("VaR_cs_garch_11_norm_SSMI.pdf",height=9,width=16)
par(mai=c(1,1,0.5,0.3))

plot(1:N,coredata(r_t_est),type="l",
     xaxt="n",ylim=range(r_t_est,ES_cs_garch_11_norm),
     xlab="",ylab="SSMI: Returns, VaR and ES (1%)",
     cex.lab=2,font.lab=2,lty=1,col="black",lwd=2)

axis(side=1, 
     end_x[1:(length(end_x))], 
     c(2000:2021),cex.axis = 1.1,xpd=T)

lines(1:N,VaR_cs_garch_11_norm,col="red",type="l",lwd=2)
lines(1:N,ES_cs_garch_11_norm,col="gray",type="l",lwd=2)

legend(
  "topright",
  c("SSMI daily ret.","csGARCH-norm VaR (1%)","csGARCH-norm ES (1%)"),
  lty=1,
  col=c("black","red","gray"),
  lwd=2)

grid(NA,NULL)

abline(v=end_x,h=NA,col="gray",lty=3)

dev.off()


# sGARCH-std VaR (1%),sGARCH-std ES (1%)

pdf("VaR_garch_11_std_SSMI.pdf",height=9,width=16)
par(mai=c(1,1,0.5,0.3))

plot(1:N,coredata(r_t_est),type="l",
     xaxt="n",ylim=range(r_t_est,ES_garch_11_std),
     xlab="",ylab="SSMI: Returns, VaR and ES (1%)",
     cex.lab=2,font.lab=2,lty=1,col="black",lwd=2)

axis(side=1, 
     end_x[1:(length(end_x))], 
     c(2000:2021),cex.axis = 1.1,xpd=T)

lines(1:N,VaR_garch_11_std,col="red",type="l",lwd=2)
lines(1:N,ES_garch_11_std,col="gray",type="l",lwd=2)

legend(
  "topright",
  c("SSMI daily ret.","sGARCH-std VaR (1%)", "sGARCH-std ES (1%)"),
  lty=1,
  col=c("black","red","gray"),
  lwd=2)

grid(NA,NULL)

abline(v=end_x,h=NA,col="gray",lty=3)

dev.off()

# gjrGARCH-std VaR (1%),gjrGARCH-std ES (1%)

pdf("VaR_gjr_garch_11_std_SSMI.pdf",height=9,width=16)
par(mai=c(1,1,0.5,0.3))

plot(1:N,coredata(r_t_est),type="l",
     xaxt="n",ylim=range(r_t_est,ES_gjr_garch_11_std),
     xlab="",ylab="SSMI: Returns, VaR and ES (1%)",
     cex.lab=2,font.lab=2,lty=1,col="black",lwd=2)

axis(side=1, 
     end_x[1:(length(end_x))], 
     c(2000:2021),cex.axis = 1.1,xpd=T)

lines(1:N,VaR_gjr_garch_11_std,col="red",type="l",lwd=2)
lines(1:N,ES_gjr_garch_11_std,col="gray",type="l",lwd=2)

legend(
  "topright",
  c("SSMI daily ret.","gjrGARCH-std VaR (1%)","gjrGARCH-std ES (1%)"),
  lty=1,
  col=c("black","red","gray"),
  lwd=2)

grid(NA,NULL)

abline(v=end_x,h=NA,col="gray",lty=3)

dev.off()


# eGARCH-std VaR (1%),eGARCH-std ES (1%)

pdf("VaR_e_garch_11_std_SSMI.pdf",height=9,width=16)
par(mai=c(1,1,0.5,0.3))

plot(1:N,coredata(r_t_est),type="l",
     xaxt="n",ylim=range(r_t_est,ES_e_garch_11_std),
     xlab="",ylab="SSMI: Returns, VaR and ES (1%)",
     cex.lab=2,font.lab=2,lty=1,col="black",lwd=2)

axis(side=1, 
     end_x[1:(length(end_x))], 
     c(2000:2021),cex.axis = 1.1,xpd=T)

lines(1:N,VaR_e_garch_11_std,col="red",type="l",lwd=2)
lines(1:N,ES_e_garch_11_std,col="gray",type="l",lwd=2)

legend(
  "topright",
  c("SSMI daily ret.","eGARCH-std VaR (1%)","eGARCH-std ES (1%)"),
  lty=1,
  col=c("black","red","gray"),
  lwd=2)

grid(NA,NULL)

abline(v=end_x,h=NA,col="gray",lty=3)

dev.off()


# csGARCH-std VaR (1%),csGARCH-std ES (1%)

pdf("VaR_cs_garch_11_std_SSMI.pdf",height=9,width=16)
par(mai=c(1,1,0.5,0.3))

plot(1:N,coredata(r_t_est),type="l",
     xaxt="n",ylim=range(r_t_est,ES_cs_garch_11_std),
     xlab="",ylab="SSMI: Returns, VaR and ES (1%)",
     cex.lab=2,font.lab=2,lty=1,col="black",lwd=2)

axis(side=1, 
     end_x[1:(length(end_x))], 
     c(2000:2021),cex.axis = 1.1,xpd=T)

lines(1:N,VaR_cs_garch_11_std,col="red",type="l",lwd=2)
lines(1:N,ES_cs_garch_11_std,col="gray",type="l",lwd=2)

legend(
  "topright",
  c("SSMI daily ret.","csGARCH-std VaR (1%)","csGARCH-std ES (1%)"),
  lty=1,
  col=c("black","red","gray"),
  lwd=2)

grid(NA,NULL)

abline(v=end_x,h=NA,col="gray",lty=3)

dev.off()

######################################## Unemployment 

# Unemployment GARCH-MIDAS-norm VaR (1%),Unemployment GARCH-MIDAS-norm ES (1%)

pdf("unemployment_VaR_gm_norm_SSMI.pdf",height=9,width=16)
par(mai=c(1,1,0.5,0.3))

plot(1:N,coredata(r_t_est),type="l",
     xaxt="n",ylim=range(r_t_est,unemployment_ES_gm_norm),
     xlab="",ylab="SSMI: Returns, VaR and ES (1%)",
     cex.lab=2,font.lab=2,lty=1,col="black",lwd=2)

axis(side=1, 
     end_x[1:(length(end_x))], 
     c(2000:2021),cex.axis = 1.1,xpd=T)

lines(1:N,unemployment_VaR_gm_norm,col="red",type="l",lwd=2)
lines(1:N,unemployment_ES_gm_norm,col="gray",type="l",lwd=2)

legend(
  "topright",
  c("SSMI daily ret.","unemployment_GARCH-MIDAS-norm VaR (1%)","unemployment_GARCH-MIDAS-norm ES (1%)"),
  lty=1,
  col=c("black","red","gray"),
  lwd=2)

grid(NA,NULL)

abline(v=end_x,h=NA,col="gray",lty=3)

dev.off()


# GARCH-MIDAS-skew norm VaR (1%),GARCH-MIDAS-skew norm ES (1%)

pdf("unemployment_VaR_gm_s_norm_SSMI.pdf",height=9,width=16)
par(mai=c(1,1,0.5,0.3))

plot(1:N,coredata(r_t_est),type="l",
     xaxt="n",ylim=range(r_t_est,unemployment_ES_gm_s_norm),
     xlab="",ylab="SSMI: Returns, VaR and ES (1%)",
     cex.lab=2,font.lab=2,lty=1,col="black",lwd=2)

axis(side=1, 
     end_x[1:(length(end_x))], 
     c(2000:2021),cex.axis = 1.1,xpd=T)

lines(1:N,unemployment_VaR_gm_s_norm,col="red",type="l",lwd=2)
lines(1:N,unemployment_ES_gm_s_norm,col="gray",type="l",lwd=2)

legend(
  "topright",
  c("SSMI daily ret.","unemployment_GARCH-MIDAS-skew norm VaR (1%)","unemployment_GARCH-MIDAS-skew norm ES (1%)"),
  lty=1,
  col=c("black","red","gray"),
  lwd=2)

grid(NA,NULL)

abline(v=end_x,h=NA,col="gray",lty=3)

dev.off()


# unemployment_GARCH-MIDAS-std VaR (1%),unemployment_GARCH-MIDAS-std ES (1%)

pdf("unemployment_VaR_gm_std_SSMI.pdf",height=9,width=16)
par(mai=c(1,1,0.5,0.3))

plot(1:N,coredata(r_t_est),type="l",
     xaxt="n",ylim=range(r_t_est,unemployment_ES_gm_std),
     xlab="",ylab="SSMI: Returns, VaR and ES (1%)",
     cex.lab=2,font.lab=2,lty=1,col="black",lwd=2)

axis(side=1, 
     end_x[1:(length(end_x))], 
     c(2000:2021),cex.axis = 1.1,xpd=T)

lines(1:N,unemployment_VaR_gm_std,col="red",type="l",lwd=2)
lines(1:N,unemployment_ES_gm_std,col="gray",type="l",lwd=2)

legend(
  "topright",
  c("SSMI daily ret.","unemployment_GARCH-MIDAS-std VaR (1%)","unemployment_GARCH-MIDAS-std ES (1%)"),
  lty=1,
  col=c("black","red","gray"),
  lwd=2)

grid(NA,NULL)

abline(v=end_x,h=NA,col="gray",lty=3)

dev.off()

# unemployment_GARCH-MIDAS-skew std VaR (1%), unemployment_GARCH-MIDAS-skew std ES (1%)

pdf("unemployment_VaR_gm_s_std_SSMI.pdf",height=9,width=16)
par(mai=c(1,1,0.5,0.3))

plot(1:N,coredata(r_t_est),type="l",
     xaxt="n",ylim=range(r_t_est,unemployment_ES_gm_s_std),
     xlab="",ylab="SSMI: Returns, VaR and ES (1%)",
     cex.lab=2,font.lab=2,lty=1,col="black",lwd=2)

axis(side=1, 
     end_x[1:(length(end_x))], 
     c(2000:2021),cex.axis = 1.1,xpd=T)

lines(1:N,unemployment_VaR_gm_s_std,col="red",type="l",lwd=2)
lines(1:N,unemployment_ES_gm_s_std,col="gray",type="l",lwd=2)

legend(
  "topright",
  c("SSMI daily ret.","unemployment_GARCH-MIDAS-skew std VaR (1%)","unemployment_GARCH-MIDAS-skew std ES (1%)"),
  lty=1,
  col=c("black","red","gray"),
  lwd=2)

grid(NA,NULL)

abline(v=end_x,h=NA,col="gray",lty=3)

dev.off()

####################### DAGM

# unemployment_DAGM-norm VaR (1%),unemployment_DAGM-norm ES (1%)

pdf("unemployment_VaR_dagm_norm_SSMI.pdf",height=9,width=16)
par(mai=c(1,1,0.5,0.3))

plot(1:N,coredata(r_t_est),type="l",
     xaxt="n",ylim=range(r_t_est,unemployment_ES_dagm_norm),
     xlab="",ylab="SSMI: Returns, VaR and ES (1%)",
     cex.lab=2,font.lab=2,lty=1,col="black",lwd=2)

axis(side=1, 
     end_x[1:(length(end_x))], 
     c(2000:2021),cex.axis = 1.1,xpd=T)

lines(1:N,unemployment_VaR_dagm_norm,col="red",type="l",lwd=2)
lines(1:N,unemployment_ES_dagm_norm,col="gray",type="l",lwd=2)

legend(
  "topright",
  c("SSMI daily ret.","unemployment_DAGM-norm VaR (1%)","unemployment_DAGM-norm ES (1%)"),
  lty=1,
  col=c("black","red","gray"),
  lwd=2)

grid(NA,NULL)

abline(v=end_x,h=NA,col="gray",lty=3)

dev.off()


# DAGM-skew norm VaR (1%),DAGM-skew norm ES (1%)

pdf("unemployment_VaR_dagm_s_norm_SSMI.pdf",height=9,width=16)
par(mai=c(1,1,0.5,0.3))

plot(1:N,coredata(r_t_est),type="l",
     xaxt="n",ylim=range(r_t_est,unemployment_ES_dagm_s_norm),
     xlab="",ylab="SSMI: Returns, VaR and ES (1%)",
     cex.lab=2,font.lab=2,lty=1,col="black",lwd=2)

axis(side=1, 
     end_x[1:(length(end_x))], 
     c(2000:2021),cex.axis = 1.1,xpd=T)

lines(1:N,unemployment_VaR_dagm_s_norm,col="red",type="l",lwd=2)
lines(1:N,unemployment_ES_dagm_s_norm,col="gray",type="l",lwd=2)

legend(
  "topright",
  c("SSMI daily ret.","unemployment_DAGM-skew norm VaR (1%)","unemployment_DAGM-skew norm ES (1%)"),
  lty=1,
  col=c("black","red","gray"),
  lwd=2)

grid(NA,NULL)

abline(v=end_x,h=NA,col="gray",lty=3)

dev.off()

######################################## real_gdp 

# real_gdp GARCH-MIDAS-norm VaR (1%),real_gdp GARCH-MIDAS-norm ES (1%)

pdf("real_gdp_VaR_gm_norm_SSMI.pdf",height=9,width=16)
par(mai=c(1,1,0.5,0.3))

plot(1:N,coredata(r_t_est),type="l",
     xaxt="n",ylim=range(r_t_est,real_gdp_ES_gm_norm),
     xlab="",ylab="SSMI: Returns, VaR and ES (1%)",
     cex.lab=2,font.lab=2,lty=1,col="black",lwd=2)

axis(side=1, 
     end_x[1:(length(end_x))], 
     c(2000:2021),cex.axis = 1.1,xpd=T)

lines(1:N,real_gdp_VaR_gm_norm,col="red",type="l",lwd=2)
lines(1:N,real_gdp_ES_gm_norm,col="gray",type="l",lwd=2)

legend(
  "topright",
  c("SSMI daily ret.","real_gdp_GARCH-MIDAS-norm VaR (1%)","real_gdp_GARCH-MIDAS-norm ES (1%)"),
  lty=1,
  col=c("black","red","gray"),
  lwd=2)

grid(NA,NULL)

abline(v=end_x,h=NA,col="gray",lty=3)

dev.off()


# GARCH-MIDAS-skew norm VaR (1%),GARCH-MIDAS-skew norm ES (1%)

pdf("real_gdp_VaR_gm_s_norm_SSMI.pdf",height=9,width=16)
par(mai=c(1,1,0.5,0.3))

plot(1:N,coredata(r_t_est),type="l",
     xaxt="n",ylim=range(r_t_est,real_gdp_ES_gm_s_norm),
     xlab="",ylab="SSMI: Returns, VaR and ES (1%)",
     cex.lab=2,font.lab=2,lty=1,col="black",lwd=2)

axis(side=1, 
     end_x[1:(length(end_x))], 
     c(2000:2021),cex.axis = 1.1,xpd=T)

lines(1:N,real_gdp_VaR_gm_s_norm,col="red",type="l",lwd=2)
lines(1:N,real_gdp_ES_gm_s_norm,col="gray",type="l",lwd=2)

legend(
  "topright",
  c("SSMI daily ret.","real_gdp_GARCH-MIDAS-skew norm VaR (1%)","real_gdp_GARCH-MIDAS-skew norm ES (1%)"),
  lty=1,
  col=c("black","red","gray"),
  lwd=2)

grid(NA,NULL)

abline(v=end_x,h=NA,col="gray",lty=3)

dev.off()


# real_gdp_GARCH-MIDAS-std VaR (1%),real_gdp_GARCH-MIDAS-std ES (1%)

pdf("real_gdp_VaR_gm_std_SSMI.pdf",height=9,width=16)
par(mai=c(1,1,0.5,0.3))

plot(1:N,coredata(r_t_est),type="l",
     xaxt="n",ylim=range(r_t_est,real_gdp_ES_gm_std),
     xlab="",ylab="SSMI: Returns, VaR and ES (1%)",
     cex.lab=2,font.lab=2,lty=1,col="black",lwd=2)

axis(side=1, 
     end_x[1:(length(end_x))], 
     c(2000:2021),cex.axis = 1.1,xpd=T)

lines(1:N,real_gdp_VaR_gm_std,col="red",type="l",lwd=2)
lines(1:N,real_gdp_ES_gm_std,col="gray",type="l",lwd=2)

legend(
  "topright",
  c("SSMI daily ret.","real_gdp_GARCH-MIDAS-std VaR (1%)","real_gdp_GARCH-MIDAS-std ES (1%)"),
  lty=1,
  col=c("black","red","gray"),
  lwd=2)

grid(NA,NULL)

abline(v=end_x,h=NA,col="gray",lty=3)

dev.off()

# real_gdp_GARCH-MIDAS-skew std VaR (1%), real_gdp_GARCH-MIDAS-skew std ES (1%)

pdf("real_gdp_VaR_gm_s_std_SSMI.pdf",height=9,width=16)
par(mai=c(1,1,0.5,0.3))

plot(1:N,coredata(r_t_est),type="l",
     xaxt="n",ylim=range(r_t_est,real_gdp_ES_gm_s_std),
     xlab="",ylab="SSMI: Returns, VaR and ES (1%)",
     cex.lab=2,font.lab=2,lty=1,col="black",lwd=2)

axis(side=1, 
     end_x[1:(length(end_x))], 
     c(2000:2021),cex.axis = 1.1,xpd=T)

lines(1:N,real_gdp_VaR_gm_s_std,col="red",type="l",lwd=2)
lines(1:N,real_gdp_ES_gm_s_std,col="gray",type="l",lwd=2)

legend(
  "topright",
  c("SSMI daily ret.","real_gdp_GARCH-MIDAS-skew std VaR (1%)","real_gdp_GARCH-MIDAS-skew std ES (1%)"),
  lty=1,
  col=c("black","red","gray"),
  lwd=2)

grid(NA,NULL)

abline(v=end_x,h=NA,col="gray",lty=3)

dev.off()

####################### DAGM

# real_gdp_DAGM-norm VaR (1%),real_gdp_DAGM-norm ES (1%)

pdf("real_gdp_VaR_dagm_norm_SSMI.pdf",height=9,width=16)
par(mai=c(1,1,0.5,0.3))

plot(1:N,coredata(r_t_est),type="l",
     xaxt="n",ylim=range(r_t_est,real_gdp_ES_dagm_norm),
     xlab="",ylab="SSMI: Returns, VaR and ES (1%)",
     cex.lab=2,font.lab=2,lty=1,col="black",lwd=2)

axis(side=1, 
     end_x[1:(length(end_x))], 
     c(2000:2021),cex.axis = 1.1,xpd=T)

lines(1:N,real_gdp_VaR_dagm_norm,col="red",type="l",lwd=2)
lines(1:N,real_gdp_ES_dagm_norm,col="gray",type="l",lwd=2)

legend(
  "topright",
  c("SSMI daily ret.","real_gdp_DAGM-norm VaR (1%)","real_gdp_DAGM-norm ES (1%)"),
  lty=1,
  col=c("black","red","gray"),
  lwd=2)

grid(NA,NULL)

abline(v=end_x,h=NA,col="gray",lty=3)

dev.off()


# DAGM-skew norm VaR (1%),DAGM-skew norm ES (1%)

pdf("real_gdp_VaR_dagm_s_norm_SSMI.pdf",height=9,width=16)
par(mai=c(1,1,0.5,0.3))

plot(1:N,coredata(r_t_est),type="l",
     xaxt="n",ylim=range(r_t_est,real_gdp_ES_dagm_s_norm),
     xlab="",ylab="SSMI: Returns, VaR and ES (1%)",
     cex.lab=2,font.lab=2,lty=1,col="black",lwd=2)

axis(side=1, 
     end_x[1:(length(end_x))], 
     c(2000:2021),cex.axis = 1.1,xpd=T)

lines(1:N,real_gdp_VaR_dagm_s_norm,col="red",type="l",lwd=2)
lines(1:N,real_gdp_ES_dagm_s_norm,col="gray",type="l",lwd=2)

legend(
  "topright",
  c("SSMI daily ret.","real_gdp_DAGM-skew norm VaR (1%)","real_gdp_DAGM-skew norm ES (1%)"),
  lty=1,
  col=c("black","red","gray"),
  lwd=2)

grid(NA,NULL)

abline(v=end_x,h=NA,col="gray",lty=3)

dev.off()


# real_gdp_DAGM-skew std VaR (1%), real_gdp_DAGM-skew std ES (1%)

pdf("real_gdp_VaR_dagm_s_std_SSMI.pdf",height=9,width=16)
par(mai=c(1,1,0.5,0.3))

plot(1:N,coredata(r_t_est),type="l",
     xaxt="n",ylim=range(r_t_est,real_gdp_ES_dagm_s_std),
     xlab="",ylab="SSMI: Returns, VaR and ES (1%)",
     cex.lab=2,font.lab=2,lty=1,col="black",lwd=2)

axis(side=1, 
     end_x[1:(length(end_x))], 
     c(2000:2021),cex.axis = 1.1,xpd=T)

lines(1:N,real_gdp_VaR_dagm_s_std,col="red",type="l",lwd=2)
lines(1:N,real_gdp_ES_dagm_s_std,col="gray",type="l",lwd=2)

legend(
  "topright",
  c("SSMI daily ret.","real_gdp_DAGM-skew std VaR (1%)","real_gdp_DAGM-skew std ES (1%)"),
  lty=1,
  col=c("black","red","gray"),
  lwd=2)


######################################## bonds 

# bonds GARCH-MIDAS-norm VaR (1%),bonds GARCH-MIDAS-norm ES (1%)

pdf("bonds_VaR_gm_norm_SSMI.pdf",height=9,width=16)
par(mai=c(1,1,0.5,0.3))

plot(1:N,coredata(r_t_est),type="l",
     xaxt="n",ylim=range(r_t_est,bonds_ES_gm_norm),
     xlab="",ylab="SSMI: Returns, VaR and ES (1%)",
     cex.lab=2,font.lab=2,lty=1,col="black",lwd=2)

axis(side=1, 
     end_x[1:(length(end_x))], 
     c(2000:2021),cex.axis = 1.1,xpd=T)

lines(1:N,bonds_VaR_gm_norm,col="red",type="l",lwd=2)
lines(1:N,bonds_ES_gm_norm,col="gray",type="l",lwd=2)

legend(
  "topright",
  c("SSMI daily ret.","bonds_GARCH-MIDAS-norm VaR (1%)","bonds_GARCH-MIDAS-norm ES (1%)"),
  lty=1,
  col=c("black","red","gray"),
  lwd=2)

grid(NA,NULL)

abline(v=end_x,h=NA,col="gray",lty=3)

dev.off()


# GARCH-MIDAS-skew norm VaR (1%),GARCH-MIDAS-skew norm ES (1%)

pdf("bonds_VaR_gm_s_norm_SSMI.pdf",height=9,width=16)
par(mai=c(1,1,0.5,0.3))

plot(1:N,coredata(r_t_est),type="l",
     xaxt="n",ylim=range(r_t_est,bonds_ES_gm_s_norm),
     xlab="",ylab="SSMI: Returns, VaR and ES (1%)",
     cex.lab=2,font.lab=2,lty=1,col="black",lwd=2)

axis(side=1, 
     end_x[1:(length(end_x))], 
     c(2000:2021),cex.axis = 1.1,xpd=T)

lines(1:N,bonds_VaR_gm_s_norm,col="red",type="l",lwd=2)
lines(1:N,bonds_ES_gm_s_norm,col="gray",type="l",lwd=2)

legend(
  "topright",
  c("SSMI daily ret.","bonds_GARCH-MIDAS-skew norm VaR (1%)","bonds_GARCH-MIDAS-skew norm ES (1%)"),
  lty=1,
  col=c("black","red","gray"),
  lwd=2)

grid(NA,NULL)

abline(v=end_x,h=NA,col="gray",lty=3)

dev.off()


# bonds_GARCH-MIDAS-std VaR (1%),bonds_GARCH-MIDAS-std ES (1%)

pdf("bonds_VaR_gm_std_SSMI.pdf",height=9,width=16)
par(mai=c(1,1,0.5,0.3))

plot(1:N,coredata(r_t_est),type="l",
     xaxt="n",ylim=range(r_t_est,bonds_ES_gm_std),
     xlab="",ylab="SSMI: Returns, VaR and ES (1%)",
     cex.lab=2,font.lab=2,lty=1,col="black",lwd=2)

axis(side=1, 
     end_x[1:(length(end_x))], 
     c(2000:2021),cex.axis = 1.1,xpd=T)

lines(1:N,bonds_VaR_gm_std,col="red",type="l",lwd=2)
lines(1:N,bonds_ES_gm_std,col="gray",type="l",lwd=2)

legend(
  "topright",
  c("SSMI daily ret.","bonds_GARCH-MIDAS-std VaR (1%)","bonds_GARCH-MIDAS-std ES (1%)"),
  lty=1,
  col=c("black","red","gray"),
  lwd=2)

grid(NA,NULL)

abline(v=end_x,h=NA,col="gray",lty=3)

dev.off()


####################### DAGM

# bonds_DAGM-norm VaR (1%),bonds_DAGM-norm ES (1%)

pdf("bonds_VaR_dagm_norm_SSMI.pdf",height=9,width=16)
par(mai=c(1,1,0.5,0.3))

plot(1:N,coredata(r_t_est),type="l",
     xaxt="n",ylim=range(r_t_est,bonds_ES_dagm_norm),
     xlab="",ylab="SSMI: Returns, VaR and ES (1%)",
     cex.lab=2,font.lab=2,lty=1,col="black",lwd=2)

axis(side=1, 
     end_x[1:(length(end_x))], 
     c(2000:2021),cex.axis = 1.1,xpd=T)

lines(1:N,bonds_VaR_dagm_norm,col="red",type="l",lwd=2)
lines(1:N,bonds_ES_dagm_norm,col="gray",type="l",lwd=2)

legend(
  "topright",
  c("SSMI daily ret.","bonds_DAGM-norm VaR (1%)","bonds_DAGM-norm ES (1%)"),
  lty=1,
  col=c("black","red","gray"),
  lwd=2)

grid(NA,NULL)

abline(v=end_x,h=NA,col="gray",lty=3)

dev.off()


# DAGM-skew norm VaR (1%),DAGM-skew norm ES (1%)

pdf("bonds_VaR_dagm_s_norm_SSMI.pdf",height=9,width=16)
par(mai=c(1,1,0.5,0.3))

plot(1:N,coredata(r_t_est),type="l",
     xaxt="n",ylim=range(r_t_est,bonds_ES_dagm_s_norm),
     xlab="",ylab="SSMI: Returns, VaR and ES (1%)",
     cex.lab=2,font.lab=2,lty=1,col="black",lwd=2)

axis(side=1, 
     end_x[1:(length(end_x))], 
     c(2000:2021),cex.axis = 1.1,xpd=T)

lines(1:N,bonds_VaR_dagm_s_norm,col="red",type="l",lwd=2)
lines(1:N,bonds_ES_dagm_s_norm,col="gray",type="l",lwd=2)

legend(
  "topright",
  c("SSMI daily ret.","bonds_DAGM-skew norm VaR (1%)","bonds_DAGM-skew norm ES (1%)"),
  lty=1,
  col=c("black","red","gray"),
  lwd=2)

grid(NA,NULL)

abline(v=end_x,h=NA,col="gray",lty=3)

dev.off()


########################################
#         Non-parametric model         #
########################################

#w<-250 # length of rolling window

pdf("VaR_HS_250_SSMI.pdf",height=9,width=16)   #### PLOT NOT WORKING 
par(mai=c(1,1,0.5,0.3))

plot(1:N,coredata(r_t_est),type="l",
     xaxt="n",ylim=range(r_t_est,ES_HS_250),
     xlab="",ylab="SSMI: Returns, VaR and ES (1%)",
     cex.lab=2,font.lab=2,lty=1,col="black",lwd=2)

axis(side=1, 
     end_x[1:(length(end_x))], 
     c(2000:2021),cex.axis = 1.1,xpd=T)


lines(1:N,VaR_HS_250,col="pink",type="l",lwd=2)
lines(1:N,ES_HS_250,col="green",type="l",lwd=2)

legend(
  "topright",
  c("SSMI daily ret.",
    "HS (w=250) VaR forecasts",
    "HS (w=250) ES forecasts"),
  lty=1,
  col=c("black","pink","green"),
  lwd=2)

grid(NA,NULL)

abline(v=end_x,h=NA,col="gray",lty=3)

dev.off()


##################################################
#            Semi-Parametric methods             #
################################################## 


## Indirect GARCH (With ES)

pdf("VaR_IG_ES_SSMI.pdf",height=9,width=16)
par(mai=c(1,1,0.5,0.3))

plot(1:N,coredata(r_t_est),type="l",
     xaxt="n",ylim=range(r_t_est,ES_IG_ES),
     xlab="",ylab="SP500: Returns, VaR and ES (1%)",
     cex.lab=2,font.lab=2,lty=1,col="black",lwd=2)

axis(side=1, 
     end_x[1:(length(end_x))], 
     c(2000:2021),cex.axis = 1.1,xpd=T)


lines(1:N,VaR_IG_ES,col="green",type="l",lwd=2)
lines(1:N,ES_IG_ES,col="red",type="l",lwd=2)

legend(
  "topright",
  c("SSMI daily ret.",
    "Indirect GARCH VaR (1%)",
    "Indirect GARCH ES (1%)"),
  lty=1,
  col=c("black","green","red"),
  lwd=2)

grid(NA,NULL)

abline(v=end_x,h=NA,col="gray",lty=3)


dev.off()


## SAV


pdf("VaR_SAV_SSMI.pdf",height=9,width=16)  ### REALLY WEIRD PLOT
par(mai=c(1,1,0.5,0.3))

plot(1:N,coredata(r_t_est),type="l",
     xaxt="n",ylim=range(r_t_est,ES_SAV),
     xlab="",ylab="SSMI: Returns, VaR and ES (1%)",
     cex.lab=2,font.lab=2,lty=1,col="black",lwd=2)

axis(side=1, 
     end_x[1:(length(end_x))], 
     c(2000:2021),cex.axis = 1.1,xpd=T)


lines(1:N,VaR_SAV,col="blue",type="l",lwd=2)
lines(1:N,ES_SAV,col="grey",type="l",lwd=2)

legend(
  "topright",
  c("SSMI daily ret.","SAV VaR (1%)","SAV ES (1%)"),
  lty=1,
  col=c("black","blue","grey"),
  lwd=2)

grid(NA,NULL)

abline(v=end_x,h=NA,col="gray",lty=3)


dev.off()



## AS




pdf("VaR_AS_SMI_5.pdf",height=9,width=16)
par(mai=c(1,1,0.5,0.3))

plot(1:N,coredata(r_t_est),type="l",
     xaxt="n",ylim=range(r_t_est,ES_AS),
     xlab="",ylab="SMI: Returns, VaR and ES (1%)",
     cex.lab=2,font.lab=2,lty=1,col="black",lwd=2)

axis(side=1, 
     end_x[1:(length(end_x))], 
     c(2000:2021),cex.axis = 1.1,xpd=T)


lines(1:N,VaR_AS,col="blue",type="l",lwd=2)
lines(1:N,ES_AS,col="red",type="l",lwd=2)

legend(
  "topright",
  c("SMI daily ret.","AS VaR (1%)","ES VaR (1%)"),
  lty=1,
  col=c("black","blue","red"),
  lwd=2)

grid(NA,NULL)

abline(v=end_x,h=NA,col="gray",lty=3)


dev.off()




##################################### linear ARCH




pdf("VaR_lARCH_SSMI.pdf",height=9,width=16)
par(mai=c(1,1,0.5,0.3))

plot(1:N,coredata(r_t_est),type="l",
     xaxt="n",ylim=range(r_t_est,ES_larch),
     xlab="",ylab="SSMI: Returns and VaR (1%)",
     cex.lab=2,font.lab=2,lty=1,col="black",lwd=2)

axis(side=1, 
     end_x[1:(length(end_x))], 
     c(2000:2021),cex.axis = 1.1,xpd=T)


lines(1:N,VaR_larch,col="blue",type="l",lwd=2)
lines(1:N,ES_larch,col="violet",type="l",lwd=2)

legend(
  "topright",
  c("SSMI daily ret.","Linear ARCH(6) VaR (1%)","Linear ARCH(6) ES (1%)"),
  lty=1,
  col=c("black","blue","violet"),
  lwd=2)

grid(NA,NULL)

abline(v=end_x,h=NA,col="gray",lty=3)


dev.off()
