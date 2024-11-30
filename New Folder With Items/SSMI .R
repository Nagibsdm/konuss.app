


library("xts")
library("highfrequency")
library("tseries")
library("fBasics")
library("rugarch") #install.packages("rugarch")
library("maxLik")
library("FinTS")
library("rumidas")
library("dccmidas")


ssmi <- read.csv("realizedvolatilities.csv",
header = T, 		
sep = ",", 			
dec = ".",
na.strings="null")

ssmi<-subset(ssmi,ssmi$Symbol==".SSMI")

head(ssmi)
dim(ssmi)


ssmi<-ssmi[complete.cases(ssmi),]
dim(ssmi)

class(ssmi)

##### Transform to Time series data 

Date_ssmi <- strptime(ssmi[,1], "%Y-%m-%d",tz="GMT")
ssmi_ts <- as.xts(ssmi[,2:ncol(ssmi)], Date_ssmi)



head(ssmi_ts)

class(ssmi)
class(ssmi_ts)

dim(ssmi_ts)
range(ssmi_ts)
range(time(ssmi_ts))

##### Plot closing prices 

plot(ssmi_ts$close_price)


###### We can also obtain the open-to-close daily log-returns 

#log(bitcoin_i$Close) - log(bitcoin_i$Open)


###### Let us obtain the close-to-close daily log-returns

r_t <- makeReturns(ssmi_ts$close_price)
N <- length(r_t) 
dim(r_t)      #WHAT IS THE DIFFERENCE BETWEEN LENGHT() AND DIM() ? 

##### Overnight Returns

ssmi_ts$OVR <- 0

ssmi_ts$OVR<-log(ssmi_ts$open_price/(lag(ssmi_ts$close_price))) #Overnight Returns  #WHY DO WE USE LOG OR LAG?
ssmi_ts$OVR[1]<-0  #WITH THIS WE ESTABLISHED THAT THE FIRST VALUE ITS ZERO

head(ssmi_ts) #As we see, now we have a table for the OVR values

##### Plot of r_t, adding different name and color 

plot(r_t, main="SSMI", col="red")

boxplot(r_t,col="red",main="SSMI")

## WHAT IS THE DIFFERENCE BETWEEN USING OR NOT COREDATA 

boxplot(coredata(r_t),col="red",main="SSMI")

##### ACF and PACF

#par(mfrow=c(2,1))   ### For showing two plots
acf(coredata(r_t))
pacf(coredata(r_t))

##### ACF on squared and absolute returns

acf(coredata(r_t^2))
acf(coredata(abs(r_t)))

##### Let's now calculate the Stylized facts that we learn in class 

skewness(r_t)
kurtosis(r_t)

qqnorm(r_t)
qqline(r_t, col = "red")

summary(ssmi_ts)

##################################################################

hist(r_t, freq = FALSE, ylim = c(0,60), breaks = 40)  #### HOW DO YOU KNOW HOW MANY YLIM AND BREAKS?
curve(dnorm(x, mean=mean(r_t), sd=sd(r_t)), add=TRUE, col="red")

Box.test(r_t,type="Ljung-Box",lag=5)  #### HOW DO WE KNOW THE LAG THAT WE NEED TO ESTABLISHED?
Box.test(r_t,type="Box-Pierce",lag=5)

Box.test(r_t^2,type="Ljung-Box", lag=5)
Box.test(r_t^2,type="Box-Pierce",lag=5)

jarque.bera.test(r_t)

adf.test(r_t)

ArchTest(r_t, lag=5, FALSE)

##### COMPLETE WITH DICKEY FULLER TEST AND arch TEST 

##################

########################################
######################################## GARCH MODELLING 
########################################



##################### estimate an arch(2) 

spec_arch_2 <- ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(2,0)), 
                          mean.model=list(armaOrder=c(0,0), include.mean=FALSE),  
                          distribution.model="norm")

fit_arch_2<-ugarchfit(spec=spec_arch_2, data=r_t)

### estimated coefficients

fit_arch_2@fit$matcoef
round(fit_arch_2@fit$matcoef,3)
round(coef(fit_arch_2),3)

### conditional volatility

cond_vol_arch_2<-fit_arch_2@fit$sigma

#### estimation with OLS

r_t_1_sq<-lag(r_t^2,k=1)
r_t_1_sq[1]<-0

r_t_2_sq<-lag(r_t^2,k=2)
r_t_2_sq[1:2]<-0

reg_arch_2<-lm(r_t^2 ~ r_t_1_sq+r_t_2_sq)
summary(reg_arch_2)

round(coef(reg_arch_2),3)
round(coef(fit_arch_2),3)

##################### estimate an arch(4) 

spec_arch_4 <- ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(4,0)), 
                          mean.model=list(armaOrder=c(0,0), include.mean=FALSE),  
                          distribution.model="norm")

fit_arch_4 <- ugarchfit(spec=spec_arch_4, data=r_t)

### estimated coefficients

fit_arch_4@fit$matcoef
round(fit_arch_4@fit$matcoef,3)
round(coef(fit_arch_4),3)

### conditional volatility

cond_vol_arch_4<-fit_arch_4@fit$sigma

##################### estimate an GARCH(1,1) #### I ARRIVED HERE !

spec_garch_11 <- ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)), 
                            mean.model=list(armaOrder=c(0,0), include.mean=FALSE),  
                            distribution.model="norm")

fit<-ugarchfit(spec=spec_garch_11, data=r_t)

### estimated coefficients

names(fit@fit)

fit@fit$matcoef
round(fit@fit$matcoef,3)
round(coef(fit),3)

fit@fit$LLH

### conditional volatility

cond_vol_garch_11<-fit@fit$sigma

#############################

#ESTIMATION OF GARCH MODELS 

##############################


### sGARCH (1,1) - norm 

spec_garch_11_norm <- ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)), 
                            mean.model=list(armaOrder=c(0,0), include.mean=FALSE),  
                            distribution.model="norm")

fit_garch_11_norm<-ugarchfit(spec=spec_garch_11_norm, data=r_t)

### estimated coefficients

fit_garch_11_norm@fit$matcoef
round(fit_garch_11_norm@fit$matcoef,3)
round(coef(fit_garch_11_norm),3)

### conditional volatility

cond_vol_garch_11_norm<-fit_garch_11_norm@fit$sigma

######################
### sGARCH (1,1) - std

spec_garch_11_std <- ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)), 
                            mean.model=list(armaOrder=c(0,0), include.mean=FALSE),  
                            distribution.model="std")

fit_garch_11_std<-ugarchfit(spec=spec_garch_11_std, data=r_t)

### estimated coefficients

fit_garch_11_std@fit$matcoef
round(fit_garch_11_std@fit$matcoef,3)
round(coef(fit_garch_11_std),3)

### conditional volatility

cond_vol_garch_11_std<-fit_garch_11_std@fit$sigma

#########################
### gjrGARCH (1,1) - norm 

spec_gjr_garch_11_norm <- ugarchspec(variance.model=list(model="gjrGARCH", garchOrder=c(1,1)), 
                                mean.model=list(armaOrder=c(0,0), include.mean=FALSE),  
                                distribution.model="norm")

fit_gjr_garch_11_norm<-ugarchfit(spec=spec_gjr_garch_11_norm, data=r_t)

### estimated coefficients

fit_gjr_garch_11_norm@fit$matcoef
round(fit_gjr_garch_11_norm@fit$matcoef,3)
round(coef(fit_gjr_garch_11_norm),3)

### conditional volatility

cond_vol_gjr_garch_11_norm<-fit_gjr_garch_11_norm@fit$sigma

########################
### gjrGARCH (1,1) - std

spec_gjr_garch_11_std <- ugarchspec(variance.model=list(model="gjrGARCH", garchOrder=c(1,1)), 
                                     mean.model=list(armaOrder=c(0,0), include.mean=FALSE),  
                                     distribution.model="std")

fit_gjr_garch_11_std<-ugarchfit(spec=spec_gjr_garch_11_std, data=r_t)

### estimated coefficients

fit_gjr_garch_11_std@fit$matcoef
round(fit_gjr_garch_11_std@fit$matcoef,3)
round(coef(fit_gjr_garch_11_std),3)

### conditional volatility

cond_vol_gjr_garch_11_std<-fit_gjr_garch_11_std@fit$sigma

#######################
### eGARCH (1,1) - norm 

spec_e_garch_11_norm <- ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(1,1)), 
                                     mean.model=list(armaOrder=c(0,0), include.mean=FALSE),  
                                     distribution.model="norm")

fit_e_garch_11_norm<-ugarchfit(spec=spec_e_garch_11_norm, data=r_t)

### estimated coefficients

fit_e_garch_11_norm@fit$matcoef
round(fit_e_garch_11_norm@fit$matcoef,3)
round(coef(fit_e_garch_11_norm),3)

### conditional volatility

cond_vol_e_garch_11_norm<-fit_e_garch_11_norm@fit$sigma

######################
### eGARCH (1,1) - std

spec_e_garch_11_std <- ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(1,1)), 
                                   mean.model=list(armaOrder=c(0,0), include.mean=FALSE),  
                                   distribution.model="std")

fit_e_garch_11_std <- ugarchfit(spec=spec_e_garch_11_std, data=r_t)

### estimated coefficients

fit_e_garch_11_std@fit$matcoef
round(fit_e_garch_11_std@fit$matcoef,3)
round(coef(fit_e_garch_11_std),3)

### conditional volatility

cond_vol_e_garch_11_std<-fit_e_garch_11_std@fit$sigma


### csGARCH (1,1) - norm 

spec_cs_garch_11_norm <- ugarchspec(variance.model=list(model="csGARCH", garchOrder=c(1,1)), 
                                      mean.model=list(armaOrder=c(0,0), include.mean=FALSE),  
                                      distribution.model="norm")

fit_cs_garch_11_norm<-ugarchfit(spec=spec_cs_garch_11_norm, data=r_t)

### estimated coefficients

fit_cs_garch_11_norm@fit$matcoef
round(fit_cs_garch_11_norm@fit$matcoef,3)
round(coef(fit_cs_garch_11_norm),3)

### conditional volatility

cond_vol_cs_garch_11_norm<-fit_cs_garch_11_norm@fit$sigma

### csGARCH (1,1) - std

spec_cs_garch_11_std <- ugarchspec(variance.model=list(model="csGARCH", garchOrder=c(1,1)), 
                                     mean.model=list(armaOrder=c(0,0), include.mean=FALSE),  
                                     distribution.model="std")

fit_cs_garch_11_std <- ugarchfit(spec=spec_cs_garch_11_std, data=r_t)

### estimated coefficients

fit_cs_garch_11_std@fit$matcoef
round(fit_cs_garch_11_std@fit$matcoef,3)
round(coef(fit_cs_garch_11_std),3)

### conditional volatility

cond_vol_cs_garch_11_std<-fit_cs_garch_11_std@fit$sigma

###################################################
#     Now lets consider them with skewness 
###################################################

### sGARCH (1,1) - s_norm 

spec_garch_11_s_norm <- ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)), 
                                 mean.model=list(armaOrder=c(0,0), include.mean=FALSE),  
                                 distribution.model="snorm")

fit_garch_11_s_norm <-ugarchfit(spec=spec_garch_11_s_norm, data=r_t)

### estimated coefficients

fit_garch_11_s_norm@fit$matcoef
round(fit_garch_11_s_norm@fit$matcoef,3)
round(coef(fit_garch_11_s_norm),3)

### conditional volatility

cond_vol_garch_11_s_norm<-fit_garch_11_s_norm@fit$sigma

########################
### sGARCH (1,1) - s_std

spec_garch_11_s_std <- ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)), 
                                mean.model=list(armaOrder=c(0,0), include.mean=FALSE),  
                                distribution.model="sstd")

fit_garch_11_s_std<-ugarchfit(spec=spec_garch_11_s_std, data=r_t)

### estimated coefficients

fit_garch_11_s_std@fit$matcoef
round(fit_garch_11_s_std@fit$matcoef,3)
round(coef(fit_garch_11_s_std),3)

### conditional volatility

cond_vol_garch_11_s_std<-fit_garch_11_s_std@fit$sigma

###########################
### gjrGARCH (1,1) - s_norm 

spec_gjr_garch_11_s_norm <- ugarchspec(variance.model=list(model="gjrGARCH", garchOrder=c(1,1)), 
                                     mean.model=list(armaOrder=c(0,0), include.mean=FALSE),  
                                     distribution.model="snorm")

fit_gjr_garch_11_s_norm <-ugarchfit(spec=spec_gjr_garch_11_s_norm, data=r_t)

### estimated coefficients

fit_gjr_garch_11_s_norm@fit$matcoef
round(fit_gjr_garch_11_s_norm@fit$matcoef,3)
round(coef(fit_gjr_garch_11_s_norm),3)

### conditional volatility

cond_vol_gjr_garch_11_s_norm<-fit_gjr_garch_11_s_norm@fit$sigma

##########################
### gjrGARCH (1,1) - s_std

spec_gjr_garch_11_s_std <- ugarchspec(variance.model=list(model="gjrGARCH", garchOrder=c(1,1)), 
                                    mean.model=list(armaOrder=c(0,0), include.mean=FALSE),  
                                    distribution.model="sstd")

fit_gjr_garch_11_s_std<-ugarchfit(spec=spec_gjr_garch_11_s_std, data=r_t)

### estimated coefficients

fit_gjr_garch_11_s_std@fit$matcoef
round(fit_gjr_garch_11_s_std@fit$matcoef,3)
round(coef(fit_gjr_garch_11_s_std),3)

### conditional volatility

cond_vol_gjr_garch_11_s_std<-fit_gjr_garch_11_s_std@fit$sigma

#########################
### eGARCH (1,1) - s_norm 

spec_e_garch_11_s_norm <- ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(1,1)), 
                                   mean.model=list(armaOrder=c(0,0), include.mean=FALSE),  
                                   distribution.model="snorm")

fit_e_garch_11_s_norm<-ugarchfit(spec=spec_e_garch_11_s_norm, data=r_t)

### estimated coefficients

fit_e_garch_11_s_norm@fit$matcoef
round(fit_e_garch_11_s_norm@fit$matcoef,3)
round(coef(fit_e_garch_11_s_norm),3)

### conditional volatility

cond_vol_e_garch_11_s_norm<-fit_e_garch_11_s_norm@fit$sigma

### eGARCH (1,1) - s_std

spec_e_garch_11_s_std <- ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(1,1)), 
                                  mean.model=list(armaOrder=c(0,0), include.mean=FALSE),  
                                  distribution.model="sstd")

fit_e_garch_11_s_std <- ugarchfit(spec=spec_e_garch_11_s_std, data=r_t)

### estimated coefficients

fit_e_garch_11_s_std@fit$matcoef
round(fit_e_garch_11_s_std@fit$matcoef,3)
round(coef(fit_e_garch_11_s_std),3)

### conditional volatility

cond_vol_e_garch_11_s_std<-fit_e_garch_11_s_std@fit$sigma

##########################
### csGARCH (1,1) - s_norm 

spec_cs_garch_11_s_norm <- ugarchspec(variance.model=list(model="csGARCH", garchOrder=c(1,1)), 
                                     mean.model=list(armaOrder=c(0,0), include.mean=FALSE),  
                                     distribution.model="snorm")

fit_cs_garch_11_s_norm<-ugarchfit(spec=spec_cs_garch_11_s_norm, data=r_t)

### estimated coefficients

fit_cs_garch_11_s_norm@fit$matcoef
round(fit_cs_garch_11_s_norm@fit$matcoef,3)
round(coef(fit_cs_garch_11_s_norm),3)

### conditional volatility

cond_vol_cs_garch_11_s_norm<-fit_cs_garch_11_s_norm@fit$sigma

#########################
### csGARCH (1,1) - s_std

spec_cs_garch_11_s_std <- ugarchspec(variance.model=list(model="csGARCH", garchOrder=c(1,1)), 
                                    mean.model=list(armaOrder=c(0,0), include.mean=FALSE),  
                                    distribution.model="sstd")

fit_cs_garch_11_s_std <- ugarchfit(spec=spec_cs_garch_11_s_std, data=r_t)

### estimated coefficients

fit_cs_garch_11_s_std@fit$matcoef
round(fit_cs_garch_11_s_std@fit$matcoef,3)
round(coef(fit_cs_garch_11_s_std),3)

### conditional volatility

cond_vol_cs_garch_11_s_std<-fit_cs_garch_11_s_std@fit$sigma



##################### importing the real-time vs revised data for NAI


swiss_gdp <- read.csv("gdp.csv",
                      header = T, 		
                      sep = ",", 			
                      dec = ".",
                      na.strings="null")

##### Transform to Time series data 

Date_swiss_gdp <- strptime(swiss_gdp[,1], "%Y-%m-%d",tz="GMT")
swiss_gdp_ts <- as.xts(swiss_gdp[,2:ncol(swiss_gdp)], Date_swiss_gdp)

swiss_gdp_ts

##################### plot of gdp

plot(swiss_gdp_ts)

##################### transformation of gdp into a matrix
##################### let us focus on the period  2007/2020

r_t_est<-r_t['1999/2020']

K<-3#number of lagged realizations entering the long-run equation

#gdp_mv<-mv_into_mat(r_t_est,swiss_gdp_ts, K=K,"Quarterly")

#dim(gdp_mv)
#length(r_t_est)

##################################################################

RV_quarterly_sum <- apply.quarterly(swiss_gdp_ts,sum)

RV_quarterly <- as.xts(coredata(RV_quarterly_sum),seq(as.Date("1999-01-01"), 
                by = "quarter", length.out = length(RV_quarterly_sum)))

gdp_mv <- mv_into_mat(r_t_est['1999/2020'],diff(RV_quarterly),K=K,type="quarterly")

#RV_quarterly

gdp_mv
View(gdp_mv)

########## GARCH-MIDAS MODELS ############

##################### estimate a GARCH-MIDAS, without skew parameter, normal distribution

fit_gm_norm <- ugmfit(model="GM",skew="NO",distribution="norm",
               daily_ret=r_t_est,mv_m=gdp_mv,K=K)
fit_gm_norm
summary.rumidas(fit_gm_norm)
names(fit_gm_norm)
fit_gm_norm$inf_criteria

M1_vol<-fit_gm_norm$est_vol_in_s

##################### estimate a GARCH-MIDAS, with skew parameter, normal distribution

fit_gm_s_norm <- ugmfit(model="GM",skew="YES",distribution="norm",
                        daily_ret=r_t_est,mv_m=gdp_mv,K=K)  
fit_gm_s_norm
summary.rumidas(fit_gm_s_norm)
names(fit_gm_s_norm)
fit_gm_s_norm$inf_criteria

M2_vol<-fit_gm_s_norm$est_vol_in_s

##################### estimate a GARCH-MIDAS, without skew parameter, student distribution

fit_gm_std <- ugmfit(model="GM",skew="NO",distribution="std",
                      daily_ret=r_t_est,mv_m=gdp_mv,K=K)
fit_gm_std
summary.rumidas(fit_gm_std)
names(fit_gm_std)
fit_gm_std$inf_criteria

M3_vol<-fit_gm_std$est_vol_in_s

##################### estimate a GARCH-MIDAS, with skew parameter, student distribution

fit_gm_s_std <- ugmfit(model="GM",skew="YES",distribution="std",
                     daily_ret=r_t_est,mv_m=gdp_mv,K=K)
fit_gm_s_std
summary.rumidas(fit_gm_s_std)
names(fit_gm_s_std)
fit_gm_s_std$inf_criteria

M4_vol<-fit_gm_s_std$est_vol_in_s


###################################
########## DAGM MODELS ############

##################### estimate a DAGM, without skew parameter, normal distribution

fit_dagm_norm <- ugmfit(model="DAGM",skew="NO",distribution="norm",
                      daily_ret=r_t_est,mv_m=gdp_mv,K=K) 
fit_dagm_norm
summary.rumidas(fit_dagm_norm)
names(fit_dagm_norm)
fit_dagm_norm$inf_criteria

M5_vol<-fit_dagm_norm$est_vol_in_s

##################### estimate a DAGM, with skew parameter, normal distribution

fit_dagm_s_norm <- ugmfit(model="DAGM",skew="YES",distribution="norm",
                        daily_ret=r_t_est,mv_m=gdp_mv,K=K)
fit_dagm_s_norm
summary.rumidas(fit_dagm_s_norm)
names(fit_dagm_s_norm)
fit_dagm_s_norm$inf_criteria

M6_vol<-fit_dagm_s_norm$est_vol_in_s

##################### estimate a DAGM, without skew parameter, std distribution

fit_dagm_std <- ugmfit(model="DAGM",skew="NO",distribution="std",
                     daily_ret=r_t_est,mv_m=gdp_mv,K=K)
fit_dagm_std
summary.rumidas(fit_dagm_std)
names(fit_dagm_std)
fit_dagm_std$inf_criteria

M7_vol<-fit_dagm_std$est_vol_in_s

##################### estimate a DAGM, without skew parameter, std distribution

fit_dagm_s_std <- ugmfit(model="DAGM",skew="YES",distribution="std",
                       daily_ret=r_t_est,mv_m=gdp_mv,K=K)
fit_dagm_s_std
summary.rumidas(fit_dagm_s_std)
names(fit_dagm_s_std)
fit_dagm_s_std$inf_criteria

M8_vol<-fit_dagm_s_std$est_vol_in_s

##############################
##############################

#      UNIT 4 - PLOTTING     # 

##############################
##############################

