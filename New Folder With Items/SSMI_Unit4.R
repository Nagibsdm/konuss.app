
setwd("~/Downloads")

##################### MEM model 

RVar<-(ssmi_oxfordmanrealizedvolatilityindices.2_ts$rv5['2000/2020'])
RVol<-RVar^0.5
View(RVol)
N<-length(RVol)

### standard MEM model

mem_fit<-umemfit(model="MEM",skew="NO",x=RVol)
mem_fit
summary.rumidas(mem_fit)
names(mem_fit)

## plot

end_x<-endpoints(RVol,on="years")
end_x[1]<-1

pdf("MEM_fit.pdf",height=9,width=16)
par(mai=c(1,1,0.5,0.3))

plot(1:N,coredata(RVol),type="l",lwd=3,
     xaxt="n",
     xlab="",ylab="Volatility",main="SSMI",
     cex.lab=1.5,font.lab=1.8,lty=1,col="red")

lines(1:N,mem_fit$est_in_s,col="black",lwd=3)

axis(side=1,  
     end_x, 
     2000:2020,cex.axis = 0.95,xpd=T)

legend("topright",
       c("Realized Vol.","MEM(1,1) Real. Vol."),
       col=c("red","black"),
       lty=c(1,1),
       lwd=c(3,3),
       cex=2)

dev.off()

### A-MEM model we use open to close

r_t_unit4 <- ssmi_oxfordmanrealizedvolatilityindices.2_ts$close_price['2000/2020']

amem_fit<-umemfit(model="MEM",skew="YES",x=RVol,daily_ret=r_t_unit4)
amem_fit
summary.rumidas(amem_fit)

## plot

end_x<-endpoints(RVol,on="years")
end_x[1]<-1

pdf("A-MEM_fit.pdf",height=9,width=16)
par(mai=c(1,1,0.5,0.3))

plot(1:N,coredata(RVol),type="l",lwd=3,
     xaxt="n",
     xlab="",ylab="Volatility",main="SSMI",
     cex.lab=1.5,font.lab=1.8,lty=1,col="red")

lines(1:N,mem_fit$est_in_s,col="black",lwd=3)
lines(1:N,amem_fit$est_in_s,col="green",lwd=3)

axis(side=1, # 
     end_x, # 
     2000:2020,cex.axis = 0.95,xpd=T) #

legend("topright",
       c("Realized Vol.","MEM(1,1) Real. Vol.","A-MEM(1,1) Real. Vol."),
       col=c("red","black","green"),
       lty=c(1,1,1),
       lwd=c(3,3,3),
       cex=2)

dev.off()

### A-MEM-MIDAS model GDP ###### NOT WORKING

amem_midas_fit_gdp<-umemfit(model="MEMMIDAS",skew="YES",x=RVol,
                            daily_ret=r_t_unit4,mv_m=gdp_mv,K=4)
amem_midas_fit_gdp
summary.rumidas(amem_midas_fit_gdp)

names(amem_midas_fit_gdp)

### plot


plot(1:N,coredata(RVol),type="l",lwd=3,
     xaxt="n",
     xlab="",ylab="Volatility",main="SSMI-GDP",
     cex.lab=1.5,font.lab=1.8,lty=1,col="red")

lines(1:N,amem_midas_fit_gdp$est_in_s,col="black",lwd=3)

axis(side=1, # 
     end_x, # 
     2013:2020,cex.axis = 0.95,xpd=T) #

legend("topright",
       c("Realized Vol.","A-MEM-MIDAS Real. Vol."),
       col=c("red","black"),
       lty=c(1,1),
       lwd=c(3,3),
       cex=1)

dev.off()

### MEM-MIDAS model GDP  #### NOT WORKING


mem_midas_fit_gdp <- umemfit(model="MEMMIDAS",skew="NO",x=RVol,
                           daily_ret=r_t_unit4,mv_m=gdp_mv,K=4)
mem_midas_fit_gdp
summary.rumidas(mem_midas_fit_gdp)

names(mem_midas_fit_gdp)

### plot


plot(1:N,coredata(RVol),type="l",lwd=3,
     xaxt="n",
     xlab="",ylab="Volatility",main="SSMI-GDP",
     cex.lab=1.5,font.lab=1.8,lty=1,col="red")

lines(1:N,mem_midas_fit_gdp$est_in_s,col="black",lwd=3)
lines(1:N,amem_midas_fit_gdp$est_in_s,col="green",lwd=3)

axis(side=1, # 
     end_x, # 
     2013:2020,cex.axis = 0.95,xpd=T) #

legend("topright",
       c("Realized Vol.","MEM-MIDAS Real. Vol.","A-MEM-MIDAS"),
       col=c("red","black","green"),
       lty=c(1,1,1),
       lwd=c(3,3,3),
       cex=1)

dev.off()


##################### HAR model

source(file="har_functions.R")

rv.mean<-mean(RVol)

rv_1<-lag(RVol)
rv_5<-.meanLags(RVol, 1:5 , rv.mean)
rv_22<-.meanLags(RVol, 1:22 , rv.mean)

data_in_sample<-cbind(rv_1,rv_5,rv_22)

Y<-RVol
X<-data_in_sample

fit_har<-lm(coredata(Y)~coredata(X))
summary(fit_har)

HAR_hat<-fitted(fit_har)
HAR_hat<-c(mean(HAR_hat),HAR_hat)

loss_har<-((HAR_hat-RVol)^2)

mean(loss_har)

names(fit_har)

## plot

pdf(file="HAR.pdf",width=16,height=9)

plot(1:N,coredata(RVol),type="l",lwd=3,
     xaxt="n",
     xlab="",ylab="Volatility",main="SSMI",
     cex.lab=1.5,font.lab=1.8,lty=1,col="red")

lines(1:N,HAR_hat,col="green",lwd=3)

axis(side=1, # 
     end_x, # 
     2000:2020,cex.axis = 0.95,xpd=T) #

legend("topright",
       c("Realized Vol.","HAR"),
       col=c("red","green"),
       lty=c(1,1),
       lwd=c(3,3),
       cex=2)

dev.off()

##################### A-HAR model (IT IS NOT CREATING THE A-HAR LINE)

r_t_neg<-ifelse(lag(r_t_unit4)<0,1,0)

data_in_sample<-cbind(rv_1,rv_5,rv_22,r_t_neg*rv_1)

Y<-RVol
X<-data_in_sample

fit_ahar<-lm(coredata(Y)~coredata(X))
summary(fit_ahar)

AHAR_hat<-fitted(fit_ahar)
AHAR_hat<-c(mean(AHAR_hat),AHAR_hat)

loss_ahar<-((AHAR_hat-RVol)^2)

mean(loss_ahar)
mean(loss_ahar*1000)
mean(loss_har*1000)

pdf(file="A-HAR.pdf",width=16,height=9)

plot(1:N,coredata(RVol),type="l",lwd=3,
     xaxt="n",
     xlab="",ylab="Volatility",main="SSMI",
     cex.lab=1.5,font.lab=1.8,lty=1,col="red")

lines(1:N,AHAR_hat,col="blue",lwd=3)
lines(1:N,HAR_hat,col="green",lwd=3)

axis(side=1, # 
     end_x, # 
     2000:2020,cex.axis = 0.95,xpd=T) #

legend("topright",
       c("Realized Vol.","A-HAR","HAR"),
       col=c("red","blue","green"),
       lty=c(1,1,1),
       lwd=c(3,3,3),
       cex=2)

dev.off()

##################### Realized GARCH model  (NOT WORKING)

#####################

garch_spec = ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)), 
                        mean.model=list(armaOrder=c(0,0), include.mean=FALSE),  
                        distribution.model="norm")

#####################

spec_real_garch_norm <- ugarchspec(variance.model=list(model="realGARCH", garchOrder=c(1,1)), 
                          mean.model=list(armaOrder=c(0,0), include.mean=FALSE),  
                          distribution.model="norm")

fit_rgarch<-ugarchfit(data=r_t_unit4,spec=spec_real_garch_norm,solver = 'hybrid', realizedVol = RVol) ## Not working

fit_rgarch@fit$robust.matcoef

## store the conditional variance

rgarch_hat<-fit_rgarch@fit$sigma

## MSE:

mean((rgarch_hat-RVol)^2)

############################################### plot

pdf(file="rgarch.pdf",width=16,height=9)

plot(1:N,coredata(RVol),type="l",lwd=3,
     xaxt="n",
     xlab="",ylab="Volatility",main="SSMI",
     cex.lab=1.5,font.lab=1.8,lty=1,col="red")

lines(1:N,rgarch_hat,col="grey",lwd=3)

axis(side=1, # 
     end_x, # 
     2000:2020,cex.axis = 0.95,xpd=T) #

legend("topright",
       c("Realized Vol.","Realized GARCH"),
       col=c("red","grey"),
       lty=c(1,1),
       lwd=c(3,3),
       cex=2)

dev.off()

