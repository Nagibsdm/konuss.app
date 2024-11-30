
# LOS GRAFICOS NO DICEN EL NOMBRE DEL MODELO


# sGARCH-norm VaR (1%),sGARCH-norm ES (1%)

pdf("VaR_garch_11_norm_SSMI.pdf",height=9,width=16)
par(mai=c(1,1,0.1,0.3))

plot(1:N,coredata(r_t_est),type="l",
     xaxt="n",ylim=range(r_t_est,ES_garch_11_norm),
     xlab="",ylab="SSMI: Returns, VaR and ES (1%)",
     cex.lab=2,font.lab=2,lty=1,col="black",lwd=2)

axis(side=1, 
     end_x[1:(length(end_x))], 
     c(2000:2020),cex.axis = 1.1,xpd=T)

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




pdf("VaR_AS_SSMI.pdf",height=9,width=16)
par(mai=c(1,1,0.5,0.3))

plot(1:N,coredata(r_t_est),type="l",
     xaxt="n",ylim=range(r_t_est,ES_AS),
     xlab="",ylab="SSMI: Returns, VaR and ES (1%)",
     cex.lab=2,font.lab=2,lty=1,col="black",lwd=2)

axis(side=1, 
     end_x[1:(length(end_x))], 
     c(2000:2021),cex.axis = 1.1,xpd=T)


lines(1:N,VaR_AS,col="grey",type="l",lwd=2)
lines(1:N,ES_AS,col="red",type="l",lwd=2)

legend(
  "topright",
  c("SSSMI daily ret.","AS VaR (1%)","ES VaR (1%)"),
  lty=1,
  col=c("black","grey","red"),
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
     c(2011:2019),cex.axis = 1.1,xpd=T)


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


##################################### linear ARCH-MIDAS



pdf("VaR_lARCHMIDAS_SSMI.pdf",height=9,width=16)
par(mai=c(1,1,0.5,0.3))

plot(1:N,coredata(r_t_est),type="l",
     xaxt="n",ylim=range(r_t_est,ES_larchmidas),
     xlab="",ylab="SSMI: Returns and VaR (1%)",
     cex.lab=2,font.lab=2,lty=1,col="black",lwd=2)

axis(side=1, 
     end_x[1:(length(end_x))], 
     c(2000:2021),cex.axis = 1.1,xpd=T)


lines(1:N,VaR_larchmidas,col="blue",type="l",lwd=2)
lines(1:N,ES_larchmidas,col="grey",type="l",lwd=2)

legend(
  "topright",
  c("SSMI daily ret.","Linear ARCH-MIDAS VaR (1%)","Linear ARCH-MIDAS ES (1%)"),
  lty=1,
  col=c("black","blue","grey"),
  lwd=2)

grid(NA,NULL)

abline(v=end_x,h=NA,col="gray",lty=3)


dev.off()


################### PLOTS OF UNIT 4 





