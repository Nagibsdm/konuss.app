
# Tuvimos que eliminar este modelo para tau -> 0.01: real_gdp_VaR_dagm_s_std & real_gdp_ES_dagm_s_std



#######################################
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




##########################################################
##########################################################
#                       UNIT 9                           #
##########################################################
##########################################################

library(xts)
library(rugarch)
library(rumidas)
library(highfrequency)
library(rqmidas)
library(GAS)
library(xtable)


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

#31


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


# Son 30 modelos 

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
colnames(db_loss)<-rownames(col_res)  ### FINO A QUA FUNZIONA 


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


################


