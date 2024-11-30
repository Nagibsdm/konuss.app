summarize
summarize CO2emissionspc rgdppc pceu

keep if CO2emissionspc!=. & rgdppc=. & pceu!=. 
gen time_n
tsset time 

describe CO2emissionspc rgdppc pceu 

tsline CO2emissionspc rgdppc pceu 
** rgpdc has an increasing trend and CO2 emissions stay constant around 0 and pceu has a weak increasing trend** 

**AC represent the calculated autocorrelation at each lag. PAC represents the calculated partial autocorrelation at each leag. Q will provide the Portmanteau test statistic and the associated pvalue. null hypothesis is that the autocorrelation is absent** 
corrgram CO2emissionspc 

**in the ACF we have a decreasing pattern and in the PACF there is a big a spike at the start so we presume this is an AR(1) process 
ac CO2emissionspc
pac CO2emissionspc

arima CO2emissionspc, arima (1,0,0)
*L1  is significant at 1%* 
ereturn list 
estat ic 
est store ARMA100

arima CO2emissionspc, arima (2,0,0)
*L2 is not significant so is a AR(1)* 

corrgram rgdppc 

ac rgdppc
pac rgdppc 

arima rgdppc, arima (1,0,0)
*L1 is significant at 1%* 
ereturn list
estat ic 
est store ARMARG100 
arima rgdppc, arima (2,0,0)
*L2 is significant at 1% so this could be the best model* 
ereturn list 
estat ic 
est store ARMARG200

arima rgdppc, arima (3,0,0)
**L3 is above 3% so it is no significant** 

corrgram pceu 


ac pceu 
pac pceu 

arima pceu, arima (1,0,0)
*L1 significant at 1%* 
ereturn list 
estat ic 
est store ARMAPC100

arima pceu, arima (2,0,0) 
*L2 not significant* 
**L2 is not significant so the best model is an AR(1) arima (1,0,0)

*the table help us to compare the models and choose the best* 
estimates table ARMARG100 ARMARG200, stats (11 aic bic) 
*we shpuld maximize L1 and minimize aic and bic doing it we can say that the best model is ARMARG200 which is a AR(2) process* 






****** stationary test****** 
dfuller CO2emissionspc, lag(2) reg trend 
*the serie is not stationary because the test statistic is bigger than the critical value* 
dfuller CO2emissionspc, lag(2) reg notrend 
*still not stationary* 

dfuller CO2emissionspc, lag(1) reg notrend
*from the dicky fuller test we can assume that this variable is not stationary* 


pperron CO2emissionspc, lag(1) reg notrend 
*we can see that the first lag is significant but the test statistic is still greater that the critical value so it's not stationary* 

dfgls CO2emissionspc, maxlag(2) notrend 
*from the result we can assume that the variable is not stationary* 

*the 3 test that we had done confirm that is not stationary* 

dfuller rgdppc, lag(2) reg trend 

*the second lag and the trend are not significant, the t statistuc is bigger than the critical value so it's not stationary* 

dfuller rgdppc, lag(2) reg notrend 
*since the trend is not significant we can try to do the same test without the trend, we had the same result telling us that it's not stationary* 
 
 
 dfuller rgdppc, lag(1) reg notrend 
 *here we can see that the first lag is not significant and the t statistic is bigger than the critical value so it's no stationary* 
 
 pperron rgdppc, lag(1) reg notrend 
 *L1 is significat but the t statistic is bigger than the critical value* 
 
 dfgls rgdppc, maxlag(2) notrend 
 
 *all the test confirm the non stationarity of the variable* 
 
 dfuller pceu, lag(2) reg trend 
 *L2 and the trend are not significant* 
 
 dfuller pceu, lag(2) reg notrend 
 
 dfuller pceu, lag(1) reg notrend 
 *not stationary* 
 
 
 pperron pceu, lag(1) reg notrend 
 
 
 dfgls pceu, maxlag(2) notrend 
 *this result confirm that the variable is not stationary* 
 
 vecrank CO2emissionspc rgdppc pceu, lags(2) trace max ic 
 *there is no cointegration because the trace is smaller than the critical value* 
 * 3 not stationary variables and 0 cointegration* 
 
 *we generate the difference of the 3 variables* 
 gen dCO2emissionspc = D.CO2emissionspc 
 gen drgdppc = D.rgdppc 
 gen dpceu = D.pceu 
 
 *now we can check for stationarity* 
 dfgls dCO2emissionspc, maxlag(2) trend 
 *stationarity* 
 
 dfgls dCO2emissionspc, maxlag(2) trend 
 *stationarity* 
 dfgls dpceu, maxlag(2) trend 
 *stationarity* 
 
 varsoc dCO2emissionspc drgdppc dpceu, maxlag(2)
 
 varbasic dCO2emissionspc drgdppc dpceu, lags(3) step(20) nograph 
 
 
 varstable, graph 
 *we can see all the eigenvalues lie inside the unit ciircle so there is stability*
 
 varlmar, mlag(3)
 *no autocorrelation in the residuals because all the p values are bigger than 5%*
 
 varwle 
 *not any significant parameters on lag(3)*
 
 varnorm 
 *AS we can see from the results that al the p values are smaller than than 5% so the residuals aren't normal distributed* 
 
 vargranger 
 *the p value is bigger than 5% so there is neutrality*
 
 pwcorr L1.dCO2emissionspc L1.drgdppc L1.dpceu, sig star(.05) sidak 
 *the most important results are between L.dpceu and L.dCO2emissionspc and L.dpceau and L.drgdppc but this correlations are not significant*
