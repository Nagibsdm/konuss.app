********Analisi descrittiva***********
describe
summarize
summ, d
*findit iqr*
iqr mpg
iqr cyl
iqr eng
iqr wgt
inspect mpg cyl eng wgt
codebook mpg cyl eng wgt
kdensity mpg, normal
kdensity cyl, normal
kdensity eng, normal
kdensity wgt, normal
sktest mpg cyl eng wgt
sfrancia mpg cyl eng wgt
swilk mpg cyl eng wgt
*write findit chens*
chens mpg cyl eng wgt
*findit onmninorm*
omninorm mpg cyl eng wgt
omninorm mpg
omninorm cyl
omninorm eng
omninorm wgt
regcheck
********************Multicollinearity**************************
pwcorr mpg cyl eng wgt, star(.05) sid
collin cyl eng wgt
reg mpg cyl eng wgt
estat vif
fgtest cyl eng wgt

*****Before checking for Heterskedasticity we fix the multicollinearity problem by removing the regressor cyl*************
reg mpg eng wgt
reg mpg eng wgt, beta
********Heterskedasticity************
estat hettest
 *reject the null hypothesis becasue p value smaller than 0.05 so we have heteroskedasticity*
 estat hettest, fstat
 estat hettest, rhs
 ***findit bpagan**
 bpagan eng wgt
 *findit whitetst*
 whitetst
 estat imtest
 estat szroeter, rhs
 *******fixing heteroskedasticity*******
 reg mpg eng wgt, robust
 ****fitting values of mpg and residuals****
 predict mpghat, xb
 predict res, res
 ***** After getting fitted values and residuals we check for the normalty assumption of the residuals****
 kdensity res, normal
 sktest res
 sfrancia res
 swilk res
 chens res 
 omninorm res
 ****Checking for misspecifications using ramsey reset test*
 ovtest
 ***if we find that we have variables causing the non linearity of the model we can try to use powers of the variables to fix the problem***
 gen eng2=eng^2
 gen wgt2=wgt^2
 *finding the correct model*
 reg mpg eng wgt eng2 wgt2, robust
 ereturn list
 estat ic
 test eng eng2
 
 reg mpg eng wgt wgt2, robust
 ereturn list
 estat ic
 *akaike criteria to determine which model is the best and for choose which is better we ll see AIC*
 
 ***Checking for Outliers**
 reg mpg eng wgt wgt2,robust
 predict mpghat1, xb
 predict res1, res
 predict D, cooksd
 twoway scatter res1 mpghat1 [w=D], yline(0)
 
 ***Robust Regression*
 rreg mpg eng wgt wgt2
 qreg mpg eng wgt wgt2
 iqreg mpg eng wgt wgt2, quantile(.25 .75) reps(100)
 sqreg mpg eng wgt wgt2, quantile(.25 .50 .75) reps(100)
 bsqreg mpg eng wgt wgt2