* Esercitazione crime 

* first thing to do 
descr
edit

* cross section panel data

summ

* analyze the distribution of the variables 
summ crime murder pctwhite pcths poverty, d

* missing values
codebook crime murder pctwhite pcths poverty
* there are not missing values 
* check inspect 
inspect crime murder pctwhite pcths poverty
* poverty and graduates seem to be normally distributed, more test should be performed to see this assumption

* check for some outlier through 'iqr'
iqr crime 
iqr murder 
iqr pctwhite 
iqr pcths 
iqr poverty
* there are two severe outliers: one in murder and one in pctwhite

* run the reg on the previous regressors 
reg crime murder pctwhite pcths poverty
* the only significant regressor is murder
* the R^2 value shows that those regressors explain about 80% of the variability of the regressor

* check the joint significance 
test pctwhite pcths poverty
test pcths poverty
test pctwhite pcths
test pctwhite poverty
* since we do not reject the null each regressors is able to explain at least a part of the variability of the dependent

ereturn list
estat ic

* check for multicollinearity
pwcorr crime murder pctwhite pcths poverty, sig star(.05) sid
* visual inspection 
graph7 crime murder pctwhite pcths poverty, matrix half label

collin murder pctwhite pcths poverty
* vif and tolerance are significant 
* the R^2 value has to be lower than 0.80 and so there is not an high value of collinearity 
* the problem is the condition number that is significantly higher than 30
* despite that there are 3 statistics against 1

predict crimehat, xb
predict resl, res

* Analysis of the residuals
* Diagnostic tests 
* Normality of the residuals

sktest resl
sfrancia resl
swilk resl 
chens resl
omninorm resl
* from the tests above we can state that the residuals are normallu distributed
* graphical inspection 
kdensity res, normal
* heteroskedastic diagnostic
hettest 
whitetst
estat szroeter, rhs
imtest
lmhgl crime murder pctwhite pcths poverty
* all the tests show the presence of homoskedasticity
qui reg crime murder pctwhite pcths poverty
regcheck

* check for a specification problem 
linktest
* the linktest shows that we have a linearity problem since all the p-value are significant 
ovtest

* Check for outliers 
predict D, cooksd
* there is a problem of NON LINEARITY 
* WE SHOULD TRY TO USE SOME SPECIFICATION 
g lcrime = log(crime)
g lpcths = log(pcths)
g lpoverty = log(poverty)
g lpctwhite = log(pctwhite)
g poverty2 = poverty^2
g pcths2 = pcths^2
g pctwhite2 = pctwhite^2
reg lcrime murder pctwhite pcths poverty
reg crime murder pctwhite lpcths poverty
reg crime murder pctwhite pcths lpoverty
reg crime murder lpctwhite pcths poverty
* all the previous transformations do not work
reg crime murder pctwhite pcths poverty poverty2
reg crime murder pctwhite pcths pcths2 poverty poverty2
* Those models seem to be the best one since all the regressors are signiificant
reg crime murder pctwhite poverty poverty2
reg crime murder pctwhite pctwhite2 poverty poverty2
* Through the quantile regression we can state that the best one out all is the first one
qreg crime murder pctwhite poverty poverty2
