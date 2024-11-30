* esercitazione *
sysuse auto 

* first thing to do *
descr

* explain the weight of the cars *
* open the dataset on the editor *
* we have a dataset since we do not have a time series *
* analyze shortly the variable *
* set the price as dependent variable *

summ

* analyze the distribution of the regressors *

summ price mpg rep78 headroom trunk, d
* verify mean median kurtosis skewness *

* if we want more descriptive statistics *
inspect price mpg rep78 headroom trunk
* to see for missing value *
codebook price mpg rep78 headroom trunk

* to know if we have some outliers *
iqr price 
iqr mpg 
iqr rep78 
iqr headroom 
iqr trunk
* check for the left and right mild and severe outliers *
* the most problematic probably is the dependent variable *

reg price mpg rep78 headroom trunk
* mpg and rep78 are signifficant against headroom and truck that are not *
* looking at the R^2 we explain only 1/4 of the variability *
* we have 69 observation with 5 degrees of freedom for each beta and 1 degree of freedom for the constant *

* check the joint signficance through the f-test *
test rep78 headroom trunk
test headroom trunk 
* why we have to launch an f-test over two non signficance regressors *
* to see if there is multicollinearity * 
ereturn list
estat ic

predict pricehat, xb
predict resl, res

* conduct the diagnostic *
* analysis of the residuals *
* normality of the residuals *

sktest resl
sfrancia resl
swilk resl 
chens resl
omninorm resl
 
* heteroskedasticity *
hettest
hettest, rhs
* hettest shows hetroskedastic while hettest, rhs shows homoskedastic *
whitetst 
* its p-value is 0.4 so there is a problem with the previous results *
* we have to inspect with the szroeter test *
estat szroeter, rhs
* single heteroskedastic for each regressor *
* the prblem is mpg *
estat imtest 

* given the previous tests we have to correct for heteroskedasticity *
reg price mpg rep78 headroom trunk, robust
* headroom and trunk still are not significant *
* in the case that we have to remove a non mandatory regressor we should drop trunk since it is the not-significant regressor with the highest p-value *

* multicollinearity assumption *
pwcorr price mpg rep78 headroom trunk, sig star(.05) sid
* strong negative collinearity between mpg and price *
* even mpg and rep *
* also trunk and mpg *

* we have to use some test to check for multicollinearity *
collin mpg rep78 headroom trunk
* vif and tolerance are ok *
* auxiliary regression: the other regressors explain the variability of the regressor through the R^2 *

reg price mpg rep78 headroom trunk
linktest 
* if our model is weel specified the hat squared value should not be significant *
* the value is around 10% so we can believe that our model is well specified * 

* another test on non linearity *
ovtest
* the null is accepted and so the model is well specified *

* JUST TO EXCERCISE AND CHECK FOR NON LINEARITY *
g mpg2 = mpg^2
g mpg3=mpg^3
g headroom2=headroom^2

reg price mpg mpg2 rep78 headroom trunk, robust
* I checked for mpg because it has a really high correlation with the other regressors *
reg price mpg mpg2 rep78 headroom, robust
test mpg mpg2
* to have another prove of non collinearity * 
* after checking the previous models, we should use and keep the following model *
* if we keep the squared variable, we have to keep also the non squared regressor *
* remember that if the f-test shows a p-value lower than 5% the regressors should be mantained inside the model *

* THIS IS ONE OF OUR BEST MODEL *
reg price mpg mpg2 mpg3 headroom, robust
* 	THE FOLLOWING IS OUR LAST MODEL *
* if we can exclude the other regressor obviously *
reg price mpg mpg2 mpg3 headroom headroom2, robust

* if we have problem with outliers we should run the following regression and then check through the cooks distance *
rreg price mpg mpg2 mpg3 headroom headroom2
qreg price mpg mpg2 mpg3 headroom headroom2
sqreg price mpg mpg2 mpg3 headroom headroom2
iqreg price mpg mpg2 mpg3 headroom headroom2
bsqreg price mpg mpg2 mpg3 headroom headroom2
mmregress price mpg mpg2 mpg3 headroom headroom2
* is our choice to use logs or powers to deal with linearity *

