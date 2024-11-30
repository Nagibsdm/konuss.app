* Linearity lecture *

* we will see wheter data meet assumptions of linearity in the relationship between the variables of interest *
* If not there will be used a series of transformations over the data *
* There will be also chiecked the issue of poolability of the observations *

* we are going to use the CHOW.dta dataset that is a cross section dataset *
* run summ lexcp (life expectancy) gdp to obtain some descriptive statistics *
summ lexp gdp
* run a visual inspection that gives us a data cloud: there is not a straight line = first sign of non linearity *
graph7 lexp gdp , s([cid]) xlabel(0,5000,10000,15000,20000) ylabel
* 1: strong variability of the life expectancy at low levels of per capita incomes *
* 2: Even with high incomes, the life expectancy tends to stabilize to a fixed level *
* 3: strong income variability between countries, though adjusted of different levels of prices *


* FIRST MODEL *
* run reg lexp gdp: we are trying to explain the life expectancy through the per capita income *
reg lexp gdp
* there is one regressor: though it is able to catch half of the variability of the dependent variable (R^2 coefficient) *
* Taking a look at the interpretation of the parameter estimates: *
* 1: when the gdp is 0, the expected life is 55.6 years *
* 2: the beta coefficient of gdp means that when the regressor increase of 1 dollar then the lexp increases of the coefficient value (0.0014 years) of the gdp per year *
* predict fit_lin *
predict fit_lin
* the previous command allows us to save the fitted value for the previous regression analysis (FIRST MODEL) *
* through the rvfplot we can see the residuals vs fitted values and if our model is good we should find a data cloud that does not follows a clear pattern (random flow) *
version 7: rvfplot, yline(0) ylabel xlabel oneway twoway box
* some residuals should be under the yline(0) and some other above the yline(0) *
* there we have a really clear pattern (parabola) *
* in this case we should fix some functional form problem * 
* non-normality and heteroskedasticity of the residuals are very probable *
* now run predict reslin, res to predict the residuals *
predict reslin, res
* check with sktest reslin ìfor the skewness kurtosis of the residuals *
sktest reslin
* the residuals are affected of fat tails *

* check for the heteroskedasticity through hettest *
hettest
* we have heteroskedasticity *

* ovtest to check the ramsey test to see if there is some omitted values *
ovtest

* we have a problem of omitted variable, heteroskedasticity and normality of the regression *

* the first thing to solve the issue is the following: log the regressor for a more reasonable model specification since the relationship between the regressor (gdp) and the dependent variable (life expectancy) is not linear *
* for this purpose: the log could be helpful to linearize the gdp *
* so generate the log gdp and put in the model * 
g lgdp = log(gdp)
* now graph the new relationship between gdp and log gdp: the relationship is really similar to the previous relationship *
graph7 lgdp gdp
* now looking at the graph of lexp and log gdp: the cloud is significantly much more linear *
graph7 lexp lgdp, xlabel ylabel s([cid])

* SECOND MODEL *
* running again the regression with the log gdp instead of gdp *
reg lexp lgdp
* the regression output suggests more sensible results than those from the linear model do *
* the log gdp (looking at the R^2) is able to explain the 80% (against the 60% of the gdp regressor) of the lexp *
* the two R^2 values are comparable since the dependent variable is the same *
* pay attention to the economics problem related to the trasformation of gdp into a natural logarithm *
* our log linear model is definitely better than the linear model * 
* save the fitted data of the transformed model *
predict fit_linlog
* looking at the predictions of the two models , the second one (lgdp) obtained a better outcome than the first one *
graph7 lexp fit_lin fit_linlog gdp, s(o..) c(.ls) xlabel ylabel(40,50,60,70,80)
* through the residual vs fitted value graph the clear pattern of the previous model is almost disappaered *
version 7: rvfplot, yline(0) ylabel xlabel oneway twoway box
* in fact some residuals are over the line and others are below *
* checking for the sktest we predict the residuals and then we can see that we do not have a problem with the kurtosis but we have a problem with the skewness and so we still have a problem with the normality *
predict reslog, res
sktest reslog

* for the heteroskedastiocity: the hettest shows the homoskedasticity. of the regressors *
hettest
* through the ovtest: we are still not able to reject the null (even with a significance decreases of the F) so this means that again we have a functional form non linearity problem *
ovtest
* it suggests to us that we have to fix more problems in the linearity: only one transformation it does not work *
* we should use two different betas with two different slopes given the significantly different kind of scenarios that we can find in our cross section data *
* there is too much heterogeneity in our dataset *

* DA QUI IN POI DA RIVEDERE * 

* A really famous diagnostic test to inspect for a problem in the functional form * 
* the idea of the chow test is to split the dataset in two sample: the problem is that we meant to know an a priori relation of heterogeneity *
* in order words: the problem is how we are considering some of the observation in the middle of the data in particular if those belong to one sample or the other *
* the simplest idea is to use the median: in this way we can create two samples with the same number of observations *
* through summ gdp, d we can see the median of the regressor but the skewness problem of the regressor also *
* now we create a dummy variable in the following way 'g poor = gdp<2600' as the lowest sample *

* now checking for the fitted values graph we can see that there is a clear linearity in the oberservations *
* to test the poolability *
* run reg lexp lgdp if poor==1   *
* the estimated betas coefficient isn 10.. and something *
* then run reg lexp lgdp if poor==0 *
* the estimated betas coefficient is more or less an half of the previous regression model *

* first of all calculate the F-test in this way: calculate the RSS for the unrestricted model that is equal to the sum of the two sub sample RSS (poor==1+poor==0) *
* then calculate the RSS restricted model value that is the model with the lgdp regressor *
* comparing the RSS of the two model we obtain a value of 8,... *

* SECOND WAY TO RUN A CHOW TEST USING AGAIN A DUMMY VARIABLE *
* the result is the same *
* according to the professor is that we can immediately compare the RSS through the first way  *
*in the second way we have to perform an f-test on the regression * 


* In conclusion compairing the two ways with a visual inspection: *
* the problem is what happens in the middle, in particular where we split the data *
* choosing two different sub sample we are considering *


* An idea it could be: to check our subsample in three different way through three different straight lines *



