**nagib stefano souki 1793926** 
*he use summarize to obtain the descriptive statistics of the variables*
summarize
summ, d
*summ, d in way to have a datailed statistic analysis of the variables * 
*with inspect we can have a graphic view pf the variables* 
inspect saleprice lotsize nbedroom nbath nstories driveway recroom basement gas aircond ngarage desireloc 
*with the graphic view we can say wich variable is normal distributed* 
codebook 
*we use codebook to see if there are any missing values* 
*we have 0 missing values in the dataset* 

reg saleprice
*reg to estimate saleprice on the constant* 
*_cons is the mean of the variable 68121.6 * 

*estimate of saleproce on lotsize* 
reg saleprice lotsize
*with this result we can afirm that one unit change in logsize (indipendent variable) rapresent a variation of  6.598768 in saleprice (dipendent variable)* 

*estimate saleprice on lotsize basement and nbedroom* 
reg saleprice lotsize basement nbedroom 
test lotsize basement nbedroom 
test lotsize
test basement
test nbedroom
* we use standardized regression in way to see which regressors have a bigger impact in the dipendet variable* 

reg saleprice lotsize basement nbedroom, beta
*to get inf criteria and the log likehood of the regression we use the estat ic* 
estat ic

*the akaike info criteria is 12418.12 and the loglikehood of the model is -6205.058*
*we can run regcheck too see the problems we can have in our model and the use the differents test to verify and correct* 
regcheck 
*we can see that we have an heteroskedasticity and the non normality of the residuals and a functional form of our problems which that we should need take in consideeration powers of the regressors* 

*checking for heteroskedasticity* 
hettest 
bpagan saleprice lotsize basement nbedroom 
estat hettest, rhs 
whitetst 
estat szroeter, rhs 
estat imtest 
*with these test we can say for sure that we have a heteroskediasticity problem* 

*from regchech we can see that we don't have any multicollinearity problem so we procede to verify it* 

collin lotsize basement nbedroom 
*looking at the collin test we can see that the values of VIF are around 1 and the tollerance doesn't surpass the 0.1 , and also for the R squared no value is above 0.8 and the condition number is 10 which is less than 30* 

*we also see that we have a problem with non normality of the residuals so we can check it with some test* 
predict pricehat 
predict res1, res 

*tests for the normality of the residuals* 

kdensity res1, normal 
sktest res1 
swilk res1 
sfrancia res1 
chens res1 
omninorm res1 

*with the test we can afirm that we have the problem in the normal distribution of the residuals*

*functional and misspecification* 
ovtest 
*we reject the null hypotesis because the pvalue is lower than 5%, this means that we have some omited variables in our model and we shloud to include poewrs of the regression* 

*we generate the powers regression* 
generate lotsize2=lotsize^2
generate lotsize3=lotsize^3

reg saleprice lotsize lotsize2 basement nbedroom, beta 


linktest 
*linktest is useful to check specification problem* 

*correlation matrix* 
pwcorr saleprice lotsize basement nbedroom, sig star(.05) sid  

*robust* 
reg saleprice lotsize basement nbedroom, robust 
*with robust we can fix the heteroskedasticity* 

*outliers* 
rreg saleprice lotsize basement nbedroom 

*with the robust regression we can fix outliers problems* 

