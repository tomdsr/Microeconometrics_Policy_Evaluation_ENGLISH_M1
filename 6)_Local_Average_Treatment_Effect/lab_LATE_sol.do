clear all
set more off

cd "C:\Users\munoza\Google Drive\TSE\Teaching\Program Evaluation\2021-2022\Lab sessions\Lab LATE"

use td_LATE.dta, clear

* Question 2: regress labor supply variables on fertility
gen hourym = weeksm*hoursm
label var hourym "moms yearly hours worked"
gen partm = hourym>0
label var partm "1 = in the labor force"

reg weeksm gradem agem kidcount graded aged income1d
reg partm gradem agem kidcount graded aged income1d
reg hourym gradem agem kidcount graded aged income1d
reg income1m gradem agem kidcount graded aged income1d

* Question 5: restrict to the population of households with at least 2 children
gen km2 = kidcount >=2
label var km2 "1 = at least two kids"
bys km2: sum kidcount weeksm gradem agem income1m partm hourym

* Question 6: Strength of the instrument (with propensity score)
gen treatment = kidcount>=3
gen instrument = (sexk==sex2nd)
label var treatment "kidcount >= 3"
label var instrument "Instrument: the sex of the first two born is the same"
tab instrument treatment
corr instrument treatment

  //we produce the propensity scores:
probit treatment gradem agem aged graded income1d instrument
logit treatment gradem agem aged graded income1d instrument
reg treatment gradem agem aged graded income1d instrument

* Question 7: Predict the propensity score
logit treatment gradem agem aged graded income1d instrument
predict phat, p
label var phat "Pr(treatment | gradem agem aged instrument)"

bys treatment: sum phat, det

graph twoway (kdensity phat), graphregion(color(white))
graph twoway (kdensity phat if treatment==0) (kdensity phat if treatment==1), legend(label(1 "Control") label(2 "Treatment"))	graphregion(color(white))



* Question 8: 2SLS

ivregress 2sls income1m (treatment=instrument) //we can compare this result with the direct OLS estimation		
eststo twosls_inc
reg income1m treatment
eststo ols_inc

ivregress 2sls hourym (treatment=instrument)
eststo twosls_hour	
reg hourym treatment
eststo ols_hour

ivregress 2sls weeksm (treatment=instrument) 
eststo twosls_week
reg weeksm treatment 
eststo ols_week

esttab twosls_inc ols_inc twosls_hour ols_hour twosls_week ols_week using twosls_ols.tex, replace ///
title("2SLS vs OLS") unstack r2 se

* Question 9: Compute the Wald estimator
sort instrument
by instrument: sum treatment weeksm income1m partm hourym

// ratio of the two regressions
reg income1m instrument
scalar yz= _b[instrument]
reg treatment instrument
scalar yd= _b[instrument]

display yz/yd
//We want to directly calculate the Wald estimator (for incomem), by using the direct formula [E(Y|Z=1) - E(Y|Z=0)] / [E(D|Z=1) - E(D|Z=0)]
 summ income1m if instrument==1
 scalar YZ1 = r(mean)
 summ income1m if instrument==0
 scalar YZ0 = r(mean)
 summ treatment if instrument==1
 scalar DZ1 = r(mean)
 summ treatment if instrument==0
 scalar DZ0 = r(mean)
 display (YZ1 - YZ0) / (DZ1 - DZ0)


* Question 10: 2SLS with different covariates and different instruments
reg income1m treatment gradem agem aged graded income1d instrument 	//OLS specification (likely biased)
eststo ols_income_ten
ivregress 2sls income1m gradem agem aged graded income1d (treatment=instrument) 	//2SLS IV specification
eststo twosls_income_ten

reg hourym treatment gradem agem aged graded income1d instrument 	//OLS specification (likely biased)
eststo ols_hours_ten
ivregress 2sls hourym gradem agem aged graded income1d (treatment=instrument) 	//2SLS IV specification
eststo twosls_hours_ten

reg weeksm treatment gradem agem aged graded income1d instrument 	//OLS specification (likely biased)
eststo ols_week_ten
ivregress 2sls weeksm gradem agem aged graded income1d (treatment=instrument) 	//2SLS IV specification
eststo twosls_week_ten


esttab ols_income_ten twosls_income_ten ols_hours_ten twosls_hours_ten ols_week_ten twosls_week_ten using twosls_ols_ten.tex, replace ///
title("2SLS vs OLS") unstack r2 se

* New instrument is that the first two children are both female
gen two_girls =instrument*sexk*sex2nd  
label var two_girls "Instrument: the sex of the first two children are both female" 


reg treatment gradem agem aged graded income1d instrument two_girls sexk				//we check strength of the instruments	
corr treatment instrument two_girls
ivregress 2sls weeksm gradem agem aged graded income1d sexk (treatment=instrument two_girls)		//2 instruments

reg income1m treatment gradem agem aged graded income1d instrument two_girls 	//OLS specification (likely biased)
eststo ols_income_ten
ivregress 2sls income1m gradem agem aged graded income1d (treatment=instrument two_girls) 	//2SLS IV specification
eststo twosls_income_ten

reg hourym treatment gradem agem aged graded income1d instrument two_girls	//OLS specification (likely biased)
eststo ols_hours_ten
ivregress 2sls hourym gradem agem aged graded income1d (treatment=instrument two_girls) 	//2SLS IV specification
eststo twosls_hours_ten

reg weeksm treatment gradem agem aged graded income1d instrument two_girls	//OLS specification (likely biased)
eststo ols_week_ten
ivregress 2sls weeksm gradem agem aged graded income1d (treatment=instrument two_girls) 	//2SLS IV specification
eststo twosls_week_ten


esttab ols_income_ten twosls_income_ten ols_hours_ten twosls_hours_ten ols_week_ten twosls_week_ten using twosls_ols_ten.tex, replace ///
title("2SLS vs OLS") unstack r2 se


* Question 11: Testing the overidentifying restrictions
ivregress 2sls income1m gradem agem aged graded income1d (treatment=instrument two_girls) 	//2SLS IV specification
estat overid 
ivregress 2sls hourym gradem agem aged graded income1d (treatment=instrument two_girls) 	//2SLS IV specification
estat overid 
ivregress 2sls weeksm gradem agem aged graded income1d (treatment=instrument two_girls) 	//2SLS IV specification
estat overid 


ivregress 2sls income1m gradem agem aged graded income1d (treatment=instrument two_girls) if sexk == 1
eststo twosls_income_girl
ivregress 2sls income1m gradem agem aged graded income1d (treatment=instrument two_girls) if sexk == 0
eststo twosls_income_boy

ivregress 2sls hourym gradem agem aged graded income1d (treatment=instrument two_girls)	if sexk == 1
eststo twosls_hours_girl
ivregress 2sls hourym gradem agem aged graded income1d (treatment=instrument two_girls) if sexk == 0
eststo twosls_hours_boy


ivregress 2sls weeksm gradem agem aged graded income1d (treatment=instrument) if sexk == 1
eststo twosls_week_girl
ivregress 2sls weeksm gradem agem aged graded income1d (treatment=instrument) if sexk == 0
eststo twosls_week_boy

 
esttab twosls_income_girl twosls_income_boy twosls_hours_girl twosls_hours_boy twosls_week_girl twosls_week_boy using twosls_eleven.tex, replace ///
title("2SLS vs OLS") unstack r2 se



