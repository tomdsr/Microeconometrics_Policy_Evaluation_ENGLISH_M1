**********************************************************************
* M1 APPLIED ECONOMETRICS, Spring 2022 *
* Applied Econometrics - Master TSE 1 - 2021/2022
* Lecture 1: Introduction to Stata, Data Manipulation, Logit
* Short version
* LAST MODIFIED: December 2021 * BY: Lisa *
**********************************************************************

***********
* OUTLINE *
***********
* 1.1) merging datasets
* 1.2) exploratory commands
* 1.3) histograms
* 1.4) twoway graphs
* 2.1) data cleaning/trimming

* -------- end of session 1 --------

* 2.2) generating new variables
* 3.1) linear probability model
* 3.2) logit
* 3.4) Postestimation: predicted probabilities
* 3.5) marginal effects


clear all

*  DIRECTORY: were your files will be read from and saved to

cd "C:\Users\lili_\Dropbox\Applied Econometrics 2021_2022\2021_2022\Lab Session 1"


**********************  PART 1 *************************


* 1.1) LOADING THE DATA & MERGING DATASETS		
/* Note: Stata can only hold one dataset at a time */

** Individual characteristics (time-invariant)
import delimited using "Indiv.csv", clear
* Check for observations that appear more than once (mistake)
duplicates drop	
* In view of the merge, we need to check whether there is 1 individual id per observation
isid numenq
save "Indiv.dta", replace   // ".dta" is the stata format. we can write "Indiv" only, and by default it will be saved in stata format.

** Employment characteristics (time-varying)
import delimited using "Employment.csv", clear
duplicates drop
isid numenq // a given individual appears more than once 
isid numenq month // an observation is defined by an individual and a month

* We proceed to the merge & will save the resulting dataset in Stata format
merge m:1 numenq using Indiv // merge can be either 1:1, m:1 or 1:m
drop _merge
save TP1_in, replace 



* 1.2) DESCRIPTIVE STATISTICS

* See data 
browse

* Look at variable types
describe

* Distribution of a variable
summarize wage, detail

* Split the dataset in 2 (man=0 and man=1) and analyze distributions
bysort man: summarize wage

* Look at how many observations take each value of a variable
tabulate sch_lev

* Conditional on a value of "man", how many observations take each "sch_lev" value? 
tab man sch_lev

* Same table, but add the proportion of the subsample man=0 taking each value of sch_lev. Similarly for the subsample with man=1. 
tab man sch_lev, row



* 1.3) HISTOGRAMS

histogram age98
/* x-axis goes until 250 when most data is below 40  */
sum age98, d // 99th percentile = 32
* Get rid of the highest observations in the histogram
histogram age98 if age98<40, width(1)


histogram wage
summ wage, d
histogram wage if wage < 3000
histogram wage if wage < 3000, width(10)
histogram wage if wage < 3000, width(100)



* 1.4) TWOWAY GRAPHS

* Graph observations by wage & age
twoway scatter wage age98 if wage < 3000 & age98 < 40
	
* Fit a line: wage = beta*age + epsilon
twoway lfit wage age98 if wage < 3000 & age98 < 40

* Combine multiple twoway graphs into one
twoway (scatter wage age98 if wage < 3000 & age98 < 40) (lfit wage age98 if wage < 3000 & age98 < 40)		




**********************  PART 2 *************************


* 2.1 ) DATA CLEANING/TRIMMING

******* TAKE OUT OUTLIERS
	
**** AGE 

* Look at the distribution
summ age98, d
* Graph box plot : the box is the 25-50-75 percentiles and the whiskers are "adjacent values"
graph box age98	
* Drop outliers
drop if age98<=14 | age98>=32		



**** WAGE

summ wage, d
/* one observation with wage = 2823000 */
hist wage
* Drop this observation
drop if wage == 2823000
hist wage
/* It is expected for wage data to be skewed to the right (= to have a very long and thin right-tail). Instead of dropping all observations with a high-wage, we can work with log(wage). 
Note: We don't observe wages for individual who are not working! This means that our wage data is censored. 
When taking the log transformation, we don't want to include wages = 0. */

/* Warning: data's missing values are considered as taking a very very large value. Therefore, if I do "drop if wage > 10000" I will erase all non-employed individuals! */

* Create a dummy variable to know if the indiv is employed
generate employed = 0 // generates var employed taking value 0 everywhere
replace employed = 1 if stat == 1 // now employed = 1 for non employed people
tab employed 

* Take a log transformation for the wage: create log(wage) variable
gen log_wage = log(wage) if employed == 1
label variable log_wage "log of wage"	
* Compare wage & log(wage) histograms	
histogram wage if employed == 1
histogram log_wage if employed==1

	
******* TREAT MISSING VALUES

/* Individuals who are not employed have missing values for all variables related to employment. Employed individuals can also have missing values, if they fail to answer a question. We need to make sure that those missing values are understood as such by Stata, and not treated as a value = 99 for example. */


* Install mdesc package
help mdesc
/* mdesc produces a table with the number of missing values for each variable */

mdesc if employed==0
/* Employment variables are all missing for non-employed individuals. This is what we expected. */

mdesc if employed==1
/* The variable "tenure_unemp" is always missing for employed individuals. This is also what we expected. 

However, some employment variables are missing for employed individuals. If we want a dataset with only justifiably-missing observations, we will need to drop observations of employed individuals with missing values in employment variables. 

We see in the variable description sheet that some missing values are filled as 99. Stata does not understand 99 as a missing value, therefore, we first need to replace those 99 with a ".", which is Stata's notation for missing values. */



*** Drop observations with employed individuals & missing employment values

local ourlist country_birth location urban_area origin_father origin_mother occup_father occup_mother firm_loc firm_status firm_sector firm_size occup work_time 

/* A "foreach" loops across all the values `i' specified in a list (in this case a varlist) and performs the required action. You have to run all the command lines in the loop at the same time */  

* Replace "99" by a ".", so Stata understands it as missing
foreach i of varlist `ourlist' {			
replace `i' = . if `i' ==99 
}

* Drop observations
drop if missing(country_birth, location, urban_area, origin_father ,origin_mother, occup_father, occup_mother, firm_loc, firm_status, firm_sector, firm_size, occup, work_time) & employed==1




*** Drop observations with missing individual characteristics

* Replace 99 by missing values
local varlist country_birth location urban_area origin_father origin_mother occup_father occup_mother
foreach i of varlist `varlist' {	
replace `i'	=. if `i' == 99
}

* Drop observations
drop if missing(country_birth, location, urban_area, origin_father, origin_mother, occup_father, occup_mother)


save TP1_in, replace // This is the "input" dataset that can be used for any analysis. Should we need to make changes to this file when conducting an analysis, we should save them as a different file. 



*---------------------------------------------------------------------
* 						END OF SESSION 1							
*---------------------------------------------------------------------



**********************  PART 3 *************************



* 3.1) VARIABLE CREATION

** Gender 

tab man

/* man already takes the value 1 if man=1, and we don't want to include a dummy for man=0 in estimation because of collinearity. */


** Parents' origin

* Father born in France
gen FiF = 0
replace FiF = 1 if origin_father == 0
label var FiF "father in france"

* Mother born in France 
gen MiF = 0
replace MiF = 1 if origin_mother == 0
label var MiF "mother in france"

* Interaction btw mother's and father's birthplace 
gen FMaF = FiF * MiF
label var FMaF "mother and father in france"


** School entry age

/* We have the variable "age_jun_hs", but for readability we use a scale correction to make 0 the "normal" entry age, negative values indicate skipping classes and positive values repeating classes */
gen hab = 0
replace hab = 11 - age_jun_hs
label var hab "11 - age_jun_hs"

/* Note: this variable is continuous, not a dummy like the previous ones */


** Schooling level

* generate dummies for education levels
tab sch_lev, gen(school) 		

* label the dummies with their corresponding values 
label var school1 "No qualification"
label var school2 "Vocational High School"
label var school3 "High School"
label var school4 "Some Higher Education"
label var school5 "Two years higher education"
label var school6 "Intermediate Degree"
label var school7 "Advanced University Degree"

/* Note: we will need to drop one dummy for regressions with a constant */


** Dependent variable: the fact that an indiv is employed

/* We want a dummy for employed. This is exactly the variable "employed" created in Part 2 */


save TP1_data_out, replace // this is the output file which is tied to the analysis of employment. We can modify this file, and it is always possible to go back to the input file TP1_data_in if we are afraid we made a mistake (e.g. dropping observations without realizing)



* 3.2) LINEAR PROBABILITY

/* All our regressions will use the same baseline covariates. We can store them in a global macro to avoid rewriting them many times. */

global covariates man  FiF##MiF  hab  school2 school3 school4 school5 school6 school7

* pooled OLS (=Linear probability model)										
regress employed $covariates

* pooled OLS with heteroskedasticity-robust standard errors
regress empl $covariates, robust>
										

										
** 3.2) LOGIT ESTIMATION

* regression
logit empl $covariates	



** 3.3) MARGINAL EFFECT FOR LOGIT

* command to get interpretable derivatives	
margins, dydx(*)



* 3.4) LPM vs LOGIT: See answer sheet


* 3.5) RESIDUALS

* generate a variable for the fitted value
predict Phat										
* generate residuals
gen resid = employed-Phat
* plot residuals 
twoway scatter resid Phat // not very readable
* Pearson residuals
predict residPearson, resid
twoway scatter residPearson Phat




********* SUPPLEMENTARY MATERIAL ***********
								

** Plot residuals against variables we did not include in the regression 
twoway (scatter residPearson age98) (lfitci residPearson age98)

			
** Test different hypotheses about the coefficients 

* H0: going to vocational high school is the same as having no qualification when getting a job
ttest  school2 = 0					
/* Answer: we reject H0 at the 1% level, it seems that going vocational high school makes you more likely to be employed (very small p-value for Ha: school2>0) */			

* H0: going to vocational high school makes you just as likely to be employed as having an intermediate degree		
ttest  school2 = school6 
/* Answer: we reject H0 at the 1% level, it seems that going to vocational high school makes you more likely to get a job than getting an intermediary degree (so in the case less time in education results in higher probability to be employed!) */


** Compute manually the coefficients observed after running the command "margins, dydc(*)"
display _b[school2]
display _b[_cons] //	_b[_namevar] calls the beta coefficient of namevar. It is in log odds ratios now
display exp(_b[_cons]) //	we get the odds ratio. Odds ratio are defined in the binary case as the ratio P(Y=1)/1-P(Y=1), i.e. P(Y=1)/P(Y=0)
display exp(_b[_cons])/(1 + exp(_b[_cons])) //	we get the derivative interpretation


