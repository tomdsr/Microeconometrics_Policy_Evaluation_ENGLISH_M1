clear all

* Change the following
global dir = "C:\Users\Username\Stata"
************************
*set directory and load data
************************
cd "$dir"
use nsw.dta
*use nsw_oldstata13_1

************************
*q4
************************

	*1) how many people... 
	count 
	
	*2) what is the average... 
	mean age
	mean education 
	
	*3) what is the proportion... 
	mean treat 
	
	//obs: alternatively, use command summarize, which also gives the mean 

	//obs2: other commands to explore the data 
	describe
	summarize age education treat black hispanic married
	tab1 education black hispanic married

************************
*q5
************************

 ttest age, by(treat) // age difference not significant
 ttest education, by(treat) // education level: reject H0 that in one sided test control is = treated.  We conclude treated is bigger than control!
 ttest black, by(treat) // not significant
 ttest hispanic, by(treat) // not significant
 ttest married, by(treat) // not significant
 ttest re75, by(treat) // not significant

 * Since we randomized, our groups are on average equal in all variables exept education
 * Education: on 10% sign. level mean(treated) > mean(control), but only 0.2 years. Very small!
 
  ttest re78, by(treat) // outcome variable has a significant difference (this is actually the av. treatment effect)

************************
*q6
************************

*Plot histograms separately 
 hist re78 if treat == 1
 hist re78 if treat == 0
 
*Plot histograms together 
twoway (hist re78 if treat == 1) (hist re78 if treat == 0, col(green)), legend(order(1 "Treatment" 2 "Control"))
 
*Plot kdensity 
twoway (kdensity re78  if treat == 1, lwidth(thick)) (kdensity re78 if treat == 0, lwidth(thick) col(green)),  legend(order(1 "Treated" 2 "Control")) graphregion(color(white)) xtitle("re78")
 
graph export "$dir\kdensity.pdf", replace	

* kdensity: estimates non parametrically the density (kernel density) of re78
* The mean of the treatment group is slightly higher than the mean of the control group (more to the right on the axis)
* However we also have more individuals in the control group which you can see
* Note we cannot identify the distribution of the treatment effects 
* By simply looking at the distribution of the outcome variable!

************************
*q7
************************

ttest re78, by(treat)  

graph bar (mean) re78, over(treat) graphregion(color(white)) // creates a bar plot with two bars: mean of treated and mean of control 
graph export "$dir\outcomebars.pdf", replace	

*To compute the relative effect as a percentage of the mean outcome of the control: 
	*Store the mean of treatment
quietly: summ re78 if treat == 1
scalar mean_treatment =  r(mean)

	*Store mean of control
quietly: summ re78 if treat == 0
scalar mean_control =  r(mean)

	*Store treatment effect 
scalar ATE = mean_treatment - mean_control

	*Compute the treatment effect as share of mean of control
	disp ATE/mean_control
	
* The average causal effect of NSW on earnings (ATE) is around 886 USD per year per person (mean of control - mean of treated)
* The treated group has significantly more earnings than the control group with a p-value of 0.03.
* the relative difference is 17.72%

************************
*q8
************************ 
ttest re78, by(treat)  
reg re78 treat

* Effect is significant at the 10% level, not at the 5% level

************************
*q9
************************ 
 
 reg re78 treat if black == 1
 reg re78 treat if hispanic == 1
 reg re78 treat if black == 0 & hispanic == 0 // if not black or hispanic then white

 * Most effective among African-Americans (only one significant).
 * Small negative for the white participants and larger negative for the hispanic participants but both are insignificant!
  
 * there exists a variable ethnic taking the values: Hispanic, Afro-Ame, White
 graph bar (mean) re78, over(treat) over(ethnic) graphregion(color(white))
 graph export "$dir\heterogeneity.pdf", replace	

 * We see that there is the biggest difference for the Afro-Americans!

************************
*q10
************************  

*Generate variable that encodes groups of education levels
 gen educ_group = 0
 replace educ_group = 1 if education <=8 
 *corresponds to completing junior high shcool ("college" in France)
 
 replace educ_group = 2 if education <=12 & education>8
 *corresponds to completing high school
 * Includes high-school dropouts
  
 replace educ_group = 3 if education >12
 *correspond to higher education

reg re78 treat if educ_group == 1
reg re78 treat if educ_group == 2
reg re78 treat if educ_group == 3

 graph bar (mean) re78, over(treat) over(educ_group) graphregion(color(white))
  graph export "$dir\heterogeneityeduc.pdf", replace	

* ATE by education category non significant except the educ_group = 2 which includes high-school drop-outs (almost the same as for entire population). 
  
************************
*q11
************************   

display (log(1-0.05*(9000/abs(ATE)))/log(0.95) ) - 1

 * The program becomes profitable in a time horizon of 13 years
 
 * Note it does not hold if the program has dynamic effects (increase in income becomes larger or fades with time). 
 * We only identify the causal effect of the treatment in the first three employment years after the JTP and
 * assumed this increase is constant for future employment years)
 
************************
*q12
************************  

reg re78 treat age education black hispanic married
*scalar diff_se_c = _b[treat]/_se[treat]
*display diff_se_c

reg re78 treat
*scalar diff_se = _b[treat]/_se[treat]
*display diff_se

ssc install outreg2 //If you don't have it installed

reg re78 treat

outreg2 using "Adding_controls.xls", excel replace

reg re78 treat age

outreg2 using "Adding_controls.xls", excel append

reg re78 treat age black

outreg2 using "Adding_controls.xls", excel append

reg re78 treat age black hispanic

outreg2 using "Adding_controls.xls", excel append

reg re78 treat age black hispanic married

outreg2 using "Adding_controls.xls", excel append

reg re78 treat age black hispanic married education

outreg2 using "Adding_controls.xls", excel append



* Adding controls does change much (820 vs 886 USD) and significant at 10%.

* Higher precision of estimation! Smaller effect in terms of standard deviation!

* Treatment and controls:
reg treat age education black hispanic married

* F test for joint significance 
test (age = 0)(education = 0) (black = 0) (hispanic = 0) (married = 0)
* This is normal if randomization is successfull!



************************
*q13 Power Analysis
************************  summ re75 
power onemean 3042.897, sd(5066.143) diff(500 800 1000) power(0.9) // estimate sample size needed given effect size(s) and power
power onemean 3042.897, sd(5066.143) diff(500 800 1000) n(500)  // estimate power needed given effect size(s) and  sample size
power onemean 3042.897, sd(5066.143) power(0.9) n(2000) table(alpha power N diff) // estimate effect size needed given power and  sample size

* we can also graph the same results
power onemean 3042.897, sd(5066.143) diff(500 800 1000) n(300(50)800) graph



