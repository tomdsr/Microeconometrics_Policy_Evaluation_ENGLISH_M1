********************************************************************************
** RD analysis: Replication of Lee (2008)
* RD software website: https://sites.google.com/site/rdpackages/
********************************************************************************
set more off
clear all

cd "C:\Users\munoza\Google Drive\TSE\Teaching\Program Evaluation\2021-2022\Lab sessions\Lab RDD"

*******************************************************************************
**# RDD REGRESSION: Polynomial Regressions
*******************************************************************************
* RDD USING COMMAND RD ROBUST (non parametric estimation, with bias correction)
********************************************************************************
*Please install the following commands before continuing this section. 
*Note that these commands are relatively new to stata, so please make sure Stata is up to date
*If not, please install updates in Stata17 before continuing. 

*You can download from online an location using the following:

*TO INSTALL RDROBUST:
net install rdrobust, from(https://raw.githubusercontent.com/rdpackages/rdrobust/master/stata) replace
** RDLOCRAND: 
net install rdlocrand, from(https://raw.githubusercontent.com/rdpackages/rdlocrand/master/stata) replace
** RDDENSITY: 
net install rddensity, from(https://raw.githubusercontent.com/rdpackages/rddensity/master/stata) replace
* ESTTAB
net install st0085_1, from(http://www.stata-journal.com/software/sj7-2/) replace
* COEFPLOT
net install gr0059, from(http://www.stata-journal.com/software/sj14-4) replace


**Open data for RDD regressions
	
use TP_rdd.dta, clear

**Creating a macro for the covariates
global covariates "demshareprev demwinprev demofficeexp othofficeexp demelectexp othelectexp"


***1. Definition of treatment
*1.1 Creating variable of incumbency equal to variable victory
*when mov is positive, then victory==1

g victory=0
replace victory=1 if mov>=0

**# 2.RD graphs

***2.1 Create graph with bins

ssc install binscatter
help binscatter

binscatter demsharenext mov, rd(0) n(60) line(qfit) ///
xtitle("Democratic Vote Share Margin of Victory, Election t") ///
ytitle("Democratic Vote Share, Election t+1")

***2.2 Graph with all data (i.e. no bins)
tw (scatter demsharenext mov, mcolor(gs10) msize(tiny)) ///
(lpolyci demsharenext mov if mov<0 & use==1,  deg(1)  fcolor(none)) ///
(lpolyci demsharenext mov if mov>0 & use==1,  deg(1)  fcolor(none)), xline(0)  ///
title("Polynomial Fit Graph, all data", color(gs0)) xtitle("Democratic Vote Share Margin of Victory, Election t") ///
ytitle("Vote Share, Election t+1")   graphregion(color(white))
				
*******************************************************************
**# 3. RD Estimates


***3.1 RD estimation: Polynomial Degree 4 (Replicates Table 2 of Paper Lee 2008, column one)

*Creating the polynomial on both sides of the discontinuity

*generating nth degree polynomial of mov variable
g mov2=mov^2
g mov3=mov^3
g mov4=mov^4
g mov5=mov^5
g mov6=mov^6

*Generating margin of victory of democrats when positive (when victorious) called rmov
*i.e. gen interaction of dummy variable of Victory and Margin of Victory (Victory*mov=rmov)
g rmov=0
replace rmov=mov*victory

*generating nth degree polynomial of rmov variable
g rmov2=rmov^2
g rmov3=rmov^3
g rmov4=rmov^4
g rmov5=rmov^5
g rmov6=rmov^6

*RD Polynomail regression w/o covariates:
program drop _all
program define fitpoly4
	regress `1' mov mov2 mov3 mov4 rmov rmov2 rmov3 rmov4 victory `2' if use==1, vce(cluster statedisdec)
end

fitpoly4 demsharenext
estimates store reg4

esttab reg4  using reg_table_4th.tex, replace ///
title("Results for polynomial 4th degree") unstack r2  ///
label eqlabels(none) se

*3.2. Local Linear Regression and plot (Non parametric estimation, polynomial order 1)
***Set up for conducting RDD estimation with rdrobust using clustered std errors

rdrobust demsharenext mov if use==1, c(0) p(1) q(0) vce(cluster statedisdec) 
eststo rd0
estimates store rd0

rdrobust demsharenext mov if use==1, c(0) p(1) q(0) vce(cluster statedisdec) covs($covariates)
eststo rd1
estimates store rd1

esttab rd0 rd1  using reg_nonparametric.tex, replace ///
title("Results for non parametric") unstack r2  ///
label eqlabels(none) se

* plotting RD: Local Linear Regression (and non parametric estimation order 1)

rdplot demsharenext mov, graph_options(title(RD Graph: Margin of victory in t vs. Vote share in t+1) xtitle("Democratic Vote Share Margin of Victory, Election t") ///
ytitle("Vote Share, Election t+1") graphregion(color(white)))  ///
p(1) kernel(tri) 

rdplot demsharenext mov, graph_options(title(RD Graph: Margin of victory in t vs. Vote share in t+1) xtitle("Democratic Vote Share Margin of Victory, Election t") ///
ytitle("Vote Share, Election t+1") graphregion(color(white)))  ///
p(1) kernel(tri) h(0.1 0.1)

rdplot demsharenext mov, graph_options(title(RD Graph: Margin of victory in t vs. Vote share in t+1) xtitle("Democratic Vote Share Margin of Victory, Election t") ///
ytitle("Vote Share, Election t+1") graphregion(color(white)))  ///
p(1) kernel(tri) h(0.3 0.4)

***3.3 RD Polynomial Regression with covariates:

reg demsharenext mov mov2 mov3 mov4 rmov rmov2 rmov3 rmov4 victory $covariates if use==1, vce(cluster statedisdec)
estimates store reg4

esttab reg4  using reg_table_4th_controls.tex, replace ///
title("Results for polynomial 4th degree with controls") unstack r2  ///
label eqlabels(none) se

*****************************************************************
**# 4. Sensitivity of RD coefficients to different polynomial degrees
***Question 4.1:  Regression with different polynomial degrees

***Polynomial degree 1
regress demsharenext victory mov rmov  if use==1, vce(cluster statedisdec)
eststo reg1
estimates store reg1

****Polynomial degree 2
regress demsharenext victory mov mov2  rmov rmov2  if use==1, vce(cluster statedisdec)
eststo reg2
estimates store reg2

****RD Polynomial degree 3
regress demsharenext victory mov mov2 mov3 rmov rmov2 rmov3  if use==1, vce(cluster statedisdec) 
eststo reg3
estimates store reg3

****RD Polynomial degree 4
fitpoly4 demsharenext
eststo reg4
estimates store reg4

***Polynomial degree 5 
regress demsharenext victory mov mov2 mov3 mov4 mov5 rmov rmov2 rmov3 rmov4 rmov5  if use==1, vce(cluster statedisdec)
eststo reg5
estimates store reg5

***Polynomial degree 6
regress demsharenext victory mov mov2 mov3 mov4 mov5 mov6 rmov rmov2 rmov3 rmov4 rmov5 rmov6  if use==1, vce(cluster statedisdec)
eststo reg6
estimates store reg6

esttab reg1 reg2 reg3 reg4 reg5 reg6  using reg_table_sens_degree.tex, replace ///
title("Results for different polynomial specifications") unstack r2  ///
label eqlabels(none) se


***4.2 Graphical representing the different RD coefficients according to sensitivity of Polynomial degree
*ssc install coefplot

coefplot(reg1, keep(victory) label("p=1")) (reg2, keep(victory)label("p=2")) (reg3, keep(victory)label("p=3")) (reg4, keep(victory) label("p=4")) ///
 (reg5, keep(victory) label("p=5")) (reg6, keep(victory)label("p=6")),  vert mlabel mlabposition(2) ///
xtitle("RD coefficients with different polynomials") graphregion(color(white))


*4.3. Non parametric reg with order 2

rdrobust demsharenext mov if use==1, c(0) p(2) q(0) vce(cluster statedisdec) covs($covariates)
eststo rd2
estimates store rd2

esttab rd2  using reg_nonparametric.tex, replace ///
title("Results for non parametric") unstack r2  ///
label eqlabels(none) se

* plotting RD: Local Linear Regression (non parametric estimation order 2)

rdplot demsharenext mov, graph_options(title(RD Graph: Margin of victory in t vs. Vote share in t+1) xtitle("Democratic Vote Share Margin of Victory, Election t") ///
ytitle("Vote Share, Election t+1") graphregion(color(white))) ///
p(2) kernel(tri) 




*****************************************************************************
**# 5. Validation: effect on covariates and placebo outcomes (taking as reference Reg with polynomial degree 4)
******************************************************************************
* 5.1. Effect on covariates 
foreach x of global covariates {
	display as error "Estimating effect for covariate `x'"
	fitpoly4 `x'
	eststo reg_`x'
estimates store reg_`x'
}

esttab reg_demshareprev reg_demwinprev reg_demofficeexp reg_othofficeexp reg_demelectexp reg_othelectexp using reg_validation_controls.tex, replace ///
title("Results for validation of controls") unstack r2  label eqlabels(none) keep( victory) se


*Note: None of the victory coefficients in each of these regressions should be significant


*5.2. Validation: effect on covariates and placebo outcomes using local linear regression
* Effect on covariates and placebo outcomes
foreach x of global covariates {
	display as error "Estimating effect for covariate `x'"
	rdrobust `x' mov, c(0) p(1) 
	eststo reg_`x'
estimates store reg_`x'
}

esttab reg_demshareprev reg_demwinprev reg_demofficeexp reg_othofficeexp reg_demelectexp reg_othelectexp using rd_validation_controls.tex, replace ///
title("Results for validation of controls in RD") unstack r2  label eqlabels(none) se


*************************
* 5.3 Placebo cutoffs

***Generate different 

*First, let's generate the pseudo-victory defined at cutoff of +0.25
g victory25=0
replace victory25=1 if mov>=0.25

*Generating margin of victory of democrats when positive (when victorious) called rmov
*i.e. gen interaction of dummy variable of Victory and Margin of Victory (Victory*mov=rmov)
g rmov25=0
replace rmov25=mov*victory25

*generating nth degree polynomial of rmov25 variable
g rmov25_2=rmov25^2
g rmov25_3=rmov25^3
g rmov25_4=rmov25^4

**Polynomial Regression with cutoff defined at +0.25
regress demsharenext mov mov2 mov3 mov4 rmov25 rmov25_2 rmov25_3 rmov25_4 victory25 if use==1, vce(cluster statedisdec)
eststo reg25
estimates store reg25



**Now, let's do it for the pseudo-victory defined at cutoff +0.15
g victory15=0
replace victory15=1 if mov>=0.15

***Generating interaction between victory defined at these placebo cutoffs 
*and margin of victory variable
g rmov15=0
replace rmov15=mov*victory15

***Generating polynomial to the right of the placebo cuttoffs
g rmov15_2=rmov15^2
g rmov15_3=rmov15^3
g rmov15_4=rmov15^4

***Polynomial Regression with cutoff defined at 0.15
regress demsharenext mov mov2 mov3 mov4 rmov15 rmov15_2 rmov15_3 rmov15_4 victory15 if use==1, vce(cluster statedisdec)
eststo reg15
estimates store reg15

esttab reg25 reg15  using reg_placebo.tex, replace ///
title("Results for placebo estimations") unstack r2  ///
label eqlabels(none) keep(victory15 victory25) se


***5.4 Plotting RD estimates with these placebo cutoffs
*****************************************
***RD graph with placebo cutoff +0.25
binscatter demsharenext mov, rd(0.25) n(60) line(qfit) ///
xtitle("Democratic Vote Share Margin of Victory, Election t") ///
ytitle("Democratic Vote Share, Election t+1") graphregion(color(white))

tw (scatter demsharenext mov, mcolor(gs10) msize(tiny)) ///
(lpolyci demsharenext mov if mov<0.25 & use==1, deg(1)  fcolor(none)) ///
(lpolyci demsharenext mov if mov>0.25 & use==1, deg(1)  fcolor(none)), xline(0.25)  ///
title("Placebo cutoff 0.25", color(gs0)) xtitle("Democratic Vote Share Margin of Victory, Election t") ///
ytitle("Vote Share, Election t+1")  graphregion(color(white))

***RD graph placebo cutoff +0.15
binscatter demsharenext mov, rd(0.15) n(60) line(qfit) ///
xtitle("Democratic Vote Share Margin of Victory, Election t") ///
ytitle("Democratic Vote Share, Election t+1") graphregion(color(white))

tw (scatter demsharenext mov, mcolor(gs10) msize(tiny)) ///
(lpolyci demsharenext mov if mov<0.15 & use==1, deg(1)  fcolor(none)) ///
(lpolyci demsharenext mov if mov>0.15 & use==1, deg(1)  fcolor(none)), xline(0.15)  ///
title("Placebo cutoff 0.15", color(gs0)) xtitle("Democratic Vote Share Margin of Victory, Election t") ///
ytitle("Vote Share, Election t+1")  graphregion(color(white))


*5.5. Placebo outcomes using local linear regression

rdrobust   demsharenext mov if use==1, c(0.25) p(1) vce(cluster statedisdec)
eststo rd25
estimates store rd25

rdplot demsharenext mov, graph_options(title(RD Graph: Margin of victory in t vs. Vote share in t+1) xtitle("Democratic Vote Share Margin of Victory, Election t") ///
ytitle("Vote Share, Election t+1") graphregion(color(white))) ///
p(1) kernel(tri) c(0.25)


rdrobust   demsharenext mov if use==1, c(0.15) p(1) vce(cluster statedisdec)

eststo rd15
estimates store rd15

esttab rd15 rd25  using reg_nonparametric.tex, replace title("Results for non parametric placebo") unstack r2  label eqlabels(none) se

rdplot demsharenext mov, graph_options(title(RD Graph: Margin of victory in t vs. Vote share in t+1) xtitle("Democratic Vote Share Margin of Victory, Election t") ///
ytitle("Vote Share, Election t+1") graphregion(color(white))) ///
p(1) kernel(tri) c(0.15)

*5.6. Sensitivity to Bandwidth and polynomial order (non parametric estimation)

***Manually changing the bandwidth
quietly: rdrobust demsharenext mov if use==1, c(0) p(1) h(0.05) vce(cluster statedisdec) 
estimates store h_005

quietly: rdrobust demsharenext mov if use==1, c(0) p(1) h(0.1) vce(cluster statedisdec) 
estimates store h_01

quietly: rdrobust demsharenext mov if use==1, c(0) p(1) h(0.2) vce(cluster statedisdec) 
estimates store h_02

quietly:rdrobust demsharenext mov if use==1, c(0) p(1) h(0.3) vce(cluster statedisdec) 
estimates store h_03

quietly: rdrobust demsharenext mov if use==1, c(0) p(1) h(0.4) vce(cluster statedisdec) 
estimates store h_04

quietly: rdrobust demsharenext mov if use==1, c(0) p(1) h(0.5) vce(cluster statedisdec) 
estimates store h_05

quietly: rdrobust demsharenext mov if use==1, c(0) p(1) h(0.6) vce(cluster statedisdec) 
estimates store h_06

quietly: rdrobust demsharenext mov if use==1, c(0) p(1) h(1) vce(cluster statedisdec) 
estimates store h_1

 
  ***Graphical representation to sensitivity to Bandwidth
coefplot (h_005, label("h=0.05")) (h_01,label("h=0.1")) (h_02, label("h=0.2")) (h_03,label("h=0.3")) ///
 (h_04, label("h=0.4")) (h_05,label("h=0.5")) (h_06, label("h=0.6")) (h_1, label("h=1")) ///
 , vert mlabel mlabposition(2) ///
xtitle("RD coefficient with different bandwidth values") graphregion(color(white))


*****Sensitivity to degree of Polynomial 
quietly: rdrobust demsharenext mov if use==1, c(0) p(1) vce(cluster statedisdec) 
estimates store p_1

quietly: rdrobust demsharenext mov if use==1, c(0) p(2) vce(cluster statedisdec) 
estimates store p_2

quietly: rdrobust demsharenext mov if use==1, c(0) p(3) vce(cluster statedisdec) 
estimates store p_3

quietly:rdrobust demsharenext mov if use==1, c(0) p(4) vce(cluster statedisdec) 
estimates store p_4

quietly: rdrobust demsharenext mov if use==1, c(0) p(5) vce(cluster statedisdec) 
estimates store p_5

quietly: rdrobust demsharenext mov if use==1, c(0) p(6) vce(cluster statedisdec) 
estimates store p_6


***Graphical representation to sensitivity to polynomial degree
coefplot (p_1, label("p=1")) (p_2,label("p=2")) (p_3, label("p=3")) (p_4,label("p=4")) ///
 (p_5, label("p=5")) (p_6,label("p=6"))   ///
, vert mlabel mlabposition(2) ///
xtitle("RD coefficient with different polynomials") graphregion(color(white))


******************************************************************************************

**# Appendix: Motivation RDD using Individual House representatives data and Democratic Group data
*Idea: What is the likelihood of winning next election given that dem is incumbent?

**** 0.1 Figure 2 (a and b) of the Lee (2008) paper
*Individual democratic candidate data

use individ_final.dta, clear

graph twoway (scatter mmyoutcomenext difshare, ms(O) mc(gs0) msize(small)) (line mpmyoutcomenext difshare, sort lcolor(gs0)) ///
				if difshare>-0.251 & difshare<0.251 & use==1, xline(0, lcolor(gs0)) title("Figure 2a", color(gs0)) xtitle("Democratic Vote Share Margin of Victory, Election t") ///
				ytitle("Probability of Winning, Election t+1") yscale(r(0 1)) ylabel(0(.1)1) xscale(r(-0.25 0.25)) xlabel(-0.25(.05)0.25) ///
				legend(label(1 "Local Average") label(2 "Logit Fit"))  graphregion(color(white))
*Note: 2.(a) Candidate's probability of winning election t þ 1, by margin of victory in election t: local averages and parametric fit

/*Comment: this replicates figure 2.a of Lee's paper: Fig. 2a illustrates the RD estimate of the incumbency advantage. 
It plots the estimated probability of a 
Democrat both running in and winning election t+ 1 as a function of the Democratic vote share margin of
victory in election t. The horizontal axis measures the Democratic vote share minus the vote share of theDemocrats' strongest opponent 
(virtually always a Republican). Each point is an average of the indicator
variable for running and winning election t+1 for each interval, which is 0.005 wide. To the left of the
dashed vertical line, the Democratic candidate lost election t; to the right, the Democrat won.
As apparent from the figure, there is a striking discontinuous jump, right at the 0 point. Democrats who
barely win an election are much more likely to run for office and succeed in the next election, compared to
Democrats who barely lose. The causal effect is enormous: about 0.45 in probability. Nowhere else is a jump
apparent, as there is a well-behaved, smooth relationship between the two variables, except at the threshold
that determines victory or defeat.*/



graph twoway (scatter mofficeexp difshare, ms(O) mc(gs0) msize(small)) (line mpofficeexp difshare, sort lcolor(gs0)) ///
				if difshare>-0.251 & difshare<0.251 & use==1, xline(0, lcolor(gs0)) ///
				title("Figure 2b", color(gs0)) xtitle("Democratic Vote Share Margin of Victory, Election t") ytitle("No. of Past Victories as of Election t") ///
				yscale(r(0 5)) ylabel(0(.5)5) xscale(r(-0.25 0.25)) xlabel(-0.25(.05)0.25) ///
				legend(label(1 "Local Average") label(2 "Polynomial Fit")) graphregion(color(white))
*Note: 2.(b) Candidate's accumulated number of past election victories, by margin of victory in election t: local averages and parametric fit.				

			


**0.2  Replicating Figures 4 (a and b) of Lee (2008) paper using Democratic party data

use group_final, clear

graph twoway (scatter mdemsharenext difdemshare, ms(O) mc(gs0) msize(small)) (line mpdemsharenext difdemshare, sort lcolor(gs0)) if ///
				difdemshare>-0.251 & difdemshare<0.251 & use==1, ///
				xline(0, lcolor(gs0)) title("Figure 4a", color(gs0)) xtitle("Democratic Vote Share Margin of Victory, Election t") ///
				ytitle("Vote Share, Election t+1") ///
				yscale(r(0.30 0.70)) ylabel(0.3(.05)0.7) xscale(r(-0.25 0.25)) xlabel(-0.25(.05)0.25) ///
				legend(label(1 "Local Average") label(2 "Polynomial Fit"))	graphregion(color(white))
*Note: 4.(a) Democrat party's vote share in election t+1, by margin of victory in election t: local averages and parametric fit.

/*In both figures (2a and 4a), there is a positive relationship between the margin of victory and the electoral outcome.
For example, as in Fig. 4a, the Democratic vote shares in election t and t þ 1 are positively correlated, both on
the left and right side of the figure. This indicates selection bias; a simple comparison of means of Democratic
winners and losers would yield biased measures of the incumbency advantage. Note also that Fig 2a exhibits important non-linearities: 
hence a linear regression specification would hence lead to misleading
inferences.*/


graph twoway (scatter mdemshareprev difdemshare, ms(O) mc(gs0) msize(small)) (line mpdemshareprev difdemshare, sort lcolor(gs0)) ///
				if difdemshare>-0.251 & difdemshare<0.251 & use==1, ///
				xline(0, lcolor(gs0)) title("Figure 4b", color(gs0)) xtitle("Democratic Vote Share Margin of Victory, Election t") ///
				ytitle("Vote Share, Election t-1") ///
				yscale(r(0.30 0.70)) ylabel(0.3(.05)0.7) xscale(r(-0.25 0.25)) xlabel(-0.25(.05)0.25) ///
				legend(label(1 "Local Average") label(2 "Polynomial Fit"))  graphregion(color(white))

*Note2: 4.(b)Democratic party vote share in election t-1, by margin of victory in election t: local averages and parametric fit.
 
*Comment:

/*
The main proposition (prop 3) of Lee's paper is that in the limit [of the threshold], there is
randomized variation in treatment status.
Figs. 2b and 4b corroborate this finding: that all pre-determined
characteristics are balanced in a neighborhood of the discontinuity threshold (implication (c) of prop 3). */





