/*

						Program evaluation - TP3/4
	Selection on observables, non-parametrics and propensity score matching	
		
*/

clear all 
set more off, permanently
gl path "/Users/~/Documents/"

cd "$path"

use "$path/data_farms_stud", clear

/* Part I */

*Q1
	reg fertilizer_2020 d
	test (d==-10)
	reg fertilizer_2020 d if eligible
	test (d==-10)

* Q2
	ttest age if eligible, by(d)
	ttest size if eligible, by(d)
	ttest machines if eligible, by(d)
	ttest chamber if eligible, by(d)
	ttest fertilizer_2018 if eligible, by(d)
	ttest protected if eligible, by(d)
	ttest label if eligible, by(d)
	
* Q3
	reg fertilizer_2020 d age size machines chamber fertilizer_2018 protected label if eligible
	test (d==-10)
	
* Q4
 // b.
	logit d age size machines chamber fertilizer_2018 protected label if eligible
	predict PS1 if eligible
	
 // c.
	bys d: sum PS1 if eligible, d
	sum PS1 if eligible & !d
	count if (PS1<`r(min)' | PS1>`r(max)') & d
	
	hist PS1 if eligible, by(d)
	
	sum PS1 if eligible & !d
	gen common_s=(PS1>=`r(min)' & PS1<=`r(max)') 
	logit d age size machines chamber fertilizer_2018 protected label if common_s // Only drop 5 observations so the estimates are very similar.
	logit d age size machines chamber fertilizer_2018 protected label if eligible
	
 // d.
	reg fertilizer_2020 d PS1 if eligible
	test (d==-10)

	

/* Part 2 */

* Q1
	reg carrots_2018 fertilizer_2018 
	predict carrots_hat1
	
* Q2
	twoway scatter carrots_hat1 fertilizer_2018 
	
* Q4/5
	npregress kernel carrots_2018 fertilizer_2018
	predict carrots_hat2
	twoway scatter carrots_hat2 fertilizer_2018

* Q6
	reg carrots_2018 i.region size machines i.seed fertilizer_2018 age chamber protected label // 'Age chamber protected label' are not significant.
	reg carrots_2018 i.region size machines i.seed fertilizer_2018
	predict carrots_hat1_cov
	
	npregress kernel carrots_2018 i.region size machines i.seed fertilizer_2018
	predict carrots_hat2_cov
	twoway (scatter carrots_hat2_cov fertilizer_2018, msize(tiny) mcolor(navy%30)) (mspline carrots_hat2_cov fertilizer_2018), graphregion(color(white))
	
	reg carrots_2018 fertilizer_2018 i.seed size
	npregress kernel carrots_2018 fertilizer_2018 i.seed size // Much faster to run
	predict carrots_hat3_cov
	twoway (scatter carrots_hat3_cov fertilizer_2018, msize(tiny) mcolor(navy%30)) (mspline carrots_hat3_cov fertilizer_2018), graphregion(color(white))


/* Part 3 */

* Q1
	capture drop PS1 // Capture executes the command without providing its output, including error messages. In this case, if PS1 was created before (line 40), it is dropped by this line. If you did not run part 1 such that PS1 does not exist, the error is ignored by stata. This command is convenient but should be used with caution.
	logit d age size machines chamber fertilizer_2018 protected label if eligible
	predict PS1 if eligible
	hist PS1 if eligible, by(d)
	
	capture drop PS_np
	npregress kernel d age size machines i.chamber fertilizer_2018 i.protected i.label if eligible
	predict PS_np if eligible
	sum PS_np if eligible & !d
	count if (PS_np<`r(min)' | PS_np>`r(max)') & d
	hist PS_np if eligible, by(d)
	
	reg fertilizer_2020 d PS_np if eligible
	
	
* Q2: Implementing PSM
	psmatch2 d if eligible, outcome(fertilizer_2020) pscore(PS1) common // Using parametric propensity score.
	psmatch2 d age size machines i.chamber fertilizer_2018 i.protected i.label if eligible, outcome(fertilizer_2020) common logit // Alternative to pre-estimated parametric PS. Gives the same result as the previous row.
	
	psmatch2 d if eligible, outcome(fertilizer_2020) pscore(PS_np) common // Using non-parametric propensity score.
	

* Q3: Balancing test
	psmatch2 d if eligible, outcome(fertilizer_2020) pscore(PS1) common
	pstest age size machines i.chamber fertilizer_2018 i.protected i.label
	
	psmatch2 d if eligible, outcome(fertilizer_2020) pscore(PS_np) common
	pstest age size machines chamber fertilizer_2018 protected label

exit


