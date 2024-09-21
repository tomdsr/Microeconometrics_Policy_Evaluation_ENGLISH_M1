* Estimation by diff in diff 

* cd 
cd "/Users/lisabotbol/Dropbox_old/Dropbox/TD donnés/PE 2020-2021/TP6"

use base_2017, replace

 
 ***** Part I *****
 
 ** Creation of variables
 gen d = ez*(per > 7)  /* Treatment: employment zone for periods after the 7th one */
 tab ez if per == 8
 gen prob_exit = esem/stock  /*Number of exits to a job from unemployment/Number of unemployed*/
 gen l_prob_exit = log(prob_exit) /* Outcome */
 
 quietly tab per, gen(time) /*Generate dummies for each period */

* Weighting by population
 quietly sum pop90 [aweight=pop90] 
 gen w = pop90*r(N)/r(sum_w) /* create the weight: municipality population * number of municipalities/total population  */
 sum w
  
 *** Question 3
* raw diff in diffs 
 reg l_prob_exit d ez time2-time20
 reg prob_exit d ez time2-time20
 reg l_prob_exit d ez time2-time20 [iweight = w]
 
  ***** Part II *****

*** Question 1
		
 probit d rvp_all60 tx_nodip tx_tech tx_univ dmin ///
              tx25 pop90 pfi_1996 [iweight = w] if per == 8
 predict dp, p /* predicts for all sample, not just per 8 */

* Support condition
 bysort ez: sum dp if per==8, det
 
 
 *** Question 2
 global dpmin = r(min)/2 /*half of the min value of PS for treated*/
 /* In the remaining of the TP we will run regressions on the subsample
 such that dp > dpmin */

  ***** Part III *****

 *** Question 1
 * Diff in diff
 reg l_prob_exit d ez time2-time20 if dp >= $dpmin [aweight = w]
 reg l_prob_exit d ez dp time2-time20 if dp >= $dpmin [aweight = w]
 
 *** Question 2
 * Panel analysis:
 tsset dc per
 //xtset dc per /* Sets the data as a panel where "dc" is the panel id variable and "per" is the time variable. Useful to generate lagged variables */

 gen dlp = D.l_prob_exit /* "D.var" generates a difference for the variables */
 gen dd = D.d /* = 1 iff first year of ez */
 
 
 * Regress the first difference equation 
 /* Note: we need to get rid of an extra time dummy since time1  is always zero (given that dlp  starts at t=2 given it's a first difference). So to avoid collinearity we get rid of time3. 
 We also need autocorrelation-robust SE */

 reg dlp dd time3-time20 if dp > $dpmin [aweight=w], vce(cluster dc)
  
 *** Question 3
 reg dlp dd if dp > $dpmin & per == 8 [aweight=w],  vce(cluster dc)
 
  
  ***** Part IV *****
  
  *** Question 1
  * Graph the average outcome in each group before treatment and compare trends
  bysort ez per: egen average_probexit = mean(l_prob_exit)
  twoway (line  average_probexit per if ez == 0) (line  average_probexit per if ez == 1), xline(8)   legend(order(1 "Control" 2 "Treatment"))
  
  * Repeat the operation with the reduced control (such that dp>dpmin)
  bysort per: egen average_probexit_reduced = mean(l_prob_exit) if ez==0 & dp>$dpmin
  twoway (line  average_probexit per if ez == 0) (line  average_probexit per if ez == 1) (line  average_probexit_reduced per if ez == 0) , xline(8)   legend(order(1 "Control" 2 "Treatment" 3 "Reduced Control"))
  
  * Add placebo variables pretending treatment happened before, or after 
  gen d2 = ez*(per >= 2)
  gen d3 = ez*(per >= 3)
  gen d4 = ez*(per >= 4)
  gen d5 = ez*(per >= 5)
  gen d6 = ez*(per >= 6)
  gen d7 = ez*(per >= 7)
  gen d9 = ez*(per >= 9)
  gen d10 = ez*(per >= 10)
  
  // Raw differences
  reg l_prob_exit d d2 d3 d4 d5 d6 d7 d9 d10 ez i.per if dp >= $dpmin [aweight = w]
  
  tsset dc per /* setting the panel structure */
  gen dd2 = D.d2
  gen dd3 = D.d3
  gen dd4 = D.d4
  gen dd5 = D.d5
  gen dd6 = D.d6
  gen dd7 = D.d7
  gen dd9 = D.d9
  gen dd10 = D.d10
  
  // Allowing for unobserved heterogeneity
  reg dlp dd dd3 dd4 dd5 dd6 dd7 dd9 dd10 ez i.per if dp >= $dpmin [aweight = w], vce(cluster dc)
  
  
  *** Question 2

 * Short term/ long term
 gen d2lp = l_prob_exit - L2.l_prob_exit  /* "L2.var" generates a 2 period lag variable */
 gen d2d = d -L2.d
 reg d2lp d2d time4-time20 if dp > = $dpmin [aweight=w]

  
 gen d6lp = l_prob_exit - L6.l_prob_exit  
 gen d6d = d -L6.d
 reg d6lp d6d time8-time20 if dp > = $dpmin [aweight=w],  vce(cluster dc)

 
 *** Question 3
 * Robust to alternative definitions of control
 gen cont5 = ((ez==0 & dmin > 5) | ez == 1)  
 gen cont10 = ((ez==0 & dmin > 10) | ez == 1)
 
 reg dlp dd if dp > $dpmin & cont5 == 1 & per==8 [aweight=w] ,  vce(cluster dc)
 reg dlp dd if dp > $dpmin & cont10 == 1 & per==8 [aweight=w] ,  vce(cluster dc)
 

 







