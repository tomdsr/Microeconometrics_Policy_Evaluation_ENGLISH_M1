**********************************************************************
			** COMPLEMENT TO TP1: BASIC STATA COMMANDS **
**********************************************************************


*** LOAD THE DATA 

/* Always try to open your dataset on as text first. Then you can see what are the delimiters of your different cells. Typical csv comma delimited (","). Some csv file are delimited with ";". If this is the case, you can add the option "delimiters(";")" to your "import delimited" command as follows:

import delimited using "YOUR DATASET.csv", clear delimiters(";") */

use TP1_in 				// load the Stata dataset you want to work on



*** EXPLORATORY COMMANDS: browse, describe, summary, list. 

browse					// show the dataset

describe				// a quick glance at the dataset structure
describe, short			// After a command, the options go after the comma. In this case, we have a shortened version of "describe"
help describe			// since we don't know the grammar of .describe, we want to call the help file
d,s						// we can shorten both the command and the option to obtain the same result

summarize 				// quick descriptive statistics  
summarize wage			// summary of variable wage
summ wage, detail		// some more informations
bysort man: summ wage	// the "bysort" prefix splits the dataset into smaller subsets that all have the same value of the variable (here "man"). It runs the command listed after ":" (here "summarize wage") for each subset separately. Here, there will be 2 subsets: man=1 and man=0.

list numenq - urban_area in 1/10	// the hypen "-" selects all the variables in the range. The "in x/y" qualifier selects the observations from x to y

tabulate location 			// tabulate specifies frequencies for categorical variables
tab location occup_father 	// tabulate with two variables gives frequency tables



*** WILD CARDS: call several variables without writing them all

summ origin*		// "*" selects all the variables with a common part, specified before or after the *
summ *father		
summ occu?			// "?" select all the variables with an unspecified single character. Notice that occup_father and occup_mother is not selected



*** RUNNING COMMANDS ON SUBSETS OF THE DATA: "IF" 

summ wage
summ wage if wage > 1000
tab occup_father if occup_father == 1						// we specify equality in expressions with "==". In functions we use "="
tab occup_mother if occup_father != 1						// "!=" is used to express inequalities. We can use if to select over different variables
tab occup_mother if occup_father == 1 | occup_father == 2 	// "|" is the boolean (logic) operator "OR"
tab occup_mother if occup_father == 1 & wage > 1000			// "&" is the boolean (logic) operator "AND"



*** RULES FOR DATA CLEANING

/* If we have to clean a dataset, most errors are detected using: 
1 - Descriptive Statistics
2 - Histograms  
3 - Scatterplots */



*** HISTOGRAMS

histogram age98
/* x-axis goes until 250 when most data is below 40  */
sum age98, d // 99th percentile = 32
* Get rid of the highest 1% of observations in the histogram
histogram age98 if age98<32
histogram age98 if age98<32, width(1) // make bars 1-unit wide
histogram age98 if age98<32, width(1) percent // y-axis in percentage of the sample

histogram wage
summ wage, d
* Get rid of outliers
histogram wage if wage < 3000
* Width of the bars (10 units? 100 units?)
histogram wage if wage < 3000, width(10)
histogram wage if wage < 3000, width(100)
* Compare wage across survey waves
histogram wage if wage < 3000 & month==16
histogram wage if wage < 3000 & month==28
histogram wage if wage < 3000 & month==40
histogram wage if wage < 3000, by(month) // 3 graphs side-by-side

twoway (histogram wage if wage < 3000 & month==16, color(navy) legend(label(1 "1st wave"))) (histogram wage if wage < 3000 & month==28, color(orange) legend(label(2 "2nd wave"))) (histogram wage if wage < 3000 & month==40, color(gold) legend(label(3 "3rd wave"))) // overlaid histograms

/* To increase readability, we can smooth histograms into a line. This is an estimate of the density of the wage */
twoway (kdensity wage if wage < 3000 & month==16, color(navy) legend(label(1 "1st wave"))) (kdensity wage if wage < 3000 & month==28, color(orange) legend(label(2 "2nd wave"))) (kdensity wage if wage < 3000 & month==40, color(gold) legend(label(3 "3rd wave"))) 

* Export a graph
histogram age98 if age98<32, width(1) percent 
graph export graph_TP1.png, replace // "replace" option to erase and replace an existing file. Otherwise if a file with this name already exists you get an error.



*** TWO WAY GRAPHS

* Graph observations by wage & age
twoway scatter wage age98 if wage < 3000 & age98 < 40
	
* Fit a line: wage = beta*age + epsilon
twoway lfit wage age98 if wage < 3000 & age98 < 40

* Quadratic fit with confidence interval: wage = beta1*age + beta2*age^2 + epsilon
twoway qfitci wage age98 if wage < 3000 & age98 < 40

* Combine multiple twoway graphs into one
twoway (scatter wage age98 if wage < 3000 & age98 < 40) (lfit wage age98 if wage < 3000 & age98 < 40)		

* Split the data according to values of "man" and show the graphs side by side
twoway (scatter wage age98 if wage < 3000 & age98 < 40, by(man)) (lfit wage age98 if wage < 3000 & age98 < 40)				



*** GENERATE A DATASET OF SUMMARY STATISTICS: THE "COLLAPSE" COMMAND

/* The "collapse" command generates a new dataset which will replace the current one is Stata's memory. In order not to loose the dataset we are currently working with, we use "preserve ... restore". All the commands in between "preserve" and "restore" need to be run at once. Alternatively, you can type "preserve" in the command window, run your commands, and then type "restore" in the command window to go back to your original dataset. */

help collapse

* Graph the average wage by gender
preserve
collapse (mean) meanwage = wage (count) n = wage, by(man)
graph twoway bar meanwage man, ylabel(900(300)1200) xlabel(0(1)1)
restore



*** MACROS AND LOOPS

* A) Macros
/* In Stata, macros store a value or text. They are used mainly for two things:
1) to store long strings (e.g. a set of variables) that are used often in the do-file & avoid rewriting them all the time;
2) to store certain values that are returned from a command and need to be accessed later on (e.g. regression coefficients which you may want to combine in a table).
There are two types of macros: locals and globals. Locals are wiped out at the end of the current run of your code! You access a local through `localname' and a global through $globalname */
	
local firmvars firm_loc firm_status firm_sector firm_size
summ `firmvars'
global sums 2 + 2
display $sums + 3
display "$sums" + 3
	
*B) Loops
/* The simple loop is called "forvalues" and loops over all the numerical values specified. The "foreach" loop accepts an existing or new varlist, a numlist and even arbitrary lists of letters, words, and numbers separated by spaces. */
	
forvalues n = 1/10 {
	local s = `n' ^ 2
	display "`n' + `n'^2 is:"
	display `n' + `s'
}


	

