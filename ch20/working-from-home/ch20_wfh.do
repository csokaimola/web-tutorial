********************************************************************************
* Chapter 20
*
* work-from-home
* v1.0

* using
*		Bloom et al. (2015): Does Working from Home Work? 
*			Evidence from a Chineses Experiment. QJE. 165-218
*
**********************************************************************


* set the path
*cd "C:\Users\GB\Dropbox (MTA KRTK)\bekes_kezdi_textbook"
cd "D:\Dropbox (MTA KRTK)\bekes_kezdi_textbook\"
*location folders
global data_in   "cases_studies_public/working-from-home/clean"
global data_out  "textbook_work/ch20/working-from-home"
global output   "$data_out/output"

* load in clean and tidy data and create workfile
**********************************************************************
* WORKFILE
* essentially same as wfh_tidy_person 
* plus ordering variables


use "$data_in/wfh_tidy_person.dta",replace

order personid-perform11  age male second high terti univ prior tenure married children ///
 ageyoungest rental costofcomm internet bedroom base bonus gross

 
*******************************************************
* Balance


*des perform10 age-grosswage

replace ageyoungest = . if children==0

* we need a table with 4 columns: [c1]=mean t=1, [c2]= mean t=0, [c3] sd, [c4] p-value of the t-test.
* !!!!TODO: make this into a single, nice looking table

tabstat perform10 age-grosswage ordertaker  if treatment==1, c(s)
tabstat perform10 age-grosswage ordertaker  if treatment==0, c(s)
tabstat perform10 age-grosswage ordertaker, s(sd) c(s)


* t-tests for equal means (we do them by regression for simplicity) - 
* need to enter p-values one by one to LaTex or Excel table
foreach z of varlist perform10 age-grosswage ordertaker {
	reg treatment `z', robust nohead
}



*******************************************************
* outcomes: 
* quit firm during 8 months of experiment
* # phone calls worked, for order takers

des quit phonecalls1


tabstat quit , by(treatment) s(mean sd n)
tabstat phonecalls1 if ordertaker==1, by(treatment) s(mean sd n)
* !!!!TODO:  - save extrenally

* Regression 1: ATE estimates, no covariates

reg quit treatment , robust
 outreg2 using "$output/Ch20_wfh_reg1", bdec(2) sdec(3) 2aster tex replace

reg phonecalls1 treatment if ordertaker==1, robust
 outreg2 using "$output/Ch20_wfh_reg1", bdec(1) sdec(2) 2aster tex append
 

* Regression 2: ATE estimates, with covariates of some unbalance

reg quit treatment married children internet, robust
 outreg2 using "$output/Ch20_wfh_reg2", bdec(2) sdec(3) 2aster tex replace

reg phonecalls1 treatment married children internet if ordertaker==1, robust
 outreg2 using "$output/Ch20_wfh_reg2", bdec(1) sdec(2) 2aster tex append
 
 
