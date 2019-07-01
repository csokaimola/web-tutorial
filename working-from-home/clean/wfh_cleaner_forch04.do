********************************************************************************
* Chapter 04
*
* work-from-home
* v1.0

* using
*		Bloom et al. (2015): Does Working from Home Work? 
*			Evidence from a Chineses Experiment. QJE. 165-218
*
********************************************************************************

**********************************************
* Cleaning file
* 
**********************************************


* set the path
cd "C:\Users\GB\Dropbox (MTA KRTK)\bekes_kezdi_textbook"

*location folders
global data_in   "cases_studies_public/working-from-home/raw"
global data_in   "cases_studies_public/working-from-home/clean"

* load in clean and tidy data and create workfile
use "$data_in/performance_during_exper.dta", clear

gen experiment=0
replace experiment=1 if year_week>=201050

*keep only treatment and control group people
keep if (expgroup==1 | expgroup==0)

sort personid year_week

*tag order takers who ever have phonecall data (reduces size of data set)
rename logcallpersec logcallpermin
bysort personid: egen _hasphonecall=min(logphonecall)
bysort personid: egen _hasphonecall2=min(logcallpermin)
gen ordertaker=0 
replace ordertaker=1 if _hasphonecall!=. & _hasphonecall2!=.

replace phonecall=round(exp(logphonecall))
gen callpermin=exp(logcallpermin)
gen calllength=round(exp(logcalllength))
gen daysworked=exp(logdaysworked)



*correct unbalance across main outcomes
tabmiss callpermin if phonecall!=.
tabmiss calllength if phonecall!=.
tabmiss phonecall if callpermin!=.
tabmiss phonecall if calllength!=.
replace phonecall=. if callpermin==. & phonecall!=.
replace callpermin=. if phonecall==. & callpermin!=.
replace calllength=. if phonecall==. & calllength!=.
tabmiss callpermin if phonecall!=.
tabmiss calllength if phonecall!=.
tabmiss phonecall if callpermin!=.
tabmiss phonecall if calllength!=.

*keep main variables
keep personid ordertaker year_week treatment experiment experiment_treatment ///
	phonecall callpermin calllength perform1 daysworked ///
	logphonecall logcallpermin logcalllength
order personid ordertaker year_week treatment experiment experiment_treatment ///
	phonecall callpermin calllength perform1 daysworked ///
	logphonecall logcallpermin logcalllength
sort treatment personid year_week

********************************************************************************
* CREATE ADDITIONAL VARIABLES FOR ATTRITION ANALYSIS
********************************************************************************	

*times
codebook year_week

tabmiss daysworked
bysort personid: egen _lastworked=max(year_week) 
tabmiss _lastworked

tabmiss phonecall
bysort personid: egen __lastphone=max(year_week) if phonecall!=.
tabmiss __lastphone
bysort personid: egen _lastphone=min(__lastphone)

gen worked_lastmonth=0
gen phone_lastmonth=0
replace worked_lastmonth=1 if _lastworked>=201130 & _lastworked!=.
replace phone_lastmonth=1 if _lastphone>=201130 & _lastphone!=.

drop _*

********************************************************************************
* AGGREGATING PERSON-WEEK DATA TO PERSON LEVEL DATA WITH 2 TIME PER, BEFORE & AFTER
********************************************************************************	

collapse (sum) phonecall calllength ///
		(first) treatment experiment_treatment ordertaker ///
		worked_lastmonth phone_lastmonth, ///
		by(personid experiment)
replace phonecall=. if phonecall==0
replace calllength=. if calllength==0

replace phonecall = phonecall/1000
replace calllength = calllength/60
replace calllength = calllength/1000


lab var phonecall "Total no. of phonecalls made ('000)"
lab var calllength "Total length of phonecalls made ('000 hours)"
lab var ordertaker "Order taker position"
lab var treatment "In treatment group"
lab var experiment "Experiment is on"
lab var experiment_treatment "experiment is on X in treatment group"
lab var worked_lastmo "Worked through last month"
lab var phone_lastmo "Worked and made phonecalls through last month"

lab def noyes 0 "No" 1 "Yes"
lab val worked_lastmo noyes
lab val treatment noyes

order personid ordertaker experiment treatment experiment_tr

save "$data_out/wfh_workfile.dta",replace
