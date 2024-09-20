/*******************************************************************************   

 Main do file for A Poor Means Test? paper by Brown, Ravallion and van de Walle
 Name: Brown_Ravallion_vandeWalle_PMT_LSMSPanel.do
 
 Created: Nov 25, 2016 
 
 Last edited: April 25, 2018
 
 Author: Cait Brown, cb575@georgetown.edu 
 
 Dataset for the panel analysis. Household level dataset for the two survey rounds 
 available for each of the countries. Using LSMS data from the World Bank. 
 Datasets have been compiled and appended into the master file Brown_Ravallion_vandeWalle_PMT_LSMSPanel.dta. 
 This do file is for cleaning data and the panel analysis in the paper (Tables 11 and 12). 
 Tables are labelled where possible. 
 

 All errors are my own.
 *****************************************************************************/ 
 
 
cd "" 

use "Brown_Ravallion_vandeWalle_PMT_Panel.dta", clear 

tab country year
replace year = 2010 if year == 2011 & country == "Uganda"
replace year = 2012 if year == 2013 & country == "Uganda" 

* create year variable 
gen round = 1 if year == 2010 | year == 2011 
replace round = 2 if year == 2012 | year == 2013 
label var round "Survey round" 
tab country round 

* fix missing month variable
bys country: sum month 
replace month = 1 if  country == "Ethiopia" | country == "Malawi"

gen popweight1 = hhweight*hhsize 
replace popweight1 = popweight if popweight1 == . 
label var popweight1 "Population weight v2"
bys country: sum popweight1 hhweight

* Nigeria's population weights are actually household weights 
replace popweight1 = popweight*hhsize if country == "Nigeria"
* need to calculate hhweight for Nigeria 
replace hhweight = popweight / hhsize if country == "Nigeria" 

* drop those with missing weights - Uganda is missing 27, Nigeria missing 390
drop if missing(popweight1) 
drop if missing(hhweight)

local vars share_widow share_disabledm share_disabledf share_orphanm share_orphanf   
foreach x of local vars { 
replace `x' = 0 if missing(`x')
}

replace month = 1 if missing(month)
replace month = 1 if month == 0 

egen hhsize_cat = cut(hhsize), at(1,3,5,7,9)
replace hhsize_cat = 9 if hhsize >=9 
label var hhsize_cat "Household size categories"


* Dropping values

**** replace consumption = . for missing values --> percentiles then only formed on those in the regression
local vars water_piped water_well toilet_flush toilet_pit  floor_finish  wall_finish  roof_finish members_per_room kitchen_room fuel_elecgas fuel_charcoal /*
*/ electric radio telev fridge bicycle motorbike car mobile_phone computer video stove_any sew_machine aircon iron satelite generator own_house /*
*/  urban  female_head edu_head_primary edu_head_secondary max_edu_primary max_edu_secondary div_sep_head widow_head nevermar_head work_paid_head work_selfemp_nonf_head  muslim christian share_05f share_05m share_614f share_614m  share_65plusf share_65plusm   /*
*/ month age_head_cat hhsize 

foreach x of local vars{
replace real_consumption_pc = . if missing(`x')
}

drop if real_consumption_pc == . 
* 2,118 obs dropped 

replace hhid = "" if hhid == "." 
replace hhid1 = "" if hhid1 == "." 
replace hhid0 = "" if hhid0 == "." 

order country hhid hhid1 hhid0 year round

replace hhid1 = hhid if country == "Uganda" & hhid != "" 
drop if hhid1 == ""

* which households have ids in both rounds 
bys country hhid1: gen nval = _N 
gen panel = 1 if nval >= 2 
tab nval
drop nval 
tab country panel, missing

**** Only including panel households now. 
drop if panel != 1 
drop panel 
// dropped 2,809 obs

tab country round 
** split off households is making this not 1-1


* deflate - 2010 base year 
* Ethiopia: average CPI between 2011/12 : 148.4	average CPI between 2013/14 : 183.3 
* Malawi: Everything in 2013 prices already.  166.1 in 2013
* Nigeria: Average CPI 2012/13: 2012 = 124.4	2013 = 134.9	Average = 129.65
* Tanzania: average CPI in 2012/13: 135.85
* Uganda: average CPI between 2011/12: 2011 118.7 2012	135.3 Average = 127
replace real_consumption_pc = real_consumption_pc / 1.484 if year == 2011 & country == "Ethiopia"
replace real_consumption_pc = real_consumption_pc / 1.833 if year == 2013 & country == "Ethiopia" 
replace real_consumption_pc = real_consumption_pc / 1.661 if  country == "Malawi" 
replace real_consumption_pc = real_consumption_pc / 1.297 if year == 2013 & country == "Nigeria" 
replace real_consumption_pc = real_consumption_pc / 1.359 if year == 2012 & country == "Tanzania" 

gen consump = log(real_consumption_pc)  
label var consump "Log real total consumption per capita" 

drop consumption_pctile
gen consumption_pctile = .
label var consumption_pctile "Consumption percentile" 

local country `" "Ethiopia"  "Malawi" "Nigeria" "Tanzania" "Uganda" "'
foreach x of local country{ 
foreach num of numlist 1/2 {
xtile percentile_`x'_`num' = real_consumption_pc  [aweight = hhweight] if country == "`x'" & round == `num' , n(100) 
replace consumption_pctile = percentile_`x'_`num' if country == "`x'"  & round == `num' 
drop percentile_`x'_`num'
}
}

* generating poverty lines - these are in linear terms

foreach num of numlist 1/2 {
gen pov = real_consumption_pc if consumption_pctile == 20 & round == `num'
bys country: egen pov_line_20_r`num' = max(pov)
drop pov 
gen pov = real_consumption_pc if consumption_pctile == 40 & round == `num'
bys country: egen pov_line_40_r`num' = max(pov)
drop pov 
label var pov_line_20_r`num' "Poverty line round `num' 20 percent" 
label var pov_line_40_r`num' "Poverty line round `num' 40 percent" 

gen poor_20_r`num' = 0 if  round == `num'
replace poor_20_r`num' = 1 if real_consumption_pc <= pov_line_20_r`num' & round == `num'
label var poor_20_r`num' "Poor in 20th pctile round `num'" 
gen poor_40_r`num' = 0 if  round == `num'
replace poor_40_r`num' = 1 if real_consumption_pc <= pov_line_40_r`num' & round == `num'
label var poor_40_r`num' "Poor in 40th pctile round `num'" 
}

tabstat pov_line_20_r1 pov_line_40_r1 pov_line_20_r2 pov_line_40_r2 [aweight=hhweight], by(country)

tabstat  poor_20_r1 poor_40_r1 [aweight=hhweight] if round == 1 , by(country)
tabstat  poor_20_r2 poor_40_r2 [aweight=hhweight] if round == 2 , by(country)

save "Brown_Ravallion_vandeWalle_PMT_Panel.dta", replace



* Basic PMT
gen yhat_pmt_sh_consump = .
label var yhat_pmt_sh_consump "Predicted values short PMT"


local wealth  toilet_flush toilet_pit floor_finish wall_finish roof_finish fuel_elecgas fuel_charcoal 
local hh  urban female_head edu_head_primary edu_head_secondary div_sep_head widow_head work_paid_head work_selfemp_nonf_head muslim christian share_05f share_05m share_614f share_614m  share_65plusf share_65plusm  

eststo clear
local country `" "Ethiopia"  "Malawi" "Nigeria" "Tanzania" "Uganda" "'
foreach x of local country{ 
foreach num of numlist 1/2 {
eststo: quietly reg consump `wealth'  `hh' i.hhsize_cat i.age_head_cat i.state i.month if country == "`x'" & round == `num', vce(cluster EA)
predict yhat
replace yhat_pmt_sh_consump = yhat if country == "`x'" & round == `num'
drop yhat
}
}
esttab using "results", replace  b(%10.3f) star(* .10 ** .05 *** .01) label se(%9.3f) obslast longtable r2 csv keep(`wealth'  `hh' )
eststo clear


* Extended PMT 
gen female_head_widow = 0 
replace female_head_widow = 1 if female_head == 1 & widow_head == 1 
label var female_head_widow "Widowed female head" 
gen female_head_div = 0 
replace female_head_div = 1 if female_head == 1 & div_sep_head == 1
label var female_head_div "Divorced female head" 
gen female_head_nevermar = 0 
replace female_head_nevermar = 1 if female_head == 1 & nevermar_head == 1
label var female_head_nevermar "Never married female head" 


gen yhat_pmt_ext_consump = .
label var yhat_pmt_ext_consump "Predicted values extended PMT"

local wealth water_piped water_well toilet_flush toilet_pit  floor_finish  wall_finish  roof_finish members_per_room kitchen_room fuel_elecgas fuel_charcoal 
local assets electric radio telev fridge bicycle motorbike car  mobile_phone computer video stove_any sew_machine aircon iron satelite generator own_house
local hh urban  female_head edu_head_primary edu_head_secondary max_edu_primary max_edu_secondary div_sep_head widow_head nevermar_head  female_head_widow female_head_div female_head_nevermar work_paid_head work_selfemp_nonf_head  muslim christian  share_05f share_05m share_614f share_614m  share_65plusf share_65plusm  share_widow share_disabledm share_disabledf share_orphanm share_orphanf   

eststo clear
local country `" "Ethiopia"  "Malawi" "Nigeria" "Tanzania" "Uganda" "'
foreach x of local country{ 
foreach num of numlist 1/2 {
eststo: quietly reg consump `wealth' `assets' `hh' i.hhsize_cat i.age_head_cat i.state  i.month if country == "`x'" & round == `num' , vce(cluster EA)
predict yhat
replace yhat_pmt_ext_consump = yhat if country == "`x'"  & round == `num'
drop yhat
}
}
esttab using "results", append  b(%10.3f) star(* .10 ** .05 *** .01) label se(%9.3f) obslast longtable r2 csv keep(`wealth' `assets' `hh' )
eststo clear


****** Method 1
* need to generate predicted values for round 1 PMT vars based on round 2 data
** Weighted regressions to the rescue. 

* Basic PMT
save "Brown_Ravallion_vandeWalle_PMT_Panel.dta", replace
use "Brown_Ravallion_vandeWalle_PMT_Panel.dta", clear

eststo clear
local wealth  toilet_flush toilet_pit floor_finish wall_finish roof_finish fuel_elecgas fuel_charcoal 
local hh  urban female_head edu_head_primary edu_head_secondary div_sep_head widow_head work_paid_head work_selfemp_nonf_head muslim christian share_05f share_05m share_614f share_614m  share_65plusf share_65plusm  

local country `" "Ethiopia"  "Malawi" "Nigeria" "Tanzania" "Uganda" "'
foreach x of local country{ 
use "Brown_Ravallion_vandeWalle_PMT_Panel.dta", clear
drop if round == 2

quietly reg consump `wealth' `hh' i.hhsize_cat i.age_head_cat i.state if country == "`x'", vce(cluster EA)
use "Brown_Ravallion_vandeWalle_PMT_Panel.dta", clear
predict yhat_b1_`x' if country == "`x'"
save "Brown_Ravallion_vandeWalle_PMT_Panel.dta", replace 
}

save "Brown_Ravallion_vandeWalle_PMT_Panel.dta", replace

* Extended PMT
eststo clear
local wealth water_piped water_well toilet_flush toilet_pit  floor_finish  wall_finish  roof_finish members_per_room kitchen_room fuel_elecgas fuel_charcoal 
local assets electric radio telev fridge bicycle motorbike car  mobile_phone computer video stove_any sew_machine aircon iron satelite generator own_house
local hh urban  female_head edu_head_primary edu_head_secondary max_edu_primary max_edu_secondary div_sep_head widow_head nevermar_head  female_head_widow female_head_div female_head_nevermar work_paid_head work_selfemp_nonf_head  muslim christian  share_05f share_05m share_614f share_614m  share_65plusf share_65plusm  share_widow share_disabledm share_disabledf share_orphanm share_orphanf   


local country `" "Ethiopia"  "Malawi" "Nigeria" "Tanzania" "Uganda" "'
foreach x of local country{ 
use "Brown_Ravallion_vandeWalle_PMT_Panel.dta", clear
drop if round == 2

quietly reg consump `wealth' `assets' `hh' i.hhsize_cat i.age_head_cat i.state  i.month if country == "`x'" , vce(cluster EA)
use "Brown_Ravallion_vandeWalle_PMT_Panel.dta", clear
predict yhat_e1_`x' if country == "`x'"
save "Brown_Ravallion_vandeWalle_PMT_Panel.dta", replace 
}



gen yhat_sh_r1_cons = . 
label var yhat_sh_r1_cons "Predicted values basic PMT using round 1" 
gen yhat_ext_r1_cons = . 
label var yhat_ext_r1_cons "Predicted values extended PMT using round 1" 


local country `" "Ethiopia"  "Malawi" "Nigeria" "Tanzania" "Uganda" "'
foreach x of local country{ 
replace yhat_sh_r1_cons = yhat_b1_`x' if country == "`x'"
drop yhat_b1_`x'
replace yhat_ext_r1_cons = yhat_e1_`x' if country == "`x'"
drop yhat_e1_`x'
}


**** Method 2
** Round 1 PMT compared to Round 2 actual - use consump_round_2 and compare to round 1 results. 
* use max yhat round 1 to compare yhat round 1  to round 2 actual poverty status

local vars yhat_pmt_sh_consump yhat_pmt_ext_consump
foreach var of local vars{ 
gen r1 = `var' if round == 1
egen `var'_max = max(r1), by(country hhid1)
drop r1
}

* checking 
bys country round: sum yhat_pmt_sh_consump yhat_pmt_sh_consump_max
bys country round: sum yhat_pmt_sh_consump yhat_sh_r1_cons

* variable names too long
rename yhat_pmt_sh_consump_max yhat_pmt_sh_max
rename yhat_pmt_ext_consump_max yhat_pmt_ext_max

save "Brown_Ravallion_vandeWalle_PMT_Panel.dta", replace



**** Poverty lines 

use "Brown_Ravallion_vandeWalle_PMT_Panel.dta", clear

** convert back to real consumption pc terms in order to compare to poverty line. 
local vars yhat_pmt_sh_consump yhat_pmt_ext_consump yhat_sh_r1_cons yhat_ext_r1_cons yhat_pmt_sh_max yhat_pmt_ext_max
foreach var of local vars {
replace `var' = exp(`var')
}


local vars yhat_pmt_sh_consump yhat_pmt_ext_consump yhat_sh_r1_cons yhat_ext_r1_cons yhat_pmt_sh_max yhat_pmt_ext_max
foreach var of local vars {
foreach num of numlist 1/2 {
gen pr20_`var'_r`num' = 0 if `var' != . & round == `num'
replace pr20_`var'_r`num' = 1 if `var' <= pov_line_20_r`num' & `var' != . & round == `num'
gen pr40_`var'_r`num' = 0 if `var' != .  & round == `num'
replace pr40_`var'_r`num' = 1 if `var' <= pov_line_40_r`num' & `var' != . & round == `num'
}
}

** Looking at total predicted poor by round
tabstat pr20_yhat_pmt_sh_consump_r1 pr20_yhat_pmt_sh_consump_r2 pr20_yhat_sh_r1_cons_r2 pr20_yhat_pmt_sh_max_r1 pr20_yhat_pmt_sh_max_r2 [aweight=hhweight] , by(country)


**** Poverty rate - need to generate mean values here.
* cutoff point - actual poverty rate

foreach num of numlist 1/2 {
by country, sort: egen poor_20_mean_r`num' = wtmean(poor_20_r`num'), weight(hhweight) 
by country, sort: egen poor_40_mean_r`num' = wtmean(poor_40_r`num'), weight(hhweight) 
}

tabstat poor_20_r1 [aweight=hhweight] , by(country)
tabstat poor_20_r2 [aweight=hhweight] , by(country)
tabstat poor_20_mean_r1 [aweight=hhweight] , by(country)
tabstat poor_20_mean_r2 [aweight=hhweight] , by(country)

* percentiles
local vars yhat_pmt_sh_consump yhat_pmt_ext_consump yhat_sh_r1_cons yhat_ext_r1_cons yhat_pmt_sh_max yhat_pmt_ext_max

foreach var of local vars {
gen pc1_`var' = . 
gen pc2_`var' = . 


local country `" "Ethiopia"  "Malawi" "Nigeria" "Tanzania" "Uganda" "'
foreach x of local country{ 
foreach num of numlist 1/2 {

xtile percentile_`x' = `var' [pweight=hhweight]  if country == "`x'" & `var' != . & round == `num' , n(100) 
replace  pc`num'_`var' = percentile_`x' if country == "`x'" & `var' != . & round == `num', 
drop percentile_`x'
}
}
replace pc1_`var' = pc1_`var' / 100
replace pc2_`var' = pc2_`var' / 100
} 


* generating indicator 
local vars yhat_pmt_sh_consump yhat_pmt_ext_consump yhat_sh_r1_cons yhat_ext_r1_cons yhat_pmt_sh_max yhat_pmt_ext_max
foreach var of local vars {
foreach num of numlist 1/2 {
gen po20_`var'_r`num' = 0 if `var' != . & round == `num'
replace po20_`var'_r`num' = 1 if pc`num'_`var' <= 0.2001 & `var' != . & round == `num'
gen po40_`var'_r`num' = 0 if `var' != . & round == `num'
replace po40_`var'_r`num' = 1 if pc`num'_`var' <= 0.4001 & `var' != . & round == `num'
drop pc`num'_`var'
}
}

* checking - mean should be equal to 0.2 or 0.4
tabstat po20_yhat_pmt_sh_consump_r1 po40_yhat_pmt_sh_consump_r1 po20_yhat_pmt_sh_consump_r2 po40_yhat_pmt_sh_consump_r2 po20_yhat_pmt_ext_consump_r1 po40_yhat_pmt_ext_consump_r1 po20_yhat_pmt_ext_consump_r2 po40_yhat_pmt_ext_consump_r2 [aweight=hhweight], by(country)
tabstat po20_yhat_sh_r1_cons_r1 po40_yhat_sh_r1_cons_r1 po20_yhat_sh_r1_cons_r2 po40_yhat_sh_r1_cons_r2 po20_yhat_ext_r1_cons_r1 po40_yhat_ext_r1_cons_r1 po20_yhat_ext_r1_cons_r2 po40_yhat_ext_r1_cons_r2 po20_yhat_pmt_sh_max_r1 po40_yhat_pmt_sh_max_r1 po20_yhat_pmt_sh_max_r2 po40_yhat_pmt_sh_max_r2 po20_yhat_pmt_ext_max_r1 po40_yhat_pmt_ext_max_r1 po20_yhat_pmt_ext_max_r2 po40_yhat_pmt_ext_max_r2 [aweight=hhweight], by(country)

save "Brown_Ravallion_vandeWalle_PMT_Panel.dta", replace


************* Inclusion and exclusion errors **************

local vars yhat_pmt_sh_consump yhat_pmt_ext_consump yhat_sh_r1_cons yhat_ext_r1_cons yhat_pmt_sh_max yhat_pmt_ext_max
local num `" "20" "40" "'

foreach var of local vars { 
foreach x of local num {

*** line

gen ie1_pr`x'_`var' = 0 if `var' != . 
replace ie1_pr`x'_`var' = 1 if poor_`x'_r1 == 0 & pr`x'_`var'_r1 == 1 & round == 1
replace ie1_pr`x'_`var' = 1 if poor_`x'_r2 == 0 & pr`x'_`var'_r2 == 1 & round == 2
gen ee1_pr`x'_`var' = 0 if `var' != . 
replace ee1_pr`x'_`var' = 1 if poor_`x'_r1 == 1 & pr`x'_`var'_r1 == 0  & round == 1
replace ee1_pr`x'_`var' = 1 if poor_`x'_r2 == 1 & pr`x'_`var'_r2 == 0  & round == 2 

* share of predicted poor
bys country round: egen ie_pr`x'_`var' = wtmean(ie1_pr`x'_`var')  if pr`x'_`var'_r1 == 1 | pr`x'_`var'_r2 == 1 , weight(hhweight) 
drop ie1_pr`x'_`var'

* share of actual poor
bys country round: egen ee_pr`x'_`var' = wtmean(ee1_pr`x'_`var')  if poor_`x'_r1 == 1  | poor_`x'_r2 == 1, weight(hhweight) 

drop ee1_pr`x'_`var'

*** rate
gen ie1_po`x'_`var' = 0 if `var' != . 
replace ie1_po`x'_`var' = 1 if poor_`x'_r1 == 0 & po`x'_`var'_r1 == 1 & round == 1
replace ie1_po`x'_`var' = 1 if poor_`x'_r2 == 0 & po`x'_`var'_r2 == 1  & round == 2

gen ee1_po`x'_`var' = 0 if `var' != .  
replace ee1_po`x'_`var' = 1 if poor_`x'_r1 == 1 & po`x'_`var'_r1 == 0 & round == 1
replace ee1_po`x'_`var' = 1 if poor_`x'_r2 == 1 & po`x'_`var'_r2 == 0  & round == 2

* share of predicted poor
bys country round: egen ie_po`x'_`var' = wtmean(ie1_po`x'_`var')  if po`x'_`var'_r1 == 1 | po`x'_`var'_r2 == 1 , weight(hhweight) 
drop ie1_po`x'_`var'

* share of actual poor
bys country round: egen ee_po`x'_`var' = wtmean(ee1_po`x'_`var')  if poor_`x'_r1 == 1 | poor_`x'_r2 == 1  , weight(hhweight) 
drop ee1_po`x'_`var'
}
}


* Inclusion/Exclusion Errors for Method 1 & Method 2

/* Table 11 */
** method 1 
local vars yhat_sh_r1_cons yhat_ext_r1_cons
foreach var of local vars { 
tabstat ie_pr20_`var' ee_pr20_`var'  ie_pr40_`var' ee_pr40_`var' ie_po20_`var' ee_po20_`var'  ie_po40_`var' ee_po40_`var' [aweight=hhweight] if round == 2, by(country) f(%9.3g)
}

/* Appendix */ 
** method 2 
local vars yhat_pmt_sh_max yhat_pmt_ext_max
foreach var of local vars { 
tabstat ie_pr20_`var' ee_pr20_`var'  ie_pr40_`var' ee_pr40_`var' ie_po20_`var' ee_po20_`var'  ie_po40_`var' ee_po40_`var' [aweight=hhweight] if round == 2, by(country) f(%9.3g)
}


bys country hhid1: gen nval = _N
tab country nval 

*** targeting differential 

* create H hat - average predicted poor 
local vars yhat_pmt_sh_consump yhat_sh_r1_cons yhat_pmt_sh_max  yhat_pmt_ext_consump yhat_ext_r1_cons  yhat_pmt_ext_max
foreach var of local vars { 
egen pr20_`var'_m = wtmean(pr20_`var'_r2) if round == 2, by(country) weight(hhweight)
}

* create max var 
local vars yhat_pmt_sh_consump yhat_sh_r1_cons yhat_pmt_sh_max  yhat_pmt_ext_consump yhat_ext_r1_cons  yhat_pmt_ext_max
foreach var of local vars { 
egen ee_pr20_`var'1 = max(ee_pr20_`var') if round == 2, by(country)
}

local vars yhat_pmt_sh_consump yhat_sh_r1_cons yhat_pmt_sh_max  yhat_pmt_ext_consump yhat_ext_r1_cons  yhat_pmt_ext_max
foreach var of local vars { 
gen td_pr20_`var' = (1-pr20_`var'_m-ee_pr20_`var'1)/0.8
}

drop nval
bys country round: gen nval = _n == 1

/* Appendix */ 
tabstat td_pr20_yhat_pmt_sh_consump td_pr20_yhat_sh_r1_cons td_pr20_yhat_pmt_sh_max td_pr20_yhat_pmt_ext_consump td_pr20_yhat_ext_r1_cons td_pr20_yhat_pmt_ext_max if nval == 1 & round == 2, by(country) f(%9.3g)

save "Brown_Ravallion_vandeWalle_PMT_Panel.dta", replace


*********** Poverty Indices ****************

* actual - these are in linear terms 

gen actual_gap_pc_r2 = pov_line_20_r2 - real_consumption_pc if poor_20_r2 == 1 & round == 2
replace actual_gap_pc_r2 = 0 if missing(actual_gap_pc_r2) 
label var actual_gap_pc_r2 "Actual household poverty gap per capita round 2" 
gen actual_gap_r2 = actual_gap_pc_r2*hhsize
label var actual_gap_r2 "Actual household poverty gap round 2" 

egen agg_actual_gap_r2 = sum(actual_gap_r2), by(country)
label var agg_actual_gap_r2 "Aggregate actual poverty gap round 2"

tabstat agg_actual_gap_r2 [aweight=hhweight], by(country) f(%12.1g)
bys country: sum actual_gap_r2 agg_actual_gap_r2

* head count index 
gen hc_actual_r2 = 0 
replace hc_actual_r2 = 1 if real_consumption_pc <= pov_line_20_r2 & round == 2

* poverty gap index - gap is only calculated for those below it
gen pg_actual_r2 = 0
replace pg_actual_r2 = (pov_line_20_r2 - real_consumption_pc) / pov_line_20_r2 if real_consumption_pc < pov_line_20_r2 & round == 2

* watts index 
gen wat_actual_r2 = 0
replace wat_actual_r2 = log(pov_line_20_r2 / real_consumption_pc) if real_consumption_pc < pov_line_20_r2 & round == 2

**** Uniform transfers 
* effect on poverty using actual consumption gap

local vars yhat_pmt_sh_consump yhat_pmt_ext_consump yhat_sh_r1_cons yhat_ext_r1_cons yhat_pmt_sh_max yhat_pmt_ext_max
foreach var of local vars {

* total number of predicted poor
egen total_poor = sum(hhsize) if pr20_`var'_r2 == 1 & round == 2 & `var' != .  , by(country) 
* transfer amount 
gen trans = agg_actual_gap_r2 / total_poor if round == 2 & `var' != .  
* total transfer per household 
gen tr_`var' = trans*hhsize if round == 2 & `var' != .  

* new consumption - transfer goes only to those predicted poor 
gen cons_`var' = real_consumption_pc if round == 2 & `var' != .  
replace cons_`var' = real_consumption_pc + tr_`var'/hhsize if pr20_`var'_r2 == 1 & round == 2 & `var' != .  

* head count index 
gen hc_`var'_r2 = 0 if round == 2 & `var' != .  
replace hc_`var'_r2 = 1 if cons_`var' <= pov_line_20_r2 & round == 2 & `var' != .  

* poverty gap index - gap is only calculated for those below it
gen pg_`var'_r2 = (pov_line_20_r2 - cons_`var') / pov_line_20_r2 if cons_`var' < pov_line_20_r2 & round == 2 & `var' != .  
replace pg_`var'_r2 = 0 if missing(pg_`var'_r2) & round == 2 & `var' != .  

* squared poverty gap index 
gen wat_`var'_r2 = log(pov_line_20_r2 / cons_`var') if cons_`var' < pov_line_20_r2 & round == 2 & `var' != . 

drop total_poor trans tr_`var' cons_`var'
}


*** making categories 
* number of household members over 65 
egen nmemb65plus = rowtotal(nmemb65plusf nmemb65plusm)
label var nmemb65plus "Number of members 65 and older"

* widows, disabled
egen nmemb_dis = rowtotal(numwidow numdisabledm numdisabledf)
label var nmemb_dis "Number of members widowed and disabled"

* elderly, widows, disabled 
egen nmemb_all = rowtotal(nmemb65plus numwidow numdisabledm numdisabledf)
label var nmemb_all "Number of members elderly, widowed, disabled"

* child payment - household members 14 and under
gen nmemb_child = nmemb05f + nmemb05m + nmemb614f + nmemb614m
// cap transfer to 3 children
replace nmemb_child = 3 if nmemb_child > 3
label var nmemb_child "Number of children 14 and under" 

* eeeveryone gets a prize 
egen nmemb_all2 = rowtotal(nmemb_all nmemb_child)
label var nmemb_all2 "Child, elderly, widowed, disabled" 

* FHH with children 
gen fhh_child = 0 
replace fhh_child = 1 if female_head == 1 & nmemb_child >= 1
label var fhh_child "Female headed household with children" 

* shocks 
gen shock_nature = 0 
replace shock_nature = 1 if shock_drought == 1 | shock_flood == 1 | shock_livestock == 1
label var shock_nature "Shock: drought, flood, death of livestock" 
gen shock_job = 0 
replace shock_job = 1 if shock_death_mainmem == 1 | shock_jobloss == 1 
label var shock_job "Shock: death of income earner or job loss"


local vars hhsize nmemb65plus nmemb_dis nmemb_all nmemb_child nmemb_all2 fhh_child shock_nature shock_job
foreach var of local vars { 

egen total_poor = sum(`var') if round == 2, by(country) 

gen trans = agg_actual_gap_r2 / total_poor if round == 2

gen tr_`var' = trans*`var' if round == 2
replace tr_`var' = 0 if missing(tr_`var')  & round == 2
 
gen cons_`var' = real_consumption_pc + tr_`var'/hhsize if round == 2

* head count index 
gen hc_`var'_r2 = 0 if round == 2
replace hc_`var'_r2 = 1 if cons_`var' <= pov_line_20_r2 & round == 2

* poverty gap index - gap is only calculated for those below it
gen pg_`var'_r2 = (pov_line_20_r2 - cons_`var') / pov_line_20_r2 if cons_`var' < pov_line_20_r2 & round == 2
replace pg_`var'_r2 = 0 if missing(pg_`var'_r2) & round == 2

* squared poverty gap index 
gen wat_`var'_r2 = log(pov_line_20_r2 / cons_`var') if cons_`var' < pov_line_20_r2 & round == 2 & `var' != . 

drop total_poor trans tr_`var' cons_`var'
}

order hc_*, last 
order pg_*, last 
order wat_*, last 


*** Tables 

* head count index 
tabstat hc_actual_r2 hc_hhsize_r2 hc_yhat_pmt_sh_consump_r2 hc_yhat_pmt_ext_consump_r2 hc_yhat_sh_r1_cons_r2 hc_yhat_ext_r1_cons_r2 hc_yhat_pmt_sh_max_r2 hc_yhat_pmt_ext_max_r2  hc_nmemb65plus_r2 hc_nmemb_dis_r2 hc_nmemb_all_r2 hc_nmemb_child_r2 hc_nmemb_all2_r2 hc_fhh_child_r2  [aweight=hhweight] if round == 2, by(country) f(%9.3g)

* poverty gap index 
tabstat pg_actual_r2 pg_hhsize_r2  pg_yhat_pmt_sh_consump_r2 pg_yhat_pmt_ext_consump_r2 pg_yhat_sh_r1_cons_r2 pg_yhat_ext_r1_cons_r2 pg_yhat_pmt_sh_max_r2 pg_yhat_pmt_ext_max_r2  pg_nmemb65plus_r2 pg_nmemb_dis_r2 pg_nmemb_all_r2 pg_nmemb_child_r2 pg_nmemb_all2_r2 pg_fhh_child_r2  [aweight=hhweight] if round == 2, by(country) f(%9.3g)

* Watts index 
tabstat wat_actual_r2 wat_hhsize_r2 wat_yhat_pmt_sh_consump_r2 wat_yhat_pmt_ext_consump_r2 wat_yhat_sh_r1_cons_r2 wat_yhat_ext_r1_cons_r2 wat_yhat_pmt_sh_max_r2 wat_yhat_pmt_ext_max_r2 wat_nmemb65plus_r2 wat_nmemb_dis_r2 wat_nmemb_all_r2 wat_nmemb_child_r2 wat_nmemb_all2_r2 wat_fhh_child_r2  [aweight=hhweight] if round == 2, by(country) f(%9.3g)


save "Brown_Ravallion_vandeWalle_PMT_Panel.dta", replace




