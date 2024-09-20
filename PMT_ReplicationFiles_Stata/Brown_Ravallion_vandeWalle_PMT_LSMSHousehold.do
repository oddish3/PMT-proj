/*******************************************************************************

 Main do file for A Poor Means Test? paper by Brown, Ravallion and van de Walle
 Name: Brown_Ravallion_vandeWalle_PMT_LSMSHousehold.do

 Created: Feb 25, 2016

 Last edited: April 25, 2018

 Author: Cait Brown, cb575@georgetown.edu

 Household level dataset using LSMS data from the World Bank. Datasets have
 already been compiled and appended into the master file Brown_Ravallion_vandeWalle_PMT.dta.
 This do file is for cleaning data and the main analysis in the paper.
 Tables are labelled where possible.


 All errors are my own.
 *****************************************************************************/


// Set path here
cd ""

/******************** Cleaning and Descriptive Stats **********************/

use "Brown_Ravallion_vandeWalle_PMT.dta", clear

egen hhsize_cat = cut(hhsize), at(1,3,5,7,9)
replace hhsize_cat = 9 if hhsize >=9
label var hhsize_cat "Household size categories"

bys country: sum month
replace month = 1 if  country == "BurkinaFaso"  |  country == "Ethiopia" | country == "Malawi" | country == "Mali"

gen popweight1 = hhweight*hhsize
replace popweight1 = popweight if popweight1 == .
label var popweight1 "Population weight v2"

* need to calculate hhweight for Nigeria
replace hhweight = popweight / hhsize if country == "Nigeria"
bys country: sum popweight popweight1 hhweight

* drop those with missing weights
drop if missing(hhweight)
drop if missing(popweight1)
* dropped 1,335 obs

tab country if missing(EA)

local vars share_05f share_05m share_614f share_614m  share_65plusf share_65plusm  share_widow share_disabledm share_disabledf share_orphanm share_orphanf
foreach x of local vars {
replace `x' = 0 if missing(`x')
}


/** Table 2 **/

* % of population below mean
egen mean_consump = wtmean(real_consumption_pc), weight(popweight1) by(country)
gen below_mean = 0
replace below_mean = 1 if real_consumption_pc <= mean_consump

tabstat below_mean [aweight = popweight1], by(country) f(%9.3g)


*****  Dropping values - only estimate the PMT and subsequent errors on households that have non-missing values for all variables

* replace consumption = . for missing values --> percentiles then only formed on those in the regression
local vars water_piped water_well toilet_flush toilet_pit  floor_finish  wall_finish  roof_finish members_per_room kitchen_room fuel_elecgas fuel_charcoal /*
*/ electric radio telev fridge bicycle motorbike car telephone mobile_phone computer video stove_any sew_machine aircon iron satelite generator own_house /*
*/  urban  female_head edu_head_primary edu_head_secondary max_edu_primary max_edu_secondary div_sep_head widow_head nevermar_head work_paid_head work_selfemp_nonf_head work_daily_head share_05f share_05m share_614f share_614m  share_65plusf share_65plusm muslim christian  /*
*/ month age_head_cat hhsize_cat

foreach x of local vars{
replace real_consumption_pc = . if missing(`x')
}
drop if real_consumption_pc == .
** dropped 1,942 obs

gen consump = log(real_consumption_pc)
label var consump "Log real consumption per capita"
tabstat consump, by(country)

drop consumption_pctile
gen consumption_pctile = .
local country `" "BurkinaFaso" "Ethiopia" "Ghana"  "Malawi" "Mali" "Niger" "Nigeria" "Tanzania" "Uganda" "'
foreach x of local country{
xtile percentile_`x' = consump [aweight = hhweight]  if country == "`x'" & consump != . , n(100)
replace consumption_pctile = percentile_`x' if country == "`x'"
drop percentile_`x'
}

/* Table A1 in Appendix */
local wealth real_consumption_pc water_piped water_well toilet_flush toilet_pit floor_natural floor_rudiment floor_finish  wall_natural wall_rudiment wall_finish  roof_natural roof_rudiment roof_finish members_per_room kitchen_room fuel_elecgas fuel_charcoal fuel_wood
local assets electric radio telev fridge bicycle motorbike car telephone mobile_phone computer video stove_any sew_machine aircon iron satelite generator own_house
local hh urban age_head female_head edu_head_primary edu_head_secondary  max_edu_primary max_edu_secondary ever_married_head married_head div_sep_head widow_head nevermar_head hhsize  work_paid_head work_selfemp_nonf_head work_selfemp_farm_head work_daily_head muslim christian share_05f share_05m share_614f share_614m share_1564f share_1564m  share_65plusf share_65plusm share_widow share_disabledm share_disabledf share_orphanm share_orphanf

tabstat `wealth' `assets' `hh' [aweight=hhweight], by(country) f(%9.3g)


* generating poverty lines - these are in log terms
gen pov = consump if consumption_pctile == 20
bys country: egen pov_line_20 = max(pov)
drop pov

gen pov = consump if consumption_pctile == 40
bys country: egen pov_line_40 = max(pov)
drop pov

label var pov_line_20 "Poverty line 20 percent"
label var pov_line_40 "Poverty line 40 percent"

* generate poverty rates pre-transfers
gen poor_20 = 0 if consump != .
replace poor_20 = 1 if consump <= pov_line_20
label var poor_20 "Poor (in 20th pctile)"

gen poor_40 = 0 if consump  != .
replace poor_40 = 1 if consump  <= pov_line_40
label var poor_40 "Poor (in 40th pctile)"

* checking
tabstat poor_40  poor_20 [aweight=hhweight], by(country)

save "Brown_Ravallion_vandeWalle_PMT.dta", replace



/*********************** PMT Regressions ****************************/

/*** TABLE 2 ***/
tab country

** Basic regression.

use "Brown_Ravallion_vandeWalle_PMT.dta", clear

* Basic PMT
gen yhat_pmt_sh_consump = .
label var yhat_pmt_sh_consump "Predicted values short PMT"

local wealth  toilet_flush toilet_pit floor_finish wall_finish roof_finish fuel_elecgas fuel_charcoal
local hh  urban female_head edu_head_primary edu_head_secondary div_sep_head widow_head work_paid_head work_selfemp_nonf_head muslim christian share_05f share_05m share_614f share_614m  share_65plusf share_65plusm

eststo clear
local country `" "BurkinaFaso" "Ethiopia" "Ghana"  "Malawi" "Mali" "Niger" "Nigeria" "Tanzania" "Uganda" "'
foreach x of local country{
eststo: quietly reg consump `wealth'  `hh' i.hhsize_cat i.age_head_cat i.state i.month if country == "`x'" , vce(cluster EA)
predict yhat
replace yhat_pmt_sh_consump = yhat if country == "`x'"
drop yhat
}
esttab using "results", replace   b(%10.3f) star(* .10 ** .05 *** .01) label se(%9.3f) obslast longtable r2 csv keep(`wealth'  `hh' )
eststo clear

* Poor indicators as dependent variable basic PMT.
*** OLS and PROBIT
local depvars poor_20 poor_40
foreach x of local depvars {
gen yhat_pmt_sh_`x' = .
gen yhat_pmt_sh_pr_`x' = .
label var yhat_pmt_sh_`x' "Predicted consumption `x'"
label var yhat_pmt_sh_pr_`x' "Predicted consumption probit `x'"
}

local wealth  toilet_flush toilet_pit floor_finish wall_finish roof_finish fuel_elecgas fuel_charcoal
local hh  urban female_head edu_head_primary edu_head_secondary div_sep_head widow_head work_paid_head work_selfemp_nonf_head muslim christian share_05f share_05m share_614f share_614m  share_65plusf share_65plusm

eststo clear
local country `" "BurkinaFaso" "Ethiopia" "Ghana"  "Malawi" "Mali" "Niger" "Nigeria" "Tanzania" "Uganda" "'

local depvars poor_20 poor_40
foreach vars of local depvars {

foreach x of local country{
* OLS
eststo: quietly reg `vars' `wealth' `assets' `hh' i.hhsize_cat i.age_head_cat i.state  i.month if country == "`x'" , vce(cluster EA)
predict yhat
replace yhat_pmt_sh_`vars' = yhat if country == "`x'"
drop yhat
}

* probit
foreach x of local country{
eststo: quietly probit `vars' `wealth' `assets' `hh' i.hhsize_cat i.age_head_cat i.state  i.month if country == "`x'" , vce(cluster EA)
matrix r_`vars'_`x' = e(r2_p)
predict yhat
replace yhat_pmt_sh_pr_`vars' = yhat if country == "`x'"
drop yhat
}
}
esttab using "results", replace  b(%10.3f) star(* .10 ** .05 *** .01) label se(%9.3f) obslast longtable r2 csv keep(`wealth'  `hh' )
eststo clear

* retrieving pseudo r-squared
local country `" "BurkinaFaso" "Ethiopia" "Ghana"  "Malawi" "Mali" "Niger" "Nigeria" "Tanzania" "Uganda" "'
local depvars poor_20 poor_40
foreach vars of local depvars {
foreach x of local country{
matrix list r_`vars'_`x'
}
}


* Quantile regression
local nums `" "20" "40" "50" "'
foreach num of local nums {
gen yhat_pmt_sh_qr`num'_cons = .
label var yhat_pmt_sh_qr`num'_cons "Predicted values quantile reg short PMT"

local wealth  toilet_flush toilet_pit floor_finish wall_finish roof_finish fuel_elecgas fuel_charcoal
local hh  urban female_head edu_head_primary edu_head_secondary div_sep_head widow_head work_paid_head work_selfemp_nonf_head muslim christian share_05f share_05m share_614f share_614m  share_65plusf share_65plusm


local country `" "BurkinaFaso" "Ethiopia" "Ghana"  "Malawi" "Mali" "Niger" "Nigeria" "Tanzania" "Uganda" "'
foreach x of local country{
* might not be able to do clustered standard errors here.
eststo: quietly qreg consump `wealth'  `hh' i.hhsize_cat i.age_head_cat i.state i.month if country == "`x'" , vce(robust) q(0.`num')
predict yhat
replace yhat_pmt_sh_qr`num'_cons = yhat if country == "`x'"
drop yhat
}
}
esttab using "results", replace  b(%10.3f) star(* .10 ** .05 *** .01) label se(%9.3f) obslast longtable r2 csv keep(`wealth' `hh' )
eststo clear



* Basic weighted regression
save "Brown_Ravallion_vandeWalle_PMT.dta", replace
use "Brown_Ravallion_vandeWalle_PMT.dta", clear
eststo clear
local wealth  toilet_flush toilet_pit floor_finish wall_finish roof_finish fuel_elecgas fuel_charcoal
local hh  urban female_head edu_head_primary edu_head_secondary div_sep_head widow_head work_paid_head work_selfemp_nonf_head muslim christian share_05f share_05m share_614f share_614m  share_65plusf share_65plusm

local country `" "BurkinaFaso" "Ethiopia" "Ghana"  "Malawi" "Mali" "Niger" "Nigeria" "Tanzania" "Uganda" "'
foreach x of local country{

use "Brown_Ravallion_vandeWalle_PMT.dta", clear
drop if consumption_pctile > 20
eststo: quietly reg consump `wealth' `hh' i.hhsize_cat i.age_head_cat i.state  i.month if country == "`x'", vce(cluster EA)
use "Brown_Ravallion_vandeWalle_PMT.dta", clear
predict yhat_20_`x' if country == "`x'"
save "Brown_Ravallion_vandeWalle_PMT.dta", replace

drop if consumption_pctile > 40
eststo: quietly reg consump `wealth'  `hh' i.hhsize_cat i.age_head_cat i.state  i.month if country == "`x'", vce(cluster EA)
use "Brown_Ravallion_vandeWalle_PMT.dta", clear
predict yhat_40_`x' if country == "`x'"
save "Brown_Ravallion_vandeWalle_PMT.dta", replace

drop if consumption_pctile > 60
eststo: quietly reg consump `wealth' `hh' i.hhsize_cat i.age_head_cat i.state  i.month if country == "`x'", vce(cluster EA)
use "Brown_Ravallion_vandeWalle_PMT.dta", clear
predict yhat_60_`x' if country == "`x'"
save "Brown_Ravallion_vandeWalle_PMT.dta", replace
}
esttab using "results", replace  b(%10.3f) star(* .10 ** .05 *** .01) label se(%9.3f) obslast longtable r2 csv keep(`wealth'  `hh' )
eststo clear

gen yhat_w20_sh_cons = .
gen yhat_w40_sh_cons = .
gen yhat_w60_sh_cons = .

label var yhat_w20_sh_cons "Predicted values basic PMT, weighted bottom 20"
label var yhat_w40_sh_cons "Predicted values basic PMT, weighted bottom 40"
label var yhat_w60_sh_cons "Predicted values basic PMT, weighted bottom 60"


local country `" "BurkinaFaso" "Ethiopia" "Ghana"  "Malawi" "Mali" "Niger" "Nigeria" "Tanzania" "Uganda" "'
foreach x of local country{
replace yhat_w20_sh_cons = yhat_20_`x' if country == "`x'"
replace yhat_w40_sh_cons = yhat_40_`x' if country == "`x'"
replace yhat_w60_sh_cons = yhat_60_`x' if country == "`x'"
drop yhat_20_`x' yhat_40_`x' yhat_60_`x'
}


* Adult equivalent expenditures

* no adult equivalence for Burkina, Mali, Nigeria
tabstat real_consumption_ae, by(country)

gen consump_ae = log(real_consumption_ae)
label var consump_ae "Log real consumption adult equivalent"

gen yhat_pmt_sh_ae_consump = .
label var yhat_pmt_sh_ae_consump "Predicted values quantile reg short PMT"

local wealth  toilet_flush toilet_pit floor_finish wall_finish roof_finish fuel_elecgas fuel_charcoal
local hh  urban female_head edu_head_primary edu_head_secondary div_sep_head widow_head work_paid_head work_selfemp_nonf_head muslim christian share_05f share_05m share_614f share_614m  share_65plusf share_65plusm

eststo clear
local country `" "Ethiopia" "Ghana"  "Malawi" "Niger" "Tanzania" "Uganda" "'
foreach x of local country{
* might not be able to do clustered standard errors here.
eststo: quietly reg consump_ae `wealth'  `hh' i.hhsize_cat i.age_head_cat i.state i.month if country == "`x'" , vce(cluster EA)
predict yhat
replace yhat_pmt_sh_ae_consump = yhat if country == "`x'"
drop yhat
}
esttab using "results", replace   b(%10.3f) star(* .10 ** .05 *** .01) label se(%9.3f) obslast longtable r2 csv keep(`wealth' `hh' )
eststo clear
drop consump_ae

* Separate for urban rural
gen yhat_pmt_sh_urb_consump = .
label var yhat_pmt_sh_urb_consump "Predicted values urban/rural short PMT"

local wealth  toilet_flush toilet_pit floor_finish wall_finish roof_finish fuel_elecgas fuel_charcoal
local hh  urban female_head edu_head_primary edu_head_secondary div_sep_head widow_head work_paid_head work_selfemp_nonf_head muslim christian share_05f share_05m share_614f share_614m  share_65plusf share_65plusm

eststo clear
local country `" "BurkinaFaso" "Ethiopia" "Ghana"  "Malawi" "Mali" "Niger" "Nigeria" "Tanzania" "Uganda" "'
foreach num of numlist 0/1 {
foreach x of local country{
eststo: quietly reg consump `wealth'  `hh' i.hhsize_cat i.age_head_cat i.state i.month if country == "`x'" & urban == `num' , vce(cluster EA)
predict yhat
replace yhat_pmt_sh_urb_consump = yhat if country == "`x'" & urban == `num'
drop yhat
}
}
esttab using "results", replace   b(%10.3f) star(* .10 ** .05 *** .01) label se(%9.3f) obslast longtable r2 csv keep(`wealth'  `hh' )
eststo clear


* extended regression
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
local assets electric radio telev fridge bicycle motorbike car telephone mobile_phone computer video stove_any sew_machine aircon iron satelite generator own_house
local hh urban  female_head edu_head_primary edu_head_secondary max_edu_primary max_edu_secondary div_sep_head widow_head nevermar_head  female_head_widow female_head_div female_head_nevermar work_paid_head work_selfemp_nonf_head work_daily_head muslim christian  share_05f share_05m share_614f share_614m  share_65plusf share_65plusm  share_widow share_disabledm share_disabledf share_orphanm share_orphanf

eststo clear
local country `" "BurkinaFaso" "Ethiopia" "Ghana"  "Malawi" "Mali" "Niger" "Nigeria" "Tanzania" "Uganda" "'
foreach x of local country{
eststo: quietly reg consump `wealth' `assets' `hh' i.hhsize_cat i.age_head_cat i.state  i.month if country == "`x'" , vce(cluster EA)
predict yhat
replace yhat_pmt_ext_consump = yhat if country == "`x'"
drop yhat
}
esttab using "results", replace  b(%10.3f) star(* .10 ** .05 *** .01) label se(%9.3f) obslast longtable r2 csv keep(`wealth' `assets' `hh' )
eststo clear


* Quantile regression for extended model
local nums `" "20" "40" "50" "'
foreach num of local nums {
gen yhat_pmt_ex_qr`num'_cons = .
label var yhat_pmt_ex_qr`num'_cons "Predicted values quantile reg extended PMT"

local wealth water_piped water_well toilet_flush toilet_pit  floor_finish  wall_finish  roof_finish members_per_room kitchen_room fuel_elecgas fuel_charcoal
local assets electric radio telev fridge bicycle motorbike car telephone mobile_phone computer video stove_any sew_machine aircon iron satelite generator own_house
local hh urban  female_head edu_head_primary edu_head_secondary max_edu_primary max_edu_secondary div_sep_head widow_head nevermar_head  female_head_widow female_head_div female_head_nevermar work_paid_head work_selfemp_nonf_head work_daily_head muslim christian  share_05f share_05m share_614f share_614m  share_65plusf share_65plusm  share_widow share_disabledm share_disabledf share_orphanm share_orphanf

local country `" "BurkinaFaso" "Ethiopia" "Ghana"  "Malawi" "Mali" "Niger" "Nigeria" "Tanzania" "Uganda" "'
foreach x of local country{
eststo: quietly qreg consump `wealth' `assets' `hh'  i.hhsize_cat i.age_head_cat i.state i.month if country == "`x'" , vce(robust) q(0.`num')
predict yhat
replace yhat_pmt_ex_qr`num'_cons = yhat if country == "`x'"
drop yhat
}
}
esttab using "results", replace   b(%10.3f) star(* .10 ** .05 *** .01) label se(%9.3f) obslast longtable r2 csv keep(`wealth' `hh' )
eststo clear


* Weighted regressions for extended model
* I.e. only those with consumption percentiles below 20 or 40
save "Brown_Ravallion_vandeWalle_PMT.dta", replace
use "Brown_Ravallion_vandeWalle_PMT.dta", clear

local wealth water_piped water_well toilet_flush toilet_pit  floor_finish  wall_finish  roof_finish members_per_room kitchen_room fuel_elecgas fuel_charcoal
local assets electric radio telev fridge bicycle motorbike car telephone mobile_phone computer video stove_any sew_machine aircon iron satelite generator own_house
local hh urban  female_head edu_head_primary edu_head_secondary max_edu_primary max_edu_secondary div_sep_head widow_head nevermar_head  female_head_widow female_head_div female_head_nevermar work_paid_head work_selfemp_nonf_head work_daily_head muslim christian  share_05f share_05m share_614f share_614m  share_65plusf share_65plusm  share_widow share_disabledm share_disabledf share_orphanm share_orphanf

local country `" "BurkinaFaso" "Ethiopia" "Ghana"  "Malawi" "Mali" "Niger" "Nigeria" "Tanzania" "Uganda" "'
foreach x of local country{

use "Brown_Ravallion_vandeWalle_PMT.dta", clear
drop if consumption_pctile > 20
eststo: quietly reg consump `wealth' `assets' `hh' i.hhsize_cat i.age_head_cat i.state  i.month if country == "`x'", vce(cluster EA)
use "Brown_Ravallion_vandeWalle_PMT.dta", clear
predict yhat_20_`x' if country == "`x'"
save "Brown_Ravallion_vandeWalle_PMT.dta", replace

drop if consumption_pctile > 40
eststo: quietly reg consump `wealth' `assets'  `hh' i.hhsize_cat i.age_head_cat i.state  i.month if country == "`x'", vce(cluster EA)
use "Brown_Ravallion_vandeWalle_PMT.dta", clear
predict yhat_40_`x' if country == "`x'"
save "Brown_Ravallion_vandeWalle_PMT.dta", replace

drop if consumption_pctile > 60
eststo: quietly reg consump `wealth' `assets'  `hh' i.hhsize_cat i.age_head_cat i.state  i.month if country == "`x'", vce(cluster EA)
use "Brown_Ravallion_vandeWalle_PMT.dta", clear
predict yhat_60_`x' if country == "`x'"
save "Brown_Ravallion_vandeWalle_PMT.dta", replace
}
esttab using "results", replace   b(%10.3f) star(* .10 ** .05 *** .01) label se(%9.3f) obslast longtable r2 csv keep(`wealth' `assets'  `hh' )
eststo clear


gen yhat_w20_cons = .
gen yhat_w40_cons = .
gen yhat_w60_cons = .

label var yhat_w20_cons "Predicted values extended PMT, weighted bottom 20"
label var yhat_w40_cons "Predicted values extended PMT, weighted bottom 40"
label var yhat_w60_cons "Predicted values extended PMT, weighted bottom 60"


local country `" "BurkinaFaso" "Ethiopia" "Ghana"  "Malawi" "Mali" "Niger" "Nigeria" "Tanzania" "Uganda" "'
foreach x of local country{
replace yhat_w20_cons = yhat_20_`x' if country == "`x'"
replace yhat_w40_cons = yhat_40_`x' if country == "`x'"
replace yhat_w60_cons = yhat_60_`x' if country == "`x'"
drop yhat_20_`x' yhat_40_`x' yhat_60_`x'
}


*  Stepwise regression

gen age_head_sq = age_head^2
label var age_head_sq "Age of head squared"
gen loghhsize = log(hhsize)
label var loghhsize "Household size logged"

* Using p = 0.01
gen yhat_step_consump = .
label var yhat_step_consump "Predicted values stepwise p(0.01)"

* added in hhsize and age head as continuous variables. Dropped month.
local wealth water_piped water_well toilet_flush toilet_pit  floor_finish  wall_finish  roof_finish members_per_room kitchen_room fuel_elecgas fuel_charcoal
local assets electric radio telev fridge bicycle motorbike car mobile_phone computer video stove_any sew_machine aircon iron satelite generator own_house
local hh urban loghhsize age_head age_head_sq female_head edu_head_primary edu_head_secondary max_edu_primary max_edu_secondary div_sep_head widow_head nevermar_head  female_head_widow female_head_div female_head_nevermar work_paid_head work_selfemp_nonf_head work_daily_head muslim christian  share_05f share_05m share_614f share_614m  share_65plusf share_65plusm  share_widow share_disabledm share_disabledf share_orphanm share_orphanf

eststo clear
local country `" "BurkinaFaso" "Ethiopia" "Ghana"  "Malawi" "Mali" "Niger" "Nigeria" "Tanzania" "Uganda" "'
foreach x of local country{
stepwise, pr(.01): quietly reg consump `wealth' `assets' `hh'  if country == "`x'" , vce(cluster EA)
eststo
predict yhat
replace yhat_step_consump = yhat if country == "`x'"
drop yhat
}
esttab using "results", replace  b(%10.3f) star(* .10 ** .05 *** .01) label se(%9.3f) obslast longtable r2 csv
eststo clear



*** Adding in additional measures e.g. food security and shocks.
local fs shock_death_mainmem shock_death_othermem shock_jobloss shock_conflict shock_drought shock_flood shock_livestock fs_notenough fs_prefer  fs_portion fs_meals fs_consump fs_borrow
foreach var of local fs {
replace `var' = 0 if missing(`var') & country != "Ghana" | country != "Mali"
}

local fs shock_death_mainmem shock_death_othermem shock_jobloss shock_conflict shock_drought shock_flood shock_livestock fs_notenough fs_prefer  fs_portion fs_meals fs_consump fs_borrow
tabstat `fs' [aweight=hhweight] , by(country) f(%9.3g)

gen yhat_pmt_fs_consump = .
label var yhat_pmt_fs_consump "Predicted values PMT food security"

local wealth water_piped water_well toilet_flush toilet_pit  floor_finish  wall_finish  roof_finish members_per_room kitchen_room fuel_elecgas fuel_charcoal
local assets electric radio telev fridge bicycle motorbike car telephone mobile_phone computer video stove_any sew_machine aircon iron satelite generator own_house
local hh urban  female_head edu_head_primary edu_head_secondary max_edu_primary max_edu_secondary div_sep_head widow_head nevermar_head  female_head_widow female_head_div female_head_nevermar work_paid_head work_selfemp_nonf_head work_daily_head muslim christian  share_05f share_05m share_614f share_614m  share_65plusf share_65plusm  share_widow share_disabledm share_disabledf share_orphanm share_orphanf
local fs shock_death_mainmem shock_death_othermem shock_jobloss shock_conflict shock_drought shock_flood shock_livestock fs_notenough fs_prefer  fs_portion fs_meals fs_consump fs_borrow
eststo clear
local country `" "BurkinaFaso" "Ethiopia"  "Malawi" "Niger" "Nigeria" "Tanzania" "Uganda" "'
foreach x of local country{
eststo: quietly reg consump `wealth' `assets' `hh' `fs' i.hhsize_cat i.age_head_cat i.state  i.month if country == "`x'" , vce(cluster EA)
predict yhat
replace yhat_pmt_fs_consump = yhat if country == "`x'"
drop yhat
}
esttab using "results", replace  b(%10.3f) star(* .10 ** .05 *** .01) label se(%9.3f) obslast longtable r2 csv keep(`wealth' `assets' `hh' `fs' )
eststo clear


* adding in community level variables
bys country EA: gen nval = _n == 1

local vars dist_primary dist_secondary dist_health dist_hosp dist_bank
foreach x of local vars {
replace `x' = `x' / 1000
}

egen dist_school = rowmin(dist_primary dist_secondary)
egen dist_healthcenter = rowmin(dist_hosp dist_health)
label var dist_school "Distance to nearest school"
label var dist_healthcenter "Distance to nearest health center"

tabstat dist_road, by(country)

local com dist_road dist_popcenter dist_market dist_borderpost dist_school dist_healthcenter af_bio_12 af_bio_13 fsrad3_agpct
foreach var of local com {
replace `var' = 0 if missing(`var')
}

local com dist_road dist_popcenter dist_market dist_borderpost dist_school dist_healthcenter af_bio_12 af_bio_13 fsrad3_agpct
tabstat  `com'  [aweight=hhweight], by(country) f(%9.3g)

gen yhat_pmt_com_consump = .
label var yhat_pmt_com_consump "Predicted values community"

local wealth water_piped water_well toilet_flush toilet_pit  floor_finish  wall_finish  roof_finish members_per_room kitchen_room fuel_elecgas fuel_charcoal
local assets electric radio telev fridge bicycle motorbike car telephone mobile_phone computer video stove_any sew_machine aircon iron satelite generator own_house
local hh urban  female_head edu_head_primary edu_head_secondary max_edu_primary max_edu_secondary div_sep_head widow_head nevermar_head  female_head_widow female_head_div female_head_nevermar work_paid_head work_selfemp_nonf_head work_daily_head muslim christian  share_05f share_05m share_614f share_614m  share_65plusf share_65plusm  share_widow share_disabledm share_disabledf share_orphanm share_orphanf
local com dist_road dist_popcenter dist_market dist_borderpost dist_school dist_healthcenter af_bio_12 af_bio_13 fsrad3_agpct

eststo clear
local country `"  "Ethiopia"  "Malawi" "Niger" "Nigeria" "Tanzania" "Uganda" "'
foreach x of local country{
eststo: quietly reg consump `wealth' `assets' `hh' `food' `com' i.hhsize_cat i.age_head_cat i.state  i.month if country == "`x'"
predict yhat
replace yhat_pmt_com_consump = yhat if country == "`x'"
drop yhat
}
esttab using "results", replace  b(%10.3f) star(* .10 ** .05 *** .01) label se(%9.3f) obslast longtable r2 csv keep(`wealth' `assets' `hh' `com' )
eststo clear


** PMT for regional Burkina only - testing to see whether just running this on poor regions is better
gen yhat_pmt_sh_cons_burk = .
label var yhat_pmt_sh_cons_burk "Predicted values short PMT Burkina"

local wealth  toilet_flush toilet_pit floor_finish wall_finish roof_finish fuel_elecgas fuel_charcoal
local hh  urban female_head edu_head_primary edu_head_secondary div_sep_head widow_head work_paid_head work_selfemp_nonf_head muslim christian share_05f share_05m share_614f share_614m  share_65plusf share_65plusm

eststo clear

eststo: quietly reg consump `wealth'  `hh' i.hhsize_cat i.age_head_cat i.state i.month if country == "BurkinaFaso" & ( region == 2 | region == 9) , vce(cluster EA)
predict yhat
replace yhat_pmt_sh_cons_burk = yhat if country == "BurkinaFaso" & (region == 2 | region == 9)
drop yhat

esttab using "results", replace   b(%10.3f) star(* .10 ** .05 *** .01) label se(%9.3f) obslast longtable r2 csv keep(`wealth'  `hh' )
eststo clear

save "Brown_Ravallion_vandeWalle_PMT.dta", replace


/*********************** Generating Poverty Indicators ****************************/

/* Figure 1 */
** graphing consumption and predicted consumption

* short PMT
local country `" "BurkinaFaso" "Ethiopia" "Ghana"  "Malawi" "Mali" "Niger" "Nigeria" "Tanzania" "Uganda" "'
foreach x of local country{
sum pov_line_20 if country == "`x'", meanonly
local b = r(mean)
graph twoway scatter consump yhat_pmt_sh_consump if country == "`x'", xline(`b') yline(`b') msize(tiny) xtitle("Predicted log consumption") ytitle("Actual log consumption") title("`x'") graphregion(color(white)) bgcolor(white)
graph save pmt_sh_`x', replace
graph export pmt_sh_`x'.png, replace
}


/* Figure 2 */
** graphing residuals

gen resid = .

local wealth  toilet_flush toilet_pit floor_finish wall_finish roof_finish fuel_elecgas fuel_charcoal
local hh  urban female_head edu_head_primary edu_head_secondary div_sep_head widow_head work_paid_head work_selfemp_nonf_head muslim christian share_05f share_05m share_614f share_614m  share_65plusf share_65plusm

local country `" "BurkinaFaso" "Ethiopia" "Ghana"  "Malawi" "Mali" "Niger" "Nigeria" "Tanzania" "Uganda" "'
foreach x of local country{
quietly reg consump `wealth'  `hh' i.hhsize_cat i.age_head_cat i.state i.month if country == "`x'" , vce(cluster EA)
predict res1, res
replace resid = res1 if country == "`x'"
drop res1
}

local country `" "BurkinaFaso" "Ethiopia" "Ghana"  "Malawi" "Mali" "Niger" "Nigeria" "Tanzania" "Uganda" "'
foreach x of local country{
sum pov_line_20 if country == "`x'", meanonly
local b = r(mean)
graph twoway scatter resid consump if country == "`x'" & consumption_pctile <= 99,  xline(`b') yline(0, lcolor(black) lpattern(dash)) msize(tiny) xtitle("Actual log consumption") ytitle("Residuals") title("`x'") graphregion(color(white)) bgcolor(white)
graph save resid_`x', replace
graph export resid_`x'.png, replace
}


**** Assigning households as predicted poor
  local vars yhat_pmt_sh_consump yhat_pmt_sh_poor_20 yhat_pmt_sh_pr_poor_20 yhat_pmt_sh_poor_40 yhat_pmt_sh_pr_poor_40 yhat_pmt_sh_qr20_cons yhat_pmt_sh_qr40_cons yhat_pmt_sh_qr50_cons yhat_w20_sh_cons yhat_w40_sh_cons yhat_w60_sh_cons yhat_pmt_sh_ae_consump yhat_pmt_sh_urb_consump yhat_pmt_ext_consump yhat_pmt_ex_qr20_cons yhat_pmt_ex_qr40_cons yhat_pmt_ex_qr50_cons yhat_w20_cons yhat_w40_cons yhat_w60_cons yhat_pmt_fs_consump yhat_step_consump yhat_pmt_com_consump yhat_pmt_sh_cons_burk
foreach var of local vars {
gen pr20_`var' = 0 if `var' != .
replace pr20_`var' = 1 if `var' <= pov_line_20 & `var' != .
gen pr40_`var' = 0 if `var' != .
replace pr40_`var' = 1 if `var' <= pov_line_40 & `var' != .
}


/** Table 3 ***/
** Looking at total predicted poor
tabstat pr20_yhat_pmt_sh_consump pr20_yhat_pmt_ext_consump  pr40_yhat_pmt_sh_consump pr40_yhat_pmt_ext_consump [aweight=hhweight], by(country) f(%9.3g)


**** Poverty rate - need to generate mean values here.

* cutoff point - actual poverty rate. Should be 20 and 40
egen poor_20_mean = wtmean(poor_20), by(country) weight(hhweight)
egen poor_40_mean = wtmean(poor_40), by(country) weight(hhweight)
bys country: sum poor_20_mean poor_40_mean


* percentiles - all regressions with all countries
local vars yhat_pmt_sh_consump yhat_pmt_sh_poor_20 yhat_pmt_sh_pr_poor_20 yhat_pmt_sh_poor_40 yhat_pmt_sh_pr_poor_40 yhat_pmt_sh_qr20_cons yhat_pmt_sh_qr40_cons yhat_pmt_sh_qr50_cons yhat_w20_sh_cons yhat_w40_sh_cons yhat_w60_sh_cons yhat_pmt_sh_ae_consump yhat_pmt_sh_urb_consump yhat_pmt_ext_consump yhat_w20_cons yhat_w40_cons yhat_w60_cons yhat_pmt_fs_consump yhat_step_consump yhat_pmt_com_consump //yhat_pmt_sh_cons_burk
bys country: sum `vars'

local vars yhat_pmt_sh_consump yhat_pmt_sh_poor_20 yhat_pmt_sh_pr_poor_20 yhat_pmt_sh_poor_40 yhat_pmt_sh_pr_poor_40 yhat_pmt_sh_qr20_cons yhat_pmt_sh_qr40_cons yhat_pmt_sh_qr50_cons yhat_w20_sh_cons yhat_w40_sh_cons yhat_w60_sh_cons yhat_pmt_sh_urb_consump yhat_pmt_ext_consump yhat_pmt_ex_qr20_cons yhat_pmt_ex_qr40_cons yhat_pmt_ex_qr50_cons yhat_w20_cons yhat_w40_cons yhat_w60_cons yhat_step_consump

foreach var of local vars {
gen pc_`var' = .

local country `" "BurkinaFaso" "Ethiopia" "Ghana"  "Malawi" "Mali" "Niger" "Nigeria" "Tanzania" "Uganda" "'
foreach x of local country{
xtile percentile_`x' = `var'  if country == "`x'" & `var' != . [aweight=hhweight] , n(100)
replace  pc_`var' = percentile_`x' if country == "`x'" & `var' != .,
drop percentile_`x'
}

replace pc_`var' = pc_`var' / 100
}

* adult equivalence
gen pc_yhat_pmt_sh_ae_consump = .

local country `" "Ethiopia" "Ghana"  "Malawi" "Niger" "Tanzania" "Uganda" "'
foreach x of local country{
xtile percentile_`x' = yhat_pmt_sh_ae_consump  if country == "`x'" & yhat_pmt_sh_ae_consump != . [aweight=hhweight]  , n(100)
replace  pc_yhat_pmt_sh_ae_consump = percentile_`x' if country == "`x'" & yhat_pmt_sh_ae_consump != . ,
drop percentile_`x'
}
replace pc_yhat_pmt_sh_ae_consump = pc_yhat_pmt_sh_ae_consump / 100

* food security
gen pc_yhat_pmt_fs_consump = .

local country `" "BurkinaFaso" "Ethiopia"  "Malawi" "Niger" "Nigeria" "Tanzania" "Uganda"  "'
foreach x of local country{
xtile percentile_`x' = yhat_pmt_fs_consump  if country == "`x'" & yhat_pmt_fs_consump != . [aweight=hhweight] , n(100)
replace  pc_yhat_pmt_fs_consump = percentile_`x' if country == "`x'" & yhat_pmt_fs_consump != . ,
drop percentile_`x'
}
replace pc_yhat_pmt_fs_consump = pc_yhat_pmt_fs_consump / 100

* community vars
gen pc_yhat_pmt_com_consump = .

local country `"  "Ethiopia"  "Malawi" "Niger" "Nigeria" "Tanzania" "Uganda" "'
foreach x of local country{
xtile percentile_`x' = yhat_pmt_com_consump  if country == "`x'" & yhat_pmt_com_consump != . [aweight=hhweight] , n(100)
replace  pc_yhat_pmt_com_consump = percentile_`x' if country == "`x'" & yhat_pmt_com_consump != . ,
drop percentile_`x'
}
replace pc_yhat_pmt_com_consump = pc_yhat_pmt_com_consump / 100

* burkina
gen pc_yhat_pmt_sh_cons_burk = .
local country `" "BurkinaFaso" "'
foreach x of local country{
xtile percentile_`x' = yhat_pmt_sh_cons_burk  if country == "`x'" & yhat_pmt_sh_cons_burk != . [aweight=hhweight]  , n(100)
replace  pc_yhat_pmt_sh_cons_burk = percentile_`x' if country == "`x'" & yhat_pmt_sh_cons_burk != . ,
drop percentile_`x'
}
replace pc_yhat_pmt_sh_cons_burk = pc_yhat_pmt_sh_cons_burk / 100



* generating indicator variable for predicted poor or not predicted poor
local vars yhat_pmt_sh_consump yhat_pmt_sh_poor_20 yhat_pmt_sh_pr_poor_20 yhat_pmt_sh_poor_40 yhat_pmt_sh_pr_poor_40 yhat_pmt_sh_qr20_cons yhat_pmt_sh_qr40_cons yhat_pmt_sh_qr50_cons yhat_w20_sh_cons yhat_w40_sh_cons yhat_w60_sh_cons yhat_pmt_sh_ae_consump yhat_pmt_sh_urb_consump yhat_pmt_ext_consump yhat_pmt_ex_qr20_cons yhat_pmt_ex_qr40_cons yhat_pmt_ex_qr50_cons yhat_w20_cons yhat_w40_cons yhat_w60_cons yhat_pmt_fs_consump yhat_step_consump yhat_pmt_com_consump yhat_pmt_sh_cons_burk

foreach var of local vars {
gen po20_`var' = 0 if `var' != .
replace po20_`var' = 1 if pc_`var' <= poor_20_mean & `var' != .
gen po40_`var' = 0 if `var' != .
replace po40_`var' = 1 if pc_`var' <= poor_40_mean & `var' != .
drop pc_`var'
}

* checking - mean should be equal to 0.2 or 0.4
tabstat po20_yhat_pmt_sh_consump po40_yhat_pmt_sh_consump po20_yhat_pmt_sh_poor_20 po40_yhat_pmt_sh_poor_20 po20_yhat_pmt_sh_pr_poor_20 po40_yhat_pmt_sh_pr_poor_20 po20_yhat_pmt_sh_poor_40 po40_yhat_pmt_sh_poor_40 po20_yhat_pmt_sh_pr_poor_40 po40_yhat_pmt_sh_pr_poor_40  po20_yhat_w20_sh_cons po40_yhat_w20_sh_cons po20_yhat_w40_sh_cons po40_yhat_w40_sh_cons po20_yhat_w60_sh_cons po40_yhat_w60_sh_cons po20_yhat_pmt_sh_ae_consump po40_yhat_pmt_sh_ae_consump po20_yhat_pmt_sh_urb_consump po40_yhat_pmt_sh_urb_consump po20_yhat_pmt_ext_consump po40_yhat_pmt_ext_consump po20_yhat_w20_cons po40_yhat_w20_cons po20_yhat_w40_cons po40_yhat_w40_cons po20_yhat_w60_cons po40_yhat_w60_cons po20_yhat_pmt_fs_consump po40_yhat_pmt_fs_consump po20_yhat_step_consump po40_yhat_step_consump po20_yhat_pmt_com_consump po40_yhat_pmt_com_consump [aweight=hhweight], by(country)


************* Inclusion and exclusion errors *****************************

drop nval
bys country: gen nval = _n == 1

local vars yhat_pmt_sh_consump yhat_pmt_sh_poor_20 yhat_pmt_sh_pr_poor_20 yhat_pmt_sh_poor_40 yhat_pmt_sh_pr_poor_40 yhat_pmt_sh_qr20_cons yhat_pmt_sh_qr40_cons yhat_pmt_sh_qr50_cons yhat_w20_sh_cons yhat_w40_sh_cons yhat_w60_sh_cons yhat_pmt_sh_ae_consump yhat_pmt_sh_urb_consump yhat_pmt_ext_consump yhat_pmt_ex_qr20_cons yhat_pmt_ex_qr40_cons yhat_pmt_ex_qr50_cons yhat_w20_cons yhat_w40_cons yhat_w60_cons yhat_pmt_fs_consump yhat_step_consump yhat_pmt_com_consump yhat_pmt_sh_cons_burk
local num `" "20" "40" "'

foreach var of local vars {
foreach x of local num {
*** line
* correctly included
gen ci1_pr`x'_`var' = 0 if `var' != .
replace ci1_pr`x'_`var' = 1 if poor_`x' == 1 & pr`x'_`var' == 1
* incorrectly included
gen ie1_pr`x'_`var' = 0 if `var' != .
replace ie1_pr`x'_`var' = 1 if poor_`x' == 0 & pr`x'_`var' == 1
* incorrectly excluded
gen ee1_pr`x'_`var' = 0 if `var' != .
replace ee1_pr`x'_`var' = 1 if poor_`x' == 1 & pr`x'_`var' == 0

* correctly included as share of total pop
egen ci_pr`x'_`var' = wtmean(ci1_pr`x'_`var') if `var' != ., by(country) weight(hhweight)
drop ci1_pr`x'_`var'

* incorrectly included as share of predicted poor
egen ie_pr`x'_`var' = wtmean(ie1_pr`x'_`var') if pr`x'_`var' == 1  & `var' != ., by(country) weight(hhweight)
drop ie1_pr`x'_`var'
* incorrectly excluded as share of actual poor
egen ee_pr`x'_`var' = wtmean(ee1_pr`x'_`var') if poor_`x' == 1  & `var' != ., by(country)  weight(hhweight)
drop ee1_pr`x'_`var'

*** rate
gen ci1_po`x'_`var' = 0 if `var' != .
replace ci1_po`x'_`var' = 1 if poor_`x' == 1 & po`x'_`var' == 1
gen ie1_po`x'_`var' = 0 if `var' != .
replace ie1_po`x'_`var' = 1 if poor_`x' == 0 & po`x'_`var' == 1
gen ee1_po`x'_`var' = 0 if `var' != .
replace ee1_po`x'_`var' = 1 if poor_`x' == 1 & po`x'_`var' == 0

* share of total pop
egen ci_po`x'_`var' = wtmean(ci1_po`x'_`var') if `var' != ., by(country)  weight(hhweight)
drop ci1_po`x'_`var'

* share of predicted poor
egen ie_po`x'_`var' = wtmean(ie1_po`x'_`var') if po`x'_`var' == 1  & `var' != ., by(country)  weight(hhweight)
drop ie1_po`x'_`var'
* share of actual poor
egen ee_po`x'_`var' = wtmean(ee1_po`x'_`var') if poor_`x' == 1  & `var' != ., by(country)  weight(hhweight)
drop ee1_po`x'_`var'
}
}

/**** Tables - Inclusion exclusion errors ***/

/** Table 4 **/
local vars yhat_pmt_sh_consump
foreach var of local vars {
tabstat  ie_pr20_`var' ee_pr20_`var'  ie_pr40_`var' ee_pr40_`var' ie_po20_`var' ee_po20_`var'  ie_po40_`var' ee_po40_`var'  [aweight=hhweight], by(country) f(%9.3g)
}

/** Table 5 **/
local vars yhat_pmt_sh_qr20_cons yhat_pmt_sh_qr40_cons
foreach var of local vars {
tabstat  ie_pr20_`var' ee_pr20_`var'  ie_pr40_`var' ee_pr40_`var' ie_po20_`var' ee_po20_`var'  ie_po40_`var' ee_po40_`var'  [aweight=hhweight], by(country) f(%9.3g)
}

/** Table 6 **/
local vars yhat_w20_sh_cons yhat_w40_sh_cons
foreach var of local vars {
tabstat  ie_pr20_`var' ee_pr20_`var'  ie_pr40_`var' ee_pr40_`var' ie_po20_`var' ee_po20_`var'  ie_po40_`var' ee_po40_`var'  [aweight=hhweight], by(country) f(%9.3g)
}

/** Table 7 **/
local vars yhat_pmt_ext_consump
foreach var of local vars {
tabstat  ie_pr20_`var' ee_pr20_`var'  ie_pr40_`var' ee_pr40_`var' ie_po20_`var' ee_po20_`var'  ie_po40_`var' ee_po40_`var'  [aweight=hhweight], by(country) f(%9.3g)
}

/** text numbers for Burkina **/
local vars yhat_pmt_sh_cons_burk
foreach var of local vars {
tabstat  ie_pr20_`var' ee_pr20_`var'  ie_pr40_`var' ee_pr40_`var' ie_po20_`var' ee_po20_`var'  ie_po40_`var' ee_po40_`var'  [aweight=hhweight] if country == "BurkinaFaso", f(%9.3g)
}

** Tables in appendix
local vars yhat_pmt_sh_ae_consump yhat_w40_sh_cons yhat_w60_sh_cons yhat_pmt_sh_urb_consump yhat_pmt_ex_qr20_cons yhat_pmt_ex_qr40_cons yhat_pmt_ex_qr50_cons yhat_w20_cons yhat_w40_cons yhat_w60_cons yhat_pmt_fs_consump yhat_step_consump yhat_pmt_com_consump yhat_pmt_sh_cons_burk
foreach var of local vars {
tabstat  ie_pr20_`var' ee_pr20_`var'  ie_pr40_`var' ee_pr40_`var' ie_po20_`var' ee_po20_`var'  ie_po40_`var' ee_po40_`var'  [aweight=hhweight], by(country) f(%9.3g)
}

* indicator variables
local vars yhat_pmt_sh_poor_20 yhat_pmt_sh_pr_poor_20 yhat_pmt_sh_poor_40 yhat_pmt_sh_pr_poor_40
foreach var of local vars {
tabstat  ie_po20_`var' ee_po20_`var'  ie_po40_`var' ee_po40_`var' [aweight=hhweight], by(country) f(%9.3g)
}


**** Targeting Differential

drop nval
bys country: gen nval = _n == 1

* create H hat - average predicted poor
local vars yhat_pmt_sh_consump  yhat_pmt_sh_qr20_cons yhat_w20_sh_cons yhat_w40_sh_cons yhat_pmt_sh_urb_consump yhat_pmt_ext_consump yhat_pmt_ex_qr20_cons yhat_w20_cons yhat_w40_cons yhat_step_consump yhat_pmt_fs_consump  yhat_pmt_com_consump
foreach var of local vars {
egen pr20_`var'_m = wtmean(pr20_`var') , by(country) weight(hhweight)
}
tabstat pr20_yhat_pmt_sh_consump_m pr20_yhat_pmt_sh_qr20_cons_m pr20_yhat_w20_sh_cons_m pr20_yhat_w40_sh_cons_m pr20_yhat_pmt_sh_urb_consump_m pr20_yhat_pmt_ext_consump_m pr20_yhat_pmt_ex_qr20_cons_m pr20_yhat_w20_cons_m pr20_yhat_w40_cons_m pr20_yhat_step_consump_m pr20_yhat_pmt_fs_consump_m pr20_yhat_pmt_com_consump_m  [aweight=hhweight] , by(country) f(%9.3g)

* create max var
local vars yhat_pmt_sh_consump  yhat_pmt_sh_qr20_cons yhat_w20_sh_cons yhat_w40_sh_cons yhat_pmt_sh_urb_consump yhat_pmt_ext_consump yhat_pmt_ex_qr20_cons yhat_w20_cons yhat_w40_cons yhat_step_consump yhat_pmt_fs_consump  yhat_pmt_com_consump
foreach var of local vars {
egen ee_pr20_`var'1 = max(ee_pr20_`var'), by(country)
}

local vars yhat_pmt_sh_consump  yhat_pmt_sh_qr20_cons yhat_w20_sh_cons yhat_w40_sh_cons yhat_pmt_sh_urb_consump yhat_pmt_ext_consump yhat_pmt_ex_qr20_cons yhat_w20_cons yhat_w40_cons yhat_step_consump yhat_pmt_fs_consump  yhat_pmt_com_consump
foreach var of local vars {
gen td_pr20_`var' = (1-pr20_`var'_m-ee_pr20_`var'1)/0.8
}

/* Table 8 */
tabstat  td_pr20_yhat_pmt_sh_consump td_pr20_yhat_pmt_sh_qr20_cons  td_pr20_yhat_w20_sh_cons td_pr20_yhat_w40_sh_cons  td_pr20_yhat_pmt_sh_urb_consump td_pr20_yhat_pmt_ext_consump td_pr20_yhat_pmt_ex_qr20_cons td_pr20_yhat_w20_cons td_pr20_yhat_w40_cons td_pr20_yhat_step_consump td_pr20_yhat_pmt_fs_consump  td_pr20_yhat_pmt_com_consump if nval == 1, by(country) f(%9.3g)


save "Brown_Ravallion_vandeWalle_PMT.dta", replace



******** Counterfactual policies using the headcount index and poverty line. *******

use "Brown_Ravallion_vandeWalle_PMT.dta", clear


** all of these are for the 20th percentile
** this needs to be converted back to real terms for the indices

* poverty line in linear terms
gen pov = real_consumption_pc if consumption_pctile == 20
bys country: egen pov_line_20_lin = max(pov)
drop pov
label var pov_line_20_lin "Poverty line 20th percentile linear"

* poverty gap
gen actual_gap_pc_lin = pov_line_20_lin - real_consumption_pc if poor_20 == 1
replace actual_gap_pc_lin = 0 if missing(actual_gap_pc_lin)
label var actual_gap_pc_lin "Actual household poverty gap per capita linear"

* total household gap
gen actual_gap_lin = actual_gap_pc_lin*hhsize
label var actual_gap_lin "Actual household poverty gap"

egen agg_actual_gap_lin = sum(actual_gap_lin), by(country)
label var agg_actual_gap_lin "Aggregate actual poverty gap"


**** Transfers based on PMT

local vars yhat_pmt_sh_consump yhat_pmt_sh_qr20_cons yhat_w20_sh_cons yhat_w40_sh_cons yhat_pmt_sh_urb_consump yhat_pmt_ext_consump yhat_pmt_ex_qr20_cons yhat_w20_cons yhat_w40_cons  yhat_step_consump yhat_pmt_fs_consump yhat_pmt_com_consump
foreach var of local vars {

* total number of predicted poor
egen total_poor = sum(hhsize) if pr20_`var' == 1, by(country)
* transfer amount
gen trans = agg_actual_gap_lin / total_poor
* total transfer per household
gen tr_`var' = trans*hhsize

* new consumption - transfer goes only to those predicted poor
gen cons_`var' = real_consumption_pc
replace cons_`var' = real_consumption_pc + tr_`var'/hhsize if pr20_`var' == 1

* head count index
gen hc_`var' = 0 if `var' != .
replace hc_`var' = 1 if cons_`var' <= pov_line_20_lin & `var' != .

* poverty gap index - gap is only calculated for those below it
gen pg_`var' = (pov_line_20_lin - cons_`var') / pov_line_20_lin if cons_`var' < pov_line_20_lin & `var' != .
replace pg_`var' = 0 if missing(pg_`var') & `var' != .

* watts index
gen wat_`var' = log(pov_line_20_lin/cons_`var') if cons_`var' < pov_line_20_lin & `var' != .
replace wat_`var' = 0 if missing(wat_`var') & `var' != .

drop total_poor trans tr_`var' cons_`var'
}



**** Categorical targeting

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

local vars hhsize nmemb65plus nmemb_dis nmemb_all nmemb_child nmemb_all2 fhh_child shock_nature
foreach var of local vars {

egen total_poor = sum(`var'), by(country)

gen trans = agg_actual_gap_lin / total_poor

gen tr_`var' = trans*`var'
replace tr_`var' = 0 if missing(tr_`var')

gen cons_`var' = real_consumption_pc + tr_`var'/hhsize

* head count index
gen hc_`var' = 0 if `var' != .
replace hc_`var' = 1 if cons_`var' <= pov_line_20_lin & `var' != .

* poverty gap index - gap is only calculated for those below it
gen pg_`var' = (pov_line_20_lin - cons_`var') / pov_line_20_lin if cons_`var' < pov_line_20_lin & `var' != .
replace pg_`var' = 0 if missing(pg_`var') & `var' != .

* watts index
gen wat_`var' = log(pov_line_20_lin/cons_`var') if cons_`var' < pov_line_20_lin & `var' != .
replace wat_`var' = 0 if missing(wat_`var') & `var' != .

drop total_poor trans tr_`var' cons_`var'
}

order hc_*, last
order pg_*, last
order wat_*, last

* starting values
gen hc_start = 0
replace hc_start = 1 if real_consumption_pc <= pov_line_20_lin

gen pg_start = (pov_line_20_lin - real_consumption_pc) / pov_line_20_lin if real_consumption_pc < pov_line_20_lin
replace pg_start = 0 if missing(pg_start)

gen wat_start = log(pov_line_20_lin/real_consumption_pc) if real_consumption_pc < pov_line_20_lin
replace wat_start = 0 if missing(wat_start)

tabstat hc_start pg_start wat_start [aweight=hhweight], by(country)  f(%9.3g)




*** Tables

/* Table 9 */
* headcount index
tabstat hc_start hc_hhsize hc_yhat_pmt_sh_consump hc_yhat_pmt_sh_qr20_cons hc_yhat_w20_sh_cons hc_yhat_w40_sh_cons  hc_yhat_pmt_sh_urb_consump hc_yhat_pmt_ext_consump hc_yhat_pmt_ex_qr20_cons hc_yhat_w20_cons hc_yhat_w40_cons  hc_yhat_step_consump hc_yhat_pmt_fs_consump hc_yhat_pmt_com_consump  hc_nmemb65plus hc_nmemb_dis hc_nmemb_all hc_nmemb_child hc_nmemb_all2 hc_fhh_child hc_shock_nature  [aweight=hhweight], by(country)  f(%9.3g)

/* Tables in Appendix */
* poverty gap index
tabstat pg_start pg_hhsize pg_yhat_pmt_sh_consump pg_yhat_pmt_sh_qr20_cons pg_yhat_w20_sh_cons pg_yhat_w40_sh_cons pg_yhat_pmt_sh_urb_consump pg_yhat_pmt_ext_consump pg_yhat_pmt_ex_qr20_cons pg_yhat_w20_cons pg_yhat_w40_cons pg_yhat_step_consump pg_yhat_pmt_fs_consump pg_yhat_pmt_com_consump  pg_nmemb65plus pg_nmemb_dis pg_nmemb_all pg_nmemb_child pg_nmemb_all2 pg_fhh_child pg_shock_nature  [aweight=hhweight], by(country)  f(%9.3g)

* Watts index
tabstat wat_start wat_hhsize wat_yhat_pmt_sh_consump wat_yhat_pmt_sh_qr20_cons wat_yhat_w20_sh_cons wat_yhat_w40_sh_cons wat_yhat_pmt_sh_urb_consump wat_yhat_pmt_ext_consump wat_yhat_pmt_ex_qr20_cons wat_yhat_w20_cons wat_yhat_w40_cons wat_yhat_step_consump wat_yhat_pmt_fs_consump wat_yhat_pmt_com_consump wat_nmemb65plus wat_nmemb_dis wat_nmemb_all wat_nmemb_child wat_nmemb_all2 wat_fhh_child wat_shock_nature  [aweight=hhweight], by(country)  f(%9.3g)



** change in indices
local vars yhat_pmt_sh_consump yhat_pmt_sh_qr20_cons yhat_w20_sh_cons yhat_w40_sh_cons yhat_pmt_sh_urb_consump yhat_pmt_ext_consump yhat_pmt_ex_qr20_cons yhat_w20_cons yhat_w40_cons  yhat_step_consump yhat_pmt_fs_consump yhat_pmt_com_consump hhsize nmemb65plus nmemb_dis nmemb_all nmemb_child nmemb_all2 fhh_child shock_nature shock_job
foreach var of local vars {
egen mean1 = wtmean(hc_`var'), by(country) weight(hhweight)
egen mean2 = wtmean(hc_actual), by(country) weight(hhweight)
gen hc_cha_`var' = (mean1 - mean2)*100/ mean2
drop mean1 mean2
egen mean1 = wtmean(pg_`var'), by(country) weight(hhweight)
egen mean2 = wtmean(actual_pg_pc), by(country) weight(hhweight)
gen pg_cha_`var' = (mean1 - mean2)*100/ mean2
drop mean1 mean2
egen mean1 = wtmean(wat_`var'), by(country) weight(hhweight)
egen mean2 = wtmean(actual_wat_pc), by(country) weight(hhweight)
gen wat_cha_`var' = (mean1 - mean2)*100/ mean2
drop mean1 mean2
}

order hc_cha_*, last
order pg_cha_*, last
order wat_cha_*, last

tabstat hc_cha_yhat_pmt_sh_consump hc_cha_yhat_pmt_ext_consump hc_cha_hhsize pg_cha_yhat_pmt_sh_consump pg_cha_yhat_pmt_ext_consump pg_cha_hhsize wat_cha_yhat_pmt_sh_consump wat_cha_yhat_pmt_ext_consump wat_cha_hhsize  [aweight=hhweight], by(country)  f(%9.3g)

save "Brown_Ravallion_vandeWalle_PMT.dta", replace



/***** In text: page 19 - focusing on subgroup of elderly and disabled. ****/

use "Brown_Ravallion_vandeWalle_PMT.dta", clear

* short PMT
gen yhat_pmt_sh_consump_dis = .
label var yhat_pmt_sh_consump_dis "Predicted values short PMT"

local wealth  toilet_flush toilet_pit floor_finish wall_finish roof_finish fuel_elecgas fuel_charcoal
local hh  urban female_head edu_head_primary edu_head_secondary div_sep_head widow_head work_paid_head work_selfemp_nonf_head muslim christian share_05f share_05m share_614f share_614m  share_65plusf share_65plusm

local country `" "BurkinaFaso" "Ethiopia" "Ghana"  "Malawi" "Mali" "Niger" "Nigeria" "Tanzania" "Uganda" "'
foreach x of local country{
quietly reg consump `wealth'  `hh' i.hhsize_cat i.age_head_cat i.state i.month if country == "`x'" & nmemb_all >= 1, vce(cluster EA)
predict yhat
replace yhat_pmt_sh_consump_dis = yhat if country == "`x'" & nmemb_all >= 1
drop yhat
}

** creating predicted poor
local vars yhat_pmt_sh_consump_dis
foreach var of local vars {

** poverty line
local nums `" "20" "40" "'
foreach num of local nums{

gen pr`num'_`var' = 0 if `var' != . & nmemb_all >= 1
replace pr`num'_`var' = 1 if `var' <= pov_line_`num' & `var' != . & nmemb_all >= 1
}

** poverty rate
gen pc_`var' = .

local country `" "BurkinaFaso" "Ethiopia" "Ghana"  "Malawi" "Mali" "Niger" "Nigeria" "Tanzania" "Uganda" "'
foreach x of local country{
xtile percentile_`x' = `var'  if country == "`x'" & `var' != . & nmemb_all >= 1 [aweight=hhweight] , n(100)
replace  pc_`var' = percentile_`x' if country == "`x'" & `var' != . & nmemb_all >= 1
drop percentile_`x'
}

replace pc_`var' = pc_`var' / 100

local nums `" "20" "40" "'
foreach num of local nums{
gen po`num'_`var' = 0 if `var' != .  & nmemb_all >= 1
replace po`num'_`var' = 1 if pc_`var' <= poor_`num'_mean & `var' != .  & nmemb_all >= 1
}

drop pc_`var'
}

tabstat pr20_yhat_pmt_sh_consump_dis pr40_yhat_pmt_sh_consump_dis po20_yhat_pmt_sh_consump_dis po40_yhat_pmt_sh_consump_dis [aweight=hhweight] if nmemb_all >= 1, by(country)

** inclusion and exclusion errors

local vars yhat_pmt_sh_consump_dis

local num `" "20" "40" "'

foreach var of local vars {
foreach x of local num {
*** line
* correctly included
gen ci1_pr`x'_`var' = 0 if `var' != .
replace ci1_pr`x'_`var' = 1 if poor_`x' == 1 & pr`x'_`var' == 1
* incorrectly included
gen ie1_pr`x'_`var' = 0 if `var' != .
replace ie1_pr`x'_`var' = 1 if poor_`x' == 0 & pr`x'_`var' == 1
* incorrectly excluded
gen ee1_pr`x'_`var' = 0 if `var' != .
replace ee1_pr`x'_`var' = 1 if poor_`x' == 1 & pr`x'_`var' == 0

* correctly included as share of total pop
egen ci_pr`x'_`var' = wtmean(ci1_pr`x'_`var') if `var' != . , by(country) weight(hhweight)
drop ci1_pr`x'_`var'

* incorrectly included as share of predicted poor
egen ie_pr`x'_`var' = wtmean(ie1_pr`x'_`var') if pr`x'_`var' == 1  & `var' != ., by(country) weight(hhweight)
drop ie1_pr`x'_`var'
* incorrectly excluded as share of actual poor
egen ee_pr`x'_`var' = wtmean(ee1_pr`x'_`var') if poor_`x' == 1  & `var' != . , by(country)  weight(hhweight)
drop ee1_pr`x'_`var'

*** rate
gen ci1_po`x'_`var' = 0 if `var' != .
replace ci1_po`x'_`var' = 1 if poor_`x' == 1 & po`x'_`var' == 1
gen ie1_po`x'_`var' = 0 if `var' != .
replace ie1_po`x'_`var' = 1 if poor_`x' == 0 & po`x'_`var' == 1
gen ee1_po`x'_`var' = 0 if `var' != .
replace ee1_po`x'_`var' = 1 if poor_`x' == 1 & po`x'_`var' == 0

* share of total pop
egen ci_po`x'_`var' = wtmean(ci1_po`x'_`var') if `var' != ., by(country)  weight(hhweight)
drop ci1_po`x'_`var'

* share of predicted poor
egen ie_po`x'_`var' = wtmean(ie1_po`x'_`var') if po`x'_`var' == 1  & `var' != . , by(country)  weight(hhweight)
drop ie1_po`x'_`var'
* share of actual poor
egen ee_po`x'_`var' = wtmean(ee1_po`x'_`var') if poor_`x' == 1  & `var' != . , by(country)  weight(hhweight)
drop ee1_po`x'_`var'

}
}

local vars yhat_pmt_sh_consump_dis
foreach var of local vars {
tabstat  ie_pr20_`var' ee_pr20_`var'  ie_pr40_`var' ee_pr40_`var' ie_po20_`var' ee_po20_`var'  ie_po40_`var' ee_po40_`var'  [aweight=hhweight] , by(country) f(%9.3g)
}


******* END OF FILE HERE ************




