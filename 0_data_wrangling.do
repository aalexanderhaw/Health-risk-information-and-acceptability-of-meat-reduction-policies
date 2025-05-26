* ========================
**# Stata code to prepare the data prior to reproducing the results of the revised manuscript titled „Does health-risk information increase the acceptability of a meat tax and meat free days? Experimental evidence from three European countries"
* ========================


* ========================
**# Section 0: Data Wrangling
* ========================



* ========================
**## Section 0.0: Data loading and combining
* ========================

clear

* Load the datasets for France, Latvia, and Italy
import delimited "data/FR_diet_data.csv", delimit(";") clear
gen country = "France"
destring id gender age agegroups income region c_0004 tcc tht d4_1a d4_1b ct v_762 v_784 as_1 as_3 ap_1  ap_3 eps1  eps3 epp1  epp3 fps1  fps3 fpp1  fpp3 cps1  cps3 cpp1  cpp3 int1 int2 mc1 mc2 ut d1 ac1 cr1 k1_1 k1_2  k1_3  k1_4 k1_5  k1_6  k1_7 k1_8 k1_9 sn_1 sn_2 sn_3 wb5 dn_1 dn_2 dn_7 so1_3 ccd_1 ccd_2 ccd_3 sd4 sd5 sd9_1 sd9_2 sd9_2a sd9_3 po1 po2 po3 po4 po5 income_detailed sds_1 sds_2 sds_3 sds_4 sds_5 sds_6 sds_7, force replace //into numeric before merging

tempfile fr_data
save `fr_data'

import delimited "data/IT_diet_data.csv", delimit(";") clear
gen country = "Italy"

destring id gender age agegroups income region c_0004 tcc tht d4_1a d4_1b ct v_762 v_784 as_1  as_3 ap_1  ap_3 eps1  eps3 epp1  epp3 fps1  fps3 fpp1  fpp3 cps1  cps3 cpp1  cpp3 int1 int2 mc1 mc2 ut d1 ac1 cr1 k1_1 k1_2  k1_3  k1_4 k1_5  k1_6  k1_7 k1_8 k1_9 sn_1 sn_2 sn_3 wb5 dn_1 dn_2 dn_7 so1_3 ccd_1 ccd_2 ccd_3 sd4 sd5 sd9_1 sd9_2 sd9_2a sd9_3 po1 po2 po3 po4 po5 income_detailed sds_1 sds_2 sds_3 sds_4 sds_5 sds_6 sds_7, force replace //into numeric before merging
tempfile it_data
save `it_data'


import delimited "data/LV_diet_data.csv", delimit(";") clear
gen country = "Latvia"

destring id gender age agegroups income region c_0004 tcc tht d4_1a d4_1b ct v_762 v_784 as_1  as_3 ap_1  ap_3 eps1  eps3 epp1  epp3 fps1  fps3 fpp1  fpp3 cps1  cps3 cpp1  cpp3 int1 int2 mc1 mc2 ut d1 ac1 cr1 k1_1 k1_2  k1_3  k1_4 k1_5  k1_6  k1_7 k1_8 k1_9 sn_1 sn_2 sn_3 wb5 dn_1 dn_2 dn_7 so1_3 ccd_1 ccd_2 ccd_3 sd4 sd5 sd9_1 sd9_2 sd9_2a sd9_3 po1 po2 po3 po4 po5 income_detailed sds_1 sds_2 sds_3 sds_4 sds_5 sds_6 sds_7, force replace //into numeric before merging

tempfile lv_data
save `lv_data'


* Combine datasets
use `fr_data', clear
append using `it_data', force
append using `lv_data', force


* ========================
**## Section 0.1: Removal of speeders etc.
* ========================

**General recoding
* Recoding missings to NA
foreach var of varlist _all {
    capture confirm numeric variable `var'
    if _rc == 0 {
        replace `var' = . if `var' == -99
        replace `var' = . if `var' == -66
    }
}


foreach var in sd4 po1 po2 po3 po4 po5 income {
    capture confirm numeric variable `var'
    if _rc == 0 {
        replace `var' = . if `var' == 99
        replace `var' = . if `var' == 66
    }
}



** Health risk information
gen health_treat = 0
replace health_treat = 1 if c_0004 == 1
replace health_treat = 0 if c_0004 == .


* Text speeders identification

rename v_762 tc1 //read CC text; 1 :yes; 2: no
rename v_784 tc2 //read health text

** speeder identification: if time to read text lower than 1/3 median
egen median_tht_FR  = median(tht)  if country=="France" & health_treat==1
egen median_tht_IT  = median(tht)  if country=="Italy" & health_treat==1
egen median_tht_LV  = median(tht)  if country=="Latvia" & health_treat==1
gen tht_speeder = cond(health_treat==0,0,.)
replace tht_speeder = 1 if country=="France" & health_treat==1 & tht < median_tht_FR/3
replace tht_speeder = 1 if country=="Italy" & health_treat==1 &tht < median_tht_IT/3
replace tht_speeder = 1 if country=="Latvia" & health_treat==1 & tht < median_tht_LV/3
replace tht_speeder = 0 if country=="France" & tht >= median_tht_FR/3
replace tht_speeder = 0 if country=="Italy" & tht >= median_tht_IT/3
replace tht_speeder = 0 if country=="Latvia" & tht >= median_tht_LV/3

gen tc2_rec = cond( health_treat==0,1,tc2) //if did not receive health_treat, could not have read health_treat text


egen median_tcc_FR  = median(tcc)  if country=="France" 
egen median_tcc_IT  = median(tcc)  if country=="Italy" 
egen median_tcc_LV  = median(tcc)  if country=="Latvia" 
gen tcc_speeder = cond(health_treat==0,0,.)
replace tcc_speeder = 1 if country=="France"  & tcc < median_tcc_FR/3
replace tcc_speeder = 1 if country=="Italy"  & tcc < median_tcc_IT/3
replace tcc_speeder = 1 if country=="Latvia"  & tcc < median_tcc_LV/3
replace tcc_speeder = 0 if country=="France" & tcc >= median_tcc_FR/3
replace tcc_speeder = 0 if country=="Italy" & tcc >= median_tcc_IT/3
replace tcc_speeder = 0 if country=="Latvia" & tcc >= median_tcc_LV/3


keep if gender!=3 //removing non-binary respondents

*Attention checks

gen ac1_cor = cond( ac1 == 8,1,0) 
gen ac2_cor = cond( so1_3 == 4,1,0) 
gen ac3_cor = cond( sds_4 == 2,1,0) 


* remove people who failed all attention checks
drop if ac1_cor == 0 & ac2_cor == 0 & ac3_cor == 0

* Removing text speeders and respondents with missings
keep if tcc_speeder==0 & tht_speeder==0 & tc2_rec==1 & tc1==1 //Keeping cases where: respondent did not read the text(s) too quickly AND responded that they read the text(s) attentatively

* remove people with who said they would not be honest
drop if ct == 2


* ========================
**## Section 0.2: Data Preparation 
* ========================

** Recoding outcome variables and policy perception variables
*Creating dummies with 1,2,3:0, and 4,5: 1
local varlist as_1  as_3 ap_1  ap_3 eps1  eps3 epp1  epp3 fps1  fps3 fpp1  fpp3 cps1  cps3 cpp1  cpp3

foreach var in `varlist' {
    * Check if the variable is numeric
    local isnumeric: type `var'

    * If it's numeric, replace "NA" with missing values
    if "`isnumeric'" == "double" {
        replace `var' = . if `var' == "NA"
	
	}
	    * Create a new variable with "_dummy" added to the original variable name
    local newvar "`var'_dummy"
    
    * Create a dummy variable with values based on the values of the original variable
    gen `newvar' = cond(`var' == 4 | `var' == 5, 1, 0)
}

** Outcome variables
rename as_1_dummy meat_tax_dummy
rename as_3_dummy meat_day_dummy
rename ap_1_dummy meat_tax_soc_dummy
rename ap_3_dummy meat_day_soc_dummy

* Likert scales for robustness tests
rename as_1 meat_tax_lik
rename as_3 meat_day_lik
rename ap_1 meat_tax_lik_soc
rename ap_3 meat_day_lik_soc


**Perceived policy characteristics

rename eps1_dummy eff_tax_self_dummy
rename eps3_dummy eff_veggie_self_dummy
rename fps1_dummy fair_tax_self_dummy
rename fps3_dummy fair_veggie_self_dummy
rename cps1_dummy cost_tax_self_dummy
rename cps3_dummy cost_veggie_self_dummy
rename epp1_dummy eff_tax_soc_dummy
rename epp3_dummy eff_veggie_soc_dummy
rename fpp1_dummy fair_tax_soc_dummy
rename fpp3_dummy fair_veggie_soc_dummy
rename cpp1_dummy cost_tax_soc_dummy
rename cpp3_dummy cost_veggie_soc_dummy

** Vulnerability indicators
* Poor health
gen health_low_rec = cond(wb5==.,.,1)
replace health_low_rec = 0 if country=="France" & wb5 >= 3
replace health_low_rec = 0 if country=="Italy" & wb5 >= 3
replace health_low_rec = 0 if country=="Latvia" & wb5 >= 3

* Food deprivation
gen food_depriv= cond(dn_1 + dn_2  +dn_7 ==3,0,1) //food deprivation: deprived if experienced any of the three deprivation questions at least once in 2022

* Unemployed
gen unemployed = 0
replace unemployed = 1 if sd5 == 6 //unemployed: 1, otherwise 0

* Only secondary education
gen educ_low = 0
replace educ_low = 1 if sd4 == 1 | sd4 == 2 | sd4 == 3 //max secondary educ 1, otherwise 0

** Sociodemographic characteristics

*** Age dummy for heterogeneity test
egen median_age_FR  = median(age)  if country=="France"
egen median_age_IT  = median(age)  if country=="Italy"
egen median_age_LV  = median(age)  if country=="Latvia"
gen age_low = cond(age==.,.,0)
replace age_low = 1 if country=="France" & age < median_age_FR
replace age_low = 1 if country=="Italy" & age < median_age_IT
replace age_low = 1 if country=="Latvia" & age < median_age_LV

*Female
gen female= 0
replace female = 1 if gender == 2
replace female = 0 if gender == 1


* Income per person
rename  income income_group

gen income_eur = .
replace income_eur = 3600 if country == "France" & income_detailed == 1
replace income_eur = 5400 if country == "France" & income_detailed == 2
replace income_eur = 9600 if country == "France" & income_detailed == 3
replace income_eur = 18100 if country == "France" & income_detailed == 4
replace income_eur = 29300 if country == "France" & income_detailed == 5
replace income_eur = 38100 if country == "France" & income_detailed == 6
replace income_eur = 45400 if country == "France" & income_detailed == 7
replace income_eur = 52850 if country == "France" & income_detailed == 8
replace income_eur = 60950 if country == "France" & income_detailed == 9
replace income_eur = 70200 if country == "France" & income_detailed == 10
replace income_eur = 81600 if country == "France" & income_detailed == 11
replace income_eur = 94000 if country == "France" & income_detailed == 12
replace income_eur = 100000 if country == "France" & income_detailed == 13
replace income_eur = 3600 if country == "Italy" & income_detailed == 1
replace income_eur = 5400 if country == "Italy" & income_detailed == 2
replace income_eur = 9600 if country == "Italy" & income_detailed == 3
replace income_eur = 18100 if country == "Italy" & income_detailed == 4
replace income_eur = 29300 if country == "Italy" & income_detailed == 5
replace income_eur = 38100 if country == "Italy" & income_detailed == 6
replace income_eur = 45400 if country == "Italy" & income_detailed == 7
replace income_eur = 52850 if country == "Italy" & income_detailed == 8
replace income_eur = 60950 if country == "Italy" & income_detailed == 9
replace income_eur = 70200 if country == "Italy" & income_detailed == 10
replace income_eur = 81600 if country == "Italy" & income_detailed == 11
replace income_eur = 94000 if country == "Italy" & income_detailed == 12
replace income_eur = 100000 if country == "Italy" & income_detailed == 13
replace income_eur = 3000 if country == "Latvia" & income_detailed == 1
replace income_eur = 4500 if country == "Latvia" & income_detailed == 2
replace income_eur = 6750 if country == "Latvia" & income_detailed == 3
replace income_eur = 8250 if country == "Latvia" & income_detailed == 4
replace income_eur = 10500 if country == "Latvia" & income_detailed == 5
replace income_eur = 13500 if country == "Latvia" & income_detailed == 6
replace income_eur = 16500 if country == "Latvia" & income_detailed == 7
replace income_eur = 19500 if country == "Latvia" & income_detailed == 8
replace income_eur = 22500 if country == "Latvia" & income_detailed == 9
replace income_eur = 26000 if country == "Latvia" & income_detailed == 10
replace income_eur = 28000 if country == "Latvia" & income_detailed == 11


* Replace missing values with 0
foreach var in sd9_1 sd9_2 sd9_2a sd9_3 {
    replace `var' = 0 if missing(`var')
}

* Adjusting household income into per person income using OECD weights
gen income_pp = income_eur / ((sd9_1 + sd9_2a) * 0.3 + 1 + (sd9_3 + sd9_2 - 1) * 0.5) //per person

* Income in € into income in 1000T€
gen income_pp_1000 = income_pp/1000 

* Lowest income decile per country
egen quant_inc=xtile(income_pp_1000 ), n(10) by(country)
gen income_Q1= cond(quant_inc==1, 1, 0) //income dummy lowest 10% for each country=1

** Lifestyle and attitudes

* Dine in canteen
tab cr1 // no missings if eat in canteen
gen canteen_dummy = cond(cr1 == 1,0,1) // 0 if never eat in canteen, 1 if at least sometimes


* Diet type dummies 
tab d1 // no missings in diet type
* Meat-free diet
gen vegan_veg_pesc_dummy = cond(inlist(d1, 4, 5, 6), 1, 0)
* Low-meat diet
gen flex_dummy = cond(inlist(d1, 3), 1, 0)
* High-meat diet
gen high_meat_dummy = cond(inlist(d1, 1), 1, 0)


* Policy support
local varlist po1 po2 po3 po4 po5 

foreach var in `varlist' {
    * Check if the variable is numeric
    local isnumeric: type `var'
    * If it's numeric, replace "NA" with missing values
    if "`isnumeric'" == "double" {
        replace `var' = . if `var' == "NA"
	
	}
	    * Create a new variable with "_dummy" added to the original variable name
    local newvar "`var'_dummy"
    
    * Create a dummy variable with values based on the values of the original variable
    gen `newvar' = cond(`var' == 4 | `var' == 5, 1, 0)
}

* Nutrition knowledge

alpha k1_1 k1_2  k1_3  k1_4 k1_5  k1_6  k1_7 k1_8 k1_9 if country == "France", item
alpha k1_1 k1_2  k1_3  k1_4 k1_5  k1_6  k1_7 k1_8 k1_9 if country == "Italy", item
alpha k1_1 k1_2  k1_3  k1_4 k1_5  k1_6  k1_7 k1_8 k1_9 if country == "Latvia", item
alpha k1_1 k1_2  k1_3  k1_4 k1_5  k1_6  k1_7 k1_8 k1_9, item

gen diet_knowl_index = (k1_1 + k1_2 + k1_3 + k1_4 + k1_5 + k1_6 + k1_7 + k1_8 + k1_9) / 9

egen median_diet_knowl_index_FR  = median(diet_knowl_index)  if country=="France"
egen median_diet_knowl_index_IT  = median(diet_knowl_index)  if country=="Italy"
egen median_diet_knowl_index_LV  = median(diet_knowl_index)  if country=="Latvia"
egen mean_diet_knowl_index_FR  = mean(diet_knowl_index)  if country=="France"
egen mean_diet_knowl_index_IT  = mean(diet_knowl_index)  if country=="Italy"
egen mean_diet_knowl_index_LV  = mean(diet_knowl_index)  if country=="Latvia"
gen diet_knowl_high = cond(diet_knowl_index==.,.,0)
replace diet_knowl_high = 1 if country=="France" & diet_knowl_index >= median_diet_knowl_index_FR
replace diet_knowl_high = 1 if country=="Italy" & diet_knowl_index >= median_diet_knowl_index_IT
replace diet_knowl_high = 1 if country=="Latvia" & diet_knowl_index >= median_diet_knowl_index_LV


* Climate change acknowledgement

alpha ccd_1 ccd_2 ccd_3 if country == "France", item 
alpha ccd_1 ccd_2 ccd_3 if country == "Italy", item
alpha ccd_1 ccd_2 ccd_3 if country == "Latvia", item
alpha ccd_1 ccd_2 ccd_3, item

gen cc_denial_index = ( ccd_1 +ccd_2+ ccd_3) / 3

egen median_cc_denial_index_FR  = median(cc_denial_index)  if country=="France"
egen median_cc_denial_index_IT  = median(cc_denial_index)  if country=="Italy"
egen median_cc_denial_index_LV  = median(cc_denial_index)  if country=="Latvia"

gen cc_acc_high = cond(cc_denial_index==.,.,0)
replace cc_acc_high = 1 if country=="France" & cc_denial_index <= median_cc_denial_index_FR
replace cc_acc_high = 1 if country=="Italy" & cc_denial_index <= median_cc_denial_index_IT
replace cc_acc_high = 1 if country=="Latvia" & cc_denial_index <= median_cc_denial_index_LV


* Meat reduction social norms

alpha sn_1 sn_2 sn_3 if country == "France", item
alpha sn_1 sn_2 sn_3 if country == "Italy", item
alpha sn_1 sn_2 sn_3 if country == "Latvia", item
alpha sn_1 sn_2 sn_3, item

gen soc_norm_index = (sn_1 +sn_2 +sn_3 ) / 3

egen median_soc_norm_index_FR  = median(soc_norm_index)  if country=="France"
egen median_soc_norm_index_IT  = median(soc_norm_index)  if country=="Italy"
egen median_soc_norm_index_LV  = median(soc_norm_index)  if country=="Latvia"
gen soc_norm_high = cond(soc_norm_index==.,.,0)
replace soc_norm_high = 1 if country=="France" & soc_norm_index >= median_soc_norm_index_FR
replace soc_norm_high = 1 if country=="Italy" & soc_norm_index >= median_soc_norm_index_IT
replace soc_norm_high = 1 if country=="Latvia" & soc_norm_index >= median_soc_norm_index_LV


** Country dummies
*France
gen FR_dummy = cond(country=="France",1,0)
*Italy
gen IT_dummy = cond(country=="Italy",1,0)
*Latvia
gen LV_dummy = cond(country=="Latvia",1,0)


save data/clean_data, replace
 
 
 
 
* ========================
**## Section 0.3: Data loading and wrangling with speeders (for robustness check)
* ========================

clear

* Load the datasets for France, Latvia, and Italy
import delimited "data/FR_diet_data.csv", delimit(";") clear
gen country = "France"
destring id gender age agegroups income region c_0004 tcc tht d4_1a d4_1b ct v_762 v_784 as_1  as_3 ap_1  ap_3 eps1  eps3 epp1  epp3 fps1  fps3 fpp1  fpp3 cps1  cps3 cpp1  cpp3 int1 int2 mc1 mc2 ut d1 ac1 cr1 k1_1 k1_2  k1_3  k1_4 k1_5  k1_6  k1_7 k1_8 k1_9 sn_1 sn_2 sn_3 wb5 dn_1 dn_2 dn_7 so1_3 ccd_1 ccd_2 ccd_3 sd4 sd5 sd9_1 sd9_2 sd9_2a sd9_3 po1 po2 po3 po4 po5 income_detailed sds_1 sds_2 sds_3 sds_4 sds_5 sds_6 sds_7, force replace //into numeric before merging

tempfile fr_data
save `fr_data'

import delimited "data/IT_diet_data.csv", delimit(";") clear
gen country = "Italy"

destring id gender age agegroups income region c_0004 tcc tht d4_1a d4_1b ct v_762 v_784 as_1  as_3 ap_1  ap_3 eps1  eps3 epp1  epp3 fps1  fps3 fpp1  fpp3 cps1  cps3 cpp1  cpp3 int1 int2 mc1 mc2 ut d1 ac1 cr1 k1_1 k1_2  k1_3  k1_4 k1_5  k1_6  k1_7 k1_8 k1_9 sn_1 sn_2 sn_3 wb5 dn_1 dn_2 dn_7 so1_3 ccd_1 ccd_2 ccd_3 sd4 sd5 sd9_1 sd9_2 sd9_2a sd9_3 po1 po2 po3 po4 po5 income_detailed sds_1 sds_2 sds_3 sds_4 sds_5 sds_6 sds_7, force replace //into numeric before merging
tempfile it_data
save `it_data'


import delimited "data/LV_diet_data.csv", delimit(";") clear
gen country = "Latvia"

destring id gender age agegroups income region c_0004 tcc tht d4_1a d4_1b ct v_762 v_784 as_1  as_3 ap_1  ap_3 eps1  eps3 epp1  epp3 fps1  fps3 fpp1  fpp3 cps1  cps3 cpp1  cpp3 int1 int2 mc1 mc2 ut d1 ac1 cr1 k1_1 k1_2  k1_3  k1_4 k1_5  k1_6  k1_7 k1_8 k1_9 sn_1 sn_2 sn_3 wb5 dn_1 dn_2 dn_7 so1_3 ccd_1 ccd_2 ccd_3 sd4 sd5 sd9_1 sd9_2 sd9_2a sd9_3 po1 po2 po3 po4 po5 income_detailed sds_1 sds_2 sds_3 sds_4 sds_5 sds_6 sds_7, force replace //into numeric before merging

tempfile lv_data
save `lv_data'


* Combine datasets
use `fr_data', clear
append using `it_data', force
append using `lv_data', force

* ======================== 
**## Section 0.4: General recoding with speeders
* ========================

**General recoding
* Recoding missings to NA
foreach var of varlist _all {
    capture confirm numeric variable `var'
    if _rc == 0 {
        replace `var' = . if `var' == -99
        replace `var' = . if `var' == -66
    }
}


foreach var in sd4 po1 po2 po3 po4 po5 income {
    capture confirm numeric variable `var'
    if _rc == 0 {
        replace `var' = . if `var' == 99
        replace `var' = . if `var' == 66
    }
}



** Health risk information
gen health_treat = 0
replace health_treat = 1 if c_0004 == 1
replace health_treat = 0 if c_0004 == .


* Text speeders identification

rename v_762 tc1 //read CC text; 1 :yes; 2: no
rename v_784 tc2 //read health text

** speeder identification: if time to read text lower than 1/3 median
egen median_tht_FR  = median(tht)  if country=="France" & health_treat==1
egen median_tht_IT  = median(tht)  if country=="Italy" & health_treat==1
egen median_tht_LV  = median(tht)  if country=="Latvia" & health_treat==1
gen tht_speeder = cond(health_treat==0,0,.)
replace tht_speeder = 1 if country=="France" & health_treat==1 & tht < median_tht_FR/3
replace tht_speeder = 1 if country=="Italy" & health_treat==1 &tht < median_tht_IT/3
replace tht_speeder = 1 if country=="Latvia" & health_treat==1 & tht < median_tht_LV/3
replace tht_speeder = 0 if country=="France" & tht >= median_tht_FR/3
replace tht_speeder = 0 if country=="Italy" & tht >= median_tht_IT/3
replace tht_speeder = 0 if country=="Latvia" & tht >= median_tht_LV/3

gen tc2_rec = cond( health_treat==0,1,tc2) //if did not receive health_treat, could not have read health_treat text


egen median_tcc_FR  = median(tcc)  if country=="France" 
egen median_tcc_IT  = median(tcc)  if country=="Italy" 
egen median_tcc_LV  = median(tcc)  if country=="Latvia" 
gen tcc_speeder = cond(health_treat==0,0,.)
replace tcc_speeder = 1 if country=="France"  & tcc < median_tcc_FR/3
replace tcc_speeder = 1 if country=="Italy"  & tcc < median_tcc_IT/3
replace tcc_speeder = 1 if country=="Latvia"  & tcc < median_tcc_LV/3
replace tcc_speeder = 0 if country=="France" & tcc >= median_tcc_FR/3
replace tcc_speeder = 0 if country=="Italy" & tcc >= median_tcc_IT/3
replace tcc_speeder = 0 if country=="Latvia" & tcc >= median_tcc_LV/3


keep if gender!=3 //removing non-binary respondents

*Attention checks

gen ac1_cor = cond( ac1 == 8,1,0) 
gen ac2_cor = cond( so1_3 == 4,1,0) 
gen ac3_cor = cond( sds_4 == 2,1,0) 


* remove people who failed all attention checks
drop if ac1_cor == 0 & ac2_cor == 0 & ac3_cor == 0

* remove people with who said they would not be honest
drop if ct == 2


* ========================
**## Section 0.5: Data Preparation with speeders
* ========================

** Recoding outcome variables and policy perception variables
*Creating dummies with 1,2,3:0, and 4,5: 1
local varlist as_1  as_3 ap_1  ap_3 eps1  eps3 epp1  epp3 fps1  fps3 fpp1  fpp3 cps1  cps3 cpp1  cpp3

foreach var in `varlist' {
    * Check if the variable is numeric
    local isnumeric: type `var'

    * If it's numeric, replace "NA" with missing values
    if "`isnumeric'" == "double" {
        replace `var' = . if `var' == "NA"
	
	}
	    * Create a new variable with "_dummy" added to the original variable name
    local newvar "`var'_dummy"
    
    * Create a dummy variable with values based on the values of the original variable
    gen `newvar' = cond(`var' == 4 | `var' == 5, 1, 0)
}

** Outcome variables
rename as_1_dummy meat_tax_dummy
rename as_3_dummy meat_day_dummy
rename ap_1_dummy meat_tax_soc_dummy
rename ap_3_dummy meat_day_soc_dummy

* Likert scales for robustness tests
rename as_1 meat_tax_lik
rename as_3 meat_day_lik
rename ap_1 meat_tax_lik_soc
rename ap_3 meat_day_lik_soc


**Perceived policy characteristics

rename eps1_dummy eff_tax_self_dummy
rename eps3_dummy eff_veggie_self_dummy
rename fps1_dummy fair_tax_self_dummy
rename fps3_dummy fair_veggie_self_dummy
rename cps1_dummy cost_tax_self_dummy
rename cps3_dummy cost_veggie_self_dummy
rename epp1_dummy eff_tax_soc_dummy
rename epp3_dummy eff_veggie_soc_dummy
rename fpp1_dummy fair_tax_soc_dummy
rename fpp3_dummy fair_veggie_soc_dummy
rename cpp1_dummy cost_tax_soc_dummy
rename cpp3_dummy cost_veggie_soc_dummy

** Vulnerability indicators
* Poor health
gen health_low_rec = cond(wb5==.,.,1)
replace health_low_rec = 0 if country=="France" & wb5 >= 3
replace health_low_rec = 0 if country=="Italy" & wb5 >= 3
replace health_low_rec = 0 if country=="Latvia" & wb5 >= 3

* Food deprivation
gen food_depriv= cond(dn_1 + dn_2  +dn_7 ==3,0,1) //food deprivation: deprived if experienced any of the three deprivation questions at least once in 2022

* Unemployed
gen unemployed = 0
replace unemployed = 1 if sd5 == 6 //unemployed: 1, otherwise 0

* Only secondary education
gen educ_low = 0
replace educ_low = 1 if sd4 == 1 | sd4 == 2 | sd4 == 3 //max secondary educ 1, otherwise 0

** Sociodemographic characteristics

*** Age dummy for heterogeneity test
egen median_age_FR  = median(age)  if country=="France"
egen median_age_IT  = median(age)  if country=="Italy"
egen median_age_LV  = median(age)  if country=="Latvia"
gen age_low = cond(age==.,.,0)
replace age_low = 1 if country=="France" & age < median_age_FR
replace age_low = 1 if country=="Italy" & age < median_age_IT
replace age_low = 1 if country=="Latvia" & age < median_age_LV

*Female
gen female= 0
replace female = 1 if gender == 2
replace female = 0 if gender == 1


* Income per person
rename  income income_group

gen income_eur = .
replace income_eur = 3600 if country == "France" & income_detailed == 1
replace income_eur = 5400 if country == "France" & income_detailed == 2
replace income_eur = 9600 if country == "France" & income_detailed == 3
replace income_eur = 18100 if country == "France" & income_detailed == 4
replace income_eur = 29300 if country == "France" & income_detailed == 5
replace income_eur = 38100 if country == "France" & income_detailed == 6
replace income_eur = 45400 if country == "France" & income_detailed == 7
replace income_eur = 52850 if country == "France" & income_detailed == 8
replace income_eur = 60950 if country == "France" & income_detailed == 9
replace income_eur = 70200 if country == "France" & income_detailed == 10
replace income_eur = 81600 if country == "France" & income_detailed == 11
replace income_eur = 94000 if country == "France" & income_detailed == 12
replace income_eur = 100000 if country == "France" & income_detailed == 13
replace income_eur = 3600 if country == "Italy" & income_detailed == 1
replace income_eur = 5400 if country == "Italy" & income_detailed == 2
replace income_eur = 9600 if country == "Italy" & income_detailed == 3
replace income_eur = 18100 if country == "Italy" & income_detailed == 4
replace income_eur = 29300 if country == "Italy" & income_detailed == 5
replace income_eur = 38100 if country == "Italy" & income_detailed == 6
replace income_eur = 45400 if country == "Italy" & income_detailed == 7
replace income_eur = 52850 if country == "Italy" & income_detailed == 8
replace income_eur = 60950 if country == "Italy" & income_detailed == 9
replace income_eur = 70200 if country == "Italy" & income_detailed == 10
replace income_eur = 81600 if country == "Italy" & income_detailed == 11
replace income_eur = 94000 if country == "Italy" & income_detailed == 12
replace income_eur = 100000 if country == "Italy" & income_detailed == 13
replace income_eur = 3000 if country == "Latvia" & income_detailed == 1
replace income_eur = 4500 if country == "Latvia" & income_detailed == 2
replace income_eur = 6750 if country == "Latvia" & income_detailed == 3
replace income_eur = 8250 if country == "Latvia" & income_detailed == 4
replace income_eur = 10500 if country == "Latvia" & income_detailed == 5
replace income_eur = 13500 if country == "Latvia" & income_detailed == 6
replace income_eur = 16500 if country == "Latvia" & income_detailed == 7
replace income_eur = 19500 if country == "Latvia" & income_detailed == 8
replace income_eur = 22500 if country == "Latvia" & income_detailed == 9
replace income_eur = 26000 if country == "Latvia" & income_detailed == 10
replace income_eur = 28000 if country == "Latvia" & income_detailed == 11


* Replace missing values with 0
foreach var in sd9_1 sd9_2 sd9_2a sd9_3 {
    replace `var' = 0 if missing(`var')
}

* Adjusting household income into per person income using OECD weights
gen income_pp = income_eur / ((sd9_1 + sd9_2a) * 0.3 + 1 + (sd9_3 + sd9_2 - 1) * 0.5) //per person

* Income in € into income in 1000T€
gen income_pp_1000 = income_pp/1000 

* Lowest income decile per country
egen quant_inc=xtile(income_pp_1000 ), n(10) by(country)
gen income_Q1= cond(quant_inc==1, 1, 0) //income dummy lowest 10% for each country=1

** Lifestyle and attitudes

* Dine in canteen
tab cr1 // no missings if eat in canteen
gen canteen_dummy = cond(cr1 == 1,0,1) // 0 if never eat in canteen, 1 if at least sometimes


* Diet type dummies 
tab d1 // no missings in diet type
* Meat-free diet
gen vegan_veg_pesc_dummy = cond(inlist(d1, 4, 5, 6), 1, 0)
* Low-meat diet
gen flex_dummy = cond(inlist(d1, 3), 1, 0)
* High-meat diet
gen high_meat_dummy = cond(inlist(d1, 1), 1, 0)


* Policy support
local varlist po1 po2 po3 po4 po5 

foreach var in `varlist' {
    * Check if the variable is numeric
    local isnumeric: type `var'
    * If it's numeric, replace "NA" with missing values
    if "`isnumeric'" == "double" {
        replace `var' = . if `var' == "NA"
	
	}
	    * Create a new variable with "_dummy" added to the original variable name
    local newvar "`var'_dummy"
    
    * Create a dummy variable with values based on the values of the original variable
    gen `newvar' = cond(`var' == 4 | `var' == 5, 1, 0)
}

* Nutrition knowledge

alpha k1_1 k1_2  k1_3  k1_4 k1_5  k1_6  k1_7 k1_8 k1_9 if country == "France", item
alpha k1_1 k1_2  k1_3  k1_4 k1_5  k1_6  k1_7 k1_8 k1_9 if country == "Italy", item
alpha k1_1 k1_2  k1_3  k1_4 k1_5  k1_6  k1_7 k1_8 k1_9 if country == "Latvia", item

gen diet_knowl_index = (k1_1 + k1_2 + k1_3 + k1_4 + k1_5 + k1_6 + k1_7 + k1_8 + k1_9) / 9

egen median_diet_knowl_index_FR  = median(diet_knowl_index)  if country=="France"
egen median_diet_knowl_index_IT  = median(diet_knowl_index)  if country=="Italy"
egen median_diet_knowl_index_LV  = median(diet_knowl_index)  if country=="Latvia"
egen mean_diet_knowl_index_FR  = mean(diet_knowl_index)  if country=="France"
egen mean_diet_knowl_index_IT  = mean(diet_knowl_index)  if country=="Italy"
egen mean_diet_knowl_index_LV  = mean(diet_knowl_index)  if country=="Latvia"
gen diet_knowl_high = cond(diet_knowl_index==.,.,0)
replace diet_knowl_high = 1 if country=="France" & diet_knowl_index >= median_diet_knowl_index_FR
replace diet_knowl_high = 1 if country=="Italy" & diet_knowl_index >= median_diet_knowl_index_IT
replace diet_knowl_high = 1 if country=="Latvia" & diet_knowl_index >= median_diet_knowl_index_LV


* Climate change acknowledgement

alpha ccd_1 ccd_2 ccd_3 if country == "France", item 
alpha ccd_1 ccd_2 ccd_3 if country == "Italy", item
alpha ccd_1 ccd_2 ccd_3 if country == "Latvia", item

gen cc_denial_index = ( ccd_1 +ccd_2+ ccd_3) / 3

egen median_cc_denial_index_FR  = median(cc_denial_index)  if country=="France"
egen median_cc_denial_index_IT  = median(cc_denial_index)  if country=="Italy"
egen median_cc_denial_index_LV  = median(cc_denial_index)  if country=="Latvia"

gen cc_acc_high = cond(cc_denial_index==.,.,0)
replace cc_acc_high = 1 if country=="France" & cc_denial_index <= median_cc_denial_index_FR
replace cc_acc_high = 1 if country=="Italy" & cc_denial_index <= median_cc_denial_index_IT
replace cc_acc_high = 1 if country=="Latvia" & cc_denial_index <= median_cc_denial_index_LV


* Meat reduction social norms

alpha sn_1 sn_2 sn_3 if country == "France", item
alpha sn_1 sn_2 sn_3 if country == "Italy", item
alpha sn_1 sn_2 sn_3 if country == "Latvia", item

gen soc_norm_index = (sn_1 +sn_2 +sn_3 ) / 3

egen median_soc_norm_index_FR  = median(soc_norm_index)  if country=="France"
egen median_soc_norm_index_IT  = median(soc_norm_index)  if country=="Italy"
egen median_soc_norm_index_LV  = median(soc_norm_index)  if country=="Latvia"
gen soc_norm_high = cond(soc_norm_index==.,.,0)
replace soc_norm_high = 1 if country=="France" & soc_norm_index >= median_soc_norm_index_FR
replace soc_norm_high = 1 if country=="Italy" & soc_norm_index >= median_soc_norm_index_IT
replace soc_norm_high = 1 if country=="Latvia" & soc_norm_index >= median_soc_norm_index_LV


** Country dummies
*France
gen FR_dummy = cond(country=="France",1,0)
*Italy
gen IT_dummy = cond(country=="Italy",1,0)
*Latvia
gen LV_dummy = cond(country=="Latvia",1,0)


save data/clean_data_with_speeder, replace
 
 
 
 
 
 