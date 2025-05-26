* ========================
**# Stata code to reproduce the robustness check results of the revised manuscript titled „Does health-risk information increase the acceptability of a meat tax and meat free days? Experimental evidence from three European countries", submitted to Food Policy, Date: 3 November 2024;
* ========================

* ========================
**# Section 0: data prep 
* ========================
* Load data
clear
use "data/clean_data.dta"
set seed 1234


*general prep
rename health_treat HT

rename meat_day_soc_dummy meat_day_s 
rename meat_tax_soc_dummy meat_tax_s


*meat consumption intention
rename int1 red_meat
rename int2 other_meat

gen red_meat_dummy=.
replace red_meat_dummy = 1 if inlist(red_meat, 1, 2, 3)
replace red_meat_dummy = 0 if inrange(red_meat, 4, 8)

gen other_meat_dummy=.
replace other_meat_dummy = 1 if inlist(other_meat, 1, 2, 3)
replace other_meat_dummy = 0 if inrange(other_meat, 4, 8)


*Recoding outcome variable
gen meat_tax_very = 0
replace meat_tax_very = 1 if meat_tax_lik == 5

gen meat_tax_very_soc = 0
replace meat_tax_very_soc = 1 if meat_tax_lik_soc  == 5

gen meat_day_very = 0
replace meat_day_very = 1 if meat_day_lik == 5

gen meat_day_very_soc = 0
replace meat_day_very_soc = 1 if meat_day_lik_soc  == 5

** Social Desireability
foreach var in sds_1 sds_2 sds_7 {
	gen `var'_rec = .
    replace `var'_rec = 8 - `var'
}

gen soc_des_index = (sds_1_rec+ sds_2_rec +sds_3  +sds_5+ sds_6+ sds_7_rec ) / 6

egen median_soc_des_index_FR  = median(soc_des_index)  if country=="France"
egen median_soc_des_index_IT  = median(soc_des_index)  if country=="Italy"
egen median_soc_des_index_LV  = median(soc_des_index)  if country=="Latvia"
gen soc_des_high = cond(soc_des_index==.,.,0)
replace soc_des_high = 1 if country=="France" & soc_des_index >= median_soc_des_index_FR
replace soc_des_high = 1 if country=="Italy" & soc_des_index >= median_soc_des_index_IT
replace soc_des_high = 1 if country=="Latvia" & soc_des_index >= median_soc_des_index_LV

** Manipulation check variables
rename mc1 mc_self 
rename mc2 mc_soc

gen  mc_self_dummy= cond(inlist(mc_self, 1,2), 0, 1)
gen  mc_soc_dummy= cond(inlist(mc_soc, 1,2), 0, 1)

*Heterogeneity variables
global het_var food_depriv health_low_rec unemployed educ_low income_Q1 age_low female vegan_veg_pesc_dummy high_meat_dummy canteen_dummy  po1_dummy po2_dummy po3_dummy po4_dummy po5_dummy  diet_knowl_high cc_denial_high soc_norm_high


*Relabelling variables
label variable HT "Health-risk information"
label variable FR_dummy "France (vs. Italy)"
label variable LV_dummy "Latvia (vs. Italy)"
label variable food_depriv "Food deprivation"
label variable health_low_rec "Poor health"
label variable unemployed "Unemployed"
label variable educ_low "Low education"
label variable canteen_dummy "Dine in canteen"
label variable age "Age"
label variable age_low "Below median age"
label variable female "Female"
label variable income_Q1 "First income decile"
label variable income_pp "Income (in T€)"
label variable vegan_veg_pesc_dummy "Meat-free diet (vs. varied and high-meat diets)"
label variable high_meat_dummy "High-meat diet (vs. low and varied-meat diets)"
label variable po1_dummy "Support nationally oriented policies"
label variable po2_dummy "Support social policies"
label variable po3_dummy "Support conservative policies"
label variable po4_dummy "Support liberal policies"
label variable po5_dummy "Support environmental policies"
label variable diet_knowl_high "Nutrition knowledge"
label variable cc_acc_high "Climate change acknowledgement"
label variable soc_norm_high "Meat reduction social norms"
label variable soc_des_high "Social desirability"
label variable mc_self_dummy "Harmfulness of meat consumption"
label variable mc_soc_dummy "Harmfulness of meat consumption"


local table_num 1 // Start from table 1

* ========================
**# Document heading
* ========================

putdocx clear
putdocx begin, font("", 10) landscape

putdocx paragraph
putdocx text ("Supplementary Material SC: Robustness check results"), bold  font("", 11)
putdocx paragraph
putdocx text ("Below we provide the detailed results of all robustness checks presented in Section 3.2.4."),  font("", 11) linebreak


* ========================
**# Table SC1: Robustness test (basic) 1: Health treatment only model intended meat consumption 
* ========================


qui {   
    qui regress red_meat_dummy HT FR_dummy LV_dummy , vce(hc3)
    matrix rtable1 = r(table)
    scalar nobs1 = e(N)
	local r2_1 = string(e(r2), "%9.3f")
	local r2_adj_1 = string(e(r2_a), "%9.3f")
    qui regress other_meat_dummy HT FR_dummy LV_dummy , vce(hc3)
    matrix rtable2 = r(table)
    scalar nobs2 = e(N)
	local r2_2 = string(e(r2), "%9.3f")
	local r2_adj_2 = string(e(r2_a), "%9.3f")

local file_path = "robustness_main/r2_meat_cons.xlsx"
    	
if !fileexists("`file_path'") {
wyoung, cmd("regress red_meat_dummy  1.HT  FR_dummy LV_dummy, vce(hc3)" ///
            "regress other_meat_dummy  1.HT  FR_dummy LV_dummy, vce(hc3)" /// ) ///
	familyp("HT" "HT" "HT" "HT") familypexp bootstraps(10000) seed(1234)
	matrix wy_results = r(table)
 	putexcel set robustness_main/r2_meat_cons.xlsx, replace
	putexcel A1=matrix(results_), names
	
	
}
preserve

import excel `file_path', sheet("Sheet1") firstrow clear
mkmat coef stderr  p pwyoung pbonf psidak, matrix(wy_results)
restore
	
*Add title
putdocx paragraph 
putdocx text ("Table SC`table_num' LPM results of the basic models, with intended meat consumption as the outcome variable - pooled sample."), font("", 11)

*Create a table with 5 rows and 5 columns (1 for covariate names, 4 for models)
putdocx table mytable = (9, 3), border(all, nil) width(2) width(100%) 

*headers for each model
putdocx table mytable(1,1) = ("")
putdocx table mytable(1,2) = ("Intended consumption of red meat")
putdocx table mytable(1,3) = ("Intended consumption of other meat")


local covariates HT FR_dummy LV_dummy _cons
local nvars : word count `covariates'
local row 2  // Start from row 2 since row 1 has headers

forvalues covar_num = 1/`nvars' {
	local covar : word `covar_num' of `covariates'
	if "`covar'" == "_cons" {
		local covar_label Constant
		putdocx table mytable(`row',1) = ("`covar_label'")
	}
		else {
			 local covar_label : variable label `covar'
			 putdocx table mytable(`row',1) = ("`covar_label'"), italic
			 }
			
    * Extract coefficients and standard errors
    local b1_val: display %9.3f rtable1[1,`covar_num']
    local se1_val: display %9.3f rtable1[2,`covar_num']
    local p1_val: display %9.3f rtable1[4,`covar_num']
	    
    local b2_val: display %9.3f rtable2[1,`covar_num']
    local se2_val: display %9.3f rtable2[2,`covar_num']
    local p2_val: display %9.3f rtable2[4,`covar_num']
    
    * Adjusted p-values
	
	local wy_pvalue1: display %9.3f (wy_results[1, 4])
	local wy_pvalue2: display %9.3f (wy_results[2, 4])
		
    * Add stars based on p-values
    local stars1 = ""
    if `p1_val' < 0.001 {
        local stars1 = "***"
    }
    else if `p1_val' < 0.01 {
        local stars1 = "**"
    }
    else if `p1_val' < 0.05 {
        local stars1 = "*"
    }

    local stars2 = ""
    if `p2_val' < 0.001 {
        local stars2 = "***"
    }
    else if `p2_val' < 0.01 {
        local stars2 = "**"
    }
    else if `p2_val' < 0.05 {
        local stars2 = "*"
    }

	
	* Add stars_adj based on adjusted p-values
	if "`covar'" == "HT" {
		local stars_adj1 = ""
		if `wy_pvalue1' < 0.001 {
			local stars_adj1 = "+++"
		}
		else if `wy_pvalue1' < 0.01 {
			local stars_adj1 = "++"
		}
		else if `wy_pvalue1' < 0.05 {
			local stars_adj1 = "+"
		}

			local stars_adj2 = ""
		if `wy_pvalue2' < 0.001 {
			local stars_adj2 = "+++"
		}
		else if `wy_pvalue2' < 0.01 {
			local stars_adj2 = "++"
		}
		else if `wy_pvalue2' < 0.05 {
			local stars_adj2 = "+"
		}

	} 
	

		if "`covar'" == "HT" {
    * Insert coefficients, standard errors, and stars for each model
    putdocx table mytable(`row',2) = ("`=strltrim("`b1_val'")'`stars1'`stars_adj1'"), halign(left) linebreak
    putdocx table mytable(`row',2) = ("(`=strltrim("`se1_val'")')"), append linebreak
    putdocx table mytable(`row',3) = ("`=strltrim("`b2_val'")'`stars2'`stars_adj2'"), halign(left) linebreak
    putdocx table mytable(`row',3) = ("(`=strltrim("`se2_val'")')"), append linebreak
	
	}
	else {
    putdocx table mytable(`row',2) = ("`=strltrim("`b1_val'")'`stars1'`stars_adj1'"), halign(left) linebreak
    putdocx table mytable(`row',2) = ("(`=strltrim("`se1_val'")')"), append
    putdocx table mytable(`row',3) = ("`=strltrim("`b2_val'")'`stars2'`stars_adj2'"), halign(left) linebreak
    putdocx table mytable(`row',3) = ("(`=strltrim("`se2_val'")')"), append
	
	}
    
	
	
    * Increment the row counter for the next covariate
    local row = `row' + 1
	
	
	* Resetting stars to null
	local stars_adj1 = ""
	local stars_adj2 = ""
}

local wy_pvalue1: display %9.3f (wy_results[1, 4])
local wy_pvalue2: display %9.3f (wy_results[2, 4])

putdocx table mytable(2, 2) = ("[`=strltrim("`wy_pvalue1'")']"), append
putdocx table mytable(2, 3) = ("[`=strltrim("`wy_pvalue2'")']"), append

*adding observations
putdocx table mytable(`row',1) = ("N")
putdocx table mytable(`row',2) = (nobs1)
putdocx table mytable(`row',3) = (nobs2)

local row = `row' + 1
local row2 = `row' + 1

*Adding R² and adjusted R²
	
putdocx table mytable(`row',1) = ("R²")
putdocx table mytable(`row2',1) = ("R²adj")

	forvalues mod = 1/2 {		
		local col_num = `mod' + 1
			
		putdocx table mytable(`row',`col_num') = ("`=strltrim("`r2_`mod''")'")
		putdocx table mytable(`row2',`col_num') =("`=strltrim("`r2_adj_`mod''")'")
		
	}
			
	local row = `row' + 1

	* Adding  horizontal lines
	forvalues col_num = 1/3 {
		local row_2=`row'-2
		putdocx table mytable(1,`col_num'), border(top, "single")
		putdocx table mytable(1,`col_num'), border(bottom, "single")
		putdocx table mytable(`row_2',`col_num'), border(top, "single")
		putdocx table mytable(`row',`col_num'), border(bottom, "single")
	}


local row = `row' + 1

putdocx table mytable(`row',1) = ("Note: Robust standard errors in parentheses."), colspan(3) linebreak 
putdocx table mytable(`row',1) = ("For health-risk information, Westfall-Young MHT adjusted p-values in brackets with 10,000 bootstrap replications. "),  append linebreak 
putdocx table mytable(`row',1) = ("***p<0.001, **p<0.01, *p<0.05 for standard p-values, +++p< 0.001, ++p < 0.01, +p < 0.05 for Westfall-Young MHT adjusted p-values."),  append

}

putdocx pagebreak

local table_num  = `table_num' + 1

* ========================
**# Tables S2 to S7: Robustness test (heterogeneity) 1: Intended meat consumption
* ========================

qui {
foreach i in 1 2 3 4 5 6 {
    if `i'== 1 {
        local var1 food_depriv
        local var2 health_low_rec
        local var3 unemployed 
    }
    else if `i'== 2 {
        local var1 educ_low
        local var2 income_Q1
        local var3 age_low
    }
    else if `i'== 3 {
        local var1 female
        local var2 vegan_veg_pesc_dummy
        local var3 high_meat_dummy
    }               
    else if `i'== 4 {
        local var1 canteen_dummy
        local var2 po1_dummy
        local var3 po2_dummy
    } 
    else if `i'== 5 {
        local var1 po3_dummy
        local var2 po4_dummy
        local var3 po5_dummy
    }
    else if `i'== 6 {
        local var1 diet_knowl_high 
        local var2 cc_acc_high 
        local var3 soc_norm_high

    }
	
	local var1 = "`var1'"
	local var2 = "`var2'"
	local var3 = "`var3'"
	    
    quietly regress red_meat_dummy  1.HT `var1' 1.HT#1.`var1' FR_dummy LV_dummy , vce(hc3)
    matrix rtable1 = r(table)
    scalar nobs1 = e(N)
	local r2_1 = string(e(r2), "%9.3f")
	local r2_adj_1 = string(e(r2_a), "%9.3f")	

    quietly regress other_meat_dummy  1.HT `var1' 1.HT#1.`var1' FR_dummy LV_dummy, vce(hc3)
    matrix rtable2 = r(table)
    scalar nobs2 = e(N)
	local r2_2 = string(e(r2), "%9.3f")
	local r2_adj_2 = string(e(r2_a), "%9.3f")
	
	qui regress red_meat_dummy  1.HT `var2' 1.HT#1.`var2' FR_dummy LV_dummy , vce(hc3)
    matrix rtable3 = r(table)
    scalar nobs3 = e(N)
	local r2_3 = string(e(r2), "%9.3f")
	local r2_adj_3 = string(e(r2_a), "%9.3f")

    qui regress other_meat_dummy  1.HT `var2' 1.HT#1.`var2' FR_dummy LV_dummy, vce(hc3)
    matrix rtable4 = r(table)
    scalar nobs4 = e(N)
	local r2_4 = string(e(r2), "%9.3f")
	local r2_adj_4 = string(e(r2_a), "%9.3f")
	
    qui regress red_meat_dummy  1.HT `var3' 1.HT#1.`var3' FR_dummy LV_dummy , vce(hc3)
    matrix rtable5 = r(table)
    scalar nobs5 = e(N)
	local r2_5 = string(e(r2), "%9.3f")
	local r2_adj_5 = string(e(r2_a), "%9.3f")

    qui regress other_meat_dummy  1.HT `var3' 1.HT#1.`var3' FR_dummy LV_dummy, vce(hc3)
    matrix rtable6 = r(table)
    scalar nobs6 = e(N)
	local r2_6 = string(e(r2), "%9.3f")
	local r2_adj_6 = string(e(r2_a), "%9.3f")

    local file_path = "robustness_heterog/r2_meat_cons_`var1'.xlsx"
    
    // Check if the file exists
    if !fileexists("`file_path'") {
*Westfall-Young adjusted p-values
        wyoung, cmd("regress red_meat_dummy  1.HT `var1' 1.HT#1.`var1' FR_dummy LV_dummy, vce(hc3)" ///
                "regress other_meat_dummy  1.HT `var1' 1.HT#1.`var1' FR_dummy LV_dummy, vce(hc3)" ///
                "regress red_meat_dummy  1.HT `var1' 1.HT#1.`var1' FR_dummy LV_dummy, vce(hc3)" ///
                "regress other_meat_dummy  1.HT `var1' 1.HT#1.`var1' FR_dummy LV_dummy, vce(hc3)") ///
            familyp(1.HT 1.HT 1.HT+1.HT#1.`var1' 1.HT+1.HT#1.`var1') ///
        bootstraps(10000) seed(1234)
		matrix wy_results1 = r(table)
        putexcel set `file_path', replace
        putexcel A1=matrix(wy_results1), names
	}
	
	preserve
	import excel `file_path', sheet("Sheet1") firstrow clear
	mkmat coef stderr  p pwyoung pbonf psidak, matrix(wy_results1)
	restore

    local file_path = "robustness_heterog/r2_meat_cons_`var2'.xlsx"
    
    // Check if the file exists
    if !fileexists("`file_path'") {
*Westfall-Young adjusted p-values
        wyoung, cmd("regress red_meat_dummy  1.HT `var2' 1.HT#1.`var2' FR_dummy LV_dummy, vce(hc3)" ///
                "regress other_meat_dummy  1.HT `var2' 1.HT#1.`var2' FR_dummy LV_dummy, vce(hc3)" ///
                "regress red_meat_dummy  1.HT `var2' 1.HT#1.`var2' FR_dummy LV_dummy, vce(hc3)" ///
                "regress other_meat_dummy  1.HT `var2' 1.HT#1.`var2' FR_dummy LV_dummy, vce(hc3)") ///
            familyp(1.HT 1.HT 1.HT+1.HT#1.`var2' 1.HT+1.HT#1.`var2') ///
        bootstraps(10000) seed(1234)
		matrix wy_results2 = r(table)

        putexcel set `file_path', replace
        putexcel A1=matrix(wy_results2), names
	}
	
	preserve
	import excel `file_path', sheet("Sheet1") firstrow clear
	mkmat coef stderr  p pwyoung pbonf psidak, matrix(wy_results2)
	restore

    local file_path = "robustness_heterog/r2_meat_cons_`var3'.xlsx"
    
    // Check if the file exists
    if !fileexists("`file_path'") {
*Westfall-Young adjusted p-values
        wyoung, cmd("regress red_meat_dummy  1.HT `var3' 1.HT#1.`var3' FR_dummy LV_dummy, vce(hc3)" ///
                "regress other_meat_dummy  1.HT `var3' 1.HT#1.`var3' FR_dummy LV_dummy, vce(hc3)" ///
                "regress red_meat_dummy  1.HT `var3' 1.HT#1.`var3' FR_dummy LV_dummy, vce(hc3)" ///
                "regress other_meat_dummy  1.HT `var3' 1.HT#1.`var3' FR_dummy LV_dummy, vce(hc3)") ///
            familyp(1.HT 1.HT 1.HT+1.HT#1.`var3' 1.HT+1.HT#1.`var3') ///
        bootstraps(10000) seed(1234)
		matrix wy_results3 = r(table)

        putexcel set `file_path', replace
        putexcel A1=matrix(wy_results3), names
	}
	
	preserve
	
	
	import excel `file_path', sheet("Sheet1") firstrow clear
	mkmat coef stderr  p pwyoung pbonf psidak, matrix(wy_results3)
	
	restore

	local lbl1 : variable label `var1'
	local lbl2 : variable label `var2'
	local lbl3 : variable label `var3'	
	
	local low_lbl1 = lower("`lbl1'")
	local low_lbl2 = lower("`lbl2'")
	local low_lbl3 = lower("`lbl3'")
		
	*Add title
	putdocx paragraph
	putdocx text ("Table SC`table_num' LPM results of the heterogeneity models for ")
	putdocx text ("`low_lbl1'"), italic
	putdocx text (", ")
	putdocx text ("`low_lbl2'"), italic
	putdocx text (", and ")
	putdocx text ("`low_lbl3'"), italic
	putdocx text (", with intended meat consumption as the outcome variable - pooled sample."), font("", 11)


	*Create a table with 5 rows and 5 columns (1 for covariate names, 4 for models)
	putdocx table mytable = (12, 7), border(all, nil) width(4) width(100%) 
	putdocx table mytable(1,2) = ("`lbl1'"), colspan(2) italic
	putdocx table mytable(1,3) = ("`lbl2'"), colspan(2) italic
	putdocx table mytable(1,4) = ("`lbl3'"), colspan(2) italic

	*headers for each model
	putdocx table mytable(2,2) = ("Intended consumption of red meat")
	putdocx table mytable(2,3) = ("Intended consumption of other meat")
	putdocx table mytable(2,4) = ("Intended consumption of red meat")
	putdocx table mytable(2,5) = ("Intended consumption of other meat")
	putdocx table mytable(2,6) = ("Intended consumption of red meat")
	putdocx table mytable(2,7) = ("Intended consumption of other meat")


	local covariates HT "`var1'" "1.HT#1.`var1'" FR_dummy LV_dummy _cons
	local interaction_label "Health-risk information # heterogeneity variable"
	local het_label "Heterogeneity variable"
	
	local nvars : word count `covariates'
	local row 3  // Start from row 2 since row 1 has headers
	local rowse 4


  forvalues covar_num = 1/`nvars' {
	local covar : word `covar_num' of `covariates'
		if "`covar'"== "_cons"  {
			local covar_label Constant
			putdocx table mytable(`row',1) = ("`covar_label'")
			}	
			else {
				if "`covar'" == "1.HT#1.`var1'" | "`covar'" == "1.HT#1.`var2'" |  "`covar'" == "1.HT#1.`var3'" {
				local covar_label "`interaction_label'"
				putdocx table mytable(`row',1) = ("`covar_label'"), italic
				}
				else {
					if "`covar'" == "`var1'" | "`covar'" == "`var2'" | "`covar'" == "`var3'" {
					local covar_label "`het_label'"
					putdocx table mytable(`row',1) = ("`covar_label'"), italic
					} 
					else {
						local covar_label : variable label `covar'
						putdocx table mytable(`row',1) = ("`covar_label'"), italic
						}
					}
				}

		
		
		* Extract coefficients and standard errors
		local b1_val: display %9.3f rtable1[1,`covar_num']
		local se1_val: display %9.3f rtable1[2,`covar_num']
		local p1_val: display %9.3f rtable1[4,`covar_num']
		
		
		local b2_val: display %9.3f rtable2[1,`covar_num']
		local se2_val: display %9.3f rtable2[2,`covar_num']
		local p2_val: display %9.3f rtable2[4,`covar_num']
		
		local b3_val: display %9.3f rtable3[1,`covar_num']
		local se3_val: display %9.3f rtable3[2,`covar_num']
		local p3_val: display %9.3f rtable3[4,`covar_num']
		
		local b4_val: display %9.3f rtable4[1,`covar_num']
		local se4_val: display %9.3f rtable4[2,`covar_num']
		local p4_val: display %9.3f rtable4[4,`covar_num']
		
		local b5_val: display %9.3f rtable5[1,`covar_num']
		local se5_val: display %9.3f rtable5[2,`covar_num']
		local p5_val: display %9.3f rtable5[4,`covar_num']
		
		local b6_val: display %9.3f rtable6[1,`covar_num']
		local se6_val: display %9.3f rtable6[2,`covar_num']
		local p6_val: display %9.3f rtable6[4,`covar_num']
		

		* Adjusted p-values
		

	local wy_pvalue1: display %9.3f (wy_results1[1, 4])
	local wy_pvalue2: display %9.3f (wy_results1[2, 4])
	local wy_pvalue3: display %9.3f (wy_results2[1, 4])
	local wy_pvalue4: display %9.3f (wy_results2[2, 4])
	local wy_pvalue5: display %9.3f (wy_results3[1, 4])
	local wy_pvalue6: display %9.3f (wy_results3[2, 4])
		
				
		* Add stars based on p-values
        local stars1 = ""
        if `p1_val' < 0.001 local stars1 = "***"
        else if `p1_val' < 0.01 local stars1 = "**"
        else if `p1_val' < 0.05 local stars1 = "*"

        local stars2 = ""
        if `p2_val' < 0.001 local stars2 = "***"
        else if `p2_val' < 0.01 local stars2 = "**"
        else if `p2_val' < 0.05 local stars2 = "*"

        local stars3 = ""
        if `p3_val' < 0.001 local stars3 = "***"
        else if `p3_val' < 0.01 local stars3 = "**"
        else if `p3_val' < 0.05 local stars3 = "*"

        local stars4 = ""
        if `p4_val' < 0.001 local stars4 = "***"
        else if `p4_val' < 0.01 local stars4 = "**"
        else if `p4_val' < 0.05 local stars4 = "*"
		
		local stars5 = ""
		if `p5_val' < 0.001 local stars5 = "***"
		else if `p5_val' < 0.01 local stars5 = "**"
		else if `p5_val' < 0.05 local stars5 = "*"

		local stars6 = ""
		if `p6_val' < 0.001 local stars6 = "***"
		else if `p6_val' < 0.01 local stars6 = "**"
		else if `p6_val' < 0.05 local stars6 = "*"
		
		* Add stars_adj based on adjusted p-values
		if "`covar'" == "HT"  {
			local stars_adj1 = ""
			if `wy_pvalue1' < 0.001 {
				local stars_adj1 = "+++"
			}
			else if `wy_pvalue1' < 0.01 {
				local stars_adj1 = "++"
			}
			else if `wy_pvalue1' < 0.05 {
				local stars_adj1 = "+"
			}

			local stars_adj2 = ""
			if `wy_pvalue2' < 0.001 {
				local stars_adj2 = "+++"
			}
			else if `wy_pvalue2' < 0.01 {
				local stars_adj2 = "++"
			}
			else if `wy_pvalue2' < 0.05 {
				local stars_adj2 = "+"
			}

			local stars_adj3 = ""
			if `wy_pvalue3' < 0.001 {
				local stars_adj3 = "+++"
			}
			else if `wy_pvalue3' < 0.01 {
				local stars_adj3 = "++"
			}
			else if `wy_pvalue3' < 0.05 {
				local stars_adj3 = "+"
			}

			local stars_adj4 = ""
			if `wy_pvalue4' < 0.001 {
				local stars_adj4 = "+++"
			}
			else if `wy_pvalue4' < 0.01 {
				local stars_adj4 = "++"
			}
			else if `wy_pvalue4' < 0.05 {
				local stars_adj4 = "+"
			}

			local stars_adj5 = ""
			if `wy_pvalue5' < 0.001 {
				local stars_adj5 = "+++"
			}
			else if `wy_pvalue5' < 0.01 {
				local stars_adj5 = "++"
			}
			else if `wy_pvalue5' < 0.05 {
				local stars_adj5 = "+"
			}

			local stars_adj6 = ""
			if `wy_pvalue6' < 0.001 {
				local stars_adj6 = "+++"
			}
			else if `wy_pvalue6' < 0.01 {
				local stars_adj6 = "++"
			}
			else if `wy_pvalue6' < 0.05 {
				local stars_adj6 = "+"
			}
		}

		if "`covar'" == "HT" {
		* Insert coefficients, standard errors, and stars for each model
		putdocx table mytable(`row',2) = ("`=strltrim("`b1_val'")'`stars1'`stars_adj1'"), halign(left) linebreak
		putdocx table mytable(`row',2) = ("(`=strltrim("`se1_val'")')"), append linebreak
		putdocx table mytable(`row',3) = ("`=strltrim("`b2_val'")'`stars2'`stars_adj2'"), halign(left) linebreak
		putdocx table mytable(`row',3) = ("(`=strltrim("`se2_val'")')"), append linebreak
		putdocx table mytable(`row',4) = ("`=strltrim("`b3_val'")'`stars3'`stars_adj3'"), halign(left) linebreak
		putdocx table mytable(`row',4) = ("(`=strltrim("`se3_val'")')"), append linebreak
		putdocx table mytable(`row',5) = ("`=strltrim("`b4_val'")'`stars4'`stars_adj4'"), halign(left) linebreak
		putdocx table mytable(`row',5) = ("(`=strltrim("`se4_val'")')"), append linebreak
		putdocx table mytable(`row',6) = ("`=strltrim("`b5_val'")'`stars5'`stars_adj5'"), halign(left) linebreak
		putdocx table mytable(`row',6) = ("(`=strltrim("`se5_val'")')"), append linebreak
		putdocx table mytable(`row',7) = ("`=strltrim("`b6_val'")'`stars6'`stars_adj6'"), halign(left) linebreak
		putdocx table mytable(`row',7) = ("(`=strltrim("`se6_val'")')"), append linebreak
	
	}
	else {
		* Insert coefficients, standard errors, and stars for each model
		putdocx table mytable(`row',2) = ("`=strltrim("`b1_val'")'`stars1'`stars_adj1'"), halign(left) linebreak
		putdocx table mytable(`row',2) = ("(`=strltrim("`se1_val'")')"), append
		putdocx table mytable(`row',3) = ("`=strltrim("`b2_val'")'`stars2'`stars_adj2'"), halign(left) linebreak
		putdocx table mytable(`row',3) = ("(`=strltrim("`se2_val'")')"), append
		putdocx table mytable(`row',4) = ("`=strltrim("`b3_val'")'`stars3'`stars_adj3'"), halign(left) linebreak
		putdocx table mytable(`row',4) = ("(`=strltrim("`se3_val'")')"), append
		putdocx table mytable(`row',5) = ("`=strltrim("`b4_val'")'`stars4'`stars_adj4'"), halign(left) linebreak
		putdocx table mytable(`row',5) = ("(`=strltrim("`se4_val'")')"), append
		putdocx table mytable(`row',6) = ("`=strltrim("`b5_val'")'`stars5'`stars_adj5'"), halign(left) linebreak
		putdocx table mytable(`row',6) = ("(`=strltrim("`se5_val'")')"), append
		putdocx table mytable(`row',7) = ("`=strltrim("`b6_val'")'`stars6'`stars_adj6'"), halign(left) linebreak
		putdocx table mytable(`row',7) = ("(`=strltrim("`se6_val'")')"), append
	
	}
    


		
		
		* Increment the row counter for the next covariate
		local row = `row' + 1
		
		
		* Resetting stars to null
		local stars_adj1 ""
		local stars_adj2 ""
		local stars_adj3 ""
		local stars_adj4 ""
		local stars_adj5 ""
		local stars_adj6 ""
	}

	

	local wy_pvalue1: display %9.3f (wy_results1[1, 4])
	local wy_pvalue2: display %9.3f (wy_results1[2, 4])
	local wy_pvalue3: display %9.3f (wy_results2[1, 4])
	local wy_pvalue4: display %9.3f (wy_results2[2, 4])
	local wy_pvalue5: display %9.3f (wy_results3[1, 4])
	local wy_pvalue6: display %9.3f (wy_results3[2, 4])



	putdocx table mytable(3, 2) = ("[`=strltrim("`wy_pvalue1'")']"), append
	putdocx table mytable(3, 3) = ("[`=strltrim("`wy_pvalue2'")']"), append
	putdocx table mytable(3, 4) = ("[`=strltrim("`wy_pvalue3'")']"), append
	putdocx table mytable(3, 5) = ("[`=strltrim("`wy_pvalue4'")']"), append
	putdocx table mytable(3, 6) = ("[`=strltrim("`wy_pvalue5'")']"), append
	putdocx table mytable(3, 7) = ("[`=strltrim("`wy_pvalue6'")']"), append

	*adding observations

	putdocx table mytable(`row',1) = ("N")
	putdocx table mytable(`row',2) = (nobs1)
	putdocx table mytable(`row',3) = (nobs2)
	putdocx table mytable(`row',4) = (nobs3)
	putdocx table mytable(`row',5) = (nobs4)
	putdocx table mytable(`row',6) = (nobs5)
	putdocx table mytable(`row',7) = (nobs6)



	local row = `row' + 1
	local row2 = `row' + 1

	*Adding R² and adjusted R²
		
	putdocx table mytable(`row',1) = ("R²")
	putdocx table mytable(`row2',1) = ("R²adj")

	forvalues mod = 1/6 {		
		local col_num = `mod' + 1
		
		putdocx table mytable(`row',`col_num') = ("`=strltrim("`r2_`mod''")'")
		putdocx table mytable(`row2',`col_num') =("`=strltrim("`r2_adj_`mod''")'")
	}
	
	local row = `row' + 1

	* Adding  horizontal lines
	forvalues col_num = 1/7 {
		local row_2=`row'-2
		putdocx table mytable(1,`col_num'), border(top, "single")
		putdocx table mytable(2,`col_num'), border(bottom, "single")
		putdocx table mytable(`row_2',`col_num'), border(top, "single")
		putdocx table mytable(`row',`col_num'), border(bottom, "single")
	}
	forvalues col_num = 2/7 {
		putdocx table mytable(1,`col_num'), border(bottom, "single")
	}

	
	
	local row = `row' + 1
	
	local table_num  = `table_num' + 1
putdocx table mytable(`row',1) = ("Note: Robust standard errors in parentheses."), colspan(7) linebreak 
putdocx table mytable(`row',1) = ("For health-risk information, Westfall-Young MHT adjusted p-values in brackets with 10,000 bootstrap replications. "),  append linebreak 
putdocx table mytable(`row',1) = ("***p<0.001, **p<0.01, *p<0.05 for standard p-values, +++p< 0.001, ++p < 0.01, +p < 0.05 for Westfall-Young MHT adjusted p-values."),  append

putdocx pagebreak
}

}

* ========================
**# Table SC8: Robustness test (basic) 2: Recoded outcome variable
* ========================

qui {
	
qui regress meat_tax_very  1.HT FR_dummy LV_dummy, vce(hc3)
matrix rtable1=r(table)
scalar nobs1 = e(N)
local r2_1 = string(e(r2), "%9.3f")
local r2_adj_1 = string(e(r2_a), "%9.3f")

qui regress meat_tax_very_soc  1.HT FR_dummy LV_dummy, vce(hc3)
matrix rtable2=r(table)
scalar nobs2 = e(N)
local r2_2 = string(e(r2), "%9.3f")
local r2_adj_2 = string(e(r2_a), "%9.3f")

qui regress meat_day_very 1.HT FR_dummy LV_dummy, vce(hc3)
matrix rtable3=r(table)
scalar nobs3 = e(N)
local r2_3 = string(e(r2), "%9.3f")
local r2_adj_3 = string(e(r2_a), "%9.3f")

qui regress meat_day_very_soc  1.HT FR_dummy LV_dummy, vce(hc3)
matrix rtable4=r(table)
scalar nobs4 = e(N)
local r2_4 = string(e(r2), "%9.3f")
local r2_adj_4 = string(e(r2_a), "%9.3f")


local file_path = "robustness_main/r8_very.xlsx"
    
if !fileexists("`file_path'") {
*Westfall-Young adjusted p-values
wyoung, cmd("regress meat_tax_very  1.HT FR_dummy LV_dummy, vce(hc3)" ///
            "regress meat_tax_very_soc  1.HT FR_dummy LV_dummy, vce(hc3)" ///
            "regress meat_day_very 1.HT FR_dummy LV_dummy, vce(hc3)" ///
            "regress meat_day_very_soc  1.HT FR_dummy LV_dummy, vce(hc3)" ///) ///
familyp("HT" "HT" "HT" "HT") familypexp strata(country) bootstraps(10000) seed(1234)
    
matrix wy_results = r(table)
putexcel set `file_path', replace
putexcel A1=matrix(wy_results), names
}
	
preserve

import excel `file_path', sheet("Sheet1") firstrow clear
mkmat coef stderr  p pwyoung pbonf psidak, matrix(wy_results)

restore

	
*Add title
putdocx paragraph
putdocx text ("Table SC`table_num' LPM results of the basic models, recoded outcome variables to test for a ceiling effect - pooled sample."), font("", 11)	

*Create a table with 5 rows and 5 columns (1 for covariate names, 4 for models)
putdocx table mytable = (9, 5), border(all, nil) width(4) width(100%) 

*headers for each model
putdocx table mytable(1,1) = ("")
putdocx table mytable(1,2) = ("Meat tax acceptability for oneself")
putdocx table mytable(1,3) = ("Meat Tax acceptability for society")
putdocx table mytable(1,4) = ("Meat-free days acceptability for oneself")
putdocx table mytable(1,5) = ("Meat-free days acceptability for society")



local covariates HT FR_dummy LV_dummy _cons
local nvars : word count `covariates'
local row 2  // Start from row 2 since row 1 has headers
local rowse 3

forvalues covar_num = 1/`nvars' {
	local covar : word `covar_num' of `covariates'
	if "`covar'" == "_cons" {
		local covar_label Constant
		putdocx table mytable(`row',1) = ("`covar_label'")
	}
		else {
			 local covar_label : variable label `covar'
			 putdocx table mytable(`row',1) = ("`covar_label'"), italic
			 }
		
	

	
    * Extract coefficients and standard errors
    local b1_val: display %9.3f rtable1[1,`covar_num']
    local se1_val: display %9.3f rtable1[2,`covar_num']
    local p1_val: display %9.3f rtable1[4,`covar_num']
	
    
    local b2_val: display %9.3f rtable2[1,`covar_num']
    local se2_val: display %9.3f rtable2[2,`covar_num']
    local p2_val: display %9.3f rtable2[4,`covar_num']
    
    local b3_val: display %9.3f rtable3[1,`covar_num']
    local se3_val: display %9.3f rtable3[2,`covar_num']
    local p3_val: display %9.3f rtable3[4,`covar_num']
    
    local b4_val: display %9.3f rtable4[1,`covar_num']
    local se4_val: display %9.3f rtable4[2,`covar_num']
    local p4_val: display %9.3f rtable4[4,`covar_num']

    * Adjusted p-values
	
	local wy_pvalue1: display %9.3f (wy_results[1, 4])
	local wy_pvalue2: display %9.3f (wy_results[2, 4])
	local wy_pvalue3: display %9.3f (wy_results[3, 4])
	local wy_pvalue4: display %9.3f (wy_results[4, 4])

	
	
    * Add stars based on p-values
    local stars1 = ""
    if `p1_val' < 0.001 {
        local stars1 = "***"
    }
    else if `p1_val' < 0.01 {
        local stars1 = "**"
    }
    else if `p1_val' < 0.05 {
        local stars1 = "*"
    }

    local stars2 = ""
    if `p2_val' < 0.001 {
        local stars2 = "***"
    }
    else if `p2_val' < 0.01 {
        local stars2 = "**"
    }
    else if `p2_val' < 0.05 {
        local stars2 = "*"
    }

    local stars3 = ""
    if `p3_val' < 0.001 {
        local stars3 = "***"
    }
    else if `p3_val' < 0.01 {
        local stars3 = "**"
    }
    else if `p3_val' < 0.05 {
        local stars3 = "*"
    }

    local stars4 = ""
    if `p4_val' < 0.001 {
        local stars4 = "***"
    }
    else if `p4_val' < 0.01 {
        local stars4 = "**"
    }
    else if `p4_val' < 0.05 {
        local stars4 = "*"
    }
	

	
	* Add stars_adj based on adjusted p-values
	if "`covar'" == "HT" {
		local stars_adj1 = ""
		if `wy_pvalue1' < 0.001 {
			local stars_adj1 = "+++"
		}
		else if `wy_pvalue1' < 0.01 {
			local stars_adj1 = "++"
		}
		else if `wy_pvalue1' < 0.05 {
			local stars_adj1 = "+"
		}

			local stars_adj2 = ""
		if `wy_pvalue2' < 0.001 {
			local stars_adj2 = "+++"
		}
		else if `wy_pvalue2' < 0.01 {
			local stars_adj2 = "++"
		}
		else if `wy_pvalue2' < 0.05 {
			local stars_adj2 = "+"
		}

			local stars_adj3 = ""
		if `wy_pvalue3' < 0.001 {
			local stars_adj3 = "+++"
		}
		else if `wy_pvalue3' < 0.01 {
			local stars_adj3 = "++"
		}
		else if `wy_pvalue3' < 0.05 {
			local stars_adj3 = "+"
		}

			local stars_adj4 = ""
		if `wy_pvalue4' < 0.001 {
			local stars_adj4 = "+++"
		}
		else if `wy_pvalue4' < 0.01 {
			local stars_adj4 = "++"
		}
		else if `wy_pvalue4' < 0.05 {
			local stars_adj4 = "+"
		}
	} 
	


    
		if "`covar'" == "HT" {
		* Insert coefficients, standard errors, and stars for each model
		putdocx table mytable(`row',2) = ("`=strltrim("`b1_val'")'`stars1'`stars_adj1'"), halign(left) linebreak
		putdocx table mytable(`row',2) = ("(`=strltrim("`se1_val'")')"), append linebreak
		putdocx table mytable(`row',3) = ("`=strltrim("`b2_val'")'`stars2'`stars_adj2'"), halign(left) linebreak
		putdocx table mytable(`row',3) = ("(`=strltrim("`se2_val'")')"), append linebreak
		putdocx table mytable(`row',4) = ("`=strltrim("`b3_val'")'`stars3'`stars_adj3'"), halign(left) linebreak
		putdocx table mytable(`row',4) = ("(`=strltrim("`se3_val'")')"), append linebreak
		putdocx table mytable(`row',5) = ("`=strltrim("`b4_val'")'`stars4'`stars_adj4'"), halign(left) linebreak
		putdocx table mytable(`row',5) = ("(`=strltrim("`se4_val'")')"), append linebreak
	
	}
	else {
		* Insert coefficients, standard errors, and stars for each model
		putdocx table mytable(`row',2) = ("`=strltrim("`b1_val'")'`stars1'`stars_adj1'"), halign(left) linebreak
		putdocx table mytable(`row',2) = ("(`=strltrim("`se1_val'")')"), append
		putdocx table mytable(`row',3) = ("`=strltrim("`b2_val'")'`stars2'`stars_adj2'"), halign(left) linebreak
		putdocx table mytable(`row',3) = ("(`=strltrim("`se2_val'")')"), append
		putdocx table mytable(`row',4) = ("`=strltrim("`b3_val'")'`stars3'`stars_adj3'"), halign(left) linebreak
		putdocx table mytable(`row',4) = ("(`=strltrim("`se3_val'")')"), append
		putdocx table mytable(`row',5) = ("`=strltrim("`b4_val'")'`stars4'`stars_adj4'"), halign(left) linebreak
		putdocx table mytable(`row',5) = ("(`=strltrim("`se4_val'")')"), append
	
	}
    


    * Increment the row counter for the next covariate
    local row = `row' + 1
	
	
	* Resetting stars to null
	local stars_adj1 = ""
	local stars_adj2 = ""
	local stars_adj3 = ""
	local stars_adj4 = ""
}



local wy_pvalue1: display %9.3f (wy_results[1, 4])
local wy_pvalue2: display %9.3f (wy_results[2, 4])
local wy_pvalue3: display %9.3f (wy_results[3, 4])
local wy_pvalue4: display %9.3f (wy_results[4, 4])



putdocx table mytable(2, 2) = ("[`=strltrim("`wy_pvalue1'")']"), append
putdocx table mytable(2, 3) = ("[`=strltrim("`wy_pvalue2'")']"), append
putdocx table mytable(2, 4) = ("[`=strltrim("`wy_pvalue3'")']"), append
putdocx table mytable(2, 5) = ("[`=strltrim("`wy_pvalue4'")']"), append

*adding observations

putdocx table mytable(`row',1) = ("N")
putdocx table mytable(`row',2) = (nobs1)
putdocx table mytable(`row',3) = (nobs2)
putdocx table mytable(`row',4) = (nobs3)
putdocx table mytable(`row',5) = (nobs4)


local row = `row' + 1
local row2 = `row' + 1

*Adding R² and adjusted R²
	
putdocx table mytable(`row',1) = ("R²")
putdocx table mytable(`row2',1) = ("R²adj")

	forvalues mod = 1/4 {		
		local col_num = `mod' + 1
		
		putdocx table mytable(`row',`col_num') = ("`=strltrim("`r2_`mod''")'")
		putdocx table mytable(`row2',`col_num') =("`=strltrim("`r2_adj_`mod''")'")
	}
	
	local row = `row' + 1

	* Adding  horizontal lines
	forvalues col_num = 1/5 {
		local row_2=`row'-2
		putdocx table mytable(1,`col_num'), border(top, "single")
		putdocx table mytable(1,`col_num'), border(bottom, "single")
		putdocx table mytable(`row_2',`col_num'), border(top, "single")
		putdocx table mytable(`row',`col_num'), border(bottom, "single")
	}

local row = `row' + 1

putdocx table mytable(`row',1) = ("Note: Robust standard errors in parentheses."), colspan(5) linebreak 
putdocx table mytable(`row',1) = ("For health-risk information, Westfall-Young MHT adjusted p-values in brackets with 10,000 bootstrap replications. "),  append linebreak 
putdocx table mytable(`row',1) = ("***p<0.001, **p<0.01, *p<0.05 for standard p-values, +++p< 0.001, ++p < 0.01, +p < 0.05 for Westfall-Young MHT adjusted p-values."),  append

putdocx pagebreak
}


local table_num  = `table_num' + 1



* ========================
**# Tables S9 to S14: Robustness test (heterogeneity) 2: Recoded outcome variable
* ========================

qui {

foreach i in 1 2 3 4 5 6 {
    if `i'== 1 {
        local var1 food_depriv
        local var2 health_low_rec
        local var3 unemployed 
    }
    else if `i'== 2 {
        local var1 educ_low
        local var2 income_Q1
        local var3 age_low
    }
    else if `i'== 3 {
        local var1 female
        local var2 vegan_veg_pesc_dummy
        local var3 high_meat_dummy
    }               
    else if `i'== 4 {
        local var1 canteen_dummy
        local var2 po1_dummy
        local var3 po2_dummy
    } 
    else if `i'== 5 {
        local var1 po3_dummy
        local var2 po4_dummy
        local var3 po5_dummy
    }
    else if `i'== 6 {
        local var1 diet_knowl_high 
        local var2 cc_acc_high 
        local var3 soc_norm_high

    }
	
	local var1 = "`var1'"
	local var2 = "`var2'"
	local var3 = "`var3'"
	

	* coeff and N for var1
    qui regress meat_tax_very 1.HT `var1' 1.HT#1.`var1' FR_dummy LV_dummy, vce(hc3)
    matrix rtable1 = r(table)
    scalar nobs1 = e(N)
	local r2_1 = string(e(r2), "%9.3f")
	local r2_adj_1 = string(e(r2_a), "%9.3f")
    qui regress meat_tax_very_soc 1.HT `var1' 1.HT#1.`var1' FR_dummy LV_dummy, vce(hc3)
    matrix rtable2 = r(table)
    scalar nobs2 = e(N)
	local r2_2 = string(e(r2), "%9.3f")
	local r2_adj_2 = string(e(r2_a), "%9.3f")
    qui regress meat_day_very 1.HT `var1' 1.HT#1.`var1' FR_dummy LV_dummy, vce(hc3)
    matrix rtable3 = r(table)
    scalar nobs3 = e(N)
	local r2_3 = string(e(r2), "%9.3f")
	local r2_adj_3 = string(e(r2_a), "%9.3f")
    qui regress meat_day_very_soc 1.HT `var1' 1.HT#1.`var1' FR_dummy LV_dummy, vce(hc3)
    matrix rtable4 = r(table)
    scalar nobs4 = e(N)
	local r2_4 = string(e(r2), "%9.3f")
	local r2_adj_4 = string(e(r2_a), "%9.3f")
	* coeff and N for var2
	qui regress meat_tax_very 1.HT `var2' 1.HT#1.`var2' FR_dummy LV_dummy, vce(hc3)
    matrix rtable5 = r(table)
    scalar nobs5 = e(N)
	local r2_5 = string(e(r2), "%9.3f")
	local r2_adj_5 = string(e(r2_a), "%9.3f")
    qui regress meat_tax_very_soc 1.HT `var2' 1.HT#1.`var2' FR_dummy LV_dummy, vce(hc3)
    matrix rtable6 = r(table)
    scalar nobs6 = e(N)
	local r2_6 = string(e(r2), "%9.3f")
	local r2_adj_6 = string(e(r2_a), "%9.3f")
    qui regress meat_day_very 1.HT `var2' 1.HT#1.`var2' FR_dummy LV_dummy, vce(hc3)
    matrix rtable7 = r(table)
    scalar nobs7 = e(N)
	local r2_7 = string(e(r2), "%9.3f")
	local r2_adj_7 = string(e(r2_a), "%9.3f")
    qui regress meat_day_very_soc 1.HT `var2' 1.HT#1.`var2' FR_dummy LV_dummy, vce(hc3)
    matrix rtable8 = r(table)
    scalar nobs8 = e(N)
	local r2_8 = string(e(r2), "%9.3f")
	local r2_adj_8 = string(e(r2_a), "%9.3f")
	* coeff and N for var3	
	qui regress meat_tax_very 1.HT `var3' 1.HT#1.`var3' FR_dummy LV_dummy, vce(hc3)
    matrix rtable9 = r(table)
    scalar nobs9 = e(N)
	local r2_9 = string(e(r2), "%9.3f")
	local r2_adj_9 = string(e(r2_a), "%9.3f")
    qui regress meat_tax_very_soc 1.HT `var3' 1.HT#1.`var3' FR_dummy LV_dummy, vce(hc3)
    matrix rtable10 = r(table)
    scalar nobs10 = e(N)
	local r2_10 = string(e(r2), "%9.3f")
	local r2_adj_10 = string(e(r2_a), "%9.3f")
    qui regress meat_day_very 1.HT `var3' 1.HT#1.`var3' FR_dummy LV_dummy, vce(hc3)
    matrix rtable11 = r(table)
    scalar nobs11 = e(N)
	local r2_11 = string(e(r2), "%9.3f")
	local r2_adj_11 = string(e(r2_a), "%9.3f")
    qui regress meat_day_very_soc 1.HT `var3' 1.HT#1.`var3' FR_dummy LV_dummy, vce(hc3)
    matrix rtable12 = r(table)
    scalar nobs12 = e(N)
	local r2_12 = string(e(r2), "%9.3f")
	local r2_adj_12 = string(e(r2_a), "%9.3f")

	local nb 10000 //number of bootsraps

    local file_path = "robustness_heterog/r8_very`var1'.xlsx"
    
    // Check if the file exists
    if !fileexists("`file_path'") {
    * Westfall-Young adjusted p-values for variable 1
    qui wyoung, cmd("regress meat_tax_very 1.HT `var1' 1.HT#1.`var1' FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_tax_very_soc 1.HT `var1' 1.HT#1.`var1' FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_day_very 1.HT `var1' 1.HT#1.`var1' FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_day_very_soc 1.HT `var1' 1.HT#1.`var1' FR_dummy LV_dummy, vce(hc3)"  ///
		"regress meat_tax_very 1.HT `var1' 1.HT#1.`var1' FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_tax_very_soc 1.HT `var1' 1.HT#1.`var1' FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_day_very 1.HT `var1' 1.HT#1.`var1' FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_day_very_soc 1.HT `var1' 1.HT#1.`var1' FR_dummy LV_dummy, vce(hc3)" ) ///
        familyp(1.HT 1.HT 1.HT 1.HT 1.HT+1.HT#1.`var1' 1.HT+1.HT#1.`var1' 1.HT+1.HT#1.`var1' 1.HT+1.HT#1.`var1') ///
        familypexp strata(country) ///
        bootstraps(`nb') seed(1234)
    
	matrix wy_results1 = r(table)
	putexcel set `file_path', replace
    putexcel A1=matrix(wy_results1), names
	
	}
		
	preserve
	
	import excel `file_path', sheet("Sheet1") firstrow clear
	mkmat coef stderr  p pwyoung pbonf psidak, matrix(wy_results1)
	

	restore
		
		
    local file_path = "robustness_heterog/r8_very`var2'.xlsx"
	    * Westfall-Young adjusted p-values for variable 2
        if !fileexists("`file_path'") {
			qui wyoung, cmd("regress meat_tax_very 1.HT `var2' 1.HT#1.`var2' FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_tax_very_soc 1.HT `var2' 1.HT#1.`var2' FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_day_very 1.HT `var2' 1.HT#1.`var2' FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_day_very_soc 1.HT `var2' 1.HT#1.`var2' FR_dummy LV_dummy, vce(hc3)"  ///
		"regress meat_tax_very 1.HT `var2' 1.HT#1.`var2' FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_tax_very_soc 1.HT `var2' 1.HT#1.`var2' FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_day_very 1.HT `var2' 1.HT#1.`var2' FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_day_very_soc 1.HT `var2' 1.HT#1.`var2' FR_dummy LV_dummy, vce(hc3)" ) ///
        familyp(1.HT 1.HT 1.HT 1.HT 1.HT+1.HT#1.`var2' 1.HT+1.HT#1.`var2' 1.HT+1.HT#1.`var2' 1.HT+1.HT#1.`var2') ///
        familypexp strata(country) ///
        bootstraps(`nb') seed(1234)
    
	matrix wy_results2 = r(table)	
	putexcel set `file_path', replace
    putexcel A1=matrix(wy_results2), names
		}
	
	preserve
	
	import excel `file_path', sheet("Sheet1") firstrow clear
	mkmat coef stderr  p pwyoung pbonf psidak, matrix(wy_results2)


	restore
	
    local file_path = "robustness_heterog/r8_very`var3'.xlsx"
		* Westfall-Young adjusted p-values for variable 3
    if !fileexists("`file_path'") {
		qui wyoung, cmd("regress meat_tax_very 1.HT `var3' 1.HT#1.`var3' FR_dummy LV_dummy,vce(hc3)" ///
        "regress meat_tax_very_soc 1.HT `var3' 1.HT#1.`var3' FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_day_very 1.HT `var3' 1.HT#1.`var3' FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_day_very_soc 1.HT `var3' 1.HT#1.`var3' FR_dummy LV_dummy, vce(hc3)"  ///
		"regress meat_tax_very 1.HT `var3' 1.HT#1.`var3' FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_tax_very_soc 1.HT `var3' 1.HT#1.`var3' FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_day_very 1.HT `var3' 1.HT#1.`var3' FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_day_very_soc 1.HT `var3' 1.HT#1.`var3' FR_dummy LV_dummy, vce(hc3)" ) ///
        familyp(1.HT 1.HT 1.HT 1.HT 1.HT+1.HT#1.`var3' 1.HT+1.HT#1.`var3' 1.HT+1.HT#1.`var3' 1.HT+1.HT#1.`var3') ///
        familypexp strata(country) ///
        bootstraps(`nb') seed(1234)
    
	matrix wy_results3 = r(table)
	putexcel set `file_path', replace
    putexcel A1=matrix(wy_results3), names
	}
		
	preserve
	
	import excel `file_path', sheet("Sheet1") firstrow clear
	
	mkmat coef stderr  p pwyoung pbonf psidak, matrix(wy_results3)


	restore
			
		
	local lbl1 : variable label `var1'
	local lbl2 : variable label `var2'
	local lbl3 : variable label `var3'	
	
	local low_lbl1 = lower("`lbl1'")
	local low_lbl2 = lower("`lbl2'")
	local low_lbl3 = lower("`lbl3'")
	
    * Add title
    putdocx paragraph
    putdocx text ("Table SC`table_num' LPM results of the heterogeneity models for ")
	putdocx text ("`low_lbl1'"), italic
	putdocx text (", ")
	putdocx text ("`low_lbl2'"), italic
	putdocx text (", and ")
	putdocx text ("`low_lbl3'"), italic
	putdocx text (", recoded outcome variables to test for a ceiling effect - pooled sample."), font("", 11)	
	
	
    putdocx table mytable = (12, 13), border(all, nil) width(4) width(100%) 
	


    * Create a table with 5 rows and 5 columns (1 for covariate names, 4 for models)
	putdocx table mytable(1,2) = ("`lbl1'"), colspan(4) italic
	putdocx table mytable(1,3) = ("`lbl2'"), colspan(4) italic
	putdocx table mytable(1,4) = ("`lbl3'"), colspan(4) italic
    * Headers for each model
    putdocx table mytable(2,2) = ("Meat tax acceptability for oneself")
    putdocx table mytable(2,3) = ("Meat tax acceptability for society")
    putdocx table mytable(2,4) = ("Meat-free days acceptability for oneself")
    putdocx table mytable(2,5) = ("Meat-free days acceptability for society")
    putdocx table mytable(2,6) = ("Meat tax acceptability for oneself")
    putdocx table mytable(2,7) = ("Meat tax acceptability for society")
    putdocx table mytable(2,8) = ("Meat-free days acceptability for oneself")
    putdocx table mytable(2,9) = ("Meat-free days acceptability for society")
    putdocx table mytable(2,10) = ("Meat tax acceptability for oneself")
    putdocx table mytable(2,11) = ("Meat tax acceptability for society")
    putdocx table mytable(2,12) = ("Meat-free days acceptability for oneself")
    putdocx table mytable(2,13) = ("Meat-free days acceptability for society")


	
    local covariates HT "`var1'" "1.HT#1.`var1'" FR_dummy LV_dummy _cons
	local interaction_label "Health-risk information # heterogeneity variable"
	local het_label "Heterogeneity variable"

    local nvars : word count `covariates'
	local row 3
    local rowse 4

	//di `nvars'
	//di `covariates'
	//matrix list rtable1

  forvalues covar_num = 1/`nvars' {
	local covar : word `covar_num' of `covariates'
		if "`covar'"== "_cons"  {
			local covar_label Constant
			putdocx table mytable(`row',1) = ("`covar_label'")
			}	
			else {
				if "`covar'" == "1.HT#1.`var1'" | "`covar'" == "1.HT#1.`var2'" |  "`covar'" == "1.HT#1.`var3'" {
				local covar_label "`interaction_label'"
				putdocx table mytable(`row',1) = ("`covar_label'"), italic
				}
				else {
					if "`covar'" == "`var1'" | "`covar'" == "`var2'" | "`covar'" == "`var3'" {
					local covar_label "`het_label'"
					putdocx table mytable(`row',1) = ("`covar_label'"), italic
					} 
					else {
						local covar_label : variable label `covar'
						putdocx table mytable(`row',1) = ("`covar_label'"), italic
						}
					}
				}

 
					

        * Extract coefficients and standard errors
		local b1_val: display %9.3f rtable1[1,`covar_num']
		local se1_val: display %9.3f rtable1[2,`covar_num']
		local p1_val: display %9.3f rtable1[4,`covar_num']
		

		local b2_val: display %9.3f rtable2[1,`covar_num']
		local se2_val: display %9.3f rtable2[2,`covar_num']
		local p2_val: display %9.3f rtable2[4,`covar_num']
		
		local b3_val: display %9.3f rtable3[1,`covar_num']
		local se3_val: display %9.3f rtable3[2,`covar_num']
		local p3_val: display %9.3f rtable3[4,`covar_num']
		
		local b4_val: display %9.3f rtable4[1,`covar_num']
		local se4_val: display %9.3f rtable4[2,`covar_num']
		local p4_val: display %9.3f rtable4[4,`covar_num']

		local b5_val: display %9.3f rtable5[1,`covar_num']
		local se5_val: display %9.3f rtable5[2,`covar_num']
		local p5_val: display %9.3f rtable5[4,`covar_num']
		
		
		local b6_val: display %9.3f rtable6[1,`covar_num']
		local se6_val: display %9.3f rtable6[2,`covar_num']
		local p6_val: display %9.3f rtable6[4,`covar_num']
		
		local b7_val: display %9.3f rtable7[1,`covar_num']
		local se7_val: display %9.3f rtable7[2,`covar_num']
		local p7_val: display %9.3f rtable7[4,`covar_num']
		
		local b8_val: display %9.3f rtable8[1,`covar_num']
		local se8_val: display %9.3f rtable8[2,`covar_num']
		local p8_val: display %9.3f rtable8[4,`covar_num']
		
		local b9_val: display %9.3f rtable9[1,`covar_num']
		local se9_val: display %9.3f rtable9[2,`covar_num']
		local p9_val: display %9.3f rtable9[4,`covar_num']
		
		
		local b10_val: display %9.3f rtable10[1,`covar_num']
		local se10_val: display %9.3f rtable10[2,`covar_num']
		local p10_val: display %9.3f rtable10[4,`covar_num']
		
		local b11_val: display %9.3f rtable11[1,`covar_num']
		local se11_val: display %9.3f rtable11[2,`covar_num']
		local p11_val: display %9.3f rtable11[4,`covar_num']
		
		local b12_val: display %9.3f rtable12[1,`covar_num']
		local se12_val: display %9.3f rtable12[2,`covar_num']
		local p12_val: display %9.3f rtable12[4,`covar_num']
		* Adjusted p-values
		
		local wy_pvalue1: display %9.3f (wy_results1[1, 4])
		local wy_pvalue2: display %9.3f (wy_results1[2, 4])
		local wy_pvalue3: display %9.3f (wy_results1[3, 4])
		local wy_pvalue4: display %9.3f (wy_results1[4, 4])
		local wy_pvalue5: display %9.3f (wy_results2[1, 4])
		local wy_pvalue6: display %9.3f (wy_results2[2, 4])
		local wy_pvalue7: display %9.3f (wy_results2[3, 4])
		local wy_pvalue8: display %9.3f (wy_results2[4, 4])
		local wy_pvalue9: display %9.3f (wy_results3[1, 4])
		local wy_pvalue10: display %9.3f (wy_results3[2, 4])
		local wy_pvalue11: display %9.3f (wy_results3[3, 4])
		local wy_pvalue12: display %9.3f (wy_results3[4, 4])
	


        * Add stars based on p-values
        local stars1 = ""
        if `p1_val' < 0.001 local stars1 = "***"
        else if `p1_val' < 0.01 local stars1 = "**"
        else if `p1_val' < 0.05 local stars1 = "*"

        local stars2 = ""
        if `p2_val' < 0.001 local stars2 = "***"
        else if `p2_val' < 0.01 local stars2 = "**"
        else if `p2_val' < 0.05 local stars2 = "*"

        local stars3 = ""
        if `p3_val' < 0.001 local stars3 = "***"
        else if `p3_val' < 0.01 local stars3 = "**"
        else if `p3_val' < 0.05 local stars3 = "*"

        local stars4 = ""
        if `p4_val' < 0.001 local stars4 = "***"
        else if `p4_val' < 0.01 local stars4 = "**"
        else if `p4_val' < 0.05 local stars4 = "*"
		
		local stars5 = ""
		if `p5_val' < 0.001 local stars5 = "***"
		else if `p5_val' < 0.01 local stars5 = "**"
		else if `p5_val' < 0.05 local stars5 = "*"

		local stars6 = ""
		if `p6_val' < 0.001 local stars6 = "***"
		else if `p6_val' < 0.01 local stars6 = "**"
		else if `p6_val' < 0.05 local stars6 = "*"

		local stars7 = ""
		if `p7_val' < 0.001 local stars7 = "***"
		else if `p7_val' < 0.01 local stars7 = "**"
		else if `p7_val' < 0.05 local stars7 = "*"

		local stars8 = ""
		if `p8_val' < 0.001 local stars8 = "***"
		else if `p8_val' < 0.01 local stars8 = "**"
		else if `p8_val' < 0.05 local stars8 = "*"

		local stars9 = ""
		if `p9_val' < 0.001 local stars9 = "***"
		else if `p9_val' < 0.01 local stars9 = "**"
		else if `p9_val' < 0.05 local stars9 = "*"

		local stars10 = ""
		if `p10_val' < 0.001 local stars10 = "***"
		else if `p10_val' < 0.01 local stars10 = "**"
		else if `p10_val' < 0.05 local stars10 = "*"

		local stars11 = ""
		if `p11_val' < 0.001 local stars11 = "***"
		else if `p11_val' < 0.01 local stars11 = "**"
		else if `p11_val' < 0.05 local stars11 = "*"

		local stars12 = ""
		if `p12_val' < 0.001 local stars12 = "***"
		else if `p12_val' < 0.01 local stars12 = "**"
		else if `p12_val' < 0.05 local stars12 = "*"


        * Adjusted p-values for HT and HT#var variable
		if "`covar'" == "HT"  {
			local stars_adj1 = ""
			if `wy_pvalue1' < 0.001 {
				local stars_adj1 = "+++"
			}
			else if `wy_pvalue1' < 0.01 {
				local stars_adj1 = "++"
			}
			else if `wy_pvalue1' < 0.05 {
				local stars_adj1 = "+"
			}

			local stars_adj2 = ""
			if `wy_pvalue2' < 0.001 {
				local stars_adj2 = "+++"
			}
			else if `wy_pvalue2' < 0.01 {
				local stars_adj2 = "++"
			}
			else if `wy_pvalue2' < 0.05 {
				local stars_adj2 = "+"
			}

			local stars_adj3 = ""
			if `wy_pvalue3' < 0.001 {
				local stars_adj3 = "+++"
			}
			else if `wy_pvalue3' < 0.01 {
				local stars_adj3 = "++"
			}
			else if `wy_pvalue3' < 0.05 {
				local stars_adj3 = "+"
			}

			local stars_adj4 = ""
			if `wy_pvalue4' < 0.001 {
				local stars_adj4 = "+++"
			}
			else if `wy_pvalue4' < 0.01 {
				local stars_adj4 = "++"
			}
			else if `wy_pvalue4' < 0.05 {
				local stars_adj4 = "+"
			}

			local stars_adj5 = ""
			if `wy_pvalue5' < 0.001 {
				local stars_adj5 = "+++"
			}
			else if `wy_pvalue5' < 0.01 {
				local stars_adj5 = "++"
			}
			else if `wy_pvalue5' < 0.05 {
				local stars_adj5 = "+"
			}

			local stars_adj6 = ""
			if `wy_pvalue6' < 0.001 {
				local stars_adj6 = "+++"
			}
			else if `wy_pvalue6' < 0.01 {
				local stars_adj6 = "++"
			}
			else if `wy_pvalue6' < 0.05 {
				local stars_adj6 = "+"
			}

			local stars_adj7 = ""
			if `wy_pvalue7' < 0.001 {
				local stars_adj7 = "+++"
			}
			else if `wy_pvalue7' < 0.01 {
				local stars_adj7 = "++"
			}
			else if `wy_pvalue7' < 0.05 {
				local stars_adj7 = "+"
			}

			local stars_adj8 = ""
			if `wy_pvalue8' < 0.001 {
				local stars_adj8 = "+++"
			}
			else if `wy_pvalue8' < 0.01 {
				local stars_adj8 = "++"
			}
			else if `wy_pvalue8' < 0.05 {
				local stars_adj8 = "+"
			}

			local stars_adj9 = ""
			if `wy_pvalue9' < 0.001 {
				local stars_adj9 = "+++"
			}
			else if `wy_pvalue9' < 0.01 {
				local stars_adj9 = "++"
			}
			else if `wy_pvalue9' < 0.05 {
				local stars_adj9 = "+"
			}

			local stars_adj10 = ""
			if `wy_pvalue10' < 0.001 {
				local stars_adj10 = "+++"
			}
			else if `wy_pvalue10' < 0.01 {
				local stars_adj10 = "++"
			}
			else if `wy_pvalue10' < 0.05 {
				local stars_adj10 = "+"
			}

			local stars_adj11 = ""
			if `wy_pvalue11' < 0.001 {
				local stars_adj11 = "+++"
			}
			else if `wy_pvalue11' < 0.01 {
				local stars_adj11 = "++"
			}
			else if `wy_pvalue11' < 0.05 {
				local stars_adj11 = "+"
			}

			local stars_adj12 = ""
			if `wy_pvalue12' < 0.001 {
				local stars_adj12 = "+++"
			}
			else if `wy_pvalue12' < 0.01 {
				local stars_adj12 = "++"
			}
			else if `wy_pvalue12' < 0.05 {
				local stars_adj12 = "+"
			}
		}
	


	
	    
		if "`covar'" == "HT" {
		* Insert coefficients, standard errors, and stars for each model
		putdocx table mytable(`row',2) = ("`=strltrim("`b1_val'")'`stars1'`stars_adj1'"), halign(left) linebreak
		putdocx table mytable(`row',2) = ("(`=strltrim("`se1_val'")')"), append linebreak
		putdocx table mytable(`row',3) = ("`=strltrim("`b2_val'")'`stars2'`stars_adj2'"), halign(left) linebreak
		putdocx table mytable(`row',3) = ("(`=strltrim("`se2_val'")')"), append linebreak
		putdocx table mytable(`row',4) = ("`=strltrim("`b3_val'")'`stars3'`stars_adj3'"), halign(left) linebreak
		putdocx table mytable(`row',4) = ("(`=strltrim("`se3_val'")')"), append linebreak
		putdocx table mytable(`row',5) = ("`=strltrim("`b4_val'")'`stars4'`stars_adj4'"), halign(left) linebreak
		putdocx table mytable(`row',5) = ("(`=strltrim("`se4_val'")')"), append linebreak
		putdocx table mytable(`row',6) = ("`=strltrim("`b5_val'")'`stars5'`stars_adj5'"), halign(left) linebreak
		putdocx table mytable(`row',6) = ("(`=strltrim("`se5_val'")')"), append linebreak
		putdocx table mytable(`row',7) = ("`=strltrim("`b6_val'")'`stars6'`stars_adj6'"), halign(left) linebreak
		putdocx table mytable(`row',7) = ("(`=strltrim("`se6_val'")')"), append linebreak
		putdocx table mytable(`row',8) = ("`=strltrim("`b7_val'")'`stars7'`stars_adj7'"), halign(left) linebreak
		putdocx table mytable(`row',8) = ("(`=strltrim("`se7_val'")')"), append linebreak
		putdocx table mytable(`row',9) = ("`=strltrim("`b8_val'")'`stars8'`stars_adj8'"), halign(left) linebreak
		putdocx table mytable(`row',9) = ("(`=strltrim("`se8_val'")')"), append linebreak
		putdocx table mytable(`row',10) = ("`=strltrim("`b9_val'")'`stars9'`stars_adj9'"), halign(left) linebreak
		putdocx table mytable(`row',10) = ("(`=strltrim("`se9_val'")')"), append linebreak
		putdocx table mytable(`row',11) = ("`=strltrim("`b10_val'")'`stars10'`stars_adj10'"), halign(left) linebreak
		putdocx table mytable(`row',11) = ("(`=strltrim("`se10_val'")')"), append linebreak
		putdocx table mytable(`row',12) = ("`=strltrim("`b11_val'")'`stars11'`stars_adj11'"), halign(left) linebreak
		putdocx table mytable(`row',12) = ("(`=strltrim("`se11_val'")')"), append linebreak
		putdocx table mytable(`row',13) = ("`=strltrim("`b12_val'")'`stars12'`stars_adj12'"), halign(left) linebreak
		putdocx table mytable(`row',13) = ("(`=strltrim("`se12_val'")')"), append linebreak
	
	}
	else {
		putdocx table mytable(`row',2) = ("`=strltrim("`b1_val'")'`stars1'`stars_adj1'"), halign(left) linebreak
		putdocx table mytable(`row',2) = ("(`=strltrim("`se1_val'")')"), append
		putdocx table mytable(`row',3) = ("`=strltrim("`b2_val'")'`stars2'`stars_adj2'"), halign(left) linebreak
		putdocx table mytable(`row',3) = ("(`=strltrim("`se2_val'")')"), append
		putdocx table mytable(`row',4) = ("`=strltrim("`b3_val'")'`stars3'`stars_adj3'"), halign(left) linebreak
		putdocx table mytable(`row',4) = ("(`=strltrim("`se3_val'")')"), append 
		putdocx table mytable(`row',5) = ("`=strltrim("`b4_val'")'`stars4'`stars_adj4'"), halign(left) linebreak
		putdocx table mytable(`row',5) = ("(`=strltrim("`se4_val'")')"), append 
		putdocx table mytable(`row',6) = ("`=strltrim("`b5_val'")'`stars5'`stars_adj5'"), halign(left) linebreak
		putdocx table mytable(`row',6) = ("(`=strltrim("`se5_val'")')"), append 
		putdocx table mytable(`row',7) = ("`=strltrim("`b6_val'")'`stars6'`stars_adj6'"), halign(left) linebreak
		putdocx table mytable(`row',7) = ("(`=strltrim("`se6_val'")')"), append 
		putdocx table mytable(`row',8) = ("`=strltrim("`b7_val'")'`stars7'`stars_adj7'"), halign(left) linebreak
		putdocx table mytable(`row',8) = ("(`=strltrim("`se7_val'")')"), append 
		putdocx table mytable(`row',9) = ("`=strltrim("`b8_val'")'`stars8'`stars_adj8'"), halign(left) linebreak
		putdocx table mytable(`row',9) = ("(`=strltrim("`se8_val'")')"), append 
		putdocx table mytable(`row',10) = ("`=strltrim("`b9_val'")'`stars9'`stars_adj9'"), halign(left) linebreak
		putdocx table mytable(`row',10) = ("(`=strltrim("`se9_val'")')"), append 
		putdocx table mytable(`row',11) = ("`=strltrim("`b10_val'")'`stars10'`stars_adj10'"), halign(left) linebreak
		putdocx table mytable(`row',11) = ("(`=strltrim("`se10_val'")')"), append 
		putdocx table mytable(`row',12) = ("`=strltrim("`b11_val'")'`stars11'`stars_adj11'"), halign(left) linebreak
		putdocx table mytable(`row',12) = ("(`=strltrim("`se11_val'")')"), append 
		putdocx table mytable(`row',13) = ("`=strltrim("`b12_val'")'`stars12'`stars_adj12'"), halign(left) linebreak
		putdocx table mytable(`row',13) = ("(`=strltrim("`se12_val'")')"), append 
	}
    

        * Increment row
        local row = `row' + 1
        
		
	local stars_adj1 ""
	local stars_adj2 ""
	local stars_adj3 ""
	local stars_adj4 ""
	local stars_adj5 ""
	local stars_adj6 ""
	local stars_adj7 ""
	local stars_adj8 ""
	local stars_adj9 ""
	local stars_adj10 ""
	local stars_adj11 ""
	local stars_adj12 ""

	}

	



local wy_pvalue1: display %9.3f (wy_results1[1, 4])
local wy_pvalue2: display %9.3f (wy_results1[2, 4])
local wy_pvalue3: display %9.3f (wy_results1[3, 4])
local wy_pvalue4: display %9.3f (wy_results1[4, 4])
local wy_pvalue5: display %9.3f (wy_results2[1, 4])
local wy_pvalue6: display %9.3f (wy_results2[2, 4])
local wy_pvalue7: display %9.3f (wy_results2[3, 4])
local wy_pvalue8: display %9.3f (wy_results2[4, 4])
local wy_pvalue9: display %9.3f (wy_results3[1, 4])
local wy_pvalue10: display %9.3f (wy_results3[2, 4])
local wy_pvalue11: display %9.3f (wy_results3[3, 4])
local wy_pvalue12: display %9.3f (wy_results3[4, 4])

putdocx table mytable(3, 2) = ("[`=strltrim("`wy_pvalue1'")']"), append
putdocx table mytable(3, 3) = ("[`=strltrim("`wy_pvalue2'")']"), append
putdocx table mytable(3, 4) = ("[`=strltrim("`wy_pvalue3'")']"), append
putdocx table mytable(3, 5) = ("[`=strltrim("`wy_pvalue4'")']"), append
putdocx table mytable(3, 6) = ("[`=strltrim("`wy_pvalue5'")']"), append
putdocx table mytable(3, 7) = ("[`=strltrim("`wy_pvalue6'")']"), append
putdocx table mytable(3, 8) = ("[`=strltrim("`wy_pvalue7'")']"), append
putdocx table mytable(3, 9) = ("[`=strltrim("`wy_pvalue8'")']"), append
putdocx table mytable(3, 10) = ("[`=strltrim("`wy_pvalue9'")']"), append
putdocx table mytable(3, 11) = ("[`=strltrim("`wy_pvalue10'")']"), append
putdocx table mytable(3, 12) = ("[`=strltrim("`wy_pvalue11'")']"), append
putdocx table mytable(3, 13) = ("[`=strltrim("`wy_pvalue12'")']"), append



*adding observations
putdocx table mytable(`row',1) = ("N")
putdocx table mytable(`row',2) = (nobs1)
putdocx table mytable(`row',3) = (nobs2)
putdocx table mytable(`row',4) = (nobs3)
putdocx table mytable(`row',5) = (nobs4)
putdocx table mytable(`row',6) = (nobs5)
putdocx table mytable(`row',7) = (nobs6)
putdocx table mytable(`row',8) = (nobs7)
putdocx table mytable(`row',9) = (nobs8)
putdocx table mytable(`row',10) = (nobs9)
putdocx table mytable(`row',11) = (nobs10)
putdocx table mytable(`row',12) = (nobs11)
putdocx table mytable(`row',13) = (nobs12)



local row = `row' + 1
local row2 = `row' + 1

*Adding R² and adjusted R²
	
putdocx table mytable(`row',1) = ("R²")
putdocx table mytable(`row2',1) = ("R²adj")

	forvalues mod = 1/12 {		
		local col_num = `mod' + 1
		
		putdocx table mytable(`row',`col_num') = ("`=strltrim("`r2_`mod''")'")
		putdocx table mytable(`row2',`col_num') =("`=strltrim("`r2_adj_`mod''")'")
	}
	
	local row = `row' + 1

	* Adding  horizontal lines
	forvalues col_num = 1/13 {
		local row_2=`row'-2
		putdocx table mytable(1,`col_num'), border(top, "single")
		putdocx table mytable(2,`col_num'), border(bottom, "single")
		putdocx table mytable(`row_2',`col_num'), border(top, "single")
		putdocx table mytable(`row',`col_num'), border(bottom, "single")
	}
	forvalues col_num = 2/13 {
		putdocx table mytable(1,`col_num'), border(bottom, "single")
	}



	local row = `row' + 1
	
	local table_num  = `table_num' + 1

putdocx table mytable(`row',1) = ("Note: Robust standard errors in parentheses."), colspan(13) linebreak 
putdocx table mytable(`row',1) = ("For health-risk information, Westfall-Young MHT adjusted p-values in brackets with 10,000 bootstrap replications. "),  append linebreak 
putdocx table mytable(`row',1) = ("***p<0.001, **p<0.01, *p<0.05 for standard p-values, +++p< 0.001, ++p < 0.01, +p < 0.05 for Westfall-Young MHT adjusted p-values."),  append
	putdocx pagebreak
}

}


* ========================
**# Table SC15: Robustness test (basic) 3: Health treatment only model using likert scales
* ========================

qui {
qui regress meat_tax_lik HT FR_dummy LV_dummy, vce(hc3)
matrix rtable1=r(table)
scalar nobs1 = e(N)
local r2_1 = string(e(r2), "%9.3f")
local r2_adj_1 = string(e(r2_a), "%9.3f")

qui regress meat_tax_lik_s HT FR_dummy LV_dummy, vce(hc3)
matrix rtable2=r(table)
scalar nobs2 = e(N)
local r2_2 = string(e(r2), "%9.3f")
local r2_adj_2 = string(e(r2_a), "%9.3f")

qui regress meat_day_lik HT FR_dummy LV_dummy, vce(hc3)
matrix rtable3=r(table)
scalar nobs3 = e(N)
local r2_3 = string(e(r2), "%9.3f")
local r2_adj_3 = string(e(r2_a), "%9.3f")

qui regress meat_day_lik_s HT FR_dummy LV_dummy, vce(hc3)
matrix rtable4=r(table)
scalar nobs4 = e(N)
local r2_4 = string(e(r2), "%9.3f")
local r2_adj_4 = string(e(r2_a), "%9.3f")



local file_path = "robustness_main/r4_lik.xlsx"

if !fileexists("`file_path'") {
wyoung, cmd("regress meat_tax_lik HT FR_dummy LV_dummy, vce(hc3)" ///
"regress meat_tax_lik_s HT FR_dummy LV_dummy, vce(hc3)" ///
"regress meat_day_lik HT FR_dummy LV_dummy, vce(hc3)" ///
"regress meat_day_lik_s HT FR_dummy LV_dummy, vce(hc3)") ///
familyp("HT" "HT" "HT" "HT") familypexp strata(country) bootstraps(10000)
matrix wy_results = r(table)
putexcel set `file_path', replace
putexcel A1=matrix(results), names
}

preserve

import excel `file_path', sheet("Sheet1") firstrow clear
mkmat coef stderr  p pwyoung pbonf psidak, matrix(wy_results)


restore

*Add title
putdocx paragraph
putdocx text ("Table SC`table_num' LPM results for the basic model, Likert scale outcome variables - pooled sample."), font("", 11)

*Create a table with 5 rows and 5 columns (1 for covariate names, 4 for models)
putdocx table mytable = (9, 5), border(all, nil) width(4) width(100%) 

*headers for each model
putdocx table mytable(1,1) = ("")
putdocx table mytable(1,2) = ("Meat tax acceptability for oneself")
putdocx table mytable(1,3) = ("Meat Tax acceptability for society")
putdocx table mytable(1,4) = ("Meat-free days acceptability for oneself")
putdocx table mytable(1,5) = ("Meat-free days acceptability for society")



local covariates HT FR_dummy LV_dummy _cons
local nvars : word count `covariates'
local row 2  // Start from row 2 since row 1 has headers
local rowse 3

forvalues covar_num = 1/`nvars' {
	local covar : word `covar_num' of `covariates'
	if "`covar'" == "_cons" {
		local covar_label Constant
		putdocx table mytable(`row',1) = ("`covar_label'")
	}
		else {
			 local covar_label : variable label `covar'
			 putdocx table mytable(`row',1) = ("`covar_label'"), italic
			 }
		
	

	
    * Extract coefficients and standard errors
    local b1_val: display %9.3f rtable1[1,`covar_num']
    local se1_val: display %9.3f rtable1[2,`covar_num']
    local p1_val: display %9.3f rtable1[4,`covar_num']
	
    
    local b2_val: display %9.3f rtable2[1,`covar_num']
    local se2_val: display %9.3f rtable2[2,`covar_num']
    local p2_val: display %9.3f rtable2[4,`covar_num']
    
    local b3_val: display %9.3f rtable3[1,`covar_num']
    local se3_val: display %9.3f rtable3[2,`covar_num']
    local p3_val: display %9.3f rtable3[4,`covar_num']
    
    local b4_val: display %9.3f rtable4[1,`covar_num']
    local se4_val: display %9.3f rtable4[2,`covar_num']
    local p4_val: display %9.3f rtable4[4,`covar_num']

    * Adjusted p-values
	
	local wy_pvalue1: display %9.3f (wy_results[1, 4])
	local wy_pvalue2: display %9.3f (wy_results[2, 4])
	local wy_pvalue3: display %9.3f (wy_results[3, 4])
	local wy_pvalue4: display %9.3f (wy_results[4, 4])

	
	
    * Add stars based on p-values
    local stars1 = ""
    if `p1_val' < 0.001 {
        local stars1 = "***"
    }
    else if `p1_val' < 0.01 {
        local stars1 = "**"
    }
    else if `p1_val' < 0.05 {
        local stars1 = "*"
    }

    local stars2 = ""
    if `p2_val' < 0.001 {
        local stars2 = "***"
    }
    else if `p2_val' < 0.01 {
        local stars2 = "**"
    }
    else if `p2_val' < 0.05 {
        local stars2 = "*"
    }

    local stars3 = ""
    if `p3_val' < 0.001 {
        local stars3 = "***"
    }
    else if `p3_val' < 0.01 {
        local stars3 = "**"
    }
    else if `p3_val' < 0.05 {
        local stars3 = "*"
    }

    local stars4 = ""
    if `p4_val' < 0.001 {
        local stars4 = "***"
    }
    else if `p4_val' < 0.01 {
        local stars4 = "**"
    }
    else if `p4_val' < 0.05 {
        local stars4 = "*"
    }
	

	
	* Add stars_adj based on adjusted p-values
	if "`covar'" == "HT" {
		local stars_adj1 = ""
		if `wy_pvalue1' < 0.001 {
			local stars_adj1 = "+++"
		}
		else if `wy_pvalue1' < 0.01 {
			local stars_adj1 = "++"
		}
		else if `wy_pvalue1' < 0.05 {
			local stars_adj1 = "+"
		}

			local stars_adj2 = ""
		if `wy_pvalue2' < 0.001 {
			local stars_adj2 = "+++"
		}
		else if `wy_pvalue2' < 0.01 {
			local stars_adj2 = "++"
		}
		else if `wy_pvalue2' < 0.05 {
			local stars_adj2 = "+"
		}

			local stars_adj3 = ""
		if `wy_pvalue3' < 0.001 {
			local stars_adj3 = "+++"
		}
		else if `wy_pvalue3' < 0.01 {
			local stars_adj3 = "++"
		}
		else if `wy_pvalue3' < 0.05 {
			local stars_adj3 = "+"
		}

			local stars_adj4 = ""
		if `wy_pvalue4' < 0.001 {
			local stars_adj4 = "+++"
		}
		else if `wy_pvalue4' < 0.01 {
			local stars_adj4 = "++"
		}
		else if `wy_pvalue4' < 0.05 {
			local stars_adj4 = "+"
		}
	} 
	


    
		if "`covar'" == "HT" {
		* Insert coefficients, standard errors, and stars for each model
		putdocx table mytable(`row',2) = ("`=strltrim("`b1_val'")'`stars1'`stars_adj1'"), halign(left) linebreak
		putdocx table mytable(`row',2) = ("(`=strltrim("`se1_val'")')"), append linebreak
		putdocx table mytable(`row',3) = ("`=strltrim("`b2_val'")'`stars2'`stars_adj2'"), halign(left) linebreak
		putdocx table mytable(`row',3) = ("(`=strltrim("`se2_val'")')"), append linebreak
		putdocx table mytable(`row',4) = ("`=strltrim("`b3_val'")'`stars3'`stars_adj3'"), halign(left) linebreak
		putdocx table mytable(`row',4) = ("(`=strltrim("`se3_val'")')"), append linebreak
		putdocx table mytable(`row',5) = ("`=strltrim("`b4_val'")'`stars4'`stars_adj4'"), halign(left) linebreak
		putdocx table mytable(`row',5) = ("(`=strltrim("`se4_val'")')"), append linebreak
	
	}
	else {
		* Insert coefficients, standard errors, and stars for each model
		putdocx table mytable(`row',2) = ("`=strltrim("`b1_val'")'`stars1'`stars_adj1'"), halign(left) linebreak
		putdocx table mytable(`row',2) = ("(`=strltrim("`se1_val'")')"), append
		putdocx table mytable(`row',3) = ("`=strltrim("`b2_val'")'`stars2'`stars_adj2'"), halign(left) linebreak
		putdocx table mytable(`row',3) = ("(`=strltrim("`se2_val'")')"), append
		putdocx table mytable(`row',4) = ("`=strltrim("`b3_val'")'`stars3'`stars_adj3'"), halign(left) linebreak
		putdocx table mytable(`row',4) = ("(`=strltrim("`se3_val'")')"), append
		putdocx table mytable(`row',5) = ("`=strltrim("`b4_val'")'`stars4'`stars_adj4'"), halign(left) linebreak
		putdocx table mytable(`row',5) = ("(`=strltrim("`se4_val'")')"), append
	
	}
    
    * Increment the row counter for the next covariate
    local row = `row' + 1
	
	
	* Resetting stars to null
	local stars_adj1 = ""
	local stars_adj2 = ""
	local stars_adj3 = ""
	local stars_adj4 = ""
}



local wy_pvalue1: display %9.3f (wy_results[1, 4])
local wy_pvalue2: display %9.3f (wy_results[2, 4])
local wy_pvalue3: display %9.3f (wy_results[3, 4])
local wy_pvalue4: display %9.3f (wy_results[4, 4])



putdocx table mytable(2, 2) = ("[`=strltrim("`wy_pvalue1'")']"), append
putdocx table mytable(2, 3) = ("[`=strltrim("`wy_pvalue2'")']"), append
putdocx table mytable(2, 4) = ("[`=strltrim("`wy_pvalue3'")']"), append
putdocx table mytable(2, 5) = ("[`=strltrim("`wy_pvalue4'")']"), append

*adding observations
putdocx table mytable(`row',1) = ("N")
putdocx table mytable(`row',2) = (nobs1)
putdocx table mytable(`row',3) = (nobs2)
putdocx table mytable(`row',4) = (nobs3)
putdocx table mytable(`row',5) = (nobs4)


local row = `row' + 1
local row2 = `row' + 1

*Adding R² and adjusted R²
	
putdocx table mytable(`row',1) = ("R²")
putdocx table mytable(`row2',1) = ("R²adj")

	forvalues mod = 1/4 {		
		local col_num = `mod' + 1
		
		putdocx table mytable(`row',`col_num') = ("`=strltrim("`r2_`mod''")'")
		putdocx table mytable(`row2',`col_num') =("`=strltrim("`r2_adj_`mod''")'")
	}
	
	local row = `row' + 1

	* Adding  horizontal lines
	forvalues col_num = 1/5 {
		local row_2=`row'-2
		putdocx table mytable(1,`col_num'), border(top, "single")
		putdocx table mytable(1,`col_num'), border(bottom, "single")
		putdocx table mytable(`row_2',`col_num'), border(top, "single")
		putdocx table mytable(`row',`col_num'), border(bottom, "single")
	}

local row = `row' + 1

putdocx table mytable(`row',1) = ("Note: Robust standard errors in parentheses."), colspan(5) linebreak 
putdocx table mytable(`row',1) = ("For health-risk information, Westfall-Young MHT adjusted p-values in brackets with 10,000 bootstrap replications. "),  append linebreak 
putdocx table mytable(`row',1) = ("***p<0.001, **p<0.01, *p<0.05 for standard p-values, +++p< 0.001, ++p < 0.01, +p < 0.05 for Westfall-Young MHT adjusted p-values."),  append
putdocx pagebreak
}


local table_num  = `table_num' + 1



* ========================
**# Tables S16 to S21: Robustness test (heterogeneity) 3: Health treatment only model using likert scales
* ========================

qui{

foreach i in 1 2 3 4 5 6 {
	    if `i'== 1 {
        local var1 food_depriv
        local var2 health_low_rec
        local var3 unemployed
		
    }
    else if `i'== 2 {
        local var1 educ_low
        local var2 income_Q1
        local var3 age_low
    }
    else if `i'== 3 {
        local var1 female
        local var2 vegan_veg_pesc_dummy
        local var3 high_meat_dummy
    }               
    else if `i'== 4 {
        local var1 canteen_dummy
        local var2 po1_dummy
        local var3 po2_dummy
    } 
    else if `i'== 5 {
        local var1 po3_dummy
        local var2 po4_dummy
        local var3 po5_dummy
    }
    else if `i'== 6 {
        local var1 diet_knowl_high 
        local var2 cc_acc_high 
        local var3 soc_norm_high

    }
	
	local var1 = "`var1'"
	local var2 = "`var2'"
	local var3 = "`var3'"
	

	* coeff and N for var1
    qui regress meat_tax_lik 1.HT `var1' 1.HT#1.`var1' FR_dummy LV_dummy, vce(hc3)
    matrix rtable1 = r(table)
    scalar nobs1 = e(N)
	local r2_1 = string(e(r2), "%9.3f")
	local r2_adj_1 = string(e(r2_a), "%9.3f")
    qui regress meat_tax_lik_soc 1.HT `var1' 1.HT#1.`var1' FR_dummy LV_dummy, vce(hc3)
    matrix rtable2 = r(table)
    scalar nobs2 = e(N)
	local r2_2 = string(e(r2), "%9.3f")
	local r2_adj_2 = string(e(r2_a), "%9.3f")
    qui regress meat_day_lik 1.HT `var1' 1.HT#1.`var1' FR_dummy LV_dummy, vce(hc3)
    matrix rtable3 = r(table)
    scalar nobs3 = e(N)
	local r2_3 = string(e(r2), "%9.3f")
	local r2_adj_3 = string(e(r2_a), "%9.3f")
    qui regress meat_day_lik_soc 1.HT `var1' 1.HT#1.`var1' FR_dummy LV_dummy, vce(hc3)
    matrix rtable4 = r(table)
    scalar nobs4 = e(N)	
	local r2_4 = string(e(r2), "%9.3f")
	local r2_adj_4 = string(e(r2_a), "%9.3f")
	* coeff and N for var2
	qui regress meat_tax_lik 1.HT `var2' 1.HT#1.`var2' FR_dummy LV_dummy, vce(hc3)
    matrix rtable5 = r(table)
    scalar nobs5 = e(N)
	local r2_5 = string(e(r2), "%9.3f")
	local r2_adj_5 = string(e(r2_a), "%9.3f")
    qui regress meat_tax_lik_soc 1.HT `var2' 1.HT#1.`var2' FR_dummy LV_dummy, vce(hc3)
    matrix rtable6 = r(table)
    scalar nobs6 = e(N)
	local r2_6 = string(e(r2), "%9.3f")
	local r2_adj_6 = string(e(r2_a), "%9.3f")
    qui regress meat_day_lik 1.HT `var2' 1.HT#1.`var2' FR_dummy LV_dummy, vce(hc3)
    matrix rtable7 = r(table)
    scalar nobs7 = e(N)
	local r2_7 = string(e(r2), "%9.3f")
	local r2_adj_7 = string(e(r2_a), "%9.3f")
    qui regress meat_day_lik_soc 1.HT `var2' 1.HT#1.`var2' FR_dummy LV_dummy, vce(hc3)
    matrix rtable8 = r(table)
    scalar nobs8 = e(N)
	local r2_8 = string(e(r2), "%9.3f")
	local r2_adj_8 = string(e(r2_a), "%9.3f")
	* coeff and N for var3	
	qui regress meat_tax_lik 1.HT `var3' 1.HT#1.`var3' FR_dummy LV_dummy, vce(hc3)
    matrix rtable9 = r(table)
    scalar nobs9 = e(N)
	local r2_9 = string(e(r2), "%9.3f")
	local r2_adj_9 = string(e(r2_a), "%9.3f")
    qui regress meat_tax_lik_soc 1.HT `var3' 1.HT#1.`var3' FR_dummy LV_dummy, vce(hc3)
    matrix rtable10 = r(table)
    scalar nobs10 = e(N)
	local r2_10 = string(e(r2), "%9.3f")
	local r2_adj_10 = string(e(r2_a), "%9.3f")
    qui regress meat_day_lik 1.HT `var3' 1.HT#1.`var3' FR_dummy LV_dummy, vce(hc3)
    matrix rtable11 = r(table)
    scalar nobs11 = e(N)
	local r2_11 = string(e(r2), "%9.3f")
	local r2_adj_11 = string(e(r2_a), "%9.3f")
    qui regress meat_day_lik_soc 1.HT `var3' 1.HT#1.`var3' FR_dummy LV_dummy, vce(hc3)
    matrix rtable12 = r(table)
    scalar nobs12 = e(N)
	local r2_12 = string(e(r2), "%9.3f")
	local r2_adj_12 = string(e(r2_a), "%9.3f")

	local nb 10000 //number of bootsraps

    local file_path = "robustness_heterog/r4_lik_`var1'.xlsx"
    
    // Check if the file exists
    if !fileexists("`file_path'") {
    * Westfall-Young adjusted p-values for variable 1
    qui wyoung, cmd("regress meat_tax_lik 1.HT `var1' 1.HT#1.`var1' FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_tax_lik_soc 1.HT `var1' 1.HT#1.`var1' FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_day_lik 1.HT `var1' 1.HT#1.`var1' FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_day_lik_soc 1.HT `var1' 1.HT#1.`var1' FR_dummy LV_dummy, vce(hc3)"  ///
		"regress meat_tax_lik 1.HT `var1' 1.HT#1.`var1' FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_tax_lik_soc 1.HT `var1' 1.HT#1.`var1' FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_day_lik 1.HT `var1' 1.HT#1.`var1' FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_day_lik_soc 1.HT `var1' 1.HT#1.`var1' FR_dummy LV_dummy, vce(hc3)" ) ///
        familyp(1.HT 1.HT 1.HT 1.HT 1.HT+1.HT#1.`var1' 1.HT+1.HT#1.`var1' 1.HT+1.HT#1.`var1' 1.HT+1.HT#1.`var1') ///
        familypexp strata(country) ///
        bootstraps(`nb') seed(1234)
    
	matrix wy_results1 = r(table)
	putexcel set `file_path', replace
    putexcel A1=matrix(wy_results1), names
	
	}
		
	preserve
	
	import excel `file_path', sheet("Sheet1") firstrow clear
	mkmat coef stderr  p pwyoung pbonf psidak, matrix(wy_results1)
	

	restore
		
		
    local file_path = "robustness_heterog/r4_lik_`var2'.xlsx"
	    * Westfall-Young adjusted p-values for variable 2
        if !fileexists("`file_path'") {
			qui wyoung, cmd("regress meat_tax_lik 1.HT `var2' 1.HT#1.`var2' FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_tax_lik_soc 1.HT `var2' 1.HT#1.`var2' FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_day_lik 1.HT `var2' 1.HT#1.`var2' FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_day_lik_soc 1.HT `var2' 1.HT#1.`var2' FR_dummy LV_dummy, vce(hc3)"  ///
		"regress meat_tax_lik 1.HT `var2' 1.HT#1.`var2' FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_tax_lik_soc 1.HT `var2' 1.HT#1.`var2' FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_day_lik 1.HT `var2' 1.HT#1.`var2' FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_day_lik_soc 1.HT `var2' 1.HT#1.`var2' FR_dummy LV_dummy, vce(hc3)" ) ///
        familyp(1.HT 1.HT 1.HT 1.HT 1.HT+1.HT#1.`var2' 1.HT+1.HT#1.`var2' 1.HT+1.HT#1.`var2' 1.HT+1.HT#1.`var2') ///
        familypexp strata(country) ///
        bootstraps(`nb') seed(1234)
    
	matrix wy_results2 = r(table)	
	putexcel set `file_path', replace
    putexcel A1=matrix(wy_results2), names
		}
	
	preserve
	
	import excel `file_path', sheet("Sheet1") firstrow clear
	mkmat coef stderr  p pwyoung pbonf psidak, matrix(wy_results2)


	restore
	
    local file_path = "robustness_heterog/r4_lik_`var3'.xlsx"
		* Westfall-Young adjusted p-values for variable 3
    if !fileexists("`file_path'") {
		qui wyoung, cmd("regress meat_tax_lik 1.HT `var3' 1.HT#1.`var3' FR_dummy LV_dummy,vce(hc3)" ///
        "regress meat_tax_lik_soc 1.HT `var3' 1.HT#1.`var3' FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_day_lik 1.HT `var3' 1.HT#1.`var3' FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_day_lik_soc 1.HT `var3' 1.HT#1.`var3' FR_dummy LV_dummy, vce(hc3)"  ///
		"regress meat_tax_lik 1.HT `var3' 1.HT#1.`var3' FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_tax_lik_soc 1.HT `var3' 1.HT#1.`var3' FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_day_lik 1.HT `var3' 1.HT#1.`var3' FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_day_lik_soc 1.HT `var3' 1.HT#1.`var3' FR_dummy LV_dummy, vce(hc3)" ) ///
        familyp(1.HT 1.HT 1.HT 1.HT 1.HT+1.HT#1.`var3' 1.HT+1.HT#1.`var3' 1.HT+1.HT#1.`var3' 1.HT+1.HT#1.`var3') ///
        familypexp strata(country) ///
        bootstraps(`nb') seed(1234)
    
	matrix wy_results3 = r(table)
	putexcel set `file_path', replace
    putexcel A1=matrix(wy_results3), names
	}
		
	preserve
	
	import excel `file_path', sheet("Sheet1") firstrow clear
	mkmat coef stderr  p pwyoung pbonf psidak, matrix(wy_results3)


	restore
			
		
	local lbl1 : variable label `var1'
	local lbl2 : variable label `var2'
	local lbl3 : variable label `var3'	
	
	local low_lbl1 = lower("`lbl1'")
	local low_lbl2 = lower("`lbl2'")
	local low_lbl3 = lower("`lbl3'")
	
    * Add title
    putdocx paragraph
    putdocx text ("Table SC`table_num' LPM results of the heterogeneity models for ")
	putdocx text ("`low_lbl1'"), italic
	putdocx text (", ")
	putdocx text ("`low_lbl2'"), italic
	putdocx text (", and ")
	putdocx text ("`low_lbl3'"), italic
	putdocx text (", Likert scale outcome variables - pooled sample."), font("", 11)	
    putdocx table mytable = (12, 13), border(all, nil) width(4) width(100%) 
	


    * Create a table with 5 rows and 5 columns (1 for covariate names, 4 for models)
	putdocx table mytable(1,2) = ("`lbl1'"), colspan(4) italic
	putdocx table mytable(1,3) = ("`lbl2'"), colspan(4) italic
	putdocx table mytable(1,4) = ("`lbl3'"), colspan(4) italic
    * Headers for each model
    putdocx table mytable(2,2) = ("Meat tax acceptability for oneself")
    putdocx table mytable(2,3) = ("Meat tax acceptability for society")
    putdocx table mytable(2,4) = ("Meat-free days acceptability for oneself")
    putdocx table mytable(2,5) = ("Meat-free days acceptability for society")
    putdocx table mytable(2,6) = ("Meat tax acceptability for oneself")
    putdocx table mytable(2,7) = ("Meat tax acceptability for society")
    putdocx table mytable(2,8) = ("Meat-free days acceptability for oneself")
    putdocx table mytable(2,9) = ("Meat-free days acceptability for society")
    putdocx table mytable(2,10) = ("Meat tax acceptability for oneself")
    putdocx table mytable(2,11) = ("Meat tax acceptability for society")
    putdocx table mytable(2,12) = ("Meat-free days acceptability for oneself")
    putdocx table mytable(2,13) = ("Meat-free days acceptability for society")


	
    local covariates HT "`var1'" "1.HT#1.`var1'" FR_dummy LV_dummy _cons
	local interaction_label "Health-risk information # heterogeneity variable"
	local het_label "Heterogeneity variable"

    local nvars : word count `covariates'
	local row 3
    local rowse 4



  forvalues covar_num = 1/`nvars' {
	local covar : word `covar_num' of `covariates'
		if "`covar'"== "_cons"  {
			local covar_label Constant
			putdocx table mytable(`row',1) = ("`covar_label'")
			}	
			else {
				if "`covar'" == "1.HT#1.`var1'" | "`covar'" == "1.HT#1.`var2'" |  "`covar'" == "1.HT#1.`var3'" {
				local covar_label "`interaction_label'"
				putdocx table mytable(`row',1) = ("`covar_label'"), italic
				}
				else {
					if "`covar'" == "`var1'" | "`covar'" == "`var2'" | "`covar'" == "`var3'" {
					local covar_label "`het_label'"
					putdocx table mytable(`row',1) = ("`covar_label'"), italic
					} 
					else {
						local covar_label : variable label `covar'
						putdocx table mytable(`row',1) = ("`covar_label'"), italic
						}
					}
				}


		 


        * Extract coefficients and standard errors
		local b1_val: display %9.3f rtable1[1,`covar_num']
		local se1_val: display %9.3f rtable1[2,`covar_num']
		local p1_val: display %9.3f rtable1[4,`covar_num']
		

		local b2_val: display %9.3f rtable2[1,`covar_num']
		local se2_val: display %9.3f rtable2[2,`covar_num']
		local p2_val: display %9.3f rtable2[4,`covar_num']
		
		local b3_val: display %9.3f rtable3[1,`covar_num']
		local se3_val: display %9.3f rtable3[2,`covar_num']
		local p3_val: display %9.3f rtable3[4,`covar_num']
		
		local b4_val: display %9.3f rtable4[1,`covar_num']
		local se4_val: display %9.3f rtable4[2,`covar_num']
		local p4_val: display %9.3f rtable4[4,`covar_num']

		local b5_val: display %9.3f rtable5[1,`covar_num']
		local se5_val: display %9.3f rtable5[2,`covar_num']
		local p5_val: display %9.3f rtable5[4,`covar_num']
		
		
		local b6_val: display %9.3f rtable6[1,`covar_num']
		local se6_val: display %9.3f rtable6[2,`covar_num']
		local p6_val: display %9.3f rtable6[4,`covar_num']
		
		local b7_val: display %9.3f rtable7[1,`covar_num']
		local se7_val: display %9.3f rtable7[2,`covar_num']
		local p7_val: display %9.3f rtable7[4,`covar_num']
		
		local b8_val: display %9.3f rtable8[1,`covar_num']
		local se8_val: display %9.3f rtable8[2,`covar_num']
		local p8_val: display %9.3f rtable8[4,`covar_num']
		
		local b9_val: display %9.3f rtable9[1,`covar_num']
		local se9_val: display %9.3f rtable9[2,`covar_num']
		local p9_val: display %9.3f rtable9[4,`covar_num']
		
		
		local b10_val: display %9.3f rtable10[1,`covar_num']
		local se10_val: display %9.3f rtable10[2,`covar_num']
		local p10_val: display %9.3f rtable10[4,`covar_num']
		
		local b11_val: display %9.3f rtable11[1,`covar_num']
		local se11_val: display %9.3f rtable11[2,`covar_num']
		local p11_val: display %9.3f rtable11[4,`covar_num']
		
		local b12_val: display %9.3f rtable12[1,`covar_num']
		local se12_val: display %9.3f rtable12[2,`covar_num']
		local p12_val: display %9.3f rtable12[4,`covar_num']
		* Adjusted p-values
		
		local wy_pvalue1: display %9.3f (wy_results1[1, 4])
		local wy_pvalue2: display %9.3f (wy_results1[2, 4])
		local wy_pvalue3: display %9.3f (wy_results1[3, 4])
		local wy_pvalue4: display %9.3f (wy_results1[4, 4])
		local wy_pvalue5: display %9.3f (wy_results2[1, 4])
		local wy_pvalue6: display %9.3f (wy_results2[2, 4])
		local wy_pvalue7: display %9.3f (wy_results2[3, 4])
		local wy_pvalue8: display %9.3f (wy_results2[4, 4])
		local wy_pvalue9: display %9.3f (wy_results3[1, 4])
		local wy_pvalue10: display %9.3f (wy_results3[2, 4])
		local wy_pvalue11: display %9.3f (wy_results3[3, 4])
		local wy_pvalue12: display %9.3f (wy_results3[4, 4])
	


        * Add stars based on p-values
        local stars1 = ""
        if `p1_val' < 0.001 local stars1 = "***"
        else if `p1_val' < 0.01 local stars1 = "**"
        else if `p1_val' < 0.05 local stars1 = "*"

        local stars2 = ""
        if `p2_val' < 0.001 local stars2 = "***"
        else if `p2_val' < 0.01 local stars2 = "**"
        else if `p2_val' < 0.05 local stars2 = "*"

        local stars3 = ""
        if `p3_val' < 0.001 local stars3 = "***"
        else if `p3_val' < 0.01 local stars3 = "**"
        else if `p3_val' < 0.05 local stars3 = "*"

        local stars4 = ""
        if `p4_val' < 0.001 local stars4 = "***"
        else if `p4_val' < 0.01 local stars4 = "**"
        else if `p4_val' < 0.05 local stars4 = "*"
		
		local stars5 = ""
		if `p5_val' < 0.001 local stars5 = "***"
		else if `p5_val' < 0.01 local stars5 = "**"
		else if `p5_val' < 0.05 local stars5 = "*"

		local stars6 = ""
		if `p6_val' < 0.001 local stars6 = "***"
		else if `p6_val' < 0.01 local stars6 = "**"
		else if `p6_val' < 0.05 local stars6 = "*"

		local stars7 = ""
		if `p7_val' < 0.001 local stars7 = "***"
		else if `p7_val' < 0.01 local stars7 = "**"
		else if `p7_val' < 0.05 local stars7 = "*"

		local stars8 = ""
		if `p8_val' < 0.001 local stars8 = "***"
		else if `p8_val' < 0.01 local stars8 = "**"
		else if `p8_val' < 0.05 local stars8 = "*"

		local stars9 = ""
		if `p9_val' < 0.001 local stars9 = "***"
		else if `p9_val' < 0.01 local stars9 = "**"
		else if `p9_val' < 0.05 local stars9 = "*"

		local stars10 = ""
		if `p10_val' < 0.001 local stars10 = "***"
		else if `p10_val' < 0.01 local stars10 = "**"
		else if `p10_val' < 0.05 local stars10 = "*"

		local stars11 = ""
		if `p11_val' < 0.001 local stars11 = "***"
		else if `p11_val' < 0.01 local stars11 = "**"
		else if `p11_val' < 0.05 local stars11 = "*"

		local stars12 = ""
		if `p12_val' < 0.001 local stars12 = "***"
		else if `p12_val' < 0.01 local stars12 = "**"
		else if `p12_val' < 0.05 local stars12 = "*"


        * Adjusted p-values for HT and HT#var variable
		if "`covar'" == "HT"  {
			local stars_adj1 = ""
			if `wy_pvalue1' < 0.001 {
				local stars_adj1 = "+++"
			}
			else if `wy_pvalue1' < 0.01 {
				local stars_adj1 = "++"
			}
			else if `wy_pvalue1' < 0.05 {
				local stars_adj1 = "+"
			}

			local stars_adj2 = ""
			if `wy_pvalue2' < 0.001 {
				local stars_adj2 = "+++"
			}
			else if `wy_pvalue2' < 0.01 {
				local stars_adj2 = "++"
			}
			else if `wy_pvalue2' < 0.05 {
				local stars_adj2 = "+"
			}

			local stars_adj3 = ""
			if `wy_pvalue3' < 0.001 {
				local stars_adj3 = "+++"
			}
			else if `wy_pvalue3' < 0.01 {
				local stars_adj3 = "++"
			}
			else if `wy_pvalue3' < 0.05 {
				local stars_adj3 = "+"
			}

			local stars_adj4 = ""
			if `wy_pvalue4' < 0.001 {
				local stars_adj4 = "+++"
			}
			else if `wy_pvalue4' < 0.01 {
				local stars_adj4 = "++"
			}
			else if `wy_pvalue4' < 0.05 {
				local stars_adj4 = "+"
			}

			local stars_adj5 = ""
			if `wy_pvalue5' < 0.001 {
				local stars_adj5 = "+++"
			}
			else if `wy_pvalue5' < 0.01 {
				local stars_adj5 = "++"
			}
			else if `wy_pvalue5' < 0.05 {
				local stars_adj5 = "+"
			}

			local stars_adj6 = ""
			if `wy_pvalue6' < 0.001 {
				local stars_adj6 = "+++"
			}
			else if `wy_pvalue6' < 0.01 {
				local stars_adj6 = "++"
			}
			else if `wy_pvalue6' < 0.05 {
				local stars_adj6 = "+"
			}

			local stars_adj7 = ""
			if `wy_pvalue7' < 0.001 {
				local stars_adj7 = "+++"
			}
			else if `wy_pvalue7' < 0.01 {
				local stars_adj7 = "++"
			}
			else if `wy_pvalue7' < 0.05 {
				local stars_adj7 = "+"
			}

			local stars_adj8 = ""
			if `wy_pvalue8' < 0.001 {
				local stars_adj8 = "+++"
			}
			else if `wy_pvalue8' < 0.01 {
				local stars_adj8 = "++"
			}
			else if `wy_pvalue8' < 0.05 {
				local stars_adj8 = "+"
			}

			local stars_adj9 = ""
			if `wy_pvalue9' < 0.001 {
				local stars_adj9 = "+++"
			}
			else if `wy_pvalue9' < 0.01 {
				local stars_adj9 = "++"
			}
			else if `wy_pvalue9' < 0.05 {
				local stars_adj9 = "+"
			}

			local stars_adj10 = ""
			if `wy_pvalue10' < 0.001 {
				local stars_adj10 = "+++"
			}
			else if `wy_pvalue10' < 0.01 {
				local stars_adj10 = "++"
			}
			else if `wy_pvalue10' < 0.05 {
				local stars_adj10 = "+"
			}

			local stars_adj11 = ""
			if `wy_pvalue11' < 0.001 {
				local stars_adj11 = "+++"
			}
			else if `wy_pvalue11' < 0.01 {
				local stars_adj11 = "++"
			}
			else if `wy_pvalue11' < 0.05 {
				local stars_adj11 = "+"
			}

			local stars_adj12 = ""
			if `wy_pvalue12' < 0.001 {
				local stars_adj12 = "+++"
			}
			else if `wy_pvalue12' < 0.01 {
				local stars_adj12 = "++"
			}
			else if `wy_pvalue12' < 0.05 {
				local stars_adj12 = "+"
			}
		}
	

	if "`covar'" == "HT" {
		* Insert coefficients, standard errors, and stars for each model
		putdocx table mytable(`row',2) = ("`=strltrim("`b1_val'")'`stars1'`stars_adj1'"), halign(left) linebreak
		putdocx table mytable(`row',2) = ("(`=strltrim("`se1_val'")')"), append linebreak
		putdocx table mytable(`row',3) = ("`=strltrim("`b2_val'")'`stars2'`stars_adj2'"), halign(left) linebreak
		putdocx table mytable(`row',3) = ("(`=strltrim("`se2_val'")')"), append linebreak
		putdocx table mytable(`row',4) = ("`=strltrim("`b3_val'")'`stars3'`stars_adj3'"), halign(left) linebreak
		putdocx table mytable(`row',4) = ("(`=strltrim("`se3_val'")')"), append linebreak
		putdocx table mytable(`row',5) = ("`=strltrim("`b4_val'")'`stars4'`stars_adj4'"), halign(left) linebreak
		putdocx table mytable(`row',5) = ("(`=strltrim("`se4_val'")')"), append linebreak
		putdocx table mytable(`row',6) = ("`=strltrim("`b5_val'")'`stars5'`stars_adj5'"), halign(left) linebreak
		putdocx table mytable(`row',6) = ("(`=strltrim("`se5_val'")')"), append linebreak
		putdocx table mytable(`row',7) = ("`=strltrim("`b6_val'")'`stars6'`stars_adj6'"), halign(left) linebreak
		putdocx table mytable(`row',7) = ("(`=strltrim("`se6_val'")')"), append linebreak
		putdocx table mytable(`row',8) = ("`=strltrim("`b7_val'")'`stars7'`stars_adj7'"), halign(left) linebreak
		putdocx table mytable(`row',8) = ("(`=strltrim("`se7_val'")')"), append linebreak
		putdocx table mytable(`row',9) = ("`=strltrim("`b8_val'")'`stars8'`stars_adj8'"), halign(left) linebreak
		putdocx table mytable(`row',9) = ("(`=strltrim("`se8_val'")')"), append linebreak
		putdocx table mytable(`row',10) = ("`=strltrim("`b9_val'")'`stars9'`stars_adj9'"), halign(left) linebreak
		putdocx table mytable(`row',10) = ("(`=strltrim("`se9_val'")')"), append linebreak
		putdocx table mytable(`row',11) = ("`=strltrim("`b10_val'")'`stars10'`stars_adj10'"), halign(left) linebreak
		putdocx table mytable(`row',11) = ("(`=strltrim("`se10_val'")')"), append linebreak
		putdocx table mytable(`row',12) = ("`=strltrim("`b11_val'")'`stars11'`stars_adj11'"), halign(left) linebreak
		putdocx table mytable(`row',12) = ("(`=strltrim("`se11_val'")')"), append linebreak
		putdocx table mytable(`row',13) = ("`=strltrim("`b12_val'")'`stars12'`stars_adj12'"), halign(left) linebreak
		putdocx table mytable(`row',13) = ("(`=strltrim("`se12_val'")')"), append linebreak
	
	}
	else {
		putdocx table mytable(`row',2) = ("`=strltrim("`b1_val'")'`stars1'`stars_adj1'"), halign(left) linebreak
		putdocx table mytable(`row',2) = ("(`=strltrim("`se1_val'")')"), append
		putdocx table mytable(`row',3) = ("`=strltrim("`b2_val'")'`stars2'`stars_adj2'"), halign(left) linebreak
		putdocx table mytable(`row',3) = ("(`=strltrim("`se2_val'")')"), append
		putdocx table mytable(`row',4) = ("`=strltrim("`b3_val'")'`stars3'`stars_adj3'"), halign(left) linebreak
		putdocx table mytable(`row',4) = ("(`=strltrim("`se3_val'")')"), append 
		putdocx table mytable(`row',5) = ("`=strltrim("`b4_val'")'`stars4'`stars_adj4'"), halign(left) linebreak
		putdocx table mytable(`row',5) = ("(`=strltrim("`se4_val'")')"), append 
		putdocx table mytable(`row',6) = ("`=strltrim("`b5_val'")'`stars5'`stars_adj5'"), halign(left) linebreak
		putdocx table mytable(`row',6) = ("(`=strltrim("`se5_val'")')"), append 
		putdocx table mytable(`row',7) = ("`=strltrim("`b6_val'")'`stars6'`stars_adj6'"), halign(left) linebreak
		putdocx table mytable(`row',7) = ("(`=strltrim("`se6_val'")')"), append 
		putdocx table mytable(`row',8) = ("`=strltrim("`b7_val'")'`stars7'`stars_adj7'"), halign(left) linebreak
		putdocx table mytable(`row',8) = ("(`=strltrim("`se7_val'")')"), append 
		putdocx table mytable(`row',9) = ("`=strltrim("`b8_val'")'`stars8'`stars_adj8'"), halign(left) linebreak
		putdocx table mytable(`row',9) = ("(`=strltrim("`se8_val'")')"), append 
		putdocx table mytable(`row',10) = ("`=strltrim("`b9_val'")'`stars9'`stars_adj9'"), halign(left) linebreak
		putdocx table mytable(`row',10) = ("(`=strltrim("`se9_val'")')"), append 
		putdocx table mytable(`row',11) = ("`=strltrim("`b10_val'")'`stars10'`stars_adj10'"), halign(left) linebreak
		putdocx table mytable(`row',11) = ("(`=strltrim("`se10_val'")')"), append 
		putdocx table mytable(`row',12) = ("`=strltrim("`b11_val'")'`stars11'`stars_adj11'"), halign(left) linebreak
		putdocx table mytable(`row',12) = ("(`=strltrim("`se11_val'")')"), append 
		putdocx table mytable(`row',13) = ("`=strltrim("`b12_val'")'`stars12'`stars_adj12'"), halign(left) linebreak
		putdocx table mytable(`row',13) = ("(`=strltrim("`se12_val'")')"), append 
	}
    
    * Increment row
    local row = `row' + 1
       
	local stars_adj1 ""
	local stars_adj2 ""
	local stars_adj3 ""
	local stars_adj4 ""
	local stars_adj5 ""
	local stars_adj6 ""
	local stars_adj7 ""
	local stars_adj8 ""
	local stars_adj9 ""
	local stars_adj10 ""
	local stars_adj11 ""
	local stars_adj12 ""
	}
	
local wy_pvalue1: display %9.3f (wy_results1[1, 4])
local wy_pvalue2: display %9.3f (wy_results1[2, 4])
local wy_pvalue3: display %9.3f (wy_results1[3, 4])
local wy_pvalue4: display %9.3f (wy_results1[4, 4])
local wy_pvalue5: display %9.3f (wy_results2[1, 4])
local wy_pvalue6: display %9.3f (wy_results2[2, 4])
local wy_pvalue7: display %9.3f (wy_results2[3, 4])
local wy_pvalue8: display %9.3f (wy_results2[4, 4])
local wy_pvalue9: display %9.3f (wy_results3[1, 4])
local wy_pvalue10: display %9.3f (wy_results3[2, 4])
local wy_pvalue11: display %9.3f (wy_results3[3, 4])
local wy_pvalue12: display %9.3f (wy_results3[4, 4])

putdocx table mytable(3, 2) = ("[`=strltrim("`wy_pvalue1'")']"), append
putdocx table mytable(3, 3) = ("[`=strltrim("`wy_pvalue2'")']"), append
putdocx table mytable(3, 4) = ("[`=strltrim("`wy_pvalue3'")']"), append
putdocx table mytable(3, 5) = ("[`=strltrim("`wy_pvalue4'")']"), append
putdocx table mytable(3, 6) = ("[`=strltrim("`wy_pvalue5'")']"), append
putdocx table mytable(3, 7) = ("[`=strltrim("`wy_pvalue6'")']"), append
putdocx table mytable(3, 8) = ("[`=strltrim("`wy_pvalue7'")']"), append
putdocx table mytable(3, 9) = ("[`=strltrim("`wy_pvalue8'")']"), append
putdocx table mytable(3, 10) = ("[`=strltrim("`wy_pvalue9'")']"), append
putdocx table mytable(3, 11) = ("[`=strltrim("`wy_pvalue10'")']"), append
putdocx table mytable(3, 12) = ("[`=strltrim("`wy_pvalue11'")']"), append
putdocx table mytable(3, 13) = ("[`=strltrim("`wy_pvalue12'")']"), append



*adding observations
putdocx table mytable(`row',1) = ("N")
putdocx table mytable(`row',2) = (nobs1)
putdocx table mytable(`row',3) = (nobs2)
putdocx table mytable(`row',4) = (nobs3)
putdocx table mytable(`row',5) = (nobs4)
putdocx table mytable(`row',6) = (nobs5)
putdocx table mytable(`row',7) = (nobs6)
putdocx table mytable(`row',8) = (nobs7)
putdocx table mytable(`row',9) = (nobs8)
putdocx table mytable(`row',10) = (nobs9)
putdocx table mytable(`row',11) = (nobs10)
putdocx table mytable(`row',12) = (nobs11)
putdocx table mytable(`row',13) = (nobs12)



local row = `row' + 1
local row2 = `row' + 1

*Adding R² and adjusted R²
	
putdocx table mytable(`row',1) = ("R²")
putdocx table mytable(`row2',1) = ("R²adj")

	forvalues mod = 1/12 {		
		local col_num = `mod' + 1
		
		putdocx table mytable(`row',`col_num') = ("`=strltrim("`r2_`mod''")'")
		putdocx table mytable(`row2',`col_num') =("`=strltrim("`r2_adj_`mod''")'")
	}
	
	local row = `row' + 1

	* Adding  horizontal lines
	forvalues col_num = 1/13 {
		local row_2=`row'-2
		putdocx table mytable(1,`col_num'), border(top, "single")
		putdocx table mytable(2,`col_num'), border(bottom, "single")
		putdocx table mytable(`row_2',`col_num'), border(top, "single")
		putdocx table mytable(`row',`col_num'), border(bottom, "single")
	}
	forvalues col_num = 2/13 {
		putdocx table mytable(1,`col_num'), border(bottom, "single")
	}



	local row = `row' + 1
	local table_num  = `table_num' + 1
	
putdocx table mytable(`row',1) = ("Note: Robust standard errors in parentheses."), colspan(13) linebreak 
putdocx table mytable(`row',1) = ("For health-risk information, Westfall-Young MHT adjusted p-values in brackets with 10,000 bootstrap replications. "),  append linebreak 
putdocx table mytable(`row',1) = ("***p<0.001, **p<0.01, *p<0.05 for standard p-values, +++p< 0.001, ++p < 0.01, +p < 0.05 for Westfall-Young MHT adjusted p-values."),  append
	putdocx pagebreak
}

}

* ========================
**# Table SC22: Robustness test (basic) 4: Health treatment only model only meat eaters
* ========================

    
qui {
	
qui regress meat_tax_dummy HT FR_dummy LV_dummy if vegan_veg_pesc_dummy==0, vce(hc3)
matrix rtable1=r(table)
scalar nobs1 = e(N)
local r2_1 = string(e(r2), "%9.3f")
local r2_adj_1 = string(e(r2_a), "%9.3f")

qui regress meat_tax_s HT FR_dummy LV_dummy if vegan_veg_pesc_dummy==0, vce(hc3)
matrix rtable2=r(table)
scalar nobs2 = e(N)
local r2_2 = string(e(r2), "%9.3f")
local r2_adj_2 = string(e(r2_a), "%9.3f")

qui regress meat_day_dummy HT FR_dummy LV_dummy if vegan_veg_pesc_dummy==0, vce(hc3)
matrix rtable3=r(table)
scalar nobs3 = e(N)
local r2_3 = string(e(r2), "%9.3f")
local r2_adj_3 = string(e(r2_a), "%9.3f")

qui regress meat_day_s HT FR_dummy LV_dummy if vegan_veg_pesc_dummy==0, vce(hc3)
matrix rtable4=r(table)
scalar nobs4 = e(N)
local r2_4 = string(e(r2), "%9.3f")
local r2_adj_4 = string(e(r2_a), "%9.3f")


local file_path = "robustness_main/r7_wo_veg.xlsx"
    
if !fileexists("`file_path'") {
*Westfall-Young adjusted p-values
wyoung, cmd("regress meat_tax_dummy HT FR_dummy LV_dummy if vegan_veg_pesc_dummy==0, vce(hc3)" ///
"regress meat_tax_s HT FR_dummy LV_dummy if vegan_veg_pesc_dummy==0, vce(hc3)" ///
"regress meat_day_dummy HT FR_dummy LV_dummy if vegan_veg_pesc_dummy==0, vce(hc3)" ///
"regress meat_day_s HT FR_dummy LV_dummy if vegan_veg_pesc_dummy==0, vce(hc3)") ///
familyp("HT" "HT" "HT" "HT") familypexp strata(country) bootstraps(10000) seed(1234)
 matrix wy_results = r(table)
putexcel set `file_path', replace
putexcel A1=matrix(wy_results), names
}
	
preserve

import excel `file_path', sheet("Sheet1") firstrow clear
mkmat coef stderr  p pwyoung pbonf psidak, matrix(wy_results)

restore



*Add title
putdocx paragraph
putdocx text ("Table SC`table_num' LPM results of the basic models, using meat eaters only - pooled sample."), font("", 11)	
*Create a table with 5 rows and 5 columns (1 for covariate names, 4 for models)
putdocx table mytable = (9, 5), border(all, nil) width(4) width(100%) 

*headers for each model
putdocx table mytable(1,1) = ("")
putdocx table mytable(1,2) = ("Meat tax acceptability for oneself")
putdocx table mytable(1,3) = ("Meat Tax acceptability for society")
putdocx table mytable(1,4) = ("Meat-free days acceptability for oneself")
putdocx table mytable(1,5) = ("Meat-free days acceptability for society")



local covariates HT FR_dummy LV_dummy _cons
local nvars : word count `covariates'
local row 2  // Start from row 2 since row 1 has headers
local rowse 3

forvalues covar_num = 1/`nvars' {
	local covar : word `covar_num' of `covariates'
	if "`covar'" == "_cons" {
		local covar_label Constant
		putdocx table mytable(`row',1) = ("`covar_label'")
	}
		else {
			 local covar_label : variable label `covar'
			 putdocx table mytable(`row',1) = ("`covar_label'"), italic
			 }
		
	

	
    * Extract coefficients and standard errors
    local b1_val: display %9.3f rtable1[1,`covar_num']
    local se1_val: display %9.3f rtable1[2,`covar_num']
    local p1_val: display %9.3f rtable1[4,`covar_num']
	
    
    local b2_val: display %9.3f rtable2[1,`covar_num']
    local se2_val: display %9.3f rtable2[2,`covar_num']
    local p2_val: display %9.3f rtable2[4,`covar_num']
    
    local b3_val: display %9.3f rtable3[1,`covar_num']
    local se3_val: display %9.3f rtable3[2,`covar_num']
    local p3_val: display %9.3f rtable3[4,`covar_num']
    
    local b4_val: display %9.3f rtable4[1,`covar_num']
    local se4_val: display %9.3f rtable4[2,`covar_num']
    local p4_val: display %9.3f rtable4[4,`covar_num']

    * Adjusted p-values
	
	local wy_pvalue1: display %9.3f (wy_results[1, 4])
	local wy_pvalue2: display %9.3f (wy_results[2, 4])
	local wy_pvalue3: display %9.3f (wy_results[3, 4])
	local wy_pvalue4: display %9.3f (wy_results[4, 4])

	
	
    * Add stars based on p-values
    local stars1 = ""
    if `p1_val' < 0.001 {
        local stars1 = "***"
    }
    else if `p1_val' < 0.01 {
        local stars1 = "**"
    }
    else if `p1_val' < 0.05 {
        local stars1 = "*"
    }

    local stars2 = ""
    if `p2_val' < 0.001 {
        local stars2 = "***"
    }
    else if `p2_val' < 0.01 {
        local stars2 = "**"
    }
    else if `p2_val' < 0.05 {
        local stars2 = "*"
    }

    local stars3 = ""
    if `p3_val' < 0.001 {
        local stars3 = "***"
    }
    else if `p3_val' < 0.01 {
        local stars3 = "**"
    }
    else if `p3_val' < 0.05 {
        local stars3 = "*"
    }

    local stars4 = ""
    if `p4_val' < 0.001 {
        local stars4 = "***"
    }
    else if `p4_val' < 0.01 {
        local stars4 = "**"
    }
    else if `p4_val' < 0.05 {
        local stars4 = "*"
    }
	

	
	* Add stars_adj based on adjusted p-values
	if "`covar'" == "HT" {
		local stars_adj1 = ""
		if `wy_pvalue1' < 0.001 {
			local stars_adj1 = "+++"
		}
		else if `wy_pvalue1' < 0.01 {
			local stars_adj1 = "++"
		}
		else if `wy_pvalue1' < 0.05 {
			local stars_adj1 = "+"
		}

			local stars_adj2 = ""
		if `wy_pvalue2' < 0.001 {
			local stars_adj2 = "+++"
		}
		else if `wy_pvalue2' < 0.01 {
			local stars_adj2 = "++"
		}
		else if `wy_pvalue2' < 0.05 {
			local stars_adj2 = "+"
		}

			local stars_adj3 = ""
		if `wy_pvalue3' < 0.001 {
			local stars_adj3 = "+++"
		}
		else if `wy_pvalue3' < 0.01 {
			local stars_adj3 = "++"
		}
		else if `wy_pvalue3' < 0.05 {
			local stars_adj3 = "+"
		}

			local stars_adj4 = ""
		if `wy_pvalue4' < 0.001 {
			local stars_adj4 = "+++"
		}
		else if `wy_pvalue4' < 0.01 {
			local stars_adj4 = "++"
		}
		else if `wy_pvalue4' < 0.05 {
			local stars_adj4 = "+"
		}
	} 
	



		if "`covar'" == "HT" {
		* Insert coefficients, standard errors, and stars for each model
		putdocx table mytable(`row',2) = ("`=strltrim("`b1_val'")'`stars1'`stars_adj1'"), halign(left) linebreak
		putdocx table mytable(`row',2) = ("(`=strltrim("`se1_val'")')"), append linebreak
		putdocx table mytable(`row',3) = ("`=strltrim("`b2_val'")'`stars2'`stars_adj2'"), halign(left) linebreak
		putdocx table mytable(`row',3) = ("(`=strltrim("`se2_val'")')"), append linebreak
		putdocx table mytable(`row',4) = ("`=strltrim("`b3_val'")'`stars3'`stars_adj3'"), halign(left) linebreak
		putdocx table mytable(`row',4) = ("(`=strltrim("`se3_val'")')"), append linebreak
		putdocx table mytable(`row',5) = ("`=strltrim("`b4_val'")'`stars4'`stars_adj4'"), halign(left) linebreak
		putdocx table mytable(`row',5) = ("(`=strltrim("`se4_val'")')"), append linebreak
	
	}
	else {
		* Insert coefficients, standard errors, and stars for each model
		putdocx table mytable(`row',2) = ("`=strltrim("`b1_val'")'`stars1'`stars_adj1'"), halign(left) linebreak
		putdocx table mytable(`row',2) = ("(`=strltrim("`se1_val'")')"), append
		putdocx table mytable(`row',3) = ("`=strltrim("`b2_val'")'`stars2'`stars_adj2'"), halign(left) linebreak
		putdocx table mytable(`row',3) = ("(`=strltrim("`se2_val'")')"), append
		putdocx table mytable(`row',4) = ("`=strltrim("`b3_val'")'`stars3'`stars_adj3'"), halign(left) linebreak
		putdocx table mytable(`row',4) = ("(`=strltrim("`se3_val'")')"), append
		putdocx table mytable(`row',5) = ("`=strltrim("`b4_val'")'`stars4'`stars_adj4'"), halign(left) linebreak
		putdocx table mytable(`row',5) = ("(`=strltrim("`se4_val'")')"), append
	
	}
	
    
    * Increment the row counter for the next covariate
    local row = `row' + 1
	
	
	* Resetting stars to null
	local stars_adj1 = ""
	local stars_adj2 = ""
	local stars_adj3 = ""
	local stars_adj4 = ""
}



local wy_pvalue1: display %9.3f (wy_results[1, 4])
local wy_pvalue2: display %9.3f (wy_results[2, 4])
local wy_pvalue3: display %9.3f (wy_results[3, 4])
local wy_pvalue4: display %9.3f (wy_results[4, 4])



putdocx table mytable(2, 2) = ("[`=strltrim("`wy_pvalue1'")']"), append
putdocx table mytable(2, 3) = ("[`=strltrim("`wy_pvalue2'")']"), append
putdocx table mytable(2, 4) = ("[`=strltrim("`wy_pvalue3'")']"), append
putdocx table mytable(2, 5) = ("[`=strltrim("`wy_pvalue4'")']"), append

*adding observations
putdocx table mytable(`row',1) = ("N")
putdocx table mytable(`row',2) = (nobs1)
putdocx table mytable(`row',3) = (nobs2)
putdocx table mytable(`row',4) = (nobs3)
putdocx table mytable(`row',5) = (nobs4)



local row = `row' + 1
local row2 = `row' + 1

*Adding R² and adjusted R²
	
putdocx table mytable(`row',1) = ("R²")
putdocx table mytable(`row2',1) = ("R²adj")

	forvalues mod = 1/4 {		
		local col_num = `mod' + 1
		
		putdocx table mytable(`row',`col_num') = ("`=strltrim("`r2_`mod''")'")
		putdocx table mytable(`row2',`col_num') =("`=strltrim("`r2_adj_`mod''")'")
	}
	
	local row = `row' + 1

	* Adding  horizontal lines
	forvalues col_num = 1/5 {
		local row_2=`row'-2
		putdocx table mytable(1,`col_num'), border(top, "single")
		putdocx table mytable(1,`col_num'), border(bottom, "single")
		putdocx table mytable(`row_2',`col_num'), border(top, "single")
		putdocx table mytable(`row',`col_num'), border(bottom, "single")
	}


local row = `row' + 1

putdocx table mytable(`row',1) = ("Note: Robust standard errors in parentheses."), colspan(5) linebreak 
putdocx table mytable(`row',1) = ("For health-risk information, Westfall-Young MHT adjusted p-values in brackets with 10,000 bootstrap replications. "),  append linebreak 
putdocx table mytable(`row',1) = ("***p<0.001, **p<0.01, *p<0.05 for standard p-values, +++p< 0.001, ++p < 0.01, +p < 0.05 for Westfall-Young MHT adjusted p-values."),  append
putdocx pagebreak
}


local table_num  = `table_num' + 1



* ========================
**# Tables S23 to S28: Robustness test (heterogeneity) 4: Only meat eaters
* ========================

qui {
foreach i in 1 2 3 4 5 6 {
	    if `i'== 1 {
        local var1 food_depriv
        local var2 health_low_rec
        local var3 unemployed 
    }
    else if `i'== 2 {
        local var1 educ_low
        local var2 income_Q1
        local var3 age_low
    }
    else if `i'== 3 {
        local var1 female
        local var2 vegan_veg_pesc_dummy
        local var3 high_meat_dummy
    }               
    else if `i'== 4 {
        local var1 canteen_dummy
        local var2 po1_dummy
        local var3 po2_dummy
    } 
    else if `i'== 5 {
        local var1 po3_dummy
        local var2 po4_dummy
        local var3 po5_dummy
    }
    else if `i'== 6 {
        local var1 diet_knowl_high 
        local var2 cc_acc_high 
        local var3 soc_norm_high

    }
	
	local var1 = "`var1'"
	local var2 = "`var2'"
	local var3 = "`var3'"
	

	* coeff and N for var1
    qui regress meat_tax_dummy 1.HT `var1' 1.HT#1.`var1' FR_dummy LV_dummy if vegan_veg_pesc_dummy==0, vce(hc3)
    matrix rtable1 = r(table)
    scalar nobs1 = e(N)
	local r2_1 = string(e(r2), "%9.3f")
	local r2_adj_1 = string(e(r2_a), "%9.3f")
    qui regress meat_tax_s 1.HT `var1' 1.HT#1.`var1' FR_dummy LV_dummy if vegan_veg_pesc_dummy==0, vce(hc3)
    matrix rtable2 = r(table)
    scalar nobs2 = e(N)
	local r2_2 = string(e(r2), "%9.3f")
	local r2_adj_2 = string(e(r2_a), "%9.3f")
    qui regress meat_day_dummy 1.HT `var1' 1.HT#1.`var1' FR_dummy LV_dummy if vegan_veg_pesc_dummy==0, vce(hc3)
    matrix rtable3 = r(table)
    scalar nobs3 = e(N)
	local r2_3 = string(e(r2), "%9.3f")
	local r2_adj_3 = string(e(r2_a), "%9.3f")
    qui regress meat_day_s 1.HT `var1' 1.HT#1.`var1' FR_dummy LV_dummy if vegan_veg_pesc_dummy==0, vce(hc3)
    matrix rtable4 = r(table)
    scalar nobs4 = e(N)
	local r2_4 = string(e(r2), "%9.3f")
	local r2_adj_4 = string(e(r2_a), "%9.3f")
	* coeff and N for var2
	qui regress meat_tax_dummy 1.HT `var2' 1.HT#1.`var2' FR_dummy LV_dummy if vegan_veg_pesc_dummy==0, vce(hc3)
    matrix rtable5 = r(table)
    scalar nobs5 = e(N)
	local r2_5 = string(e(r2), "%9.3f")
	local r2_adj_5 = string(e(r2_a), "%9.3f")
    qui regress meat_tax_s 1.HT `var2' 1.HT#1.`var2' FR_dummy LV_dummy if vegan_veg_pesc_dummy==0, vce(hc3)
    matrix rtable6 = r(table)
    scalar nobs6 = e(N)
	local r2_6 = string(e(r2), "%9.3f")
	local r2_adj_6 = string(e(r2_a), "%9.3f")
    qui regress meat_day_dummy 1.HT `var2' 1.HT#1.`var2' FR_dummy LV_dummy if vegan_veg_pesc_dummy==0, vce(hc3)
    matrix rtable7 = r(table)
    scalar nobs7 = e(N)
	local r2_7 = string(e(r2), "%9.3f")
	local r2_adj_7 = string(e(r2_a), "%9.3f")
    qui regress meat_day_s 1.HT `var2' 1.HT#1.`var2' FR_dummy LV_dummy if vegan_veg_pesc_dummy==0, vce(hc3)
    matrix rtable8 = r(table)
    scalar nobs8 = e(N)
	local r2_8 = string(e(r2), "%9.3f")
	local r2_adj_8 = string(e(r2_a), "%9.3f")
	* coeff and N for var3	
	qui regress meat_tax_dummy 1.HT `var3' 1.HT#1.`var3' FR_dummy LV_dummy if vegan_veg_pesc_dummy==0, vce(hc3)
    matrix rtable9 = r(table)
    scalar nobs9 = e(N)
	local r2_9 = string(e(r2), "%9.3f")
	local r2_adj_9 = string(e(r2_a), "%9.3f")
    qui regress meat_tax_s 1.HT `var3' 1.HT#1.`var3' FR_dummy LV_dummy if vegan_veg_pesc_dummy==0, vce(hc3)
    matrix rtable10 = r(table)
    scalar nobs10 = e(N)
	local r2_10 = string(e(r2), "%9.3f")
	local r2_adj_10 = string(e(r2_a), "%9.3f")
    qui regress meat_day_dummy 1.HT `var3' 1.HT#1.`var3' FR_dummy LV_dummy if vegan_veg_pesc_dummy==0, vce(hc3)
    matrix rtable11 = r(table)
    scalar nobs11 = e(N)
	local r2_11 = string(e(r2), "%9.3f")
	local r2_adj_11 = string(e(r2_a), "%9.3f")
    qui regress meat_day_s 1.HT `var3' 1.HT#1.`var3' FR_dummy LV_dummy if vegan_veg_pesc_dummy==0, vce(hc3)
    matrix rtable12 = r(table)
    scalar nobs12 = e(N)
	local r2_12 = string(e(r2), "%9.3f")
	local r2_adj_12 = string(e(r2_a), "%9.3f")

	local nb 10000 //number of bootsraps

    local file_path = "robustness_heterog/r6_wo_veg_`var1'.xlsx"
    
    // Check if the file exists
    if !fileexists("`file_path'") {
    * Westfall-Young adjusted p-values for variable 1
    qui wyoung, cmd("regress meat_tax_dummy 1.HT `var1' 1.HT#1.`var1' FR_dummy LV_dummy if vegan_veg_pesc_dummy==0, vce(hc3)" ///
        "regress meat_tax_s 1.HT `var1' 1.HT#1.`var1' FR_dummy LV_dummy if vegan_veg_pesc_dummy==0, vce(hc3)" ///
        "regress meat_day_dummy 1.HT `var1' 1.HT#1.`var1' FR_dummy LV_dummy if vegan_veg_pesc_dummy==0, vce(hc3)" ///
        "regress meat_day_s 1.HT `var1' 1.HT#1.`var1' FR_dummy LV_dummy if vegan_veg_pesc_dummy==0, vce(hc3)"  ///
		"regress meat_tax_dummy 1.HT `var1' 1.HT#1.`var1' FR_dummy LV_dummy if vegan_veg_pesc_dummy==0, vce(hc3)" ///
        "regress meat_tax_s 1.HT `var1' 1.HT#1.`var1' FR_dummy LV_dummy if vegan_veg_pesc_dummy==0, vce(hc3)" ///
        "regress meat_day_dummy 1.HT `var1' 1.HT#1.`var1' FR_dummy LV_dummy if vegan_veg_pesc_dummy==0, vce(hc3)" ///
        "regress meat_day_s 1.HT `var1' 1.HT#1.`var1' FR_dummy LV_dummy if vegan_veg_pesc_dummy==0, vce(hc3)" ) ///
        familyp(1.HT 1.HT 1.HT 1.HT 1.HT+1.HT#1.`var1' 1.HT+1.HT#1.`var1' 1.HT+1.HT#1.`var1' 1.HT+1.HT#1.`var1') ///
        familypexp strata(country) ///
        bootstraps(`nb') seed(1234)
    
	matrix wy_results1 = r(table)
	putexcel set `file_path', replace
    putexcel A1=matrix(wy_results1), names
	
	}
		
	preserve
	
	import excel `file_path', sheet("Sheet1") firstrow clear
	mkmat coef stderr  p pwyoung pbonf psidak, matrix(wy_results1)
	

	restore
		
		
    local file_path = "robustness_heterog/r6_wo_veg_`var2'.xlsx"
	    * Westfall-Young adjusted p-values for variable 2
        if !fileexists("`file_path'") {
			qui wyoung, cmd("regress meat_tax_dummy 1.HT `var2' 1.HT#1.`var2' FR_dummy LV_dummy if vegan_veg_pesc_dummy==0, vce(hc3)" ///
        "regress meat_tax_s 1.HT `var2' 1.HT#1.`var2' FR_dummy LV_dummy if vegan_veg_pesc_dummy==0, vce(hc3)" ///
        "regress meat_day_dummy 1.HT `var2' 1.HT#1.`var2' FR_dummy LV_dummy if vegan_veg_pesc_dummy==0, vce(hc3)" ///
        "regress meat_day_s 1.HT `var2' 1.HT#1.`var2' FR_dummy LV_dummy if vegan_veg_pesc_dummy==0, vce(hc3)"  ///
		"regress meat_tax_dummy 1.HT `var2' 1.HT#1.`var2' FR_dummy LV_dummy if vegan_veg_pesc_dummy==0, vce(hc3)" ///
        "regress meat_tax_s 1.HT `var2' 1.HT#1.`var2' FR_dummy LV_dummy if vegan_veg_pesc_dummy==0, vce(hc3)" ///
        "regress meat_day_dummy 1.HT `var2' 1.HT#1.`var2' FR_dummy LV_dummy if vegan_veg_pesc_dummy==0, vce(hc3)" ///
        "regress meat_day_s 1.HT `var2' 1.HT#1.`var2' FR_dummy LV_dummy if vegan_veg_pesc_dummy==0, vce(hc3)" ) ///
        familyp(1.HT 1.HT 1.HT 1.HT 1.HT+1.HT#1.`var2' 1.HT+1.HT#1.`var2' 1.HT+1.HT#1.`var2' 1.HT+1.HT#1.`var2') ///
        familypexp strata(country) ///
        bootstraps(`nb') seed(1234)
    
	matrix wy_results2 = r(table)	
	putexcel set `file_path', replace
    putexcel A1=matrix(wy_results2), names
		}
	
	preserve
	
	import excel `file_path', sheet("Sheet1") firstrow clear
	mkmat coef stderr  p pwyoung pbonf psidak, matrix(wy_results2)


	restore
	
    local file_path = "robustness_heterog/r6_wo_veg_`var3'.xlsx"
		* Westfall-Young adjusted p-values for variable 3
    if !fileexists("`file_path'") {
		qui wyoung, cmd("regress meat_tax_dummy 1.HT `var3' 1.HT#1.`var3' FR_dummy LV_dummy if vegan_veg_pesc_dummy==0,vce(hc3)" ///
        "regress meat_tax_s 1.HT `var3' 1.HT#1.`var3' FR_dummy LV_dummy if vegan_veg_pesc_dummy==0, vce(hc3)" ///
        "regress meat_day_dummy 1.HT `var3' 1.HT#1.`var3' FR_dummy LV_dummy if vegan_veg_pesc_dummy==0, vce(hc3)" ///
        "regress meat_day_s 1.HT `var3' 1.HT#1.`var3' FR_dummy LV_dummy if vegan_veg_pesc_dummy==0, vce(hc3)"  ///
		"regress meat_tax_dummy 1.HT `var3' 1.HT#1.`var3' FR_dummy LV_dummy if vegan_veg_pesc_dummy==0, vce(hc3)" ///
        "regress meat_tax_s 1.HT `var3' 1.HT#1.`var3' FR_dummy LV_dummy if vegan_veg_pesc_dummy==0, vce(hc3)" ///
        "regress meat_day_dummy 1.HT `var3' 1.HT#1.`var3' FR_dummy LV_dummy if vegan_veg_pesc_dummy==0, vce(hc3)" ///
        "regress meat_day_s 1.HT `var3' 1.HT#1.`var3' FR_dummy LV_dummy if vegan_veg_pesc_dummy==0, vce(hc3)" ) ///
        familyp(1.HT 1.HT 1.HT 1.HT 1.HT+1.HT#1.`var3' 1.HT+1.HT#1.`var3' 1.HT+1.HT#1.`var3' 1.HT+1.HT#1.`var3') ///
        familypexp strata(country) ///
        bootstraps(`nb') seed(1234)
    
	matrix wy_results3 = r(table)
	putexcel set `file_path', replace
    putexcel A1=matrix(wy_results3), names
	}
		
	preserve
	
	import excel `file_path', sheet("Sheet1") firstrow clear
	
	mkmat coef stderr  p pwyoung pbonf psidak, matrix(wy_results3)


	restore
			
		
	local lbl1 : variable label `var1'
	local lbl2 : variable label `var2'
	local lbl3 : variable label `var3'	
	
	local low_lbl1 = lower("`lbl1'")
	local low_lbl2 = lower("`lbl2'")
	local low_lbl3 = lower("`lbl3'")
	
    * Add title
    putdocx paragraph
    putdocx text ("Table SC`table_num' LPM results of the heterogeneity models for ")
	putdocx text ("`low_lbl1'"), italic
	putdocx text (", ")
	putdocx text ("`low_lbl2'"), italic
	putdocx text (", and ")
	putdocx text ("`low_lbl3'"), italic
	putdocx text (", using meat eaters only - pooled sample."), font("", 11)
	
    putdocx table mytable = (12, 13), border(all, nil) width(4) width(100%) 
	


    * Create a table with 5 rows and 5 columns (1 for covariate names, 4 for models)
	putdocx table mytable(1,2) = ("`lbl1'"), colspan(4) italic
	putdocx table mytable(1,3) = ("`lbl2'"), colspan(4) italic
	putdocx table mytable(1,4) = ("`lbl3'"), colspan(4) italic
    * Headers for each model
    putdocx table mytable(2,2) = ("Meat tax acceptability for oneself")
    putdocx table mytable(2,3) = ("Meat tax acceptability for society")
    putdocx table mytable(2,4) = ("Meat-free days acceptability for oneself")
    putdocx table mytable(2,5) = ("Meat-free days acceptability for society")
    putdocx table mytable(2,6) = ("Meat tax acceptability for oneself")
    putdocx table mytable(2,7) = ("Meat tax acceptability for society")
    putdocx table mytable(2,8) = ("Meat-free days acceptability for oneself")
    putdocx table mytable(2,9) = ("Meat-free days acceptability for society")
    putdocx table mytable(2,10) = ("Meat tax acceptability for oneself")
    putdocx table mytable(2,11) = ("Meat tax acceptability for society")
    putdocx table mytable(2,12) = ("Meat-free days acceptability for oneself")
    putdocx table mytable(2,13) = ("Meat-free days acceptability for society")


	
    local covariates HT "`var1'" "1.HT#1.`var1'" FR_dummy LV_dummy _cons
	local interaction_label "Health-risk information # heterogeneity variable"
	local het_label "Heterogeneity variable"

    local nvars : word count `covariates'
	local row 3
    local rowse 4

	//di `nvars'
	//di `covariates'
	//matrix list rtable1

  forvalues covar_num = 1/`nvars' {
	local covar : word `covar_num' of `covariates'
		if "`covar'"== "_cons"  {
			local covar_label Constant
			putdocx table mytable(`row',1) = ("`covar_label'")
			}	
			else {
				if "`covar'" == "1.HT#1.`var1'" | "`covar'" == "1.HT#1.`var2'" |  "`covar'" == "1.HT#1.`var3'" {
				local covar_label "`interaction_label'"
				putdocx table mytable(`row',1) = ("`covar_label'"), italic
				}
				else {
					if "`covar'" == "`var1'" | "`covar'" == "`var2'" | "`covar'" == "`var3'" {
					local covar_label "`het_label'"
					putdocx table mytable(`row',1) = ("`covar_label'"), italic
					} 
					else {
						local covar_label : variable label `covar'
						putdocx table mytable(`row',1) = ("`covar_label'"), italic
						}
					}
				}


		 
					

        * Extract coefficients and standard errors
		local b1_val: display %9.3f rtable1[1,`covar_num']
		local se1_val: display %9.3f rtable1[2,`covar_num']
		local p1_val: display %9.3f rtable1[4,`covar_num']
		

		local b2_val: display %9.3f rtable2[1,`covar_num']
		local se2_val: display %9.3f rtable2[2,`covar_num']
		local p2_val: display %9.3f rtable2[4,`covar_num']
		
		local b3_val: display %9.3f rtable3[1,`covar_num']
		local se3_val: display %9.3f rtable3[2,`covar_num']
		local p3_val: display %9.3f rtable3[4,`covar_num']
		
		local b4_val: display %9.3f rtable4[1,`covar_num']
		local se4_val: display %9.3f rtable4[2,`covar_num']
		local p4_val: display %9.3f rtable4[4,`covar_num']

		local b5_val: display %9.3f rtable5[1,`covar_num']
		local se5_val: display %9.3f rtable5[2,`covar_num']
		local p5_val: display %9.3f rtable5[4,`covar_num']
		
		
		local b6_val: display %9.3f rtable6[1,`covar_num']
		local se6_val: display %9.3f rtable6[2,`covar_num']
		local p6_val: display %9.3f rtable6[4,`covar_num']
		
		local b7_val: display %9.3f rtable7[1,`covar_num']
		local se7_val: display %9.3f rtable7[2,`covar_num']
		local p7_val: display %9.3f rtable7[4,`covar_num']
		
		local b8_val: display %9.3f rtable8[1,`covar_num']
		local se8_val: display %9.3f rtable8[2,`covar_num']
		local p8_val: display %9.3f rtable8[4,`covar_num']
		
		local b9_val: display %9.3f rtable9[1,`covar_num']
		local se9_val: display %9.3f rtable9[2,`covar_num']
		local p9_val: display %9.3f rtable9[4,`covar_num']
		
		
		local b10_val: display %9.3f rtable10[1,`covar_num']
		local se10_val: display %9.3f rtable10[2,`covar_num']
		local p10_val: display %9.3f rtable10[4,`covar_num']
		
		local b11_val: display %9.3f rtable11[1,`covar_num']
		local se11_val: display %9.3f rtable11[2,`covar_num']
		local p11_val: display %9.3f rtable11[4,`covar_num']
		
		local b12_val: display %9.3f rtable12[1,`covar_num']
		local se12_val: display %9.3f rtable12[2,`covar_num']
		local p12_val: display %9.3f rtable12[4,`covar_num']
		* Adjusted p-values
		
		local wy_pvalue1: display %9.3f (wy_results1[1, 4])
		local wy_pvalue2: display %9.3f (wy_results1[2, 4])
		local wy_pvalue3: display %9.3f (wy_results1[3, 4])
		local wy_pvalue4: display %9.3f (wy_results1[4, 4])
		local wy_pvalue5: display %9.3f (wy_results2[1, 4])
		local wy_pvalue6: display %9.3f (wy_results2[2, 4])
		local wy_pvalue7: display %9.3f (wy_results2[3, 4])
		local wy_pvalue8: display %9.3f (wy_results2[4, 4])
		local wy_pvalue9: display %9.3f (wy_results3[1, 4])
		local wy_pvalue10: display %9.3f (wy_results3[2, 4])
		local wy_pvalue11: display %9.3f (wy_results3[3, 4])
		local wy_pvalue12: display %9.3f (wy_results3[4, 4])
	


        * Add stars based on p-values
        local stars1 = ""
        if `p1_val' < 0.001 local stars1 = "***"
        else if `p1_val' < 0.01 local stars1 = "**"
        else if `p1_val' < 0.05 local stars1 = "*"

        local stars2 = ""
        if `p2_val' < 0.001 local stars2 = "***"
        else if `p2_val' < 0.01 local stars2 = "**"
        else if `p2_val' < 0.05 local stars2 = "*"

        local stars3 = ""
        if `p3_val' < 0.001 local stars3 = "***"
        else if `p3_val' < 0.01 local stars3 = "**"
        else if `p3_val' < 0.05 local stars3 = "*"

        local stars4 = ""
        if `p4_val' < 0.001 local stars4 = "***"
        else if `p4_val' < 0.01 local stars4 = "**"
        else if `p4_val' < 0.05 local stars4 = "*"
		
		local stars5 = ""
		if `p5_val' < 0.001 local stars5 = "***"
		else if `p5_val' < 0.01 local stars5 = "**"
		else if `p5_val' < 0.05 local stars5 = "*"

		local stars6 = ""
		if `p6_val' < 0.001 local stars6 = "***"
		else if `p6_val' < 0.01 local stars6 = "**"
		else if `p6_val' < 0.05 local stars6 = "*"

		local stars7 = ""
		if `p7_val' < 0.001 local stars7 = "***"
		else if `p7_val' < 0.01 local stars7 = "**"
		else if `p7_val' < 0.05 local stars7 = "*"

		local stars8 = ""
		if `p8_val' < 0.001 local stars8 = "***"
		else if `p8_val' < 0.01 local stars8 = "**"
		else if `p8_val' < 0.05 local stars8 = "*"

		local stars9 = ""
		if `p9_val' < 0.001 local stars9 = "***"
		else if `p9_val' < 0.01 local stars9 = "**"
		else if `p9_val' < 0.05 local stars9 = "*"

		local stars10 = ""
		if `p10_val' < 0.001 local stars10 = "***"
		else if `p10_val' < 0.01 local stars10 = "**"
		else if `p10_val' < 0.05 local stars10 = "*"

		local stars11 = ""
		if `p11_val' < 0.001 local stars11 = "***"
		else if `p11_val' < 0.01 local stars11 = "**"
		else if `p11_val' < 0.05 local stars11 = "*"

		local stars12 = ""
		if `p12_val' < 0.001 local stars12 = "***"
		else if `p12_val' < 0.01 local stars12 = "**"
		else if `p12_val' < 0.05 local stars12 = "*"


        * Adjusted p-values for HT and HT#var variable
		if "`covar'" == "HT"  {
			local stars_adj1 = ""
			if `wy_pvalue1' < 0.001 {
				local stars_adj1 = "+++"
			}
			else if `wy_pvalue1' < 0.01 {
				local stars_adj1 = "++"
			}
			else if `wy_pvalue1' < 0.05 {
				local stars_adj1 = "+"
			}

			local stars_adj2 = ""
			if `wy_pvalue2' < 0.001 {
				local stars_adj2 = "+++"
			}
			else if `wy_pvalue2' < 0.01 {
				local stars_adj2 = "++"
			}
			else if `wy_pvalue2' < 0.05 {
				local stars_adj2 = "+"
			}

			local stars_adj3 = ""
			if `wy_pvalue3' < 0.001 {
				local stars_adj3 = "+++"
			}
			else if `wy_pvalue3' < 0.01 {
				local stars_adj3 = "++"
			}
			else if `wy_pvalue3' < 0.05 {
				local stars_adj3 = "+"
			}

			local stars_adj4 = ""
			if `wy_pvalue4' < 0.001 {
				local stars_adj4 = "+++"
			}
			else if `wy_pvalue4' < 0.01 {
				local stars_adj4 = "++"
			}
			else if `wy_pvalue4' < 0.05 {
				local stars_adj4 = "+"
			}

			local stars_adj5 = ""
			if `wy_pvalue5' < 0.001 {
				local stars_adj5 = "+++"
			}
			else if `wy_pvalue5' < 0.01 {
				local stars_adj5 = "++"
			}
			else if `wy_pvalue5' < 0.05 {
				local stars_adj5 = "+"
			}

			local stars_adj6 = ""
			if `wy_pvalue6' < 0.001 {
				local stars_adj6 = "+++"
			}
			else if `wy_pvalue6' < 0.01 {
				local stars_adj6 = "++"
			}
			else if `wy_pvalue6' < 0.05 {
				local stars_adj6 = "+"
			}

			local stars_adj7 = ""
			if `wy_pvalue7' < 0.001 {
				local stars_adj7 = "+++"
			}
			else if `wy_pvalue7' < 0.01 {
				local stars_adj7 = "++"
			}
			else if `wy_pvalue7' < 0.05 {
				local stars_adj7 = "+"
			}

			local stars_adj8 = ""
			if `wy_pvalue8' < 0.001 {
				local stars_adj8 = "+++"
			}
			else if `wy_pvalue8' < 0.01 {
				local stars_adj8 = "++"
			}
			else if `wy_pvalue8' < 0.05 {
				local stars_adj8 = "+"
			}

			local stars_adj9 = ""
			if `wy_pvalue9' < 0.001 {
				local stars_adj9 = "+++"
			}
			else if `wy_pvalue9' < 0.01 {
				local stars_adj9 = "++"
			}
			else if `wy_pvalue9' < 0.05 {
				local stars_adj9 = "+"
			}

			local stars_adj10 = ""
			if `wy_pvalue10' < 0.001 {
				local stars_adj10 = "+++"
			}
			else if `wy_pvalue10' < 0.01 {
				local stars_adj10 = "++"
			}
			else if `wy_pvalue10' < 0.05 {
				local stars_adj10 = "+"
			}

			local stars_adj11 = ""
			if `wy_pvalue11' < 0.001 {
				local stars_adj11 = "+++"
			}
			else if `wy_pvalue11' < 0.01 {
				local stars_adj11 = "++"
			}
			else if `wy_pvalue11' < 0.05 {
				local stars_adj11 = "+"
			}

			local stars_adj12 = ""
			if `wy_pvalue12' < 0.001 {
				local stars_adj12 = "+++"
			}
			else if `wy_pvalue12' < 0.01 {
				local stars_adj12 = "++"
			}
			else if `wy_pvalue12' < 0.05 {
				local stars_adj12 = "+"
			}
		}
	
			if "`covar'" == "HT" {
		* Insert coefficients, standard errors, and stars for each model
		putdocx table mytable(`row',2) = ("`=strltrim("`b1_val'")'`stars1'`stars_adj1'"), halign(left) linebreak
		putdocx table mytable(`row',2) = ("(`=strltrim("`se1_val'")')"), append linebreak
		putdocx table mytable(`row',3) = ("`=strltrim("`b2_val'")'`stars2'`stars_adj2'"), halign(left) linebreak
		putdocx table mytable(`row',3) = ("(`=strltrim("`se2_val'")')"), append linebreak
		putdocx table mytable(`row',4) = ("`=strltrim("`b3_val'")'`stars3'`stars_adj3'"), halign(left) linebreak
		putdocx table mytable(`row',4) = ("(`=strltrim("`se3_val'")')"), append linebreak
		putdocx table mytable(`row',5) = ("`=strltrim("`b4_val'")'`stars4'`stars_adj4'"), halign(left) linebreak
		putdocx table mytable(`row',5) = ("(`=strltrim("`se4_val'")')"), append linebreak
		putdocx table mytable(`row',6) = ("`=strltrim("`b5_val'")'`stars5'`stars_adj5'"), halign(left) linebreak
		putdocx table mytable(`row',6) = ("(`=strltrim("`se5_val'")')"), append linebreak
		putdocx table mytable(`row',7) = ("`=strltrim("`b6_val'")'`stars6'`stars_adj6'"), halign(left) linebreak
		putdocx table mytable(`row',7) = ("(`=strltrim("`se6_val'")')"), append linebreak
		putdocx table mytable(`row',8) = ("`=strltrim("`b7_val'")'`stars7'`stars_adj7'"), halign(left) linebreak
		putdocx table mytable(`row',8) = ("(`=strltrim("`se7_val'")')"), append linebreak
		putdocx table mytable(`row',9) = ("`=strltrim("`b8_val'")'`stars8'`stars_adj8'"), halign(left) linebreak
		putdocx table mytable(`row',9) = ("(`=strltrim("`se8_val'")')"), append linebreak
		putdocx table mytable(`row',10) = ("`=strltrim("`b9_val'")'`stars9'`stars_adj9'"), halign(left) linebreak
		putdocx table mytable(`row',10) = ("(`=strltrim("`se9_val'")')"), append linebreak
		putdocx table mytable(`row',11) = ("`=strltrim("`b10_val'")'`stars10'`stars_adj10'"), halign(left) linebreak
		putdocx table mytable(`row',11) = ("(`=strltrim("`se10_val'")')"), append linebreak
		putdocx table mytable(`row',12) = ("`=strltrim("`b11_val'")'`stars11'`stars_adj11'"), halign(left) linebreak
		putdocx table mytable(`row',12) = ("(`=strltrim("`se11_val'")')"), append linebreak
		putdocx table mytable(`row',13) = ("`=strltrim("`b12_val'")'`stars12'`stars_adj12'"), halign(left) linebreak
		putdocx table mytable(`row',13) = ("(`=strltrim("`se12_val'")')"), append linebreak
	
	}
	else {
		putdocx table mytable(`row',2) = ("`=strltrim("`b1_val'")'`stars1'`stars_adj1'"), halign(left) linebreak
		putdocx table mytable(`row',2) = ("(`=strltrim("`se1_val'")')"), append
		putdocx table mytable(`row',3) = ("`=strltrim("`b2_val'")'`stars2'`stars_adj2'"), halign(left) linebreak
		putdocx table mytable(`row',3) = ("(`=strltrim("`se2_val'")')"), append
		putdocx table mytable(`row',4) = ("`=strltrim("`b3_val'")'`stars3'`stars_adj3'"), halign(left) linebreak
		putdocx table mytable(`row',4) = ("(`=strltrim("`se3_val'")')"), append 
		putdocx table mytable(`row',5) = ("`=strltrim("`b4_val'")'`stars4'`stars_adj4'"), halign(left) linebreak
		putdocx table mytable(`row',5) = ("(`=strltrim("`se4_val'")')"), append 
		putdocx table mytable(`row',6) = ("`=strltrim("`b5_val'")'`stars5'`stars_adj5'"), halign(left) linebreak
		putdocx table mytable(`row',6) = ("(`=strltrim("`se5_val'")')"), append 
		putdocx table mytable(`row',7) = ("`=strltrim("`b6_val'")'`stars6'`stars_adj6'"), halign(left) linebreak
		putdocx table mytable(`row',7) = ("(`=strltrim("`se6_val'")')"), append 
		putdocx table mytable(`row',8) = ("`=strltrim("`b7_val'")'`stars7'`stars_adj7'"), halign(left) linebreak
		putdocx table mytable(`row',8) = ("(`=strltrim("`se7_val'")')"), append 
		putdocx table mytable(`row',9) = ("`=strltrim("`b8_val'")'`stars8'`stars_adj8'"), halign(left) linebreak
		putdocx table mytable(`row',9) = ("(`=strltrim("`se8_val'")')"), append 
		putdocx table mytable(`row',10) = ("`=strltrim("`b9_val'")'`stars9'`stars_adj9'"), halign(left) linebreak
		putdocx table mytable(`row',10) = ("(`=strltrim("`se9_val'")')"), append 
		putdocx table mytable(`row',11) = ("`=strltrim("`b10_val'")'`stars10'`stars_adj10'"), halign(left) linebreak
		putdocx table mytable(`row',11) = ("(`=strltrim("`se10_val'")')"), append 
		putdocx table mytable(`row',12) = ("`=strltrim("`b11_val'")'`stars11'`stars_adj11'"), halign(left) linebreak
		putdocx table mytable(`row',12) = ("(`=strltrim("`se11_val'")')"), append 
		putdocx table mytable(`row',13) = ("`=strltrim("`b12_val'")'`stars12'`stars_adj12'"), halign(left) linebreak
		putdocx table mytable(`row',13) = ("(`=strltrim("`se12_val'")')"), append 
	}
    

	

        * Increment row
        local row = `row' + 1
        
		
	local stars_adj1 ""
	local stars_adj2 ""
	local stars_adj3 ""
	local stars_adj4 ""
	local stars_adj5 ""
	local stars_adj6 ""
	local stars_adj7 ""
	local stars_adj8 ""
	local stars_adj9 ""
	local stars_adj10 ""
	local stars_adj11 ""
	local stars_adj12 ""


	}

	

	



local wy_pvalue1: display %9.3f (wy_results1[1, 4])
local wy_pvalue2: display %9.3f (wy_results1[2, 4])
local wy_pvalue3: display %9.3f (wy_results1[3, 4])
local wy_pvalue4: display %9.3f (wy_results1[4, 4])
local wy_pvalue5: display %9.3f (wy_results2[1, 4])
local wy_pvalue6: display %9.3f (wy_results2[2, 4])
local wy_pvalue7: display %9.3f (wy_results2[3, 4])
local wy_pvalue8: display %9.3f (wy_results2[4, 4])
local wy_pvalue9: display %9.3f (wy_results3[1, 4])
local wy_pvalue10: display %9.3f (wy_results3[2, 4])
local wy_pvalue11: display %9.3f (wy_results3[3, 4])
local wy_pvalue12: display %9.3f (wy_results3[4, 4])

putdocx table mytable(3, 2) = ("[`=strltrim("`wy_pvalue1'")']"), append
putdocx table mytable(3, 3) = ("[`=strltrim("`wy_pvalue2'")']"), append
putdocx table mytable(3, 4) = ("[`=strltrim("`wy_pvalue3'")']"), append
putdocx table mytable(3, 5) = ("[`=strltrim("`wy_pvalue4'")']"), append
putdocx table mytable(3, 6) = ("[`=strltrim("`wy_pvalue5'")']"), append
putdocx table mytable(3, 7) = ("[`=strltrim("`wy_pvalue6'")']"), append
putdocx table mytable(3, 8) = ("[`=strltrim("`wy_pvalue7'")']"), append
putdocx table mytable(3, 9) = ("[`=strltrim("`wy_pvalue8'")']"), append
putdocx table mytable(3, 10) = ("[`=strltrim("`wy_pvalue9'")']"), append
putdocx table mytable(3, 11) = ("[`=strltrim("`wy_pvalue10'")']"), append
putdocx table mytable(3, 12) = ("[`=strltrim("`wy_pvalue11'")']"), append
putdocx table mytable(3, 13) = ("[`=strltrim("`wy_pvalue12'")']"), append



*adding observations
putdocx table mytable(`row',1) = ("N")
putdocx table mytable(`row',2) = (nobs1)
putdocx table mytable(`row',3) = (nobs2)
putdocx table mytable(`row',4) = (nobs3)
putdocx table mytable(`row',5) = (nobs4)
putdocx table mytable(`row',6) = (nobs5)
putdocx table mytable(`row',7) = (nobs6)
putdocx table mytable(`row',8) = (nobs7)
putdocx table mytable(`row',9) = (nobs8)
putdocx table mytable(`row',10) = (nobs9)
putdocx table mytable(`row',11) = (nobs10)
putdocx table mytable(`row',12) = (nobs11)
putdocx table mytable(`row',13) = (nobs12)



local row = `row' + 1
local row2 = `row' + 1

*Adding R² and adjusted R²
	
putdocx table mytable(`row',1) = ("R²")
putdocx table mytable(`row2',1) = ("R²adj")

	forvalues mod = 1/12 {		
		local col_num = `mod' + 1
		
		putdocx table mytable(`row',`col_num') = ("`=strltrim("`r2_`mod''")'")
		putdocx table mytable(`row2',`col_num') =("`=strltrim("`r2_adj_`mod''")'")
	}
	
	local row = `row' + 1

	* Adding  horizontal lines
	forvalues col_num = 1/13 {
		local row_2=`row'-2
		putdocx table mytable(1,`col_num'), border(top, "single")
		putdocx table mytable(2,`col_num'), border(bottom, "single")
		putdocx table mytable(`row_2',`col_num'), border(top, "single")
		putdocx table mytable(`row',`col_num'), border(bottom, "single")
	}
	 forvalues col_num = 2/13 {
		putdocx table mytable(1,`col_num'), border(bottom, "single")
	}



	local row = `row' + 1
	local table_num  = `table_num' + 1

putdocx table mytable(`row',1) = ("Note: Robust standard errors in parentheses."), colspan(13) linebreak 
putdocx table mytable(`row',1) = ("For health-risk information, Westfall-Young MHT adjusted p-values in brackets with 10,000 bootstrap replications. "),  append linebreak 
putdocx table mytable(`row',1) = ("***p<0.001, **p<0.01, *p<0.05 for standard p-values, +++p< 0.001, ++p < 0.01, +p < 0.05 for Westfall-Young MHT adjusted p-values."),  append
	putdocx pagebreak
}

}


* ========================
**# Table SC29: Robustness test (basic) 5: Including speeders
* ========================
   

qui {
		
preserve

use "data/clean_data_with_speeder.dta", clear
rename health_treat HT 

qui regress meat_tax_dummy HT FR_dummy LV_dummy, vce(hc3)
matrix rtable1=r(table)
scalar nobs1 = e(N) 
	local r2_1 = string(e(r2), "%9.3f")
	local r2_adj_1 = string(e(r2_a), "%9.3f")

qui regress meat_tax_s HT FR_dummy LV_dummy, vce(hc3)
matrix rtable2=r(table)
scalar nobs2 = e(N)
	local r2_2 = string(e(r2), "%9.3f")
	local r2_adj_2 = string(e(r2_a), "%9.3f")

qui regress meat_day_dummy HT FR_dummy LV_dummy, vce(hc3)
matrix rtable3=r(table)
scalar nobs3 = e(N)
	local r2_3 = string(e(r2), "%9.3f")
	local r2_adj_3 = string(e(r2_a), "%9.3f")

qui regress meat_day_s HT FR_dummy LV_dummy, vce(hc3)
matrix rtable4=r(table)
scalar nobs4 = e(N)
	local r2_4 = string(e(r2), "%9.3f")
	local r2_adj_4 = string(e(r2_a), "%9.3f")

restore 
local file_path = "robustness_main/r10_speed.xlsx"
    
if !fileexists("`file_path'") {
*Westfall-Young adjusted p-values
wyoung, cmd("regress meat_tax_dummy HT FR_dummy LV_dummy, vce(hc3)" ///
"regress meat_tax_s HT FR_dummy LV_dummy, vce(hc3)" ///
"regress meat_day_dummy HT FR_dummy LV_dummy, vce(hc3)" ///
"regress meat_day_s HT FR_dummy LV_dummy, vce(hc3)") ///
familyp("HT" "HT" "HT" "HT") familypexp strata(country) bootstraps(10000) seed(1234)
    
matrix wy_results = r(table)
putexcel set `file_path', replace
putexcel A1=matrix(wy_results), names
}
	
preserve

import excel `file_path', sheet("Sheet1") firstrow clear
mkmat coef stderr  p pwyoung pbonf psidak, matrix(wy_results)

restore

preserve

use "data/clean_data_with_speeder.dta", clear
rename health_treat HT 

label variable HT "Health-risk information"
label variable FR_dummy "France (vs. Italy)"
label variable LV_dummy "Latvia (vs. Italy)"
label variable food_depriv "Food deprivation"
label variable health_low_rec "Poor health"
label variable unemployed "Unemployed"
label variable educ_low "Only secondary education"
label variable canteen_dummy "Dine in canteen"
label variable age "Age"
label variable age_low "Below median age"
label variable female "Female"
label variable income_Q1 "First income decile"
label variable income_pp "Income (in T€)"
label variable vegan_veg_pesc_dummy "Meat-free diet (vs. varied and high-meat diets)"
label variable high_meat_dummy "High-meat diet (vs. low and varied-meat diets)"
label variable po1_dummy "Support nationally oriented policies"
label variable po2_dummy "Support social policies"
label variable po3_dummy "Support conservative policies"
label variable po4_dummy "Support liberal policies"
label variable po5_dummy "Support environmental policies"
label variable diet_knowl_high "Nutrition knowledge"
label variable cc_acc_high "Climate change acknowledgement"
label variable soc_norm_high "Meat reduction social norms"



*Add title
putdocx paragraph
putdocx text ("Table SC`table_num' LPM results of the basic models, including speeders - pooled sample."), font("", 11)	

*Create a table with 5 rows and 5 columns (1 for covariate names, 4 for models)
putdocx table mytable = (9, 5), border(all, nil) width(4) width(100%) 

*headers for each model
putdocx table mytable(1,1) = ("")
putdocx table mytable(1,2) = ("Meat tax acceptability for oneself")
putdocx table mytable(1,3) = ("Meat Tax acceptability for society")
putdocx table mytable(1,4) = ("Meat-free days acceptability for oneself")
putdocx table mytable(1,5) = ("Meat-free days acceptability for society")



local covariates HT FR_dummy LV_dummy _cons
local nvars : word count `covariates'
local row 2  // Start from row 2 since row 1 has headers
local rowse 3

forvalues covar_num = 1/`nvars' {
	local covar : word `covar_num' of `covariates'
	if "`covar'" == "_cons" {
		local covar_label Constant
		putdocx table mytable(`row',1) = ("`covar_label'")
	}
		else {
			 local covar_label : variable label `covar'
			 putdocx table mytable(`row',1) = ("`covar_label'"), italic
			 }
		
	

	
    * Extract coefficients and standard errors
    local b1_val: display %9.3f rtable1[1,`covar_num']
    local se1_val: display %9.3f rtable1[2,`covar_num']
    local p1_val: display %9.3f rtable1[4,`covar_num']
	
    
    local b2_val: display %9.3f rtable2[1,`covar_num']
    local se2_val: display %9.3f rtable2[2,`covar_num']
    local p2_val: display %9.3f rtable2[4,`covar_num']
    
    local b3_val: display %9.3f rtable3[1,`covar_num']
    local se3_val: display %9.3f rtable3[2,`covar_num']
    local p3_val: display %9.3f rtable3[4,`covar_num']
    
    local b4_val: display %9.3f rtable4[1,`covar_num']
    local se4_val: display %9.3f rtable4[2,`covar_num']
    local p4_val: display %9.3f rtable4[4,`covar_num']

    * Adjusted p-values
	
	local wy_pvalue1: display %9.3f (wy_results[1, 4])
	local wy_pvalue2: display %9.3f (wy_results[2, 4])
	local wy_pvalue3: display %9.3f (wy_results[3, 4])
	local wy_pvalue4: display %9.3f (wy_results[4, 4])

	
	
    * Add stars based on p-values
    local stars1 = ""
    if `p1_val' < 0.001 {
        local stars1 = "***"
    }
    else if `p1_val' < 0.01 {
        local stars1 = "**"
    }
    else if `p1_val' < 0.05 {
        local stars1 = "*"
    }

    local stars2 = ""
    if `p2_val' < 0.001 {
        local stars2 = "***"
    }
    else if `p2_val' < 0.01 {
        local stars2 = "**"
    }
    else if `p2_val' < 0.05 {
        local stars2 = "*"
    }

    local stars3 = ""
    if `p3_val' < 0.001 {
        local stars3 = "***"
    }
    else if `p3_val' < 0.01 {
        local stars3 = "**"
    }
    else if `p3_val' < 0.05 {
        local stars3 = "*"
    }

    local stars4 = ""
    if `p4_val' < 0.001 {
        local stars4 = "***"
    }
    else if `p4_val' < 0.01 {
        local stars4 = "**"
    }
    else if `p4_val' < 0.05 {
        local stars4 = "*"
    }
	

	
	* Add stars_adj based on adjusted p-values
	if "`covar'" == "HT" {
		local stars_adj1 = ""
		if `wy_pvalue1' < 0.001 {
			local stars_adj1 = "+++"
		}
		else if `wy_pvalue1' < 0.01 {
			local stars_adj1 = "++"
		}
		else if `wy_pvalue1' < 0.05 {
			local stars_adj1 = "+"
		}

			local stars_adj2 = ""
		if `wy_pvalue2' < 0.001 {
			local stars_adj2 = "+++"
		}
		else if `wy_pvalue2' < 0.01 {
			local stars_adj2 = "++"
		}
		else if `wy_pvalue2' < 0.05 {
			local stars_adj2 = "+"
		}

			local stars_adj3 = ""
		if `wy_pvalue3' < 0.001 {
			local stars_adj3 = "+++"
		}
		else if `wy_pvalue3' < 0.01 {
			local stars_adj3 = "++"
		}
		else if `wy_pvalue3' < 0.05 {
			local stars_adj3 = "+"
		}

			local stars_adj4 = ""
		if `wy_pvalue4' < 0.001 {
			local stars_adj4 = "+++"
		}
		else if `wy_pvalue4' < 0.01 {
			local stars_adj4 = "++"
		}
		else if `wy_pvalue4' < 0.05 {
			local stars_adj4 = "+"
		}
	} 
	



		if "`covar'" == "HT" {
		* Insert coefficients, standard errors, and stars for each model
		putdocx table mytable(`row',2) = ("`=strltrim("`b1_val'")'`stars1'`stars_adj1'"), halign(left) linebreak
		putdocx table mytable(`row',2) = ("(`=strltrim("`se1_val'")')"), append linebreak
		putdocx table mytable(`row',3) = ("`=strltrim("`b2_val'")'`stars2'`stars_adj2'"), halign(left) linebreak
		putdocx table mytable(`row',3) = ("(`=strltrim("`se2_val'")')"), append linebreak
		putdocx table mytable(`row',4) = ("`=strltrim("`b3_val'")'`stars3'`stars_adj3'"), halign(left) linebreak
		putdocx table mytable(`row',4) = ("(`=strltrim("`se3_val'")')"), append linebreak
		putdocx table mytable(`row',5) = ("`=strltrim("`b4_val'")'`stars4'`stars_adj4'"), halign(left) linebreak
		putdocx table mytable(`row',5) = ("(`=strltrim("`se4_val'")')"), append linebreak
	
	}
	else {
		* Insert coefficients, standard errors, and stars for each model
		putdocx table mytable(`row',2) = ("`=strltrim("`b1_val'")'`stars1'`stars_adj1'"), halign(left) linebreak
		putdocx table mytable(`row',2) = ("(`=strltrim("`se1_val'")')"), append
		putdocx table mytable(`row',3) = ("`=strltrim("`b2_val'")'`stars2'`stars_adj2'"), halign(left) linebreak
		putdocx table mytable(`row',3) = ("(`=strltrim("`se2_val'")')"), append
		putdocx table mytable(`row',4) = ("`=strltrim("`b3_val'")'`stars3'`stars_adj3'"), halign(left) linebreak
		putdocx table mytable(`row',4) = ("(`=strltrim("`se3_val'")')"), append
		putdocx table mytable(`row',5) = ("`=strltrim("`b4_val'")'`stars4'`stars_adj4'"), halign(left) linebreak
		putdocx table mytable(`row',5) = ("(`=strltrim("`se4_val'")')"), append
	
	}
	
    
    * Increment the row counter for the next covariate
    local row = `row' + 1
	
	
	* Resetting stars to null
	local stars_adj1 = ""
	local stars_adj2 = ""
	local stars_adj3 = ""
	local stars_adj4 = ""
}



local wy_pvalue1: display %9.3f (wy_results[1, 4])
local wy_pvalue2: display %9.3f (wy_results[2, 4])
local wy_pvalue3: display %9.3f (wy_results[3, 4])
local wy_pvalue4: display %9.3f (wy_results[4, 4])



putdocx table mytable(2, 2) = ("[`=strltrim("`wy_pvalue1'")']"), append
putdocx table mytable(2, 3) = ("[`=strltrim("`wy_pvalue2'")']"), append
putdocx table mytable(2, 4) = ("[`=strltrim("`wy_pvalue3'")']"), append
putdocx table mytable(2, 5) = ("[`=strltrim("`wy_pvalue4'")']"), append

*adding observations
putdocx table mytable(`row',1) = ("N")
putdocx table mytable(`row',2) = (nobs1)
putdocx table mytable(`row',3) = (nobs2)
putdocx table mytable(`row',4) = (nobs3)
putdocx table mytable(`row',5) = (nobs4)




local row = `row' + 1
local row2 = `row' + 1

*Adding R² and adjusted R²
	
putdocx table mytable(`row',1) = ("R²")
putdocx table mytable(`row2',1) = ("R²adj")

	forvalues mod = 1/4 {		
		local col_num = `mod' + 1
		
		putdocx table mytable(`row',`col_num') = ("`=strltrim("`r2_`mod''")'")
		putdocx table mytable(`row2',`col_num') =("`=strltrim("`r2_adj_`mod''")'")
	}
	
	local row = `row' + 1

	* Adding  horizontal lines
	forvalues col_num = 1/5 {
		local row_2=`row'-2
		putdocx table mytable(1,`col_num'), border(top, "single")
		putdocx table mytable(1,`col_num'), border(bottom, "single")
		putdocx table mytable(`row_2',`col_num'), border(top, "single")
		putdocx table mytable(`row',`col_num'), border(bottom, "single")
	}


local row = `row' + 1

putdocx table mytable(`row',1) = ("Note: Robust standard errors in parentheses."), colspan(5) linebreak 
putdocx table mytable(`row',1) = ("For health-risk information, Westfall-Young MHT adjusted p-values in brackets with 10,000 bootstrap replications. "),  append linebreak 
putdocx table mytable(`row',1) = ("***p<0.001, **p<0.01, *p<0.05 for standard p-values, +++p< 0.001, ++p < 0.01, +p < 0.05 for Westfall-Young MHT adjusted p-values."),  append
putdocx pagebreak
}

restore


local table_num  = `table_num' + 1



* ========================
**# Tables S30 to S35: Robustness test (heterogeneity) 5: Including speeders
* ========================

qui{

foreach i in 1 2 3 4 5 6 {
	preserve

	use "data/clean_data_with_speeder.dta", clear

	rename health_treat HT 
	
    if `i'== 1 {
        local var1 food_depriv
        local var2 health_low_rec
        local var3 unemployed 
    }
    else if `i'== 2 {
        local var1 educ_low
        local var2 income_Q1
        local var3 age_low
    }
    else if `i'== 3 {
        local var1 female
        local var2 vegan_veg_pesc_dummy
        local var3 high_meat_dummy
    }               
    else if `i'== 4 {
        local var1 canteen_dummy
        local var2 po1_dummy
        local var3 po2_dummy
    } 
    else if `i'== 5 {
        local var1 po3_dummy
        local var2 po4_dummy
        local var3 po5_dummy
    }
    else if `i'== 6 {
        local var1 diet_knowl_high 
        local var2 cc_acc_high 
        local var3 soc_norm_high

    }
	
	local var1 = "`var1'"
	local var2 = "`var2'"
	local var3 = "`var3'"
	

	* coeff and N for var1
    qui regress meat_tax_dummy 1.HT `var1' 1.HT#1.`var1' FR_dummy LV_dummy, vce(hc3)
    matrix rtable1 = r(table)
    scalar nobs1 = e(N)
	local r2_1 = string(e(r2), "%9.3f")
	local r2_adj_1 = string(e(r2_a), "%9.3f")
    qui regress meat_tax_s 1.HT `var1' 1.HT#1.`var1' FR_dummy LV_dummy, vce(hc3)
    matrix rtable2 = r(table)
    scalar nobs2 = e(N)
	local r2_2 = string(e(r2), "%9.3f")
	local r2_adj_2 = string(e(r2_a), "%9.3f")
    qui regress meat_day_dummy 1.HT `var1' 1.HT#1.`var1' FR_dummy LV_dummy, vce(hc3)
    matrix rtable3 = r(table)
    scalar nobs3 = e(N)
	local r2_3 = string(e(r2), "%9.3f")
	local r2_adj_3 = string(e(r2_a), "%9.3f")
    qui regress meat_day_s 1.HT `var1' 1.HT#1.`var1' FR_dummy LV_dummy, vce(hc3)
    matrix rtable4 = r(table)
    scalar nobs4 = e(N)
	local r2_4 = string(e(r2), "%9.3f")
	local r2_adj_4 = string(e(r2_a), "%9.3f")
	* coeff and N for var2
	qui regress meat_tax_dummy 1.HT `var2' 1.HT#1.`var2' FR_dummy LV_dummy, vce(hc3)
    matrix rtable5 = r(table)
    scalar nobs5 = e(N)
	local r2_5 = string(e(r2), "%9.3f")
	local r2_adj_5 = string(e(r2_a), "%9.3f")
    qui regress meat_tax_s 1.HT `var2' 1.HT#1.`var2' FR_dummy LV_dummy, vce(hc3)
    matrix rtable6 = r(table)
    scalar nobs6 = e(N)
	local r2_6 = string(e(r2), "%9.3f")
	local r2_adj_6 = string(e(r2_a), "%9.3f")
    qui regress meat_day_dummy 1.HT `var2' 1.HT#1.`var2' FR_dummy LV_dummy, vce(hc3)
    matrix rtable7 = r(table)
    scalar nobs7 = e(N)
	local r2_7 = string(e(r2), "%9.3f")
	local r2_adj_7 = string(e(r2_a), "%9.3f")
    qui regress meat_day_s 1.HT `var2' 1.HT#1.`var2' FR_dummy LV_dummy, vce(hc3)
    matrix rtable8 = r(table)
    scalar nobs8 = e(N)
	local r2_8 = string(e(r2), "%9.3f")
	local r2_adj_8 = string(e(r2_a), "%9.3f")
	* coeff and N for var3	
	qui regress meat_tax_dummy 1.HT `var3' 1.HT#1.`var3' FR_dummy LV_dummy, vce(hc3)
    matrix rtable9 = r(table)
    scalar nobs9 = e(N)
	local r2_9 = string(e(r2), "%9.3f")
	local r2_adj_9 = string(e(r2_a), "%9.3f")
    qui regress meat_tax_s 1.HT `var3' 1.HT#1.`var3' FR_dummy LV_dummy, vce(hc3)
    matrix rtable10 = r(table)
    scalar nobs10 = e(N)
	local r2_10 = string(e(r2), "%9.3f")
	local r2_adj_10 = string(e(r2_a), "%9.3f")
    qui regress meat_day_dummy 1.HT `var3' 1.HT#1.`var3' FR_dummy LV_dummy, vce(hc3)
    matrix rtable11 = r(table)
    scalar nobs11 = e(N)
	local r2_11 = string(e(r2), "%9.3f")
	local r2_adj_11 = string(e(r2_a), "%9.3f")
    qui regress meat_day_s 1.HT `var3' 1.HT#1.`var3' FR_dummy LV_dummy, vce(hc3)
    matrix rtable12 = r(table)
    scalar nobs12 = e(N)
	local r2_12 = string(e(r2), "%9.3f")
	local r2_adj_12 = string(e(r2_a), "%9.3f")

	restore
	local nb 10000 //number of bootsraps

    local file_path = "robustness_heterog/r10_speed`var1'.xlsx"
    
    // Check if the file exists
    if !fileexists("`file_path'") {
    * Westfall-Young adjusted p-values for variable 1
    qui wyoung, cmd("regress meat_tax_dummy 1.HT `var1' 1.HT#1.`var1' FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_tax_s 1.HT `var1' 1.HT#1.`var1' FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_day_dummy 1.HT `var1' 1.HT#1.`var1' FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_day_s 1.HT `var1' 1.HT#1.`var1' FR_dummy LV_dummy, vce(hc3)"  ///
		"regress meat_tax_dummy 1.HT `var1' 1.HT#1.`var1' FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_tax_s 1.HT `var1' 1.HT#1.`var1' FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_day_dummy 1.HT `var1' 1.HT#1.`var1' FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_day_s 1.HT `var1' 1.HT#1.`var1' FR_dummy LV_dummy, vce(hc3)" ) ///
        familyp(1.HT 1.HT 1.HT 1.HT 1.HT+1.HT#1.`var1' 1.HT+1.HT#1.`var1' 1.HT+1.HT#1.`var1' 1.HT+1.HT#1.`var1') ///
        familypexp strata(country) ///
        bootstraps(`nb') seed(1234)
    
	matrix wy_results1 = r(table)
	putexcel set `file_path', replace
    putexcel A1=matrix(wy_results1), names
	
	}
		
	preserve
	
	import excel `file_path', sheet("Sheet1") firstrow clear
	mkmat coef stderr  p pwyoung pbonf psidak, matrix(wy_results1)
	

	restore
		
		
    local file_path = "robustness_heterog/r10_speed`var2'.xlsx"
	    * Westfall-Young adjusted p-values for variable 2
        if !fileexists("`file_path'") {
			qui wyoung, cmd("regress meat_tax_dummy 1.HT `var2' 1.HT#1.`var2' FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_tax_s 1.HT `var2' 1.HT#1.`var2' FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_day_dummy 1.HT `var2' 1.HT#1.`var2' FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_day_s 1.HT `var2' 1.HT#1.`var2' FR_dummy LV_dummy, vce(hc3)"  ///
		"regress meat_tax_dummy 1.HT `var2' 1.HT#1.`var2' FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_tax_s 1.HT `var2' 1.HT#1.`var2' FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_day_dummy 1.HT `var2' 1.HT#1.`var2' FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_day_s 1.HT `var2' 1.HT#1.`var2' FR_dummy LV_dummy, vce(hc3)" ) ///
        familyp(1.HT 1.HT 1.HT 1.HT 1.HT+1.HT#1.`var2' 1.HT+1.HT#1.`var2' 1.HT+1.HT#1.`var2' 1.HT+1.HT#1.`var2') ///
        familypexp strata(country) ///
        bootstraps(`nb') seed(1234)
    
	matrix wy_results2 = r(table)	
	putexcel set `file_path', replace
    putexcel A1=matrix(wy_results2), names
		}
	
	preserve
	
	import excel `file_path', sheet("Sheet1") firstrow clear
	mkmat coef stderr  p pwyoung pbonf psidak, matrix(wy_results2)


	restore
	
    local file_path = "robustness_heterog/r10_speed`var3'.xlsx"
		* Westfall-Young adjusted p-values for variable 3
    if !fileexists("`file_path'") {
		qui wyoung, cmd("regress meat_tax_dummy 1.HT `var3' 1.HT#1.`var3' FR_dummy LV_dummy,vce(hc3)" ///
        "regress meat_tax_s 1.HT `var3' 1.HT#1.`var3' FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_day_dummy 1.HT `var3' 1.HT#1.`var3' FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_day_s 1.HT `var3' 1.HT#1.`var3' FR_dummy LV_dummy, vce(hc3)"  ///
		"regress meat_tax_dummy 1.HT `var3' 1.HT#1.`var3' FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_tax_s 1.HT `var3' 1.HT#1.`var3' FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_day_dummy 1.HT `var3' 1.HT#1.`var3' FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_day_s 1.HT `var3' 1.HT#1.`var3' FR_dummy LV_dummy, vce(hc3)" ) ///
        familyp(1.HT 1.HT 1.HT 1.HT 1.HT+1.HT#1.`var3' 1.HT+1.HT#1.`var3' 1.HT+1.HT#1.`var3' 1.HT+1.HT#1.`var3') ///
        familypexp strata(country) ///
        bootstraps(`nb') seed(1234)
    
	matrix wy_results3 = r(table)
	putexcel set `file_path', replace
    putexcel A1=matrix(wy_results3), names
	}
		
	preserve
	
	import excel `file_path', sheet("Sheet1") firstrow clear
	
	mkmat coef stderr  p pwyoung pbonf psidak, matrix(wy_results3)


	restore
	
	preserve


	use "data/clean_data_with_speeder.dta", clear

	rename health_treat HT 
	
	label variable HT "Health-risk information"
	label variable FR_dummy "France (vs. Italy)"
	label variable LV_dummy "Latvia (vs. Italy)"
	label variable food_depriv "Food deprivation"
	label variable health_low_rec "Poor health"
	label variable unemployed "Unemployed"
	label variable educ_low "Only secondary education"
	label variable canteen_dummy "Dine in canteen"
	label variable age "Age"
	label variable age_low "Below median age"
	label variable female "Female"
	label variable income_Q1 "First income decile"
	label variable income_pp "Income (in T€)"
	label variable vegan_veg_pesc_dummy "Meat-free diet (vs. varied and high-meat diets)"
	label variable high_meat_dummy "High-meat diet (vs. low and varied-meat diets)"
	label variable po1_dummy "Support nationally oriented policies"
	label variable po2_dummy "Support social policies"
	label variable po3_dummy "Support conservative policies"
	label variable po4_dummy "Support liberal policies"
	label variable po5_dummy "Support environmental policies"
	label variable diet_knowl_high "Nutrition knowledge"
	label variable cc_acc_high "Climate change acknowledgement"
	label variable soc_norm_high "Meat reduction social norms"
	

		
	local lbl1 : variable label `var1'
	local lbl2 : variable label `var2'
	local lbl3 : variable label `var3'	
	
    * Add title
    putdocx paragraph
    putdocx text ("Table SC`table_num' LPM results of the heterogeneity models for `lbl1', `lbl2', and `lbl3', including speeders - pooled sample."), font("", 11)		
    putdocx table mytable = (12, 13), border(all, nil) width(4) width(100%) 
	


    * Create a table with 5 rows and 5 columns (1 for covariate names, 4 for models)
	putdocx table mytable(1,2) = ("`lbl1'"), colspan(4) italic
	putdocx table mytable(1,3) = ("`lbl2'"), colspan(4) italic
	putdocx table mytable(1,4) = ("`lbl3'"), colspan(4) italic
    * Headers for each model
    putdocx table mytable(2,2) = ("Meat tax acceptability for oneself")
    putdocx table mytable(2,3) = ("Meat tax acceptability for society")
    putdocx table mytable(2,4) = ("Meat-free days acceptability for oneself")
    putdocx table mytable(2,5) = ("Meat-free days acceptability for society")
    putdocx table mytable(2,6) = ("Meat tax acceptability for oneself")
    putdocx table mytable(2,7) = ("Meat tax acceptability for society")
    putdocx table mytable(2,8) = ("Meat-free days acceptability for oneself")
    putdocx table mytable(2,9) = ("Meat-free days acceptability for society")
    putdocx table mytable(2,10) = ("Meat tax acceptability for oneself")
    putdocx table mytable(2,11) = ("Meat tax acceptability for society")
    putdocx table mytable(2,12) = ("Meat-free days acceptability for oneself")
    putdocx table mytable(2,13) = ("Meat-free days acceptability for society")


	
    local covariates HT "`var1'" "1.HT#1.`var1'" FR_dummy LV_dummy _cons
	local interaction_label "Health-risk information # heterogeneity variable"
	local het_label "Heterogeneity variable"

    local nvars : word count `covariates'
	local row 3
    local rowse 4

	//di `nvars'
	//di `covariates'
	//matrix list rtable1

  forvalues covar_num = 1/`nvars' {
	local covar : word `covar_num' of `covariates'
		if "`covar'"== "_cons"  {
			local covar_label Constant
			putdocx table mytable(`row',1) = ("`covar_label'")
			}	
			else {
				if "`covar'" == "1.HT#1.`var1'" | "`covar'" == "1.HT#1.`var2'" |  "`covar'" == "1.HT#1.`var3'" {
				local covar_label "`interaction_label'"
				putdocx table mytable(`row',1) = ("`covar_label'"), italic
				}
				else {
					if "`covar'" == "`var1'" | "`covar'" == "`var2'" | "`covar'" == "`var3'" {
					local covar_label "`het_label'"
					putdocx table mytable(`row',1) = ("`covar_label'"), italic
					} 
					else {
						local covar_label : variable label `covar'
						putdocx table mytable(`row',1) = ("`covar_label'"), italic
						}
					}
				}


		 					

        * Extract coefficients and standard errors
		local b1_val: display %9.3f rtable1[1,`covar_num']
		local se1_val: display %9.3f rtable1[2,`covar_num']
		local p1_val: display %9.3f rtable1[4,`covar_num']
		

		local b2_val: display %9.3f rtable2[1,`covar_num']
		local se2_val: display %9.3f rtable2[2,`covar_num']
		local p2_val: display %9.3f rtable2[4,`covar_num']
		
		local b3_val: display %9.3f rtable3[1,`covar_num']
		local se3_val: display %9.3f rtable3[2,`covar_num']
		local p3_val: display %9.3f rtable3[4,`covar_num']
		
		local b4_val: display %9.3f rtable4[1,`covar_num']
		local se4_val: display %9.3f rtable4[2,`covar_num']
		local p4_val: display %9.3f rtable4[4,`covar_num']

		local b5_val: display %9.3f rtable5[1,`covar_num']
		local se5_val: display %9.3f rtable5[2,`covar_num']
		local p5_val: display %9.3f rtable5[4,`covar_num']
		
		
		local b6_val: display %9.3f rtable6[1,`covar_num']
		local se6_val: display %9.3f rtable6[2,`covar_num']
		local p6_val: display %9.3f rtable6[4,`covar_num']
		
		local b7_val: display %9.3f rtable7[1,`covar_num']
		local se7_val: display %9.3f rtable7[2,`covar_num']
		local p7_val: display %9.3f rtable7[4,`covar_num']
		
		local b8_val: display %9.3f rtable8[1,`covar_num']
		local se8_val: display %9.3f rtable8[2,`covar_num']
		local p8_val: display %9.3f rtable8[4,`covar_num']
		
		local b9_val: display %9.3f rtable9[1,`covar_num']
		local se9_val: display %9.3f rtable9[2,`covar_num']
		local p9_val: display %9.3f rtable9[4,`covar_num']
		
		
		local b10_val: display %9.3f rtable10[1,`covar_num']
		local se10_val: display %9.3f rtable10[2,`covar_num']
		local p10_val: display %9.3f rtable10[4,`covar_num']
		
		local b11_val: display %9.3f rtable11[1,`covar_num']
		local se11_val: display %9.3f rtable11[2,`covar_num']
		local p11_val: display %9.3f rtable11[4,`covar_num']
		
		local b12_val: display %9.3f rtable12[1,`covar_num']
		local se12_val: display %9.3f rtable12[2,`covar_num']
		local p12_val: display %9.3f rtable12[4,`covar_num']
		* Adjusted p-values
		
		local wy_pvalue1: display %9.3f (wy_results1[1, 4])
		local wy_pvalue2: display %9.3f (wy_results1[2, 4])
		local wy_pvalue3: display %9.3f (wy_results1[3, 4])
		local wy_pvalue4: display %9.3f (wy_results1[4, 4])
		local wy_pvalue5: display %9.3f (wy_results2[1, 4])
		local wy_pvalue6: display %9.3f (wy_results2[2, 4])
		local wy_pvalue7: display %9.3f (wy_results2[3, 4])
		local wy_pvalue8: display %9.3f (wy_results2[4, 4])
		local wy_pvalue9: display %9.3f (wy_results3[1, 4])
		local wy_pvalue10: display %9.3f (wy_results3[2, 4])
		local wy_pvalue11: display %9.3f (wy_results3[3, 4])
		local wy_pvalue12: display %9.3f (wy_results3[4, 4])
	


        * Add stars based on p-values
        local stars1 = ""
        if `p1_val' < 0.001 local stars1 = "***"
        else if `p1_val' < 0.01 local stars1 = "**"
        else if `p1_val' < 0.05 local stars1 = "*"

        local stars2 = ""
        if `p2_val' < 0.001 local stars2 = "***"
        else if `p2_val' < 0.01 local stars2 = "**"
        else if `p2_val' < 0.05 local stars2 = "*"

        local stars3 = ""
        if `p3_val' < 0.001 local stars3 = "***"
        else if `p3_val' < 0.01 local stars3 = "**"
        else if `p3_val' < 0.05 local stars3 = "*"

        local stars4 = ""
        if `p4_val' < 0.001 local stars4 = "***"
        else if `p4_val' < 0.01 local stars4 = "**"
        else if `p4_val' < 0.05 local stars4 = "*"
		
		local stars5 = ""
		if `p5_val' < 0.001 local stars5 = "***"
		else if `p5_val' < 0.01 local stars5 = "**"
		else if `p5_val' < 0.05 local stars5 = "*"

		local stars6 = ""
		if `p6_val' < 0.001 local stars6 = "***"
		else if `p6_val' < 0.01 local stars6 = "**"
		else if `p6_val' < 0.05 local stars6 = "*"

		local stars7 = ""
		if `p7_val' < 0.001 local stars7 = "***"
		else if `p7_val' < 0.01 local stars7 = "**"
		else if `p7_val' < 0.05 local stars7 = "*"

		local stars8 = ""
		if `p8_val' < 0.001 local stars8 = "***"
		else if `p8_val' < 0.01 local stars8 = "**"
		else if `p8_val' < 0.05 local stars8 = "*"

		local stars9 = ""
		if `p9_val' < 0.001 local stars9 = "***"
		else if `p9_val' < 0.01 local stars9 = "**"
		else if `p9_val' < 0.05 local stars9 = "*"

		local stars10 = ""
		if `p10_val' < 0.001 local stars10 = "***"
		else if `p10_val' < 0.01 local stars10 = "**"
		else if `p10_val' < 0.05 local stars10 = "*"

		local stars11 = ""
		if `p11_val' < 0.001 local stars11 = "***"
		else if `p11_val' < 0.01 local stars11 = "**"
		else if `p11_val' < 0.05 local stars11 = "*"

		local stars12 = ""
		if `p12_val' < 0.001 local stars12 = "***"
		else if `p12_val' < 0.01 local stars12 = "**"
		else if `p12_val' < 0.05 local stars12 = "*"


        * Adjusted p-values for HT and HT#var variable
		if "`covar'" == "HT"  {
			local stars_adj1 = ""
			if `wy_pvalue1' < 0.001 {
				local stars_adj1 = "+++"
			}
			else if `wy_pvalue1' < 0.01 {
				local stars_adj1 = "++"
			}
			else if `wy_pvalue1' < 0.05 {
				local stars_adj1 = "+"
			}

			local stars_adj2 = ""
			if `wy_pvalue2' < 0.001 {
				local stars_adj2 = "+++"
			}
			else if `wy_pvalue2' < 0.01 {
				local stars_adj2 = "++"
			}
			else if `wy_pvalue2' < 0.05 {
				local stars_adj2 = "+"
			}

			local stars_adj3 = ""
			if `wy_pvalue3' < 0.001 {
				local stars_adj3 = "+++"
			}
			else if `wy_pvalue3' < 0.01 {
				local stars_adj3 = "++"
			}
			else if `wy_pvalue3' < 0.05 {
				local stars_adj3 = "+"
			}

			local stars_adj4 = ""
			if `wy_pvalue4' < 0.001 {
				local stars_adj4 = "+++"
			}
			else if `wy_pvalue4' < 0.01 {
				local stars_adj4 = "++"
			}
			else if `wy_pvalue4' < 0.05 {
				local stars_adj4 = "+"
			}

			local stars_adj5 = ""
			if `wy_pvalue5' < 0.001 {
				local stars_adj5 = "+++"
			}
			else if `wy_pvalue5' < 0.01 {
				local stars_adj5 = "++"
			}
			else if `wy_pvalue5' < 0.05 {
				local stars_adj5 = "+"
			}

			local stars_adj6 = ""
			if `wy_pvalue6' < 0.001 {
				local stars_adj6 = "+++"
			}
			else if `wy_pvalue6' < 0.01 {
				local stars_adj6 = "++"
			}
			else if `wy_pvalue6' < 0.05 {
				local stars_adj6 = "+"
			}

			local stars_adj7 = ""
			if `wy_pvalue7' < 0.001 {
				local stars_adj7 = "+++"
			}
			else if `wy_pvalue7' < 0.01 {
				local stars_adj7 = "++"
			}
			else if `wy_pvalue7' < 0.05 {
				local stars_adj7 = "+"
			}

			local stars_adj8 = ""
			if `wy_pvalue8' < 0.001 {
				local stars_adj8 = "+++"
			}
			else if `wy_pvalue8' < 0.01 {
				local stars_adj8 = "++"
			}
			else if `wy_pvalue8' < 0.05 {
				local stars_adj8 = "+"
			}

			local stars_adj9 = ""
			if `wy_pvalue9' < 0.001 {
				local stars_adj9 = "+++"
			}
			else if `wy_pvalue9' < 0.01 {
				local stars_adj9 = "++"
			}
			else if `wy_pvalue9' < 0.05 {
				local stars_adj9 = "+"
			}

			local stars_adj10 = ""
			if `wy_pvalue10' < 0.001 {
				local stars_adj10 = "+++"
			}
			else if `wy_pvalue10' < 0.01 {
				local stars_adj10 = "++"
			}
			else if `wy_pvalue10' < 0.05 {
				local stars_adj10 = "+"
			}

			local stars_adj11 = ""
			if `wy_pvalue11' < 0.001 {
				local stars_adj11 = "+++"
			}
			else if `wy_pvalue11' < 0.01 {
				local stars_adj11 = "++"
			}
			else if `wy_pvalue11' < 0.05 {
				local stars_adj11 = "+"
			}

			local stars_adj12 = ""
			if `wy_pvalue12' < 0.001 {
				local stars_adj12 = "+++"
			}
			else if `wy_pvalue12' < 0.01 {
				local stars_adj12 = "++"
			}
			else if `wy_pvalue12' < 0.05 {
				local stars_adj12 = "+"
			}
		}
	


	
			if "`covar'" == "HT" {
		* Insert coefficients, standard errors, and stars for each model
		putdocx table mytable(`row',2) = ("`=strltrim("`b1_val'")'`stars1'`stars_adj1'"), halign(left) linebreak
		putdocx table mytable(`row',2) = ("(`=strltrim("`se1_val'")')"), append linebreak
		putdocx table mytable(`row',3) = ("`=strltrim("`b2_val'")'`stars2'`stars_adj2'"), halign(left) linebreak
		putdocx table mytable(`row',3) = ("(`=strltrim("`se2_val'")')"), append linebreak
		putdocx table mytable(`row',4) = ("`=strltrim("`b3_val'")'`stars3'`stars_adj3'"), halign(left) linebreak
		putdocx table mytable(`row',4) = ("(`=strltrim("`se3_val'")')"), append linebreak
		putdocx table mytable(`row',5) = ("`=strltrim("`b4_val'")'`stars4'`stars_adj4'"), halign(left) linebreak
		putdocx table mytable(`row',5) = ("(`=strltrim("`se4_val'")')"), append linebreak
		putdocx table mytable(`row',6) = ("`=strltrim("`b5_val'")'`stars5'`stars_adj5'"), halign(left) linebreak
		putdocx table mytable(`row',6) = ("(`=strltrim("`se5_val'")')"), append linebreak
		putdocx table mytable(`row',7) = ("`=strltrim("`b6_val'")'`stars6'`stars_adj6'"), halign(left) linebreak
		putdocx table mytable(`row',7) = ("(`=strltrim("`se6_val'")')"), append linebreak
		putdocx table mytable(`row',8) = ("`=strltrim("`b7_val'")'`stars7'`stars_adj7'"), halign(left) linebreak
		putdocx table mytable(`row',8) = ("(`=strltrim("`se7_val'")')"), append linebreak
		putdocx table mytable(`row',9) = ("`=strltrim("`b8_val'")'`stars8'`stars_adj8'"), halign(left) linebreak
		putdocx table mytable(`row',9) = ("(`=strltrim("`se8_val'")')"), append linebreak
		putdocx table mytable(`row',10) = ("`=strltrim("`b9_val'")'`stars9'`stars_adj9'"), halign(left) linebreak
		putdocx table mytable(`row',10) = ("(`=strltrim("`se9_val'")')"), append linebreak
		putdocx table mytable(`row',11) = ("`=strltrim("`b10_val'")'`stars10'`stars_adj10'"), halign(left) linebreak
		putdocx table mytable(`row',11) = ("(`=strltrim("`se10_val'")')"), append linebreak
		putdocx table mytable(`row',12) = ("`=strltrim("`b11_val'")'`stars11'`stars_adj11'"), halign(left) linebreak
		putdocx table mytable(`row',12) = ("(`=strltrim("`se11_val'")')"), append linebreak
		putdocx table mytable(`row',13) = ("`=strltrim("`b12_val'")'`stars12'`stars_adj12'"), halign(left) linebreak
		putdocx table mytable(`row',13) = ("(`=strltrim("`se12_val'")')"), append linebreak
	
	}
	else {
		putdocx table mytable(`row',2) = ("`=strltrim("`b1_val'")'`stars1'`stars_adj1'"), halign(left) linebreak
		putdocx table mytable(`row',2) = ("(`=strltrim("`se1_val'")')"), append
		putdocx table mytable(`row',3) = ("`=strltrim("`b2_val'")'`stars2'`stars_adj2'"), halign(left) linebreak
		putdocx table mytable(`row',3) = ("(`=strltrim("`se2_val'")')"), append
		putdocx table mytable(`row',4) = ("`=strltrim("`b3_val'")'`stars3'`stars_adj3'"), halign(left) linebreak
		putdocx table mytable(`row',4) = ("(`=strltrim("`se3_val'")')"), append 
		putdocx table mytable(`row',5) = ("`=strltrim("`b4_val'")'`stars4'`stars_adj4'"), halign(left) linebreak
		putdocx table mytable(`row',5) = ("(`=strltrim("`se4_val'")')"), append 
		putdocx table mytable(`row',6) = ("`=strltrim("`b5_val'")'`stars5'`stars_adj5'"), halign(left) linebreak
		putdocx table mytable(`row',6) = ("(`=strltrim("`se5_val'")')"), append 
		putdocx table mytable(`row',7) = ("`=strltrim("`b6_val'")'`stars6'`stars_adj6'"), halign(left) linebreak
		putdocx table mytable(`row',7) = ("(`=strltrim("`se6_val'")')"), append 
		putdocx table mytable(`row',8) = ("`=strltrim("`b7_val'")'`stars7'`stars_adj7'"), halign(left) linebreak
		putdocx table mytable(`row',8) = ("(`=strltrim("`se7_val'")')"), append 
		putdocx table mytable(`row',9) = ("`=strltrim("`b8_val'")'`stars8'`stars_adj8'"), halign(left) linebreak
		putdocx table mytable(`row',9) = ("(`=strltrim("`se8_val'")')"), append 
		putdocx table mytable(`row',10) = ("`=strltrim("`b9_val'")'`stars9'`stars_adj9'"), halign(left) linebreak
		putdocx table mytable(`row',10) = ("(`=strltrim("`se9_val'")')"), append 
		putdocx table mytable(`row',11) = ("`=strltrim("`b10_val'")'`stars10'`stars_adj10'"), halign(left) linebreak
		putdocx table mytable(`row',11) = ("(`=strltrim("`se10_val'")')"), append 
		putdocx table mytable(`row',12) = ("`=strltrim("`b11_val'")'`stars11'`stars_adj11'"), halign(left) linebreak
		putdocx table mytable(`row',12) = ("(`=strltrim("`se11_val'")')"), append 
		putdocx table mytable(`row',13) = ("`=strltrim("`b12_val'")'`stars12'`stars_adj12'"), halign(left) linebreak
		putdocx table mytable(`row',13) = ("(`=strltrim("`se12_val'")')"), append 
	}

	

        * Increment row
        local row = `row' + 1
        
		
	local stars_adj1 ""
	local stars_adj2 ""
	local stars_adj3 ""
	local stars_adj4 ""
	local stars_adj5 ""
	local stars_adj6 ""
	local stars_adj7 ""
	local stars_adj8 ""
	local stars_adj9 ""
	local stars_adj10 ""
	local stars_adj11 ""
	local stars_adj12 ""


	}




local wy_pvalue1: display %9.3f (wy_results1[1, 4])
local wy_pvalue2: display %9.3f (wy_results1[2, 4])
local wy_pvalue3: display %9.3f (wy_results1[3, 4])
local wy_pvalue4: display %9.3f (wy_results1[4, 4])
local wy_pvalue5: display %9.3f (wy_results2[1, 4])
local wy_pvalue6: display %9.3f (wy_results2[2, 4])
local wy_pvalue7: display %9.3f (wy_results2[3, 4])
local wy_pvalue8: display %9.3f (wy_results2[4, 4])
local wy_pvalue9: display %9.3f (wy_results3[1, 4])
local wy_pvalue10: display %9.3f (wy_results3[2, 4])
local wy_pvalue11: display %9.3f (wy_results3[3, 4])
local wy_pvalue12: display %9.3f (wy_results3[4, 4])

putdocx table mytable(3, 2) = ("[`=strltrim("`wy_pvalue1'")']"), append
putdocx table mytable(3, 3) = ("[`=strltrim("`wy_pvalue2'")']"), append
putdocx table mytable(3, 4) = ("[`=strltrim("`wy_pvalue3'")']"), append
putdocx table mytable(3, 5) = ("[`=strltrim("`wy_pvalue4'")']"), append
putdocx table mytable(3, 6) = ("[`=strltrim("`wy_pvalue5'")']"), append
putdocx table mytable(3, 7) = ("[`=strltrim("`wy_pvalue6'")']"), append
putdocx table mytable(3, 8) = ("[`=strltrim("`wy_pvalue7'")']"), append
putdocx table mytable(3, 9) = ("[`=strltrim("`wy_pvalue8'")']"), append
putdocx table mytable(3, 10) = ("[`=strltrim("`wy_pvalue9'")']"), append
putdocx table mytable(3, 11) = ("[`=strltrim("`wy_pvalue10'")']"), append
putdocx table mytable(3, 12) = ("[`=strltrim("`wy_pvalue11'")']"), append
putdocx table mytable(3, 13) = ("[`=strltrim("`wy_pvalue12'")']"), append



*adding observations
putdocx table mytable(`row',1) = ("N")
putdocx table mytable(`row',2) = (nobs1)
putdocx table mytable(`row',3) = (nobs2)
putdocx table mytable(`row',4) = (nobs3)
putdocx table mytable(`row',5) = (nobs4)
putdocx table mytable(`row',6) = (nobs5)
putdocx table mytable(`row',7) = (nobs6)
putdocx table mytable(`row',8) = (nobs7)
putdocx table mytable(`row',9) = (nobs8)
putdocx table mytable(`row',10) = (nobs9)
putdocx table mytable(`row',11) = (nobs10)
putdocx table mytable(`row',12) = (nobs11)
putdocx table mytable(`row',13) = (nobs12)

local row = `row' + 1
local row2 = `row' + 1

*Adding R² and adjusted R²
	
putdocx table mytable(`row',1) = ("R²")
putdocx table mytable(`row2',1) = ("R²adj")

	forvalues mod = 1/12 {		
		local col_num = `mod' + 1
		
		putdocx table mytable(`row',`col_num') = ("`=strltrim("`r2_`mod''")'")
		putdocx table mytable(`row2',`col_num') =("`=strltrim("`r2_adj_`mod''")'")
	}
	
	local row = `row' + 1

	* Adding  horizontal lines
	forvalues col_num = 1/13 {
		local row_2=`row'-2
		putdocx table mytable(1,`col_num'), border(top, "single")
		putdocx table mytable(2,`col_num'), border(bottom, "single")
		putdocx table mytable(`row_2',`col_num'), border(top, "single")
		putdocx table mytable(`row',`col_num'), border(bottom, "single")
	}
	 forvalues col_num = 2/13 {
		putdocx table mytable(1,`col_num'), border(bottom, "single")
	}



	local row = `row' + 1
	local table_num  = `table_num' + 1
	
putdocx table mytable(`row',1) = ("Note: Robust standard errors in parentheses."), colspan(13) linebreak 
putdocx table mytable(`row',1) = ("For health-risk information, Westfall-Young MHT adjusted p-values in brackets with 10,000 bootstrap replications. "),  append linebreak 
putdocx table mytable(`row',1) = ("***p<0.001, **p<0.01, *p<0.05 for standard p-values, +++p< 0.001, ++p < 0.01, +p < 0.05 for Westfall-Young MHT adjusted p-values."),  append

restore
	putdocx pagebreak
}

}

* ========================
**# Table SC36: Robustness test (basic) 6: social desireability
* ========================


qui {
	
qui regress meat_tax_dummy HT soc_des_high FR_dummy LV_dummy, vce(hc3)
matrix rtable1=r(table)
scalar nobs1 = e(N)
local r2_1 = string(e(r2), "%9.3f")
local r2_adj_1 = string(e(r2_a), "%9.3f")

qui regress meat_tax_s HT soc_des_high FR_dummy LV_dummy, vce(hc3)
matrix rtable2=r(table)
scalar nobs2 = e(N)
local r2_2 = string(e(r2), "%9.3f")
local r2_adj_2 = string(e(r2_a), "%9.3f")

qui regress meat_day_dummy HT soc_des_high FR_dummy LV_dummy, vce(hc3)
matrix rtable3=r(table)
scalar nobs3 = e(N)
local r2_3 = string(e(r2), "%9.3f")
local r2_adj_3 = string(e(r2_a), "%9.3f")

qui regress meat_day_s HT soc_des_high FR_dummy LV_dummy, vce(hc3)
matrix rtable4=r(table)
scalar nobs4 = e(N)
local r2_4 = string(e(r2), "%9.3f")
local r2_adj_4 = string(e(r2_a), "%9.3f")


local file_path = "robustness_main/r11_soc_des.xlsx"
    
if !fileexists("`file_path'") {
*Westfall-Young adjusted p-values
wyoung, cmd("regress meat_tax_dummy HT soc_des_high FR_dummy LV_dummy, vce(hc3)" ///
"regress meat_tax_s HT soc_des_high FR_dummy LV_dummy, vce(hc3)" ///
"regress meat_day_dummy HT soc_des_high FR_dummy LV_dummy, vce(hc3)" ///
"regress meat_day_s HT soc_des_high FR_dummy LV_dummy, vce(hc3)") ///
familyp("HT" "HT" "HT" "HT") familypexp strata(country) bootstraps(10000) seed(1234)
    
matrix wy_results = r(table)
putexcel set `file_path', replace
putexcel A1=matrix(wy_results), names
}
	
preserve

import excel `file_path', sheet("Sheet1") firstrow clear

mkmat coef stderr  p pwyoung pbonf psidak, matrix(wy_results)

restore

	
*Add title
putdocx paragraph
putdocx text ("Table SC`table_num' LPM results of the basic models, controlling for social desirability  - pooled sample."), font("", 11)	

*Create a table with 5 rows and 5 columns (1 for covariate names, 4 for models)
putdocx table mytable = (10, 5), border(all, nil) width(4) width(100%) 

*headers for each model
putdocx table mytable(1,1) = ("")
putdocx table mytable(1,2) = ("Meat tax acceptability for oneself")
putdocx table mytable(1,3) = ("Meat Tax acceptability for society")
putdocx table mytable(1,4) = ("Meat-free days acceptability for oneself")
putdocx table mytable(1,5) = ("Meat-free days acceptability for society")



local covariates HT soc_des_high FR_dummy LV_dummy _cons
local nvars : word count `covariates'
local row 2  // Start from row 2 since row 1 has headers
local rowse 3

forvalues covar_num = 1/`nvars' {
	local covar : word `covar_num' of `covariates'
	if "`covar'" == "_cons" {
		local covar_label Constant
		putdocx table mytable(`row',1) = ("`covar_label'")
	}
		else {
			 local covar_label : variable label `covar'
			 putdocx table mytable(`row',1) = ("`covar_label'"), italic
			 }
		
	

	
    * Extract coefficients and standard errors
    local b1_val: display %9.3f rtable1[1,`covar_num']
    local se1_val: display %9.3f rtable1[2,`covar_num']
    local p1_val: display %9.3f rtable1[4,`covar_num']
	
    
    local b2_val: display %9.3f rtable2[1,`covar_num']
    local se2_val: display %9.3f rtable2[2,`covar_num']
    local p2_val: display %9.3f rtable2[4,`covar_num']
    
    local b3_val: display %9.3f rtable3[1,`covar_num']
    local se3_val: display %9.3f rtable3[2,`covar_num']
    local p3_val: display %9.3f rtable3[4,`covar_num']
    
    local b4_val: display %9.3f rtable4[1,`covar_num']
    local se4_val: display %9.3f rtable4[2,`covar_num']
    local p4_val: display %9.3f rtable4[4,`covar_num']

    * Adjusted p-values
	
	local wy_pvalue1: display %9.3f (wy_results[1, 4])
	local wy_pvalue2: display %9.3f (wy_results[2, 4])
	local wy_pvalue3: display %9.3f (wy_results[3, 4])
	local wy_pvalue4: display %9.3f (wy_results[4, 4])

	
	
    * Add stars based on p-values
    local stars1 = ""
    if `p1_val' < 0.001 {
        local stars1 = "***"
    }
    else if `p1_val' < 0.01 {
        local stars1 = "**"
    }
    else if `p1_val' < 0.05 {
        local stars1 = "*"
    }

    local stars2 = ""
    if `p2_val' < 0.001 {
        local stars2 = "***"
    }
    else if `p2_val' < 0.01 {
        local stars2 = "**"
    }
    else if `p2_val' < 0.05 {
        local stars2 = "*"
    }

    local stars3 = ""
    if `p3_val' < 0.001 {
        local stars3 = "***"
    }
    else if `p3_val' < 0.01 {
        local stars3 = "**"
    }
    else if `p3_val' < 0.05 {
        local stars3 = "*"
    }

    local stars4 = ""
    if `p4_val' < 0.001 {
        local stars4 = "***"
    }
    else if `p4_val' < 0.01 {
        local stars4 = "**"
    }
    else if `p4_val' < 0.05 {
        local stars4 = "*"
    }
	

	
	* Add stars_adj based on adjusted p-values
	if "`covar'" == "HT" {
		local stars_adj1 = ""
		if `wy_pvalue1' < 0.001 {
			local stars_adj1 = "+++"
		}
		else if `wy_pvalue1' < 0.01 {
			local stars_adj1 = "++"
		}
		else if `wy_pvalue1' < 0.05 {
			local stars_adj1 = "+"
		}

			local stars_adj2 = ""
		if `wy_pvalue2' < 0.001 {
			local stars_adj2 = "+++"
		}
		else if `wy_pvalue2' < 0.01 {
			local stars_adj2 = "++"
		}
		else if `wy_pvalue2' < 0.05 {
			local stars_adj2 = "+"
		}

			local stars_adj3 = ""
		if `wy_pvalue3' < 0.001 {
			local stars_adj3 = "+++"
		}
		else if `wy_pvalue3' < 0.01 {
			local stars_adj3 = "++"
		}
		else if `wy_pvalue3' < 0.05 {
			local stars_adj3 = "+"
		}

			local stars_adj4 = ""
		if `wy_pvalue4' < 0.001 {
			local stars_adj4 = "+++"
		}
		else if `wy_pvalue4' < 0.01 {
			local stars_adj4 = "++"
		}
		else if `wy_pvalue4' < 0.05 {
			local stars_adj4 = "+"
		}
	} 
	



		if "`covar'" == "HT" {
		* Insert coefficients, standard errors, and stars for each model
		putdocx table mytable(`row',2) = ("`=strltrim("`b1_val'")'`stars1'`stars_adj1'"), halign(left) linebreak
		putdocx table mytable(`row',2) = ("(`=strltrim("`se1_val'")')"), append linebreak
		putdocx table mytable(`row',3) = ("`=strltrim("`b2_val'")'`stars2'`stars_adj2'"), halign(left) linebreak
		putdocx table mytable(`row',3) = ("(`=strltrim("`se2_val'")')"), append linebreak
		putdocx table mytable(`row',4) = ("`=strltrim("`b3_val'")'`stars3'`stars_adj3'"), halign(left) linebreak
		putdocx table mytable(`row',4) = ("(`=strltrim("`se3_val'")')"), append linebreak
		putdocx table mytable(`row',5) = ("`=strltrim("`b4_val'")'`stars4'`stars_adj4'"), halign(left) linebreak
		putdocx table mytable(`row',5) = ("(`=strltrim("`se4_val'")')"), append linebreak
	
	}
	else {
		* Insert coefficients, standard errors, and stars for each model
		putdocx table mytable(`row',2) = ("`=strltrim("`b1_val'")'`stars1'`stars_adj1'"), halign(left) linebreak
		putdocx table mytable(`row',2) = ("(`=strltrim("`se1_val'")')"), append
		putdocx table mytable(`row',3) = ("`=strltrim("`b2_val'")'`stars2'`stars_adj2'"), halign(left) linebreak
		putdocx table mytable(`row',3) = ("(`=strltrim("`se2_val'")')"), append
		putdocx table mytable(`row',4) = ("`=strltrim("`b3_val'")'`stars3'`stars_adj3'"), halign(left) linebreak
		putdocx table mytable(`row',4) = ("(`=strltrim("`se3_val'")')"), append
		putdocx table mytable(`row',5) = ("`=strltrim("`b4_val'")'`stars4'`stars_adj4'"), halign(left) linebreak
		putdocx table mytable(`row',5) = ("(`=strltrim("`se4_val'")')"), append
	
	}
    
	
    
    * Increment the row counter for the next covariate
    local row = `row' + 1
	
	
	* Resetting stars to null
	local stars_adj1 = ""
	local stars_adj2 = ""
	local stars_adj3 = ""
	local stars_adj4 = ""
}



local wy_pvalue1: display %9.3f (wy_results[1, 4])
local wy_pvalue2: display %9.3f (wy_results[2, 4])
local wy_pvalue3: display %9.3f (wy_results[3, 4])
local wy_pvalue4: display %9.3f (wy_results[4, 4])



putdocx table mytable(2, 2) = ("[`=strltrim("`wy_pvalue1'")']"), append
putdocx table mytable(2, 3) = ("[`=strltrim("`wy_pvalue2'")']"), append
putdocx table mytable(2, 4) = ("[`=strltrim("`wy_pvalue3'")']"), append
putdocx table mytable(2, 5) = ("[`=strltrim("`wy_pvalue4'")']"), append

*adding observations
putdocx table mytable(`row',1) = ("N")
putdocx table mytable(`row',2) = (nobs1)
putdocx table mytable(`row',3) = (nobs2)
putdocx table mytable(`row',4) = (nobs3)
putdocx table mytable(`row',5) = (nobs4)




local row = `row' + 1
local row2 = `row' + 1

*Adding R² and adjusted R²
	
putdocx table mytable(`row',1) = ("R²")
putdocx table mytable(`row2',1) = ("R²adj")

	forvalues mod = 1/4 {		
		local col_num = `mod' + 1
		
		putdocx table mytable(`row',`col_num') = ("`=strltrim("`r2_`mod''")'")
		putdocx table mytable(`row2',`col_num') =("`=strltrim("`r2_adj_`mod''")'")
	}
	
	local row = `row' + 1

	* Adding  horizontal lines
	forvalues col_num = 1/5 {
		local row_2=`row'-2
		putdocx table mytable(1,`col_num'), border(top, "single")
		putdocx table mytable(1,`col_num'), border(bottom, "single")
		putdocx table mytable(`row_2',`col_num'), border(top, "single")
		putdocx table mytable(`row',`col_num'), border(bottom, "single")
	}


local row = `row' + 1

putdocx table mytable(`row',1) = ("Note: Robust standard errors in parentheses."), colspan(5) linebreak 
putdocx table mytable(`row',1) = ("For health-risk information, Westfall-Young MHT adjusted p-values in brackets with 10,000 bootstrap replications. "),  append linebreak 
putdocx table mytable(`row',1) = ("***p<0.001, **p<0.01, *p<0.05 for standard p-values, +++p< 0.001, ++p < 0.01, +p < 0.05 for Westfall-Young MHT adjusted p-values."),  append

putdocx pagebreak
}


local table_num  = `table_num' + 1



* ========================
**# Tables S37 to S42: Robustness test (heterogeneity) 6: social desireability
* ========================

qui {
foreach i in 1 2 3 4 5 6 {
    if `i'== 1 {
        local var1 food_depriv
        local var2 health_low_rec
        local var3 unemployed 
    }
    else if `i'== 2 {
        local var1 educ_low
        local var2 income_Q1
        local var3 age_low
    }
    else if `i'== 3 {
        local var1 female
        local var2 vegan_veg_pesc_dummy
        local var3 high_meat_dummy
    }               
    else if `i'== 4 {
        local var1 canteen_dummy
        local var2 po1_dummy
        local var3 po2_dummy
    } 
    else if `i'== 5 {
        local var1 po3_dummy
        local var2 po4_dummy
        local var3 po5_dummy
    }
    else if `i'== 6 {
        local var1 diet_knowl_high 
        local var2 cc_acc_high 
        local var3 soc_norm_high

    }
	
	local var1 = "`var1'"
	local var2 = "`var2'"
	local var3 = "`var3'"
	

	* coeff and N for var1
    qui regress meat_tax_dummy 1.HT `var1' 1.HT#1.`var1' soc_des_high FR_dummy LV_dummy, vce(hc3)
    matrix rtable1 = r(table)
    scalar nobs1 = e(N)
	local r2_1 = string(e(r2), "%9.3f")
	local r2_adj_1 = string(e(r2_a), "%9.3f")
    qui regress meat_tax_s 1.HT `var1' 1.HT#1.`var1' soc_des_high FR_dummy LV_dummy, vce(hc3)
    matrix rtable2 = r(table)
    scalar nobs2 = e(N)
	local r2_2 = string(e(r2), "%9.3f")
	local r2_adj_2 = string(e(r2_a), "%9.3f")
    qui regress meat_day_dummy 1.HT `var1' 1.HT#1.`var1' soc_des_high FR_dummy LV_dummy, vce(hc3)
    matrix rtable3 = r(table)
    scalar nobs3 = e(N)
	local r2_3 = string(e(r2), "%9.3f")
	local r2_adj_3 = string(e(r2_a), "%9.3f")
    qui regress meat_day_s 1.HT `var1' 1.HT#1.`var1' soc_des_high FR_dummy LV_dummy, vce(hc3)
    matrix rtable4 = r(table)
    scalar nobs4 = e(N)
	local r2_4 = string(e(r2), "%9.3f")
	local r2_adj_4 = string(e(r2_a), "%9.3f")
	* coeff and N for var2
	qui regress meat_tax_dummy 1.HT `var2' 1.HT#1.`var2' soc_des_high FR_dummy LV_dummy, vce(hc3)
    matrix rtable5 = r(table)
    scalar nobs5 = e(N)
	local r2_5 = string(e(r2), "%9.3f")
	local r2_adj_5 = string(e(r2_a), "%9.3f")
    qui regress meat_tax_s 1.HT `var2' 1.HT#1.`var2' soc_des_high FR_dummy LV_dummy, vce(hc3)
    matrix rtable6 = r(table)
    scalar nobs6 = e(N)
	local r2_6 = string(e(r2), "%9.3f")
	local r2_adj_6 = string(e(r2_a), "%9.3f")
    qui regress meat_day_dummy 1.HT `var2' 1.HT#1.`var2' soc_des_high FR_dummy LV_dummy, vce(hc3)
    matrix rtable7 = r(table)
    scalar nobs7 = e(N)
	local r2_7 = string(e(r2), "%9.3f")
	local r2_adj_7 = string(e(r2_a), "%9.3f")
    qui regress meat_day_s 1.HT `var2' 1.HT#1.`var2' soc_des_high FR_dummy LV_dummy, vce(hc3)
    matrix rtable8 = r(table)
    scalar nobs8 = e(N)
	local r2_8 = string(e(r2), "%9.3f")
	local r2_adj_8 = string(e(r2_a), "%9.3f")
	* coeff and N for var3	
	qui regress meat_tax_dummy 1.HT `var3' 1.HT#1.`var3' soc_des_high FR_dummy LV_dummy, vce(hc3)
    matrix rtable9 = r(table)
    scalar nobs9 = e(N)
	local r2_9 = string(e(r2), "%9.3f")
	local r2_adj_9 = string(e(r2_a), "%9.3f")
    qui regress meat_tax_s 1.HT `var3' 1.HT#1.`var3' soc_des_high FR_dummy LV_dummy, vce(hc3)
    matrix rtable10 = r(table)
    scalar nobs10 = e(N)
	local r2_10 = string(e(r2), "%9.3f")
	local r2_adj_10 = string(e(r2_a), "%9.3f")
    qui regress meat_day_dummy 1.HT `var3' 1.HT#1.`var3' soc_des_high FR_dummy LV_dummy, vce(hc3)
    matrix rtable11 = r(table)
    scalar nobs11 = e(N)
	local r2_11 = string(e(r2), "%9.3f")
	local r2_adj_11 = string(e(r2_a), "%9.3f")
    qui regress meat_day_s 1.HT `var3' 1.HT#1.`var3' soc_des_high FR_dummy LV_dummy, vce(hc3)
    matrix rtable12 = r(table)
    scalar nobs12 = e(N)
	local r2_12 = string(e(r2), "%9.3f")
	local r2_adj_12 = string(e(r2_a), "%9.3f")

	local nb 10000 //number of bootsraps

    local file_path = "robustness_heterog/r11_soc_des_`var1'.xlsx"
    
    // Check if the file exists
    if !fileexists("`file_path'") {
    * Westfall-Young adjusted p-values for variable 1
    qui wyoung, cmd("regress meat_tax_dummy 1.HT `var1' 1.HT#1.`var1' soc_des_high FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_tax_s 1.HT `var1' 1.HT#1.`var1' soc_des_high FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_day_dummy 1.HT `var1' 1.HT#1.`var1' soc_des_high FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_day_s 1.HT `var1' 1.HT#1.`var1' soc_des_high FR_dummy LV_dummy, vce(hc3)"  ///
		"regress meat_tax_dummy 1.HT `var1' 1.HT#1.`var1' soc_des_high FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_tax_s 1.HT `var1' 1.HT#1.`var1' soc_des_high FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_day_dummy 1.HT `var1' 1.HT#1.`var1' soc_des_high FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_day_s 1.HT `var1' 1.HT#1.`var1' soc_des_high FR_dummy LV_dummy, vce(hc3)" ) ///
        familyp(1.HT 1.HT 1.HT 1.HT 1.HT+1.HT#1.`var1' 1.HT+1.HT#1.`var1' 1.HT+1.HT#1.`var1' 1.HT+1.HT#1.`var1') ///
        familypexp strata(country) ///
        bootstraps(`nb') seed(1234)
    
	matrix wy_results1 = r(table)
	putexcel set `file_path', replace
    putexcel A1=matrix(wy_results1), names
	
	}
		
	preserve
	
	import excel `file_path', sheet("Sheet1") firstrow clear
	mkmat coef stderr  p pwyoung pbonf psidak, matrix(wy_results1)
	

	restore
		
		
    local file_path = "robustness_heterog/r11_soc_des_`var2'.xlsx"
	    * Westfall-Young adjusted p-values for variable 2
        if !fileexists("`file_path'") {
			qui wyoung, cmd("regress meat_tax_dummy 1.HT `var2' 1.HT#1.`var2' soc_des_high FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_tax_s 1.HT `var2' 1.HT#1.`var2' soc_des_high FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_day_dummy 1.HT `var2' 1.HT#1.`var2' soc_des_high FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_day_s 1.HT `var2' 1.HT#1.`var2' soc_des_high FR_dummy LV_dummy, vce(hc3)"  ///
		"regress meat_tax_dummy 1.HT `var2' 1.HT#1.`var2' soc_des_high FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_tax_s 1.HT `var2' 1.HT#1.`var2' soc_des_high FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_day_dummy 1.HT `var2' 1.HT#1.`var2' soc_des_high FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_day_s 1.HT `var2' 1.HT#1.`var2' soc_des_high FR_dummy LV_dummy, vce(hc3)" ) ///
        familyp(1.HT 1.HT 1.HT 1.HT 1.HT+1.HT#1.`var2' 1.HT+1.HT#1.`var2' 1.HT+1.HT#1.`var2' 1.HT+1.HT#1.`var2') ///
        familypexp strata(country) ///
        bootstraps(`nb') seed(1234)
    
	matrix wy_results2 = r(table)	
	putexcel set `file_path', replace
    putexcel A1=matrix(wy_results2), names
		}
	
	preserve
	
	import excel `file_path', sheet("Sheet1") firstrow clear
	mkmat coef stderr  p pwyoung pbonf psidak, matrix(wy_results2)


	restore
	
    local file_path = "robustness_heterog/r11_soc_des_`var3'.xlsx"
		* Westfall-Young adjusted p-values for variable 3
    if !fileexists("`file_path'") {
		qui wyoung, cmd("regress meat_tax_dummy 1.HT `var3' 1.HT#1.`var3' soc_des_high FR_dummy LV_dummy,vce(hc3)" ///
        "regress meat_tax_s 1.HT `var3' 1.HT#1.`var3' soc_des_high FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_day_dummy 1.HT `var3' 1.HT#1.`var3' soc_des_high FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_day_s 1.HT `var3' 1.HT#1.`var3' soc_des_high FR_dummy LV_dummy, vce(hc3)"  ///
		"regress meat_tax_dummy 1.HT `var3' 1.HT#1.`var3' soc_des_high FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_tax_s 1.HT `var3' 1.HT#1.`var3' soc_des_high FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_day_dummy 1.HT `var3' 1.HT#1.`var3' soc_des_high FR_dummy LV_dummy, vce(hc3)" ///
        "regress meat_day_s 1.HT `var3' 1.HT#1.`var3' soc_des_high FR_dummy LV_dummy, vce(hc3)" ) ///
        familyp(1.HT 1.HT 1.HT 1.HT 1.HT+1.HT#1.`var3' 1.HT+1.HT#1.`var3' 1.HT+1.HT#1.`var3' 1.HT+1.HT#1.`var3') ///
        familypexp strata(country) ///
        bootstraps(`nb') seed(1234)
    
	matrix wy_results3 = r(table)
	putexcel set `file_path', replace
    putexcel A1=matrix(wy_results3), names
	}
		
	preserve
	
	import excel `file_path', sheet("Sheet1") firstrow clear
	mkmat coef stderr  p pwyoung pbonf psidak, matrix(wy_results3)

	restore
			
		
	local lbl1 : variable label `var1'
	local lbl2 : variable label `var2'
	local lbl3 : variable label `var3'	
	
    * Add title
    putdocx paragraph
    putdocx text ("Table SC`table_num' LPM results of the heterogeneity models for `lbl1', `lbl2', and `lbl3', controlling for social desirability - pooled sample."), font("", 11)	
    putdocx table mytable = (13, 13), border(all, nil) width(4) width(100%) 
	


    * Create a table with 5 rows and 5 columns (1 for covariate names, 4 for models)
	putdocx table mytable(1,2) = ("`lbl1'"), colspan(4) italic
	putdocx table mytable(1,3) = ("`lbl2'"), colspan(4) italic
	putdocx table mytable(1,4) = ("`lbl3'"), colspan(4) italic
    * Headers for each model
    putdocx table mytable(2,2) = ("Meat tax acceptability for oneself")
    putdocx table mytable(2,3) = ("Meat tax acceptability for society")
    putdocx table mytable(2,4) = ("Meat-free days acceptability for oneself")
    putdocx table mytable(2,5) = ("Meat-free days acceptability for society")
    putdocx table mytable(2,6) = ("Meat tax acceptability for oneself")
    putdocx table mytable(2,7) = ("Meat tax acceptability for society")
    putdocx table mytable(2,8) = ("Meat-free days acceptability for oneself")
    putdocx table mytable(2,9) = ("Meat-free days acceptability for society")
    putdocx table mytable(2,10) = ("Meat tax acceptability for oneself")
    putdocx table mytable(2,11) = ("Meat tax acceptability for society")
    putdocx table mytable(2,12) = ("Meat-free days acceptability for oneself")
    putdocx table mytable(2,13) = ("Meat-free days acceptability for society")


	
    local covariates HT "`var1'" "1.HT#1.`var1'" soc_des_high FR_dummy LV_dummy _cons
	local interaction_label "Health-risk information # heterogeneity variable"
	local het_label "Heterogeneity variable"

    local nvars : word count `covariates'
	local row 3
    local rowse 4

	//di `nvars'
	//di `covariates'
	//matrix list rtable1

  forvalues covar_num = 1/`nvars' {
	local covar : word `covar_num' of `covariates'
		if "`covar'"== "_cons"  {
			local covar_label Constant
			putdocx table mytable(`row',1) = ("`covar_label'")
			}	
			else {
				if "`covar'" == "1.HT#1.`var1'" | "`covar'" == "1.HT#1.`var2'" |  "`covar'" == "1.HT#1.`var3'" {
				local covar_label "`interaction_label'"
				putdocx table mytable(`row',1) = ("`covar_label'"), italic
				}
				else {
					if "`covar'" == "`var1'" | "`covar'" == "`var2'" | "`covar'" == "`var3'" {
					local covar_label "`het_label'"
					putdocx table mytable(`row',1) = ("`covar_label'"), italic
					} 
					else {
						local covar_label : variable label `covar'
						putdocx table mytable(`row',1) = ("`covar_label'"), italic
						}
					}
				}


		 
        * Extract coefficients and standard errors
		local b1_val: display %9.3f rtable1[1,`covar_num']
		local se1_val: display %9.3f rtable1[2,`covar_num']
		local p1_val: display %9.3f rtable1[4,`covar_num']
		

		local b2_val: display %9.3f rtable2[1,`covar_num']
		local se2_val: display %9.3f rtable2[2,`covar_num']
		local p2_val: display %9.3f rtable2[4,`covar_num']
		
		local b3_val: display %9.3f rtable3[1,`covar_num']
		local se3_val: display %9.3f rtable3[2,`covar_num']
		local p3_val: display %9.3f rtable3[4,`covar_num']
		
		local b4_val: display %9.3f rtable4[1,`covar_num']
		local se4_val: display %9.3f rtable4[2,`covar_num']
		local p4_val: display %9.3f rtable4[4,`covar_num']

		local b5_val: display %9.3f rtable5[1,`covar_num']
		local se5_val: display %9.3f rtable5[2,`covar_num']
		local p5_val: display %9.3f rtable5[4,`covar_num']
		
		
		local b6_val: display %9.3f rtable6[1,`covar_num']
		local se6_val: display %9.3f rtable6[2,`covar_num']
		local p6_val: display %9.3f rtable6[4,`covar_num']
		
		local b7_val: display %9.3f rtable7[1,`covar_num']
		local se7_val: display %9.3f rtable7[2,`covar_num']
		local p7_val: display %9.3f rtable7[4,`covar_num']
		
		local b8_val: display %9.3f rtable8[1,`covar_num']
		local se8_val: display %9.3f rtable8[2,`covar_num']
		local p8_val: display %9.3f rtable8[4,`covar_num']
		
		local b9_val: display %9.3f rtable9[1,`covar_num']
		local se9_val: display %9.3f rtable9[2,`covar_num']
		local p9_val: display %9.3f rtable9[4,`covar_num']
		
		
		local b10_val: display %9.3f rtable10[1,`covar_num']
		local se10_val: display %9.3f rtable10[2,`covar_num']
		local p10_val: display %9.3f rtable10[4,`covar_num']
		
		local b11_val: display %9.3f rtable11[1,`covar_num']
		local se11_val: display %9.3f rtable11[2,`covar_num']
		local p11_val: display %9.3f rtable11[4,`covar_num']
		
		local b12_val: display %9.3f rtable12[1,`covar_num']
		local se12_val: display %9.3f rtable12[2,`covar_num']
		local p12_val: display %9.3f rtable12[4,`covar_num']
		* Adjusted p-values
		
		local wy_pvalue1: display %9.3f (wy_results1[1, 4])
		local wy_pvalue2: display %9.3f (wy_results1[2, 4])
		local wy_pvalue3: display %9.3f (wy_results1[3, 4])
		local wy_pvalue4: display %9.3f (wy_results1[4, 4])
		local wy_pvalue5: display %9.3f (wy_results2[1, 4])
		local wy_pvalue6: display %9.3f (wy_results2[2, 4])
		local wy_pvalue7: display %9.3f (wy_results2[3, 4])
		local wy_pvalue8: display %9.3f (wy_results2[4, 4])
		local wy_pvalue9: display %9.3f (wy_results3[1, 4])
		local wy_pvalue10: display %9.3f (wy_results3[2, 4])
		local wy_pvalue11: display %9.3f (wy_results3[3, 4])
		local wy_pvalue12: display %9.3f (wy_results3[4, 4])
	


        * Add stars based on p-values
        local stars1 = ""
        if `p1_val' < 0.001 local stars1 = "***"
        else if `p1_val' < 0.01 local stars1 = "**"
        else if `p1_val' < 0.05 local stars1 = "*"

        local stars2 = ""
        if `p2_val' < 0.001 local stars2 = "***"
        else if `p2_val' < 0.01 local stars2 = "**"
        else if `p2_val' < 0.05 local stars2 = "*"

        local stars3 = ""
        if `p3_val' < 0.001 local stars3 = "***"
        else if `p3_val' < 0.01 local stars3 = "**"
        else if `p3_val' < 0.05 local stars3 = "*"

        local stars4 = ""
        if `p4_val' < 0.001 local stars4 = "***"
        else if `p4_val' < 0.01 local stars4 = "**"
        else if `p4_val' < 0.05 local stars4 = "*"
		
		local stars5 = ""
		if `p5_val' < 0.001 local stars5 = "***"
		else if `p5_val' < 0.01 local stars5 = "**"
		else if `p5_val' < 0.05 local stars5 = "*"

		local stars6 = ""
		if `p6_val' < 0.001 local stars6 = "***"
		else if `p6_val' < 0.01 local stars6 = "**"
		else if `p6_val' < 0.05 local stars6 = "*"

		local stars7 = ""
		if `p7_val' < 0.001 local stars7 = "***"
		else if `p7_val' < 0.01 local stars7 = "**"
		else if `p7_val' < 0.05 local stars7 = "*"

		local stars8 = ""
		if `p8_val' < 0.001 local stars8 = "***"
		else if `p8_val' < 0.01 local stars8 = "**"
		else if `p8_val' < 0.05 local stars8 = "*"

		local stars9 = ""
		if `p9_val' < 0.001 local stars9 = "***"
		else if `p9_val' < 0.01 local stars9 = "**"
		else if `p9_val' < 0.05 local stars9 = "*"

		local stars10 = ""
		if `p10_val' < 0.001 local stars10 = "***"
		else if `p10_val' < 0.01 local stars10 = "**"
		else if `p10_val' < 0.05 local stars10 = "*"

		local stars11 = ""
		if `p11_val' < 0.001 local stars11 = "***"
		else if `p11_val' < 0.01 local stars11 = "**"
		else if `p11_val' < 0.05 local stars11 = "*"

		local stars12 = ""
		if `p12_val' < 0.001 local stars12 = "***"
		else if `p12_val' < 0.01 local stars12 = "**"
		else if `p12_val' < 0.05 local stars12 = "*"


        * Adjusted p-values for HT and HT#var variable
		if "`covar'" == "HT"  {
			local stars_adj1 = ""
			if `wy_pvalue1' < 0.001 {
				local stars_adj1 = "+++"
			}
			else if `wy_pvalue1' < 0.01 {
				local stars_adj1 = "++"
			}
			else if `wy_pvalue1' < 0.05 {
				local stars_adj1 = "+"
			}

			local stars_adj2 = ""
			if `wy_pvalue2' < 0.001 {
				local stars_adj2 = "+++"
			}
			else if `wy_pvalue2' < 0.01 {
				local stars_adj2 = "++"
			}
			else if `wy_pvalue2' < 0.05 {
				local stars_adj2 = "+"
			}

			local stars_adj3 = ""
			if `wy_pvalue3' < 0.001 {
				local stars_adj3 = "+++"
			}
			else if `wy_pvalue3' < 0.01 {
				local stars_adj3 = "++"
			}
			else if `wy_pvalue3' < 0.05 {
				local stars_adj3 = "+"
			}

			local stars_adj4 = ""
			if `wy_pvalue4' < 0.001 {
				local stars_adj4 = "+++"
			}
			else if `wy_pvalue4' < 0.01 {
				local stars_adj4 = "++"
			}
			else if `wy_pvalue4' < 0.05 {
				local stars_adj4 = "+"
			}

			local stars_adj5 = ""
			if `wy_pvalue5' < 0.001 {
				local stars_adj5 = "+++"
			}
			else if `wy_pvalue5' < 0.01 {
				local stars_adj5 = "++"
			}
			else if `wy_pvalue5' < 0.05 {
				local stars_adj5 = "+"
			}

			local stars_adj6 = ""
			if `wy_pvalue6' < 0.001 {
				local stars_adj6 = "+++"
			}
			else if `wy_pvalue6' < 0.01 {
				local stars_adj6 = "++"
			}
			else if `wy_pvalue6' < 0.05 {
				local stars_adj6 = "+"
			}

			local stars_adj7 = ""
			if `wy_pvalue7' < 0.001 {
				local stars_adj7 = "+++"
			}
			else if `wy_pvalue7' < 0.01 {
				local stars_adj7 = "++"
			}
			else if `wy_pvalue7' < 0.05 {
				local stars_adj7 = "+"
			}

			local stars_adj8 = ""
			if `wy_pvalue8' < 0.001 {
				local stars_adj8 = "+++"
			}
			else if `wy_pvalue8' < 0.01 {
				local stars_adj8 = "++"
			}
			else if `wy_pvalue8' < 0.05 {
				local stars_adj8 = "+"
			}

			local stars_adj9 = ""
			if `wy_pvalue9' < 0.001 {
				local stars_adj9 = "+++"
			}
			else if `wy_pvalue9' < 0.01 {
				local stars_adj9 = "++"
			}
			else if `wy_pvalue9' < 0.05 {
				local stars_adj9 = "+"
			}

			local stars_adj10 = ""
			if `wy_pvalue10' < 0.001 {
				local stars_adj10 = "+++"
			}
			else if `wy_pvalue10' < 0.01 {
				local stars_adj10 = "++"
			}
			else if `wy_pvalue10' < 0.05 {
				local stars_adj10 = "+"
			}

			local stars_adj11 = ""
			if `wy_pvalue11' < 0.001 {
				local stars_adj11 = "+++"
			}
			else if `wy_pvalue11' < 0.01 {
				local stars_adj11 = "++"
			}
			else if `wy_pvalue11' < 0.05 {
				local stars_adj11 = "+"
			}

			local stars_adj12 = ""
			if `wy_pvalue12' < 0.001 {
				local stars_adj12 = "+++"
			}
			else if `wy_pvalue12' < 0.01 {
				local stars_adj12 = "++"
			}
			else if `wy_pvalue12' < 0.05 {
				local stars_adj12 = "+"
			}
		}
	

		if "`covar'" == "HT" {
		* Insert coefficients, standard errors, and stars for each model
		putdocx table mytable(`row',2) = ("`=strltrim("`b1_val'")'`stars1'`stars_adj1'"), halign(left) linebreak
		putdocx table mytable(`row',2) = ("(`=strltrim("`se1_val'")')"), append linebreak
		putdocx table mytable(`row',3) = ("`=strltrim("`b2_val'")'`stars2'`stars_adj2'"), halign(left) linebreak
		putdocx table mytable(`row',3) = ("(`=strltrim("`se2_val'")')"), append linebreak
		putdocx table mytable(`row',4) = ("`=strltrim("`b3_val'")'`stars3'`stars_adj3'"), halign(left) linebreak
		putdocx table mytable(`row',4) = ("(`=strltrim("`se3_val'")')"), append linebreak
		putdocx table mytable(`row',5) = ("`=strltrim("`b4_val'")'`stars4'`stars_adj4'"), halign(left) linebreak
		putdocx table mytable(`row',5) = ("(`=strltrim("`se4_val'")')"), append linebreak
		putdocx table mytable(`row',6) = ("`=strltrim("`b5_val'")'`stars5'`stars_adj5'"), halign(left) linebreak
		putdocx table mytable(`row',6) = ("(`=strltrim("`se5_val'")')"), append linebreak
		putdocx table mytable(`row',7) = ("`=strltrim("`b6_val'")'`stars6'`stars_adj6'"), halign(left) linebreak
		putdocx table mytable(`row',7) = ("(`=strltrim("`se6_val'")')"), append linebreak
		putdocx table mytable(`row',8) = ("`=strltrim("`b7_val'")'`stars7'`stars_adj7'"), halign(left) linebreak
		putdocx table mytable(`row',8) = ("(`=strltrim("`se7_val'")')"), append linebreak
		putdocx table mytable(`row',9) = ("`=strltrim("`b8_val'")'`stars8'`stars_adj8'"), halign(left) linebreak
		putdocx table mytable(`row',9) = ("(`=strltrim("`se8_val'")')"), append linebreak
		putdocx table mytable(`row',10) = ("`=strltrim("`b9_val'")'`stars9'`stars_adj9'"), halign(left) linebreak
		putdocx table mytable(`row',10) = ("(`=strltrim("`se9_val'")')"), append linebreak
		putdocx table mytable(`row',11) = ("`=strltrim("`b10_val'")'`stars10'`stars_adj10'"), halign(left) linebreak
		putdocx table mytable(`row',11) = ("(`=strltrim("`se10_val'")')"), append linebreak
		putdocx table mytable(`row',12) = ("`=strltrim("`b11_val'")'`stars11'`stars_adj11'"), halign(left) linebreak
		putdocx table mytable(`row',12) = ("(`=strltrim("`se11_val'")')"), append linebreak
		putdocx table mytable(`row',13) = ("`=strltrim("`b12_val'")'`stars12'`stars_adj12'"), halign(left) linebreak
		putdocx table mytable(`row',13) = ("(`=strltrim("`se12_val'")')"), append linebreak
	
	}
	else {
		putdocx table mytable(`row',2) = ("`=strltrim("`b1_val'")'`stars1'`stars_adj1'"), halign(left) linebreak
		putdocx table mytable(`row',2) = ("(`=strltrim("`se1_val'")')"), append
		putdocx table mytable(`row',3) = ("`=strltrim("`b2_val'")'`stars2'`stars_adj2'"), halign(left) linebreak
		putdocx table mytable(`row',3) = ("(`=strltrim("`se2_val'")')"), append
		putdocx table mytable(`row',4) = ("`=strltrim("`b3_val'")'`stars3'`stars_adj3'"), halign(left) linebreak
		putdocx table mytable(`row',4) = ("(`=strltrim("`se3_val'")')"), append 
		putdocx table mytable(`row',5) = ("`=strltrim("`b4_val'")'`stars4'`stars_adj4'"), halign(left) linebreak
		putdocx table mytable(`row',5) = ("(`=strltrim("`se4_val'")')"), append 
		putdocx table mytable(`row',6) = ("`=strltrim("`b5_val'")'`stars5'`stars_adj5'"), halign(left) linebreak
		putdocx table mytable(`row',6) = ("(`=strltrim("`se5_val'")')"), append 
		putdocx table mytable(`row',7) = ("`=strltrim("`b6_val'")'`stars6'`stars_adj6'"), halign(left) linebreak
		putdocx table mytable(`row',7) = ("(`=strltrim("`se6_val'")')"), append 
		putdocx table mytable(`row',8) = ("`=strltrim("`b7_val'")'`stars7'`stars_adj7'"), halign(left) linebreak
		putdocx table mytable(`row',8) = ("(`=strltrim("`se7_val'")')"), append 
		putdocx table mytable(`row',9) = ("`=strltrim("`b8_val'")'`stars8'`stars_adj8'"), halign(left) linebreak
		putdocx table mytable(`row',9) = ("(`=strltrim("`se8_val'")')"), append 
		putdocx table mytable(`row',10) = ("`=strltrim("`b9_val'")'`stars9'`stars_adj9'"), halign(left) linebreak
		putdocx table mytable(`row',10) = ("(`=strltrim("`se9_val'")')"), append 
		putdocx table mytable(`row',11) = ("`=strltrim("`b10_val'")'`stars10'`stars_adj10'"), halign(left) linebreak
		putdocx table mytable(`row',11) = ("(`=strltrim("`se10_val'")')"), append 
		putdocx table mytable(`row',12) = ("`=strltrim("`b11_val'")'`stars11'`stars_adj11'"), halign(left) linebreak
		putdocx table mytable(`row',12) = ("(`=strltrim("`se11_val'")')"), append 
		putdocx table mytable(`row',13) = ("`=strltrim("`b12_val'")'`stars12'`stars_adj12'"), halign(left) linebreak
		putdocx table mytable(`row',13) = ("(`=strltrim("`se12_val'")')"), append 
	}

        * Increment row
        local row = `row' + 1
        
		
	local stars_adj1 ""
	local stars_adj2 ""
	local stars_adj3 ""
	local stars_adj4 ""
	local stars_adj5 ""
	local stars_adj6 ""
	local stars_adj7 ""
	local stars_adj8 ""
	local stars_adj9 ""
	local stars_adj10 ""
	local stars_adj11 ""
	local stars_adj12 ""
	}

	
local wy_pvalue1: display %9.3f (wy_results1[1, 4])
local wy_pvalue2: display %9.3f (wy_results1[2, 4])
local wy_pvalue3: display %9.3f (wy_results1[3, 4])
local wy_pvalue4: display %9.3f (wy_results1[4, 4])
local wy_pvalue5: display %9.3f (wy_results2[1, 4])
local wy_pvalue6: display %9.3f (wy_results2[2, 4])
local wy_pvalue7: display %9.3f (wy_results2[3, 4])
local wy_pvalue8: display %9.3f (wy_results2[4, 4])
local wy_pvalue9: display %9.3f (wy_results3[1, 4])
local wy_pvalue10: display %9.3f (wy_results3[2, 4])
local wy_pvalue11: display %9.3f (wy_results3[3, 4])
local wy_pvalue12: display %9.3f (wy_results3[4, 4])

putdocx table mytable(3, 2) = ("[`=strltrim("`wy_pvalue1'")']"), append 
putdocx table mytable(3, 3) = ("[`=strltrim("`wy_pvalue2'")']"), append
putdocx table mytable(3, 4) = ("[`=strltrim("`wy_pvalue3'")']"), append
putdocx table mytable(3, 5) = ("[`=strltrim("`wy_pvalue4'")']"), append
putdocx table mytable(3, 6) = ("[`=strltrim("`wy_pvalue5'")']"), append
putdocx table mytable(3, 7) = ("[`=strltrim("`wy_pvalue6'")']"), append
putdocx table mytable(3, 8) = ("[`=strltrim("`wy_pvalue7'")']"), append
putdocx table mytable(3, 9) = ("[`=strltrim("`wy_pvalue8'")']"), append
putdocx table mytable(3, 10) = ("[`=strltrim("`wy_pvalue9'")']"), append
putdocx table mytable(3, 11) = ("[`=strltrim("`wy_pvalue10'")']"), append
putdocx table mytable(3, 12) = ("[`=strltrim("`wy_pvalue11'")']"), append
putdocx table mytable(3, 13) = ("[`=strltrim("`wy_pvalue12'")']"), append



*adding observations
putdocx table mytable(`row',1) = ("N")
putdocx table mytable(`row',2) = (nobs1)
putdocx table mytable(`row',3) = (nobs2)
putdocx table mytable(`row',4) = (nobs3)
putdocx table mytable(`row',5) = (nobs4)
putdocx table mytable(`row',6) = (nobs5)
putdocx table mytable(`row',7) = (nobs6)
putdocx table mytable(`row',8) = (nobs7)
putdocx table mytable(`row',9) = (nobs8)
putdocx table mytable(`row',10) = (nobs9)
putdocx table mytable(`row',11) = (nobs10)
putdocx table mytable(`row',12) = (nobs11)
putdocx table mytable(`row',13) = (nobs12)




local row = `row' + 1
local row2 = `row' + 1

*Adding R² and adjusted R²
	
putdocx table mytable(`row',1) = ("R²")
putdocx table mytable(`row2',1) = ("R²adj")

	forvalues mod = 1/12 {		
		local col_num = `mod' + 1
		
		putdocx table mytable(`row',`col_num') = ("`=strltrim("`r2_`mod''")'")
		putdocx table mytable(`row2',`col_num') =("`=strltrim("`r2_adj_`mod''")'")
	}
	
	local row = `row' + 1

	* Adding  horizontal lines
	forvalues col_num = 1/13 {
		local row_2=`row'-2
		putdocx table mytable(1,`col_num'), border(top, "single")
		putdocx table mytable(2,`col_num'), border(bottom, "single")
		putdocx table mytable(`row_2',`col_num'), border(top, "single")
		putdocx table mytable(`row',`col_num'), border(bottom, "single")
	}
	 forvalues col_num = 2/13 {
		putdocx table mytable(1,`col_num'), border(bottom, "single")
	}



	local row = `row' + 1
	local table_num  = `table_num' + 1
	
putdocx table mytable(`row',1) = ("Note: Robust standard errors in parentheses."), colspan(13) linebreak 
putdocx table mytable(`row',1) = ("For health-risk information, Westfall-Young MHT adjusted p-values in brackets with 10,000 bootstrap replications. "),  append linebreak 
putdocx table mytable(`row',1) = ("***p<0.001, **p<0.01, *p<0.05 for standard p-values, +++p< 0.001, ++p < 0.01, +p < 0.05 for Westfall-Young MHT adjusted p-values."),  append

	putdocx pagebreak
}

}

* ========================
**# Table SC43: Robustness test (basic) 7: Instrumented variable
* ========================
*putdocx text ("Table SC`table_num' Two stage least square results of the basic models, utilising an instrument variable approach - pooled sample."), font("", 11)	


qui {
	
qui	ivregress 2sls meat_tax_dummy  FR_dummy LV_dummy (mc_self_dummy =  HT), vce(robust)
matrix rtable1=r(table)
scalar nobs1 = e(N)

qui ivregress 2sls meat_tax_s  FR_dummy LV_dummy (mc_soc_dummy =  HT), vce(robust)
matrix rtable2=r(table)
scalar nobs2 = e(N)

qui ivregress 2sls meat_day_dummy  FR_dummy LV_dummy (mc_self_dummy =  HT), vce(robust)
matrix rtable3=r(table)
scalar nobs3 = e(N)

qui ivregress 2sls meat_day_s  FR_dummy LV_dummy (mc_soc_dummy =  HT), vce(robust)
matrix rtable4=r(table)
scalar nobs4 = e(N)
	
	
*Add title
putdocx paragraph
putdocx text ("Table SC`table_num' Two stage least square results of the basic models, with harmfulness of meat consumption instrumented on health-risk information - pooled sample."), font("", 11)	

*Create a table with 5 rows and 5 columns (1 for covariate names, 4 for models)
putdocx table mytable = (7, 5), border(all, nil) width(4) width(100%) 

*headers for each model
putdocx table mytable(1,1) = ("")
putdocx table mytable(1,2) = ("Meat tax acceptability for oneself")
putdocx table mytable(1,3) = ("Meat Tax acceptability for society")
putdocx table mytable(1,4) = ("Meat-free days acceptability for oneself")
putdocx table mytable(1,5) = ("Meat-free days acceptability for society")



local covariates mc_soc_dummy FR_dummy LV_dummy _cons
local nvars : word count `covariates'
local row 2  // Start from row 2 since row 1 has headers
local rowse 3

forvalues covar_num = 1/`nvars' {
	local covar : word `covar_num' of `covariates'
	if "`covar'" == "_cons" {
		local covar_label Constant
		putdocx table mytable(`row',1) = ("`covar_label'")
	}
		else {
			 local covar_label : variable label `covar'
			 putdocx table mytable(`row',1) = ("`covar_label'"), italic
			 }
		
		
    * Extract coefficients and standard errors
    local b1_val: display %9.3f rtable1[1,`covar_num']
    local se1_val: display %9.3f rtable1[2,`covar_num']
    local p1_val: display %9.3f rtable1[4,`covar_num']
	
    
    local b2_val: display %9.3f rtable2[1,`covar_num']
    local se2_val: display %9.3f rtable2[2,`covar_num']
    local p2_val: display %9.3f rtable2[4,`covar_num']
    
    local b3_val: display %9.3f rtable3[1,`covar_num']
    local se3_val: display %9.3f rtable3[2,`covar_num']
    local p3_val: display %9.3f rtable3[4,`covar_num']
    
    local b4_val: display %9.3f rtable4[1,`covar_num']
    local se4_val: display %9.3f rtable4[2,`covar_num']
    local p4_val: display %9.3f rtable4[4,`covar_num']

    * Adjusted p-values
	
	
	
    * Add stars based on p-values
    local stars1 = ""
    if `p1_val' < 0.001 {
        local stars1 = "***"
    }
    else if `p1_val' < 0.01 {
        local stars1 = "**"
    }
    else if `p1_val' < 0.05 {
        local stars1 = "*"
    }

    local stars2 = ""
    if `p2_val' < 0.001 {
        local stars2 = "***"
    }
    else if `p2_val' < 0.01 {
        local stars2 = "**"
    }
    else if `p2_val' < 0.05 {
        local stars2 = "*"
    }

    local stars3 = ""
    if `p3_val' < 0.001 {
        local stars3 = "***"
    }
    else if `p3_val' < 0.01 {
        local stars3 = "**"
    }
    else if `p3_val' < 0.05 {
        local stars3 = "*"
    }

    local stars4 = ""
    if `p4_val' < 0.001 {
        local stars4 = "***"
    }
    else if `p4_val' < 0.01 {
        local stars4 = "**"
    }
    else if `p4_val' < 0.05 {
        local stars4 = "*"
    }
	



    * Insert coefficients, standard errors, and stars for each model
    putdocx table mytable(`row',2) = ("`=strltrim("`b1_val'")'`stars1'"), halign(left) linebreak
    putdocx table mytable(`row',2) = ("(`=strltrim("`se1_val'")')"), append 
    putdocx table mytable(`row',3) = ("`=strltrim("`b2_val'")'`stars2'"), halign(left) linebreak
    putdocx table mytable(`row',3) = ("(`=strltrim("`se2_val'")')"), append 
    putdocx table mytable(`row',4) = ("`=strltrim("`b3_val'")'`stars3'"), halign(left) linebreak
    putdocx table mytable(`row',4) = ("(`=strltrim("`se3_val'")')"), append 
    putdocx table mytable(`row',5) = ("`=strltrim("`b4_val'")'`stars4'"), halign(left) linebreak
    putdocx table mytable(`row',5) = ("(`=strltrim("`se4_val'")')"), append 
	
    
    * Increment the row counter for the next covariate
    local row = `row' + 1
	
	
}

	putdocx table mytable(`row',1) = ("N")
	putdocx table mytable(`row',2) = (nobs1)
	putdocx table mytable(`row',3) = (nobs2)
	putdocx table mytable(`row',4) = (nobs3)
	putdocx table mytable(`row',5) = (nobs4)


	* Adding  horizontal lines
	forvalues col_num = 1/5 {
		putdocx table mytable(1,`col_num'), border(top, "single")
		putdocx table mytable(1,`col_num'), border(bottom, "single")
		putdocx table mytable(`row',`col_num'), border(top, "single")
		putdocx table mytable(`row',`col_num'), border(bottom, "single")
	}


local row = `row' + 1

putdocx table mytable(`row',1) = ("Note: Robust standard errors in parentheses."), colspan(5) linebreak 
putdocx table mytable(`row',1) = ("***p<0.001, **p<0.01, *p<0.05"),  append


}


putdocx save "Supplementary materials/Supplementary material SC.docx", replace
