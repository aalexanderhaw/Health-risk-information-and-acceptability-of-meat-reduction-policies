* ========================
**# Stata code to reproduce results in the Supplementary material of the revised manuscript titled „Does health-risk information increase the acceptability of a meat tax and meat free days? Experimental evidence from three European countries"
* ========================


* ========================
**# Section 1: Data Wrangling
* ========================



* Load data
clear
use "data/clean_data.dta"


* ========================
**## Section 0: Data prep
* ========================

rename health_treat HT 

gen meat_diet_dummy = cond(inlist(d1, 1,2), 1, 0)


label variable meat_tax_dummy  "Meat tax acceptability for oneself"
label variable meat_tax_soc  "Meat tax acceptability for society"
label variable meat_day_dummy "Meat-free days acceptability for oneself"
label variable meat_day_s "Meat-free days acceptability for society"

label variable cost_tax_self_dummy "Expense for oneself (Meat tax)"
label variable cost_tax_soc_dummy "Expense for society (Meat tax)"
label variable eff_tax_self_dummy "Effectiveness for oneself (Meat tax)"
label variable eff_tax_soc_dummy "Effectiveness for society (Meat tax)"
label variable fair_tax_self_dummy "Fairness for oneself (Meat tax)"
label variable fair_tax_soc_dummy "Fairness for society (Meat tax)"

label variable cost_veggie_self_dummy "Expense for oneself (Meat-free days)"
label variable cost_veggie_soc_dummy "Expense for society (Meat-free days)"
label variable eff_veggie_self_dummy "Effectiveness for oneself (Meat-free days)"
label variable eff_veggie_soc_dummy "Effectiveness for society (Meat-free days)"
label variable fair_veggie_self_dummy "Fairness for oneself (Meat-free days)"
label variable fair_veggie_soc_dummy "Fairness for society (Meat-free days)"

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
label variable income_pp_1000 "Income (in T€)"
label variable vegan_veg_pesc_dummy "Meat-free diet"
label variable meat_diet_dummy "High-meat diet"
label variable flex_dummy "Low-meat diet" 
label variable high_meat_dummy "Varied and high-meat diet"
label variable po1_dummy "Support nationally oriented policies"
label variable po2_dummy "Support social policies"
label variable po3_dummy "Support conservative policies"
label variable po4_dummy "Support liberal policies"
label variable po5_dummy "Support environmental policies"
label variable diet_knowl_high "Nutrition knowledge"
label variable cc_acc_high "Climate change acknowledgement"
label variable soc_norm_high "Meat reduction social norms"

label variable mc1 "Harmfulness of meat consumption for oneself"
label variable mc2 "Harmfulness of meat consumption for society"


* ========================
**# Supplementary material SB.1: Balancing and manipulation checks
* ========================

* ========================
**## Table SB1 Balancing checks
* ========================

qui {
putdocx clear
putdocx begin

putdocx paragraph
putdocx text ("Table SB1 Balancing check tests")
putdocx table mytable = (23, 9), border(all, nil) 

* Row 1: Table SBolumn Headers
putdocx table mytable(1, 1) = ("country"), colspan(1)
putdocx table mytable(1, 2) = ("Control group"), colspan(3)
putdocx table mytable(1, 3) = ("Treatment group"), colspan(3)
putdocx table mytable(1, 4) = ("T-test"), colspan(2)


* Row 2: Specific Headers
putdocx table mytable(2, 2) = ("N")
putdocx table mytable(2, 3) = ("Mean")
putdocx table mytable(2, 4) = ("SD")
putdocx table mytable(2, 5) = ("N")
putdocx table mytable(2, 6) = ("Mean")
putdocx table mytable(2, 7) = ("SD")
putdocx table mytable(2, 8) = ("Test statistic")
putdocx table mytable(2, 9) = ("P-value")

putdocx table mytable(3,1) = ("Age"), colspan(9) italic
putdocx table mytable(8,1) = ("Female"), colspan(9) italic
putdocx table mytable(13,1) = ("Low education"), colspan(9) italic
putdocx table mytable(18,1) = ("Income (in T€)"), colspan(9) italic


* Initialize row counter
local row = 3

* Loop through each country
	
	
		local variables age female educ_low income_pp_1000 
		local nvars : word count `variables'
		
    	forvalues var_num = 1/`nvars' {
		local row = `row' + 1 
		local var : word `var_num' of `variables'
		
		
foreach country in "France" "Italy" "Latvia" {

      
        * Summarize for the non-treatment group
        quietly summarize `var' if HT == 0 & country == "`country'"
		local nonHT_N = r(N)
        local nonHT_mean: display %9.3f r(mean)
        local nonHT_sd: display %9.3f r(sd)

        
        * Summarize for the treatment group
        quietly summarize `var' if HT == 1 & country == "`country'"
		local HT_N = r(N)
        local HT_mean: display %9.3f r(mean)
        local HT_sd: display %9.3f r(sd)

        * t-test for the difference between groups
        quietly ttest `var', by(HT) unequal, if country == "`country'"
        local p_value: display %9.3f r(p)
        local t_value: display %9.3f r(t)
	
	    local stars ""
        if `p_value' < 0.001 {
            local stars "***"
        }
        else if `p_value' < 0.01 {
            local stars "**"
        }
        else if `p_value' < 0.05 {
            local stars "*"
        }
 
        putdocx table mytable(`row', 1) = ("`country'")
		putdocx table mytable(`row', 2) = ("`nonHT_N'")
		putdocx table mytable(`row', 3) = ("`=strltrim("`nonHT_mean'")'")
        putdocx table mytable(`row', 4) = ("`=strltrim("`nonHT_sd'")'")
		putdocx table mytable(`row', 5) = ("`HT_N'")
        putdocx table mytable(`row', 6) = ("`=strltrim("`HT_mean'")'")
        putdocx table mytable(`row', 7) = ("`=strltrim("`HT_sd'")'")
        putdocx table mytable(`row', 8) = ("`=strltrim("`t_value'")'")
        putdocx table mytable(`row', 9) = ("`=strltrim("`p_value'`stars'")'")
		
        * Move to the next row

		local row = `row' + 1
    }
	

	*Pooled
        * Summarize for the non-treatment group
        quietly summarize `var' if HT == 0 
		local nonHT_N = r(N)
        local nonHT_mean: display %9.3f r(mean)
        local nonHT_sd: display %9.3f r(sd)

        
        * Summarize for the treatment group
        quietly summarize `var' if HT == 1 
		local HT_N = r(N)
        local HT_mean: display %9.3f r(mean)
        local HT_sd: display %9.3f r(sd)

        * t-test for the difference between groups
        quietly ttest `var', by(HT) unequal,
        local p_value: display %9.3f r(p)
        local t_value: display %9.3f = r(t)
	
	        local stars ""
        if `p_value' < 0.001 {
            local stars "***"
        }
        else if `p_value' < 0.01 {
            local stars "**"
        }
        else if `p_value' < 0.05 {
            local stars "*"
        }
 
 
        putdocx table mytable(`row', 1) = ("Pooled")
		putdocx table mytable(`row', 2) = ("`nonHT_N'")
		putdocx table mytable(`row', 3) = ("`=strltrim("`nonHT_mean'")'")
        putdocx table mytable(`row', 4) = ("`=strltrim("`nonHT_sd'")'")
		putdocx table mytable(`row', 5) = ("`HT_N'")
        putdocx table mytable(`row', 6) = ("`=strltrim("`HT_mean'")'")
        putdocx table mytable(`row', 7) = ("`=strltrim("`HT_sd'")'")
        putdocx table mytable(`row', 8) = ("`=strltrim("`t_value'")'")
        putdocx table mytable(`row', 9) = ("`=strltrim("`p_value'`stars'")'")
		
		
		
        * Move to the next row

		local row = `row' + 1

	}
	
* Adding  horizontal lines
forvalues col_num = 1/9 {
	putdocx table mytable(1,`col_num'), border(top, "single")
	putdocx table mytable(2,`col_num'), border(bottom, "single")
	putdocx table mytable(`row',`col_num'), border(top, "single")
	putdocx table mytable(3,`col_num'), border(top, "single")
	putdocx table mytable(3,`col_num'), border(bottom, "single")
	putdocx table mytable(8,`col_num'), border(top, "single")
	putdocx table mytable(8,`col_num'), border(bottom, "single")
	putdocx table mytable(13,`col_num'), border(top, "single")
	putdocx table mytable(13,`col_num'), border(bottom, "single")
	putdocx table mytable(18,`col_num'), border(top, "single")
	putdocx table mytable(18,`col_num'), border(bottom, "single")
	}
	
putdocx table mytable(`row',1) = ("***p<0.001, **p<0.01, *p<0.05"), colspan(9) 
}


* ========================
**## Table SB2 Manipulation checks
* ========================

qui {

putdocx paragraph
putdocx text ("Table SB2 Manipulation check tests")
putdocx table mytable = (13, 9), border(all, nil)

* Row 1: Table SBolumn Headers
putdocx table mytable(1, 1) = ("country"), colspan(1)
putdocx table mytable(1, 2) = ("Control group"), colspan(3)
putdocx table mytable(1, 3) = ("Treatment group"), colspan(3)
putdocx table mytable(1, 4) = ("T-test"), colspan(2)

* Row 2: Specific Headers
putdocx table mytable(2, 2) = ("N")
putdocx table mytable(2, 3) = ("Mean")
putdocx table mytable(2, 4) = ("SD")
putdocx table mytable(2, 5) = ("N")
putdocx table mytable(2, 6) = ("Mean")
putdocx table mytable(2, 7) = ("SD")
putdocx table mytable(2, 8) = ("Test statistic")
putdocx table mytable(2, 9) = ("P-value")


putdocx table mytable(3,1) = ("Harmfulness of meat consumption for oneself"), colspan(9) italic
putdocx table mytable(8,1) = ("Harmfulness of meat consumption for society"), colspan(9) italic

* Initialize row counter
local row = 3

* Manipulation Check for each variable and country
foreach var in mc1 mc2 {
	local row = `row' + 1 
    foreach country in "France" "Italy" "Latvia" {
         * Summarize for the non-treatment group
        quietly summarize `var' if HT == 0 & country == "`country'"
        local control_N = r(N)
        local control_mean: display %9.3f r(mean)
        local control_sd: display %9.3f r(sd)

        * Summarize for the treatment group
        quietly summarize `var' if HT == 1 & country == "`country'"
        local treatment_N = r(N)
        local treatment_mean: display %9.3f r(mean)
        local treatment_sd: display %9.3f r(sd)

        * T-test for the difference between groups
        quietly ttest `var', by(HT) unequal, if country == "`country'"
        local t_value: display %9.3f r(t)
        local p_value: display %9.3f r(p)
		
         * Significance stars for p-value
	        local stars ""
        if `p_value' < 0.001 {
            local stars "***"
        }
        else if `p_value' < 0.01 {
            local stars "**"
        }
        else if `p_value' < 0.05 {
            local stars "*"
        }

		putdocx table mytable(`row', 1) = ("`country'")	
        putdocx table mytable(`row', 2) = ("`=strltrim("`control_N'")'")
        putdocx table mytable(`row', 3) = ("`=strltrim("`control_mean'")'")
        putdocx table mytable(`row', 4) = ("`=strltrim("`control_sd'")'")
        putdocx table mytable(`row', 5) = ("`=strltrim("`treatment_N'")'")
        putdocx table mytable(`row', 6) = ("`=strltrim("`treatment_mean'")'")
        putdocx table mytable(`row', 7) = ("`=strltrim("`treatment_sd'")'")
        putdocx table mytable(`row', 8) = ("`=strltrim("`t_value'")'")
        putdocx table mytable(`row', 9) = ("`=strltrim("`p_value'`stars'")'")
		

	local row = `row' + 1 
	}


        * Summarize for the non-treatment group
        quietly summarize `var' if HT == 0 
        local control_N = r(N)
        local control_mean: display %9.3f r(mean)
        local control_sd: display %9.3f r(sd)

        * Summarize for the treatment group
        quietly summarize `var' if HT == 1
        local treatment_N = r(N)
        local treatment_mean: display %9.3f r(mean)
        local treatment_sd: display %9.3f r(sd)

        * T-test for the difference between groups
        quietly ttest `var', by(HT) unequal
        local t_value: display %9.3f r(t)
        local p_value: display %9.3f r(p)
	

        * Significance stars for p-value
	        local stars ""
        if `p_value' < 0.001 {
            local stars "***"
        }
        else if `p_value' < 0.01 {
            local stars "**"
        }
        else if `p_value' < 0.05 {
            local stars "*"
        }
		
		
        * Populate table
		putdocx table mytable(`row', 1) = ("Pooled")	
        putdocx table mytable(`row', 2) = ("`=strltrim("`control_N'")'")
        putdocx table mytable(`row', 3) = ("`=strltrim("`control_mean'")'")
        putdocx table mytable(`row', 4) = ("`=strltrim("`control_sd'")'")
        putdocx table mytable(`row', 5) = ("`=strltrim("`treatment_N'")'")
        putdocx table mytable(`row', 6) = ("`=strltrim("`treatment_mean'")'")
        putdocx table mytable(`row', 7) = ("`=strltrim("`treatment_sd'")'")
        putdocx table mytable(`row', 8) = ("`=strltrim("`t_value'")'")
        putdocx table mytable(`row', 9) = ("`=strltrim("`p_value'`stars'")'")
		
        local row = `row' + 1
		
				
    
	}

* Add horizontal lines
forvalues col_num = 1/9 {
    putdocx table mytable(1,`col_num'), border(top, "single")
    putdocx table mytable(2,`col_num'), border(bottom, "single")
    putdocx table mytable(`row',`col_num'), border(top, "single")
	putdocx table mytable(3,`col_num'), border(top, "single")
	putdocx table mytable(3,`col_num'), border(bottom, "single")
	putdocx table mytable(8,`col_num'), border(top, "single")
	putdocx table mytable(8,`col_num'), border(bottom, "single")
}

* Footnote with p-value significance levels
putdocx table mytable(`row', 1) = ("***p<0.001, **p<0.01, *p<0.05"), colspan(9)

}
* Save document
putdocx save "Supplementary materials/Supplementary material SB1.docx", replace


* ========================
**## Table SB4: Descriptive Statistics tables
* ========================

qui {
	
local row = 2
* Display the number of observations, mean, SD, min, and max
* Store results in a matrix and display the final table
* Initialize the Excel file and define the headers
putexcel set "Supplementary materials/Supplementary_material_Table_SB4.xlsx", replace
putexcel A1 = ("country") B1 = ("Variable") C1 = ("N") D1 = ("Mean") E1 = ("SD") F1 = ("Min") G1 = ("Max")

* Loop through each country
foreach cntry in "France" "Italy" "Latvia" { 
	
    preserve
    
    * Filter the data for the current country
    keep if country == "`cntry'"
    
    * Initialize a row counter
        
    * Loop through each variable and calculate summary statistics
    foreach var in meat_tax_dummy meat_tax_soc meat_day_dummy meat_day_s HT ///
    cost_tax_self_dummy cost_tax_soc_dummy  eff_tax_self_dummy eff_tax_soc_dummy fair_tax_self_dummy fair_tax_soc_dummy ///
    cost_veggie_self_dummy cost_veggie_soc_dummy  eff_veggie_self_dummy eff_veggie_soc_dummy fair_veggie_self_dummy fair_veggie_soc_dummy ///
    health_low food_depriv income_Q1 unemployed educ_low age female  income_pp_1000 canteen_dummy vegan_veg_pesc_dummy flex_dummy meat_diet_dummy  high_meat_dummy po1_dummy po2_dummy po3_dummy po4_dummy po5_dummy  diet_knowl_high cc_acc_high soc_norm_high {
        
        * Summarize the variable
        summarize `var'
        
        * Store the results in a matrix
		 matrix results = (r(N), round(r(mean), .001), round(r(sd), .001), round(r(min), .001), round(r(max), .001))
      
        
		local var_label : variable label `var'
		
        * Export results to Excel
        putexcel A`row' = "`cntry'" B`row' = "`var_label'" ///
            C`row' = (results[1,1]) D`row' = (results[1,2]) E`row' = (results[1,3]) ///
            F`row' = (results[1,4]) G`row' = (results[1,5])
        
        * Move to the next row
        local row = `row' + 1
    }
    
	
    restore
}

}

* ========================
**## Table SB5: Policy preferences
* ========================


putdocx clear
putdocx begin


qui {
preserve

* Data transformation into long format

rename meat_tax_dummy as_1
rename meat_tax_soc_dummy ap_1
rename meat_day_dummy as_3
rename meat_day_soc_dummy ap_3

tostring id, gen(id_str) 
gen id_country = country + "_" + id_str 
reshape long as_ ap_, i(id_country) j(policy)

*Only if in control group 
keep if HT==0

gen tax = cond(policy==1,1,0)
gen veggy = cond(policy==3,1,0)


gen policy_nom = "."
replace policy_nom = "tax" if tax == 1


destring  as_ ap_, force replace //into numeric


label variable tax  "Meat tax (vs. Meat-free day)"


* Table creation
	

*Add title
putdocx paragraph
putdocx text ("Table SB5 LPM estimation results of policy acceptability by policy – control group only."), font("", 11)	

*Create a table with 7 rows and 9 columns
putdocx table mytable = (10, 9), border(all, nil) width(4)  width(100%) 

// Define column headers
putdocx table mytable(1,1) = ("") 
putdocx table mytable(1,2) = ("Pooled"), colspan(2) 
putdocx table mytable(1,3) = ("France"), colspan(2) 
putdocx table mytable(1,4) = ("Italy"), colspan(2) 
putdocx table mytable(1,5) = ("Latvia"), colspan(2) 

putdocx table mytable(2,1) = ("") 
putdocx table mytable(2,2) = ("Policy acceptability for oneself") 
putdocx table mytable(2,3) = ("Policy acceptability for society") 
putdocx table mytable(2,4) = ("Policy acceptability for oneself") 
putdocx table mytable(2,5) = ("Policy acceptability for society") 
putdocx table mytable(2,6) = ("Policy acceptability for oneself") 
putdocx table mytable(2,7) = ("Policy acceptability for society") 
putdocx table mytable(2,8) = ("Policy acceptability for oneself") 
putdocx table mytable(2,9) = ("Policy acceptability for society") 


// Run regressions and store results
regress as_ tax FR_dummy LV_dummy, vce(hc3)
matrix rtable1 = r(table)
scalar nobs1 = e(N)
local r2_1 =  round(e(r2),0.001)
local r2_adj_1 = string(e(r2_a), "%9.3f")
regress ap_ tax FR_dummy LV_dummy, vce(hc3)
matrix rtable2 = r(table)
scalar nobs2 = e(N)
local r2_2 = string(e(r2), "%9.3f")
local r2_adj_2 = string(e(r2_a), "%9.3f")
regress as_ tax if IT_dummy == 1, vce(hc3)
matrix rtable3 = r(table)
scalar nobs3 = e(N)
local r2_3 = string(e(r2), "%9.3f")
local r2_adj_3 = string(e(r2_a), "%9.3f")
regress ap_ tax if IT_dummy == 1, vce(hc3)
matrix rtable4 = r(table)
scalar nobs4 = e(N)
local r2_4 = string(e(r2), "%9.3f")
local r2_adj_4 = string(e(r2_a), "%9.3f")
regress as_ tax if FR_dummy == 1, vce(hc3)
matrix rtable5 = r(table)
scalar nobs5 = e(N)
local r2_5 = string(e(r2), "%9.3f")
local r2_adj_5 = string(e(r2_a), "%9.3f")
regress ap_ tax if FR_dummy == 1, vce(hc3)
matrix rtable6 = r(table)
scalar nobs6 = e(N)
local r2_6 = string(e(r2), "%9.3f")
local r2_adj_6 = string(e(r2_a), "%9.3f")
regress as_ tax if LV_dummy == 1, vce(hc3)
matrix rtable7 = r(table)
scalar nobs7 = e(N)
local r2_7 = string(e(r2), "%9.3f")
local r2_adj_7 = string(e(r2_a), "%9.3f")
regress ap_ tax if LV_dummy == 1, vce(hc3)
matrix rtable8 = r(table)
scalar nobs8 = e(N)
local r2_8 = string(e(r2), "%9.3f")
local r2_adj_8 = string(e(r2_a), "%9.3f")


local covariates tax FR_dummy LV_dummy _cons
local nvars : word count `covariates'
local row 3  // Start from row 2 since row 1 has headers


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
		
	
	
	* For each model
	
	local col = 2
	forvalues modnum = 1/8 {
		* Extract coefficients and standard errors
		local b_val: display %9.3f rtable`modnum'[1,`covar_num']
		local se_val: display %9.3f rtable`modnum'[2,`covar_num']
		local p_val: display %9.3f rtable`modnum'[4,`covar_num']
		
		
		local stars ""
        if `p_val' < 0.001 {
            local stars "***"
        }
        else if `p_val' < 0.01 {
            local stars "**"
        }
        else if `p_val' < 0.05 {
            local stars "*"
        }

        // Insert coefficient + stars
        putdocx table mytable(`row', `col') = ("`=strltrim("`b_val'")'`stars'"),  halign(left) linebreak
        putdocx table mytable(`row', `col') = ("`=strltrim("`se_val'")'"), append

        local col = `col' + 1
    }
	
		
    * Increment the row counter for the next covariate
    local row = `row' + 1
	
	
}



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



local row = `row' + 1
local row2 = `row' + 1

*Adding R² and adjusted R²
	
putdocx table mytable(`row',1) = ("R²")
putdocx table mytable(`row2',1) = ("R²adj")

	forvalues mod = 1/8 {		
		local col_num = `mod' + 1
		
		
		
		putdocx table mytable(`row',`col_num') = ("`=strltrim("`r2_`mod''")'")
		putdocx table mytable(`row2',`col_num') =("`=strltrim("`r2_adj_`mod''")'")
	}
	
	local row = `row' + 1

	* Adding  horizontal lines
	forvalues col_num = 1/9 {
		local row_2=`row'-2
		putdocx table mytable(1,`col_num'), border(top, "single")
		putdocx table mytable(2,`col_num'), border(bottom, "single")
		putdocx table mytable(`row_2',`col_num'), border(top, "single")
		putdocx table mytable(`row',`col_num'), border(bottom, "single")
	}




local row = `row' + 1

putdocx table mytable(`row',1) = ("Note: Robust standard errors in parentheses. P-values based on robust standard errors, with ***p<0.001, **p<0.01, *p<0.05."), colspan(9) linebreak 
 

restore

}


* ========================
**## Table SB6: Policy acceptability by eat in canteen
* ========================

qui {
	
putdocx paragraph
putdocx text ("Table SB6 Percentage of respondents who consider the meat reduction policies acceptable by eat in canteen, and by policy, and country.")

* Headers
putdocx table mytable = (6, 7), border(all, nil) width(4)  width(100%) 
putdocx table mytable(1,1) = ("") , bold
putdocx table mytable(1,2) = ("France"), colspan(2)
putdocx table mytable(1,3) = ("Italy"), colspan(2)
putdocx table mytable(1,4) = ("Latvia"), colspan(2)

putdocx table mytable(2,1) = ("")
putdocx table mytable(2,2) = ("Does not eat in canteen")
putdocx table mytable(2,3) = ("Eats in canteen")
putdocx table mytable(2,4) = ("Does not eat in canteen")
putdocx table mytable(2,5) = ("Eats in canteen")
putdocx table mytable(2,6) = ("Does not eat in canteen")
putdocx table mytable(2,7) = ("Eats in canteen")


local row = 3

foreach var in meat_tax_dummy meat_day_dummy {

    local col = 2
    foreach c in France Italy Latvia {
        preserve
        keep if country == "`c'"

        mean `var' if canteen_dummy == 0
        scalar m0 = r(table)[1,1]*100
        scalar N0 = e(N)
		

        qui mean `var' if canteen_dummy == 1
        scalar m1 = r(table)[1,1]*100
        scalar N1 = e(N)

        qui ttest `var', by(canteen_dummy)
        scalar pval = r(p)

        * Star logic
        local stars ""
        if (pval < 0.01) local stars "***"
        else if (pval < 0.05) local stars "**"
        else if (pval < 0.1) local stars "*"

        * Fill values
        local val0 = string(round(m0, 0.1)) + "%" + "`stars'"
        local val1 = string(round(m1, 0.1)) + "%" + "`stars'"

        * Store in Word table
        putdocx table mytable(`row', `col') = ("`val0'")
        local col = `col' + 1
        putdocx table mytable(`row', `col') = ("`val1'")
        local col = `col' + 1

        restore
    }

    local row = `row' + 1
}

* Add N row
putdocx table mytable(`row',1) = ("N")
local col = 2
foreach c in France Italy Latvia {
    preserve
    keep if country == "`c'"
    count if canteen_dummy == 0
    local N0 = r(N)
    count if canteen_dummy == 1
    local N1 = r(N)

    putdocx table mytable(`row', `col') = ("`N0'")
    local ++col
    putdocx table mytable(`row', `col') = ("`N1'")
    local ++col

    restore
}


putdocx table mytable(3,1) = ("Meat Tax")
putdocx table mytable(4,1) = ("Meat-free Day")

* Adding  horizontal lines
forvalues col_num = 1/7 {
	putdocx table mytable(1,`col_num'), border(top, "single")
	putdocx table mytable(2,`col_num'), border(bottom, "single")
	putdocx table mytable(6,`col_num'), border(top, "single")
}


* Footnote
putdocx table mytable(6,1) = ("T-test of difference in means between does not eat in canteen and eats in canteen; *p<0.1, **p<0.05, ***p<0.01."), colspan(7) 
}

* ========================
**## Table SB7 policy preferences oneself vs other
* ========================

qui {
preserve

* Data transformation into long format

rename meat_tax_dummy tax_1
rename meat_tax_soc_dummy tax_2
rename meat_day_dummy veg_1
rename meat_day_soc_dummy veg_2


tostring id, gen(id_str) 
gen id_country = country + "_" + id_str 
reshape long tax_ veg_, i(id_country) j(policy)

gen self = cond(policy==1,1,0)
gen soc = cond(policy==2,1,0)


gen policy_nom = "."
replace policy_nom = "self" if self == 1


destring  tax_ veg_, force replace //into numeric



label variable self "Acceptability for oneself (vs. for society)"


* Table creation
	

*Add title
putdocx paragraph
putdocx text ("Table SB7. LPM estimation results of policy acceptability by perspective - all samples."), font("", 11)	

*Create a table with 10 rows and 9 columns
putdocx table mytable = (12, 9), border(all, nil) width(4)  width(100%) 

// Define column headers
putdocx table mytable(1,1) = ("") 
putdocx table mytable(1,2) = ("Pooled"), colspan(2) 
putdocx table mytable(1,3) = ("France"), colspan(2) 
putdocx table mytable(1,4) = ("Italy"), colspan(2) 
putdocx table mytable(1,5) = ("Latvia"), colspan(2) 

putdocx table mytable(2,1) = ("") 
putdocx table mytable(2,2) = ("Meat tax acceptability") 
putdocx table mytable(2,3) = ("Meat-free day acceptability") 
putdocx table mytable(2,4) = ("Meat tax acceptability") 
putdocx table mytable(2,5) = ("Meat-free day acceptability") 
putdocx table mytable(2,6) = ("Meat tax acceptability") 
putdocx table mytable(2,7) = ("Meat-free day acceptability") 
putdocx table mytable(2,8) = ("Meat tax acceptability") 
putdocx table mytable(2,9) = ("Meat-free day acceptability") 


// Run regressions and store results
regress tax_ 1.self 1.HT 1.self#1.HT FR_dummy LV_dummy, vce(hc3)
matrix rtable1 = r(table)
scalar nobs1 = e(N)
local r2_1 =  round(e(r2),0.001)
local r2_adj_1 = string(e(r2_a), "%9.3f")
regress veg_ 1.self 1.HT 1.self#1.HT FR_dummy LV_dummy, vce(hc3)
matrix rtable2 = r(table)
scalar nobs2 = e(N)
local r2_2 = string(e(r2), "%9.3f")
local r2_adj_2 = string(e(r2_a), "%9.3f")
regress tax_ 1.self 1.HT 1.self#1.HT if IT_dummy == 1, vce(hc3)
matrix rtable3 = r(table)
scalar nobs3 = e(N)
local r2_3 = string(e(r2), "%9.3f")
local r2_adj_3 = string(e(r2_a), "%9.3f")
regress veg_ 1.self 1.HT 1.self#1.HT if IT_dummy == 1, vce(hc3)
matrix rtable4 = r(table)
scalar nobs4 = e(N)
local r2_4 = string(e(r2), "%9.3f")
local r2_adj_4 = string(e(r2_a), "%9.3f")
regress tax_ 1.self 1.HT 1.self#1.HT if FR_dummy == 1, vce(hc3)
matrix rtable5 = r(table)
scalar nobs5 = e(N)
local r2_5 = string(e(r2), "%9.3f")
local r2_adj_5 = string(e(r2_a), "%9.3f")
regress veg_ 1.self 1.HT 1.self#1.HT if FR_dummy == 1, vce(hc3)
matrix rtable6 = r(table)
scalar nobs6 = e(N)
local r2_6 = string(e(r2), "%9.3f")
local r2_adj_6 = string(e(r2_a), "%9.3f")
regress tax_ 1.self 1.HT 1.self#1.HT if LV_dummy == 1, vce(hc3)
matrix rtable7 = r(table)
scalar nobs7 = e(N)
local r2_7 = string(e(r2), "%9.3f")
local r2_adj_7 = string(e(r2_a), "%9.3f")
regress veg_ 1.self 1.HT 1.self#1.HT if LV_dummy == 1, vce(hc3)
matrix rtable8 = r(table)
scalar nobs8 = e(N)
local r2_8 = string(e(r2), "%9.3f")
local r2_adj_8 = string(e(r2_a), "%9.3f")


local covariates HT self "1.HT#1.self" FR_dummy LV_dummy _cons
local interaction_label "Health-risk information # Acceptability for oneself (vs. for society)"

local nvars : word count `covariates'
local row 3  // Start from row 2 since row 1 has headers



forvalues covar_num = 1/`nvars' {
local covar : word `covar_num' of `covariates'
	if "`covar'"== "_cons"  {
		local covar_label Constant
		putdocx table mytable(`row',1) = ("`covar_label'")
		}	
		else {
			if "`covar'" == "1.HT#1.self"  {
			local covar_label "`interaction_label'"
			putdocx table mytable(`row',1) = ("`covar_label'"), italic
			}
			else {
				local covar_label : variable label `covar'
				putdocx table mytable(`row',1) = ("`covar_label'"), italic
				}
			}
			
		local row = `row' + 1
		
}

* Pooled sample

local covariates HT self "1.HT#1.self" FR_dummy LV_dummy _cons
local nvars : word count `covariates'
local row 3

forvalues covar_num = 1/`nvars' {
	* For each model
	local col = 2
	forvalues modnum = 1/2 {
		* Extract coefficients and standard errors
		local b_val: display %9.3f rtable`modnum'[1,`covar_num']
		local se_val: display %9.3f rtable`modnum'[2,`covar_num']
		local p_val: display %9.3f rtable`modnum'[4,`covar_num']
				
		local stars ""
        if `p_val' < 0.001 {
            local stars "***"
        }
        else if `p_val' < 0.01 {
            local stars "**"
        }
        else if `p_val' < 0.05 {
            local stars "*"
        }

        // Insert coefficient + stars
        putdocx table mytable(`row', `col') = ("`=strltrim("`b_val'")'`stars'"),  halign(left) linebreak
        putdocx table mytable(`row', `col') = ("(`=strltrim("`se_val'")')"), append

        local col = `col' + 1
    }
	
		
    * Increment the row counter for the next covariate
    local row = `row' + 1
	
}

* country samples


local covariates HT self "1.HT#1.self" _cons
local nvars_c : word count `covariates'
local row 3  // Start from row 2 since row 1 has headers

forvalues covar_num_c = 1/`nvars_c' {	
local covar : word `covar_num_c' of `covariates'
	* For each model
	local col = 4
	forvalues modnum = 3/8 {
		* Extract coefficients and standard errors
		local b_val: display %9.3f rtable`modnum'[1,`covar_num_c']
		local se_val: display %9.3f rtable`modnum'[2,`covar_num_c']
		local p_val: display %9.3f rtable`modnum'[4,`covar_num_c']
		
		
		local stars ""
        if `p_val' < 0.001 {
            local stars "***"
        }
        else if `p_val' < 0.01 {
            local stars "**"
        }
        else if `p_val' < 0.05 {
            local stars "*"
        }

        // Insert coefficient + stars
        putdocx table mytable(`row', `col') = ("`=strltrim("`b_val'")'`stars'"),  halign(left) linebreak
        putdocx table mytable(`row', `col') = ("`=strltrim("`se_val'")'"), append

        local col = `col' + 1
    }
	
		
    * Increment the row counter for the next covariate
	*same number rows as pooled sample
		if "`covar'" == "1.HT#1.self"  {
			local row = `row' + 3
		}
		else {
			local row = `row' + 1
		}
	
	
}


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



local row = `row' + 1
local row2 = `row' + 1

*Adding R² and adjusted R²
	
putdocx table mytable(`row',1) = ("R²")
putdocx table mytable(`row2',1) = ("R²adj")

	forvalues mod = 1/8 {		
		local col_num = `mod' + 1
		
		
		
		putdocx table mytable(`row',`col_num') = ("`=strltrim("`r2_`mod''")'")
		putdocx table mytable(`row2',`col_num') =("`=strltrim("`r2_adj_`mod''")'")
	}
	
	local row = `row' + 1

	* Adding  horizontal lines
	forvalues col_num = 1/9 {
		local row_2=`row'-2
		putdocx table mytable(1,`col_num'), border(top, "single")
		putdocx table mytable(2,`col_num'), border(bottom, "single")
		putdocx table mytable(`row_2',`col_num'), border(top, "single")
		putdocx table mytable(`row',`col_num'), border(bottom, "single")
	}




local row = `row' + 1

putdocx table mytable(`row',1) = ("Note: Robust standard errors in parentheses. P-values based on robust standard errors, with ***p<0.001, **p<0.01, *p<0.05."), colspan(9)
 

restore


}



// Save the document
putdocx save "Supplementary materials/Supplementary material SB2.docx", replace




* ========================
**# Supplementary material SB.3: Results of main models
* ========================

label variable vegan_veg_pesc_dummy "Meat-free diet (vs. varied and high-meat diets)"
label variable flex_dummy "Low-meat diet (vs. varied and high-meat diets)"
label variable high_meat_dummy "High-meat diet (vs. low and varied-meat diets)"

local table_num 8 // Start from table 7


* ========================
**## Table SB8 and SB9 Basic models
* ========================

putdocx clear
putdocx begin, font("", 10) landscape
		
qui {
	
regress meat_tax_dummy HT FR_dummy LV_dummy, vce(hc3)
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

regress meat_day_s HT FR_dummy LV_dummy, vce(hc3)
matrix rtable4=r(table)
scalar nobs4 = e(N)
local r2_4 = string(e(r2), "%9.3f")
local r2_adj_4 = string(e(r2_a), "%9.3f")



local file_path = "main_results/main_model.xlsx"
    
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



*Add title
putdocx paragraph
putdocx text ("Table SB`table_num' LPM results of the basic models - pooled sample."), font("", 11)	

*Create a table with 5 rows and 5 columns (1 for covariate names, 4 for models)
putdocx table mytable = (9, 5), border(all, nil) width(4)  width(100%) 

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
local table_num  = `table_num' + 1

putdocx table mytable(`row',1) = ("Note: Robust standard errors in parentheses."), colspan(5) linebreak 
putdocx table mytable(`row',1) = ("For health-risk information, Westfall-Young MHT adjusted p-values in brackets with 10,000 bootstrap replications. "),  append linebreak 
putdocx table mytable(`row',1) = ("***p<0.001, **p<0.01, *p<0.05 for standard p-values, +++p< 0.001, ++p < 0.01, +p < 0.05 for Westfall-Young MHT adjusted p-values."),  append


}



putdocx pagebreak

qui {


preserve

keep if country == "France"

regress meat_tax_dummy HT , vce(hc3)
matrix rtable1 = r(table)
scalar nobs1 = e(N)
local r2_1 = string(e(r2), "%9.3f")
local r2_adj_1 = string(e(r2_a), "%9.3f")

regress meat_tax_s HT , vce(hc3)
matrix rtable2 = r(table)
scalar nobs2 = e(N)
local r2_2 = string(e(r2), "%9.3f")
local r2_adj_2 = string(e(r2_a), "%9.3f")

regress meat_day_dummy HT , vce(hc3)
matrix rtable3 = r(table)
scalar nobs3 = e(N)
local r2_3 = string(e(r2), "%9.3f")
local r2_adj_3 = string(e(r2_a), "%9.3f")

regress meat_day_s HT , vce(hc3)
matrix rtable4 = r(table)
scalar nobs4 = e(N)
local r2_4 = string(e(r2), "%9.3f")
local r2_adj_4 = string(e(r2_a), "%9.3f")

restore



preserve

keep if country == "Italy"

regress meat_tax_dummy HT , vce(hc3)
matrix rtable5= r(table)
scalar nobs5 = e(N)
local r2_5 = string(e(r2), "%9.3f")
local r2_adj_5 = string(e(r2_a), "%9.3f")

regress meat_tax_s HT , vce(hc3)
matrix rtable6 = r(table)
scalar nobs6 = e(N)
local r2_6 = string(e(r2), "%9.3f")
local r2_adj_6 = string(e(r2_a), "%9.3f")

regress meat_day_dummy HT , vce(hc3)
matrix rtable7 = r(table)
scalar nobs7 = e(N)
local r2_7 = string(e(r2), "%9.3f")
local r2_adj_7 = string(e(r2_a), "%9.3f")

regress meat_day_s HT , vce(hc3)
matrix rtable8 = r(table)
scalar nobs8 = e(N)
local r2_8 = string(e(r2), "%9.3f")
local r2_adj_8 = string(e(r2_a), "%9.3f")
restore


preserve

keep if country == "Latvia"

regress meat_tax_dummy HT , vce(hc3)
matrix rtable9 = r(table)
scalar nobs9 = e(N)
local r2_9 = string(e(r2), "%9.3f")
local r2_adj_9 = string(e(r2_a), "%9.3f")

regress meat_tax_s HT , vce(hc3)
matrix rtable10 = r(table)
scalar nobs10 = e(N)
local r2_10 = string(e(r2), "%9.3f")
local r2_adj_10 = string(e(r2_a), "%9.3f")

regress meat_day_dummy HT , vce(hc3)
matrix rtable11 = r(table)
scalar nobs11 = e(N)
local r2_11 = string(e(r2), "%9.3f")
local r2_adj_11 = string(e(r2_a), "%9.3f")

regress meat_day_s HT , vce(hc3)
matrix rtable12 = r(table)
scalar nobs12 = e(N)
local r2_12 = string(e(r2), "%9.3f")
local r2_adj_12 = string(e(r2_a), "%9.3f")

restore	



local file_path = "main_results/main_model_France.xlsx"
    
if !fileexists("`file_path'") {
	preserve
	keep if country == "France"
	wyoung, cmd("regress meat_tax_dummy HT , vce(hc3)" ///
	"regress meat_tax_s HT , vce(hc3)" ///
	"regress meat_day_dummy HT , vce(hc3)" ///
	"regress meat_day_s HT , vce(hc3)") ///
	familyp("HT" "HT" "HT" "HT") familypexp bootstraps(10000) seed(1234)
    
	matrix wy_results1 = r(table)
	putexcel set `file_path', replace
    putexcel A1=matrix(wy_results1), names
	
	restore 
}
		
preserve

import excel `file_path', sheet("Sheet1") firstrow clear
mkmat coef stderr  p pwyoung pbonf psidak, matrix(wy_results1)


restore


local file_path = "main_results/main_model_Italy.xlsx"

if !fileexists("`file_path'") {
	preserve
	keep if country == "Italy"
	wyoung, cmd("regress meat_tax_dummy HT , vce(hc3)" ///
	"regress meat_tax_s HT , vce(hc3)" ///
	"regress meat_day_dummy HT , vce(hc3)" ///
	"regress meat_day_s HT , vce(hc3)") ///
	familyp("HT" "HT" "HT" "HT") familypexp bootstraps(10000) seed(1234)
    
	matrix wy_results2 = r(table)
	putexcel set `file_path', replace
    putexcel A1=matrix(wy_results), names
	restore
	}
		
preserve

import excel `file_path', sheet("Sheet1") firstrow clear

mkmat coef stderr  p pwyoung pbonf psidak, matrix(wy_results2)


restore	


local file_path = "main_results/main_model_Latvia.xlsx"

if !fileexists("`file_path'") {
	preserve
	keep if country == "Latvia"
	wyoung, cmd("regress meat_tax_dummy HT , vce(hc3)" ///
	"regress meat_tax_s HT , vce(hc3)" ///
	"regress meat_day_dummy HT , vce(hc3)" ///
	"regress meat_day_s HT , vce(hc3)") ///
	familyp("HT" "HT" "HT" "HT") familypexp bootstraps(10000) seed(1234)
    
	matrix wy_results3 = r(table)
	putexcel set `file_path', replace
    putexcel A1=matrix(wy_results3), names
	restore
}
		
preserve

import excel `file_path', sheet("Sheet1") firstrow clear

mkmat coef stderr  p pwyoung pbonf psidak, matrix(wy_results3)


restore



*Add title
putdocx paragraph
putdocx text ("Table SB`table_num' LPM results for the basic model by country"), font("", 11)


putdocx table mytable = (8, 13), border(all, nil) width(4) 
* Create a table with 5 rows and 5 columns (1 for covariate names, 4 for models)
putdocx table mytable(1,2) = ("France"), colspan(4)
putdocx table mytable(1,3) = ("Italy"), colspan(4)
putdocx table mytable(1,4) = ("Latvia"), colspan(4)
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



local covariates HT _cons

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



* Insert coefficients, standard errors, and stars for each model

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

}

putdocx pagebreak

* ========================
**## Table SB10 Country affiliation heterogeneity models 
* ========================

qui{

qui regress meat_tax_dummy 1.HT FR_dummy LV_dummy 1.HT#1.FR_dummy 1.HT#1.LV_dummy, vce(hc3) 
matrix rtable1=r(table)
scalar nobs1 = e(N)
local r2_1 = string(e(r2), "%9.3f")
local r2_adj_1 = string(e(r2_a), "%9.3f")

qui regress meat_tax_s 1.HT FR_dummy LV_dummy 1.HT#1.FR_dummy 1.HT#1.LV_dummy, vce(hc3) 
matrix rtable2=r(table)
scalar nobs2 = e(N)
local r2_2 = string(e(r2), "%9.3f")
local r2_adj_2 = string(e(r2_a), "%9.3f")

qui regress meat_day_dummy 1.HT FR_dummy LV_dummy 1.HT#1.FR_dummy 1.HT#1.LV_dummy, vce(hc3) 
matrix rtable3=r(table)
scalar nobs3 = e(N)
local r2_3 = string(e(r2), "%9.3f")
local r2_adj_3 = string(e(r2_a), "%9.3f")

qui regress meat_day_s 1.HT FR_dummy LV_dummy 1.HT#1.FR_dummy 1.HT#1.LV_dummy, vce(hc3) 
matrix rtable4=r(table)
scalar nobs4 = e(N)
local r2_4 = string(e(r2), "%9.3f")
local r2_adj_4 = string(e(r2_a), "%9.3f")


local file_path = "main_results/country_comp_MHT_all.xlsx"
    
if !fileexists("`file_path'") {
	
local file_path = "main_results/country_comp_MHT_all.xlsx"
	wyoung, cmd("regress meat_tax_dummy  HT FR_dummy LV_dummy 1.HT#1.FR_dummy 1.HT#1.LV_dummy, vce(hc3) "  ///
			"regress  meat_tax_s  HT FR_dummy LV_dummy 1.HT#1.FR_dummy 1.HT#1.LV_dummy, vce(hc3) "  ///
			"regress meat_day_dummy  HT FR_dummy LV_dummy 1.HT#1.FR_dummy 1.HT#1.LV_dummy, vce(hc3) "  ///
			"regress  meat_day_s  HT FR_dummy LV_dummy 1.HT#1.FR_dummy 1.HT#1.LV_dummy, vce(hc3) "  ///
			"regress meat_tax_dummy  HT FR_dummy LV_dummy 1.HT#1.FR_dummy 1.HT#1.LV_dummy, vce(hc3) "  ///
			"regress  meat_tax_s  HT FR_dummy LV_dummy 1.HT#1.FR_dummy 1.HT#1.LV_dummy, vce(hc3) "  ///
			"regress meat_day_dummy  HT FR_dummy LV_dummy 1.HT#1.FR_dummy 1.HT#1.LV_dummy, vce(hc3) "  ///
			"regress  meat_day_s  HT FR_dummy LV_dummy 1.HT#1.FR_dummy 1.HT#1.LV_dummy, vce(hc3) "  ///
			"regress meat_tax_dummy  HT FR_dummy LV_dummy 1.HT#1.FR_dummy 1.HT#1.LV_dummy, vce(hc3) "  ///
			"regress  meat_tax_s  HT FR_dummy LV_dummy 1.HT#1.FR_dummy 1.HT#1.LV_dummy, vce(hc3) "  ///
			"regress meat_day_dummy  HT FR_dummy LV_dummy 1.HT#1.FR_dummy 1.HT#1.LV_dummy, vce(hc3) "  ///
			"regress  meat_day_s  HT FR_dummy LV_dummy 1.HT#1.FR_dummy 1.HT#1.LV_dummy, vce(hc3) "  )  ///
			familyp("1.HT#1.FR_dummy" "1.HT#1.FR_dummy" "1.HT#1.FR_dummy" "1.HT#1.FR_dummy" ///
					"1.HT#1.LV_dummy" "1.HT#1.LV_dummy" "1.HT#1.LV_dummy" "1.HT#1.LV_dummy" ///
					"1.HT#1.FR_dummy-1.HT#1.LV_dummy" "1.HT#1.FR_dummy-1.HT#1.LV_dummy" "1.HT#1.FR_dummy-1.HT#1.LV_dummy" "1.HT#1.FR_dummy-1.HT#1.LV_dummy")  ///
			familypexp strata(country) bootstraps(10000) seed(1234)
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
putdocx text ("Table SB`table_num' LPM results of the country affiliation heterogeneity models - pooled sample."), font("", 11)	

*Create a table with 5 rows and 5 columns (1 for covariate names, 4 for models)
putdocx table mytable = (11, 5), border(all, nil) width(4)  width(100%) 

*headers for each model
putdocx table mytable(1,1) = ("")
putdocx table mytable(1,2) = ("Meat tax acceptability for oneself")
putdocx table mytable(1,3) = ("Meat Tax acceptability for society")
putdocx table mytable(1,4) = ("Meat-free days acceptability for oneself")
putdocx table mytable(1,5) = ("Meat-free days acceptability for society")




local covariates  HT FR_dummy LV_dummy "1.HT#1.FR_dummy" "1.HT#1.LV_dummy" _cons
local nvars : word count `covariates'
local interaction_label_FR "Health-risk information # France"
local interaction_label_LV "Health-risk information # Latvia"
local row 2  // Start from row 2 since row 1 has headers
local rowse 3

forvalues covar_num = 1/`nvars' {
	local covar : word `covar_num' of `covariates'
	if "`covar'" == "_cons" {
		local covar_label Constant
		putdocx table mytable(`row',1) = ("`covar_label'")
		}
		else {
			if "`covar'" == "1.HT#1.FR_dummy" {
			local covar_label "`interaction_label_FR'"
			putdocx table mytable(`row',1) = ("`covar_label'"), italic
			}
			else {
			if "`covar'" == "1.HT#1.LV_dummy" {
			local covar_label "`interaction_label_LV'"
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

	local table_num  = `table_num' + 1


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

}
putdocx pagebreak
* ========================
**## Table SB11 to SB34 Heterogeneity models individual characteristics
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
	
	


	local nb 10000 //number of bootsraps

    local file_path = "heterogeneity_results/heter_res_`var1'.xlsx"
    
    // Check if the file exists
    if !fileexists("`file_path'") {
    * Westfall-Young adjusted p-values for variable 1
    wyoung, cmd("regress meat_tax_dummy 1.HT `var1' 1.HT#1.`var1' FR_dummy LV_dummy, vce(hc3)" ///
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
		
		
    local file_path = "heterogeneity_results/heter_res_`var2'.xlsx"
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
	
    local file_path = "heterogeneity_results/heter_res_`var3'.xlsx"
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
	
		
	local lbl1 : variable label `var1'
	local lbl2 : variable label `var2'
	local lbl3 : variable label `var3'	
	
		
	local low_lbl1 = lower("`lbl1'")
	local low_lbl2 = lower("`lbl2'")
	local low_lbl3 = lower("`lbl3'")
		
	*Add title
	putdocx paragraph
	putdocx text ("Table SB`table_num' LPM results of the heterogeneity models for ")
	putdocx text ("`low_lbl1'"), italic
	putdocx text (", ")
	putdocx text ("`low_lbl2'"), italic
	putdocx text (", and ")
	putdocx text ("`low_lbl3'"), italic
	putdocx text (" - pooled sample."), font("", 11)
		
	


    * Create a table with 5 rows and 5 columns (1 for covariate names, 4 for models)
    putdocx table mytable = (12, 13), border(all, nil) width(4)  width(100%) 
	
	* Add headers
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





local country France Italy Latvia

foreach cntry of local country {
	
preserve

keep if country == "`cntry'"
	
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
    qui regress meat_tax_dummy 1.HT `var1' 1.HT#1.`var1' , vce(hc3)
    matrix rtable1 = r(table)
    scalar nobs1 = e(N)
	local r2_1 = string(e(r2), "%9.3f")
	local r2_adj_1 = string(e(r2_a), "%9.3f")
    qui regress meat_tax_s 1.HT `var1' 1.HT#1.`var1' , vce(hc3)
    matrix rtable2 = r(table)
    scalar nobs2 = e(N)
	local r2_2 = string(e(r2), "%9.3f")
	local r2_adj_2 = string(e(r2_a), "%9.3f")
    qui regress meat_day_dummy 1.HT `var1' 1.HT#1.`var1' , vce(hc3)
    matrix rtable3 = r(table)
    scalar nobs3 = e(N)
	local r2_3 = string(e(r2), "%9.3f")
	local r2_adj_3 = string(e(r2_a), "%9.3f")
    qui regress meat_day_s 1.HT `var1' 1.HT#1.`var1' , vce(hc3)
    matrix rtable4 = r(table)
    scalar nobs4 = e(N)
	local r2_4 = string(e(r2), "%9.3f")
	local r2_adj_4 = string(e(r2_a), "%9.3f")
	* coeff and N for var2
	qui regress meat_tax_dummy 1.HT `var2' 1.HT#1.`var2' , vce(hc3)
    matrix rtable5 = r(table)
    scalar nobs5 = e(N)
	local r2_5 = string(e(r2), "%9.3f")
	local r2_adj_5 = string(e(r2_a), "%9.3f")
    qui regress meat_tax_s 1.HT `var2' 1.HT#1.`var2' , vce(hc3)
    matrix rtable6 = r(table)
    scalar nobs6 = e(N)
	local r2_6 = string(e(r2), "%9.3f")
	local r2_adj_6 = string(e(r2_a), "%9.3f")
    qui regress meat_day_dummy 1.HT `var2' 1.HT#1.`var2' , vce(hc3)
    matrix rtable7 = r(table)
    scalar nobs7 = e(N)
	local r2_7 = string(e(r2), "%9.3f")
	local r2_adj_7 = string(e(r2_a), "%9.3f")
    qui regress meat_day_s 1.HT `var2' 1.HT#1.`var2' , vce(hc3)
    matrix rtable8 = r(table)
    scalar nobs8 = e(N)
	local r2_8 = string(e(r2), "%9.3f")
	local r2_adj_8 = string(e(r2_a), "%9.3f")
	* coeff and N for var3	
	qui regress meat_tax_dummy 1.HT `var3' 1.HT#1.`var3' , vce(hc3)
    matrix rtable9 = r(table)
    scalar nobs9 = e(N)
	local r2_9 = string(e(r2), "%9.3f")
	local r2_adj_9 = string(e(r2_a), "%9.3f")
    qui regress meat_tax_s 1.HT `var3' 1.HT#1.`var3' , vce(hc3)
    matrix rtable10 = r(table)
    scalar nobs10 = e(N)
	local r2_10 = string(e(r2), "%9.3f")
	local r2_adj_10 = string(e(r2_a), "%9.3f")
    qui regress meat_day_dummy 1.HT `var3' 1.HT#1.`var3' , vce(hc3)
    matrix rtable11 = r(table)
    scalar nobs11 = e(N)
	local r2_11 = string(e(r2), "%9.3f")
	local r2_adj_11 = string(e(r2_a), "%9.3f")
    qui regress meat_day_s 1.HT `var3' 1.HT#1.`var3' , vce(hc3)
    matrix rtable12 = r(table)
    scalar nobs12 = e(N)
	local r2_12 = string(e(r2), "%9.3f")
	local r2_adj_12 = string(e(r2_a), "%9.3f")
	

	restore

	local nb 10000 //number of bootsraps

    local file_path = "heterogeneity_results/heter_res_`var1'_`cntry'.xlsx"
    
    // Check if the file exists
    if !fileexists("`file_path'") {
    * Westfall-Young adjusted p-values for variable 1
    qui wyoung, cmd("regress meat_tax_dummy 1.HT `var1' 1.HT#1.`var1' , vce(hc3)" ///
        "regress meat_tax_s 1.HT `var1' 1.HT#1.`var1' , vce(hc3)" ///
        "regress meat_day_dummy 1.HT `var1' 1.HT#1.`var1' , vce(hc3)" ///
        "regress meat_day_s 1.HT `var1' 1.HT#1.`var1' , vce(hc3)"  ///
		"regress meat_tax_dummy 1.HT `var1' 1.HT#1.`var1' , vce(hc3)" ///
        "regress meat_tax_s 1.HT `var1' 1.HT#1.`var1' , vce(hc3)" ///
        "regress meat_day_dummy 1.HT `var1' 1.HT#1.`var1' , vce(hc3)" ///
        "regress meat_day_s 1.HT `var1' 1.HT#1.`var1' , vce(hc3)" ) ///
        familyp(1.HT 1.HT 1.HT 1.HT 1.HT+1.HT#1.`var1' 1.HT+1.HT#1.`var1' 1.HT+1.HT#1.`var1' 1.HT+1.HT#1.`var1') ///
        familypexp///
        bootstraps(`nb') seed(1234)
    
	matrix wy_results1 = r(table)
	putexcel set `file_path', replace
    putexcel A1=matrix(wy_results1), names
	
	}
		
	preserve
	
	import excel `file_path', sheet("Sheet1") firstrow clear
	mkmat coef stderr  p pwyoung pbonf psidak, matrix(wy_results1)
	

	restore
		
		
    local file_path = "heterogeneity_results/heter_res_`var2'_`cntry'.xlsx"
	    * Westfall-Young adjusted p-values for variable 2
        if !fileexists("`file_path'") {
			qui wyoung, cmd("regress meat_tax_dummy 1.HT `var2' 1.HT#1.`var2' , vce(hc3)" ///
        "regress meat_tax_s 1.HT `var2' 1.HT#1.`var2' , vce(hc3)" ///
        "regress meat_day_dummy 1.HT `var2' 1.HT#1.`var2' , vce(hc3)" ///
        "regress meat_day_s 1.HT `var2' 1.HT#1.`var2' , vce(hc3)"  ///
		"regress meat_tax_dummy 1.HT `var2' 1.HT#1.`var2' , vce(hc3)" ///
        "regress meat_tax_s 1.HT `var2' 1.HT#1.`var2' , vce(hc3)" ///
        "regress meat_day_dummy 1.HT `var2' 1.HT#1.`var2' , vce(hc3)" ///
        "regress meat_day_s 1.HT `var2' 1.HT#1.`var2' , vce(hc3)" ) ///
        familyp(1.HT 1.HT 1.HT 1.HT 1.HT+1.HT#1.`var2' 1.HT+1.HT#1.`var2' 1.HT+1.HT#1.`var2' 1.HT+1.HT#1.`var2') ///
        familypexp  ///
        bootstraps(`nb') seed(1234)
    
	matrix wy_results2 = r(table)	
	putexcel set `file_path', replace
    putexcel A1=matrix(wy_results2), names
		}
	
	preserve
	
	import excel `file_path', sheet("Sheet1") firstrow clear
	mkmat coef stderr  p pwyoung pbonf psidak, matrix(wy_results2)


	restore
	
    local file_path = "heterogeneity_results/heter_res_`var3'_`cntry'.xlsx"
		* Westfall-Young adjusted p-values for variable 3
    if !fileexists("`file_path'") {
		qui wyoung, cmd("regress meat_tax_dummy 1.HT `var3' 1.HT#1.`var3' ,vce(hc3)" ///
        "regress meat_tax_s 1.HT `var3' 1.HT#1.`var3' , vce(hc3)" ///
        "regress meat_day_dummy 1.HT `var3' 1.HT#1.`var3' , vce(hc3)" ///
        "regress meat_day_s 1.HT `var3' 1.HT#1.`var3' , vce(hc3)"  ///
		"regress meat_tax_dummy 1.HT `var3' 1.HT#1.`var3' , vce(hc3)" ///
        "regress meat_tax_s 1.HT `var3' 1.HT#1.`var3' , vce(hc3)" ///
        "regress meat_day_dummy 1.HT `var3' 1.HT#1.`var3' , vce(hc3)" ///
        "regress meat_day_s 1.HT `var3' 1.HT#1.`var3' , vce(hc3)" ) ///
        familyp(1.HT 1.HT 1.HT 1.HT 1.HT+1.HT#1.`var3' 1.HT+1.HT#1.`var3' 1.HT+1.HT#1.`var3' 1.HT+1.HT#1.`var3') ///
        familypexp ///
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

keep if country == "`cntry'"

		
	local lbl1 : variable label `var1'
	local lbl2 : variable label `var2'
	local lbl3 : variable label `var3'	
	
	
	local low_lbl1 = lower("`lbl1'")
	local low_lbl2 = lower("`lbl2'")
	local low_lbl3 = lower("`lbl3'")
	
    * Add title
    putdocx paragraph
	putdocx text ("Table SB`table_num' LPM results of the heterogeneity models for ")
	putdocx text ("`low_lbl1'"), italic
	putdocx text (", ")
	putdocx text ("`low_lbl2'"), italic
	putdocx text (", and ")
	putdocx text ("`low_lbl3'"), italic
	putdocx text (" - `cntry' only."), font("", 11)
	
    putdocx table mytable = (10, 13), border(all, nil) width(4)  width(100%) 
	


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


	
    local covariates HT "`var1'" "1.HT#1.`var1'"  _cons
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
	


		* Insert coefficients, standard errors, and stars for each model
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

restore
}
}


* ========================
**## Table SB35 and SB36 Comprehensive models
* ========================

global rhs_tax  health_low food_depriv unemployed educ_low age female  income_pp_1000 vegan_veg_pesc_dummy flex_dummy po1_dummy po2_dummy po3_dummy po4_dummy po5_dummy diet_knowl_high cc_acc_high soc_norm_high
global rhs_veg  health_low food_depriv unemployed educ_low age female  income_pp_1000 canteen_dummy vegan_veg_pesc_dummy flex_dummy po1_dummy po2_dummy po3_dummy po4_dummy po5_dummy  diet_knowl_high cc_acc_high soc_norm_high


qui {
regress meat_tax_dummy  HT cost_tax_self_dummy eff_tax_self_dummy fair_tax_self_dummy  $rhs_tax FR_dummy LV_dummy, vce(hc3)
matrix rtable1=r(table)
scalar nobs1 = e(N)
local r2_1 = string(e(r2), "%9.3f")
local r2_adj_1 = string(e(r2_a), "%9.3f")

regress meat_tax_s HT cost_tax_soc_dummy eff_tax_soc_dummy fair_tax_soc_dummy  $rhs_tax FR_dummy LV_dummy, vce(hc3)
matrix rtable2=r(table)
scalar nobs2 = e(N)
local r2_2 = string(e(r2), "%9.3f")
local r2_adj_2 = string(e(r2_a), "%9.3f")


regress meat_day_dummy HT cost_veggie_self_dummy eff_veggie_self_dummy fair_veggie_self_dummy  $rhs_veg FR_dummy LV_dummy, vce(hc3)
matrix rtable3=r(table)
scalar nobs3 = e(N)
local r2_3 = string(e(r2), "%9.3f")
local r2_adj_3 = string(e(r2_a), "%9.3f")

regress meat_day_s HT cost_veggie_soc_dummy eff_veggie_soc_dummy fair_veggie_soc_dummy  $rhs_veg FR_dummy LV_dummy, vce(hc3)
matrix rtable4=r(table)
scalar nobs4 = e(N)
local r2_4 = string(e(r2), "%9.3f")
local r2_adj_4 = string(e(r2_a), "%9.3f")


local file_path = "main_results/comp_res.xlsx"
    
    // Check if the file exists
    if !fileexists("`file_path'") {
wyoung, cmd("regress meat_tax_dummy  HT cost_tax_self_dummy eff_tax_self_dummy fair_tax_self_dummy  $rhs_tax FR_dummy LV_dummy , vce(hc3) " ///
"regress  meat_tax_s HT cost_tax_soc_dummy eff_tax_soc_dummy fair_tax_soc_dummy  $rhs_tax FR_dummy LV_dummy , vce(hc3) "  ///
"regress meat_day_dummy  HT cost_veggie_self_dummy eff_veggie_self_dummy fair_veggie_self_dummy   $rhs_veg FR_dummy LV_dummy, vce(hc3) "  ///
"regress  meat_day_s HT cost_veggie_soc_dummy eff_veggie_soc_dummy fair_veggie_soc_dummy  $rhs_veg FR_dummy LV_dummy , vce(hc3) ") familyp("HT" "HT" "HT" "HT" ) familypexp  strata(country) bootstraps(10000) 	
		matrix results = r(table)
		putexcel set main_results/comp_res.xlsx, replace
		// putexcel A1="`var'"
		putexcel A1=matrix(results), names
	}
	
	preserve
	
	import excel `file_path', sheet("Sheet1") firstrow clear
	mkmat coef stderr  p pwyoung pbonf psidak, matrix(wy_results)

	restore


*Add title
putdocx paragraph
putdocx text ("Table SB`table_num' LPM estimation results for the comprehensive model - pooled sample."), font("", 11)

*Create a table with 5 rows and 5 columns (1 for covariate names, 4 for models)
putdocx table mytable = (30, 5), border(all, nil) width(4) 

*headers for each model
putdocx table mytable(1,1) = ("")
putdocx table mytable(1,2) = ("Meat tax acceptability for oneself")
putdocx table mytable(1,3) = ("Meat Tax acceptability for society")
putdocx table mytable(1,4) = ("Meat-free days acceptability for oneself")
putdocx table mytable(1,5) = ("Meat-free days acceptability for society")



local covariates HT cost_tax_self_dummy eff_tax_self_dummy fair_tax_self_dummy  $rhs_veg FR_dummy LV_dummy _cons
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
		if "`covar'" == "cost_tax_self_dummy" | "`covar'" == "cost_tax_soc_dummy" | "`covar'" == "cost_veggie_self_dummy" | "`covar'" == "veggie_tax_soc_dummy" {
		local covar_label Expense
		putdocx table mytable(`row',1) = ("`covar_label'"), italic
		}
		else {	
			if "`covar'" == "eff_tax_self_dummy" | "`covar'" == "eff_tax_soc_dummy" | "`covar'" == "eff_veggie_self_dummy" | "`covar'" == "veggie_tax_soc_dummy" {
			local covar_label Effectiveness
			putdocx table mytable(`row',1) = ("`covar_label'"), italic
			}
			else {	
				if "`covar'" == "fair_tax_self_dummy" | "`covar'" == "fair_tax_soc_dummy" | "`covar'" == "fair_veggie_self_dummy" | "`covar'" == "veggie_tax_soc_dummy" {
				local covar_label Fairness
				putdocx table mytable(`row',1) = ("`covar_label'"), italic
				}
				
				else {
					 local covar_label : variable label `covar'
					 putdocx table mytable(`row',1) = ("`covar_label'"), italic
					 }
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


	


			* Insert coefficients, standard errors, and stars for each model
			*Taking into account that dine in canteen is only in meat-free days models
			if `covar_num' > 11 {
				local row2 = `row' +1
				putdocx table mytable(`row2',2) = ("`=strltrim("`b1_val'")'`stars1'`stars_adj1'"), halign(left) linebreak
				putdocx table mytable(`row2',2) = ("(`=strltrim("`se1_val'")')"), append
				putdocx table mytable(`row2',3) = ("`=strltrim("`b2_val'")'`stars2'`stars_adj2'"), halign(left) linebreak
				putdocx table mytable(`row2',3) = ("(`=strltrim("`se2_val'")')"), append
				putdocx table mytable(`row',4) = ("`=strltrim("`b3_val'")'`stars3'`stars_adj3'"), halign(left) linebreak
				putdocx table mytable(`row',4) = ("(`=strltrim("`se3_val'")')"), append 
				putdocx table mytable(`row',5) = ("`=strltrim("`b4_val'")'`stars4'`stars_adj4'"), halign(left) linebreak
				putdocx table mytable(`row',5) = ("(`=strltrim("`se4_val'")')"), append 

			}
			else if "`covar'" == "HT" {
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
local table_num  = `table_num' + 1

putdocx table mytable(`row',1) = ("Note: Robust standard errors in parentheses."), colspan(5) linebreak 
putdocx table mytable(`row',1) = ("For health-risk information, Westfall-Young MHT adjusted p-values in brackets with 10,000 bootstrap replications. "),  append linebreak 
putdocx table mytable(`row',1) = ("***p<0.001, **p<0.01, *p<0.05 for standard p-values, +++p< 0.001, ++p < 0.01, +p < 0.05 for Westfall-Young MHT adjusted p-values."),  append

}


putdocx pagebreak



preserve

keep if country == "France"

regress meat_tax_dummy  HT cost_tax_self_dummy eff_tax_self_dummy fair_tax_self_dummy  $rhs_tax , vce(hc3)
matrix rtable1 = r(table)
scalar nobs1 = e(N)
local r2_1 = string(e(r2), "%9.3f")
local r2_adj_1 = string(e(r2_a), "%9.3f")

regress meat_tax_s HT cost_tax_soc_dummy eff_tax_soc_dummy fair_tax_soc_dummy  $rhs_tax , vce(hc3)
matrix rtable2 = r(table)
scalar nobs2 = e(N)
local r2_2 = string(e(r2), "%9.3f")
local r2_adj_2 = string(e(r2_a), "%9.3f")

regress meat_day_dummy HT cost_veggie_self_dummy eff_veggie_self_dummy fair_veggie_self_dummy  $rhs_veg , vce(hc3)
matrix rtable3 = r(table)
scalar nobs3 = e(N)
local r2_3 = string(e(r2), "%9.3f")
local r2_adj_3 = string(e(r2_a), "%9.3f")

regress meat_day_s HT cost_veggie_soc_dummy eff_veggie_soc_dummy fair_veggie_soc_dummy  $rhs_veg , vce(hc3)
matrix rtable4 = r(table)
scalar nobs4 = e(N)
local r2_4 = string(e(r2), "%9.3f")
local r2_adj_4 = string(e(r2_a), "%9.3f")

restore

preserve

keep if country == "Italy"

regress meat_tax_dummy  HT cost_tax_self_dummy eff_tax_self_dummy fair_tax_self_dummy  $rhs_tax , vce(hc3)
matrix rtable5 = r(table)
scalar nobs5 = e(N)
local r2_5 = string(e(r2), "%9.3f")
local r2_adj_5 = string(e(r2_a), "%9.3f")

regress meat_tax_s HT cost_tax_soc_dummy eff_tax_soc_dummy fair_tax_soc_dummy  $rhs_tax , vce(hc3)
matrix rtable6 = r(table)
scalar nobs6 = e(N)
local r2_6 = string(e(r2), "%9.3f")
local r2_adj_6 = string(e(r2_a), "%9.3f")

regress meat_day_dummy HT cost_veggie_self_dummy eff_veggie_self_dummy fair_veggie_self_dummy  $rhs_veg , vce(hc3)
matrix rtable7 = r(table)
scalar nobs7 = e(N)
local r2_7 = string(e(r2), "%9.3f")
local r2_adj_7 = string(e(r2_a), "%9.3f")

regress meat_day_s HT cost_veggie_soc_dummy eff_veggie_soc_dummy fair_veggie_soc_dummy  $rhs_veg , vce(hc3)
matrix rtable8 = r(table)
scalar nobs8 = e(N)
local r2_8 = string(e(r2), "%9.3f")
local r2_adj_8 = string(e(r2_a), "%9.3f") 

restore



preserve

keep if country == "Latvia"

regress meat_tax_dummy  HT cost_tax_self_dummy eff_tax_self_dummy fair_tax_self_dummy  $rhs_tax , vce(hc3)
matrix rtable9 = r(table)
scalar nobs9 = e(N)
local r2_9 = string(e(r2), "%9.3f")
local r2_adj_9 = string(e(r2_a), "%9.3f")

regress meat_tax_s HT cost_tax_soc_dummy eff_tax_soc_dummy fair_tax_soc_dummy  $rhs_tax , vce(hc3)
matrix rtable10 = r(table)
scalar nobs10 = e(N)
local r2_10 = string(e(r2), "%9.3f")
local r2_adj_10 = string(e(r2_a), "%9.3f")

regress meat_day_dummy HT cost_veggie_self_dummy eff_veggie_self_dummy fair_veggie_self_dummy  $rhs_veg , vce(hc3)
matrix rtable11 = r(table)
scalar nobs11 = e(N)
local r2_11 = string(e(r2), "%9.3f")
local r2_adj_11 = string(e(r2_a), "%9.3f")

regress meat_day_s HT cost_veggie_soc_dummy eff_veggie_soc_dummy fair_veggie_soc_dummy  $rhs_veg , vce(hc3)
matrix rtable12 = r(table)
scalar nobs12 = e(N)
local r2_12 = string(e(r2), "%9.3f")
local r2_adj_12 = string(e(r2_a), "%9.3f")

restore	



local file_path = "main_results/comp_resFrance.xlsx"
    
if !fileexists("`file_path'") {
	
	preserve
	keep if country == "France"
	wyoung, cmd("regress meat_tax_dummy  HT cost_tax_self_dummy eff_tax_self_dummy fair_tax_self_dummy  $rhs_tax , vce(hc3)" ///
	"regress meat_tax_s HT cost_tax_soc_dummy eff_tax_soc_dummy fair_tax_soc_dummy  $rhs_tax , vce(hc3)" ///
	"regress meat_day_dummy HT cost_veggie_self_dummy eff_veggie_self_dummy fair_veggie_self_dummy  $rhs_veg , vce(hc3)" ///
	"regress meat_day_s HT cost_veggie_soc_dummy eff_veggie_soc_dummy fair_veggie_soc_dummy  $rhs_veg , vce(hc3)") ///
	familyp("HT" "HT" "HT" "HT") familypexp bootstraps(10000) seed(1234)
    
	matrix wy_results1 = r(table)
	putexcel set `file_path', replace
    putexcel A1=matrix(wy_results1), names
	restore
}
		
preserve

import excel `file_path', sheet("Sheet1") firstrow clear

mkmat coef stderr  p pwyoung pbonf psidak, matrix(wy_results1)


restore


local file_path = "main_results/comp_resItaly.xlsx"

if !fileexists("`file_path'") {
	preserve
	keep if country == "Italy"
	wyoung, cmd("regress meat_tax_dummy  HT cost_tax_self_dummy eff_tax_self_dummy fair_tax_self_dummy  $rhs_tax , vce(hc3)" ///
	"regress meat_tax_s HT cost_tax_soc_dummy eff_tax_soc_dummy fair_tax_soc_dummy  $rhs_tax , vce(hc3)" ///
	"regress meat_day_dummy HT cost_veggie_self_dummy eff_veggie_self_dummy fair_veggie_self_dummy  $rhs_veg , vce(hc3)" ///
	"regress meat_day_s HT cost_veggie_soc_dummy eff_veggie_soc_dummy fair_veggie_soc_dummy  $rhs_veg , vce(hc3)") ///
	familyp("HT" "HT" "HT" "HT") familypexp bootstraps(10000) seed(1234)
    
	matrix wy_results2 = r(table)
	putexcel set `file_path', replace
    putexcel A1=matrix(wy_results), names
	restore
	}
		
preserve

import excel `file_path', sheet("Sheet1") firstrow clear

mkmat coef stderr  p pwyoung pbonf psidak, matrix(wy_results2)


restore	


local file_path = "main_results/comp_resLatvia.xlsx"

if !fileexists("`file_path'") {
	
	preserve
	keep if country == "Latvia"
	wyoung, cmd("regress meat_tax_dummy  HT cost_tax_self_dummy eff_tax_self_dummy fair_tax_self_dummy  $rhs_tax , vce(hc3)" ///
	"regress meat_tax_s HT cost_tax_soc_dummy eff_tax_soc_dummy fair_tax_soc_dummy  $rhs_tax , vce(hc3)" ///
	"regress meat_day_dummy HT cost_veggie_self_dummy eff_veggie_self_dummy fair_veggie_self_dummy  $rhs_veg , vce(hc3)" ///
	"regress meat_day_s HT cost_veggie_soc_dummy eff_veggie_soc_dummy fair_veggie_soc_dummy  $rhs_veg , vce(hc3)") ///
	familyp("HT" "HT" "HT" "HT") familypexp bootstraps(10000) seed(1234)
    
	matrix wy_results3 = r(table)
	putexcel set `file_path', replace
    putexcel A1=matrix(wy_results3), names
	restore
}
		
preserve

import excel `file_path', sheet("Sheet1") firstrow clear
mkmat coef stderr  p pwyoung pbonf psidak, matrix(wy_results3)


restore

		

*Add title
putdocx paragraph
putdocx text ("Table SB`table_num' LPM results for the basic model by country"), font("", 11)


putdocx table mytable = (29, 13), border(all, nil) width(4) 
* Create a table with 5 rows and 5 columns (1 for covariate names, 4 for models)
putdocx table mytable(1,2) = ("France"), colspan(4)
putdocx table mytable(1,3) = ("Italy"), colspan(4)
putdocx table mytable(1,4) = ("Latvia"), colspan(4)
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



local covariates HT cost_veggie_self_dummy eff_veggie_self_dummy fair_veggie_self_dummy  $rhs_veg  _cons

local nvars : word count `covariates'
local row 3
local rowse 4

//di `nvars'
//di `covariates'
//matrix list rtable1

forvalues covar_num = 1/`nvars' {
	local covar : word `covar_num' of `covariates'
	if "`covar'" == "_cons" {
		local covar_label Constant
		putdocx table mytable(`row',1) = ("`covar_label'")
	}
	else {	
		if "`covar'" == "cost_tax_self_dummy" | "`covar'" == "cost_tax_soc_dummy" | "`covar'" == "cost_veggie_self_dummy" | "`covar'" == "veggie_tax_soc_dummy" {
		local covar_label Expense
		putdocx table mytable(`row',1) = ("`covar_label'"), italic
		}
		else {	
			if "`covar'" == "eff_tax_self_dummy" | "`covar'" == "eff_tax_soc_dummy" | "`covar'" == "eff_veggie_self_dummy" | "`covar'" == "veggie_tax_soc_dummy" {
			local covar_label Effectiveness
			putdocx table mytable(`row',1) = ("`covar_label'"), italic
			}
			else {	
				if "`covar'" == "fair_tax_self_dummy" | "`covar'" == "fair_tax_soc_dummy" | "`covar'" == "fair_veggie_self_dummy" | "`covar'" == "veggie_tax_soc_dummy" {
				local covar_label Fairness
				putdocx table mytable(`row',1) = ("`covar_label'"), italic
				}
				
				else {
					 local covar_label : variable label `covar'
					 putdocx table mytable(`row',1) = ("`covar_label'"), italic
					 }
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

		* Insert coefficients, standard errors, and stars for each model
		*Taking into account that dine in canteen is only in meat-free days models
		if `covar_num' > 11 {
				local row2 = `row' +1
				putdocx table mytable(`row2',2) = ("`=strltrim("`b1_val'")'`stars1'`stars_adj1'"), halign(left) linebreak
				putdocx table mytable(`row2',2) = ("(`=strltrim("`se1_val'")')"), append
				putdocx table mytable(`row2',3) = ("`=strltrim("`b2_val'")'`stars2'`stars_adj2'"), halign(left) linebreak
				putdocx table mytable(`row2',3) = ("(`=strltrim("`se2_val'")')"), append
				putdocx table mytable(`row',4) = ("`=strltrim("`b3_val'")'`stars3'`stars_adj3'"), halign(left) linebreak
				putdocx table mytable(`row',4) = ("(`=strltrim("`se3_val'")')"), append 
				putdocx table mytable(`row',5) = ("`=strltrim("`b4_val'")'`stars4'`stars_adj4'"), halign(left) linebreak
				putdocx table mytable(`row',5) = ("(`=strltrim("`se4_val'")')"), append 
				putdocx table mytable(`row2',6) = ("`=strltrim("`b5_val'")'`stars5'`stars_adj5'"), halign(left) linebreak
				putdocx table mytable(`row2',6) = ("(`=strltrim("`se5_val'")')"), append 
				putdocx table mytable(`row2',7) = ("`=strltrim("`b6_val'")'`stars6'`stars_adj6'"), halign(left) linebreak
				putdocx table mytable(`row2',7) = ("(`=strltrim("`se6_val'")')"), append 
				putdocx table mytable(`row',8) = ("`=strltrim("`b7_val'")'`stars7'`stars_adj7'"), halign(left) linebreak
				putdocx table mytable(`row',8) = ("(`=strltrim("`se7_val'")')"), append 
				putdocx table mytable(`row',9) = ("`=strltrim("`b8_val'")'`stars8'`stars_adj8'"), halign(left) linebreak
				putdocx table mytable(`row',9) = ("(`=strltrim("`se8_val'")')"), append 
				putdocx table mytable(`row2',10) = ("`=strltrim("`b9_val'")'`stars9'`stars_adj9'"), halign(left) linebreak
				putdocx table mytable(`row2',10) = ("(`=strltrim("`se9_val'")')"), append 
				putdocx table mytable(`row2',11) = ("`=strltrim("`b10_val'")'`stars10'`stars_adj10'"), halign(left) linebreak
				putdocx table mytable(`row2',11) = ("(`=strltrim("`se10_val'")')"), append 
				putdocx table mytable(`row',12) = ("`=strltrim("`b11_val'")'`stars11'`stars_adj11'"), halign(left) linebreak
				putdocx table mytable(`row',12) = ("(`=strltrim("`se11_val'")')"), append 
				putdocx table mytable(`row',13) = ("`=strltrim("`b12_val'")'`stars12'`stars_adj12'"), halign(left) linebreak
				putdocx table mytable(`row',13) = ("(`=strltrim("`se12_val'")')"), append 

			}
			else if "`covar'" == "HT" {
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
			
	
		local row = `row' + 1

* Increment row


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

putdocx table mytable(`row',1) = ("Note: Robust standard errors in parentheses."), colspan(13) linebreak 
putdocx table mytable(`row',1) = ("For health-risk information, Westfall-Young MHT adjusted p-values in brackets with 10,000 bootstrap replications. "),  append linebreak 
putdocx table mytable(`row',1) = ("***p<0.001, **p<0.01, *p<0.05 for standard p-values, +++p< 0.001, ++p < 0.01, +p < 0.05 for Westfall-Young MHT adjusted p-values."),  append




putdocx save "Supplementary materials/Supplementary material SB3.docx", replace


* ========================
**# Supplementary material B.4
* ========================


* ========================
**## Table SB37 Multinomial model on respondent preferences for use of meat tax revenue spending
* ========================

gen tax_spending = ut

/* 1: animal welfare; 2: federal budget; 3: climate protection; 4: lower taxes on plant-based diets; 5: lower taxes on meat substitutes; 6: lower meat production in country and use revenues to support farmers who turn to other products; 7: other
*/


global rhs  food_depriv health_low_rec unemployed educ_low age female income_pp_1000  vegan_veg_pesc_dummy flex_dummy  po1_dummy po2_dummy po3_dummy po4_dummy po5_dummy  diet_knowl_high cc_acc_high soc_norm_high 


forvalues i = 1/7 {
	 qui mlogit tax_spending $rhs, robust baseoutcome(`i') //necessary to get stata to do dydx(*) afterwards when looping over i
	 qui eststo mn_`i': margins, dydx (*) predict (outcome (`i')) post
}

esttab mn_1 mn_2 mn_3 mn_4 mn_5 mn_6 mn_7, cells(b(star fmt(3)) se(par))  stats(N , fmt(0) label(N))star(* 0.10 ** 0.05 *** 0.01) nobaselevels , using "Supplementary materials/Supplementary materials SB4.rtf", replace



