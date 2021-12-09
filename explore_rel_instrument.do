
global path "/Users/jaqueline.oliveira/Dropbox/Roads/AEJ_replication_files/temporary"

*testing github


global prehead prehead(\begin{table}[hbpt] \begin{center} \begin{threeparttable} \begin{adjustbox}{max width=\textwidth,max totalheight=\textheight,keepaspectratio} \begin{minipage} {\textwidth} \caption{{@title}} \label{@title} \begin{tabular}{l*{@M}c} \toprule) posthead("\midrule")
global postfoot postfoot(\bottomrule \end{tabular}  \begin{tablenotes} \item {\footnotesize \emph{Notes: } @note } \end{tablenotes}  \end{minipage} \end{adjustbox} \end{threeparttable}  \end{center} \end{table})
global content style(tex)


foreach outcome in mig trade {
	
	if "`outcome'" == "mig" {
		use "/Users/jaqueline.oliveira/Dropbox/BrasiliaProject_clean/Data/SecData/Analysis_data/mig_UF1940", clear
		global depvar N_od_stock
		local b 1950
		local vartext migration stock
		local varexplain the number of people by state of birth
	}
	else {
		use "/Users/jaqueline.oliveira/Dropbox/BrasiliaProject_clean/Data/SecData/Analysis_data/trade_UF1940", clear
		global depvar value
		local b 1949
		local vartext trade flow
		local varexplain the value of imports by source
	}
gen depvar = $depvar
gen log_depvar = log($depvar)
capture keep if source == "Historical"
*capture keep if log_depvar ~= .


*! Replace travel times with zeros:
foreach var of varlist log_fm_mst_pie log_fm_empty log_fm_road log_dist_km {
gen `var'_0 = `var'
replace `var'_0 = 0 if orig_id == dest_id
}


egen pair_id = group(orig_id dest_id)
egen orig_year = group(orig_id year)
egen dest_year = group(dest_id year)

foreach type in "" _0 _collapse {
gen rel_iv`type' = abs(log_fm_mst_pie`type' - log_fm_empty`type') /* a large positive instrument means a large change in tt due to MST*/
qui sum rel_iv`type', detail
gen lg_shock`type' = (rel_iv`type' > `r(p50)')

qui sum log_dist_km`type', detail
gen lg_logdist`type' = (log_dist_km`type' < `r(p50)')


gen log_alternate_iv`type' = log_fm_empty`type' if year < 1960
replace log_alternate_iv`type' = log_fm_mst_pie`type' if year >= 1960
}


qui sum dist_km, detail
gen lg_dist = (dist_km < `r(p50)')



gen pairGo = (orig_id == 52 | dest_id == 52)

gen region_orig = int(orig_id/10)
gen region_dest = int(dest_id/10)

gen dum_mig = (orig_id ~= dest_id)

sum dest_dist_brasilia, det
gen pair_close_brasilia = (dest_dist_brasilia < `r(p50)' & orig_dist_brasilia < `r(p50)')

sum dest_dist_coast, det
gen pair_close_coast = (dest_dist_coast < `r(p50)' & orig_dist_coast < `r(p50)')

 

gen dum_post = (year>=1960)
label var dum_post "After 1950"

foreach type in "" _0 _collapse {
label var rel_iv`type' "Log decrease traveltime"
label var lg_shock`type' "Large decrease traveltime"
label var log_fm_road`type' "Log traveltime"
label var log_fm_mst_pie`type' "Log MST" 
}

global cluster cluster(pair_id)

foreach type in "" _0 _collapse {

	foreach model in m1 m2 m3  {

if 	"`model'" == "m1" {
		local instvar rel_iv`type'
		local controls  
		local inst c.dum_post#c.`instvar'
		local dropgo no 
		local dist  no
		local if if log_depvar ~= .
		local ifppml  
		}
else if 	"`model'" == "m2" {
		local instvar rel_iv`type'
		local controls 
		local inst c.dum_post#c.`instvar'
		local dropgo yes 
		local dist no
		local if if log_depvar ~= . & pairGo == 0
		local ifppml if pairGo == 0
		}

		else if 	"`model'" == "m3" {
		local instvar rel_iv`type'
		local controls c.dum_post#c.lg_dist c.dum_post#c.pair_close_brasilia c.dum_post#c.pair_close_coast
		local inst c.dum_post#c.`instvar'
		local dropgo yes 
		local dist yes
		local if if log_depvar ~= . & pairGo == 0
		local ifppml if pairGo == 0
		}
		
		else if 	"`model'" == "m4" {
		local instvar rel_iv`type'
		local controls c.dum_post#c.log_dist_km`type'
		local inst c.dum_post#c.`instvar'
		local dropgo yes 
		local dist yes
		local if if log_depvar ~= . & pairGo == 0
		local ifppml if pairGo == 0
		}
	
			else if 	"`model'" == "m5" {
		local instvar lg_shock`type'
		local controls c.dum_post#c.lg_dist`type'
		local inst c.dum_post#c.`instvar'
		local dropgo yes 
		local dist yes
		local if if log_depvar ~= . & pairGo == 0
		local ifppml if pairGo == 0
		}
		
			else if 	"`model'" == "m6" {
		local instvar log_fm_mst_pie`type'
		local controls c.dum_post#c.log_dist_km`type'
		local inst c.dum_post#c.`instvar'
		local dropgo yes 
		local dist yes
		local if if log_depvar ~= . & pairGo == 0
		local ifppml if pairGo == 0
		}

		else if "`model'" == "m7" {
		local instvar log_alternate_iv`type'
		local controls c.dum_post#c.log_dist_km`type'
		local inst c.dum_post#c.`instvar'
		local dropgo yes 
		local dist yes
		local if if log_depvar ~= . & pairGo == 0
		local ifppml if pairGo == 0
		}

		
reghdfe log_fm_road`type' `inst' `controls' `if', absorb(orig_id#year dest_id#year orig_id#dest_id) $cluster
eststo fs_`model'
sum log_fm_road`type' if e(sample) == 1
estadd local meandepvar = round(`r(mean)', .02)
estadd local dropGo "`dropgo'"
estadd local dist "`dist'"

reghdfe log_depvar `inst' `controls' `if', absorb(orig_id#year dest_id#year orig_id#dest_id) $cluster
eststo rf_`model'
sum log_depvar if e(sample) == 1
estadd local meandepvar = round(`r(mean)', .02)
estadd local dropGo "`dropgo'"
estadd local dist "`dist'"

ppmlhdfe depvar log_fm_road`type' `ifppml', absorb(orig_id#year dest_id#year orig_id#dest_id) $cluster
eststo ppml_`model'
sum depvar if e(sample) == 1
estadd local meandepvar = round(`r(mean)', .02)
estadd local dropGo "`dropgo'"
estadd local dist "`dist'"

reghdfe log_depvar log_fm_road`type' `if', absorb(orig_id#year dest_id#year orig_id#dest_id) $cluster
eststo ols_`model'
sum log_depvar if e(sample) == 1
estadd local meandepvar = round(`r(mean)', .02)
estadd local dropGo "`dropgo'"
estadd local dist "`dist'"

ivreghdfe log_depvar (log_fm_road`type' = `inst') `controls' `if', absorb(orig_id#year dest_id#year orig_id#dest_id) $cluster
eststo iv_`model'
sum log_depvar if e(sample) == 1
estadd local meandepvar = round(`r(mean)', .02)
estadd local dropGo "`dropgo'"
estadd local dist "`dist'"


estout  fs_`model' rf_`model' ppml_`model' ols_`model' iv_`model'  using "$path/results/`outcome'_`model'`type'.tex", replace $prehead $postfoot $content cell(b(fmt(3)) se(fmt(3) star)) stat( dropGo dist N, labels("Drop Goias" "Controls" "No. obs") fmt("" ""  %12.0gc)) title("Results for `vartext'") collabels(,none) keep(`inst'  log_fm_road`type') label drop(*o.* _cons, relax) noomit numbers /*mgroups("DiD" "Event study", pattern(1 0 1 0 ) span prefix(\multicolumn{@span}{c}{) suffix(}) erepeat(\cmidrule(lr){@span}))*/ mlabel("FS" "RF" "PPML" "OLS" "IV") note("Units are state pairs, and data available decennial for period 1940-1980. Standard erros clustered at the pair level. Pair-level controls (interacted with ``after 1950'' dummy) include: indicator for shared border, indicator for pair being close to each other, indicator for pair being close to Brasilia, and indicator for pair being close to the coast. Closeness is defined as being within median distance. Data source: Historical yearbooks, 1940-1980.")

		}

	}

}





use "/Users/jaqueline.oliveira/Dropbox/BrasiliaProject_clean/Data/SecData/Analysis_data/N_od_meso", clear

gen log_N_od = log(N_od_flow)

egen pair_id = group(orig_id dest_id)
egen orig_year = group(orig_id year)
egen dest_year = group(dest_id year)

foreach var of varlist log_fm_mst_pie log_fm_empty log_fm_road log_dist_km {
rename `var'_collapse `var'_col
}

foreach var of varlist log_fm_mst_pie log_fm_empty log_fm_road log_dist_km {
gen `var'_0 = `var'
replace `var'_0 = 0 if orig_id == dest_id
}


foreach type in "" _0 _col {
gen rel_iv`type' = abs(log_fm_mst_pie`type' - log_fm_empty`type') /* a large positive instrument means a large change in tt due to MST*/
qui sum rel_iv`type', detail
gen lg_shock`type' = (rel_iv`type' > `r(p50)')

qui sum log_dist_km`type', detail
gen lg_logdist`type' = (log_dist_km`type' > `r(p50)')

gen log_alternate_iv`type' = log_fm_empty`type' if year < 1960
replace log_alternate_iv`type' = log_fm_mst_pie`type' if year >= 1960
}


qui sum dist_km, detail
gen lg_dist = (dist_km < `r(p50)')



sum dest_dist_brasilia, det
gen pair_close_brasilia = (dest_dist_brasilia < `r(p50)' & orig_dist_brasilia < `r(p50)')

sum dest_dist_coast, det
gen pair_close_coast = (dest_dist_coast < `r(p50)' & orig_dist_coast < `r(p50)')


sum dest_dist_nearest_capital, det
gen pair_close_capital = (dest_dist_nearest_capital < `r(p50)' & orig_dist_nearest_capital < `r(p50)')

sum dest_dist_nearest_city, det
gen pair_close_city = (dest_dist_nearest_city < `r(p50)' & orig_dist_nearest_city < `r(p50)')

gen pair_capital = (orig_dum_has_cap == 1 |  dest_dum_has_cap == 1)

gen state_orig = int(orig_id/100)
gen state_dest = int(dest_id/100)

gen region_orig = int(state_orig/10)
gen region_dest = int(state_dest/10)



gen pairGo = (state_orig == 52 | state_dest == 52 | state_orig == 53 | state_dest == 53)


egen pair_id_state = group(state_orig state_dest)

capture drop coastal*
gen coastal_orig = 1 if region_orig == 2 | region_orig == 3 | region_orig == 4
replace coastal_orig = 0 if state_orig == 31
replace coastal_orig = 1 if state_orig == 15
gen coastal_dest = 1 if region_dest == 2 | region_dest == 3 | region_dest == 4
replace coastal_dest = 0 if state_orig == 31
replace coastal_dest = 1 if state_orig == 15

gen coastal_pair = (coastal_orig == 1 & coastal_dest == 1)
gen inland = coastal_pair 

gen dum_mig = (orig_id ~= dest_id)



foreach type in "" _0 _col {
label var rel_iv`type' "Log decrease traveltime"
label var lg_shock`type' "Large decrease traveltime"
label var log_fm_road`type' "Log traveltime"
label var log_fm_mst_pie`type' "Log MST" 
}


global cluster cluster(pair_id_state)


foreach type in "" _0 _col {
foreach model in m1 m2 m3 m4 {

if 	"`model'" == "m1" {
		local controls  
		local statepair pair_id_state##year
		local dropcp no 	
		local paircntr no
		local pairfe yes
		local if 
		}
else if  "`model'" == "m2" {
		local controls  
		local statepair pair_id_state##year
		local dropcp yes 	
		local paircntr no
		local pairfe yes
		local if if pair_capital == 0
		}
		
		else if  "`model'" == "m3" {
		local controls lg_dist pair_close_brasilia pair_close_coast pair_close_city pair_capital
		local statepair pair_id_state##year
		local dropcp yes 	
		local paircntr yes
		local pairfe yes
		local if if pair_capital == 0 
		}
		
		else if  "`model'" == "m4" {
		local controls log_dist_km_0 pair_close_brasilia pair_close_coast pair_close_city pair_capital
		local statepair pair_id_state##year 
		local dropcp yes 	
		local paircntr yes
		local pairfe yes
		local if if pair_capital == 0 
		}


reghdfe log_N_od log_fm_road`type' `controls' `if', absorb(orig_id##year dest_id##year `statepair' ) $cluster
estimates store ols_`model'`type'
estadd local dropcp "`dropcp'"
estadd local pairfe "`pairfe'"
estadd local paircntr "`paircntr'"

/*
ppmlhdfe N_od_flow log_fm_road`type' `if', absorb(orig_id##year dest_id##year `statepair') $cluster
estimates store ppml_`model'
estadd local dropcp "`dropcp'"
estadd local pairfe "`pairfe'"
estadd local paircntr "`paircntr'"
*/
ivreghdfe log_N_od (log_fm_road`type' = i.year#c.log_fm_mst_pie`type') `controls' `if', absorb(orig_id##year dest_id##year `statepair' ) $cluster savefirst savefprefix(fs_`model'`type')
estimates store iv_`model'`type'
estadd local dropcp "`dropcp'"
estadd local pairfe "`pairfe'"
estadd local paircntr "`paircntr'"

estimates restore fs_`model'`type'log_fm_road`type'
estadd scalar ftest = round(`e(rkf)', 0.01)
estadd local dropcp "`dropcp'"
estadd local pairfe "`pairfe'"
estadd local paircntr "`paircntr'"


estout  fs_`model'`type'log_fm_road`type'  ols_`model'`type' /*ppml_`model'`type'*/ iv_`model'`type'  using "$path/results/mig_meso_`model'`type'.tex", replace $prehead $postfoot $content cell(b(fmt(3)) se(fmt(3) star)) stat(dropcp paircntr pairfe N, labels("Drop capitals" "Controls" "State pair X year FE" "N. obs") fmt("" "" "" %12.0gc)) title("Results for migration flows - Meso") collabels(,none) keep( *log_fm_mst_pie`type'* log_fm_road`type') label drop(*o.* _cons, relax) noomit numbers /*mgroups("FS" "OLS" "PPML" "IV", pattern(1 0 1 0 ) span prefix(\multicolumn{@span}{c}{) suffix(}) erepeat(\cmidrule(lr){@span}))*/ mlabel("FS" "OLS"  "IV") note("Units are meso-region pairs. Meso-pair controls: indicator for being close to each other, for being close to Brasilia, for being close to a capital city, for being close to a city, and for being close to the coast. Closeness is defined by distance being smaller than the median. Units are meso pairs, and data available decennial for period 1980-2010. Standard erros clustered at the state pair level. Data source: Census, 1980-2010.")

	}	
}



