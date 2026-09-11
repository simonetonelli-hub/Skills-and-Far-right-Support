*******************************************************************************
**  Skills and Far-right Support: How Education and Training Systems Moderate  
**  Voting Behaviour in The Knowledge Economy.
**  Authors: Simone Tonelli, Niccolo Durazzi, Philip Rathgeb
**  Journal of European Public Policy
**  Replication part 2 of 2 - analysis
**  Input : vet_rrp_analysis.dta, vet_rrp_analysis_empunemp.dta,
**          vet_rrp_analysis_allactive.dta, vet_rrp_analysis_ches.dta
**  Output: every table and figure in the manuscript and the appendix
**  Run this file whole, after 01_build_data.R
**  Requires: estout, grc1leg
*******************************************************************************

clear all
set more off
version 17

*-----------------------------------------------------------------------------*
* 0.1 Paths (edit PROJECT)
*-----------------------------------------------------------------------------*

global PROJECT  "/Users/Library/Progetti/VET & RRP/Review/R2/Replication"

global DATA     "$PROJECT/output"
global TABLES   "$PROJECT/output/tables"
global FIGURES  "$PROJECT/output/figures"
global LOGS     "$PROJECT/output/logs"

capture mkdir "$TABLES"
capture mkdir "$FIGURES"
capture mkdir "$LOGS"

*-----------------------------------------------------------------------------*
* 0.2 Run log
*-----------------------------------------------------------------------------*

capture log close _all

local stamp = subinstr(trim("`c(current_date)'"), " ", "", .) ///
            + "_" + subinstr("`c(current_time)'", ":", "", .)
global RUNLOG "$LOGS/02_analysis_`stamp'.log"

log using "$RUNLOG", replace text

global T0 = clock("`c(current_date)' `c(current_time)'", "DMY hms")

display as text "{hline 64}"
display as text "02_analysis.do"
display as text "Started : `c(current_date)' `c(current_time)'"
display as text "Stata   : `c(stata_version)'  (`c(machine_type)')"
display as text "Log     : $RUNLOG"
display as text "{hline 64}"


*=============================================================================*
* 1.1 Load the analysis file and apply the sample restrictions
*=============================================================================*

use "$DATA/vet_rrp_analysis.dta", clear

drop if missing(dualvet_round)
drop if essround == 11
drop if agea > 65

*=============================================================================*
* 1.2 Derived variables
*=============================================================================*

gen     edu3new = 1 if eduvet2 == 2 | inlist(edulvlb, 0, 113, 212, 213)
replace edu3new = 2 if eduvet2 == 3 | inlist(edulvlb, 129, 221, 222, 223)
replace edu3new = 3 if eduvet2 == 4

gen     mtu = 1 if mbtru == 1
replace mtu = 0 if mbtru == 2 | mbtru == 3

encode sector, gen(industry)
tabulate industry, gen(sec_)

encode cntry,  gen(country)
encode region, gen(regione)
egen regionround = group(region essround)
egen cntryround  = group(cntry essround)

gen     age_group = 1 if agea < 35
replace age_group = 2 if agea >= 35 & agea < 50
replace age_group = 3 if agea >= 50

gen byte period = (eyear >= 2014) if !missing(eyear)

*=============================================================================*
* 1.3 Value and variable labels
*=============================================================================*

label define edu3new_lbl 1 "General" 2 "Vocational (ref: General)" ///
                         3 "Higher (ref: General)", replace
label values edu3new edu3new_lbl

label define mtu_lbl   0 "Trade union member (No)" 1 "Trade union member (Yes)", replace
label values mtu mtu_lbl
label define sec5_lbl  0 "Manufacturing (No)"      1 "Manufacturing (Yes)", replace
label values sec_5 sec5_lbl
label define urban_lbl 0 "Urban (No)"              1 "Urban (Yes)", replace
label values urban urban_lbl
label define age_group_lbl 1 "18-34" 2 "35-49" 3 "50+", replace
label values age_group age_group_lbl
label define period_lbl 0 "Crisis and austerity (elections to 2013)" ///
                        1 "Refugee crisis and after (2014 onwards)", replace
label values period period_lbl

label variable edu3new       "Educational attainment"
label variable occupation    "Occupation"
label variable dualvet_round "Dual VET share"
label variable agea          "Age"
label variable age2          "Age squared"
label variable gndr          "Sex"
label variable rlgdgr        "Religiosity"
label variable urban         "Urban"
label variable mtu           "Trade union member"
label variable sec_5         "Manufacturing"
label variable imwbcnt       "Immigrants good for country"
label variable unemp         "Unemployment rate"
label variable logGDPpc      "GDP per capita"
label variable emprot_reg    "Employment protection legislation"
label variable gini          "Gini coefficient"
label variable wage_p50_p10  "50/10 wage ratio"
label variable wage_p90_p10  "90/10 wage ratio"
label variable wage_p90_p50  "90/50 wage ratio"
label variable degreemom     "Mother has a degree"
label variable degreedad     "Father has a degree"
label variable age_group     "Age band"
label variable period        "Election period"

*=============================================================================*
* 1.4 Specification shorthands used throughout
*=============================================================================*

global demo       "agea age2 i.gndr rlgdgr i.urban"
global demo_par   "agea age2 i.gndr rlgdgr i.urban degreedad degreemom"
global downstream "i.occupation i.mtu i.sec_5 imwbcnt"
global macro      "dualvet_round unemp emprot_reg logGDPpc"
global macro_int  "unemp emprot_reg logGDPpc"
global core       "i.edu3new##c.dualvet_round"
global FE         "i.year i.country"

*=============================================================================*
* 1.5 Sample check
*=============================================================================*

count if !missing(edu3new, farright, region, election, anweight)
display as text "Table 1 Model 1 N = " r(N)

levelsof cntry, local(cc)
display as text "Countries : " `: word count `cc''

levelsof election if !missing(edu3new, farright, anweight), local(ee)
display as text "Elections : " `: word count `ee''


*=============================================================================*
* 2. TABLE 1 - far-right vote choice, multilevel logit
*=============================================================================*

eststo clear

eststo M1: quietly melogit farright i.edu3new ///
    [pweight = anweight] || region: || election:

eststo M2: quietly melogit farright i.edu3new $demo ///
    [pweight = anweight] || region: || election:

eststo M3: quietly melogit farright i.edu3new##c.dualvet_round $demo ///
    [pweight = anweight] || region: || election:

eststo M4: quietly melogit farright i.edu3new $demo $downstream ///
    [pweight = anweight] || region: || election:

eststo M5: quietly melogit farright i.edu3new $demo $downstream $macro ///
    [pweight = anweight] || region: || election:

eststo M6: quietly melogit farright i.edu3new##c.dualvet_round $demo $downstream $macro_int ///
    [pweight = anweight] || region: || election:

esttab M1 M2 M3 M4 M5 M6 using "$TABLES/Table1_vote_models.rtf", replace ///
    b(3) se(3) label star(* 0.10 ** 0.05 *** 0.01) ///
    mtitles("Model 1" "Model 2" "Model 3" "Model 4" "Model 5" "Model 6") ///
    nodepvars nogaps compress nobaselevels noomitted drop(/:*) ///
    stats(N ll, labels("Observations" "Log likelihood") fmt(0 3)) nonotes ///
    addnotes("Robust standard errors in parentheses" "*** p<0.01, ** p<0.05, * p<0.1")

display as result _n "TABLE 1  Far-right vote choice"
capture noisily esttab M1 M2 M3 M4 M5 M6, ///
    b(3) se(3) label star(* 0.10 ** 0.05 *** 0.01) ///
    mtitles("Model 1" "Model 2" "Model 3" "Model 4" "Model 5" "Model 6") ///
    nodepvars nogaps compress nobaselevels noomitted drop(/:*) ///
    stats(N ll, labels("Observations" "Log likelihood") fmt(0 3))


*=============================================================================*
* 3. FIGURE 1 - predicted-probability contrasts from Model 3
*=============================================================================*

quietly melogit farright i.edu3new##c.dualvet_round $demo ///
    [pweight = anweight] || region: || election:

margins r.edu3new, at(dualvet_round = (0(5)60))

marginsplot, recast(line) ///
    plot1opts(lcolor(gs8) lpattern(solid)) plot2opts(lcolor(gs8) lpattern(dash)) ///
    ciopt(color(black%20)) recastci(rarea) yline(0) ///
    title("") xtitle("Dual VET share") ///
    ytitle("Contrasts of predicted probabilities") ///
    ylabel(0 "{bf:General}", add labsize(small)) ///
    plot(, label("Vocational" "Tertiary")) ///
    xlabel(0 "0" 10 20 30 40 50 60 "60") ///
    name(fig1, replace)

graph save   "$FIGURES/Figure1_vote_contrasts.gph", replace
graph export "$FIGURES/Figure1_vote_contrasts.tif", width(4000) replace
graph export "$FIGURES/Figure1_vote_contrasts.eps", replace


*=============================================================================*
* 4. TABLE 2 - occupational allocation, multilevel multinomial logit
*=============================================================================*

eststo clear

eststo O1: quietly gsem (i.occupation <- i.edu3new M2[region]@1) ///
    [pweight = anweight], mlogit vce(robust)

eststo O2: quietly gsem (i.occupation <- i.edu3new agea age2 i.gndr i.urban ///
    M2[region]@1) [pweight = anweight], mlogit vce(robust)

eststo O3: quietly gsem (i.occupation <- i.edu3new##c.dualvet_round ///
    agea age2 i.gndr i.urban M2[region]@1) [pweight = anweight], mlogit vce(robust)

eststo O4: quietly gsem (i.occupation <- i.edu3new agea age2 i.gndr i.urban ///
    sec_5 unemp emprot_reg logGDPpc M2[region]@1) [pweight = anweight], mlogit vce(robust)

eststo O5: quietly gsem (i.occupation <- i.edu3new##c.dualvet_round ///
    agea age2 i.gndr i.urban sec_5 unemp emprot_reg logGDPpc ///
    M2[region]@1) [pweight = anweight], mlogit vce(robust)

esttab O1 O2 O3 O4 O5 using "$TABLES/Table2_occupation_models.rtf", replace ///
    b(3) se(3) label star(* 0.10 ** 0.05 *** 0.01) ///
    mtitles("Model 1" "Model 2" "Model 3" "Model 4" "Model 5") ///
    nodepvars nogaps compress nobaselevels noomitted drop(/:*) ///
    stats(N ll, labels("Observations" "Log likelihood") fmt(0 3)) nonotes ///
    addnotes("Robust standard errors in parentheses" "*** p<0.01, ** p<0.05, * p<0.1" ///
             "First equation: routine versus non-routine cognitive." ///
             "Second equation: non-routine manual versus non-routine cognitive.")

display as result _n "TABLE 2  Occupational allocation"
capture noisily esttab O1 O2 O3 O4 O5, ///
    b(3) se(3) label star(* 0.10 ** 0.05 *** 0.01) ///
    mtitles("Model 1" "Model 2" "Model 3" "Model 4" "Model 5") ///
    nodepvars nogaps compress nobaselevels noomitted drop(/:*) ///
    stats(N ll, labels("Observations" "Log likelihood") fmt(0 3))


*=============================================================================*
* 5. FIGURE 2 - occupational probabilities of vocational respondents,
*    pooled sample and respondents under 35
*=============================================================================*

quietly gsem (i.occupation <- i.edu3new##c.dualvet_round agea age2 i.gndr i.urban ///
    M2[region]@1) [pweight = anweight], mlogit vce(robust)

margins edu3new if edu3new == 2, at(dualvet_round = (0(3)60))

marginsplot, recast(line) recastci(rarea) ///
    ciopt(lpattern(solid) lcolor(black) acolor(gs13%40) lcolor(none)) ///
    plot1opts(lpattern(solid) lcolor(black)) ///
    plot2opts(lpattern(dot)   lcolor(black)) ///
    plot3opts(lpattern(dash)  lcolor(black)) ///
    title("") ytitle("{bf:Marginal probability}") ///
    xtitle("{bf:Dual VET share}") ///
    plot(, label("Non-routine cognitive" "Routine" "Non-routine manual")) ///
    legend(title("{bf:Occupation}", size(3.5))) ///
    xlabel(0 "0" 20 40 60 "60") ///
    name(fig2a, replace)
graph save "$FIGURES/fig2a_full.gph", replace

quietly gsem (i.occupation <- i.edu3new##c.dualvet_round agea age2 i.gndr i.urban ///
    M2[region]@1) if agea < 35 [pweight = anweight], mlogit vce(robust)

margins edu3new if edu3new == 2, at(dualvet_round = (0(3)60))

marginsplot, recast(line) recastci(rarea) ///
    ciopt(lpattern(solid) lcolor(black) acolor(gs13%40) lcolor(none)) ///
    plot1opts(lpattern(solid) lcolor(black)) ///
    plot2opts(lpattern(dot)   lcolor(black)) ///
    plot3opts(lpattern(dash)  lcolor(black)) ///
    title("") ytitle("{bf:Marginal probability}") ///
    xtitle("{bf:Dual VET share}") ///
    plot(, label("Non-routine cognitive" "Routine" "Non-routine manual")) ///
    legend(title("{bf:Occupation}", size(3.5))) ///
    xlabel(0 "0" 20 40 60 "60") ///
    name(fig2b, replace)
graph save "$FIGURES/fig2b_under35.gph", replace

grc1leg "$FIGURES/fig2a_full.gph" "$FIGURES/fig2b_under35.gph", rows(1)
graph save   "$FIGURES/Figure2_occupation.gph", replace
graph export "$FIGURES/Figure2_occupation.tif", width(4000) replace
graph export "$FIGURES/Figure2_occupation.eps", replace


*=============================================================================*
* 6. TABLE A1 - country and election random intercepts
*=============================================================================*

eststo clear
eststo A1_1: quietly melogit farright i.edu3new                          [pweight = anweight] || cntry: || election:
eststo A1_2: quietly melogit farright i.edu3new $demo                    [pweight = anweight] || cntry: || election:
eststo A1_3: quietly melogit farright i.edu3new##c.dualvet_round $demo   [pweight = anweight] || cntry: || election:
eststo A1_4: quietly melogit farright i.edu3new $demo $downstream        [pweight = anweight] || cntry: || election:
eststo A1_5: quietly melogit farright i.edu3new $demo $downstream $macro [pweight = anweight] || cntry: || election:
eststo A1_6: quietly melogit farright i.edu3new##c.dualvet_round $demo $downstream $macro_int [pweight = anweight] || cntry:

esttab A1_* using "$TABLES/TableA1_country_RE.rtf", replace ///
    b(3) se(3) label star(* 0.10 ** 0.05 *** 0.01) ///
    mtitles("Model 1" "Model 2" "Model 3" "Model 4" "Model 5" "Model 6") ///
    nodepvars nogaps compress nobaselevels noomitted drop(/:*) ///
    stats(N ll, labels("Observations" "Log likelihood") fmt(0 3)) nonotes ///
    addnotes("Robust standard errors in parentheses" "*** p<0.01, ** p<0.05, * p<0.1")

display as result _n "TABLE A1  Country and election random intercepts"
capture noisily esttab A1_*, ///
    b(3) se(3) label star(* 0.10 ** 0.05 *** 0.01) ///
    mtitles("Model 1" "Model 2" "Model 3" "Model 4" "Model 5" "Model 6") ///
    nodepvars nogaps compress nobaselevels noomitted drop(/:*) ///
    stats(N ll, labels("Observations" "Log likelihood") fmt(0 3))


*=============================================================================*
* 7. TABLE A2 - year fixed effects
*=============================================================================*

eststo clear
eststo A2_1: quietly melogit farright i.edu3new i.year                          [pweight = anweight] || region: || election:
eststo A2_2: quietly melogit farright i.edu3new $demo i.year                    [pweight = anweight] || region: || election:
eststo A2_3: quietly melogit farright i.edu3new##c.dualvet_round $demo i.year   [pweight = anweight] || region: || election:
eststo A2_4: quietly melogit farright i.edu3new $demo $downstream i.year        [pweight = anweight] || region: || election:
eststo A2_5: quietly melogit farright i.edu3new $demo $downstream $macro i.year [pweight = anweight] || region: || election:
eststo A2_6: quietly melogit farright i.edu3new##c.dualvet_round $demo $downstream $macro_int i.year [pweight = anweight] || region: || election:

esttab A2_* using "$TABLES/TableA2_yearFE.rtf", replace ///
    b(3) se(3) label star(* 0.10 ** 0.05 *** 0.01) ///
    mtitles("Model 1" "Model 2" "Model 3" "Model 4" "Model 5" "Model 6") ///
    nodepvars nogaps compress nobaselevels noomitted drop(/:* *.year) ///
    stats(N ll, labels("Observations" "Log likelihood") fmt(0 3)) nonotes ///
    addnotes("Robust standard errors in parentheses" "*** p<0.01, ** p<0.05, * p<0.1")

display as result _n "TABLE A2  Year fixed effects"
capture noisily esttab A2_*, ///
    b(3) se(3) label star(* 0.10 ** 0.05 *** 0.01) ///
    mtitles("Model 1" "Model 2" "Model 3" "Model 4" "Model 5" "Model 6") ///
    nodepvars nogaps compress nobaselevels noomitted drop(/:* *.year) ///
    stats(N ll, labels("Observations" "Log likelihood") fmt(0 3))


*=============================================================================*
* 8. TABLE A3 - occupational models with year fixed effects
*=============================================================================*

eststo clear
eststo A3_1: quietly gsem (i.occupation <- i.edu3new i.year M2[region]@1) ///
    [pweight = anweight], mlogit vce(robust)
eststo A3_2: quietly gsem (i.occupation <- i.edu3new agea age2 i.gndr i.urban i.year ///
    M2[region]@1) [pweight = anweight], mlogit vce(robust)
eststo A3_3: quietly gsem (i.occupation <- i.edu3new##c.dualvet_round ///
    agea age2 i.gndr i.urban i.year M2[region]@1) [pweight = anweight], mlogit vce(robust)
eststo A3_4: quietly gsem (i.occupation <- i.edu3new agea age2 i.gndr i.urban ///
    sec_5 i.year unemp emprot_reg logGDPpc M2[region]@1) [pweight = anweight], mlogit vce(robust)
eststo A3_5: quietly gsem (i.occupation <- i.edu3new##c.dualvet_round ///
    agea age2 i.gndr i.urban sec_5 i.year unemp emprot_reg logGDPpc ///
    M2[region]@1) [pweight = anweight], mlogit vce(robust)

esttab A3_* using "$TABLES/TableA3_occupation_yearFE.rtf", replace ///
    b(3) se(3) label star(* 0.10 ** 0.05 *** 0.01) ///
    mtitles("Model 1" "Model 2" "Model 3" "Model 4" "Model 5") ///
    nodepvars nogaps compress nobaselevels noomitted drop(/:* *.year) ///
    stats(N ll, labels("Observations" "Log likelihood") fmt(0 3)) nonotes ///
    addnotes("Robust standard errors in parentheses" "*** p<0.01, ** p<0.05, * p<0.1")

display as result _n "TABLE A3  Occupational models, year fixed effects"
capture noisily esttab A3_*, ///
    b(3) se(3) label star(* 0.10 ** 0.05 *** 0.01) ///
    mtitles("Model 1" "Model 2" "Model 3" "Model 4" "Model 5") ///
    nodepvars nogaps compress nobaselevels noomitted drop(/:* *.year) ///
    stats(N ll, labels("Observations" "Log likelihood") fmt(0 3))


*=============================================================================*
* 9. TABLE A4 - leave one country out, countries with no estimation
*    observations skipped
*=============================================================================*

eststo clear
levelsof cntry, local(countries)
local loo_models ""
local loo_titles ""
local j = 0

foreach c of local countries {
    quietly count if cntry == "`c'" & !missing(edu3new, farright, region, election, anweight)
    if r(N) == 0 {
        display as text "   `c' contributes 0 estimation observations - column skipped"
        continue
    }
    local ++j
    preserve
        keep if cntry != "`c'"
        capture quietly melogit farright i.edu3new##c.dualvet_round i.occupation ///
            $demo [pweight = anweight] || region: || election:
        if !_rc {
            eststo LOO_`j'
            local loo_models "`loo_models' LOO_`j'"
            local loo_titles `"`loo_titles' "Excl. `c'""'
        }
        else display as error "   LOO model failed when excluding `c' (rc=" _rc ")"
    restore
}

esttab `loo_models' using "$TABLES/TableA4_leave_one_out.rtf", replace ///
    b(3) se(3) label star(* 0.10 ** 0.05 *** 0.01) ///
    mtitles(`loo_titles') ///
    nodepvars nogaps compress nobaselevels noomitted drop(/:*) ///
    stats(N ll, labels("Observations" "Log likelihood") fmt(0 3)) nonotes ///
    addnotes("Robust standard errors in parentheses" "*** p<0.01, ** p<0.05, * p<0.1")

display as result _n "TABLE A4  Leave one country out"
capture noisily esttab `loo_models', ///
    b(3) se(3) label star(* 0.10 ** 0.05 *** 0.01) ///
    mtitles(`loo_titles') ///
    nodepvars nogaps compress nobaselevels noomitted drop(/:*) ///
    stats(N ll, labels("Observations" "Log likelihood") fmt(0 3))


*=============================================================================*
* 10. TABLE A5 - inequality and wage dispersion controls
*=============================================================================*

eststo clear
foreach v in gini wage_p50_p10 wage_p90_p10 wage_p90_p50 {
    eststo A5_`v': quietly melogit farright i.edu3new##c.dualvet_round $demo `v' ///
        [pweight = anweight] || region: || election:, vce(robust)
}

esttab A5_* using "$TABLES/TableA5_inequality.rtf", replace ///
    b(3) se(3) label star(* 0.10 ** 0.05 *** 0.01) ///
    mtitles("Gini" "50/10" "90/10" "90/50") ///
    nodepvars nogaps compress nobaselevels noomitted drop(/:*) ///
    stats(N ll, labels("Observations" "Log likelihood") fmt(0 3)) nonotes ///
    addnotes("Robust standard errors in parentheses" "*** p<0.01, ** p<0.05, * p<0.1")

display as result _n "TABLE A5  Inequality and wage dispersion"
capture noisily esttab A5_*, ///
    b(3) se(3) label star(* 0.10 ** 0.05 *** 0.01) ///
    mtitles("Gini" "50/10" "90/10" "90/50") ///
    nodepvars nogaps compress nobaselevels noomitted drop(/:*) ///
    stats(N ll, labels("Observations" "Log likelihood") fmt(0 3))


*=============================================================================*
* 11. TABLE A6 - country and year fixed effects, standard errors clustered
*     by country
*=============================================================================*

eststo clear
eststo A6_1: quietly logit farright i.edu3new $FE                          [pweight = anweight], vce(cluster country)
eststo A6_2: quietly logit farright i.edu3new $demo $FE                    [pweight = anweight], vce(cluster country)
eststo A6_3: quietly logit farright i.edu3new##c.dualvet_round $demo $FE   [pweight = anweight], vce(cluster country)
eststo A6_4: quietly logit farright i.edu3new $demo $downstream $FE        [pweight = anweight], vce(cluster country)
eststo A6_5: quietly logit farright i.edu3new $demo $downstream $macro $FE [pweight = anweight], vce(cluster country)
eststo A6_6: quietly logit farright i.edu3new##c.dualvet_round $demo $downstream $macro_int $FE [pweight = anweight], vce(cluster country)

esttab A6_* using "$TABLES/TableA6_country_year_FE.rtf", replace ///
    b(3) se(3) label star(* 0.10 ** 0.05 *** 0.01) ///
    mtitles("Model 1" "Model 2" "Model 3" "Model 4" "Model 5" "Model 6") ///
    nodepvars nogaps compress nobaselevels noomitted drop(*.year *.country) ///
    stats(N ll, labels("Observations" "Log likelihood") fmt(0 3)) nonotes ///
    addnotes("Robust standard errors clustered by country in parentheses" ///
             "*** p<0.01, ** p<0.05, * p<0.1")

display as result _n "TABLE A6  Country and year fixed effects"
capture noisily esttab A6_*, ///
    b(3) se(3) label star(* 0.10 ** 0.05 *** 0.01) ///
    mtitles("Model 1" "Model 2" "Model 3" "Model 4" "Model 5" "Model 6") ///
    nodepvars nogaps compress nobaselevels noomitted drop(*.year *.country) ///
    stats(N ll, labels("Observations" "Log likelihood") fmt(0 3))


*=============================================================================*
* 12. TABLE A7 - mechanism checks on subjective insecurity and evaluations
*=============================================================================*

eststo clear
eststo Hincfel: quietly mixed hincfel $core $demo $downstream unemp logGDPpc [pweight = anweight] || region: || essround:, vce(robust)
eststo Stfeco:  quietly mixed stfeco  $core $demo $downstream unemp logGDPpc [pweight = anweight] || region: || essround:, vce(robust)
eststo Stfjb:   quietly mixed stfjb   $core $demo $downstream unemp logGDPpc [pweight = anweight] || region: || essround:, vce(robust)
eststo Stflife: quietly mixed stflife $core $demo $downstream                [pweight = anweight] || region: || essround:, vce(robust)
eststo Lkuemp:  quietly mixed lkuemp  $core $demo $downstream unemp logGDPpc [pweight = anweight] || region: || essround:, vce(robust)
eststo Status:  quietly mixed status  $core $demo $downstream unemp logGDPpc [pweight = anweight] || region: || essround:, vce(robust)

esttab Hincfel Stfeco Stfjb Stflife Lkuemp Status using "$TABLES/TableA7_mechanisms.rtf", replace ///
    b(3) se(3) label star(* 0.10 ** 0.05 *** 0.01) ///
    mtitles("Income difficulty" "Econ. satisfaction" "Job satisfaction" ///
            "Life satisfaction" "Unemp. risk" "Subjective status") ///
    nodepvars nogaps compress nobaselevels noomitted ///
    stats(N ll, labels("Observations" "Log likelihood") fmt(0 3)) nonotes ///
    addnotes("Robust standard errors in parentheses" "*** p<0.01, ** p<0.05, * p<0.1" ///
             "The job-satisfaction model did not converge; see the run log." ///
             "Delete the lns1_1_1 / lns2_1_1 / lnsig_e rows before pasting.")

display as result _n "TABLE A7  Mechanism checks"
capture noisily esttab Hincfel Stfeco Stfjb Stflife Lkuemp Status, ///
    b(3) se(3) label star(* 0.10 ** 0.05 *** 0.01) ///
    mtitles("Income difficulty" "Econ. satisfaction" "Job satisfaction" ///
            "Life satisfaction" "Unemp. risk" "Subjective status") ///
    nodepvars nogaps compress nobaselevels noomitted ///
    stats(N ll, labels("Observations" "Log likelihood") fmt(0 3))


*=============================================================================*
* 13. TABLE A8 - parental background controls
*=============================================================================*

eststo clear
eststo A8_1: quietly melogit farright i.edu3new                              [pweight = anweight] || region: || election:
eststo A8_2: quietly melogit farright i.edu3new $demo_par                    [pweight = anweight] || region: || election:
eststo A8_3: quietly melogit farright i.edu3new##c.dualvet_round $demo_par   [pweight = anweight] || region: || election:
eststo A8_4: quietly melogit farright i.edu3new $demo_par $downstream        [pweight = anweight] || region: || election:
eststo A8_5: quietly melogit farright i.edu3new $demo_par $downstream $macro [pweight = anweight] || region: || election:
eststo A8_6: quietly melogit farright i.edu3new##c.dualvet_round $demo_par $downstream $macro_int [pweight = anweight] || region: || election:

esttab A8_* using "$TABLES/TableA8_parental_background.rtf", replace ///
    b(3) se(3) label star(* 0.10 ** 0.05 *** 0.01) ///
    mtitles("Model 1" "Model 2" "Model 3" "Model 4" "Model 5" "Model 6") ///
    nodepvars nogaps compress nobaselevels noomitted drop(/:*) ///
    stats(N ll, labels("Observations" "Log likelihood") fmt(0 3)) nonotes ///
    addnotes("Robust standard errors in parentheses" "*** p<0.01, ** p<0.05, * p<0.1")

display as result _n "TABLE A8  Parental background"
capture noisily esttab A8_*, ///
    b(3) se(3) label star(* 0.10 ** 0.05 *** 0.01) ///
    mtitles("Model 1" "Model 2" "Model 3" "Model 4" "Model 5" "Model 6") ///
    nodepvars nogaps compress nobaselevels noomitted drop(/:*) ///
    stats(N ll, labels("Observations" "Log likelihood") fmt(0 3))


*=============================================================================*
* 14. TABLE A9 - excluding Austria, Germany and Switzerland
*=============================================================================*

local nodual `"cntry != "AT" & cntry != "DE" & cntry != "CH""'

eststo clear
eststo A9_1: quietly melogit farright i.edu3new                              if `nodual' [pweight = anweight] || region: || election:
eststo A9_2: quietly melogit farright i.edu3new $demo                        if `nodual' [pweight = anweight] || region: || election:
eststo A9_3: quietly melogit farright i.edu3new##c.dualvet_round $demo       if `nodual' [pweight = anweight] || region: || election:
eststo A9_4: quietly melogit farright i.edu3new $demo $downstream            if `nodual' [pweight = anweight] || region: || election:
eststo A9_5: quietly melogit farright i.edu3new $demo $downstream $macro     if `nodual' [pweight = anweight] || region: || election:
eststo A9_6: quietly melogit farright i.edu3new##c.dualvet_round $demo $downstream $macro_int if `nodual' [pweight = anweight] || region: || election:

esttab A9_* using "$TABLES/TableA9_no_AT_DE_CH.rtf", replace ///
    b(3) se(3) label star(* 0.10 ** 0.05 *** 0.01) ///
    mtitles("Model 1" "Model 2" "Model 3" "Model 4" "Model 5" "Model 6") ///
    nodepvars nogaps compress nobaselevels noomitted drop(/:*) ///
    stats(N ll, labels("Observations" "Log likelihood") fmt(0 3)) nonotes ///
    addnotes("Robust standard errors in parentheses" "*** p<0.01, ** p<0.05, * p<0.1")

display as result _n "TABLE A9  Excluding AT, DE and CH"
capture noisily esttab A9_*, ///
    b(3) se(3) label star(* 0.10 ** 0.05 *** 0.01) ///
    mtitles("Model 1" "Model 2" "Model 3" "Model 4" "Model 5" "Model 6") ///
    nodepvars nogaps compress nobaselevels noomitted drop(/:*) ///
    stats(N ll, labels("Observations" "Log likelihood") fmt(0 3))


*=============================================================================*
* 15. TABLE A10 - alternative far-right codings from CHES, column 5 unweighted
*=============================================================================*

preserve

use "$DATA/vet_rrp_analysis_ches.dta", clear

drop if missing(dualvet_round)
drop if essround == 11
drop if agea > 65

gen     edu3new = 1 if eduvet2 == 2 | inlist(edulvlb, 0, 113, 212, 213)
replace edu3new = 2 if eduvet2 == 3 | inlist(edulvlb, 129, 221, 222, 223)
replace edu3new = 3 if eduvet2 == 4

label define edu3new_lbl 1 "General" 2 "Vocational (ref: General)" ///
                         3 "Higher (ref: General)", replace
label values edu3new edu3new_lbl
label define urban_lbl 0 "Urban (No)" 1 "Urban (Yes)", replace
label values urban urban_lbl
label variable edu3new       "Educational attainment"
label variable dualvet_round "Dual VET share"
label variable agea          "Age"
label variable age2          "Age squared"
label variable gndr          "Sex"
label variable rlgdgr        "Religiosity"
label variable urban         "Urban"

foreach dv in farright_ches_family farright_ches_strict farright_ches_broad ///
              farright_ches_nativist farright_ches_welfare {
    quietly count if `dv' == 1
    local npos = r(N)
    quietly levelsof election if `dv' == 1, local(elpos)
    display as text "`dv': `npos' positives in " `: word count `elpos'' " elections"
}

eststo clear

eststo CHESfam:   quietly melogit farright_ches_family   $core $demo ///
    [pweight = anweight] || region: || election:, vce(robust)

eststo CHESstr:   quietly melogit farright_ches_strict   $core $demo ///
    [pweight = anweight] || region: || election:, vce(robust)

eststo CHESbroad: quietly melogit farright_ches_broad    $core $demo ///
    [pweight = anweight] || region: || election:, vce(robust)

eststo Nativist:  quietly melogit farright_ches_nativist $core $demo ///
    [pweight = anweight] || region: || election:, vce(robust)

eststo Welfare:   quietly melogit farright_ches_welfare  $core $demo ///
    || region: || election:, vce(robust)

local a10titles `" "CHES Radical Right" "Restricted CHES Radical Right" "Broadened CHES Radical Right" "Primarily nativist" "Welfare chauvinist" "'

esttab CHESfam CHESstr CHESbroad Nativist Welfare using "$TABLES/TableA10_alternative_DV.rtf", replace ///
    b(3) se(3) label star(* 0.10 ** 0.05 *** 0.01) ///
    mtitles(`a10titles') ///
    nodepvars nogaps compress nobaselevels noomitted drop(/:*) ///
    stats(N ll, labels("Observations" "Log likelihood") fmt(0 3)) nonotes ///
    addnotes("Robust standard errors in parentheses" "*** p<0.01, ** p<0.05, * p<0.1" ///
             "Columns 1-4 are weighted by the ESS design weight. Column 5 is" ///
             "unweighted: the weighted model does not converge on this outcome," ///
             "which has the fewest positives. Its coefficients are therefore not" ///
             "directly comparable with columns 1-4.")

display as result _n "TABLE A10  Alternative far-right codings (CHES)"
capture noisily esttab CHESfam CHESstr CHESbroad Nativist Welfare, ///
    b(3) se(3) label star(* 0.10 ** 0.05 *** 0.01) ///
    mtitles(`a10titles') ///
    nodepvars nogaps compress nobaselevels noomitted drop(/:*) ///
    stats(N ll, labels("Observations" "Log likelihood") fmt(0 3))

restore


*=============================================================================*
* 16. TABLE A11 - dual VET prevalence by country, on the estimation sample
*=============================================================================*

preserve

    keep if !missing(edu3new, farright, region, election, anweight)

    bysort cntry year: keep if _n == 1

    collapse (count) cyears = dualvet_round ///
             (mean)  mean_  = dualvet_round ///
             (min)   min_   = dualvet_round ///
             (max)   max_   = dualvet_round ///
             (sd)    sd_    = dualvet_round ///
             (min)   ymin   = year ///
             (max)   ymax   = year, by(cntry)

    gsort -mean_

    gen str32 cname = ""
    replace cname = "Austria"        if cntry == "AT"
    replace cname = "Belgium"        if cntry == "BE"
    replace cname = "Switzerland"    if cntry == "CH"
    replace cname = "Czechia"        if cntry == "CZ"
    replace cname = "Germany"        if cntry == "DE"
    replace cname = "Denmark"        if cntry == "DK"
    replace cname = "Estonia"        if cntry == "EE"
    replace cname = "Spain"          if cntry == "ES"
    replace cname = "France"         if cntry == "FR"
    replace cname = "United Kingdom" if cntry == "GB"
    replace cname = "Greece"         if cntry == "GR"
    replace cname = "Hungary"        if cntry == "HU"
    replace cname = "Italy"          if cntry == "IT"
    replace cname = "Netherlands"    if cntry == "NL"
    replace cname = "Poland"         if cntry == "PL"
    replace cname = "Portugal"       if cntry == "PT"
    replace cname = "Sweden"         if cntry == "SE"
    replace cname = "Slovenia"       if cntry == "SI"
    replace cname = "Slovakia"       if cntry == "SK"
    replace cname = cntry if cname == ""

    display as result _n "TABLE A11  Dual VET prevalence by country"
    list cname ymin ymax cyears mean_ min_ max_ sd_, noobs sep(0) abbrev(12)

    tempname a11
    file open `a11' using "$TABLES/TableA11_dualvet_by_country.rtf", write replace text
    file write `a11' "{\rtf1\ansi\deff0" _n
    file write `a11' "{\fonttbl{\f0\froman Times New Roman;}}" _n
    file write `a11' "\fs20" _n
    file write `a11' "\trowd\trgaph80"                                       ///
                     "\cellx2400\cellx3900\cellx5200\cellx6900\cellx8000"    ///
                     "\cellx9100\cellx10100" _n
    file write `a11' "\intbl\b Country\cell Years observed\cell Country-years\cell " ///
                     "Mean dual-VET share\cell Minimum\cell Maximum\cell SD\b0\cell\row" _n

    local nrows = _N
    forvalues i = 1/`nrows' {
        local cn = cname[`i']
        local y0 = ymin[`i']
        local y1 = ymax[`i']
        local cy = cyears[`i']
        local mn : display %4.1f mean_[`i']
        local lo : display %4.1f min_[`i']
        local hi : display %4.1f max_[`i']
        local sd = "n.a."
        if !missing(sd_[`i'])  local sd : display %4.1f sd_[`i']
        file write `a11' "\trowd\trgaph80"                                    ///
                         "\cellx2400\cellx3900\cellx5200\cellx6900\cellx8000" ///
                         "\cellx9100\cellx10100" _n
        file write `a11' "\intbl `cn'\cell `y0'-`y1'\cell `cy'\cell "         ///
                         "`=trim("`mn'")'\cell `=trim("`lo'")'\cell "         ///
                         "`=trim("`hi'")'\cell `=trim("`sd'")'\cell\row" _n
    }
    file write `a11' "\pard\fs18 Note: Dual-VET share is the percentage of upper-secondary " ///
                     "enrolment in combined school- and work-based vocational programmes. " ///
                     "The table is computed on the estimation sample, so it covers the "     ///
                     "countries and survey years that enter the models. Portugal and Greece " ///
                     "each contribute a single survey year, so no standard deviation is "    ///
                     "defined for them.\par" _n
    file write `a11' "}" _n
    file close `a11'

    display as text _n "   written: $TABLES/TableA11_dualvet_by_country.rtf"

restore


*=============================================================================*
* 17. TABLE A12 - sample definition: employees, employees plus unemployed,
*     and the whole economically active population
*=============================================================================*

eststo clear
local a12models ""
local a12titles `""'
local k = 0

foreach f in "vet_rrp_analysis" "vet_rrp_analysis_empunemp" "vet_rrp_analysis_allactive" {

    local ++k
    if `k' == 1  local slab "Employees"
    if `k' == 2  local slab "+ Unemployed"
    if `k' == 3  local slab "All active"

    capture confirm file "$DATA/`f'.dta"
    if _rc {
        display as error "Table A12: `f'.dta not found - `slab' columns skipped."
        continue
    }

    preserve
        use "$DATA/`f'.dta", clear

        quietly {
            drop if missing(dualvet_round)
            drop if essround == 11
            drop if agea > 65

            gen     edu3new = 1 if eduvet2 == 2 | inlist(edulvlb, 0, 113, 212, 213)
            replace edu3new = 2 if eduvet2 == 3 | inlist(edulvlb, 129, 221, 222, 223)
            replace edu3new = 3 if eduvet2 == 4

            label define edu3new_lbl 1 "General" 2 "Vocational (ref: General)" ///
                                     3 "Higher (ref: General)", replace
            label values edu3new edu3new_lbl
            label define urban_lbl 0 "Urban (No)" 1 "Urban (Yes)", replace
            label values urban urban_lbl
            label variable edu3new       "Educational attainment"
            label variable dualvet_round "Dual VET share"
            label variable agea          "Age"
            label variable age2          "Age squared"
            label variable gndr          "Sex"
            label variable rlgdgr        "Religiosity"
            label variable urban         "Urban"
        }

        quietly count if !missing(edu3new, farright, region, election, anweight)
        local nn = r(N)
        local nu = "n/a"
        capture confirm variable unemployed
        if !_rc {
            quietly count if unemployed == 1 & !missing(edu3new, farright, region, election, anweight)
            local nu = r(N)
        }
        local ns = "n/a"
        capture confirm variable selfemp
        if !_rc {
            quietly count if selfemp == 1 & !missing(edu3new, farright, region, election, anweight)
            local ns = r(N)
        }
        display as text "A12 `slab': N = `nn'  unemployed = `nu'  self-employed = `ns'"

        eststo A12_`k'a: quietly melogit farright i.edu3new ///
            [pweight = anweight] || region: || election:
        eststo A12_`k'b: quietly melogit farright i.edu3new $demo ///
            [pweight = anweight] || region: || election:
        eststo A12_`k'c: quietly melogit farright i.edu3new##c.dualvet_round $demo ///
            [pweight = anweight] || region: || election:

        local a12models "`a12models' A12_`k'a A12_`k'b A12_`k'c"
        local a12titles `"`a12titles' "`slab' M1" "`slab' M2" "`slab' M3""'
    restore
}

esttab `a12models' using "$TABLES/TableA12_sample_definition.rtf", replace ///
    b(3) se(3) label star(* 0.10 ** 0.05 *** 0.01) ///
    mtitles(`a12titles') ///
    nodepvars nogaps compress nobaselevels noomitted drop(/:*) ///
    stats(N ll, labels("Observations" "Log likelihood") fmt(0 3)) nonotes ///
    addnotes("M1 education only; M2 adds sociodemographic controls;" ///
             "M3 adds the education x dual VET interaction." ///
             "Models conditioning on current occupation are estimated on the" ///
             "employed sample only and are reported in Table 1." ///
             "Robust standard errors in parentheses" "*** p<0.01, ** p<0.05, * p<0.1")

display as result _n "TABLE A12  Sample definition"
capture noisily esttab `a12models', ///
    b(3) se(3) label star(* 0.10 ** 0.05 *** 0.01) ///
    mtitles(`a12titles') ///
    nodepvars nogaps compress nobaselevels noomitted drop(/:*) ///
    stats(N ll, labels("Observations" "Log likelihood") fmt(0 3))


*=============================================================================*
* 18. TABLE A13 - period heterogeneity: triple interaction and split sample
*     at the 2014 election year, parsimonious specification
*=============================================================================*

forvalues q = 0/1 {
    quietly count if period == `q' & !missing(edu3new, farright, region, election, anweight)
    local np = r(N)
    quietly levelsof election if period == `q' & !missing(edu3new, farright, anweight), local(pe)
    quietly levelsof cntry    if period == `q' & !missing(edu3new, farright, anweight), local(pc)
    display as text "A13 period `q': N = `np'  elections = " `: word count `pe'' ///
                    "  countries = " `: word count `pc''
}

eststo clear

eststo A13_tri: quietly melogit farright i.edu3new##c.dualvet_round##i.period $demo ///
    [pweight = anweight] || region: || election:

eststo A13_p0: quietly melogit farright i.edu3new##c.dualvet_round $demo ///
    if period == 0 [pweight = anweight] || region: || election:

eststo A13_p1: quietly melogit farright i.edu3new##c.dualvet_round $demo ///
    if period == 1 [pweight = anweight] || region: || election:

esttab A13_tri A13_p0 A13_p1 using "$TABLES/TableA13_period_heterogeneity.rtf", replace ///
    b(3) se(3) label star(* 0.10 ** 0.05 *** 0.01) ///
    mtitles("Triple, parsimonious" "To 2013, parsimonious" "2014+, parsimonious") ///
    nodepvars nogaps compress nobaselevels noomitted drop(/:*) ///
    stats(N ll, labels("Observations" "Log likelihood") fmt(0 3)) nonotes ///
    addnotes("Periods are defined by election year and cut at 2014." ///
             "Column 1 interacts education x dual VET with period;" ///
             "columns 2-3 split the sample, so the interaction coefficient in" ///
             "each column is that period's slope." ///
             "All columns report the parsimonious specification of Model 3, Table 1." ///
             "Robust standard errors in parentheses" "*** p<0.01, ** p<0.05, * p<0.1")

display as result _n "TABLE A13  Period heterogeneity"
capture noisily esttab A13_tri A13_p0 A13_p1, ///
    b(3) se(3) label star(* 0.10 ** 0.05 *** 0.01) ///
    mtitles("Triple, parsimonious" "To 2013, parsimonious" "2014+, parsimonious") ///
    nodepvars nogaps compress nobaselevels noomitted drop(/:*) ///
    stats(N ll, labels("Observations" "Log likelihood") fmt(0 3))


*=============================================================================*
* 19. FIGURE A1 - occupational probabilities for all education groups
*=============================================================================*

quietly gsem (i.occupation <- i.edu3new##c.dualvet_round agea age2 i.gndr i.urban ///
    M2[region]@1) [pweight = anweight], mlogit vce(robust)

margins edu3new, at(dualvet_round = (0(3)60))

marginsplot, bydimension(edu3new) byopts(rescale rows(1) legend(pos(6)) title("")) ///
    recast(line) recastci(rarea) ///
    ciopt(lpattern(solid) lcolor(black) acolor(gs13%40) lcolor(none)) ///
    plot1opts(lpattern(solid) lcolor(black)) ///
    plot2opts(lpattern(dot)   lcolor(black)) ///
    plot3opts(lpattern(dash)  lcolor(black)) ///
    title("") ytitle("{bf:Marginal probability}") ///
    xtitle("{bf:Dual VET pupils (% of total upper secondary pupils)}") ///
    plot(, label("Non-routine cognitive" "Routine" "Non-routine manual")) ///
    legend(rows(1) title("{bf:Occupation}", size(3.5))) ///
    xlabel(0 "0" 20 40 60 "60") name(figA1, replace)

graph save   "$FIGURES/FigureA1_occupation_all_edu.gph", replace
graph export "$FIGURES/FigureA1_occupation_all_edu.tif", width(4000) replace


*=============================================================================*
* 20. FIGURE A2 - occupational probabilities by education group and age band
*=============================================================================*

quietly mlogit occupation i.edu3new##c.dualvet_round##i.age_group ///
    i.gndr i.urban i.sec_5 unemp emprot_reg logGDPpc i.essround ///
    [pweight = anweight], vce(cluster regionround) baseoutcome(2)

margins edu3new, at(dualvet_round = (0(3)60) age_group = (1 2 3))

marginsplot, bydimension(edu3new age_group, ///
        labels("Gen. (18-34)" "Gen. (35-49)" "Gen. (50+)" ///
               "VET (18-34)" "VET (35-49)" "VET (50+)" ///
               "HE (18-34)"  "HE (35-49)"  "HE (50+)")) ///
    byopts(rescale legend() title("")) ///
    recast(line) recastci(rarea) ///
    ciopt(lpattern(solid) lcolor(black) acolor(gs13%40) lcolor(none)) ///
    plot1opts(lpattern(solid) lcolor(black)) ///
    plot2opts(lpattern(dot)   lcolor(black)) ///
    plot3opts(lpattern(dash)  lcolor(black)) ///
    title("") ytitle("{bf:Marginal probability}") ///
    xtitle("{bf:Dual VET pupils (% of total upper secondary pupils)}") ///
    plot(, label("Non-routine cognitive" "Routine" "Non-routine manual")) ///
    legend(title("{bf:Occupation}", size(3.5))) ///
    xlabel(0 "0" 20 40 60 "60") name(figA2, replace)

graph save   "$FIGURES/FigureA2_occupation_by_age_band.gph", replace
graph export "$FIGURES/FigureA2_occupation_by_age_band.tif", width(4000) replace


*=============================================================================*
* 21. FIGURES A3 to A5 - the same occupational models on the samples younger
*     than 45, 35 and 25
*=============================================================================*

local k = 3
foreach cut in 45 35 25 {
    quietly mlogit occupation i.edu3new##c.dualvet_round agea age2 i.gndr i.urban ///
        i.sec_5 unemp emprot_reg logGDPpc i.essround if agea < `cut' ///
        [pweight = anweight], vce(cluster regionround) baseoutcome(2)

    margins edu3new, at(dualvet_round = (0(3)60))

    marginsplot, bydimension(edu3new) ///
        byopts(rescale rows(1) legend(pos(6)) title("Less than `cut'")) ///
        recast(line) recastci(rarea) ///
        ciopt(lpattern(solid) lcolor(black) acolor(gs13%40) lcolor(none)) ///
        plot1opts(lpattern(solid) lcolor(black)) ///
        plot2opts(lpattern(dot)   lcolor(black)) ///
        plot3opts(lpattern(dash)  lcolor(black)) ///
        title("") ytitle("{bf:Marginal probability}") ///
        xtitle("{bf:Dual VET pupils (% of total upper secondary pupils)}") ///
        plot(, label("Non-routine cognitive" "Routine" "Non-routine manual")) ///
        legend(rows(1) title("{bf:Occupation}", size(3.5))) ///
        xlabel(0 "0" 20 40 60 "60") name(figA`k', replace)

    graph save   "$FIGURES/FigureA`k'_occupation_under`cut'.gph", replace
    graph export "$FIGURES/FigureA`k'_occupation_under`cut'.tif", width(4000) replace
    local ++k
}


*=============================================================================*
* 22. Completion log and inventory of the files produced
*=============================================================================*

global T1 = clock("`c(current_date)' `c(current_time)'", "DMY hms")
local mins = round((${T1} - ${T0}) / 60000, 0.1)

display as text _n "{hline 64}"
display as result "RUN COMPLETED"
display as text "Ended   : `c(current_date)' `c(current_time)'"
display as text "Elapsed : `mins' minutes"
display as text "Tables  : $TABLES"
display as text "Figures : $FIGURES"
display as text "{hline 64}"

local expected "Table1_vote_models Table2_occupation_models TableA1_country_RE TableA2_yearFE TableA3_occupation_yearFE TableA4_leave_one_out TableA5_inequality TableA6_country_year_FE TableA7_mechanisms TableA8_parental_background TableA9_no_AT_DE_CH TableA10_alternative_DV TableA11_dualvet_by_country TableA12_sample_definition TableA13_period_heterogeneity"

display as text _n "Tables written:"
local nmiss = 0
foreach t of local expected {
    capture confirm file "$TABLES/`t'.rtf"
    if !_rc  display as text "   ok       `t'.rtf"
    else {
        display as error "   MISSING   `t'.rtf"
        local ++nmiss
    }
}
if `nmiss' == 0  display as result _n "All `: word count `expected'' tables present."
else             display as error  _n "`nmiss' table(s) missing - check the log above."

local expfig "Figure1_vote_contrasts Figure2_occupation FigureA1_occupation_all_edu FigureA2_occupation_by_age_band FigureA3_occupation_under45 FigureA4_occupation_under35 FigureA5_occupation_under25"

display as text _n "Figures written:"
local nfmiss = 0
foreach f of local expfig {
    capture confirm file "$FIGURES/`f'.gph"
    if !_rc  display as text "   ok       `f'.gph"
    else {
        display as error "   MISSING   `f'.gph"
        local ++nfmiss
    }
}
if `nfmiss' == 0  display as result "All `: word count `expfig'' figures present."
else              display as error  "`nfmiss' figure(s) missing - check the log above."

tempname done
file open `done' using "$PROJECT/output/_ANALYSIS_COMPLETED.txt", write replace text
file write `done' "02_analysis.do completed `c(current_date)' `c(current_time)'" _n
file write `done' "vet_filter: dualvet_round" _n
file write `done' "elapsed_minutes: `mins'" _n
file write `done' "tables_missing: `nmiss'" _n
file write `done' "figures_missing: `nfmiss'" _n
file write `done' "tables: $TABLES" _n
file write `done' "log: $RUNLOG" _n
file close `done'

log close

*******************************************************************************
*  END
*******************************************************************************
