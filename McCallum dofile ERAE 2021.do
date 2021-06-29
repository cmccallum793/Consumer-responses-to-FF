// Code was ran in Stata15
cd "H:\cmccallum\ETH\Stata"

use "H:\cmccallum\PhD\Paper 2 – WTP to reduce food fraud\Data and do files\Data (fish con fixed).dta", clear

//Treatment=0 is SPVA, Treatment =1 BDM 
//Willingness to pay for each portion - HGHigh is pollan, HGLow is herring, HGRisk is riksy, HGAmbig is uncertain
// generate premiums 
gen PP_diff=HGHigh-HGHigh
gen PH_diff=HGHigh-HGLow
gen PR_diff=HGHigh-HGRisk
gen PA_diff=HGHigh-HGAmbig

//summary stats
// Table 2 - Premiums (mWTP)  
tabstat PH_diff PR_diff PA_diff, by(Treatment) s(N mean sd)

// Table 1 sample statistics
tabstat female income1 emp unemp retired caring student secondary college undergrad postgrad city, by(Treatment) s(N mean sd min max) 
tab agecat
tab agecat if Treatment==0 
tab agecat if Treatment==1

// Seemingly unrealted regression (Table 4, Model 2)
bootstrap, reps(1000): sureg (PH_diff BDM female age income1 college undergrad postgrad city fishconsumption LNheard frozen pollantried herringtried exptastePollan impprice impauth imptaste RA_a RN AA_a AN pollan_corrected) (PR_diff BDM female age income1 college undergrad postgrad city fishconsumption LNheard frozen pollantried herringtried exptastePollan impprice impauth imptaste RA_a RN AA_a AN pollan_corrected) (PA_diff BDM female age income1 college undergrad postgrad city fishconsumption LNheard frozen pollantried herringtried exptastePollan impprice impauth imptaste RA_a RN AA_a AN pollan_corrected)
outreg2 using sureg.doc, replace dec(3) pdec(2)

//change to long format for panel regression 
rename PP_diff HGdiff0
rename PH_diff HGdiff1
rename PR_diff HGdiff2
rename PA_diff HGdiff3
reshape long HGdiff, i(id) j(Situ)

xtset id Situ

drop if MPL==1 

gen pollan=1 if Situ==0
replace pollan=0 if Situ!=0 

gen herring=1 if Situ==1
replace herring=0 if Situ!=1 

gen risk=1 if Situ==2
replace risk=0 if Situ!=2 

gen unc=1 if Situ==3
replace unc=0 if Situ!=3

// GLS estimation Model 1, Table 3
xtgls HGdiff herring risk unc BDM, p(h)
outreg2 using xtglsf.doc, append pdec(2) dec(3)
test herring=risk
test herring=unc
test risk=unc


************************************************************************************
// Appendix 
use "H:\PhD\Paper 2 – WTP to reduce food fraud\Data and do files\Data (fish con fixed).dta", clear

//Appendix D (Table D1)- FGLS showing BDM=SPVA
reg PH_diff BDM female
predict uhat, resid
gen uhat2=uhat*uhat 
gen loguhat2=ln(uhat2)

reg loguhat2 BDM female
predict ghat, xb
gen hhat=exp(ghat)

reg PH_diff BDM female [aw=1/hhat], robust
outreg2 using HGdiff_fgls.doc, replace dec(3) pdec(2)

drop uhat uhat2 loguhat2 ghat hhat 

reg PR_diff BDM  female
predict uhat, resid
gen uhat2=uhat*uhat 
gen loguhat2=ln(uhat2)

reg loguhat2 BDM female
predict ghat, xb
gen hhat=exp(ghat)

reg PR_diff BDM female [aw=1/hhat], robust
outreg2 using HGdiff_fgls.doc, append dec(3) pdec(2)

drop uhat uhat2 loguhat2 ghat hhat 

reg PA_diff BDM female
predict uhat, resid
gen uhat2=uhat*uhat 
gen loguhat2=ln(uhat2)

reg loguhat2 BDM female
predict ghat, xb
gen hhat=exp(ghat)

reg PA_diff BDM female[aw=1/hhat], robust
outreg2 using HGdiff_fgls.doc, append dec(3) pdec(2)

drop uhat uhat2 loguhat2 ghat hhat 

reg RH_diff BDM  female
predict uhat, resid
gen uhat2=uhat*uhat 
gen loguhat2=ln(uhat2)

reg loguhat2 BDM female
predict ghat, xb
gen hhat=exp(ghat)

reg RH_diff BDM female[aw=1/hhat], robust
outreg2 using HGdiff_fgls.doc, append dec(3) pdec(2)

drop uhat uhat2 loguhat2 ghat hhat 

reg AH_diff BDM female
predict uhat, resid
gen uhat2=uhat*uhat 
gen loguhat2=ln(uhat2)

reg loguhat2 BDM female
predict ghat, xb
gen hhat=exp(ghat)

reg AH_diff BDM female [aw=1/hhat], robust
outreg2 using HGdiff_fgls.doc, append dec(3) pdec(2)

drop uhat uhat2 loguhat2 ghat hhat 



// Appendix I
//ols - Table I4
reg PH_diff RA_a RN AA_a AN pollan_corrected fishconsumption LNheard frozen pollantried herringtried exptastePollan impprice impauth imptaste female age income1 college undergrad postgrad city BDM, robust 
outreg2 using OLSreg.doc, replace pdec(2) dec(3)

reg PR_diff RA_a RN AA_a AN pollan_corrected fishconsumption LNheard frozen pollantried herringtried exptastePollan impprice impauth imptaste female age income1 college undergrad postgrad city BDM, robust 
outreg2 using OLSreg.doc, append pdec(2) dec(3)

reg PA_diff RA_a RN AA_a AN pollan_corrected fishconsumption LNheard frozen pollantried herringtried exptastePollan impprice impauth imptaste female age income1 college undergrad postgrad city BDM, robust 
outreg2 using OLSreg.doc, append pdec(2) dec(3)

// tobit - Table I5
tobit PH_diff RA_a RN AA_a AN pollan_corrected fishconsumption LNheard frozen pollantried herringtried exptastePollan impprice impauth imptaste female age income1 college undergrad postgrad city BDM, vce(robust) ll(0) 
outreg2 using OLSreg.doc, replace pdec(2) dec(3)

tobit PR_diff RA_a RN AA_a AN pollan_corrected fishconsumption LNheard frozen pollantried herringtried exptastePollan impprice impauth imptaste female age income1 college undergrad postgrad city BDM, vce(robust) ll(0) 
outreg2 using OLSreg.doc, append pdec(2) dec(3)

tobit PA_diff RA_a RN AA_a AN pollan_corrected fishconsumption LNheard frozen pollantried herringtried exptastePollan impprice impauth imptaste female age income1 college undergrad postgrad city BDM, vce(robust) ll(0)
outreg2 using OLSreg.doc, append pdec(2) dec(3)



// reshape long for Appendix 
cd "H:\ETH\Stata"
use "H:\cmccallum\PhD\Paper 2 – WTP to reduce food fraud\Data and do files\Data (fish con fixed).dta", clear

gen PP_diff=HGHigh-HGHigh
gen PH_diff=HGHigh-HGLow
gen PR_diff=HGHigh-HGRisk
gen PA_diff=HGHigh-HGAmbig

rename PP_diff HGdiff0
rename PH_diff HGdiff1
rename PR_diff HGdiff2
rename PA_diff HGdiff3
reshape long HGdiff, i(id) j(Situ)

xtset id Situ

drop if MPL==1 

gen pollan=1 if Situ==0
replace pollan=0 if Situ!=0 

gen herring=1 if Situ==1
replace herring=0 if Situ!=1 

gen risk=1 if Situ==2
replace risk=0 if Situ!=2 

gen unc=1 if Situ==3
replace unc=0 if Situ!=3

//Appendix E - Table E1
xtreg HGdiff herring risk unc, fe 
predict r, ue
swilk r
// Table E2 
xtreg HGdiff herring risk unc, fe 
xttest3 



// GLS no constant Model 1
// from revision to show it is equivelent
xtgls HGdiff herring risk unc, p(h) nocons
outreg2 using xtgls_nc.doc, replace pdec(2) dec(3)

xtgls HGdiff herring risk unc BDM, p(h) nocons
outreg2 using xtgls_nc.doc, append pdec(2) dec(3)
test herring=risk
test herring=unc
test risk=unc


// Appendix J - panel interaction model
xtgls HGdiff herring risk unc female BDM age income1 college undergrad postgrad city fishcons LNheard frozen pollantried herringtried exptastePollan imptaste impauth impprice RA RN AA AN pollan_corrected herring_BDM herring_female herring_age herring_income1 herring_college herring_undergrad herring_postgrad herring_city herring_fishcons herring_LNheard herring_frozen herring_ptried herring_htried herring_exptastep herring_taste herring_auth herring_price herring_RA herring_RN herring_AA herring_AN herring_subprob risk_BDM risk_female risk_age risk_income1 risk_college risk_undergrad risk_postgrad risk_city risk_fishcons risk_LNheard risk_frozen risk_ptried risk_htried risk_exptastep risk_taste risk_auth risk_price risk_RA risk_RN risk_AA risk_AN risk_subprob unc_BDM unc_female unc_age unc_income1 unc_college unc_undergrad unc_postgrad unc_city unc_fishcons unc_LNheard unc_frozen unc_ptried unc_htried unc_exptastep unc_taste unc_auth unc_price unc_RA unc_RN unc_AA unc_AN unc_subprob, p(h)
outreg2 using xtgls4BDM.doc, replace pdec(2) dec(3)

// check for colinearity - showing why panel interaction model is not suitable
reg HGdiff herring risk unc female BDM age income1 college undergrad postgrad city fishcons LNheard frozen pollantried herringtried exptastePollan imptaste impauth impprice RA RN AA AN pollan_corrected herring_BDM herring_female herring_age herring_income1 herring_college herring_undergrad herring_postgrad herring_city herring_fishcons herring_LNheard herring_frozen herring_ptried herring_htried herring_exptastep herring_taste herring_auth herring_price herring_RA herring_RN herring_AA herring_AN herring_subprob risk_BDM risk_female risk_age risk_income1 risk_college risk_undergrad risk_postgrad risk_city risk_fishcons risk_LNheard risk_frozen risk_ptried risk_htried risk_exptastep risk_taste risk_auth risk_price risk_RA risk_RN risk_AA risk_AN risk_subprob unc_BDM unc_female unc_age unc_income1 unc_college unc_undergrad unc_postgrad unc_city unc_fishcons unc_LNheard unc_frozen unc_ptried unc_htried unc_exptastep unc_taste unc_auth unc_price unc_RA unc_RN unc_AA unc_AN unc_subprob
vif
