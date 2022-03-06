use "~/ecoproduce.dta"

browse
describe
tab ecoprc regprc

egen pricegroup = group(regprc ecoprc)
tab pricegroup 
browse ecoprc regprc pricegroup

gen pg1 = 1 if pricegroup == 1
replace pg1 = 0 if pricegroup != 1
gen pg2 = pricegroup == 2
gen pg3 = pricegroup == 3
gen pg4 = pricegroup == 4
gen pg5 = pricegroup == 5
gen pg6 = pricegroup == 6
gen pg7 = pricegroup == 7
gen pg8 = pricegroup == 8
gen pg9 = pricegroup == 9

quietly {
	eststo clear
	eststo: regress faminc pg1 pg2 pg3 pg4 pg5 pg6 pg7 pg8, r  
	eststo: regress hhsize pg1 pg2 pg3 pg4 pg5 pg6 pg7 pg8, r 
	eststo: regress educ pg1 pg2 pg3 pg4 pg5 pg6 pg7 pg8, r 
	eststo: regress age pg1 pg2 pg3 pg4 pg5 pg6 pg7 pg8, r  
}

esttab, compress 

reg ecolbs ecoprc regprc, r
predict ecoprc_hat if e(sample) 
tab ecoprc_hat 

regress ecolbs ecoprc regprc faminc hhsize educ age, r   
test faminc hhsize educ age

quietly {
	eststo clear
	regress ecolbs ecoprc regprc, r 
	eststo m1, title(Model 1) 
	regress ecolbs ecoprc faminc hhsize educ age, r 
	eststo m2, title(Model 2) 
	regress ecolbs ecoprc, r 
	eststo m3, title(Model 3) 
	regress ecolbs regprc ,r  
	eststo m4, title(Model 4)  
}

esttab m1 m3 m4, r2 mtitles nonumbers

corr ecoprc regprc 

gen ecobuy = 1 if ecolbs > 0
replace ecobuy = 0 if ecolbs == 0
tab ecobuy

quietly {
	eststo clear
	eststo: reg ecobuy ecoprc regprc faminc hhsize educ age, robust 
	predict ecobuy_lpm, xb
	gen ecobuytilde_lpm		= (ecobuy_lpm>0.5 & ecobuy==1)|(ecobuy_lpm<0.5 & ecobuy==0)
	summ ecobuytilde_lpm
	estadd scalar prop=r(mean)
	
	eststo: logit ecobuy ecoprc regprc faminc hhsize educ age
	predict ecobuy_log
	gen ecobuytilde_log		= (ecobuy_log>0.5 & ecobuy==1)|(ecobuy_log<0.5 & ecobuy==0)
	summ ecobuytilde_log
	estadd scalar prop=r(mean)
	
	eststo: probit ecobuy ecoprc regprc faminc hhsize educ age
	predict ecobuy_pro
	gen ecobuytilde_pro		= (ecobuy_pro>0.5 & ecobuy==1)|(ecobuy_pro<0.5 & ecobuy==0)
	summ ecobuytilde_pro
	estadd scalar prop=r(mean)
}

esttab, mtit("LPM" "Logit" "Probit") se nostar ///
	title("Model Five") ///
	scalars("prop PropCorrect" "ll Log Lik." "r2_p Pseudo R2" "r2 R2")

quietly {
	eststo clear
	reg ecobuy ecoprc regprc faminc hhsize educ age, robust
	eststo: margins, dydx(ecoprc regprc faminc hhsize educ age) atmeans post
	
	logit ecobuy ecoprc regprc faminc hhsize educ age
	eststo: margins, dydx(ecoprc regprc faminc hhsize educ age) atmeans post
	
	probit ecobuy ecoprc regprc faminc hhsize educ age
	eststo: margins, dydx(ecoprc regprc faminc hhsize educ age) atmeans post	
}

esttab, se nostar ///
	title("Model Five - Marginal Effects") ///
	mtit("LPM" "Logit" Probit) ///

count if ecobuy_lpm < 0 | ecobuy_lpm > 1 

generate lnfaminc = ln(faminc) 
regress ecolbs ecoprc regprc faminc hhsize educ age, r
regress ecolbs ecoprc regprc lnfaminc hhsize educ age, r	
