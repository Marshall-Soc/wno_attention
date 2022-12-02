//Some models for attention-focusing chapter

//The model
permute lo_immigrant_dup _b, reps(1000) seed(10) strata(org2): ///
xtreg lo_immigrant_dup c.stdlagconabst_pol2_dup##(c.lo_pop c.lo_legal) ///
	laglo_immigrant_dup lo_popr per_repub1_dup ib1.catpage lo_vcrime_rate ///
	stdleadpublic_dup vocality_dup word_count_dup i.admin42 if dupflag==1, fe 
margins, at(lo_pop=(-1(.5)1) stdlagconabst_pol2_dup=(-1(1)1)) asobserved post
marginsplot, noci
	
//Main effects aren't significant for any grievance
foreach i of varlist black govern immigrant sstate god {
	permute lo_`i'_dup _b, reps(1000) seed(10) strata(org2) nodots: ///
		xtreg lo_`i'_dup c.stdlagconabst_pol2_dup##(c.lo_pop c.lo_legal) ///
			laglo_`i'_dup lo_popr per_repub1_dup page_time2 lo_vcrime_rate ///
			stdleadpublic_dup vocality_dup word_count_dup i.admin42 if dupflag==1, fe 	
	}
	
//Not driven by 9/11 (would expect post-2001 to be more pronounced and therefore
	//sig. cross-product term)
permute lo_immigrant_dup _b, reps(1000) seed(10) strata(org2): ///
xtreg lo_immigrant_dup c.stdlagconabst_pol2_dup##(c.lo_pop c.lo_legal)##i.p2001 ///
	laglo_immigrant_dup lo_popr per_repub1_dup page_time2 lo_vcrime_rate ///
	stdleadpublic_dup vocality_dup word_count_dup i.admin42 if dupflag==1, fe 
	

//Adds in an immigration reform dummy.
set seed 10
permute lo_immigrant_dup _b, reps(1000) strata(org2): ///
	xtreg lo_immigrant_dup lo_pop if dupflag==1, fe 
permute lo_immigrant_dup _b, reps(1000) strata(org2): ///
	xtreg lo_immigrant_dup c.stdlagconabst_pol2_dup##c.lo_pop ///
		if dupflag==1, fe 
permute lo_immigrant_dup _b, reps(1000) strata(org2): ///
	xtreg lo_immigrant_dup c.stdlagconabst_pol2_dup##(c.lo_pop c.lo_legal) ///
		laglo_immigrant_dup lo_popr per_repub1_dup ib1.catpage lo_vcrime_rate ///
		stdleadpublic_dup i.reform logvocality_dup word_count_dup i.admin42 ///
		if dupflag==1, fe 
		
permute lo_immigrant_dup _b, reps(1000) strata(org2): ///
	xtreg lo_immigrant_dup c.stdlagconabst_pol2_dup##(c.lo_pop c.lo_kill) ///
		laglo_immigrant_dup lo_popr lo_rkill lo_legal per_repub1_dup ib1.catpage lo_vcrime_rate ///
		stdleadpublic_dup i.reform logvocality_dup word_count_dup i.admin42 ///
		if dupflag==1, fe //Alternative model with NRKill/Comm ratio added. Makes no difference
						//for effect of interest.
		
//Plots
mat p_model = J(2, 3, .)

xtreg lo_immigrant_dup lo_pop if dupflag==1, fe

mat def beta = e(b)
forvalues i = 1/2 {
	global i`i' = round(beta[1,`i'], .001)
	}
permute lo_immigrant_dup _b, reps(1000) strata(org2): ///
	xtreg lo_immigrant_dup lo_pop if dupflag==1, fe
mat def m1 = r(p)
mat def m12 = r(ci)
forvalues k = 1/2 {
	mat p_model[`k',1] = m1[1,`k']
	mat p_model[`k',2] = m12[1,`k']
	mat p_model[`k',3] = m12[2,`k']
	}
	
mat rownames p_model = lo_pop constant

mat p_model2 = J(6, 3, .)

xtreg lo_immigrant_dup c.stdlagconabst_pol2_dup##c.lo_pop if dupflag==1, fe

mat def beta = e(b)
forvalues j = 1/4 {
	global j`j' = round(beta[1,`j'], .001)
	}
permute lo_immigrant_dup _b, reps(1000) strata(org2): ///
	xtreg lo_immigrant_dup c.stdlagconabst_pol2_dup##c.lo_pop if dupflag==1, fe
mat def m1 = r(p)
mat def m12 = r(ci)
forvalues k = 1/4 {
	mat p_model2[`k',1] = m1[1,`k']
	mat p_model2[`k',2] = m12[1,`k']
	mat p_model2[`k',3] = m12[2,`k']
	}
	
mat rownames p_model2 = stdlagconabst_pol2_dup lo_pop ///
	c.stdlagconabst_pol2_dup#c.lo_pop constant
	
mat p_model3 = J(22, 3, .)

xtreg lo_immigrant_dup c.stdlagconabst_pol2_dup##(c.lo_pop c.lo_legal) ///
	laglo_immigrant_dup lo_popr per_repub1_dup ib1.catpage lo_vcrime_rate ///
	stdleadpublic_dup i.reform logvocality_dup word_count_dup i.admin42 ///
	if dupflag==1, fe 

mat def beta = e(b)
forvalues k = 1/22 {
	global k`k' = round(beta[1,`k'], .001)
	}
set seed 10
permute lo_immigrant_dup _b, reps(1000) seed(10) strata(org2): ///
	xtreg lo_immigrant_dup c.stdlagconabst_pol2_dup##(c.lo_pop c.lo_legal) ///
		laglo_immigrant_dup lo_popr per_repub1_dup ib1.catpage lo_vcrime_rate ///
		stdleadpublic_dup i.reform p2001 logvocality_dup word_count_dup i.admin42 ///
		if dupflag==1, fe //Continuous term for year as well?
mat def m1 = r(p)
mat def m12 = r(ci)
forvalues k = 1/22 {
	mat p_model3[`k',1] = m1[1,`k']
	mat p_model3[`k',2] = m12[1,`k']
	mat p_model3[`k',3] = m12[2,`k']
	}
	
mat rownames p_model3 = stdlagconabst_pol2_dup lo_pop lo_legal ///
	c.stdlagconabst_pol2_dup#c.lo_pop c.stdlagconabst_pol2_dup#c.lo_legal ///
	laglo_immigrant_dup lo_popr per_repub1_dup 1.catpage 2.catpage 3.catpage ///
	lo_vcrime_rate stdleadpublic_dup 1.reform 2.reform logvocality_dup ///
	word_count_dup 1.admin42 2.admin42 3.admin42 4.admin42 constant
	
#delimit ;

coefplot (matrix(p_model[,1]), ci((p_model[,2] p_model[,3])) bcolor(gs14)) 
	(matrix(p_model2[,1]), ci((p_model2[,2] p_model2[,3])) bcolor(gs7))
	(matrix(p_model3[,1]), ci((p_model3[,2] p_model3[,3])) bcolor(black)), 
	graphregion(fcolor(white) lcolor(white) lwidth(thick)) keep(lo_pop stdlagconabst_pol2_dup
	c.stdlagconabst_pol2_dup#c.lo_pop constant) coeflabels(
	stdlagconabst_pol2_dup=`""{bf:Construal Style (std)}" "{&beta}2 = ${j1}" "{&beta}3 = ${k1}""'
	lo_pop=`""{bf:NRTerror/Comm (log)}" "{&beta}1 = ${i1}" "{&beta}2 = ${j2}" "{&beta}3 = ${k2}""'
	c.stdlagconabst_pol2_dup#c.lo_pop=`""{bf:NRTerror/Comm * Const}" "{&beta}2 = ${j4}" "{&beta}3 = ${k3}""'
	constant=`""{bf:Constant}" "{&beta}1 = ${i2}" "{&beta}2 = ${j4}" "{&beta}3 = ${k22}""', 
	labsize(vsmall))
	order(lo_pop stdlagconabst_pol2_dup c.stdlagconabst_pol2_dup#c.lo_pop constant)
	ciopts(lcolor(black) recast(rcap rcap rcap))
	recast(bar) vertical yline(0.05, lcolor(black))
	ytitle("{it:P}-Value") barwidth(.2) fcolor(*.8) citop xlabel(, labsize(vsmall)) 
	legend(order(1 "Model 1" 3 "Model 2" 5 "Model 3") rows(1)) saving(ch4plot.gph, replace) ;
	
#delimit cr

#delimit ;

coefplot (matrix(p_model[,1]), ci((p_model[,2] p_model[,3])) bcolor(gs14)) 
	(matrix(p_model2[,1]), ci((p_model2[,2] p_model2[,3])) bcolor(gs7))
	(matrix(p_model3[,1]), ci((p_model3[,2] p_model3[,3])) bcolor(black)), 
	graphregion(fcolor(white) lcolor(white) lwidth(thick)) drop(2.catpage 1.reform 
	1.admin42) coeflabels(
	stdlagconabst_pol2_dup=`""{bf:Construal Style (std)}" "{&beta}1 = ${i1}" "{&beta}2 = ${j1}" "{&beta}3 = ${k1}""'
	lo_pop=`""{bf:NRTerror/Comm (log)}" "{&beta}2 = ${j2}" "{&beta}3 = ${k2}""'
	lo_legal=`""{bf:Legal/Comm (log}" "{&beta}2 = ${j3}" "{&beta}3 = ${k3}""'
	c.stdlagconabst_pol2_dup#c.lo_pop=`""{bf:NRTerror/Comm * Const}" "{&beta}2 = ${j4}" "{&beta}3 = ${k4}""'
	c.stdlagconabst_pol2_dup#c.lo_legal=`""{bf:Legal/Comm * Const}" "{&beta}2 = ${j5}" "{&beta}3 = ${k5}""'
	laglo_immigrant_dup=`""{bf:Immigrant (lag)}" "{&beta}3 = ${k6}""'
	lo_popr=`""{bf:RTerror/Comm (log)}" "{&beta}3 = ${k7}"'
	per_repub1_dup=`""{bf:% Rep Voting}" "{&beta}3 = ${k8}"'
	1.catpage=`""{bf:0 Name-Drops}" "{&beta}3 = ${k9}"'
	3.catpage=`""{bf:>4 Name-Drops}" "{&beta}3 = ${k11}"'
	lo_vcrime_rate=`""{bf:Violent Crime Rate (log)}" "{&beta}3 = ${k12}"'
	stdleadpublic_dup=`""{bf:Public Awareness}" "{&beta}3 = ${k13}"'
	2.reform=`""{bf:Imm Legislation}" "{&beta}3 = ${k15}"'
	logvocality_dup=`""{bf:Vocality (log)}" "{&beta}3 = ${k16}"'
	word_count_dup=`""{bf:(Avg) Word Count}" "{&beta}3 = ${k17}"'
	2.admin42=`""{bf:H.W. Bush}" "{&beta}3 = ${k19}"'
	3.admin42=`""{bf:Reagan}" "{&beta}3 = ${k20}"'
	4.admin42=`""{bf:W. Bush}" "{&beta}3 = ${k21}"'
	constant=`""{bf:Constant}" "{&beta}1 = ${i2}" "{&beta}2 = ${j6}" "{&beta}3 = ${k22}""', 
	labsize(vsmall) headings(1.catpage="{bf:Peer Attention}" 2.admin42="{bf:Pres Admin}", nogap) 
	groups(1.catpage 3.catpage = " " 2.admin42 3.admin42 4.admin42 = " ") 
	order(lo_pop stdlagconabst_pol2_dup lo_legal c.stdlagconabst_pol2_dup#c.lo_pop 
	c.stdlagconabst_pol2_dup#c.lo_legal laglo_immigrant_dup lo_popr per_repub1_dup
	?.catpage lo_vcrime_rate stdleadpublic_dup ?.reform logvocality_dup
	word_count_dup ?.admin42 constant)
	ciopts(lcolor(black) recast(rcap rcap))
	recast(bar) vertical yline(0.05, lcolor(black))
	ytitle("{it:P}-Value") barwidth(.3) fcolor(*.8) citop xlabel(, labsize(vsmall)
	angle(90)) legend(order(1 "Model 1" 3 "Model 2" 5 "Model 3")) saving(ch4plot.gph, replace) ;
	
#delimit cr
	
sort org year
by org: gen id = _n
egen concat = concat(org2 year) 
destring concat, gen(concat2)
sort org concat2
by org: gen lag1 = lo_immigrant_dup[_n-1] if concat2==concat2[_n-1]+1
by org: gen lag2 = lo_immigrant_dup[_n-2] if concat2==concat2[_n-2]+2

foreach i of varlist stdlagconabst_pol2_dup terror_nr {
	permute `i' _b, reps(1000) strata(org2): ///
		reg `i' lag1 lag2 if lagflag==1
	}

sort org year
by org: egen lopop_m = mean(lo_pop)
by org: gen lopop_y = lo_pop - lopop_m
by org: egen conabst_m = mean(stdlagconabst_pol2_dup)
by org: gen conabst_y = stdlagconabst_pol2_dup - conabst_m

permute lo_immigrant_dup _b, reps(1000) strata(org2): ///
reg lo_immigrant_dup lopop_y lopop_m conabst_y conabst_m c.lopop_y#c.conabst_y ///
	i.admin42 i.admin42#(c.lopop_m c.conabst_m)
	
sort org year
foreach i of varlist laglo_black_dup laglo_govern_dup ///
	laglo_immigrant_dup laglo_sstate_dup laglo_god_dup {
		gen cat2`i' = .
		by org: egen m_`i' = mean(`i') 
		qui sum `i' 
		replace cat2`i' = 0 if m_`i' < r(mean)
		replace cat2`i' = 1 if m_`i' >= r(mean)
		}

rename cat2laglo_black_dup catlagblack2
rename cat2laglo_govern_dup catlaggov2
rename cat2laglo_immigrant_dup catlagimm2
rename cat2laglo_sstate_dup catlagss2
rename cat2laglo_god_dup catlaggod2

foreach i of varlist catlagblack2 catlaggov2 catlagimm2 catlagss2 catlaggod2 {
	permute lo_immigrant_dup _b, reps(1000) strata(org2): ///
		xtreg lo_immigrant_dup c.stdlagconabst_pol2_dup##(c.lo_pop c.lo_legal) ///
			lo_popr per_repub1_dup ib1.catpage lo_vcrime_rate ///
			stdleadpublic_dup i.reform logvocality_dup word_count_dup i.admin42 ///
			if dupflag==1 & `i'==0, fe
	} 
	


	
