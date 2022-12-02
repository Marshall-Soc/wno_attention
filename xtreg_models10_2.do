permute lo_immigrant_dup _b, reps(1000) seed(10) strata(org2): ///
	xtreg lo_immigrant_dup c.stdlagconabst_pol2_dup##(c.terror_nr c.legal_nr) ///
		laglo_immigrant_dup terror_r per_repub1_dup ib1.catpage lo_vcrime_rate ///
		stdleadpublic_dup i.reform p2001 logvocality_dup word_count_dup i.admin42 ///
		if dupflag==1, fe 
margins, at(terror_nr=(-3(.5)1) stdlagconabst_pol2_dup=(-1(1)1)) asobserved post
marginsplot, noci
		
mat p_model3 = J(23, 3, .)

xtreg lo_immigrant_dup c.stdlagconabst_pol2_dup##(c.terror_nr c.legal_nr) ///
	laglo_immigrant_dup terror_r per_repub1_dup ib1.catpage lo_vcrime_rate ///
	stdleadpublic_dup i.reform p2001 logvocality_dup word_count_dup i.admin42 ///
	if dupflag==1, fe 

mat def beta = e(b)
forvalues k = 1/22 {
	global k`k' = round(beta[1,`k'], .001)
	}
set seed 10
permute lo_immigrant_dup _b, reps(1000) seed(10) strata(org2): ///
	xtreg lo_immigrant_dup c.stdlagconabst_pol2_dup##(c.terror_nr c.legal_nr) ///
		laglo_immigrant_dup terror_r per_repub1_dup ib1.catpage lo_vcrime_rate ///
		stdleadpublic_dup i.reform p2001 logvocality_dup word_count_dup i.admin42 ///
		if dupflag==1, fe
mat def m1 = r(p)
mat def m12 = r(ci)
forvalues k = 1/22 {
	mat p_model3[`k',1] = m1[1,`k']
	mat p_model3[`k',2] = m12[1,`k']
	mat p_model3[`k',3] = m12[2,`k']
	}
	
mat rownames p_model3 = stdlagconabst_pol2_dup terror_nr legal_nr ///
	c.stdlagconabst_pol2_dup#c.lo_pop c.stdlagconabst_pol2_dup#c.lo_legal ///
	laglo_immigrant_dup teror_r per_repub1_dup 1.catpage 2.catpage 3.catpage ///
	lo_vcrime_rate stdleadpublic_dup 1.reform 2.reform p2001 logvocality_dup ///
	word_count_dup 1.admin42 2.admin42 3.admin42 4.admin42 constant
	
set seed 10
permute lo_immigrant_dup _b, reps(1000) seed(10) strata(org2): ///
	xtreg lo_immigrant_dup c.stdlagconabst_pol2_dup##(c.kill_nr c.terror_nr c.legal_nr) ///
		laglo_immigrant_dup terror_r kill_r per_repub1_dup ib1.catpage lo_vcrime_rate ///
		stdleadpublic_dup i.reform p2001 logvocality_dup word_count_dup i.admin42 ///
		if dupflag==1, fe 
		
	*9/11 bias? No.
set seed 10
permute lo_immigrant_dup _b, reps(1000) seed(10) strata(org2): ///
	xtreg lo_immigrant_dup c.stdlagconabst_pol2_dup##(c.terror_nr c.legal_nr) ///
		laglo_immigrant_dup terror_r per_repub1_dup ib1.catpage lo_vcrime_rate ///
		stdleadpublic_dup i.reform p2001 logvocality_dup word_count_dup i.admin42 ///
		if dupflag==1 & year!=2002, fe 
margins, at(terror_nr=(-3(.5)1) stdlagconabst_pol2_dup=(-1(1)1)) asobserved post
marginsplot, noci

set seed 10
permute lo_immigrant_dup _b, reps(1000) seed(10) strata(org2): ///
	xtreg lo_immigrant_dup c.stdlagconabst_pol2_dup##(c.terror_nr c.legal_nr)##i.p2001 ///
		laglo_immigrant_dup terror_r per_repub1_dup ib1.catpage lo_vcrime_rate ///
		stdleadpublic_dup i.reform logvocality_dup word_count_dup i.admin42 ///
		if dupflag==1, fe 
margins, at(terror_nr=(-3(.5)1) stdlagconabst_pol2_dup=(-1(1)1) p2001=1) asobserved post
marginsplot, noci
