cd "~/Private/"

use "diss_data_stata4.dta", clear

log using perm_marg.log, replace

program define perm_marg
version 13.1

qui xtreg lo_immigrant_dup c.stdlagconabst_pol2_dup##(c.terror_nr c.legal_nr)##i.p2001 ///
		laglo_immigrant_dup terror_r per_repub1_dup ib1.catpage lo_vcrime_rate ///
		stdleadpublic_dup i.reform logvocality_dup word_count_dup i.admin42 ///
		if dupflag==1, fe 
qui margins, at(terror_nr=(-3(.5)1) stdlagconabst_pol2_dup=(-1(1)1) p2001=(0 1)) ///
	asobserved post

test 1._at = 2._at

end

set seed 10
permute lo_immigrant_dup r(chi2), reps(1000) strata(org2) right nodots: perm_marg if dupflag==1

program drop perm_marg
program define perm_marg
version 13.1

qui xtreg lo_immigrant_dup c.stdlagconabst_pol2_dup##(c.terror_nr c.legal_nr)##i.p2001 ///
		laglo_immigrant_dup terror_r per_repub1_dup ib1.catpage lo_vcrime_rate ///
		stdleadpublic_dup i.reform logvocality_dup word_count_dup i.admin42 ///
		if dupflag==1, fe 
qui margins, at(terror_nr=(-3(.5)1) stdlagconabst_pol2_dup=(-1(1)1) p2001=(0 1)) ///
	asobserved post

test 3._at = 4._at

end

set seed 10
permute lo_immigrant_dup r(chi2), reps(1000) strata(org2) right nodots: perm_marg if dupflag==1

program drop perm_marg
program define perm_marg
version 13.1

qui xtreg lo_immigrant_dup c.stdlagconabst_pol2_dup##(c.terror_nr c.legal_nr)##i.p2001 ///
		laglo_immigrant_dup terror_r per_repub1_dup ib1.catpage lo_vcrime_rate ///
		stdleadpublic_dup i.reform logvocality_dup word_count_dup i.admin42 ///
		if dupflag==1, fe 
qui margins, at(terror_nr=(-3(.5)1) stdlagconabst_pol2_dup=(-1(1)1) p2001=(0 1)) ///
	asobserved post

test 5._at = 6._at

end

set seed 10
permute lo_immigrant_dup r(chi2), reps(1000) strata(org2) right nodots: perm_marg if dupflag==1

program drop perm_marg
program define perm_marg
version 13.1

qui xtreg lo_immigrant_dup c.stdlagconabst_pol2_dup##(c.terror_nr c.legal_nr)##i.p2001 ///
		laglo_immigrant_dup terror_r per_repub1_dup ib1.catpage lo_vcrime_rate ///
		stdleadpublic_dup i.reform logvocality_dup word_count_dup i.admin42 ///
		if dupflag==1, fe 
qui margins, at(terror_nr=(-3(.5)1) stdlagconabst_pol2_dup=(-1(1)1) p2001=(0 1)) ///
	asobserved post

test 7._at = 8._at

end

set seed 10
permute lo_immigrant_dup r(chi2), reps(1000) strata(org2) right nodots: perm_marg if dupflag==1

program drop perm_marg
program define perm_marg
version 13.1

qui xtreg lo_immigrant_dup c.stdlagconabst_pol2_dup##(c.terror_nr c.legal_nr)##i.p2001 ///
		laglo_immigrant_dup terror_r per_repub1_dup ib1.catpage lo_vcrime_rate ///
		stdleadpublic_dup i.reform logvocality_dup word_count_dup i.admin42 ///
		if dupflag==1, fe 
qui margins, at(terror_nr=(-3(.5)1) stdlagconabst_pol2_dup=(-1(1)1) p2001=(0 1)) ///
	asobserved post

test 9._at = 10._at

end

set seed 10
permute lo_immigrant_dup r(chi2), reps(1000) strata(org2) right nodots: perm_marg if dupflag==1

program drop perm_marg
program define perm_marg
version 13.1

qui xtreg lo_immigrant_dup c.stdlagconabst_pol2_dup##(c.terror_nr c.legal_nr)##i.p2001 ///
		laglo_immigrant_dup terror_r per_repub1_dup ib1.catpage lo_vcrime_rate ///
		stdleadpublic_dup i.reform logvocality_dup word_count_dup i.admin42 ///
		if dupflag==1, fe 
qui margins, at(terror_nr=(-3(.5)1) stdlagconabst_pol2_dup=(-1(1)1) p2001=(0 1)) ///
	asobserved post

test 11._at = 12._at

end

set seed 10
permute lo_immigrant_dup r(chi2), reps(1000) strata(org2) right nodots: perm_marg if dupflag==1

program drop perm_marg
program define perm_marg
version 13.1

qui xtreg lo_immigrant_dup c.stdlagconabst_pol2_dup##(c.terror_nr c.legal_nr)##i.p2001 ///
		laglo_immigrant_dup terror_r per_repub1_dup ib1.catpage lo_vcrime_rate ///
		stdleadpublic_dup i.reform logvocality_dup word_count_dup i.admin42 ///
		if dupflag==1, fe 
qui margins, at(terror_nr=(-3(.5)1) stdlagconabst_pol2_dup=(-1(1)1) p2001=(0 1)) ///
	asobserved post

test 13._at = 14._at

end

set seed 10
permute lo_immigrant_dup r(chi2), reps(1000) strata(org2) right nodots: perm_marg if dupflag==1

program drop perm_marg
program define perm_marg
version 13.1

qui xtreg lo_immigrant_dup c.stdlagconabst_pol2_dup##(c.terror_nr c.legal_nr)##i.p2001 ///
		laglo_immigrant_dup terror_r per_repub1_dup ib1.catpage lo_vcrime_rate ///
		stdleadpublic_dup i.reform logvocality_dup word_count_dup i.admin42 ///
		if dupflag==1, fe 
qui margins, at(terror_nr=(-3(.5)1) stdlagconabst_pol2_dup=(-1(1)1) p2001=(0 1)) ///
	asobserved post

test 15._at = 16._at

end

set seed 10
permute lo_immigrant_dup r(chi2), reps(1000) strata(org2) right nodots: perm_marg if dupflag==1

program drop perm_marg
program define perm_marg
version 13.1

qui xtreg lo_immigrant_dup c.stdlagconabst_pol2_dup##(c.terror_nr c.legal_nr)##i.p2001 ///
		laglo_immigrant_dup terror_r per_repub1_dup ib1.catpage lo_vcrime_rate ///
		stdleadpublic_dup i.reform logvocality_dup word_count_dup i.admin42 ///
		if dupflag==1, fe 
qui margins, at(terror_nr=(-3(.5)1) stdlagconabst_pol2_dup=(-1(1)1) p2001=(0 1)) ///
	asobserved post

test 17._at = 18._at

end

set seed 10
permute lo_immigrant_dup r(chi2), reps(1000) strata(org2) right nodots: perm_marg if dupflag==1

program drop perm_marg
program define perm_marg
version 13.1

qui xtreg lo_immigrant_dup c.stdlagconabst_pol2_dup##(c.terror_nr c.legal_nr)##i.p2001 ///
		laglo_immigrant_dup terror_r per_repub1_dup ib1.catpage lo_vcrime_rate ///
		stdleadpublic_dup i.reform logvocality_dup word_count_dup i.admin42 ///
		if dupflag==1, fe 
qui margins, at(terror_nr=(-3(.5)1) stdlagconabst_pol2_dup=(-1(1)1) p2001=(0 1)) ///
	asobserved post

test 19._at = 20._at

end

set seed 10
permute lo_immigrant_dup r(chi2), reps(1000) strata(org2) right nodots: perm_marg if dupflag==1

program drop perm_marg
program define perm_marg
version 13.1

qui xtreg lo_immigrant_dup c.stdlagconabst_pol2_dup##(c.terror_nr c.legal_nr)##i.p2001 ///
		laglo_immigrant_dup terror_r per_repub1_dup ib1.catpage lo_vcrime_rate ///
		stdleadpublic_dup i.reform logvocality_dup word_count_dup i.admin42 ///
		if dupflag==1, fe 
qui margins, at(terror_nr=(-3(.5)1) stdlagconabst_pol2_dup=(-1(1)1) p2001=(0 1)) ///
	asobserved post

test 21._at = 22._at

end

set seed 10
permute lo_immigrant_dup r(chi2), reps(1000) strata(org2) right nodots: perm_marg if dupflag==1

program drop perm_marg
program define perm_marg
version 13.1

qui xtreg lo_immigrant_dup c.stdlagconabst_pol2_dup##(c.terror_nr c.legal_nr)##i.p2001 ///
		laglo_immigrant_dup terror_r per_repub1_dup ib1.catpage lo_vcrime_rate ///
		stdleadpublic_dup i.reform logvocality_dup word_count_dup i.admin42 ///
		if dupflag==1, fe 
qui margins, at(terror_nr=(-3(.5)1) stdlagconabst_pol2_dup=(-1(1)1) p2001=(0 1)) ///
	asobserved post

test 23._at = 24._at

end

set seed 10
permute lo_immigrant_dup r(chi2), reps(1000) strata(org2) right nodots: perm_marg if dupflag==1

program drop perm_marg
program define perm_marg
version 13.1

qui xtreg lo_immigrant_dup c.stdlagconabst_pol2_dup##(c.terror_nr c.legal_nr)##i.p2001 ///
		laglo_immigrant_dup terror_r per_repub1_dup ib1.catpage lo_vcrime_rate ///
		stdleadpublic_dup i.reform logvocality_dup word_count_dup i.admin42 ///
		if dupflag==1, fe 
qui margins, at(terror_nr=(-3(.5)1) stdlagconabst_pol2_dup=(-1(1)1) p2001=(0 1)) ///
	asobserved post

test 25._at = 26._at

end

set seed 10
permute lo_immigrant_dup r(chi2), reps(1000) strata(org2) right nodots: perm_marg if dupflag==1

program drop perm_marg
program define perm_marg
version 13.1

qui xtreg lo_immigrant_dup c.stdlagconabst_pol2_dup##(c.terror_nr c.legal_nr)##i.p2001 ///
		laglo_immigrant_dup terror_r per_repub1_dup ib1.catpage lo_vcrime_rate ///
		stdleadpublic_dup i.reform logvocality_dup word_count_dup i.admin42 ///
		if dupflag==1, fe 
qui margins, at(terror_nr=(-3(.5)1) stdlagconabst_pol2_dup=(-1(1)1) p2001=(0 1)) ///
	asobserved post

test 27._at = 28._at

end

set seed 10
permute lo_immigrant_dup r(chi2), reps(1000) strata(org2) right nodots: perm_marg if dupflag==1

program drop perm_marg
program define perm_marg
version 13.1

qui xtreg lo_immigrant_dup c.stdlagconabst_pol2_dup##(c.terror_nr c.legal_nr)##i.p2001 ///
		laglo_immigrant_dup terror_r per_repub1_dup ib1.catpage lo_vcrime_rate ///
		stdleadpublic_dup i.reform logvocality_dup word_count_dup i.admin42 ///
		if dupflag==1, fe 
qui margins, at(terror_nr=(-3(.5)1) stdlagconabst_pol2_dup=(-1(1)1) p2001=(0 1)) ///
	asobserved post

test 29._at = 30._at

end

set seed 10
permute lo_immigrant_dup r(chi2), reps(1000) strata(org2) right nodots: perm_marg if dupflag==1

program drop perm_marg
program define perm_marg
version 13.1

qui xtreg lo_immigrant_dup c.stdlagconabst_pol2_dup##(c.terror_nr c.legal_nr)##i.p2001 ///
		laglo_immigrant_dup terror_r per_repub1_dup ib1.catpage lo_vcrime_rate ///
		stdleadpublic_dup i.reform logvocality_dup word_count_dup i.admin42 ///
		if dupflag==1, fe 
qui margins, at(terror_nr=(-3(.5)1) stdlagconabst_pol2_dup=(-1(1)1) p2001=(0 1)) ///
	asobserved post

test 31._at = 32._at

end

set seed 10
permute lo_immigrant_dup r(chi2), reps(1000) strata(org2) right nodots: perm_marg if dupflag==1

program drop perm_marg
program define perm_marg
version 13.1

qui xtreg lo_immigrant_dup c.stdlagconabst_pol2_dup##(c.terror_nr c.legal_nr)##i.p2001 ///
		laglo_immigrant_dup terror_r per_repub1_dup ib1.catpage lo_vcrime_rate ///
		stdleadpublic_dup i.reform logvocality_dup word_count_dup i.admin42 ///
		if dupflag==1, fe 
qui margins, at(terror_nr=(-3(.5)1) stdlagconabst_pol2_dup=(-1(1)1) p2001=(0 1)) ///
	asobserved post

test 33._at = 34._at

end

set seed 10
permute lo_immigrant_dup r(chi2), reps(1000) strata(org2) right nodots: perm_marg if dupflag==1

program drop perm_marg
program define perm_marg
version 13.1

qui xtreg lo_immigrant_dup c.stdlagconabst_pol2_dup##(c.terror_nr c.legal_nr)##i.p2001 ///
		laglo_immigrant_dup terror_r per_repub1_dup ib1.catpage lo_vcrime_rate ///
		stdleadpublic_dup i.reform logvocality_dup word_count_dup i.admin42 ///
		if dupflag==1, fe 
qui margins, at(terror_nr=(-3(.5)1) stdlagconabst_pol2_dup=(-1(1)1) p2001=(0 1)) ///
	asobserved post

test 35._at = 36._at

end

set seed 10
permute lo_immigrant_dup r(chi2), reps(1000) strata(org2) right nodots: perm_marg if dupflag==1

program drop perm_marg
program define perm_marg
version 13.1

qui xtreg lo_immigrant_dup c.stdlagconabst_pol2_dup##(c.terror_nr c.legal_nr)##i.p2001 ///
		laglo_immigrant_dup terror_r per_repub1_dup ib1.catpage lo_vcrime_rate ///
		stdleadpublic_dup i.reform logvocality_dup word_count_dup i.admin42 ///
		if dupflag==1, fe 
qui margins, at(terror_nr=(-3(.5)1) stdlagconabst_pol2_dup=(-1(1)1) p2001=(0 1)) ///
	asobserved post

test 37._at = 38._at

end

set seed 10
permute lo_immigrant_dup r(chi2), reps(1000) strata(org2) right nodots: perm_marg if dupflag==1

program drop perm_marg
program define perm_marg
version 13.1

qui xtreg lo_immigrant_dup c.stdlagconabst_pol2_dup##(c.terror_nr c.legal_nr)##i.p2001 ///
		laglo_immigrant_dup terror_r per_repub1_dup ib1.catpage lo_vcrime_rate ///
		stdleadpublic_dup i.reform logvocality_dup word_count_dup i.admin42 ///
		if dupflag==1, fe 
qui margins, at(terror_nr=(-3(.5)1) stdlagconabst_pol2_dup=(-1(1)1) p2001=(0 1)) ///
	asobserved post

test 39._at = 40._at

end

set seed 10
permute lo_immigrant_dup r(chi2), reps(1000) strata(org2) right nodots: perm_marg if dupflag==1

program drop perm_marg
program define perm_marg
version 13.1

qui xtreg lo_immigrant_dup c.stdlagconabst_pol2_dup##(c.terror_nr c.legal_nr)##i.p2001 ///
		laglo_immigrant_dup terror_r per_repub1_dup ib1.catpage lo_vcrime_rate ///
		stdleadpublic_dup i.reform logvocality_dup word_count_dup i.admin42 ///
		if dupflag==1, fe 
qui margins, at(terror_nr=(-3(.5)1) stdlagconabst_pol2_dup=(-1(1)1) p2001=(0 1)) ///
	asobserved post

test 41._at = 42._at

end

set seed 10
permute lo_immigrant_dup r(chi2), reps(1000) strata(org2) right nodots: perm_marg if dupflag==1

program drop perm_marg
program define perm_marg
version 13.1

qui xtreg lo_immigrant_dup c.stdlagconabst_pol2_dup##(c.terror_nr c.legal_nr)##i.p2001 ///
		laglo_immigrant_dup terror_r per_repub1_dup ib1.catpage lo_vcrime_rate ///
		stdleadpublic_dup i.reform logvocality_dup word_count_dup i.admin42 ///
		if dupflag==1, fe 
qui margins, at(terror_nr=(-3(.5)1) stdlagconabst_pol2_dup=(-1(1)1) p2001=(0 1)) ///
	asobserved post

test 43._at = 44._at

end

set seed 10
permute lo_immigrant_dup r(chi2), reps(1000) strata(org2) right nodots: perm_marg if dupflag==1

program drop perm_marg
program define perm_marg
version 13.1

qui xtreg lo_immigrant_dup c.stdlagconabst_pol2_dup##(c.terror_nr c.legal_nr)##i.p2001 ///
		laglo_immigrant_dup terror_r per_repub1_dup ib1.catpage lo_vcrime_rate ///
		stdleadpublic_dup i.reform logvocality_dup word_count_dup i.admin42 ///
		if dupflag==1, fe 
qui margins, at(terror_nr=(-3(.5)1) stdlagconabst_pol2_dup=(-1(1)1) p2001=(0 1)) ///
	asobserved post

test 45._at = 46._at

end

set seed 10
permute lo_immigrant_dup r(chi2), reps(1000) strata(org2) right nodots: perm_marg if dupflag==1

program drop perm_marg
program define perm_marg
version 13.1

qui xtreg lo_immigrant_dup c.stdlagconabst_pol2_dup##(c.terror_nr c.legal_nr)##i.p2001 ///
		laglo_immigrant_dup terror_r per_repub1_dup ib1.catpage lo_vcrime_rate ///
		stdleadpublic_dup i.reform logvocality_dup word_count_dup i.admin42 ///
		if dupflag==1, fe 
qui margins, at(terror_nr=(-3(.5)1) stdlagconabst_pol2_dup=(-1(1)1) p2001=(0 1)) ///
	asobserved post

test 47._at = 48._at

end

set seed 10
permute lo_immigrant_dup r(chi2), reps(1000) strata(org2) right nodots: perm_marg if dupflag==1

program drop perm_marg
program define perm_marg
version 13.1

qui xtreg lo_immigrant_dup c.stdlagconabst_pol2_dup##(c.terror_nr c.legal_nr)##i.p2001 ///
		laglo_immigrant_dup terror_r per_repub1_dup ib1.catpage lo_vcrime_rate ///
		stdleadpublic_dup i.reform logvocality_dup word_count_dup i.admin42 ///
		if dupflag==1, fe 
qui margins, at(terror_nr=(-3(.5)1) stdlagconabst_pol2_dup=(-1(1)1) p2001=(0 1)) ///
	asobserved post

test 49._at = 50._at

end

set seed 10
permute lo_immigrant_dup r(chi2), reps(1000) strata(org2) right nodots: perm_marg if dupflag==1

program drop perm_marg
program define perm_marg
version 13.1

qui xtreg lo_immigrant_dup c.stdlagconabst_pol2_dup##(c.terror_nr c.legal_nr)##i.p2001 ///
		laglo_immigrant_dup terror_r per_repub1_dup ib1.catpage lo_vcrime_rate ///
		stdleadpublic_dup i.reform logvocality_dup word_count_dup i.admin42 ///
		if dupflag==1, fe 
qui margins, at(terror_nr=(-3(.5)1) stdlagconabst_pol2_dup=(-1(1)1) p2001=(0 1)) ///
	asobserved post

test 51._at = 52._at

end

set seed 10
permute lo_immigrant_dup r(chi2), reps(1000) strata(org2) right nodots: perm_marg if dupflag==1

program drop perm_marg
program define perm_marg
version 13.1

qui xtreg lo_immigrant_dup c.stdlagconabst_pol2_dup##(c.terror_nr c.legal_nr)##i.p2001 ///
		laglo_immigrant_dup terror_r per_repub1_dup ib1.catpage lo_vcrime_rate ///
		stdleadpublic_dup i.reform logvocality_dup word_count_dup i.admin42 ///
		if dupflag==1, fe 
qui margins, at(terror_nr=(-3(.5)1) stdlagconabst_pol2_dup=(-1(1)1) p2001=(0 1)) ///
	asobserved post

test 53._at = 54._at

end

set seed 10
permute lo_immigrant_dup r(chi2), reps(1000) strata(org2) right nodots: perm_marg if dupflag==1

program drop perm_marg

log close

save "diss_data_stata4.dta", replace


