***Code for Chapter 5.

*Marshall A. Taylor

//NOTE ABOUT ENTREP VARIABLE: This variable was originally constructed so that
	//any entrepreneur present during org's time range as represented by the 
	//sampled documents gave the doc a 1 (0 if otherwise). This could have been
	//problematic for the ZIP models below, since the DV in those models is 
	//the org-year and not the org (as it is in the BE models). However, all
	//orgs coded as having an entrepreneur either (1) had an entrepreneur that
	//was present during the entire time span, (2) had immediate change over in
	//entrepreneurs, as with the NA after Pierce died, or (3) there wasn't 
	//sufficient information to suggest that the person was not affiliated with
	//the organization during the time span (such as with John Tanton and AIC).

//Getting models for paper
	//ZIP models
set seed 10
permute page_time2 _b, reps(1000): ///
	zip page_time2 i.entrep2 if flag==1, ///
		inflate(i.admin42 founding docsn outpage_time2) vuong   
set seed 10
permute page_time2 _b, reps(1000): ///
	zip page_time2 i.entrep2 c.stdfear2 c.stdanger2 c.stdpos if flag==1, ///
		inflate(i.admin42 founding docsn outpage_time2) vuong 
set seed 10
permute page_time2 _b, reps(1000): ///
	zip page_time2 i.entrep2##(c.stdfear2 c.stdanger2 c.stdpos) if flag==1, ///
		inflate(i.admin42 founding docsn outpage_time2) vuong  
set seed 10
permute page_time2 _b, reps(1000): ///
	zip page_time2 i.entrep2##(c.stdfear2 c.stdanger2 c.stdpos) ///
		outpage_time2 govern immigrant sstate god (c.terror c.stdleadp)##c.docsn2 ///
		i.adminzip docsn if flag==1, inflate(i.admin42 founding docsn ///
		outpage_time2) vuong //Vuong test prefers zip. Also, No appreciable differences 
						//with NFP collapsed into CSCS. Would only impact docsn
						//variable anyway, since the unit in these models is
						//the org-year and CSCS and NFP do not overlap in the sample.
						//But it doesn't really impact the BE models either (see note
						//below). Results are still sig and in the same direction
						//when removing AIC and/or American Policy Institute
						//(though I have to remove another variable in order for
						//model to converge when taking out AIC [in this case, no admin42
						//in the inflation model]--when I do, the results are consistent).
						//(FYI: Results are consistent with admin42 removed from
						//inflation model. I'll also point out that a zinb converges
						//on this model w/o removing admin42 from the inflate model, and 
						//results are consistent.) Results consistent with 2000s docs, but
						//I have to remove the interaction in the control variable
						//specification in order for it to converge. However, it
						//won't converge at all with 1990s docs...
						//Consistent results when omitting questionable "white nationalist" groups,
						//namely SCV, AIC, API.
						
						//Another note about the full model: Fit stats much
						 //prefer this control variable specification over the
						 //other plausible ones I tried out.
						 
						//One more note: docsn and docsn2 are right-skewed, but
						 //no appreciable differences with logged versions
						 //(log[docsn2+1] in the case of docsn2 because of zeros).
						 
permute logpage2 _b, reps(1000): ///
	xtreg logpage2 i.entrep2##(c.stdfear2 c.stdanger2 c.stdpos) ///
		logopage2 govern immigrant sstate god (c.terror c.stdleadp)##c.docsn2 ///
		i.adminzip docsn founding if flag==1 & page_time2!=0, i(level_id2) be //Results are similar in
														//magnitude and direction
														//when running as a BE model when 
														//page_time2=0 (so conditional on
														//obs having at least 1 name-drop,
														//which is comparable to the log count
														//estimates in the ZIP models).
						 
	//Issue with clustering? Doesn't appear to be the case.
set seed 100
permute page_time2 _b, reps(100) strata(org2): ///
	zip page_time2 i.entrep2##(c.stdfear2 c.stdanger2 c.stdpos) ///
		outpage_time2 govern immigrant sstate god (c.terror c.stdleadp)##c.docsn2 ///
		i.adminzip docsn if flag==1, inflate(i.admin42 founding docsn ///
		outpage_time2) vuong

	//BE models
set seed 10
permute stdpage_nod _b, reps(1000): ///	
	xtreg stdpage_nod i.entrep2 if flag==1, be
set seed 10
permute stdpage_nod _b, reps(1000): ///	
	xtreg stdpage_nod i.entrep2 c.stdfear2 c.stdanger2 c.stdpos ///
		if flag==1, be
set seed 10
permute stdpage_nod _b, reps(1000): ///	
	xtreg stdpage_nod i.entrep2##(c.stdfear2 c.stdanger2 c.stdpos) ///
		if flag==1, be
set seed 10
permute stdpage_nod _b, reps(1000): ///	
	xtreg stdpage_nod i.entrep2##(c.stdfear2 c.stdanger2 c.stdpos) ///
		stdopage_nod govern immigrant sstate god terror c.stdleadp ///
		i.adminzip founding if flag==1, be //No appreciable differences w/ NFP collapsed into CSCS,
									//though effect sizes are smaller.
									//Fear*leader term is  still sig and in the same 
									//direction when removing AIC and API. Anger*leader
									//term is also sig and POSITIVE when removing both
									//AIC and API, but the effect is still much smaller
									//than the fear*leader effect (8.0 vs. 3.7). This means
									//the effect of fear for WNOs w/ E-I leaders is still
									//considerably bigger than the effect of anger for these
									//groups, even with the latter now being positive and
									//sig (7.1 vs. -1.04). Also, in the actual model,
									////Wald tests (asymptotically-derived)
									//suggest difference in effects of fear and anger
									//for groups with E-I leaders (test stdanger2+(1.entrep2#c.stdanger2) 
									//= stdfear2+(1.entrep2#c.stdfear2)). The results
									//are also consistent if I remove those cases
									//that produce crazy high predicted values. 
									//Results fairly consistent when limiting analysis
									//to docs in the 2000s; consistent and marginally
									//sig for docs in the 1990s. Consistent results
									//when omitting questionable "white nationalist" groups,
									//namely SCV, AIC, API.
									
									//Also, logging the page variables make no significant difference.
		
	//Margins after full ZIP model
margins 1.entrep2, at(stdfear2=(-1(.5)3)) asobserved 
														
margins 1.entrep2, at(stdanger2=(-1(.5)3)) asobserved

	//Margins after full BE model
margins, at(entrep2=1 stdfear2=(-.5(.1).5)) asobserved 
margins, at(entrep2=1 stdanger2=(-.5(.1).5)) asobserved 
margins, at(entrep2=0 stdfear2=(-.5(.1).5)) asobserved 
margins, at(entrep2=0 stdanger2=(-.5(.1).5)) asobserved 
		
//Different specifications of the emotion variables.
	//Weak correlations between these measures. The NRC estimates are quite different,
		//perhaps of the time difference in construction of that dictionary and
		//the production of many of these texts (late 80s, 90s, and early to mid
		//2000s). The ML estimates were in the right direction but not sig after
		//permutations. To see if there was any common variance across these times
		//(presumably captuing the "true" measure of fearness and angerness net of measurement
		//error across the measures), I performed ML factor 
		//analysis on these three indicators. Got an eigenvalue
		//of .9 for the fear vars--so not ideal, but not terrible either.
		//Got an eigenvalue of over 1 for the anger variable. If I plug in the 
		//factor variable versions, the effects are in the same direction as 
		//the actual vars. I chose to stick with the emotion variables I originally 
		//used since the effects that are sig are in the same direction, of similar magnitude,
		//and because the original vars load highest on the factor--presumably
		//indicating that, even with measurement error, it is the "closest" to
		//measuring the emotions in question. Also, fit stats consistently favor
		//original vars.
		
	//stdanger2 & stdfear2: uses WordNet-Affect, as supplied by Tim Jurka. (This
		//is the actual model.)
permute page_time2 _b, reps(200): ///
	zip page_time2 i.entrep2##(c.stdfear2 c.stdanger2 c.stdpos) ///
		outpage_time2 govern immigrant sstate god (c.terror c.stdleadp)##c.docsn2 ///
		i.adminzip docsn if flag==1, inflate(i.admin42 founding docsn ///
		outpage_time2) 
		
	//stdangernrc & stdfearnrc: uses the NRC lexicon. Like stdfear2 and stdanger2,
		//the counts are divided by td_freq (word_count in Stata).
permute page_time2 _b, reps(1000): ///
	zip page_time2 i.entrep2##(c.stdfearnrc c.stdangernrc c.stdpos) ///
		outpage_time2 govern immigrant sstate god (c.terror c.stdleadp)##c.docsn2 ///
		i.adminzip docsn if flag==1, inflate(i.admin42 founding docsn ///
		outpage_time2) 
		
	//stdangerml & stdfearml: uses Jurka's implementation of the WordNet-Affect
		//lexicon, but computing using a naive Bayes classifier.
permute page_time2 _b, reps(1000): ///
	zip page_time2 i.entrep2##(c.stdfearml c.stdangerml c.stdpos) ///
		outpage_time2 govern immigrant sstate god (c.terror c.stdleadp)##c.docsn2 ///
		i.adminzip docsn if flag==1, inflate(i.admin42 founding docsn ///
		outpage_time2)    
		
	//fanger & ffear: latent variable from maximum-likelihood factor analysis
		//with the above three measures as indicators.
permute page_time2 _b, reps(200): ///
	zip page_time2 i.entrep2##(c.ffear c.fanger c.stdpos) ///
		outpage_time2 govern immigrant sstate god (c.terror c.stdleadp)##c.docsn2 ///
		i.adminzip docsn if flag==1, inflate(i.admin42 founding docsn ///
		outpage_time2)  

//Robustness checks for bias from particularly powerful orgs that have
	//many docs in the corpus: within-org analysis. Since the variable of interest
	//is an org-year variable, an FE should still model this variation--just netting
	//out between-org variance. Of course, can't estimate a main effect
	//for entrepreneurship, so also estimates a RE model. Benefit of both is that they
	//partition variance within and between orgs. Effects of interest
	//are in the same direction and sig (at .1 for the FE model for the leader*fear
	//term. Defends against worry that between-org
	//variance could be biasing estimates, since the sample is so unbalanced across
	//org-years. Results are also consistent if NFP is collapsed into CSCS. Also
	//consistent results if API and AIC are removed from the sample (marginally significant
	//fear*leader term [about .07] after permutations). 
permute page_time2 _b, reps(1000) strata(org2): ///
	xtpoisson page_time2 i.entrep2##(c.stdfear2 c.stdanger2 c.stdpos) ///
		outpage_time2 govern immigrant sstate god (c.terror c.stdleadp)##c.docsn2 ///
		i.adminzip year docsn if flag==1, fe
		
permute page_time2 _b, reps(1000): ///
	xtpoisson page_time2 i.entrep2##(c.stdfear2 c.stdanger2 c.stdpos) ///
		outpage_time2 govern immigrant sstate god (c.terror c.stdleadp)##c.docsn2 ///
		i.adminzip year docsn if flag==1, re
		
	//Can also just remove EURO and NA, since they both have a lot of docs. 
		//Problem here is that I have to remove a variable in order for the model
		//to converge (in this case, i.admin42 from the inflation model). When
		//I do, though, results are consistent for vars of interest (though with
		//a p = .164 for leader*fear; see the footnote in Chapter 5 for how I
		//deal with this).
qui zip page_time2 i.entrep2##(c.stdfear2 c.stdanger2 c.stdpos) ///
	outpage_time2 govern immigrant sstate god (c.terror c.stdleadp)##c.docsn2 ///
	i.adminzip docsn if flag==1 & org!="David Duke/Euro-American Unity and Rights", ///
	inflate(docsn outpage_time2) vuong
gen robust1 = e(sample)
set seed 100
permute page_time2 _b, reps(1000): ///
	zip page_time2 i.entrep2##(c.stdfear2 c.stdanger2 c.stdpos) ///
		outpage_time2 govern immigrant sstate god (c.terror c.stdleadp)##c.docsn2 ///
		i.adminzip docsn if robust1==1, inflate(docsn ///
		outpage_time2) vuong

zip page_time2 i.entrep2##(c.stdfear2 c.stdanger2 c.stdpos) ///
	outpage_time2 govern immigrant sstate god (c.terror c.stdleadp)##c.docsn2 ///
	i.adminzip docsn if flag==1 & org!="National Alliance", ///
	inflate(docsn outpage_time2) vuong
gen robust2 = e(sample)
set seed 100
permute page_time2 _b, reps(1000): ///
	zip page_time2 i.entrep2##(c.stdfear2 c.stdanger2 c.stdpos) ///
		outpage_time2 govern immigrant sstate god (c.terror c.stdleadp)##c.docsn2 ///
		i.adminzip docsn if robust2==1, inflate(docsn ///
		outpage_time2) vuong
		
	//Check agains outlier bias as it relates to core theoretical variables.
		//Seems fine. Went with |x| < 3 b/c model would not converge at 1 or 2
		//without making the model a little more parsimonious, typically by
		//removing a variable from the inflation model (though, when I remove
		//admin42, |x| < 2 produces similar results).
permute page_time2 _b, reps(1000): ///
	zip page_time2 i.entrep2##(c.stdfear2 c.stdanger2 c.stdpos) ///
		outpage_time2 govern immigrant sstate god (c.terror c.stdleadp)##c.docsn2 ///
		i.adminzip docsn if flag==1 & abs(stdfear2)<2, inflate(i.admin42 founding docsn ///
		outpage_time2) vuong
permute page_time2 _b, reps(1000): ///
	zip page_time2 i.entrep2##(c.stdfear2 c.stdanger2 c.stdpos) ///
		outpage_time2 govern immigrant sstate god (c.terror c.stdleadp)##c.docsn2 ///
		i.adminzip docsn if flag==1 & abs(stdanger2)<2, inflate(i.admin42 founding docsn ///
		outpage_time2) vuong

//Robustness checks for BE models.
	//Modeling the mean OK? Seems to be.
permute pagecat _b, reps(1000): ///	
	xtreg pagecat i.entrep2##(c.stdfear2 c.stdanger2 c.stdpos) ///
		opagecat govern immigrant sstate god terror c.stdleadp ///
		i.adminzip founding if flag==1, be
		
permute stdpage_nod _b, reps(1000): ///	
	xtreg stdpage_nod i.entrep2##(c.stdfear2 c.stdanger2 c.stdpos) ///
		stdopage_nod govern immigrant sstate god terror c.stdleadp ///
		i.adminzip founding if flag==1 & p<3, be
		
	//Outlier issues?
permute stdpage_nod _b, reps(1000): ///	
	xtreg stdpage_nod i.entrep2##(c.stdfear2 c.stdanger2 c.stdpos) ///
		stdopage_nod govern immigrant sstate god terror c.stdleadp ///
		i.adminzip founding if flag==1 & abs(stdfear2)<2, be
		
permute stdpage_nod _b, reps(1000): ///	
	xtreg stdpage_nod i.entrep2##(c.stdfear2 c.stdanger2 c.stdpos) ///
		stdopage_nod govern immigrant sstate god terror c.stdleadp ///
		i.adminzip founding if flag==1 & abs(stdanger2)<2, be
		
	//Difference in BE models when using just the 28x28 WNO matrix? No.
permute stdpage_nod2 _b, reps(1000) nodots: ///	
	xtreg stdpage_nod2 i.entrep2##(c.stdfear2 c.stdanger2 c.stdpos) ///
		stdopage_nod2 govern immigrant sstate god terror c.stdleadp ///
		i.adminzip founding if flag==1, be
		
	//Difference when exlcuding 1989 docs? Not for fear, but kinda for anger.
		//See paper for how I address this.
permute stdpage_no89 _b, reps(1000): ///	
	xtreg stdpage_no89 i.entrep2##(c.stdfear2 c.stdanger2 c.stdpos) ///
		stdopage_no89 govern immigrant sstate god terror c.stdleadp ///
		i.adminzip founding if flag==1 & year!=1989, be

		
