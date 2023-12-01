

plm(immigration ~ log(terror_nright+1)*construal_style*p2001 + discursive_style + word_count +
      per_repub + factor(admin) + factor(reform) + vcrime_rate,
    data = wno_data,
    index = c("org","year"),
    model = "within") %>%
  summary()


wno_data$sd_log_terror <- scale(log(wno_data$terror_nright+1))

model <- plm(immigration ~ sd_log_terror*hisp_inter + discursive_style + word_count,
             data = wno_data,
             index = c("org","year"),
             model = "within") 

model2 <- plm(immigration ~ sd_log_terror*hisp_inter*p2001 + discursive_style + word_count,
             data = wno_data,
             index = c("org","year"),
             model = "within") 




predictions(model, variables = list(sd_log_terror = seq(-1,1, by  = 1),
                                            hisp_inter = seq(quantile(wno_data$hisp_inter, .25, na.rm = T),
                                                             1, by = .5))) %>%
  rbind(.,
        predictions(model2, variables = list(stdlagconabst_pol2_dup = seq(-1,1, by  = 1),
                                             terror_nr = seq(-3,1, by = .5)))) %>%
  mutate(model_id = c(rep("model", 2970), rep("model2", (tally(.) - 2970)))) %>%
  select(rowid, predicted, stdlagconabst_pol2_dup, terror_nr, model_id) %>%
  mutate(predicted = (exp(predicted)/(1 + exp(predicted)))) %>%
  group_by(model_id, stdlagconabst_pol2_dup, terror_nr) %>%
  summarise(predicted_sum = mean(predicted)) %>%
  ggplot(aes(x = terror_nr, y = predicted_sum, linetype = as.factor(stdlagconabst_pol2_dup))) +
  geom_line() +
  labs(y = '"Borders and Immigration" Grievance Probability',
       x = "Ratio of Terror Events per 1,000 Non-Hispanic/Latinx County Residents (logged)") +
  scale_linetype_discrete(breaks = c(-1,0,1),
                          labels = c("-1 SD Construal Style",
                                     "Mean Construal Style",
                                     "+1 SD Construal Style"),
                          name = "") +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"),
        legend.position = "top") +
  facet_grid(~model_id,
             labeller = labeller(model_id = labs))


temp$temp_id <- paste0(temp$state, temp$county, temp$year)
pre.meta$temp_id <- paste0(pre.meta$state, pre.meta$county,
                           pre.meta$year)
pre.meta$doc <- rownames(pre.meta)

pre.meta <- left_join(pre.meta, temp[,c("temp_id","hisp_interprop")], by = "temp_id")

pre.meta <- pre.meta[!duplicated(pre.meta$doc),]

rownames(pre.meta) <- pre.meta$doc
pre.meta$temp_id <- NULL


saveRDS(pre.meta, "pre_topic_meta.rds")
