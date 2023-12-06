plm(immigration ~ construal_style*terror + discursive_style +
      factor(admin),
    data = wno_data,
    index = c("org","year"),
    model = "within") %>%
  summary()


wno_data$log_terror <- log(wno_data$terror_nright+1)
wno_data$sd_construal <- scale(wno_data$construal_style)
wno_data$hisp_pop <- wno_data$population * wno_data$hisp_interprop
wno_data$log_hisppop <- log(wno_data$hisp_pop)

model <- plm(immigration ~ log_terror*log_hisppop + discursive_style + word_count +
               per_repub + factor(admin) + factor(reform) + vcrime_rate + log_vocality + p2001,
             data = wno_data,
             index = c("org","year"),
             model = "within") |> summary()

perm_table(data = wno_data, model = model,
                  perm_v = "immigration", statistic = "coefficients", 
                  strata_v = "org", seed = 123)

model2 <- plm(immigration ~ sd_log_terror*sd_construal*p2001 + discursive_style + word_count +
                per_repub + factor(admin) + factor(reform) + vcrime_rate,
              data = wno_data,
              index = c("org","year"),
              model = "within")



model.data <- as.data.frame(model$model)

model.data <- model.data %>%
  rename(admin = `factor.admin.`,
         reform = `factor.reform.`)

model.data2 <- as.data.frame(model2$model)

model.data2 <- model.data2 %>%
  rename(admin = `factor.admin.`,
         reform = `factor.reform.`)





predictions(model, variables = list(log_terror = seq(quantile(wno_data$log_terror, .25, na.rm = T), 
                                                     quantile(wno_data$log_terror, .75, na.rm = T),
                                                     by = .25),
                                    log_hisppop = c(quantile(wno_data$log_hisppop, .25, na.rm = T),
                                                    quantile(wno_data$log_hisppop, .5, na.rm = T),
                                                    quantile(wno_data$log_hisppop, .75, na.rm = T))),
            newdata = model.data) %>%
  mutate(log_terror = (exp(log_terror)-1),
         log_hisppop = exp(log_hisppop)) %>%
  # mutate(p2001 = NA) %>%
  # rbind(.,
  #       predictions(model2, variables = list(sd_construal = seq(-1,1, by  = 1),
  #                                            sd_log_terror = seq(-2,2, by = .5),
  #                                            p2001 = c(0,1)),
  #                   newdata = model.data2) %>%
  # # mutate(model_id = c(rep("model", 1650), rep("model2", (tally(.) - 1650)))) %>%
  select(rowid, predicted, log_hisppop, log_terror) %>%
  mutate(predicted = (exp(predicted)/(1 + exp(predicted)))) %>%
  group_by(log_hisppop, log_terror) %>%
  summarise(predicted_sum = mean(predicted)) %>%
  ggplot(aes(x = log_terror, y = predicted_sum, linetype = as.factor(log_hisppop))) +
  geom_line() +
  labs(y = '"Borders and Immigration" Grievance Probability',
       x = "Number of Non-Right Wing Terror Threats or Attacks in U.S. in Previous Year") +
  # scale_x_continuous(trans = scales::log_trans()) +
  # coord_trans(x = "log") +
  scale_linetype_discrete(#breaks = c(-1,0,1),
                          labels = c(expression("25"^{th}~"Percentile Hispanic Population in County"),
                                     "Median Hispanic Population in County",
                                     expression("75"^{th}~"Percentile Hispanic Population in County")),
                          name = "") +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"),
        legend.position = "top")
  # facet_grid(~model_id,
  #            labeller = labeller(model_id = labs))


temp$temp_id <- paste0(temp$state, temp$county, temp$year)
pre.meta$temp_id <- paste0(pre.meta$state, pre.meta$county,
                           pre.meta$year)
pre.meta$doc <- rownames(pre.meta)

pre.meta <- left_join(pre.meta, temp[,c("temp_id","hisp_interprop")], by = "temp_id")

pre.meta <- pre.meta[!duplicated(pre.meta$doc),]

rownames(pre.meta) <- pre.meta$doc
pre.meta$temp_id <- NULL


saveRDS(pre.meta, "pre_topic_meta.rds")
