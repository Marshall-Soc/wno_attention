
##############################
##  wno_attention.R: Code for attention-focusing paper
##  Author: Marshall A. Taylor
##############################

### BEGIN ###


######################################
#  Necessary packages and functions
######################################


pacman::p_load(tidyverse, plm, permute, data.table, 
               marginaleffects, ggpubr, forcats,
               freqtables, lme4, broom.mixed, psych,
               fixest, modelsummary, text2map,
               install = T)


######################################
#  Data
######################################

doc_type <- readRDS("doc_type.rds") #Doc type for all 384 docs (for frequency table)

desc9708 <- read.csv("group_desc.csv", header = T) %>%
  pivot_longer(cols = KKK:White.Nationalist) #for the intro figure

wno_data <- readRDS("wno_data.rds") #Main data, processed and ready to go


######################################
#  Figure #1
######################################

fig0 <- ggplot(data = desc9708, aes(x = Year, y = value, group = name, colour = name)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(1997, 2008, 2)) +
  theme_bw() +
  theme(axis.title.y = element_text(size = 10, face = "bold"),
        legend.position = "bottom",
        legend.title=element_text(face = "bold")) +
  xlab("") + ylab("Total Number of Organizations in U.S.") +
  scale_colour_manual(name = "",breaks = c("KKK","Neo.Nazi","Racist.Skinhead","Identity","Neo.Confederate","White.Nationalist"),
                      labels = c("KKK","Neo-Nazi","Racist Skinhead","Christian Identity","Neo-Confederate",
                               "White Nationalist"),
                      values = c("#1696d2","#fdbf11","#ec008b",
                               "#55b748","#5c5859","#db2b27")) 

png("figures/fig0.png", res = 750, height = 6, width = 8, units = "in")
  fig0
dev.off()


######################################
#  Figure #1
######################################

fig1 <- meta %>%
  group_by(state, org) %>%
  dplyr::summarize(sum = 1) %>%
  ungroup() %>%
  group_by(state) %>%
  dplyr::summarize(sum = n()) %>%
  mutate(state = recode_factor(state, "Mississippii" = "Mississippi")) %>%
  mutate(state = fct_reorder(state, desc(sum))) %>%
  ggplot(aes(x = state, y = sum)) +
  geom_col(color = "black", fill = "#1696d2") +
  theme_bw() +
  labs(x = "", y = "Total Number of Sampled Organizations") +
  theme(axis.title.y = element_text(face = "bold")) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

png("figures/fig1.png", res = 750, height = 6, width = 12, units = "in")
  fig1
dev.off()


######################################
#  Table #1
######################################

freq_table(as.data.frame(doc_type), doc_type)


######################################
#  Main Model
######################################
  
model <- plm(immigration ~ log_terror*log_hisppop + discursive_style +
               per_repub + vcrime_rate + factor(reform) + p2001 + log_vocality + 
               word_count + factor(admin),
             data = wno_data,
             index = c("org","year"),
             model = "within")

mat <- perm_tester(data = wno_data, model = model,
                  perm_var = "immigration", statistic = "coefficients", 
                  strat_var = "org", seed = 123)


######################################
#  Figure #3
######################################

mat$var <- factor(mat$rowname, levels = rev(mat$rowname))

labels <- c("U.S. Terror Threats/Attacks (logged)","Hispanic Population (logged)",
            "Discursive Style (logit-transformed)","% Republican Voting in County",
            "Violent Crime Rate in County (logged)","Immigration Legislation",
            "Pre-2001 Publication","Writer Heterogeneity (logged)",
            "(Mean) Word Count","Reagan Administration",
            "H.W. Bush Administration","W. Bush Administration",
            "Terror \u00D7 Hispanic")

coefs <- mat$stat %>% round(3)
coefs.lab <- list()
  
for (i in coefs) {
  
  coefs.lab[[match(i, coefs)]] <- substitute(paste(hat(beta), " = ", i),
                                           list(i = i))
  
}

fig2 <- mat %>%
  ggplot(aes(x = var, y = P_two)) +
  geom_col(color = "black", fill = "gray50") +
  geom_errorbar(aes(ymin = CI_two_lo, ymax = CI_two_up), width = 0.1) +
  geom_text(aes(label = coefs.lab), parse = T, hjust = -.75) +
  geom_hline(yintercept = 0.05, linetype = "dashed") +
  labs(x = "", y = expression(bold(paste("Two-Tailed ", italic("P"), "-Values")))) +
  scale_x_discrete(labels = rev(labels)) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"),
        axis.text.y = element_text(face = "bold"),
        axis.text.x = element_text(size = 8),
        panel.grid.minor.x = element_blank()) +
  scale_y_continuous(expand = c(0.005,0),
                     limits = c(0,1.2),
                     breaks = c(0,.05,seq(.25, 1, by = .25)),
                     labels = c("0.00", expression(paste(italic("p"),
                                                         " = 0.05")),
                                "0.25", "0.50", "0.75", "1.00")) +
  coord_flip()

png("figures/fig2.png", res = 750, height = 6, width = 12, units = "in")
  fig2
dev.off()


######################################
#  Figure #4
######################################

    #Note: This figure is the prediction plot for the full model (left panel) and 
    #the model with 2002 org-years removed (right panel). The right panel is one
    #of the two 9/11 bias robustness checks.

wno_data2 <- wno_data[wno_data$year != 2002,] #Removing 2002 org-years

model2 <- plm(immigration ~ log_terror*log_hisppop + discursive_style +
                per_repub + vcrime_rate + factor(reform) + p2001 + log_vocality + 
                word_count + factor(admin),
              data = wno_data2,
              index = c("org","year"),
              model = "within") #the model with 2002 org-years removed

mat2 <- perm_tester(data = wno_data2, model = model2,
                  perm_var = "immigration", statistic = "coefficients",
                  strat_var = "org", seed = 123)

labs <- c("2002 Org-Years Included","2002 Org-Years Not Included")
names(labs) <- c("model","model2")

model.data <- as.data.frame(model$model)

model.data <- model.data %>%
  rename(admin = `factor.admin.`,
         reform = `factor.reform.`)

model.data2 <- as.data.frame(model2$model)

model.data2 <- model.data2 %>%
  rename(admin = `factor.admin.`,
         reform = `factor.reform.`)

fig3 <- predictions(model, variables = list(log_terror = seq(quantile(wno_data$log_terror, .25, na.rm = T), 
                                                             quantile(wno_data$log_terror, .75, na.rm = T),
                                                             by = .25),
                                            log_hisppop = c(quantile(wno_data$log_hisppop, .25, na.rm = T),
                                                            quantile(wno_data$log_hisppop, .5, na.rm = T),
                                                            quantile(wno_data$log_hisppop, .75, na.rm = T))),
                    newdata = model.data) %>%
  rbind(.,
        predictions(model2, variables = list(log_terror = seq(quantile(wno_data$log_terror, .25, na.rm = T), 
                                                             quantile(wno_data$log_terror, .75, na.rm = T),
                                                             by = .25),
                                            log_hisppop = c(quantile(wno_data$log_hisppop, .25, na.rm = T),
                                                            quantile(wno_data$log_hisppop, .5, na.rm = T),
                                                            quantile(wno_data$log_hisppop, .75, na.rm = T))),
                    newdata = model.data2)) %>%
  mutate(model_id = c(rep("model", 1296), rep("model2", (tally(.) - 1296))),
         log_terror = (exp(log_terror)-1),
         log_hisppop = exp(log_hisppop)) %>%
  select(rowid, predicted, log_hisppop, log_terror, model_id) %>%
  mutate(predicted = (exp(predicted)/(1 + exp(predicted)))) %>%
  group_by(log_hisppop, log_terror, model_id) %>%
  summarise(predicted_sum = mean(predicted)) %>%
  ggplot(aes(x = log_terror, y = predicted_sum, linetype = as.factor(log_hisppop))) +
  geom_line() +
  labs(y = '"Borders and Immigration" Grievance Probability',
       x = "Number of Non-Right Wing Terror Threats or Attacks in U.S. in Previous Year") +
  scale_linetype_discrete(#breaks = c(-1,0,1),
    labels = c(expression("25"^{th}~"Percentile Hispanic Population in County"),
               "Median Hispanic Population in County",
               expression("75"^{th}~"Percentile Hispanic Population in County")),
    name = "") +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"),
        legend.position = "top") +
  facet_grid(~model_id,
           labeller = labeller(model_id = labs))

png("figures/fig3.png", res = 750, height = 6, width = 12, units = "in")
  fig3
dev.off()


######################################
#  Figure #5
######################################

  #Note: This is the other 9/11 robustness check, where, instead of removing
      #2002 org-years, I made a terro*hisp*post-2001 three-way interaction.

model3 <- plm(immigration ~ log_terror*log_hisppop*p2001 + discursive_style +
               per_repub + vcrime_rate + factor(reform) + log_vocality + 
               word_count + factor(admin),
             data = wno_data,
             index = c("org","year"),
             model = "within")

mat3 <- perm_tester(data = wno_data, model = model3,
                  perm_var = "immigration", statistic = "coefficients", 
                  strat_var = "org", seed = 123)

model.data3 <- as.data.frame(model3$model)

model.data3 <- model.data3 %>%
  rename(admin = `factor.admin.`,
         reform = `factor.reform.`)

labs2 <- c("Pre-9/11","Post-9/11")
names(labs2) <- c("0","1")

fig7 <- predictions(model3, variables = list(log_terror = seq(quantile(wno_data$log_terror, .25, na.rm = T), 
                                                             quantile(wno_data$log_terror, .75, na.rm = T),
                                                             by = .25),
                                            log_hisppop = c(quantile(wno_data$log_hisppop, .25, na.rm = T),
                                                            quantile(wno_data$log_hisppop, .5, na.rm = T),
                                                            quantile(wno_data$log_hisppop, .75, na.rm = T)),
                                            p2001 = c(0,1)),
                    newdata = model.data3) %>%
  mutate(log_terror = (exp(log_terror)-1),
         log_hisppop = exp(log_hisppop),
         p2001 = as.factor(p2001)) %>%
  select(rowid, predicted, log_hisppop, log_terror, p2001) %>%
  mutate(predicted = (exp(predicted)/(1 + exp(predicted)))) %>%
  group_by(log_hisppop, log_terror, p2001) %>%
  summarise(predicted_sum = mean(predicted)) %>%
  ggplot(aes(x = log_terror, y = predicted_sum, 
             linetype = as.factor(log_hisppop),
             group = interaction(p2001, as.factor(log_hisppop)),
             color = p2001)) +
  geom_line() +
  labs(y = '"Borders and Immigration" Grievance Probability',
       x = "Number of Non-Right Wing Terror Threats or Attacks in U.S. in Previous Year",
       color = "", linetype = "") +
  scale_linetype_discrete(#breaks = c(-1,0,1),
    labels = c(expression("25"^{th}~"Percentile Hispanic Population in County"),
               "Median Hispanic Population in County",
               expression("75"^{th}~"Percentile Hispanic Population in County")),
    name = "") +
  scale_color_manual(breaks = c(0,1),
                     values = c("#1696d2","#fdbf11"),
                     labels = c("Before/Including 2001","Post-2001")) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"),
        legend.position = "top",
        legend.box = "vertical",
        legend.direction = "horizontal")

png("figures/fig7.png", res = 750, height = 6, width =8, units = "in")
  fig7
dev.off()


######################################
#  Appendix Items
######################################

#Variance decomposition model
vd <- lmer(immigration ~ 1 + (1|org),
           data = wno_data[complete.cases(wno_data),])
summary(vd)

vd.re <- VarCorr(vd) %>% as.data.frame()
vd.re$vcov[2]/(vd.re$vcov[1] + vd.re$vcov[2]) #1 - ICC


#Unconditional growth model
ugm <- wno_data %>%
  filter(complete.cases(.)) %>%
  group_by(org) %>%
  mutate(imm_y = immigration - mean(immigration)) %>%
  ungroup() %>%
  ggplot(aes(x = year, y = imm_y)) +
  geom_point(shape = 1) +    
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  xlab("Year") + ylab(expression(paste("(Imm Focus)"[ij] - bar("(Imm Focus)")[j]))) +
  facet_wrap(~org, labeller = label_wrap_gen(width = 50)) +
  xlim(1980, 2008) +
  theme_bw() +
  theme(strip.text = element_text(size = 6))

png("figures/ugm.png", res = 750, height = 10, width = 13, units = "in")
  ugm
dev.off()


#Descriptive statistics
  #For continuous variables
vars.con <- c("immigration","log_terror","log_hisppop",
               "discursive_style","per_repub","vcrime_rate",
               "log_vocality","word_count")
describe(wno_data[vars.con])

  #For categorical variables, dummy
vars.cat <- c("reform","p2001")
apply(wno_data[vars.cat], 2, mean)

  #For categorical variables, multiple categories
freq_table(wno_data, admin)

#Models 1 and 2 in Table 5 of the Appendix
  #Model 1
model.1 <- plm(immigration ~ log_terror*log_hisppop,
               data = wno_data,
               index = c("org","year"),
               model = "within")

mat.1 <- perm_tester(data = wno_data, model = model.1,
                    perm_var = "immigration", statistic = "coefficients",
                    strat_var = "org", seed = 123)

  #Fit statistics for all models
fit <- list(
  feols(immigration ~ log_terror*log_hisppop | org, data = wno_data),
  feols(immigration ~ log_terror*log_hisppop + discursive_style +
          per_repub + vcrime_rate + factor(reform) + p2001 + log_vocality + 
          word_count + factor(admin) | org, data = wno_data)
)

lapply(fit, r2) #r2, adj r2, within-r2, adj within-r2
lapply(fit, function(x) sd(wno_data$immigration - x$fitted.values)) #rmse

fit.2 <- list(
  feols(immigration ~ log_terror*log_hisppop + discursive_style +
          per_repub + vcrime_rate + factor(reform) + p2001 + log_vocality + 
          word_count + factor(admin) | org, data = wno_data2),
  feols(immigration ~ log_terror*log_hisppop*p2001 + discursive_style +
          per_repub + vcrime_rate + factor(reform) + log_vocality + 
          word_count + factor(admin) | org, data = wno_data)
)

lapply(fit.2, r2) #r2, adj r2, within-r2, adj within-r2
lapply(fit.2, function(x) sd(wno_data$immigration - x$fitted.values)) #rmse


### END ###
