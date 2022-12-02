
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
               freqtables, reshape2, lme4, 
               broom.mixed, rrapply,
               install = T)

source("perm_table.R") #function to perform stratified Monte Carlo permutation tests


######################################
#  Data
######################################

meta <- readRDS("meta.rds") #Metadata for all 384 docs (for frequency table)

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
  summarize(sum = 1) %>%
  ungroup() %>%
  group_by(state) %>%
  summarize(sum = n()) %>%
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

freq_table(meta, doc_type)


######################################
#  Main Model
######################################
  
  #Note: Two other models are in the appendix section below: one with just
      #the environmental shock DV, and another that adds construal style
      #and the envr*construal interaction
model <- plm(lo_immigrant_dup ~ stdlagconabst_pol2_dup*terror_nr + laglo_immigrant_dup +
               per_repub1_dup + lo_vcrime_rate + factor(reform) +
               factor(p2001) + logvocality_dup + word_count_dup +
               factor(admin42),
             data = wno_data,
             index = c("org2","year"),
             model = "within")

mat <- perm_table(data = wno_data, model = model,
                  perm_v = "lo_immigrant_dup", 
                  strata_v = "org2", seed = 123)


######################################
#  Figure #3
######################################

  #Note: This figure is the PPV plot for the full model (left panel) and 
  #the model with 2002 org-years removed (right panel). The right panel is one
  #of the two 9/11 bias robustness checks.

mat$var <- rownames(mat) %>%
  as.factor()
mat$var <- factor(mat$var, levels = rev(mat$var))

labels <- c("Construal Style (sd)","Environmental Shock (logged)",
            "Discursive Style (logit-transformed)","% Republican Voting in County",
            "Violent Crime Rate in County (logged)","Immigration Legislation",
            "Pre-2001 Publication","Writer Heterogeneity (logged)",
            "(Mean) Word Count","Reagan Administration",
            "H.W. Bush Administration","W. Bush Administration",
            "Env Shock \u00D7 Construal")

coefs <- mat$coef %>% round(3)
coefs.lab <- list()
  
for (i in coefs) {
  
  coefs.lab[[match(i, coefs)]] <- substitute(paste(hat(beta), " = ", i),
                                           list(i = i))
  
}

fig2 <- mat %>%
  ggplot(aes(x = var, y = Pt)) +
  geom_col(color = "black", fill = "gray50") +
  geom_errorbar(aes(ymin = CIt_l, ymax = CIt_u), width = 0.1) +
  geom_text(aes(label = coefs.lab), parse = T, hjust = -.75) +
  geom_hline(yintercept = 0.05, linetype = "dashed") +
  labs(x = "", y = expression(bold(paste("Two-Tailed ", italic("P"), "-Values")))) +
  scale_x_discrete(labels = rev(labels)) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"),
        axis.text.y = element_text(face = "bold"),
        axis.text.x = element_text(size = 8)) +
  scale_y_continuous(expand = c(0.005,0),
                     limits = c(0,1),
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

#Note: This figure is the PPV plot for the full model (left panel) and 
#the model with 2002 org-years removed (right panel). The right panel is one
#of the two 9/11 bias robustness checks.

wno_data2 <- wno_data[wno_data$year != 2002,] #Removing 2002 org-years

model2 <- plm(lo_immigrant_dup ~ stdlagconabst_pol2_dup*terror_nr + laglo_immigrant_dup +
                per_repub1_dup + lo_vcrime_rate + factor(reform) +
                factor(p2001) + logvocality_dup + word_count_dup +
                factor(admin42),
              data = wno_data2,
              index = c("org2","year"),
              model = "within") #the model with 2002 org-years removed

mat2 <- perm_table(data = wno_data2, model = model2,
                  perm_v = "lo_immigrant_dup", 
                  strata_v = "org2", seed = 123)

labs <- c("2002 Org-Years Included","2002 Org-Years Not Included")
names(labs) <- c("model","model2")

fig3 <- predictions(model, variables = list(stdlagconabst_pol2_dup = seq(-1,1, by  = 1),
                                         terror_nr = seq(-3,1, by = .5))) %>%
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

png("figures/fig3.png", res = 750, height = 6, width = 12, units = "in")
  fig3
dev.off()


######################################
#  Figure #5
######################################

  #Note: This is the other 9/11 robustness check, where, instead of removing
      #2002 org-years, I made a env*construal*post-2001 three-way interaction.

model3 <- plm(lo_immigrant_dup ~ stdlagconabst_pol2_dup*terror_nr*factor(p2001) + 
               laglo_immigrant_dup + per_repub1_dup + lo_vcrime_rate + factor(reform) +
               logvocality_dup + word_count_dup +
               factor(admin42),
             data = wno_data,
             index = c("org2","year"),
             model = "within")

mat3 <- perm_table(data = wno_data, model = model3,
                  perm_v = "lo_immigrant_dup", 
                  strata_v = "org2", seed = 123)

labs2 <- c("Pre-9/11","Post-9/11")
names(labs2) <- c("0","1")

fig7 <- predictions(model3, variables = list(stdlagconabst_pol2_dup = seq(-1,1, by  = 1),
                                    terror_nr = seq(-3,1, by = .5),
                                    p2001 = c(0,1))) %>%
  select(rowid, predicted, stdlagconabst_pol2_dup, terror_nr, p2001) %>%
  mutate(predicted = (exp(predicted)/(1 + exp(predicted)))) %>%
  group_by(p2001, stdlagconabst_pol2_dup, terror_nr) %>%
  summarise(predicted_sum = mean(predicted)) %>%
  ggplot(aes(x = terror_nr, y = predicted_sum, 
             linetype = as.factor(stdlagconabst_pol2_dup),
             group = interaction(p2001, as.factor(stdlagconabst_pol2_dup)),
             color = p2001)) +
  geom_line() +
  labs(y = '"Borders and Immigration" Grievance Probability',
       x = "Ratio of Terror Events per 1,000 Non-Hispanic/Latinx County Residents (logged)",
       color = "", linetype = "") +
  scale_linetype_discrete(breaks = c(-1,0,1),
                        labels = c("-1 SD Construal Style",
                                     "Mean Construal Style",
                                     "+1 SD Construal Style")) +
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
vd <- lmer(lo_immigrant_dup ~ 1 + (1|org2),
           data = wno_data)
summary(vd)

vd.re <- VarCorr(vd) %>% as.data.frame()
1 - (vd.re$vcov[1]/(vd.re$vcov[1] + vd.re$vcov[2])) #1 - ICC


#Unconditional growth model
ugm <- wno_data %>%
  group_by(org_shortened) %>%
  mutate(imm_y = lo_immigrant_dup - mean(lo_immigrant_dup)) %>%
  ungroup() %>%
  ggplot(aes(x = year, y = imm_y)) +
  geom_point(shape = 1) +    
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  xlab("Year") + ylab(expression(paste("(Imm Focus)"[ij] - bar("(Imm Focus)")[j]))) +
  facet_wrap(~org_shortened) +
  xlim(1980, 2008) +
  theme_bw() 

png("figures/ugm.png", res = 750, height = 6, width = 8, units = "in")
  ugm
dev.off()


#Descriptive statistics

