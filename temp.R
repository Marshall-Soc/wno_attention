
pacman::p_load(tidyverse, plm, permute, data.table, marginaleffects, install = T)

diss.data <- haven::read_dta("diss_data_stata4.dta")
diss.data2 <- diss.data[diss.data$dupflag == 1,]

perm_n <- 1000

model <- plm(lo_immigrant_dup ~ stdlagconabst_pol2_dup*terror_nr +
               stdlagconabst_pol2_dup*legal_nr + laglo_immigrant_dup +
               terror_r + per_repub1_dup + relevel(factor(catpage), ref = "1") +
               lo_vcrime_rate + stdleadpublic_dup + factor(reform) +
               factor(p2001) + logvocality_dup + word_count_dup +
               factor(admin42),
             data = diss.data2,
             index = c("org2","year"),
             model = "within")

# model <- plm(lo_immigrant_dup ~ terror_nright*hisp_interprop +
#                legal*hisp_interprop +
#                laglo_immigrant_dup + terror_right + per_repub1_dup + 
#                relevel(factor(catpage), ref = "1") +
#                lo_vcrime_rate + stdleadpublic_dup + factor(reform) +
#                factor(p2001) + logvocality_dup + word_count_dup +
#                factor(admin42), 
#              data = diss.data2,
#              index = c("org2","year"),
#              model = "within")

# model <- plm(lo_immigrant_dup ~ terror_nright*hisp_interprop +
#                laglo_immigrant_dup + 
#                factor(p2001), 
#              data = diss.data2,
#              index = c("org2","year"),
#              model = "within")

# perms <- diss.data2 %>%
#   group_by(org2) %>%
#   permute(perm_n, lo_immigrant_dup)

perms <- data.frame(matrix(ncol = perm_n, nrow = nrow(diss.data2)))
colnames(perms) <- rep("lo_immigrant_dup_perm", perm_n)

control <- how(plots = Plots(strata = diss.data2$org2))

perm_list <- list()
for (i in 1:perm_n) {
  
  perms[,i] <- diss.data2$lo_immigrant_dup[shuffle(length(diss.data2$lo_immigrant_dup), 
                                                   control = control)]
  
  perm_list[[i]] <- cbind(perms[i], diss.data2)
  
}

models <- map(perm_list, ~ plm(lo_immigrant_dup_perm ~ stdlagconabst_pol2_dup*terror_nr +
                                  stdlagconabst_pol2_dup*legal_nr + laglo_immigrant_dup +
                                  terror_r + per_repub1_dup + relevel(factor(catpage), ref = "1") +
                                  lo_vcrime_rate + stdleadpublic_dup + factor(reform) +
                                  factor(p2001) + logvocality_dup + word_count_dup +
                                  factor(admin42),
                                data = .,
                                index = c("org2","year"),
                                model = "within"))

# models <- map(perm_list, ~ plm(lo_immigrant_dup_perm ~ terror_nright*hisp_interprop +
#                                  legal*hisp_interprop +
#                                  laglo_immigrant_dup + terror_right + per_repub1_dup + 
#                                  relevel(factor(catpage), ref = "1") +
#                                  lo_vcrime_rate + stdleadpublic_dup + factor(reform) +
#                                  factor(p2001) + logvocality_dup + word_count_dup +
#                                  factor(admin42), 
#                                data = .,
#                                index = c("org2","year"),
#                                model = "within"))

# models <- map(perm_list, ~ plm(lo_immigrant_dup_perm ~ terror_nright*hisp_interprop +
#                                  laglo_immigrant_dup + 
#                                  factor(p2001), 
#                                data = .,
#                                index = c("org2","year"),
#                                model = "within"))

perm_mat <- map_df(models, coefficients, .id = "id") %>%
  transpose() %>%
  slice(-1)

perm_mat <- sapply(perm_mat, as.numeric)

mat <- data.frame(matrix(ncol = 1, nrow = length(model$coefficients)))
colnames(mat) <- "coef"
rownames(mat) <- names(model$coefficients)
mat$coef <- model$coefficients

mat$Pl <- rowSums(perm_mat <= model$coefficients)/perm_n
mat$Pu <- rowSums(perm_mat >= model$coefficients)/perm_n
mat$Pt <- rowSums(abs(perm_mat) >= abs(model$coefficients))/perm_n

mat$SEl <- sqrt((mat$Pl * (1 - mat$Pl))/perm_n) %>% round(3)
mat$SEu <- sqrt((mat$Pu * (1 - mat$Pu))/perm_n) %>% round(3)
mat$SEt <- sqrt((mat$Pt * (1 - mat$Pt))/perm_n) %>% round(3)

mat$CIl_l <- mat$Pl - (qnorm(.05/2, mean = 0, sd = 1, 
                             lower.tail = F) * mat$SEl) %>% round(3)
mat$CIl_u <- mat$Pl + (qnorm(.05/2, mean = 0, sd = 1, 
                             lower.tail = F) * mat$SEl) %>% round(3)
mat$CIu_l <- mat$Pu - (qnorm(.05/2, mean = 0, sd = 1, 
                             lower.tail = F) * mat$SEu) %>% round(3)
mat$CIu_u <- mat$Pu + (qnorm(.05/2, mean = 0, sd = 1, 
                             lower.tail = F) * mat$SEu) %>% round(3)
mat$CIt_l <- mat$Pt - (qnorm(.05/2, mean = 0, sd = 1, 
                             lower.tail = F) * mat$SEt) %>% round(3)
mat$CIt_u <- mat$Pt + (qnorm(.05/2, mean = 0, sd = 1, 
                             lower.tail = F) * mat$SEt) %>% round(3)

mat %>% as_tibble(rownames = NA) %>%
  rownames_to_column()

