

perm_table2 <- function(data, model, perm_v, strata_v = NULL, statistic,
                        perm_n = 1000, alternative = "all", alpha = 0.05, 
                        seed = NULL) {
  
  if (length(model[[statistic]]) >= 2) {
  
  set.seed(seed)
  
  perms <- data.frame(matrix(ncol = perm_n, nrow = nrow(data)))
  colnames(perms) <- rep(paste0(perm_v, "_perm"), perm_n)
  perm_v_n <- paste0(perm_v, "_perm")
  
  if (is.null(strata_v) == F) {
    control <- how(plots = Plots(strata = data[[strata_v]]))
    
    perm_list <- list()
    for (i in 1:perm_n) {
      
      perms[,i] <- data[[perm_v]][shuffle(length(data[[perm_v]]), 
                                          control = control)]
      
      perm_list[[i]] <- cbind(perms[i], data)
      
    }
  }
  
  if (is.null(strata_v) == T) {
    perm_list <- list()
    for (i in 1:perm_n) {
      
      perms[,i] <- data[[perm_v]][shuffle(length(data[[perm_v]]))]
      
      perm_list[[i]] <- cbind(perms[i], data)
      
    }
  }
  
  
  models <- list()
  
  for (i in 1:length(perm_list)) {
    
    models[[i]] <- update(model, eval(as.name(perm_v_n)) ~ ., data = perm_list[[i]])
    
  }
  
  perm_mat <- map_df(models, statistic, .id = "id") %>%
    transpose() %>%
    slice(-1)
  
  perm_mat <- sapply(perm_mat, as.numeric)
  
  mat <- data.frame(matrix(ncol = 1, nrow = length(model[[statistic]])))
  colnames(mat) <- "coef"
  rownames(mat) <- names(model[[statistic]])
  mat$coef <- model[[statistic]]
  
  if (alternative == "all") {
    
    mat$Pl <- rowSums(perm_mat <= model[[statistic]])/perm_n
    mat$Pu <- rowSums(perm_mat >= model[[statistic]])/perm_n
    mat$Pt <- rowSums(abs(perm_mat) >= abs(model[[statistic]]))/perm_n
    
    mat$SEl <- sqrt((mat$Pl * (1 - mat$Pl))/perm_n)
    mat$SEu <- sqrt((mat$Pu * (1 - mat$Pu))/perm_n)
    mat$SEt <- sqrt((mat$Pt * (1 - mat$Pt))/perm_n)
    
    mat$CIl_l <- mat$Pl - (qnorm(alpha/2, mean = 0, sd = 1, 
                                 lower.tail = F) * mat$SEl)
    mat$CIl_u <- mat$Pl + (qnorm(alpha/2, mean = 0, sd = 1, 
                                 lower.tail = F) * mat$SEl)
    mat$CIu_l <- mat$Pu - (qnorm(alpha/2, mean = 0, sd = 1, 
                                 lower.tail = F) * mat$SEu)
    mat$CIu_u <- mat$Pu + (qnorm(alpha/2, mean = 0, sd = 1, 
                                 lower.tail = F) * mat$SEu)
    mat$CIt_l <- mat$Pt - (qnorm(alpha/2, mean = 0, sd = 1, 
                                 lower.tail = F) * mat$SEt)
    mat$CIt_u <- mat$Pt + (qnorm(alpha/2, mean = 0, sd = 1, 
                                 lower.tail = F) * mat$SEt)
    
  }
  
  if (alternative == "two.sided") {
    
    mat$Pt <- rowSums(abs(perm_mat) >= abs(model[[statistic]]))/perm_n
    
    mat$SEt <- sqrt((mat$Pt * (1 - mat$Pt))/perm_n)
    
    mat$CIt_l <- mat$Pt - (qnorm(alpha/2, mean = 0, sd = 1, 
                                 lower.tail = F) * mat$SEt)
    mat$CIt_u <- mat$Pt + (qnorm(alpha/2, mean = 0, sd = 1, 
                                 lower.tail = F) * mat$SEt)
    
  }
  
  if (alternative == "lower") {
    
    mat$Pl <- rowSums(perm_mat <= model[[statistic]])/perm_n
    
    mat$SEl <- sqrt((mat$Pl * (1 - mat$Pl))/perm_n)
    
    mat$CIl_l <- mat$Pl - (qnorm(alpha/2, mean = 0, sd = 1, 
                                 lower.tail = F) * mat$SEl)
    mat$CIl_u <- mat$Pl + (qnorm(alpha/2, mean = 0, sd = 1, 
                                 lower.tail = F) * mat$SEl)
    
  }
  
  if (alternative == "upper") {
    
    mat$Pu <- rowSums(perm_mat >= model[[statistic]])/perm_n
    
    mat$CIu_l <- mat$Pu - (qnorm(alpha/2, mean = 0, sd = 1, 
                                 lower.tail = F) * mat$SEu)
    mat$CIu_u <- mat$Pu + (qnorm(alpha/2, mean = 0, sd = 1, 
                                 lower.tail = F) * mat$SEu)
    
  }
  
  mat <- mat %>% 
    as_tibble(rownames = NA) %>%
    rownames_to_column()
  
  return(mat)
  
  } else {
    
    set.seed(seed)
    
    perms <- data.frame(matrix(ncol = perm_n, nrow = nrow(data)))
    colnames(perms) <- rep(paste0(perm_v, "_perm"), perm_n)
    perm_v_n <- paste0(perm_v, "_perm")
    
    if (is.null(strata_v) == F) {
      control <- how(plots = Plots(strata = data[[strata_v]]))
      
      perm_list <- list()
      for (i in 1:perm_n) {
        
        perms[,i] <- data[[perm_v]][shuffle(length(data[[perm_v]]), 
                                            control = control)]
        
        perm_list[[i]] <- cbind(perms[i], data)
        
      }
    }
    
    if (is.null(strata_v) == T) {
      perm_list <- list()
      for (i in 1:perm_n) {
        
        perms[,i] <- data[[perm_v]][shuffle(length(data[[perm_v]]))]
        
        perm_list[[i]] <- cbind(perms[i], data)
        
      }
    }
    
    
    models <- list()
    
    for (i in 1:length(perm_list)) {
      
      models[[i]] <- update(model, eval(as.name(perm_v_n)) ~ ., data = perm_list[[i]])
      
    }
    
    perm_mat <- map_df(models, statistic, .id = "id") %>%
      transpose() %>%
      slice(-1)
    
    perm_mat <- sapply(perm_mat, as.numeric)
    
    mat <- data.frame(matrix(ncol = 1, nrow = length(model[[statistic]])))
    colnames(mat) <- "coef"
    rownames(mat) <- names(model[[statistic]])
    mat$coef <- model[[statistic]]
    
    if (alternative == "all") {
      
      mat$Pl <- sum(as.matrix(perm_mat <= model[[statistic]]))/perm_n
      mat$Pu <- sum(as.matrix(perm_mat >= model[[statistic]]))/perm_n
      mat$Pt <- sum(as.matrix(abs(perm_mat) >= abs(model[[statistic]])))/perm_n
      
      mat$SEl <- sqrt((mat$Pl * (1 - mat$Pl))/perm_n)
      mat$SEu <- sqrt((mat$Pu * (1 - mat$Pu))/perm_n)
      mat$SEt <- sqrt((mat$Pt * (1 - mat$Pt))/perm_n)
      
      mat$CIl_l <- mat$Pl - (qnorm(alpha/2, mean = 0, sd = 1, 
                                   lower.tail = F) * mat$SEl)
      mat$CIl_u <- mat$Pl + (qnorm(alpha/2, mean = 0, sd = 1, 
                                   lower.tail = F) * mat$SEl)
      mat$CIu_l <- mat$Pu - (qnorm(alpha/2, mean = 0, sd = 1, 
                                   lower.tail = F) * mat$SEu)
      mat$CIu_u <- mat$Pu + (qnorm(alpha/2, mean = 0, sd = 1, 
                                   lower.tail = F) * mat$SEu)
      mat$CIt_l <- mat$Pt - (qnorm(alpha/2, mean = 0, sd = 1, 
                                   lower.tail = F) * mat$SEt)
      mat$CIt_u <- mat$Pt + (qnorm(alpha/2, mean = 0, sd = 1, 
                                   lower.tail = F) * mat$SEt)
      
    }
    
    if (alternative == "two.sided") {
      
      mat$Pt <- sum(as.matrix(abs(perm_mat) >= abs(model[[statistic]])))/perm_n
      
      mat$SEt <- sqrt((mat$Pt * (1 - mat$Pt))/perm_n)
      
      mat$CIt_l <- mat$Pt - (qnorm(alpha/2, mean = 0, sd = 1, 
                                   lower.tail = F) * mat$SEt) 
      mat$CIt_u <- mat$Pt + (qnorm(alpha/2, mean = 0, sd = 1, 
                                   lower.tail = F) * mat$SEt)
      
    }
    
    if (alternative == "lower") {
      
      mat$Pl <- sum(as.matrix(perm_mat <= model[[statistic]]))/perm_n
      
      mat$SEl <- sqrt((mat$Pl * (1 - mat$Pl))/perm_n)
      
      mat$CIl_l <- mat$Pl - (qnorm(alpha/2, mean = 0, sd = 1, 
                                   lower.tail = F) * mat$SEl)
      mat$CIl_u <- mat$Pl + (qnorm(alpha/2, mean = 0, sd = 1, 
                                   lower.tail = F) * mat$SEl)
      
    }
    
    if (alternative == "upper") {
      
      mat$Pu <- sum(as.matrix(perm_mat >= model[[statistic]]))/perm_n
      
      mat$CIu_l <- mat$Pu - (qnorm(alpha/2, mean = 0, sd = 1, 
                                   lower.tail = F) * mat$SEu)
      mat$CIu_u <- mat$Pu + (qnorm(alpha/2, mean = 0, sd = 1, 
                                   lower.tail = F) * mat$SEu)
      
    }
    
    mat <- mat %>% 
      as_tibble(rownames = NA) %>%
      rownames_to_column()
    
    return(mat)
    
  }
  
}
