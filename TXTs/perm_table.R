
perm_table <- function(data, model, perm_v, strata_v = NULL, statistic,
                       perm_n = 1000, alternative = "all", alpha = 0.05, 
                       seed = NULL) {
  
  if (length(model[[statistic]]) >= 2) {
    
    set.seed(seed)
    
    perms <- data.frame(matrix(ncol = perm_n, nrow = nrow(data)))
    colnames(perms) <- rep(perm_v, perm_n)
    
    if (is.null(strata_v) == F) {
    control <- permute::how(plots = permute::Plots(strata = data[[strata_v]]))
    
    perm_list <- list()
    for (i in 1:perm_n) {
      
      perms[,i] <- data[[perm_v]][permute::shuffle(length(data[[perm_v]]), 
                                          control = control)]
      
      perm_list[[i]] <- cbind(perms[i], data[, !names(data) %in% perm_v])
      
    }
  }
  
  if (is.null(strata_v) == T) {
    perm_list <- list()
    for (i in 1:perm_n) {
      
      perms[,i] <- data[[perm_v]][permute::shuffle(length(data[[perm_v]]))]
      
      perm_list[[i]] <- cbind(perms[i], data[, !names(data) %in% perm_v])
      
    }
  }
  
  
  models <- list()
  
  for (i in 1:length(perm_list)) {
    
    models[[i]] <- stats::update(model, . ~ ., data = perm_list[[i]])
    
  }
  
  perm_mat <- purrr::map_df(models, statistic, .id = "id") |>
    data.table::transpose() |>
    dplyr::slice(-1)
  
  perm_mat <- sapply(perm_mat, as.numeric)
  
  mat <- data.frame(matrix(ncol = 1, nrow = length(model[[statistic]])))
  colnames(mat) <- "stat"
  rownames(mat) <- names(model[[statistic]])
  mat$stat <- model[[statistic]]
  
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
    
    mat$SEu <- sqrt((mat$Pu * (1 - mat$Pu))/perm_n)
    
    mat$CIu_l <- mat$Pu - (qnorm(alpha/2, mean = 0, sd = 1, 
                                 lower.tail = F) * mat$SEu)
    mat$CIu_u <- mat$Pu + (qnorm(alpha/2, mean = 0, sd = 1, 
                                 lower.tail = F) * mat$SEu)
    
  }
  
  mat <- mat |> 
    tibble::as_tibble(rownames = NA) |>
    tibble::rownames_to_column()
  
  return(mat)
  
  } else {
  
  set.seed(seed)
  
  perms <- data.frame(matrix(ncol = perm_n, nrow = nrow(data)))
  colnames(perms) <- rep(perm_v, perm_n)
  
  if (is.null(strata_v) == F) {
    control <- permute::how(plots = permute::Plots(strata = data[[strata_v]]))
    
    perm_list <- list()
    for (i in 1:perm_n) {
      
      perms[,i] <- data[[perm_v]][permute::shuffle(length(data[[perm_v]]), 
                                          control = control)]
      
      perm_list[[i]] <- cbind(perms[i], data[, !names(data) %in% perm_v])
      
    }
  }
  
  if (is.null(strata_v) == T) {
    perm_list <- list()
    for (i in 1:perm_n) {
      
      perms[,i] <- data[[perm_v]][permute::shuffle(length(data[[perm_v]]))]
      
      perm_list[[i]] <- cbind(perms[i], data[, !names(data) %in% perm_v])
      
    }
  }
  
  
  models <- list()
  
  for (i in 1:length(perm_list)) {
    
    models[[i]] <- stats::update(model, . ~ ., data = perm_list[[i]])
    
  }
  
  perm_mat <- purrr::map_df(models, statistic, .id = "id") |>
    data.table::transpose() |>
    dplyr::slice(-1)
  
  perm_mat <- sapply(perm_mat, as.numeric)
  
  mat <- data.frame(matrix(ncol = 1, nrow = length(model[[statistic]])))
  colnames(mat) <- "stat"
  rownames(mat) <- names(model[[statistic]])
  mat$stat <- model[[statistic]]
  
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
    
    mat$SEu <- sqrt((mat$Pu * (1 - mat$Pu))/perm_n)
    
    mat$CIu_l <- mat$Pu - (qnorm(alpha/2, mean = 0, sd = 1, 
                                 lower.tail = F) * mat$SEu)
    mat$CIu_u <- mat$Pu + (qnorm(alpha/2, mean = 0, sd = 1, 
                                 lower.tail = F) * mat$SEu)
    
  }
  
  mat <- mat |> 
    tibble::as_tibble(rownames = NA) |>
    tibble::rownames_to_column()
  
  return(mat)
  
  }

}
