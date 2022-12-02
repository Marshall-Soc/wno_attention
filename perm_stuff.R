


gsub(perm_v, paste0(perm_v, "_perm"), model$call) %>% as.expression()



as.expression(do.call("substitute", list(exp[[1]], list(T = as.name("I")))))

f <- function(x) as.name(perm_v_n)
condition <- function(x) as.name(perm_v)

temp <- rrapply(model$call, condition = condition, f = f, how = "replace")

f2 <- function(x) as.name(".")
condition2 <- function(x) as.name(deparse(substitute(wno_data2)))

temp <- rrapply(temp, condition = condition2, f = f2, how = "replace")

update.formula(model, perm_v_n ~ .)
