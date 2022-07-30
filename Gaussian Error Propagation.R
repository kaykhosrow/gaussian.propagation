formula = 

mutate.error = function(.data, f) {
  exprs = list(
    deparse(f[[3]]),
    sapply(all.vars(f[[3]]), function(v) {
      dfdp = deparse(D(f[[3]], v))
      sprintf('(d%s*(%s))^2', v, dfdp)
    }) %>%
      paste(collapse='+') %>%
      sprintf('sqrt(%s)', .)
  )
  names(exprs) = c(
    deparse(f[[2]]),
    sprintf('d%s', deparse(f[[2]]))
  )
  .data %>%
    mutate_(.dots=exprs)
}

results <- data %>% mutate.error(formula)

