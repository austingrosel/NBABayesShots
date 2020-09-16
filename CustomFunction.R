build_bayesian_estimate_table = function(df, make_var, att_var, make_str, att_str) {
  make_var = enquo(make_var)
  att_var = enquo(att_var)
  
  df_model = df %>%
    dplyr::select(basic_pos, !! make_var, !! att_var)
  
  f = paste0("cbind(", make_str, ", ", att_str, " - ", make_str, ") ~ basic_pos")
  
  fit <- gamlss(formula(f),
                data = df_model %>%
                  filter(!! att_var > 0),
                family = BB(mu.link = "identity"))
  
  returning_df = df %>%
    filter((!! att_var) > 0) %>%
    ungroup() %>%
    dplyr::select(PLAYER_NAME, basic_pos, nbaDebutYear, !! make_var, !! att_var) %>%
    mutate(FG_PCT = (!! make_var)/(!! att_var),
           mu = fitted(fit, "mu"),
           sigma = fitted(fit, "sigma"),
           alpha0 = mu / sigma,
           beta0 = (1 - mu) / sigma,
           alpha1 = alpha0 + (!! make_var),
           beta1 = beta0 + (!! att_var) - (!! make_var),
           estimate = alpha1 / (alpha1 + beta1)) %>%
    group_by(basic_pos) %>%
    mutate(est_percentile = round(100 * rank(estimate)/length(estimate)))
  
  return(returning_df)
}




build_prior = function(df, make_var, att_var) {
  df = df %>%
    filter(!! enquo(att_var) >= 50) #%>%
  #mutate(FG_PCT = (!! make_var)/(!! att_var))
  m <- MASS::fitdistr(df$FG_PCT, dbeta,
                      start = list(shape1 = 1, shape2 = 10))
  return(list(alpha0 = m$estimate[1], beta0 = m$estimate[2]))
}



