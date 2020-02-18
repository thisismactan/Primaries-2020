source("code/shape_fivethirtyeight_polls.R")

start_date <- as.Date("2019-03-01")
end_date <- today()
dates <- seq(from = start_date, to = end_date, by = 1)
n_days <- length(dates)

polls_df_list <- poll_average_list <- poll_var_list <- poll_eff_n_list <- vector("list", n_days)

for(i in 1:n_days) {
  polls_df_list[[i]] <- national_polls_adjusted %>%
    filter(median_date <= dates[i], !is.na(pct)) %>%
    mutate(age = as.numeric(dates[i] - median_date),
           weight = loess_weight*(age <= 45)/exp(age^0.5)) %>%
    dplyr::select(-age, -spread, -n) %>%
    filter(weight > 0)
  
  poll_average_list[[i]] <- polls_df_list[[i]] %>%
    group_by(candidate) %>%
    summarise(pct = wtd.mean(pct, weight)) %>%
    mutate(median_date = dates[i])
  
  poll_var_list[[i]] <- polls_df_list[[i]] %>%
    group_by(candidate) %>%
    summarise(var = wtd.var(pct, weight)) %>%
    mutate(median_date = dates[i])
  
  poll_eff_n_list[[i]] <- polls_df_list[[i]] %>%
    group_by(candidate) %>%
    summarise(eff_n = sum(weight)^2 / sum(weight^2)) %>%
    mutate(median_date = dates[i])
}

averages <- bind_rows(poll_average_list) %>%
  left_join(bind_rows(poll_var_list), by = c("candidate", "median_date")) %>%
  left_join(bind_rows(poll_eff_n_list), by = c("candidate", "median_date")) %>%
  arrange(candidate, median_date)

alphas <- betas <- rep(0, nrow(averages))

for(i in 1:nrow(averages)) {
  beta_estimates <- betaMOM(mu = averages$pct[i], v = averages$var[i]/averages$eff_n[i])
  alphas[i] <- beta_estimates["alpha"]
  betas[i] <- beta_estimates["beta"]
}

national_averages_adjusted <- averages %>%
  mutate(alpha = alphas,
         beta = betas) %>%
  group_by(candidate) %>%
  mutate(pct = 100 * (lag(pct, 4) + lag(pct, 3) + lag(pct, 2) + lag(pct) + pct) / 5,
         alpha = (lag(alpha, 4) + lag(alpha, 3) + lag(alpha, 2) + lag(alpha) + alpha) / 5,
         beta = (lag(beta, 4) + lag(beta, 3) + lag(beta, 2) + lag(beta) + beta) / 5,
         lower = 100 * qbeta(p = 0.05, shape1 = alpha, shape2 = beta),
         upper = 100 * qbeta(p = 0.95, shape1 = alpha, shape2 = beta)) %>%
  ungroup() %>%
  dplyr::select(candidate, median_date, pct, lower, upper, alpha, beta)
