source("code/shape_fivethirtyeight_polls.R")

start_date <- as.Date("2019-06-01")
end_date <- today()
dates <- seq(from = start_date, to = end_date, by = 1)
n_days <- length(dates)

polls_df_list <- poll_average_list <- poll_var_list <- poll_eff_n_list <- vector("list", n_days)

state_poll_leans <- state_polls %>%
  mutate(lean = (pct_adjusted - natl_pct) / 100)

for(i in 1:n_days) {
  polls_df_list[[i]] <- state_poll_leans %>%
    filter(median_date <= dates[i]) %>%
    mutate(age = as.numeric(dates[i] - median_date),
           weight = loess_weight / exp(age^(0.4))) %>%
    dplyr::select(-age, -spread, -n, -pop) %>%
    filter(weight > 0) 
  
  poll_average_list[[i]] <- polls_df_list[[i]] %>%
    group_by(state, candidate) %>%
    summarise(lean = wtd.mean(lean, weight)) %>%
    mutate(median_date = dates[i])
  
  poll_var_list[[i]] <- polls_df_list[[i]] %>%
    group_by(state, candidate) %>%
    summarise(state_var = max(0, wtd.var(lean, weight))) %>%
    mutate(median_date = dates[i])
  
  poll_eff_n_list[[i]] <- polls_df_list[[i]] %>%
    group_by(state, candidate) %>%
    summarise(state_eff_n = sum(weight)^2 / sum(weight^2)) %>%
    mutate(median_date = dates[i])
}

# Estimate alpha and beta at every day
state_averages_over_time_unsmoothed <- bind_rows(poll_average_list) %>%
  left_join(bind_rows(poll_var_list), by = c("state", "candidate", "median_date")) %>%
  left_join(bind_rows(poll_eff_n_list), by = c("state", "candidate", "median_date")) %>%
  arrange(state, candidate, median_date) %>%
  left_join(national_averages_adjusted %>% dplyr::select(candidate, median_date, natl_pct = pct, natl_var = var, natl_eff_n = eff_n), 
            by = c("candidate", "median_date")) %>%
  mutate(pct = pmax(0.001, lean + natl_pct / 100),
         var = state_var / state_eff_n + natl_var / natl_eff_n) %>%
  na.omit() %>%
  ungroup() %>%
  filter(!(state %in% c("Iowa", "New Hampshire", "Nevada", "South Carolina") & candidate == "bloomberg"))

state_alphas_over_time <- state_betas_over_time <- rep(0, nrow(state_averages_over_time_unsmoothed))

for(i in 1:nrow(state_averages_over_time_unsmoothed)) {
  beta_estimates <- betaMOM(mu = state_averages_over_time_unsmoothed$pct[i], v = state_averages_over_time_unsmoothed$var[i])
  state_alphas_over_time[i] <- beta_estimates["alpha"]
  state_betas_over_time[i] <- beta_estimates["beta"]
}

state_averages_over_time <- state_averages_over_time_unsmoothed %>%
  mutate(alpha = state_alphas_over_time,
         beta = state_betas_over_time) %>%
  group_by(state, candidate) %>%
  mutate(pct = 100 * (lag(pct, 4) + lag(pct, 3) + lag(pct, 2) + lag(pct) + pct) / 5,
         alpha = (lag(alpha, 4) + lag(alpha, 3) + lag(alpha, 2) + lag(alpha) + alpha) / 5,
         beta = (lag(beta, 4) + lag(beta, 3) + lag(beta, 2) + lag(beta) + beta) / 5) %>%
  ungroup() %>%
  mutate(lower = 100 * qbeta(p = 0.05, shape1 = alpha, shape2 = beta),
         upper = 100 * qbeta(p = 0.95, shape1 = alpha, shape2 = beta)) %>%
  dplyr::select(state, candidate, median_date, pct, lower, upper, alpha, beta, state_eff_n)
