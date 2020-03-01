source("code/library.R")
# Download polls from FiveThirtyEight
download.file(url = "https://projects.fivethirtyeight.com/2020-primary-data/primary_polls_2020.csv",
              destfile = "data/fivethirtyeight-data/primary_polls_2020.csv")

primary_polls_post_sc <- read_csv("data/fivethirtyeight-data/primary_polls_2020.csv") %>%
  filter(race == "2020D", population != "a") %>%
  mutate(start_date = as.Date(startdate, format = "%m/%d/%Y"),
         end_date = as.Date(enddate, format = "%m/%d/%Y"),
         spread = as.numeric(end_date - start_date) + 1,
         median_date = start_date + round(spread/2),
         age = as.numeric(today() - median_date),
         pop = toupper(population),
         loess_weight = ifelse(median_date >= as.Date("2020-02-24"), 16, 1) * 10 * samplesize^0.25 / (ifelse(pop == "LV", 1, 3) * sqrt(abs(spread - 4) + 2) * ifelse(spread == 1, 5, 1)),
         weight = case_when(state == "National" ~ loess_weight * (age <= 45) / exp(age^0.8),
                            state != "National" ~ loess_weight / exp(age^(0.8))),
         candidate = case_when(grepl("Bennet", candidate_name) ~ "bennet",
                               grepl("Biden", candidate_name) ~ "biden",
                               grepl("Bloomberg", candidate_name) ~ "bloomberg",
                               grepl("Booker", candidate_name) ~ "booker",
                               grepl("Buttigieg", candidate_name) ~ "buttigieg",
                               grepl("Delaney", candidate_name) ~ "delaney",
                               grepl("Gabbard", candidate_name) ~ "gabbard",
                               grepl("Klobuchar", candidate_name) ~ "klobuchar",
                               grepl("Patrick", candidate_name) ~ "patrick",
                               grepl("Sanders", candidate_name) ~ "sanders",
                               grepl("Steyer", candidate_name) ~ "steyer",
                               grepl("Warren", candidate_name) ~ "warren",
                               grepl("Yang", candidate_name) ~ "yang"),
         pct = pct / 100) %>%
  arrange(age, pollster, candidate_name) %>%
  dplyr::select(state, pollster, median_date, age, spread, n = samplesize, pop, loess_weight, weight, candidate, pct) %>%
  spread(candidate, pct) %>%
  filter(!is.na(biden), !is.na(sanders), !is.na(warren), n >= 30) %>%
  melt(id.vars = c("state", "pollster", "median_date", "age", "spread", "n", "pop", "loess_weight", "weight"), 
       variable.name = "candidate", value.name = "pct") %>%
  arrange(state, median_date, pollster, candidate) %>%
  filter(!candidate %in% c("bennet", "booker", "delaney", "gabbard", "patrick", "harris"),
         !(candidate == "bloomberg" & state %in% c("Iowa", "New Hampshire", "Nevada", "South Carolina"))) %>%
  as.tbl()

primary_polls_pre_sc <- read_csv("data/fivethirtyeight-data/primary_polls_2020_pre_sc.csv") %>%
  filter(race == "2020D", population != "a") %>%
  mutate(start_date = as.Date(startdate, format = "%m/%d/%Y"),
         end_date = as.Date(enddate, format = "%m/%d/%Y"),
         spread = as.numeric(end_date - start_date) + 1,
         median_date = start_date + round(spread/2),
         age = as.numeric(today() - median_date),
         pop = toupper(population),
         loess_weight = ifelse(median_date >= as.Date("2020-02-29"), 16, 1) * 10 * samplesize^0.25 / (ifelse(pop == "LV", 1, 3) * sqrt(abs(spread - 4) + 2) * ifelse(spread == 1, 5, 1)),
         weight = case_when(state == "National" ~ loess_weight * (age <= 45) / exp(age^0.8),
                            state != "National" ~ loess_weight / exp(age^(0.8))),
         candidate = case_when(grepl("Bennet", candidate_name) ~ "bennet",
                               grepl("Biden", candidate_name) ~ "biden",
                               grepl("Bloomberg", candidate_name) ~ "bloomberg",
                               grepl("Booker", candidate_name) ~ "booker",
                               grepl("Buttigieg", candidate_name) ~ "buttigieg",
                               grepl("Delaney", candidate_name) ~ "delaney",
                               grepl("Gabbard", candidate_name) ~ "gabbard",
                               grepl("Klobuchar", candidate_name) ~ "klobuchar",
                               grepl("Patrick", candidate_name) ~ "patrick",
                               grepl("Sanders", candidate_name) ~ "sanders",
                               grepl("Steyer", candidate_name) ~ "steyer",
                               grepl("Warren", candidate_name) ~ "warren",
                               grepl("Yang", candidate_name) ~ "yang"),
         pct = pct / 100) %>%
  arrange(age, pollster, candidate_name) %>%
  dplyr::select(state, pollster, median_date, age, spread, n = samplesize, pop, loess_weight, weight, candidate, pct) %>%
  spread(candidate, pct) %>%
  filter(!is.na(biden), !is.na(sanders), !is.na(warren), n >= 30) %>%
  melt(id.vars = c("state", "pollster", "median_date", "age", "spread", "n", "pop", "loess_weight", "weight"), 
       variable.name = "candidate", value.name = "pct") %>%
  arrange(state, median_date, pollster, candidate) %>%
  filter(!candidate %in% c("bennet", "booker", "delaney", "gabbard", "patrick", "harris"),
         !(candidate == "bloomberg" & state %in% c("Iowa", "New Hampshire", "Nevada", "South Carolina"))) %>%
  as.tbl()

primary_polls <- bind_rows(primary_polls_pre_sc, primary_polls_post_sc) %>%
  distinct(pollster, state, median_date, spread, candidate, .keep_all = TRUE) %>%
  mutate(pct = case_when(is.na(pct) ~ 0,
                         !is.na(pct) ~ pct))

national_polls <- primary_polls %>%
  filter(state == "National")

# Calculate national polling averages over time without adjustment
start_date <- as.Date("2019-03-01")
end_date <- today()
dates <- seq(from = start_date, to = end_date, by = 1)
n_days <- length(dates)

polls_df_list <- poll_average_list <- poll_var_list <- poll_eff_n_list <- vector("list", n_days)

for(i in 1:n_days) {
  polls_df_list[[i]] <- national_polls %>%
    filter(median_date <= dates[i], !is.na(pct)) %>%
    mutate(age = as.numeric(dates[i] - median_date),
           weight = loess_weight*(age <= 45)/exp(age^0.4)) %>%
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

national_averages <- averages %>%
  mutate(alpha = alphas,
         beta = betas) %>%
  group_by(candidate) %>%
  mutate(pct = (lag(pct, 4) + lag(pct, 3) + lag(pct, 2) + lag(pct) + pct) / 5,
         alpha = (lag(alpha, 4) + lag(alpha, 3) + lag(alpha, 2) + lag(alpha) + alpha) / 5,
         beta = (lag(beta, 4) + lag(beta, 3) + lag(beta, 2) + lag(beta) + beta) / 5,
         lower = qbeta(p = 0.05, shape1 = alpha, shape2 = beta),
         upper = qbeta(p = 0.95, shape1 = alpha, shape2 = beta)) %>%
  ungroup() %>%
  dplyr::select(candidate, median_date, pct_natl = pct)

national_polls_with_avg <- national_polls %>%
  left_join(national_averages %>% mutate(pct_natl = lead(pct_natl, 2)), by = c("candidate", "median_date")) %>%
  filter(!is.na(pct_natl))

house_effect_umm <- lmer(pct ~ pct_natl + (1 | candidate/pollster), data = national_polls_with_avg, weights = loess_weight)
house_effect_ranefs <- ranef(house_effect_umm)$`pollster:candidate` %>%
  as.data.frame() 

house_effects <- house_effect_ranefs %>%
  mutate(rownames = rownames(house_effect_ranefs),
         pollster = str_split(rownames, ":") %>% sapply(head, n = 1),
         candidate = str_split(rownames, ":") %>% sapply(tail, n = 1)) %>%
  dplyr::select(pollster, candidate, house = `(Intercept)`) %>%
  as.tbl()

national_polls_adjusted <- national_polls %>%
  left_join(house_effects, by = c("pollster", "candidate")) %>%
  mutate(pct = pct - house)

# Recalculate national averages with house effect adjustments
start_date <- as.Date("2019-03-01")
end_date <- today()
dates <- seq(from = start_date, to = end_date, by = 1)
n_days <- length(dates)

polls_df_list <- poll_average_list <- poll_var_list <- poll_eff_n_list <- vector("list", n_days)

for(i in 1:n_days) {
  polls_df_list[[i]] <- national_polls_adjusted %>%
    filter(median_date <= dates[i], !is.na(pct)) %>%
    mutate(age = as.numeric(dates[i] - median_date),
           weight = loess_weight*(age <= 45)/exp(age^0.4)) %>%
    dplyr::select(-age, -spread, -n) %>%
    filter(weight > 0)
  
  poll_average_list[[i]] <- polls_df_list[[i]] %>%
    group_by(candidate) %>%
    summarise(pct = wtd.mean(pct, weight, na.rm = TRUE)) %>%
    mutate(median_date = dates[i])
  
  poll_var_list[[i]] <- polls_df_list[[i]] %>%
    group_by(candidate) %>%
    summarise(var = wtd.var(pct, weight, na.rm = TRUE)) %>%
    mutate(median_date = dates[i])
  
  poll_eff_n_list[[i]] <- polls_df_list[[i]] %>%
    group_by(candidate) %>%
    summarise(eff_n = sum(weight, na.rm = TRUE)^2 / sum(weight^2, na.rm = TRUE)) %>%
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
  dplyr::select(candidate, median_date, pct, var, eff_n, lower, upper, alpha, beta)

# Current average
national_average <- national_polls_adjusted %>%
  filter(weight > 0) %>%
  group_by(candidate) %>%
  summarise(avg = wtd.mean(pct, weight),
            eff_n = sum(weight)^2 / sum(weight^2),
            var = wtd.var(pct, weight))

alphas_avg <- betas_avg <- rep(0, nrow(national_average))
for(i in 1:nrow(national_average)) {
  beta_estimates <- betaMOM(mu = national_average$avg[i], v = national_average$var[i] / national_average$eff_n[i])
  alphas_avg[i] <- beta_estimates["alpha"]
  betas_avg[i] <- beta_estimates["beta"]
}

national_average <- national_average %>%
  mutate(alpha = alphas_avg,
         beta = betas_avg,
         lower = 100 * qbeta(p = 0.05, shape1 = alpha, shape2 = beta),
         upper = 100 * qbeta(p = 0.95, shape1 = alpha, shape2 = beta),
         avg = 100 * avg)

# State polls + national trend line adjustment
state_polls <- primary_polls %>%
  filter(state != "National") %>%
  left_join(national_averages_adjusted %>% dplyr::select(candidate, median_date, natl_pct = pct), by = c("candidate", "median_date")) %>%
  left_join(house_effects, by = c("pollster", "candidate")) %>%
  mutate(house = case_when(is.na(house) ~ 0,
                           !is.na(house) ~ house),
         pct_adjusted = 100*(pct - house))

state_leans <- state_polls %>%
  mutate(state_lean = pct_adjusted - natl_pct) %>%
  group_by(state, candidate) %>%
  summarise(lean_avg = wtd.mean(state_lean / 100, weight),
            lean_var = pmax(wtd.var(state_lean / 100, weight), 0) + 0.0025,
            lean_eff_n = sum(weight)^2 / sum(weight^2)) %>%
  mutate(lean_avg = case_when(is.na(lean_avg) ~ 0,
                              !is.na(lean_avg) ~ lean_avg)) %>%
  ungroup()

state_averages <- state_leans %>%
  left_join(national_average %>% dplyr::select(candidate, natl_avg = avg, natl_var = var, natl_eff_n = eff_n), by = "candidate") %>%
  mutate(state_avg = lean_avg + natl_avg / 100) %>%
  group_by(state) %>%
  mutate(state_avg_sum = sum(state_avg),
         state_var = (lean_var / lean_eff_n + natl_var / natl_eff_n) / state_avg_sum^2) %>%
  dplyr::select(state, candidate, state_avg, state_var, state_eff_n = lean_eff_n) %>%
  ungroup()

state_alphas <- state_betas <- rep(0, nrow(state_averages))
for(i in 1:nrow(state_averages)) {
  beta_estimates <- betaMOM(mu = state_averages$state_avg[i], v = state_averages$state_var[i])
  state_alphas[i] <- beta_estimates["alpha"]
  state_betas[i] <- beta_estimates["beta"]
}

state_averages <- state_averages %>%
  mutate(alpha = state_alphas,
         beta = state_betas, 
         lower = 100 * qbeta(p = 0.05, shape1 = alpha, shape2 = beta),
         upper = 100 * qbeta(p = 0.95, shape1 = alpha, shape2 = beta),
         state_avg = 100 * state_avg)