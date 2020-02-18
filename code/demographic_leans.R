source("code/library.R")

poll_demographics <- read_csv("data/poll_demographics.csv") %>%
  dplyr::select(-inc_50_100, -inc_100)

demographic_leans <- poll_demographics %>%
  mutate(age = as.numeric(today() - median_date),
         weight = ifelse(median_date <= as.Date("2020-02-12"), 16, 1) * 10 * (n^0.25) * (age <= 45) * (3^(pop == "LV")) / (exp(age^0.5) * (5^(mode == "IVR")) * sqrt(abs(spread - 4) + 3))) %>%
  melt(id.vars = c("pollster", "median_date", "age", "spread", "n", "pop", "mode", "candidate", "weight", "total"), 
       variable.name = "demographic", value.name = "pct") %>%
  filter(weight > 0, !is.na(pct)) %>%
  mutate(lean = pct - total) %>%
  group_by(candidate = tolower(candidate), demographic) %>%
  summarise(avg_lean = wtd.mean(lean, weight, na.rm = TRUE),
            lean_sd = sqrt(n() * wtd.var(lean, weight, na.rm = TRUE) / (n() - 1) + 0.5 / wtd.mean(n, weight)),
            n_eff = sum(weight)^2 / sum(weight^2)) %>%
  mutate(lean_se = lean_sd / sqrt(n_eff),
         demographic_var = case_when(demographic %in% c("white", "black", "latino") ~ "race",
                                     demographic %in% c("men", "women") ~ "gender",
                                     demographic %in% c("college", "no_college") ~ "education",
                                     demographic %in% c("inc_0_50", "inc_50") ~ "income",
                                     grepl("age", demographic) ~ "age")) %>%
  ungroup()

demographic_leans %>%
  filter(candidate %in% c("biden", "bloomberg", "buttigieg", "klobuchar", "sanders", "warren")) %>%
  dplyr::select(candidate, demographic, avg_lean) %>%
  mutate(avg_lean = round(avg_lean, 1)) %>%
  spread(candidate, avg_lean)

demographic_leans %>%
  ungroup() %>%
  mutate(candidate = tolower(candidate)) %>%
  left_join(national_average %>% dplyr::select(candidate, avg), by = "candidate") %>%
  mutate(pct_est = avg + avg_lean) %>%
  dplyr::select(candidate, demographic, pct_est) %>%
  spread(candidate, pct_est)
