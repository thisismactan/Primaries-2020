if("cces_2016.csv" %in% list.files("data")) {
  source("code/primary_vote_prob.R")
}

if("dem_primary_logit.rds" %in% list.files("code")) {
  dem_primary_logit <- read_rds("code/dem_primary_logit.rds")
}

# Some functions ####
## Creating a synthetic population
createSynthPop <- function(state_name, data = district_demographics) {
  require(dplyr)
  state_demographics <- data %>%
    filter(state == state_name) %>%
    mutate(prob = predict(dem_primary_logit, newdata = ., type = "response"),
           n_vote = round(pop * prob * turnout * 0.1)) %>%
    select(-pop, -prob)
  
  state_synth_pop <- state_demographics %>%
    lapply(rep, state_demographics$n_vote) %>%
    as.data.frame() %>%
    as.tbl() %>%
    mutate(rand = runif(n()),
           age_regroup = case_when(age == "18-24" ~ "18-29",
                                   age == "25-34" & rand < 0.5 ~ "18-29",
                                   age == "25-34" & rand >= 0.5 ~ "30-44",
                                   age == "35-44" ~ "30-44",
                                   age == "45-64" ~ "45-64",
                                   age == "65+" ~ "65+") %>% factor()) %>%
    select(state, district, primary_type, district_delegates, gender, race, age = age_regroup, education)
  
  return(state_synth_pop)
}

## Add demographic leans to state averages
demographicBreakdown <- function(state_name, poll_average_tbl = state_averages, demographic_lean_tbl = demographic_lean_sim) {
  require(dplyr)
  
  # State polling average
  state_average <- poll_average_tbl %>%
    filter(state == state_name) %>%
    mutate(candidate = as.character(candidate)) %>%
    left_join(demographic_lean_tbl, by = "candidate") %>%
    na.omit() %>%
    dplyr::select(state, candidate, state_avg, demographic_var, demographic, lean) %>%
    melt(id.vars = c("state", "candidate", "state_avg", "demographic_var"), variable.name = "demographic", value.name = "lean") %>%
    mutate(state_pct = state_avg + lean) %>%
    dplyr::select(candidate, demographic, pct = state_pct) %>%
    spread(demographic, pct) %>%
    dplyr::select(-state_var, -alpha, -beta, -lower, -upper)
  
  return(state_average)
}

district_demographics <- read_csv("data/district_demographics.csv") %>%
  left_join(read_csv("data/demographic_turnout.csv"), by = c("gender", "race", "age", "education"))