start_time <- Sys.time()
source("code/state_poll_average_over_time.R")
source("code/demographic_leans.R")
source("code/state_similarities.R")

if(!("district_demographics.csv" %in% list.files("data"))) {
  source("code/census-mining/get_demographics.R")
}
state_results <- read_csv("data/state_results.csv")
district_demographics <- read_csv("data/district_demographics.csv") %>%
  left_join(read_csv("data/demographic_turnout.csv"))

# National demographics ####
natl_marginal_demographics <- district_demographics %>%
  group_by(gender, race, age, education) %>%
  summarise(pop = sum(pop, na.rm = TRUE)) %>%
  na.omit()

natl_gender_demographics <- natl_marginal_demographics %>%
  group_by(gender) %>%
  summarise(pop = sum(pop) / sum(natl_marginal_demographics$pop)) %>%
  mutate(gender = case_when(gender == "Female" ~ "women",
                            gender == "Male" ~ "men")) %>%
  dplyr::select(demographic = gender, natl_demo_pct = pop)

natl_race_demographics <- natl_marginal_demographics %>%
  group_by(race) %>%
  summarise(pop = sum(pop) / sum(natl_marginal_demographics$pop)) %>%
  mutate(race = tolower(race)) %>%
  dplyr::select(demographic = race, natl_demo_pct = pop)

natl_age_demographics <- natl_marginal_demographics %>%
  group_by(age) %>%
  summarise(pop = sum(pop) / sum(natl_marginal_demographics$pop)) %>%
  mutate(pop = case_when(age == "18-24" ~ pop + 0.5*lead(pop),
                         age == "35-44" ~ pop + 0.5*lag(pop),
                         !(age %in% c("18-24", "35-44")) ~ pop),
         age = case_when(age == "18-24" ~ "age_1829",
                         age == "35-44" ~ "age_3044",
                         age == "45-64" ~ "age_4564",
                         age == "65+" ~ "age_65")) %>%
  na.omit() %>%
  dplyr::select(demographic = age, natl_demo_pct = pop)

natl_education_demographics <- natl_marginal_demographics %>%
  group_by(education) %>%
  summarise(pop = sum(pop) / sum(natl_marginal_demographics$pop)) %>%
  mutate(education = case_when(education == "College" ~ "college",
                               education == "Less than college" ~ "no_college")) %>%
  dplyr::select(demographic = education, natl_demo_pct = pop)

natl_marginal_demographics <- bind_rows(natl_gender_demographics, natl_race_demographics,
                                        natl_age_demographics, natl_education_demographics)

source("code/create_synth_pop.R")

# Functions to shape state demographics
shapeStateDemo <- function(state_name) {
  require(dplyr)
  require(reshape2)
  require(tidyr)
  require(Hmisc)
  
  state_marginal_demographics <- district_demographics %>%
    filter(state == state_name) %>%
    group_by(state, gender, race, age, education) %>%
    summarise(pop = sum(pop, na.rm = TRUE))
  
  state_gender_demographics <- state_marginal_demographics %>%
    group_by(gender) %>%
    summarise(pop = sum(pop) / sum(state_marginal_demographics$pop)) %>%
    mutate(gender = case_when(gender == "Female" ~ "women",
                              gender == "Male" ~ "men")) %>%
    dplyr::select(demographic = gender, state_demo_pct = pop)
  
  state_race_demographics <- state_marginal_demographics %>%
    group_by(race) %>%
    summarise(pop = sum(pop) / sum(state_marginal_demographics$pop)) %>%
    mutate(race = tolower(race)) %>%
    dplyr::select(demographic = race, state_demo_pct = pop)
  
  state_age_demographics <- state_marginal_demographics %>%
    group_by(age) %>%
    summarise(pop = sum(pop) / sum(state_marginal_demographics$pop)) %>%
    mutate(pop = case_when(age == "18-24" ~ pop + 0.5*lead(pop),
                           age == "35-44" ~ pop + 0.5*lag(pop),
                           !(age %in% c("18-24", "35-44")) ~ pop),
           age = case_when(age == "18-24" ~ "age_1829",
                           age == "35-44" ~ "age_3044",
                           age == "45-64" ~ "age_4564",
                           age == "65+" ~ "age_65")) %>%
    na.omit() %>%
    dplyr::select(demographic = age, state_demo_pct = pop)
  
  state_education_demographics <- state_marginal_demographics %>%
    group_by(education) %>%
    summarise(pop = sum(pop) / sum(state_marginal_demographics$pop)) %>%
    mutate(education = case_when(education == "College" ~ "college",
                                 education == "Less than college" ~ "no_college")) %>%
    dplyr::select(demographic = education, state_demo_pct = pop)
  
  state_marginal_demographics <- bind_rows(state_gender_demographics, state_race_demographics,
                                           state_age_demographics, state_education_demographics)
  
  return(state_marginal_demographics)
}

demoCandidateBreakdown <- function(sim_state, statewide_percentages, demo_leans, state_demo_breakdown) {
  require(dplyr)
  require(Hmisc)
  state_lean_sim <- state_averages %>%
    # Filter to right state
    filter(state == sim_state) %>%
    
    # Simulate actual state share of vote
    left_join(this_sim_state_percentage, by = "candidate") %>%
    left_join(demographic_lean_sim, by = "candidate") %>%
    left_join(state_marginal_demographics, by = "demographic") %>%
    left_join(natl_marginal_demographics, by = "demographic") %>%
    dplyr::select(-state_avg, -state_var, -alpha, -beta, -lower, -upper) %>%
    
    # Scale using state-national demographic ratio, and shift so that when scaled and shifted the leans cancel to zero
    mutate(disproportionality_factor = natl_demo_pct / state_demo_pct) %>%
    group_by(demographic_var, candidate) %>%
    mutate(scale_factor = disproportionality_factor^2 / prod(disproportionality_factor),
           scaled_lean = scale_factor*lean,
           scaled_lean = scaled_lean - wtd.mean(scaled_lean, state_demo_pct)) %>%
    ungroup() %>%
    dplyr::select(sim_id, state, candidate, state_pct, demographic, scaled_lean)
  
  state_demographic_sim <- state_lean_sim %>%
    mutate(demo_pct = pmax(state_pct + scaled_lean/100, 0))
  
  return(state_demographic_sim)
}

# Second choices and district delegates
second_choice <- read_csv("data/second_choice.csv")
second_choice_t <- second_choice %>%
  dplyr::select(-first_choice) %>%
  as.matrix() %>%
  t() %>%
  as.data.frame() %>%
  mutate(second_choice = c("biden", "bloomberg", "buttigieg", "klobuchar", "sanders", "steyer", "warren", "yang")) %>%
  as.tbl() %>%
  dplyr::select(second_choice, biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, steyer = V6, 
                warren = V7, yang = V8)

district_delegates <- read_csv("data/district_delegates.csv") %>%
  dplyr::select(state, district, delegates)

# Setting up the cluster ####
no_cores <- 10
cl <- makeCluster(no_cores, outfile = "output/sim_log.txt")
registerDoParallel(cl)

# Simulate all of the baseline simulations for all of the contests
set.seed(2020)
n_sims <- 1000

############### SIMULATIONS START HERE #################
# Iowa (3 Feb 2020) ####
ia_start_time <- Sys.time()
# Draw votes
sim_demographics_table <- district_demographics %>%
  filter(state == "Iowa") %>%
  mutate(prob = predict(dem_primary_logit, newdata = ., type = "response"),
         n_vote = round(pop * prob * turnout * 0.05)) %>%
  dplyr::select(-pop, -prob)

sim_demographics <- sim_demographics_table %>%
  lapply(rep, sim_demographics_table$n_vote) %>%
  as.data.frame() 

state_marginal_demographics <- shapeStateDemo("Iowa")

### Vote share
ia_results <- state_results %>%
  filter(state == "Iowa")

ia_dirichlet_params <- ((ia_results %>%
  filter(district == "At-large") %>%
  dplyr::select(candidate, pct) %>%
  spread(candidate, pct) %>%
  as.data.frame() %>%
  as.matrix() %>%
  diri.est())$param) * 100000

ia_state_percentages <- rdirichlet(n_sims, ia_dirichlet_params) %>%
  as.data.frame() %>%
  dplyr::select(biden = V1, buttigieg = V2, klobuchar = V3, sanders = V4, steyer = V5, warren = V6, yang = V7) %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

ia_sims <- vector("list", n_sims)

ia_sims <- foreach(i = 1:n_sims, .packages = c("dplyr")) %dopar% {
  ia_results %>% 
    mutate(sim_id = i)
}

primaryPlace <- function(candidate_name, sims) {
  sapply(sims, function(df) df %>% 
           filter(district == "At-large") %>%
           arrange(desc(pct)) %>%
           mutate(place = 1:n()) %>%
           filter(candidate == candidate_name) %>%
           pull(place))
}

biden_ia_place <- primaryPlace("biden", ia_sims)
sanders_ia_place <- primaryPlace("sanders", ia_sims) 
warren_ia_place <- primaryPlace("warren", ia_sims)
steyer_ia_place <- primaryPlace("steyer", ia_sims)
buttigieg_ia_place <- primaryPlace("buttigieg", ia_sims)
klobuchar_ia_place <- primaryPlace("klobuchar", ia_sims)
yang_ia_place <- primaryPlace("yang", ia_sims)

biden_dropout_prob <- 0
buttigieg_dropout_prob <- 0
klobuchar_dropout_prob <- 0
sanders_dropout_prob <- 0
steyer_dropout_prob <- 0
warren_dropout_prob <- 0
yang_dropout_prob <- 0

biden_dropout <- rbernoulli(n_sims, biden_dropout_prob)
buttigieg_dropout <- rbernoulli(n_sims, buttigieg_dropout_prob)
klobuchar_dropout <- rbernoulli(n_sims, klobuchar_dropout_prob)
sanders_dropout <- rbernoulli(n_sims, sanders_dropout_prob)
steyer_dropout <- rbernoulli(n_sims, steyer_dropout_prob)
warren_dropout <- rbernoulli(n_sims, warren_dropout_prob)
yang_dropout <- rbernoulli(n_sims, yang_dropout_prob)

ia_stayin_matrix <- !cbind("biden" = biden_dropout, 
                           "buttigieg" = buttigieg_dropout,
                           "klobuchar" = klobuchar_dropout,
                           "sanders" = sanders_dropout,
                           "steyer" = steyer_dropout,
                           "warren" = warren_dropout,
                           "yang" = yang_dropout)

ia_second_choice_division <- ia_stayin_matrix %>%
  as.data.frame() %>%
  melt(id.vars = NULL, variable.name = "candidate", value.name = "stay_in") %>%
  mutate(candidate = as.character(candidate),
         sim_id = rep(1:n_sims, 7)) %>%
  as.tbl() %>%
  left_join(second_choice %>% dplyr::select(-bloomberg), by = c("candidate" = "first_choice"))

### How much did the candidates overperform in Iowa?
ia_overperformance <- ia_state_percentages %>%
  left_join(state_averages_over_time_unsmoothed %>% filter(state == "Iowa", median_date == as.Date("2020-02-03")) %>% 
              dplyr::select(candidate, state_avg = pct), by = c("candidate")) %>%
  as.tbl() %>%
  mutate(overperformance = state_pct - state_avg/100) %>%
  dplyr::select(sim_id, candidate, overperformance)


# New Hampshire (11 Feb 2020) ####
nh_start_time <- Sys.time()

nh_results <- state_results %>%
  filter(state == "New Hampshire")

nh_dirichlet_params <- ((nh_results %>%
                           filter(district == "At-large") %>%
                           dplyr::select(candidate, pct) %>%
                           spread(candidate, pct) %>%
                           as.data.frame() %>%
                           as.matrix() %>%
                           diri.est())$param) * 100000


nh_state_percentages_adj <- nh_results %>%
  filter(district == "At-large") %>%
  dplyr::select(candidate, state_pct = pct) %>%
  dplyr::slice(rep(1:n(), each = n_sims)) %>%
  mutate(sim_id = rep(1:n_sims, 7))

nh_sims <- vector("list", n_sims)

nh_sims <- foreach(i = 1:n_sims, .packages = c("dplyr")) %dopar% {
  nh_results %>% 
    mutate(sim_id = i)
}

biden_nh_place <- primaryPlace("biden", nh_sims)
sanders_nh_place <- primaryPlace("sanders", nh_sims) 
warren_nh_place <- primaryPlace("warren", nh_sims)
steyer_nh_place <- primaryPlace("steyer", nh_sims)
buttigieg_nh_place <- primaryPlace("buttigieg", nh_sims)
klobuchar_nh_place <- primaryPlace("klobuchar", nh_sims)
yang_nh_place <- primaryPlace("yang", nh_sims)

biden_dropout_prob <- 0
buttigieg_dropout_prob <- 0
klobuchar_dropout_prob <- 0
sanders_dropout_prob <- 0
steyer_dropout_prob <- 0
warren_dropout_prob <- 0
yang_dropout_prob <- 1

biden_dropout <- rbernoulli(n_sims, biden_dropout_prob)
buttigieg_dropout <- rbernoulli(n_sims, buttigieg_dropout_prob)
klobuchar_dropout <- rbernoulli(n_sims, klobuchar_dropout_prob)
sanders_dropout <- rbernoulli(n_sims, sanders_dropout_prob)
steyer_dropout <- rbernoulli(n_sims, steyer_dropout_prob)
warren_dropout <- rbernoulli(n_sims, warren_dropout_prob)
yang_dropout <- rbernoulli(n_sims, yang_dropout_prob)

nh_places <- c(biden_nh_place, buttigieg_nh_place, klobuchar_nh_place, sanders_nh_place, steyer_nh_place, warren_nh_place,
               yang_nh_place)

nh_stayin_matrix <- !cbind("biden" = biden_dropout, 
                           "buttigieg" = buttigieg_dropout,
                           "klobuchar" = klobuchar_dropout,
                           "sanders" = sanders_dropout,
                           "steyer" = steyer_dropout,
                           "warren" = warren_dropout,
                           "yang" = yang_dropout)

# How much did the candidates overperform in New Hampshire?
nh_overperformance <- nh_state_percentages_adj %>%
  left_join(state_averages %>% filter(state == "New Hampshire") %>% dplyr::select(candidate, state_avg), by = c("candidate")) %>%
  mutate(overperformance = state_pct - state_avg/100) %>%
  dplyr::select(sim_id, candidate, overperformance) 

nh_second_choice_division <- nh_stayin_matrix %>%
  as.data.frame() %>%
  melt(id.vars = NULL, variable.name = "candidate", value.name = "stay_in") %>%
  mutate(candidate = as.character(candidate),
         sim_id = rep(1:n_sims, 7)) %>%
  as.tbl() %>%
  left_join(second_choice %>% dplyr::select(-bloomberg), by = c("candidate" = "first_choice"))

# Nevada (22 Feb 2020) ####
nv_start_time <- Sys.time()
# Draw votes
sim_demographics_table <- district_demographics %>%
  filter(state == "Nevada") %>%
  mutate(prob = predict(dem_primary_logit, newdata = ., type = "response"),
         n_vote = round(pop * prob * turnout * 0.05)) %>%
  dplyr::select(-pop, -prob)

sim_demographics <- sim_demographics_table %>%
  lapply(rep, sim_demographics_table$n_vote) %>%
  as.data.frame() 

state_marginal_demographics <- shapeStateDemo("Nevada")

### Draw vote shares
nv_averages_over_time <- state_averages_over_time_unsmoothed %>%
  filter(state == "Nevada", as.numeric(today() - median_date) <= 1) %>%
  mutate(age = as.numeric(today() - median_date),
         nreps = 2 - age)

nv_dirichlet_params <- (nv_averages_over_time %>%
                          lapply(rep, nv_averages_over_time$nreps) %>%
                          as.data.frame() %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          dplyr::select(-median_date) %>%
                          group_by(candidate) %>%
                          mutate(i = 1:n()) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-i) %>%
                          as.matrix() %>%
                          diri.est())$param

nv_dirichlet_params[7] <- 0

nv_state_percentages <- (rdirichlet(n_sims, nv_dirichlet_params) %>%
                            as.data.frame() %>%
                            dplyr::select(biden = V1, buttigieg = V2, klobuchar = V3, sanders = V4, 
                                          steyer = V5, warren = V6, yang = V7)) %>%
  apply(MARGIN = 1, FUN = function(x) x / sum(x)) %>%
  t() %>%
  as.data.frame() %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

nv_state_percentages_adj <- nv_state_percentages %>%
  left_join(nh_overperformance, by = c("sim_id", "candidate")) %>%
  mutate(bounce_factor = 1.5 * rbeta(n(), shape1 = 7, shape2 = 2),
         nh_place = nh_places,
         state_pct = pmax(state_pct + 1.5 * overperformance * bounce_factor + 0.02 * (3.5 - nh_place), 0)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct)) %>%
  dplyr::select(-nh_place) %>%
  left_join(nh_second_choice_division, by = c("candidate", "sim_id")) %>%
  melt(id.vars = c("sim_id", "candidate", "state_pct", "overperformance", "bounce_factor", "stay_in"), 
       variable.name = "second_choice", value.name = "pct") %>%
  mutate(pct = pct*state_pct*(!stay_in),
         state_pct = case_when(candidate == second_choice ~ state_pct,
                               candidate != second_choice ~ pct)) %>%
  arrange(sim_id, second_choice, candidate)

overperformance_stats <- nv_state_percentages_adj %>%
  group_by(sim_id, candidate) %>%
  summarise(overperformance = mean(overperformance),
            bounce_factor = mean(bounce_factor))

nv_state_percentages_adj <- nv_state_percentages_adj %>%
  group_by(sim_id, second_choice) %>%
  summarise(state_pct = sum(state_pct)) %>%
  dplyr::select(everything(), candidate = second_choice) %>%
  left_join(nh_stayin_matrix %>% melt(id.vars = NULL, value.name = "stay_in") %>%
              dplyr::select(sim_id = Var1, candidate = Var2, stay_in), by = c("sim_id", "candidate")) %>%
  mutate(state_pct = state_pct * stay_in) %>%
  left_join(overperformance_stats, by = c("sim_id", "candidate"))

nv_sims <- vector("list", n_sims)

### Simulate
nv_sims <- foreach(i = 1:n_sims, .packages = c("Hmisc", "dplyr", "reshape2", "tidyr")) %dopar% {
  demographic_lean_sim <- demographic_leans %>%
    mutate(lean = rnorm(n(), mean = avg_lean, sd = lean_se)) %>%
    dplyr::select(candidate, demographic_var, demographic, lean)
  
  # This simulation's national percentages
  this_sim_state_percentage <- nv_state_percentages_adj %>%
    filter(sim_id == i)
  
  # Apply simulated leans to state average
  state_demographic_sim <- demoCandidateBreakdown("Nevada", this_sim_state_percentage, demographic_lean_sim, state_marginal_demographics)
  
  # Turn these into probabilities
  gender_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("men", "women")) %>%
    dplyr::select(candidate, gender = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(gender = case_when(gender == "men" ~ "Male",
                              gender == "women" ~ "Female"))
  
  names(gender_breakdown)[-1] <- paste0("gender_", names(gender_breakdown))[-1]
  
  race_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("white", "black", "latino")) %>%
    dplyr::select(candidate, race = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(race = case_when(race == "white" ~ "White",
                            race == "black" ~ "Black",
                            race == "latino" ~ "Latino"))
  
  names(race_breakdown)[-1] <- paste0("race_", names(race_breakdown))[-1]
  
  age_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("age_1829", "age_3044", "age_4564", "age_65")) %>%
    dplyr::select(candidate, age = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(age = case_when(age == "age_1829" ~ "18-29",
                           age == "age_3044" ~ "30-44",
                           age == "age_4564" ~ "45-64",
                           age == "age_65" ~ "65+"))
  
  names(age_breakdown)[-1] <- paste0("age_", names(age_breakdown)[-1])
  
  education_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("college", "no_college")) %>%
    dplyr::select(candidate, education = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(education = case_when(education == "college" ~ "College",
                                 education == "no_college" ~ "Less than college"))
  
  names(education_breakdown)[-1] <- paste0("education_", names(education_breakdown)[-1])
  
  synth_pop <- sim_demographics %>%
    mutate(rand = runif(n()),
           age_regroup = case_when(age == "18-24" ~ "18-29",
                                   age == "25-34" & rand < 0.5 ~ "18-29",
                                   age == "25-34" & rand >= 0.5 ~ "30-44",
                                   age == "35-44" ~ "30-44",
                                   age == "45-64" ~ "45-64",
                                   age == "65+" ~ "65+"),
           id = 1:n()) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age = age_regroup, education) %>%
    mutate_if(is.factor, as.character) %>%
    left_join(gender_breakdown, by = "gender") %>%
    left_join(race_breakdown, by = "race") %>%
    left_join(age_breakdown, by = "age") %>%
    left_join(education_breakdown, by = "education") %>%
    mutate(biden_prob = gender_biden + race_biden + age_biden + education_biden,
           buttigieg_prob = gender_buttigieg + race_buttigieg + age_buttigieg + education_buttigieg,
           klobuchar_prob = gender_klobuchar + race_klobuchar + age_klobuchar + education_klobuchar,
           sanders_prob = gender_sanders + race_sanders + age_sanders + education_sanders,
           steyer_prob = gender_steyer + race_steyer + age_steyer + education_steyer,
           warren_prob = gender_warren + race_warren + age_warren + education_warren,
           yang_prob = 0) %>%
    mutate_at(vars(c("biden_prob", "buttigieg_prob", "klobuchar_prob", "sanders_prob", "steyer_prob", "warren_prob", "yang_prob")),
              function(x) pmax(x/100, 0)) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age, education, biden_prob, buttigieg_prob,
                  klobuchar_prob, sanders_prob, steyer_prob, warren_prob, yang_prob) %>%
    melt(id.vars = c("id", "state", "district", "primary_type", "district_delegates", "gender", "race", "age", "education"),
         variable.name = "candidate_prob", value.name = "prob") %>%
    arrange(id, candidate_prob) %>%
    group_by(id) %>%
    mutate(prob = prob / sum(prob),
           cum_prob = cumsum(prob)) %>%
    dplyr::select(-prob) %>%
    ungroup() %>%
    spread(candidate_prob, cum_prob) %>%
    mutate(rand = runif(n()),
           vote = case_when(rand <= biden_prob ~ "biden",
                            rand > biden_prob & rand <= buttigieg_prob ~ "buttigieg",
                            rand > buttigieg_prob & rand <= klobuchar_prob ~ "klobuchar",
                            rand > klobuchar_prob & rand <= sanders_prob ~ "sanders",
                            rand > sanders_prob & rand <= steyer_prob ~ "steyer",
                            rand > steyer_prob & rand <= warren_prob ~ "warren",
                            rand > warren_prob ~ "yang")) %>%
    as.tbl()
  
  synth_district_pop <- synth_pop %>%
    group_by(state, district) %>%
    summarise(district_pop = n())
  
  second_choice_sim <- second_choice %>%
    melt(id.vars = "first_choice", variable.name = "second_choice", value.name = "pct") %>%
    mutate(alpha = 50 * pct / (1 - pct),
           second_pct_sim = rbeta(n(), shape1 = alpha, shape2 = 30)) %>%
    dplyr::select(first_choice, second_choice, second_pct_sim)
  
  district_sim_pct <- synth_pop %>%
    group_by(state, district) %>%
    mutate(district_total = n()) %>%
    group_by(state, district, vote) %>%
    summarise(pct = n() / mean(district_total)) %>%
    ungroup() %>%
    mutate(met_threshold = pct >= 0.15)
  
  district_sim_delegates <- district_sim_pct %>%
    merge(second_choice_sim, by.x = "vote", by.y = "first_choice", all.x = TRUE) %>%
    merge(district_sim_pct, by.x = c("state", "district", "second_choice"), by.y = c("state", "district", "vote")) %>%
    mutate(pct_from_second_choice = pct.y * second_pct_sim * (second_choice != vote) * (met_threshold.x) * (!met_threshold.y)) %>%
    group_by(state, district, vote, pct.x, met_threshold = met_threshold.x) %>%
    summarise(pct_from_second_choice = sum(pct_from_second_choice)) %>%
    mutate(pct_valid = (pct.x + pct_from_second_choice)*met_threshold) %>% 
    group_by(state, district) %>%
    mutate(pct_valid = pct_valid / sum(pct_valid)) %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(candidate_delegates = delegates * pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, district, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate = vote, pct = pct.x, pct_valid, candidate_delegates) %>%
    arrange(district, candidate)
  
  state_sim_delegates <- district_sim_delegates %>%
    left_join(synth_district_pop, by = c("state", "district")) %>%
    mutate(synth_votes = pct * district_pop) %>%
    group_by(state) %>%
    mutate(state_vote = sum(synth_votes)) %>%
    group_by(state, candidate) %>%
    summarise(state_pct = sum(synth_votes) / mean(state_vote)) %>%
    mutate(district = "At-large") %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(state_pct = state_pct / sum(state_pct),
           met_threshold = state_pct >= 0.15,
           state_pct_valid = (state_pct*met_threshold) / sum(state_pct*met_threshold),
           candidate_delegates = delegates * state_pct,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate, pct = state_pct, pct_valid = state_pct_valid, candidate_delegates) %>%
    arrange(district, candidate) %>%
    bind_rows(district_sim_delegates) %>%
    mutate(sim_id = i)
}

### Handle dropouts
biden_nv_place <- primaryPlace("biden", nv_sims)
sanders_nv_place <- primaryPlace("sanders", nv_sims) 
warren_nv_place <- primaryPlace("warren", nv_sims)
steyer_nv_place <- primaryPlace("steyer", nv_sims)
buttigieg_nv_place <- primaryPlace("buttigieg", nv_sims)
klobuchar_nv_place <- primaryPlace("klobuchar", nv_sims)

nv_places <- c(biden_nv_place, buttigieg_nv_place, klobuchar_nv_place, sanders_nv_place, steyer_nv_place, warren_nv_place, rep(3.5, n_sims))

biden_dropout_prob <- (biden_nv_place^5) / (7^5) - (1/7)^5
buttigieg_dropout_prob <- (buttigieg_nv_place^2) / (7^2) - (1/7)^2
klobuchar_dropout_prob <- (klobuchar_nv_place^2) / (7^2) - (1/7)^2
sanders_dropout_prob <- (sanders_nv_place^4) / (7^4) - (1/7)^4
steyer_dropout_prob <- (steyer_nv_place^4) / (7^4) - (1/7)^4
warren_dropout_prob <- (warren_nv_place^4) / (7^4) - (1/7)^4
yang_dropout_prob <- 1

biden_dropout <- rbernoulli(n_sims, biden_dropout_prob) * !(biden_nh_place %in% 1:3)
buttigieg_dropout <- rbernoulli(n_sims, buttigieg_dropout_prob) * !(buttigieg_nh_place %in% 1:2)
klobuchar_dropout <- rbernoulli(n_sims, klobuchar_dropout_prob) * !(klobuchar_nh_place %in% 1:2)
sanders_dropout <- rbernoulli(n_sims, sanders_dropout_prob) * !(sanders_nh_place %in% 1:3)
steyer_dropout <- rbernoulli(n_sims, steyer_dropout_prob) * !(steyer_nh_place %in% 1:3)
warren_dropout <- rbernoulli(n_sims, warren_dropout_prob) * !(warren_nh_place %in% 1:2)
yang_dropout <- rbernoulli(n_sims, yang_dropout_prob) * !(yang_nh_place %in% 1:2)

nv_stayin_matrix <- !cbind("biden" = biden_dropout, 
                           "buttigieg" = buttigieg_dropout,
                           "klobuchar" = klobuchar_dropout,
                           "sanders" = sanders_dropout,
                           "steyer" = steyer_dropout,
                           "warren" = warren_dropout,
                           "yang" = yang_dropout) & nh_stayin_matrix

nv_second_choice_division <- nv_stayin_matrix %>%
  as.data.frame() %>%
  melt(id.vars = NULL, variable.name = "candidate", value.name = "stay_in") %>%
  mutate(candidate = as.character(candidate),
         sim_id = rep(1:n_sims, 7)) %>%
  as.tbl() %>%
  left_join(second_choice %>% dplyr::select(-bloomberg), by = c("candidate" = "first_choice"))

### How much did the candidates overperform in Nevada?
nv_overperformance <- nv_state_percentages_adj %>%
  left_join(state_averages %>% filter(state == "Nevada") %>% dplyr::select(candidate, state_avg), by = c("candidate")) %>%
  mutate(overperformance = state_pct - (state_avg/100 + 0.2*overperformance*bounce_factor)) %>%
  dplyr::select(sim_id, candidate, overperformance)

# South Carolina (29 Feb 2020) ####
sc_start_time <- Sys.time()
# Draw votes
sim_demographics_table <- district_demographics %>%
  filter(state == "South Carolina") %>%
  mutate(prob = predict(dem_primary_logit, newdata = ., type = "response"),
         n_vote = round(pop * prob * turnout * 0.05)) %>%
  dplyr::select(-pop, -prob)

sim_demographics <- sim_demographics_table %>%
  lapply(rep, sim_demographics_table$n_vote) %>%
  as.data.frame() 

state_marginal_demographics <- shapeStateDemo("South Carolina")

### Apply Nevada overperformance to South Carolina polls
sc_averages_over_time <- state_averages_over_time_unsmoothed %>%
  filter(state == "South Carolina", as.numeric(today() - median_date) <= 1) %>%
  mutate(age = as.numeric(today() - median_date),
         nreps = 2 - age)

sc_dirichlet_params <- (sc_averages_over_time %>%
                          lapply(rep, sc_averages_over_time$nreps) %>%
                          as.data.frame() %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          dplyr::select(-median_date) %>%
                          group_by(candidate) %>%
                          mutate(i = 1:n()) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-i) %>%
                          as.matrix() %>%
                          diri.est())$param

sc_dirichlet_params[7] <- 0

sc_state_percentages <- (rdirichlet(n_sims, sc_dirichlet_params) %>%
                            as.data.frame() %>%
                            dplyr::select(biden = V1, buttigieg = V2, klobuchar = V3, sanders = V4, 
                                          steyer = V5, warren = V6, yang = V7)) %>%
  apply(MARGIN = 1, FUN = function(x) x / sum(x)) %>%
  t() %>%
  as.data.frame() %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

sc_state_percentages_adj <- sc_state_percentages %>%
  left_join(nv_overperformance, by = c("sim_id", "candidate")) %>%
  mutate(bounce_factor = 1.2 * rbeta(n(), shape1 = 9, shape2 = 2),
         nh_place = nh_places,
         nv_place = nv_places,
         state_pct = pmax(state_pct + overperformance * bounce_factor + 0.02 * (3.5 - nv_place) + 0.01 * (3.5 - nh_place), 0)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct)) %>%
  dplyr::select(-nh_place, -nv_place) %>%
  left_join(nv_second_choice_division, by = c("candidate", "sim_id")) %>%
  melt(id.vars = c("sim_id", "candidate", "state_pct", "overperformance", "bounce_factor", "stay_in"), 
       variable.name = "second_choice", value.name = "pct") %>%
  mutate(pct = pct*state_pct*(!stay_in),
         state_pct = case_when(candidate == second_choice ~ state_pct,
                               candidate != second_choice ~ pct)) %>%
  arrange(sim_id, second_choice, candidate)

overperformance_stats <- sc_state_percentages_adj %>%
  group_by(sim_id, candidate) %>%
  summarise(overperformance = mean(overperformance),
            bounce_factor = mean(bounce_factor))

sc_state_percentages_adj <- sc_state_percentages_adj %>%
  group_by(sim_id, second_choice) %>%
  summarise(state_pct = sum(state_pct)) %>%
  dplyr::select(everything(), candidate = second_choice) %>%
  left_join(nv_stayin_matrix %>% melt(id.vars = NULL, value.name = "stay_in") %>%
              dplyr::select(sim_id = Var1, candidate = Var2, stay_in), by = c("sim_id", "candidate")) %>%
  mutate(state_pct = state_pct * stay_in) %>%
  left_join(overperformance_stats, by = c("sim_id", "candidate"))

sc_sims <- vector("list", n_sims)

sc_sims <- foreach(i = 1:n_sims, .packages = c("Hmisc", "dplyr", "reshape2", "tidyr")) %dopar% {
  demographic_lean_sim <- demographic_leans %>%
    mutate(lean = rnorm(n(), mean = avg_lean, sd = lean_se)) %>%
    dplyr::select(candidate, demographic_var, demographic, lean)
  
  # This simulation's national percentages
  this_sim_state_percentage <- sc_state_percentages_adj %>%
    filter(sim_id == i)
  
  # Apply simulated leans to state average
  state_demographic_sim <- demoCandidateBreakdown("South Carolina", this_sim_state_percentage, demographic_lean_sim, 
                                                  state_marginal_demographics)
  
  # Turn these into probabilities
  gender_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("men", "women")) %>%
    dplyr::select(candidate, gender = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(gender = case_when(gender == "men" ~ "Male",
                              gender == "women" ~ "Female"))
  
  names(gender_breakdown)[-1] <- paste0("gender_", names(gender_breakdown))[-1]
  
  race_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("white", "black", "latino")) %>%
    dplyr::select(candidate, race = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(race = case_when(race == "white" ~ "White",
                            race == "black" ~ "Black",
                            race == "latino" ~ "Latino"))
  
  names(race_breakdown)[-1] <- paste0("race_", names(race_breakdown))[-1]
  
  age_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("age_1829", "age_3044", "age_4564", "age_65")) %>%
    dplyr::select(candidate, age = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(age = case_when(age == "age_1829" ~ "18-29",
                           age == "age_3044" ~ "30-44",
                           age == "age_4564" ~ "45-64",
                           age == "age_65" ~ "65+"))
  
  names(age_breakdown)[-1] <- paste0("age_", names(age_breakdown)[-1])
  
  education_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("college", "no_college")) %>%
    dplyr::select(candidate, education = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(education = case_when(education == "college" ~ "College",
                                 education == "no_college" ~ "Less than college"))
  
  names(education_breakdown)[-1] <- paste0("education_", names(education_breakdown)[-1])
  
  synth_pop <- sim_demographics %>%
    mutate(rand = runif(n()),
           age_regroup = case_when(age == "18-24" ~ "18-29",
                                   age == "25-34" & rand < 0.5 ~ "18-29",
                                   age == "25-34" & rand >= 0.5 ~ "30-44",
                                   age == "35-44" ~ "30-44",
                                   age == "45-64" ~ "45-64",
                                   age == "65+" ~ "65+"),
           id = 1:n()) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age = age_regroup, education) %>%
    mutate_if(is.factor, as.character) %>%
    left_join(gender_breakdown, by = "gender") %>%
    left_join(race_breakdown, by = "race") %>%
    left_join(age_breakdown, by = "age") %>%
    left_join(education_breakdown, by = "education") %>%
    mutate(biden_prob = gender_biden + race_biden + age_biden + education_biden,
           buttigieg_prob = gender_buttigieg + race_buttigieg + age_buttigieg + education_buttigieg,
           klobuchar_prob = gender_klobuchar + race_klobuchar + age_klobuchar + education_klobuchar,
           sanders_prob = gender_sanders + race_sanders + age_sanders + education_sanders,
           steyer_prob = gender_steyer + race_steyer + age_steyer + education_steyer,
           warren_prob = gender_warren + race_warren + age_warren + education_warren,
           yang_prob = gender_yang + race_yang + age_yang + education_yang) %>%
    mutate_at(vars(c("biden_prob", "buttigieg_prob", "klobuchar_prob", "sanders_prob", "steyer_prob", "warren_prob", "yang_prob")),
              function(x) pmax(x/100, 0)) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age, education, biden_prob, buttigieg_prob,
                  klobuchar_prob, sanders_prob, steyer_prob, warren_prob, yang_prob) %>%
    melt(id.vars = c("id", "state", "district", "primary_type", "district_delegates", "gender", "race", "age", "education"),
         variable.name = "candidate_prob", value.name = "prob") %>%
    arrange(id, candidate_prob) %>%
    group_by(id) %>%
    mutate(prob = prob / sum(prob),
           cum_prob = cumsum(prob)) %>%
    dplyr::select(-prob) %>%
    ungroup() %>%
    spread(candidate_prob, cum_prob) %>%
    mutate(rand = runif(n()),
           vote = case_when(rand <= biden_prob ~ "biden",
                            rand > biden_prob & rand <= buttigieg_prob ~ "buttigieg",
                            rand > buttigieg_prob & rand <= klobuchar_prob ~ "klobuchar",
                            rand > klobuchar_prob & rand <= sanders_prob ~ "sanders",
                            rand > sanders_prob & rand <= steyer_prob ~ "steyer",
                            rand > steyer_prob & rand <= warren_prob ~ "warren",
                            rand > warren_prob ~ "yang")) %>%
    as.tbl()
  
  synth_district_pop <- synth_pop %>%
    group_by(state, district) %>%
    summarise(district_pop = n())
  
  district_sim_delegates <- synth_pop %>%
    group_by(state, district) %>%
    mutate(district_total = n()) %>%
    group_by(state, district, vote) %>%
    summarise(pct = n() / mean(district_total)) %>%
    ungroup() %>%
    mutate(met_threshold = pct >= 0.15) %>% 
    group_by(state, district) %>%
    mutate(pct_valid = (pct*met_threshold) / sum(pct*met_threshold)) %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(candidate_delegates = delegates * pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, district, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate = vote, pct, pct_valid, candidate_delegates) %>%
    arrange(district, candidate)
  
  state_sim_delegates <- district_sim_delegates %>%
    left_join(synth_district_pop, by = c("state", "district")) %>%
    mutate(synth_votes = pct * district_pop) %>%
    group_by(state) %>%
    mutate(state_vote = sum(synth_votes)) %>%
    group_by(state, candidate) %>%
    summarise(state_pct = sum(synth_votes) / mean(state_vote)) %>%
    mutate(district = "At-large") %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(met_threshold = state_pct >= 0.15,
           state_pct_valid = (state_pct*met_threshold) / sum(state_pct*met_threshold),
           candidate_delegates = delegates * state_pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate, pct = state_pct, pct_valid = state_pct_valid, candidate_delegates) %>%
    arrange(district, candidate) %>%
    bind_rows(district_sim_delegates) %>%
    mutate(sim_id = i)
}

# How much did the candidates overperform in South Carolina?
sc_overperformance <- sc_state_percentages_adj %>%
  left_join(state_averages %>% filter(state == "South Carolina") %>% dplyr::select(candidate, state_avg), by = c("candidate")) %>%
  mutate(overperformance = state_pct - (state_avg/100 + 0.25*overperformance*bounce_factor)) %>%
  dplyr::select(sim_id, candidate, overperformance) 

### Handle dropouts
biden_sc_place <- primaryPlace("biden", sc_sims)
sanders_sc_place <- primaryPlace("sanders", sc_sims) 
warren_sc_place <- primaryPlace("warren", sc_sims)
steyer_sc_place <- primaryPlace("steyer", sc_sims)
buttigieg_sc_place <- primaryPlace("buttigieg", sc_sims)
klobuchar_sc_place <- primaryPlace("klobuchar", sc_sims)
yang_sc_place <- primaryPlace("yang", sc_sims)

sc_places <- c(biden_sc_place, buttigieg_sc_place, klobuchar_sc_place, sanders_sc_place, steyer_sc_place, warren_sc_place,
               yang_sc_place)

biden_dropout_prob <- (biden_sc_place^3) / (7^3) - (1/7)^3
buttigieg_dropout_prob <- (buttigieg_sc_place^2) / (7^2) - (1/7)^2
klobuchar_dropout_prob <- (klobuchar_sc_place^2) / (7^2) - (1/7)^2
sanders_dropout_prob <- (sanders_sc_place^4) / (7^4) - (1/7)^4
steyer_dropout_prob <- (steyer_sc_place^2) / (7^2) - (1/7)^2
warren_dropout_prob <- (warren_sc_place^3) / (7^3) - (1/7)^3
yang_dropout_prob <- (yang_sc_place^2) / (7^2) - (1/7)^2

biden_dropout <- rbernoulli(n_sims, biden_dropout_prob) * !(biden_nv_place %in% 1)
buttigieg_dropout <- rbernoulli(n_sims, buttigieg_dropout_prob) * !(buttigieg_nv_place %in% 1:4)
klobuchar_dropout <- rbernoulli(n_sims, klobuchar_dropout_prob) * !(klobuchar_nv_place %in% 1:4)
sanders_dropout <- rbernoulli(n_sims, sanders_dropout_prob) * !(sanders_nv_place %in% 1:3)
steyer_dropout <- rbernoulli(n_sims, steyer_dropout_prob) * !(steyer_nv_place %in% 1:3)
warren_dropout <- rbernoulli(n_sims, warren_dropout_prob) * !(warren_nv_place %in% 1:3)
yang_dropout <- rbernoulli(n_sims, yang_dropout_prob) * !(yang_nv_place %in% 1:2)

sc_stayin_matrix <- !cbind("biden" = biden_dropout, 
                           "buttigieg" = buttigieg_dropout,
                           "klobuchar" = klobuchar_dropout,
                           "sanders" = sanders_dropout,
                           "steyer" = steyer_dropout,
                           "warren" = warren_dropout,
                           "yang" = yang_dropout) & nv_stayin_matrix

sc_stayin_matrix <- cbind("biden" = sc_stayin_matrix[, 1],
                          "bloomberg" = TRUE,
                          sc_stayin_matrix[, 2:7])

sc_second_choice_division <- sc_stayin_matrix %>%
  as.data.frame() %>%
  melt(id.vars = NULL, variable.name = "candidate", value.name = "stay_in") %>%
  mutate(candidate = as.character(candidate),
         sim_id = rep(1:n_sims, 8)) %>%
  as.tbl() %>%
  left_join(second_choice, by = c("candidate" = "first_choice"))


# SUPER TUESDAY (3 Mar 2020) ####
## Applying the overperformance bounce to every state
bloomberg_overperformance_bounce <- sc_overperformance %>%
  group_by(sim_id) %>%
  summarise(overperformance = -10*mean(overperformance)) %>%
  mutate(candidate = "bloomberg")

bounce_factor <- tibble(sim_id = 1:n_sims, bounce_factor = rbeta(n_sims, shape1 = 7, shape2 = 3))

overperformance_bounce <- sc_overperformance %>%
  bind_rows(bloomberg_overperformance_bounce) %>%
  left_join(bounce_factor, by = "sim_id") %>%
  mutate(bounce = overperformance * bounce_factor) 

nh_places <- c(biden_nh_place, rep(3.5, n_sims), buttigieg_nh_place, klobuchar_nh_place, sanders_nh_place, steyer_nh_place, 
               warren_nh_place, yang_nh_place)
nv_places <- c(biden_nv_place, rep(3.5, n_sims), buttigieg_nv_place, klobuchar_nv_place, sanders_nv_place, steyer_nv_place, 
               warren_nv_place, rep(7, n_sims))
sc_places <- c(biden_sc_place, rep(3.5, n_sims), buttigieg_sc_place, klobuchar_sc_place, sanders_sc_place, steyer_sc_place, 
               warren_sc_place, rep(7, n_sims))

# Alabama ####
al_start_time <- Sys.time()
# Draw votes
sim_demographics_table <- district_demographics %>%
  filter(state == "Alabama") %>%
  mutate(prob = predict(dem_primary_logit, newdata = ., type = "response"),
         n_vote = round(pop * prob * turnout * 0.05)) %>%
  dplyr::select(-pop, -prob)

sim_demographics <- sim_demographics_table %>%
  lapply(rep, sim_demographics_table$n_vote) %>%
  as.data.frame() 
state_marginal_demographics <- shapeStateDemo("Alabama")

# Use national polling for Bloomberg and Steyer
natl_bloomberg_steyer <- tibble(state = "Alabama",
                                candidate = rep(c("bloomberg", "steyer"), each = 2),
                                median_date = rep(c(today() - 1, today()), 2),
                                age = rep(1:0, 2)) %>%
  mutate(nreps = 2 - age) %>%
  left_join(national_averages_adjusted %>% 
              dplyr::select(candidate, median_date, pct) %>%
              dplyr::mutate(pct = pct / 100), by = c("candidate", "median_date")) 

al_averages_over_time <- state_averages_over_time_unsmoothed %>%
  filter(state == "Alabama", as.numeric(today() - median_date) <= 1) %>%
  mutate(age = as.numeric(today() - median_date),
         nreps = 2 - age)

al_dirichlet_params <- (al_averages_over_time %>%
                          lapply(rep, al_averages_over_time$nreps) %>%
                          as.data.frame() %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          dplyr::select(-median_date) %>%
                          group_by(candidate) %>%
                          mutate(i = 1:n()) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-i) %>%
                          as.matrix() %>%
                          diri.est())$param

al_dirichlet_params[8] <- 0

al_dirichlet_params <- al_dirichlet_params / 2

al_state_percentages <- (rdirichlet(n_sims, al_dirichlet_params) %>%
                            as.data.frame() %>%
                            dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, 
                                          steyer = V6, warren = V7, yang = V8)) %>%
  apply(MARGIN = 1, FUN = function(x) x / sum(x)) %>%
  t() %>%
  as.data.frame() %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

al_state_percentages_adj <- al_state_percentages %>%
  left_join(overperformance_bounce, by = c("sim_id", "candidate")) %>%
  mutate(nv_place = nv_places,
         sc_place = sc_places,
         state_pct = pmax(state_pct + bounce + 0.04 * (3.5 - sc_place) + 0.01 * (3.5 - nv_place), 0)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct)) %>%
  dplyr::select(-nv_place, -sc_place) %>%
  left_join(sc_second_choice_division, by = c("candidate", "sim_id")) %>%
  melt(id.vars = c("sim_id", "candidate", "state_pct", "overperformance", "bounce_factor", "stay_in", "bounce"), 
       variable.name = "second_choice", value.name = "pct") %>%
  mutate(pct = pct*state_pct*(!stay_in),
         state_pct = case_when(candidate == second_choice ~ state_pct,
                               candidate != second_choice ~ pct)) %>%
  arrange(sim_id, second_choice, candidate)

overperformance_stats <- al_state_percentages_adj %>%
  group_by(sim_id, candidate) %>%
  summarise(overperformance = mean(overperformance),
            bounce_factor = mean(bounce_factor))

al_state_percentages_adj <- al_state_percentages_adj %>%
  group_by(sim_id, second_choice) %>%
  summarise(state_pct = sum(state_pct)) %>%
  dplyr::select(everything(), candidate = second_choice) %>%
  left_join(sc_stayin_matrix %>% melt(id.vars = NULL, value.name = "stay_in") %>%
              dplyr::select(sim_id = Var1, candidate = Var2, stay_in), by = c("sim_id", "candidate")) %>%
  mutate(state_pct = state_pct * stay_in) %>%
  left_join(overperformance_stats, by = c("sim_id", "candidate"))

al_sims <- vector("list", n_sims)

al_sims <- foreach(i = 1:n_sims, .packages = c("Hmisc", "dplyr", "reshape2", "tidyr")) %dopar% {
  demographic_lean_sim <- demographic_leans %>%
    mutate(lean = rnorm(n(), mean = avg_lean, sd = lean_se)) %>%
    dplyr::select(candidate, demographic_var, demographic, lean)
  
  # This simulation's national percentages
  this_sim_state_percentage <- al_state_percentages_adj %>%
    filter(sim_id == i)
  
  # Apply simulated leans to state average
  state_demographic_sim <- demoCandidateBreakdown("Alabama", this_sim_state_percentage, demographic_lean_sim, 
                                                  state_marginal_demographics)
  
  # Turn these into probabilities
  gender_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("men", "women")) %>%
    dplyr::select(candidate, gender = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(gender = case_when(gender == "men" ~ "Male",
                              gender == "women" ~ "Female"))
  
  names(gender_breakdown)[-1] <- paste0("gender_", names(gender_breakdown))[-1]
  
  race_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("white", "black", "latino")) %>%
    dplyr::select(candidate, race = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(race = case_when(race == "white" ~ "White",
                            race == "black" ~ "Black",
                            race == "latino" ~ "Latino"))
  
  names(race_breakdown)[-1] <- paste0("race_", names(race_breakdown))[-1]
  
  age_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("age_1829", "age_3044", "age_4564", "age_65")) %>%
    dplyr::select(candidate, age = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(age = case_when(age == "age_1829" ~ "18-29",
                           age == "age_3044" ~ "30-44",
                           age == "age_4564" ~ "45-64",
                           age == "age_65" ~ "65+"))
  
  names(age_breakdown)[-1] <- paste0("age_", names(age_breakdown)[-1])
  
  education_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("college", "no_college")) %>%
    dplyr::select(candidate, education = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(education = case_when(education == "college" ~ "College",
                                 education == "no_college" ~ "Less than college"))
  
  names(education_breakdown)[-1] <- paste0("education_", names(education_breakdown)[-1])
  
  synth_pop <- sim_demographics %>%
    mutate(rand = runif(n()),
           age_regroup = case_when(age == "18-24" ~ "18-29",
                                   age == "25-34" & rand < 0.5 ~ "18-29",
                                   age == "25-34" & rand >= 0.5 ~ "30-44",
                                   age == "35-44" ~ "30-44",
                                   age == "45-64" ~ "45-64",
                                   age == "65+" ~ "65+"),
           id = 1:n()) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age = age_regroup, education) %>%
    mutate_if(is.factor, as.character) %>%
    left_join(gender_breakdown, by = "gender") %>%
    left_join(race_breakdown, by = "race") %>%
    left_join(age_breakdown, by = "age") %>%
    left_join(education_breakdown, by = "education") %>%
    mutate(biden_prob = gender_biden + race_biden + age_biden + education_biden,
           bloomberg_prob = gender_bloomberg + race_bloomberg + age_bloomberg + education_bloomberg,
           buttigieg_prob = gender_buttigieg + race_buttigieg + age_buttigieg + education_buttigieg,
           klobuchar_prob = gender_klobuchar + race_klobuchar + age_klobuchar + education_klobuchar,
           sanders_prob = gender_sanders + race_sanders + age_sanders + education_sanders,
           steyer_prob = gender_steyer + race_steyer + age_steyer + education_steyer,
           warren_prob = gender_warren + race_warren + age_warren + education_warren,
           yang_prob = gender_yang + race_yang + age_yang + education_yang) %>%
    mutate_at(vars(c("biden_prob", "bloomberg_prob", "buttigieg_prob", "klobuchar_prob", "sanders_prob", "steyer_prob", "warren_prob", 
                     "yang_prob")), function(x) pmax(x/100, 0)) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age, education, biden_prob, 
                  bloomberg_prob, buttigieg_prob, klobuchar_prob, sanders_prob, steyer_prob, warren_prob, yang_prob) %>%
    melt(id.vars = c("id", "state", "district", "primary_type", "district_delegates", "gender", "race", "age", "education"),
         variable.name = "candidate_prob", value.name = "prob") %>%
    arrange(id, candidate_prob) %>%
    group_by(id) %>%
    mutate(prob = prob / sum(prob),
           cum_prob = cumsum(prob)) %>%
    dplyr::select(-prob) %>%
    ungroup() %>%
    spread(candidate_prob, cum_prob) %>%
    mutate(rand = runif(n()),
           vote = case_when(rand <= biden_prob ~ "biden",
                            rand > biden_prob & rand <= bloomberg_prob ~ "bloomberg",
                            rand > bloomberg_prob & rand <= buttigieg_prob ~ "buttigieg",
                            rand > buttigieg_prob & rand <= klobuchar_prob ~ "klobuchar",
                            rand > klobuchar_prob & rand <= sanders_prob ~ "sanders",
                            rand > sanders_prob & rand <= steyer_prob ~ "steyer",
                            rand > steyer_prob & rand <= warren_prob ~ "warren",
                            rand > warren_prob ~ "yang")) %>%
    as.tbl()
  
  
  synth_district_pop <- synth_pop %>%
    group_by(state, district) %>%
    summarise(district_pop = n())
  
  district_sim_delegates <- synth_pop %>%
    group_by(state, district) %>%
    mutate(district_total = n()) %>%
    group_by(state, district, vote) %>%
    summarise(pct = n() / mean(district_total)) %>%
    ungroup() %>%
    mutate(met_threshold = pct >= 0.15) %>% 
    group_by(state, district) %>%
    mutate(pct_valid = (pct*met_threshold) / sum(pct*met_threshold)) %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(candidate_delegates = delegates * pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, district, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate = vote, pct, pct_valid, candidate_delegates) %>%
    arrange(district, candidate)
  
  state_sim_delegates <- district_sim_delegates %>%
    left_join(synth_district_pop, by = c("state", "district")) %>%
    mutate(synth_votes = pct * district_pop) %>%
    group_by(state) %>%
    mutate(state_vote = sum(synth_votes)) %>%
    group_by(state, candidate) %>%
    summarise(state_pct = sum(synth_votes) / mean(state_vote)) %>%
    mutate(district = "At-large") %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(met_threshold = state_pct >= 0.15,
           state_pct_valid = (state_pct*met_threshold) / sum(state_pct*met_threshold),
           candidate_delegates = delegates * state_pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate, pct = state_pct, pct_valid = state_pct_valid, candidate_delegates) %>%
    arrange(district, candidate) %>%
    bind_rows(district_sim_delegates) %>%
    mutate(sim_id = i)
}

# How much did the candidates overperform in Alabama?
al_overperformance <- al_state_percentages_adj %>%
  left_join(state_averages %>% filter(state == "Alabama") %>% dplyr::select(candidate, state_avg), by = c("candidate")) %>%
  mutate(overperformance = state_pct - (state_avg/100 + 0.5*overperformance*bounce_factor)) %>%
  dplyr::select(sim_id, candidate, al_overperformance = overperformance) 

# American Samoa ####
as_start_time <- Sys.time()
# Shortcut since I know nothing about what the situation looks like on the ground: 
### Draw vote shares
as_averages_over_time <- national_averages_adjusted %>%
  filter(as.numeric(today() - median_date) <= 1) %>%
  mutate(age = as.numeric(today() - median_date),
         nreps = 2 - age)

as_dirichlet_params <- (as_averages_over_time %>%
                          lapply(rep, as_averages_over_time$nreps) %>%
                          as.data.frame() %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          mutate(pct = pct / 100) %>%
                          dplyr::select(-median_date) %>%
                          group_by(candidate) %>%
                          mutate(i = 1:n()) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-i) %>%
                          as.matrix() %>%
                          diri.est())$param

as_dirichlet_params <- 0.5 * as_dirichlet_params / max(1, min(as_dirichlet_params))

as_dirichlet_params[8] <- 0

as_state_percentages <- rdirichlet(n_sims, as_dirichlet_params) %>%
  as.data.frame() %>%
  dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, steyer = V6, warren = V7, yang = V8) %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

as_sims <- vector("list", n_sims)

### Simulate
as_sims <- foreach(i = 1:n_sims, .packages = c("Hmisc", "dplyr", "reshape2", "tidyr")) %dopar% {
  this_sim_state_percentage <- as_state_percentages %>%
    filter(sim_id == i)
  
  second_choice_sim <- second_choice %>%
    melt(id.vars = "first_choice", variable.name = "second_choice", value.name = "pct") %>%
    mutate(alpha = 50 * pct / (1 - pct),
           second_pct_sim = rbeta(n(), shape1 = alpha, shape2 = 30)) %>%
    dplyr::select(first_choice, second_choice, second_pct_sim)
  
  district_sim_pct <- this_sim_state_percentage %>%
    mutate(met_threshold = state_pct >= 0.15)
  
  state_sim_delegates <- district_sim_pct %>%
    merge(second_choice_sim, by.x = "candidate", by.y = "first_choice", all.x = TRUE) %>%
    merge(district_sim_pct, by.x = "second_choice", by.y = "candidate") %>%
    mutate(pct_from_second_choice = state_pct.y * second_pct_sim * (second_choice != candidate) * (met_threshold.x) * (!met_threshold.y)) %>%
    group_by(candidate, state_pct.x, met_threshold = met_threshold.x) %>%
    summarise(pct_from_second_choice = sum(pct_from_second_choice)) %>%
    ungroup() %>%
    mutate(pct_valid = (state_pct.x + pct_from_second_choice)*met_threshold) %>% 
    mutate(pct_valid = pct_valid / sum(pct_valid),
           state = "American Samoa",
           district = "At-large") %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(candidate_delegates = delegates * pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, district, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate, pct = state_pct.x, pct_valid, candidate_delegates) %>%
    arrange(district, candidate) %>%
    mutate(sim_id = i)
  
}

# Arkansas ####
ar_start_time <- Sys.time()
# Draw votes
sim_demographics_table <- district_demographics %>%
  filter(state == "Arkansas") %>%
  mutate(prob = predict(dem_primary_logit, newdata = ., type = "response"),
         n_vote = round(pop * prob * turnout * 0.05)) %>%
  dplyr::select(-pop, -prob)

sim_demographics <- sim_demographics_table %>%
  lapply(rep, sim_demographics_table$n_vote) %>%
  as.data.frame() 
state_marginal_demographics <- shapeStateDemo("Arkansas")

# Have no Arkansas polls; use nearest neighbors
ar_averages_over_time <- state_averages_over_time_unsmoothed %>%
  filter(state == "Arkansas", as.numeric(today() - median_date) <= 1) %>%
  mutate(age = as.numeric(today() - median_date),
         nreps = 2 - age)

ar_dirichlet_params <- ((ar_averages_over_time %>%
                          lapply(rep, ar_averages_over_time$nreps) %>%
                          as.data.frame() %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          dplyr::select(-median_date) %>%
                          group_by(candidate) %>%
                          mutate(i = 1:n()) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-i) %>%
                          as.matrix() %>%
                          diri.est())$param)

ar_dirichlet_params <- 0.5 * ar_dirichlet_params / max(1, min(ar_dirichlet_params))
ar_dirichlet_params[8] <- 0

ar_state_percentages <- (rdirichlet(n_sims, ar_dirichlet_params) %>%
                            as.data.frame() %>%
                            dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, 
                                          steyer = V6, warren = V7, yang = V8)) %>%
  apply(MARGIN = 1, FUN = function(x) x / sum(x)) %>%
  t() %>%
  as.data.frame() %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

ar_state_percentages_adj <- ar_state_percentages %>%
  left_join(overperformance_bounce, by = c("sim_id", "candidate")) %>%
  mutate(sc_place = sc_places,
         nv_place = nv_places,
         state_pct = pmax(state_pct + bounce + 0.02 * (3.5 - sc_place) + 0.01 * (3.5 - nv_place), 0)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct)) %>%
  dplyr::select(-sc_place, -nv_place) %>%
  left_join(sc_second_choice_division, by = c("candidate", "sim_id")) %>%
  melt(id.vars = c("sim_id", "candidate", "state_pct", "overperformance", "bounce_factor", "stay_in", "bounce"), 
       variable.name = "second_choice", value.name = "pct") %>%
  mutate(pct = pct*state_pct*(!stay_in),
         state_pct = case_when(candidate == second_choice ~ state_pct,
                               candidate != second_choice ~ pct)) %>%
  arrange(sim_id, second_choice, candidate)

overperformance_stats <- ar_state_percentages_adj %>%
  group_by(sim_id, candidate) %>%
  summarise(overperformance = mean(overperformance),
            bounce_factor = mean(bounce_factor))

ar_state_percentages_adj <- ar_state_percentages_adj %>%
  group_by(sim_id, second_choice) %>%
  summarise(state_pct = sum(state_pct)) %>%
  dplyr::select(everything(), candidate = second_choice) %>%
  left_join(sc_stayin_matrix %>% melt(id.vars = NULL, value.name = "stay_in") %>%
              dplyr::select(sim_id = Var1, candidate = Var2, stay_in), by = c("sim_id", "candidate")) %>%
  mutate(state_pct = state_pct * stay_in) %>%
  left_join(overperformance_stats, by = c("sim_id", "candidate"))

ar_sims <- vector("list", n_sims)

ar_sims <- foreach(i = 1:n_sims, .packages = c("Hmisc", "dplyr", "reshape2", "tidyr")) %dopar% {
  demographic_lean_sim <- demographic_leans %>%
    mutate(lean = rnorm(n(), mean = avg_lean, sd = lean_se)) %>%
    dplyr::select(candidate, demographic_var, demographic, lean)
  
  # This simulation's national percentages
  this_sim_state_percentage <- ar_state_percentages_adj %>%
    filter(sim_id == i)
  
  # Apply simulated leans to state average
  state_demographic_sim <- demoCandidateBreakdown("Tennessee", this_sim_state_percentage, demographic_lean_sim, 
                                                  state_marginal_demographics)
  
  # Turn these into probabilities
  gender_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("men", "women")) %>%
    dplyr::select(candidate, gender = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(gender = case_when(gender == "men" ~ "Male",
                              gender == "women" ~ "Female"))
  
  names(gender_breakdown)[-1] <- paste0("gender_", names(gender_breakdown))[-1]
  
  race_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("white", "black", "latino")) %>%
    dplyr::select(candidate, race = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(race = case_when(race == "white" ~ "White",
                            race == "black" ~ "Black",
                            race == "latino" ~ "Latino"))
  
  names(race_breakdown)[-1] <- paste0("race_", names(race_breakdown))[-1]
  
  age_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("age_1829", "age_3044", "age_4564", "age_65")) %>%
    dplyr::select(candidate, age = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(age = case_when(age == "age_1829" ~ "18-29",
                           age == "age_3044" ~ "30-44",
                           age == "age_4564" ~ "45-64",
                           age == "age_65" ~ "65+"))
  
  names(age_breakdown)[-1] <- paste0("age_", names(age_breakdown)[-1])
  
  education_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("college", "no_college")) %>%
    dplyr::select(candidate, education = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(education = case_when(education == "college" ~ "College",
                                 education == "no_college" ~ "Less than college"))
  
  names(education_breakdown)[-1] <- paste0("education_", names(education_breakdown)[-1])
  
  synth_pop <- sim_demographics %>%
    mutate(rand = runif(n()),
           age_regroup = case_when(age == "18-24" ~ "18-29",
                                   age == "25-34" & rand < 0.5 ~ "18-29",
                                   age == "25-34" & rand >= 0.5 ~ "30-44",
                                   age == "35-44" ~ "30-44",
                                   age == "45-64" ~ "45-64",
                                   age == "65+" ~ "65+"),
           id = 1:n()) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age = age_regroup, education) %>%
    mutate_if(is.factor, as.character) %>%
    left_join(gender_breakdown, by = "gender") %>%
    left_join(race_breakdown, by = "race") %>%
    left_join(age_breakdown, by = "age") %>%
    left_join(education_breakdown, by = "education") %>%
    mutate(biden_prob = gender_biden + race_biden + age_biden + education_biden,
           bloomberg_prob = gender_bloomberg + race_bloomberg + age_bloomberg + education_bloomberg,
           buttigieg_prob = gender_buttigieg + race_buttigieg + age_buttigieg + education_buttigieg,
           klobuchar_prob = gender_klobuchar + race_klobuchar + age_klobuchar + education_klobuchar,
           sanders_prob = gender_sanders + race_sanders + age_sanders + education_sanders,
           steyer_prob = gender_steyer + race_steyer + age_steyer + education_steyer,
           warren_prob = gender_warren + race_warren + age_warren + education_warren,
           yang_prob = gender_yang + race_yang + age_yang + education_yang) %>%
    mutate_at(vars(c("biden_prob", "bloomberg_prob", "buttigieg_prob", "klobuchar_prob", "sanders_prob", "steyer_prob", "warren_prob", 
                     "yang_prob")), function(x) pmax(x/100, 0)) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age, education, biden_prob, 
                  bloomberg_prob, buttigieg_prob, klobuchar_prob, sanders_prob, steyer_prob, warren_prob, yang_prob) %>%
    melt(id.vars = c("id", "state", "district", "primary_type", "district_delegates", "gender", "race", "age", "education"),
         variable.name = "candidate_prob", value.name = "prob") %>%
    arrange(id, candidate_prob) %>%
    group_by(id) %>%
    mutate(prob = prob / sum(prob),
           cum_prob = cumsum(prob)) %>%
    dplyr::select(-prob) %>%
    ungroup() %>%
    spread(candidate_prob, cum_prob) %>%
    mutate(rand = runif(n()),
           vote = case_when(rand <= biden_prob ~ "biden",
                            rand > biden_prob & rand <= bloomberg_prob ~ "bloomberg",
                            rand > bloomberg_prob & rand <= buttigieg_prob ~ "buttigieg",
                            rand > buttigieg_prob & rand <= klobuchar_prob ~ "klobuchar",
                            rand > klobuchar_prob & rand <= sanders_prob ~ "sanders",
                            rand > sanders_prob & rand <= steyer_prob ~ "steyer",
                            rand > steyer_prob & rand <= warren_prob ~ "warren",
                            rand > warren_prob ~ "yang")) %>%
    as.tbl()
  
  
  synth_district_pop <- synth_pop %>%
    group_by(state, district) %>%
    summarise(district_pop = n())
  
  district_sim_delegates <- synth_pop %>%
    group_by(state, district) %>%
    mutate(district_total = n()) %>%
    group_by(state, district, vote) %>%
    summarise(pct = n() / mean(district_total)) %>%
    ungroup() %>%
    mutate(met_threshold = pct >= 0.15) %>% 
    group_by(state, district) %>%
    mutate(pct_valid = (pct*met_threshold) / sum(pct*met_threshold)) %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(candidate_delegates = delegates * pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, district, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate = vote, pct, pct_valid, candidate_delegates) %>%
    arrange(district, candidate)
  
  state_sim_delegates <- district_sim_delegates %>%
    left_join(synth_district_pop, by = c("state", "district")) %>%
    mutate(synth_votes = pct * district_pop) %>%
    group_by(state) %>%
    mutate(state_vote = sum(synth_votes)) %>%
    group_by(state, candidate) %>%
    summarise(state_pct = sum(synth_votes) / mean(state_vote)) %>%
    mutate(district = "At-large") %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(met_threshold = state_pct >= 0.15,
           state_pct_valid = (state_pct*met_threshold) / sum(state_pct*met_threshold),
           candidate_delegates = delegates * state_pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate, pct = state_pct, pct_valid = state_pct_valid, candidate_delegates) %>%
    arrange(district, candidate) %>%
    bind_rows(district_sim_delegates) %>%
    mutate(sim_id = i)
}

# California ####
ca_start_time <- Sys.time()
# Draw votes
sim_demographics_table <- district_demographics %>%
  filter(state == "California") %>%
  mutate(prob = predict(dem_primary_logit, newdata = ., type = "response"),
         n_vote = round(pop * prob * turnout * 0.05)) %>%
  dplyr::select(-pop, -prob)

sim_demographics <- sim_demographics_table %>%
  lapply(rep, sim_demographics_table$n_vote) %>%
  as.data.frame() 

state_marginal_demographics <- shapeStateDemo("California")

ca_averages_over_time <- state_averages_over_time_unsmoothed %>%
  filter(state == "California", as.numeric(today() - median_date) <= 1) %>%
  mutate(age = as.numeric(today() - median_date),
         nreps = 2 - age)

ca_dirichlet_params <- (ca_averages_over_time %>%
                          lapply(rep, ca_averages_over_time$nreps) %>%
                          as.data.frame() %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          dplyr::select(-median_date) %>%
                          group_by(candidate) %>%
                          mutate(i = 1:n()) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-i) %>%
                          as.matrix() %>%
                          diri.est())$param

ca_dirichlet_params <- ca_dirichlet_params / 2
ca_dirichlet_params[8] <- 0

ca_state_percentages <- (rdirichlet(n_sims, ca_dirichlet_params) %>%
                            as.data.frame() %>%
                            dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, 
                                          steyer = V6, warren = V7, yang = V8)) %>%
  apply(MARGIN = 1, FUN = function(x) x / sum(x)) %>%
  t() %>%
  as.data.frame() %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

ca_state_percentages_adj <- ca_state_percentages %>%
  left_join(overperformance_bounce, by = c("sim_id", "candidate")) %>%
  mutate(nh_place = nh_places,
         nv_place = nv_places,
         sc_place = sc_places,
         state_pct = pmax(state_pct + bounce + 0.02 * (3.5 - nv_place) + 0.02 * (3.5 - sc_place) + 0.01 * (3.5 - nh_place), 0)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct)) %>%
  dplyr::select(-nh_place, -nv_place, -sc_place) %>%
  left_join(sc_second_choice_division, by = c("candidate", "sim_id")) %>%
  melt(id.vars = c("sim_id", "candidate", "state_pct", "overperformance", "bounce_factor", "stay_in", "bounce"), 
       variable.name = "second_choice", value.name = "pct") %>%
  mutate(pct = pct*state_pct*(!stay_in),
         state_pct = case_when(candidate == second_choice ~ state_pct,
                               candidate != second_choice ~ pct)) %>%
  arrange(sim_id, second_choice, candidate)

overperformance_stats <- ca_state_percentages_adj %>%
  group_by(sim_id, candidate) %>%
  summarise(overperformance = mean(overperformance),
            bounce_factor = mean(bounce_factor))

ca_state_percentages_adj <- ca_state_percentages_adj %>%
  group_by(sim_id, second_choice) %>%
  summarise(state_pct = sum(state_pct)) %>%
  dplyr::select(everything(), candidate = second_choice) %>%
  left_join(sc_stayin_matrix %>% melt(id.vars = NULL, value.name = "stay_in") %>%
              dplyr::select(sim_id = Var1, candidate = Var2, stay_in), by = c("sim_id", "candidate")) %>%
  mutate(state_pct = state_pct * stay_in) %>%
  left_join(overperformance_stats, by = c("sim_id", "candidate"))

ca_sims <- vector("list", n_sims)

ca_sims <- foreach(i = 1:n_sims, .packages = c("Hmisc", "dplyr", "reshape2", "tidyr")) %dopar% {
  demographic_lean_sim <- demographic_leans %>%
    mutate(lean = rnorm(n(), mean = avg_lean, sd = lean_se)) %>%
    dplyr::select(candidate, demographic_var, demographic, lean)
  
  # This simulation's national percentages
  this_sim_state_percentage <- ca_state_percentages_adj %>%
    filter(sim_id == i)
  
  # Apply simulated leans to state average
  state_demographic_sim <- demoCandidateBreakdown("California", this_sim_state_percentage, demographic_lean_sim, 
                                                  state_marginal_demographics)
  
  # Turn these into probabilities
  gender_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("men", "women")) %>%
    dplyr::select(candidate, gender = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(gender = case_when(gender == "men" ~ "Male",
                              gender == "women" ~ "Female"))
  
  names(gender_breakdown)[-1] <- paste0("gender_", names(gender_breakdown))[-1]
  
  race_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("white", "black", "latino")) %>%
    dplyr::select(candidate, race = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(race = case_when(race == "white" ~ "White",
                            race == "black" ~ "Black",
                            race == "latino" ~ "Latino"))
  
  names(race_breakdown)[-1] <- paste0("race_", names(race_breakdown))[-1]
  
  age_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("age_1829", "age_3044", "age_4564", "age_65")) %>%
    dplyr::select(candidate, age = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(age = case_when(age == "age_1829" ~ "18-29",
                           age == "age_3044" ~ "30-44",
                           age == "age_4564" ~ "45-64",
                           age == "age_65" ~ "65+"))
  
  names(age_breakdown)[-1] <- paste0("age_", names(age_breakdown)[-1])
  
  education_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("college", "no_college")) %>%
    dplyr::select(candidate, education = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(education = case_when(education == "college" ~ "College",
                                 education == "no_college" ~ "Less than college"))
  
  names(education_breakdown)[-1] <- paste0("education_", names(education_breakdown)[-1])
  
  synth_pop <- sim_demographics %>%
    mutate(rand = runif(n()),
           age_regroup = case_when(age == "18-24" ~ "18-29",
                                   age == "25-34" & rand < 0.5 ~ "18-29",
                                   age == "25-34" & rand >= 0.5 ~ "30-44",
                                   age == "35-44" ~ "30-44",
                                   age == "45-64" ~ "45-64",
                                   age == "65+" ~ "65+"),
           id = 1:n()) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age = age_regroup, education) %>%
    mutate_if(is.factor, as.character) %>%
    left_join(gender_breakdown, by = "gender") %>%
    left_join(race_breakdown, by = "race") %>%
    left_join(age_breakdown, by = "age") %>%
    left_join(education_breakdown, by = "education") %>%
    mutate(biden_prob = gender_biden + race_biden + age_biden + education_biden,
           bloomberg_prob = gender_bloomberg + race_bloomberg + age_bloomberg + education_bloomberg,
           buttigieg_prob = gender_buttigieg + race_buttigieg + age_buttigieg + education_buttigieg,
           klobuchar_prob = gender_klobuchar + race_klobuchar + age_klobuchar + education_klobuchar,
           sanders_prob = gender_sanders + race_sanders + age_sanders + education_sanders,
           steyer_prob = gender_steyer + race_steyer + age_steyer + education_steyer,
           warren_prob = gender_warren + race_warren + age_warren + education_warren,
           yang_prob = gender_yang + race_yang + age_yang + education_yang) %>%
    mutate_at(vars(c("biden_prob", "bloomberg_prob", "buttigieg_prob", "klobuchar_prob", "sanders_prob", "steyer_prob", "warren_prob", 
                     "yang_prob")),
              function(x) pmax(x/100, 0)) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age, education, biden_prob, 
                  bloomberg_prob, buttigieg_prob, klobuchar_prob, sanders_prob, steyer_prob, warren_prob, yang_prob) %>%
    melt(id.vars = c("id", "state", "district", "primary_type", "district_delegates", "gender", "race", "age", "education"),
         variable.name = "candidate_prob", value.name = "prob") %>%
    arrange(id, candidate_prob) %>%
    group_by(id) %>%
    mutate(prob = prob / sum(prob),
           cum_prob = cumsum(prob)) %>%
    dplyr::select(-prob) %>%
    ungroup() %>%
    spread(candidate_prob, cum_prob) %>%
    mutate(rand = runif(n()),
           vote = case_when(rand <= biden_prob ~ "biden",
                            rand > biden_prob & rand <= bloomberg_prob ~ "bloomberg",
                            rand > bloomberg_prob & rand <= buttigieg_prob ~ "buttigieg",
                            rand > buttigieg_prob & rand <= klobuchar_prob ~ "klobuchar",
                            rand > klobuchar_prob & rand <= sanders_prob ~ "sanders",
                            rand > sanders_prob & rand <= steyer_prob ~ "steyer",
                            rand > steyer_prob & rand <= warren_prob ~ "warren",
                            rand > warren_prob ~ "yang")) %>%
    as.tbl()
  
  synth_district_pop <- synth_pop %>%
    group_by(state, district) %>%
    summarise(district_pop = n())
  
  district_sim_delegates <- synth_pop %>%
    group_by(state, district) %>%
    mutate(district_total = n()) %>%
    group_by(state, district, vote) %>%
    summarise(pct = n() / mean(district_total)) %>%
    ungroup() %>%
    mutate(met_threshold = pct >= 0.15) %>% 
    group_by(state, district) %>%
    mutate(pct_valid = (pct*met_threshold) / sum(pct*met_threshold)) %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(candidate_delegates = delegates * pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, district, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate = vote, pct, pct_valid, candidate_delegates) %>%
    arrange(district, candidate)
  
  state_sim_delegates <- district_sim_delegates %>%
    left_join(synth_district_pop, by = c("state", "district")) %>%
    mutate(synth_votes = pct * district_pop) %>%
    group_by(state) %>%
    mutate(state_vote = sum(synth_votes)) %>%
    group_by(state, candidate) %>%
    summarise(state_pct = sum(synth_votes) / mean(state_vote)) %>%
    mutate(district = "At-large") %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(met_threshold = state_pct >= 0.15,
           state_pct_valid = (state_pct*met_threshold) / sum(state_pct*met_threshold),
           candidate_delegates = delegates * state_pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate, pct = state_pct, pct_valid = state_pct_valid, candidate_delegates) %>%
    arrange(district, candidate) %>%
    bind_rows(district_sim_delegates) %>%
    mutate(sim_id = i)
}

# How much did the candidates overperform in California?
ca_overperformance <- ca_state_percentages_adj %>%
  left_join(state_averages %>% filter(state == "California") %>% dplyr::select(candidate, state_avg), by = c("candidate")) %>%
  mutate(overperformance = state_pct - (state_avg/100 + 0.25*overperformance*bounce_factor)) %>%
  dplyr::select(sim_id, candidate, ca_overperformance = overperformance) 


# Colorado ####
co_start_time <- Sys.time()
# Draw votes
sim_demographics_table <- district_demographics %>%
  filter(state == "Colorado") %>%
  mutate(prob = predict(dem_primary_logit, newdata = ., type = "response"),
         n_vote = round(pop * prob * turnout * 0.05)) %>%
  dplyr::select(-pop, -prob)

sim_demographics <- sim_demographics_table %>%
  lapply(rep, sim_demographics_table$n_vote) %>%
  as.data.frame() 

state_marginal_demographics <- shapeStateDemo("Colorado")

co_averages_over_time <- state_averages_over_time_unsmoothed %>%
  filter(state == "Colorado", as.numeric(today() - median_date) <= 1) %>%
  mutate(age = as.numeric(today() - median_date),
         nreps = 2 - age)

co_dirichlet_params <- (co_averages_over_time %>%
                          lapply(rep, co_averages_over_time$nreps) %>%
                          as.data.frame() %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          dplyr::select(-median_date) %>%
                          group_by(candidate) %>%
                          mutate(i = 1:n()) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-i) %>%
                          as.matrix() %>%
                          diri.est())$param

co_dirichlet_params <- 0.5 * co_dirichlet_params / max(1, min(co_dirichlet_params))
co_dirichlet_params[8] <- 0

co_state_percentages <- (rdirichlet(n_sims, co_dirichlet_params) %>%
                            as.data.frame() %>%
                            dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, 
                                          steyer = V6, warren = V7, yang = V8)) %>%
  apply(MARGIN = 1, FUN = function(x) x / sum(x)) %>%
  t() %>%
  as.data.frame() %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

co_state_percentages_adj <- co_state_percentages %>%
  left_join(overperformance_bounce, by = c("sim_id", "candidate")) %>%
  mutate(nh_place = nh_places,
         nv_place = nv_places,
         sc_place = sc_places,
         state_pct = pmax(state_pct + bounce + 0.02 * (3.5 - nv_place) + 0.01 * (3.5 - sc_place) + 0.01 * (3.5 - nh_place), 0)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct)) %>%
  dplyr::select(-nh_place, -nv_place, -sc_place) %>%
  left_join(sc_second_choice_division, by = c("candidate", "sim_id")) %>%
  melt(id.vars = c("sim_id", "candidate", "state_pct", "overperformance", "bounce_factor", "stay_in", "bounce"), 
       variable.name = "second_choice", value.name = "pct") %>%
  mutate(pct = pct*state_pct*(!stay_in),
         state_pct = case_when(candidate == second_choice ~ state_pct,
                               candidate != second_choice ~ pct)) %>%
  arrange(sim_id, second_choice, candidate)

overperformance_stats <- co_state_percentages_adj %>%
  group_by(sim_id, candidate) %>%
  summarise(overperformance = mean(overperformance),
            bounce_factor = mean(bounce_factor))

co_state_percentages_adj <- co_state_percentages_adj %>%
  group_by(sim_id, second_choice) %>%
  summarise(state_pct = sum(state_pct)) %>%
  dplyr::select(everything(), candidate = second_choice) %>%
  left_join(sc_stayin_matrix %>% melt(id.vars = NULL, value.name = "stay_in") %>%
              dplyr::select(sim_id = Var1, candidate = Var2, stay_in), by = c("sim_id", "candidate")) %>%
  mutate(state_pct = state_pct * stay_in) %>%
  left_join(overperformance_stats, by = c("sim_id", "candidate"))

co_sims <- vector("list", n_sims)

co_sims <- foreach(i = 1:n_sims, .packages = c("Hmisc", "dplyr", "reshape2", "tidyr")) %dopar% {
  demographic_lean_sim <- demographic_leans %>%
    mutate(lean = rnorm(n(), mean = avg_lean, sd = lean_se)) %>%
    dplyr::select(candidate, demographic_var, demographic, lean)
  
  # This simulation's national percentages
  this_sim_state_percentage <- co_state_percentages_adj %>%
    filter(sim_id == i)
  
  # Apply simulated leans to state average
  state_demographic_sim <- demoCandidateBreakdown("Colorado", this_sim_state_percentage, demographic_lean_sim, 
                                                  state_marginal_demographics)
  
  # Turn these into probabilities
  gender_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("men", "women")) %>%
    dplyr::select(candidate, gender = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(gender = case_when(gender == "men" ~ "Male",
                              gender == "women" ~ "Female"))
  
  names(gender_breakdown)[-1] <- paste0("gender_", names(gender_breakdown))[-1]
  
  race_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("white", "black", "latino")) %>%
    dplyr::select(candidate, race = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(race = case_when(race == "white" ~ "White",
                            race == "black" ~ "Black",
                            race == "latino" ~ "Latino"))
  
  names(race_breakdown)[-1] <- paste0("race_", names(race_breakdown))[-1]
  
  age_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("age_1829", "age_3044", "age_4564", "age_65")) %>%
    dplyr::select(candidate, age = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(age = case_when(age == "age_1829" ~ "18-29",
                           age == "age_3044" ~ "30-44",
                           age == "age_4564" ~ "45-64",
                           age == "age_65" ~ "65+"))
  
  names(age_breakdown)[-1] <- paste0("age_", names(age_breakdown)[-1])
  
  education_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("college", "no_college")) %>%
    dplyr::select(candidate, education = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(education = case_when(education == "college" ~ "College",
                                 education == "no_college" ~ "Less than college"))
  
  names(education_breakdown)[-1] <- paste0("education_", names(education_breakdown)[-1])
  
  synth_pop <- sim_demographics %>%
    mutate(rand = runif(n()),
           age_regroup = case_when(age == "18-24" ~ "18-29",
                                   age == "25-34" & rand < 0.5 ~ "18-29",
                                   age == "25-34" & rand >= 0.5 ~ "30-44",
                                   age == "35-44" ~ "30-44",
                                   age == "45-64" ~ "45-64",
                                   age == "65+" ~ "65+"),
           id = 1:n()) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age = age_regroup, education) %>%
    mutate_if(is.factor, as.character) %>%
    left_join(gender_breakdown, by = "gender") %>%
    left_join(race_breakdown, by = "race") %>%
    left_join(age_breakdown, by = "age") %>%
    left_join(education_breakdown, by = "education") %>%
    mutate(biden_prob = gender_biden + race_biden + age_biden + education_biden,
           bloomberg_prob = gender_bloomberg + race_bloomberg + age_bloomberg + education_bloomberg,
           buttigieg_prob = gender_buttigieg + race_buttigieg + age_buttigieg + education_buttigieg,
           klobuchar_prob = gender_klobuchar + race_klobuchar + age_klobuchar + education_klobuchar,
           sanders_prob = gender_sanders + race_sanders + age_sanders + education_sanders,
           steyer_prob = gender_steyer + race_steyer + age_steyer + education_steyer,
           warren_prob = gender_warren + race_warren + age_warren + education_warren,
           yang_prob = gender_yang + race_yang + age_yang + education_yang) %>%
    mutate_at(vars(c("biden_prob", "bloomberg_prob", "buttigieg_prob", "klobuchar_prob", "sanders_prob", "steyer_prob", "warren_prob", 
                     "yang_prob")),
              function(x) pmax(x/100, 0)) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age, education, biden_prob, 
                  bloomberg_prob, buttigieg_prob, klobuchar_prob, sanders_prob, steyer_prob, warren_prob, yang_prob) %>%
    melt(id.vars = c("id", "state", "district", "primary_type", "district_delegates", "gender", "race", "age", "education"),
         variable.name = "candidate_prob", value.name = "prob") %>%
    arrange(id, candidate_prob) %>%
    group_by(id) %>%
    mutate(prob = prob / sum(prob),
           cum_prob = cumsum(prob)) %>%
    dplyr::select(-prob) %>%
    ungroup() %>%
    spread(candidate_prob, cum_prob) %>%
    mutate(rand = runif(n()),
           vote = case_when(rand <= biden_prob ~ "biden",
                            rand > biden_prob & rand <= bloomberg_prob ~ "bloomberg",
                            rand > bloomberg_prob & rand <= buttigieg_prob ~ "buttigieg",
                            rand > buttigieg_prob & rand <= klobuchar_prob ~ "klobuchar",
                            rand > klobuchar_prob & rand <= sanders_prob ~ "sanders",
                            rand > sanders_prob & rand <= steyer_prob ~ "steyer",
                            rand > steyer_prob & rand <= warren_prob ~ "warren",
                            rand > warren_prob ~ "yang")) %>%
    as.tbl()
  
  synth_district_pop <- synth_pop %>%
    group_by(state, district) %>%
    summarise(district_pop = n())
  
  district_sim_delegates <- synth_pop %>%
    group_by(state, district) %>%
    mutate(district_total = n()) %>%
    group_by(state, district, vote) %>%
    summarise(pct = n() / mean(district_total)) %>%
    ungroup() %>%
    mutate(met_threshold = pct >= 0.15) %>% 
    group_by(state, district) %>%
    mutate(pct_valid = (pct*met_threshold) / sum(pct*met_threshold)) %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(candidate_delegates = delegates * pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, district, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate = vote, pct, pct_valid, candidate_delegates) %>%
    arrange(district, candidate)
  
  state_sim_delegates <- district_sim_delegates %>%
    left_join(synth_district_pop, by = c("state", "district")) %>%
    mutate(synth_votes = pct * district_pop) %>%
    group_by(state) %>%
    mutate(state_vote = sum(synth_votes)) %>%
    group_by(state, candidate) %>%
    summarise(state_pct = sum(synth_votes) / mean(state_vote)) %>%
    mutate(district = "At-large") %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(met_threshold = state_pct >= 0.15,
           state_pct_valid = (state_pct*met_threshold) / sum(state_pct*met_threshold),
           candidate_delegates = delegates * state_pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate, pct = state_pct, pct_valid = state_pct_valid, candidate_delegates) %>%
    arrange(district, candidate) %>%
    bind_rows(district_sim_delegates) %>%
    mutate(sim_id = i)
}

# How much did the candidates overperform in Colorado?
co_overperformance <- co_state_percentages_adj %>%
  left_join(state_averages %>% filter(state == "Colorado") %>% dplyr::select(candidate, state_avg), by = c("candidate")) %>%
  mutate(overperformance = state_pct - (state_avg/100 + 0.5*overperformance*bounce_factor)) %>%
  dplyr::select(sim_id, candidate, co_overperformance = overperformance) 

# Democrats Abroad ####
da_start_time <- Sys.time()
# Shortcut since I know nothing about what the situation looks like on the ground: use polls from California
### Draw vote shares
da_dirichlet_params <- 0.5 * ca_dirichlet_params
da_dirichlet_params[1] <- da_dirichlet_params[1] / 2
da_dirichlet_params[8] <- 0

da_state_percentages <- rdirichlet(n_sims, da_dirichlet_params) %>%
  as.data.frame() %>%
  dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, steyer = V6, warren = V7, yang = V8) %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct") %>%
  mutate(nh_place = nh_places,
         nv_place = nv_places,
         sc_place = sc_places,
         state_pct = pmax(state_pct + 0.03 * (3.5 - nv_place) + 0.02 * (3.5 - sc_place) + 0.01 * (3.5 - nh_place), 0)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct)) %>%
  dplyr::select(-nh_place, -nv_place, -sc_place) %>%
  ungroup()

da_sims <- vector("list", n_sims)

### Simulate
da_sims <- foreach(i = 1:n_sims, .packages = c("Hmisc", "dplyr", "reshape2", "tidyr")) %dopar% {
  this_sim_state_percentage <- da_state_percentages %>%
    filter(sim_id == i)
  
  district_sim_pct <- this_sim_state_percentage %>%
    mutate(met_threshold = state_pct >= 0.15)
  
  state_sim_delegates <- district_sim_pct %>%
    mutate(pct_valid = state_pct * met_threshold) %>% 
    mutate(pct_valid = pct_valid / sum(pct_valid),
           state = "Democrats Abroad",
           district = "At-large") %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(candidate_delegates = delegates * pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, district, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate, pct = state_pct, pct_valid, candidate_delegates) %>%
    arrange(district, candidate) %>%
    mutate(sim_id = i)
}

# Massachusetts ####
ma_start_time <- Sys.time()
# Draw votes
sim_demographics_table <- district_demographics %>%
  filter(state == "Massachusetts") %>%
  mutate(prob = predict(dem_primary_logit, newdata = ., type = "response"),
         n_vote = round(pop * prob * turnout * 0.05)) %>%
  dplyr::select(-pop, -prob)

sim_demographics <- sim_demographics_table %>%
  lapply(rep, sim_demographics_table$n_vote) %>%
  as.data.frame() 

state_marginal_demographics <- shapeStateDemo("Massachusetts")

ma_averages_over_time <- state_averages_over_time_unsmoothed %>%
  filter(state == "Massachusetts", as.numeric(today() - median_date) <= 1) %>%
  mutate(age = as.numeric(today() - median_date),
         nreps = 2 - age)

ma_dirichlet_params <- (ma_averages_over_time %>%
                          lapply(rep, ma_averages_over_time$nreps) %>%
                          as.data.frame() %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          dplyr::select(-median_date) %>%
                          group_by(candidate) %>%
                          mutate(i = 1:n()) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-i) %>%
                          as.matrix() %>%
                          diri.est())$param

ma_dirichlet_params <- ma_dirichlet_params / 2
ma_dirichlet_params[8] <- 0

ma_state_percentages <- (rdirichlet(n_sims, ma_dirichlet_params) %>%
                            as.data.frame() %>%
                            dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, 
                                          steyer = V6, warren = V7, yang = V8)) %>%
  apply(MARGIN = 1, FUN = function(x) x / sum(x)) %>%
  t() %>%
  as.data.frame() %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

ma_state_percentages_adj <- ma_state_percentages %>%
  left_join(overperformance_bounce, by = c("sim_id", "candidate")) %>%
  mutate(nh_place = nh_places,
         nv_place = nv_places,
         sc_place = sc_places,
         state_pct = pmax(state_pct + bounce + 0.03 * (3.5 - nh_place) + 0.01 * (3.5 - sc_place) + 0.01 * (3.5 - nv_place), 0)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct)) %>%
  dplyr::select(-nh_place, -sc_place, -nv_place) %>%
  left_join(sc_second_choice_division, by = c("candidate", "sim_id")) %>%
  melt(id.vars = c("sim_id", "candidate", "state_pct", "overperformance", "bounce_factor", "stay_in", "bounce"), 
       variable.name = "second_choice", value.name = "pct") %>%
  mutate(pct = pct*state_pct*(!stay_in),
         state_pct = case_when(candidate == second_choice ~ state_pct,
                               candidate != second_choice ~ pct)) %>%
  arrange(sim_id, second_choice, candidate)

overperformance_stats <- ma_state_percentages_adj %>%
  group_by(sim_id, candidate) %>%
  summarise(overperformance = mean(overperformance),
            bounce_factor = mean(bounce_factor))

ma_state_percentages_adj <- ma_state_percentages_adj %>%
  group_by(sim_id, second_choice) %>%
  summarise(state_pct = sum(state_pct)) %>%
  dplyr::select(everything(), candidate = second_choice) %>%
  left_join(sc_stayin_matrix %>% melt(id.vars = NULL, value.name = "stay_in") %>%
              dplyr::select(sim_id = Var1, candidate = Var2, stay_in), by = c("sim_id", "candidate")) %>%
  mutate(state_pct = state_pct * stay_in) %>%
  left_join(overperformance_stats, by = c("sim_id", "candidate"))

ma_sims <- vector("list", n_sims)

ma_sims <- foreach(i = 1:n_sims, .packages = c("Hmisc", "dplyr", "reshape2", "tidyr")) %dopar% {
  demographic_lean_sim <- demographic_leans %>%
    mutate(lean = rnorm(n(), mean = avg_lean, sd = lean_se)) %>%
    dplyr::select(candidate, demographic_var, demographic, lean)
  
  # This simulation's national percentages
  this_sim_state_percentage <- ma_state_percentages_adj %>%
    filter(sim_id == i)
  
  # Apply simulated leans to state average
  state_demographic_sim <- demoCandidateBreakdown("Massachusetts", this_sim_state_percentage, demographic_lean_sim, 
                                                  state_marginal_demographics)
  
  # Turn these into probabilities
  gender_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("men", "women")) %>%
    dplyr::select(candidate, gender = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(gender = case_when(gender == "men" ~ "Male",
                              gender == "women" ~ "Female"))
  
  names(gender_breakdown)[-1] <- paste0("gender_", names(gender_breakdown))[-1]
  
  race_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("white", "black", "latino")) %>%
    dplyr::select(candidate, race = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(race = case_when(race == "white" ~ "White",
                            race == "black" ~ "Black",
                            race == "latino" ~ "Latino"))
  
  names(race_breakdown)[-1] <- paste0("race_", names(race_breakdown))[-1]
  
  age_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("age_1829", "age_3044", "age_4564", "age_65")) %>%
    dplyr::select(candidate, age = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(age = case_when(age == "age_1829" ~ "18-29",
                           age == "age_3044" ~ "30-44",
                           age == "age_4564" ~ "45-64",
                           age == "age_65" ~ "65+"))
  
  names(age_breakdown)[-1] <- paste0("age_", names(age_breakdown)[-1])
  
  education_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("college", "no_college")) %>%
    dplyr::select(candidate, education = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(education = case_when(education == "college" ~ "College",
                                 education == "no_college" ~ "Less than college"))
  
  names(education_breakdown)[-1] <- paste0("education_", names(education_breakdown)[-1])
  
  synth_pop <- sim_demographics %>%
    mutate(rand = runif(n()),
           age_regroup = case_when(age == "18-24" ~ "18-29",
                                   age == "25-34" & rand < 0.5 ~ "18-29",
                                   age == "25-34" & rand >= 0.5 ~ "30-44",
                                   age == "35-44" ~ "30-44",
                                   age == "45-64" ~ "45-64",
                                   age == "65+" ~ "65+"),
           id = 1:n()) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age = age_regroup, education) %>%
    mutate_if(is.factor, as.character) %>%
    left_join(gender_breakdown, by = "gender") %>%
    left_join(race_breakdown, by = "race") %>%
    left_join(age_breakdown, by = "age") %>%
    left_join(education_breakdown, by = "education") %>%
    mutate(biden_prob = gender_biden + race_biden + age_biden + education_biden,
           bloomberg_prob = gender_bloomberg + race_bloomberg + age_bloomberg + education_bloomberg,
           buttigieg_prob = gender_buttigieg + race_buttigieg + age_buttigieg + education_buttigieg,
           klobuchar_prob = gender_klobuchar + race_klobuchar + age_klobuchar + education_klobuchar,
           sanders_prob = gender_sanders + race_sanders + age_sanders + education_sanders,
           steyer_prob = gender_steyer + race_steyer + age_steyer + education_steyer,
           warren_prob = gender_warren + race_warren + age_warren + education_warren,
           yang_prob = gender_yang + race_yang + age_yang + education_yang) %>%
    mutate_at(vars(c("biden_prob", "bloomberg_prob", "buttigieg_prob", "klobuchar_prob", "sanders_prob", "steyer_prob", "warren_prob", 
                     "yang_prob")),
              function(x) pmax(x/100, 0)) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age, education, biden_prob, 
                  bloomberg_prob, buttigieg_prob, klobuchar_prob, sanders_prob, steyer_prob, warren_prob, yang_prob) %>%
    melt(id.vars = c("id", "state", "district", "primary_type", "district_delegates", "gender", "race", "age", "education"),
         variable.name = "candidate_prob", value.name = "prob") %>%
    arrange(id, candidate_prob) %>%
    group_by(id) %>%
    mutate(prob = prob / sum(prob),
           cum_prob = cumsum(prob)) %>%
    dplyr::select(-prob) %>%
    ungroup() %>%
    spread(candidate_prob, cum_prob) %>%
    mutate(rand = runif(n()),
           vote = case_when(rand <= biden_prob ~ "biden",
                            rand > biden_prob & rand <= bloomberg_prob ~ "bloomberg",
                            rand > bloomberg_prob & rand <= buttigieg_prob ~ "buttigieg",
                            rand > buttigieg_prob & rand <= klobuchar_prob ~ "klobuchar",
                            rand > klobuchar_prob & rand <= sanders_prob ~ "sanders",
                            rand > sanders_prob & rand <= steyer_prob ~ "steyer",
                            rand > steyer_prob & rand <= warren_prob ~ "warren",
                            rand > warren_prob ~ "yang")) %>%
    as.tbl()
  
  synth_district_pop <- synth_pop %>%
    group_by(state, district) %>%
    summarise(district_pop = n())
  
  district_sim_delegates <- synth_pop %>%
    group_by(state, district) %>%
    mutate(district_total = n()) %>%
    group_by(state, district, vote) %>%
    summarise(pct = n() / mean(district_total)) %>%
    ungroup() %>%
    mutate(met_threshold = pct >= 0.15) %>% 
    group_by(state, district) %>%
    mutate(pct_valid = (pct*met_threshold) / sum(pct*met_threshold)) %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(candidate_delegates = delegates * pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, district, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate = vote, pct, pct_valid, candidate_delegates) %>%
    arrange(district, candidate)
  
  state_sim_delegates <- district_sim_delegates %>%
    left_join(synth_district_pop, by = c("state", "district")) %>%
    mutate(synth_votes = pct * district_pop) %>%
    group_by(state) %>%
    mutate(state_vote = sum(synth_votes)) %>%
    group_by(state, candidate) %>%
    summarise(state_pct = sum(synth_votes) / mean(state_vote)) %>%
    mutate(district = "At-large") %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(met_threshold = state_pct >= 0.15,
           state_pct_valid = (state_pct*met_threshold) / sum(state_pct*met_threshold),
           candidate_delegates = delegates * state_pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate, pct = state_pct, pct_valid = state_pct_valid, candidate_delegates) %>%
    arrange(district, candidate) %>%
    bind_rows(district_sim_delegates) %>%
    mutate(sim_id = i)
}

# How much did the candidates overperform in Massachusetts?
ma_overperformance <- ma_state_percentages_adj %>%
  left_join(state_averages %>% filter(state == "Massachusetts") %>% dplyr::select(candidate, state_avg), by = c("candidate")) %>%
  mutate(overperformance = state_pct - (state_avg/100 + 0.5*overperformance*bounce_factor)) %>%
  dplyr::select(sim_id, candidate, ma_overperformance = overperformance) 

# Minnesota ####
mn_start_time <- Sys.time()
# Draw votes
sim_demographics_table <- district_demographics %>%
  filter(state == "Minnesota") %>%
  mutate(prob = predict(dem_primary_logit, newdata = ., type = "response"),
         n_vote = round(pop * prob * turnout * 0.05)) %>%
  dplyr::select(-pop, -prob)

sim_demographics <- sim_demographics_table %>%
  lapply(rep, sim_demographics_table$n_vote) %>%
  as.data.frame() 

state_marginal_demographics <- shapeStateDemo("Minnesota")

mn_averages_over_time <- state_averages_over_time_unsmoothed %>%
  filter(state == "Minnesota", as.numeric(today() - median_date) <= 1) %>%
  mutate(age = as.numeric(today() - median_date),
         nreps = 2 - age)

mn_dirichlet_params <- (mn_averages_over_time %>%
                          lapply(rep, mn_averages_over_time$nreps) %>%
                          as.data.frame() %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          dplyr::select(-median_date) %>%
                          group_by(candidate) %>%
                          mutate(i = 1:n()) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-i) %>%
                          as.matrix() %>%
                          diri.est())$param

mn_dirichlet_params <- 0.5 * mn_dirichlet_params / max(1, min(mn_dirichlet_params))
mn_dirichlet_params[8] <- 0

mn_state_percentages <- (rdirichlet(n_sims, mn_dirichlet_params) %>%
                            as.data.frame() %>%
                            dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, 
                                          steyer = V6, warren = V7, yang = V8)) %>%
  apply(MARGIN = 1, FUN = function(x) x / sum(x)) %>%
  t() %>%
  as.data.frame() %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

mn_state_percentages_adj <- mn_state_percentages %>%
  left_join(overperformance_bounce, by = c("sim_id", "candidate")) %>%
  mutate(nh_place = nh_places,
         nv_place = nv_places,
         sc_place = sc_places,
         state_pct = pmax(state_pct + bounce + 0.01 * (3.5 - nh_place) + 0.01 *(3.5 - sc_place), 0)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct)) %>%
  dplyr::select(-nh_place, -nv_place, -sc_place) %>%
  left_join(sc_second_choice_division, by = c("candidate", "sim_id")) %>%
  melt(id.vars = c("sim_id", "candidate", "state_pct", "overperformance", "bounce_factor", "stay_in", "bounce"), 
       variable.name = "second_choice", value.name = "pct") %>%
  mutate(pct = pct*state_pct*(!stay_in),
         state_pct = case_when(candidate == second_choice ~ state_pct,
                               candidate != second_choice ~ pct)) %>%
  arrange(sim_id, second_choice, candidate)

overperformance_stats <- mn_state_percentages_adj %>%
  group_by(sim_id, candidate) %>%
  summarise(overperformance = mean(overperformance),
            bounce_factor = mean(bounce_factor))

mn_state_percentages_adj <- mn_state_percentages_adj %>%
  group_by(sim_id, second_choice) %>%
  summarise(state_pct = sum(state_pct)) %>%
  dplyr::select(everything(), candidate = second_choice) %>%
  left_join(sc_stayin_matrix %>% melt(id.vars = NULL, value.name = "stay_in") %>%
              dplyr::select(sim_id = Var1, candidate = Var2, stay_in), by = c("sim_id", "candidate")) %>%
  mutate(state_pct = state_pct * stay_in) %>%
  left_join(overperformance_stats, by = c("sim_id", "candidate"))

mn_sims <- vector("list", n_sims)

mn_sims <- foreach(i = 1:n_sims, .packages = c("Hmisc", "dplyr", "reshape2", "tidyr")) %dopar% {
  demographic_lean_sim <- demographic_leans %>%
    mutate(lean = rnorm(n(), mean = avg_lean, sd = lean_se)) %>%
    dplyr::select(candidate, demographic_var, demographic, lean)
  
  # This simulation's national percentages
  this_sim_state_percentage <- mn_state_percentages_adj %>%
    filter(sim_id == i)
  
  # Apply simulated leans to state average
  state_demographic_sim <- demoCandidateBreakdown("Minnesota", this_sim_state_percentage, demographic_lean_sim, 
                                                  state_marginal_demographics)
  
  # Turn these into probabilities
  gender_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("men", "women")) %>%
    dplyr::select(candidate, gender = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(gender = case_when(gender == "men" ~ "Male",
                              gender == "women" ~ "Female"))
  
  names(gender_breakdown)[-1] <- paste0("gender_", names(gender_breakdown))[-1]
  
  race_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("white", "black", "latino")) %>%
    dplyr::select(candidate, race = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(race = case_when(race == "white" ~ "White",
                            race == "black" ~ "Black",
                            race == "latino" ~ "Latino"))
  
  names(race_breakdown)[-1] <- paste0("race_", names(race_breakdown))[-1]
  
  age_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("age_1829", "age_3044", "age_4564", "age_65")) %>%
    dplyr::select(candidate, age = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(age = case_when(age == "age_1829" ~ "18-29",
                           age == "age_3044" ~ "30-44",
                           age == "age_4564" ~ "45-64",
                           age == "age_65" ~ "65+"))
  
  names(age_breakdown)[-1] <- paste0("age_", names(age_breakdown)[-1])
  
  education_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("college", "no_college")) %>%
    dplyr::select(candidate, education = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(education = case_when(education == "college" ~ "College",
                                 education == "no_college" ~ "Less than college"))
  
  names(education_breakdown)[-1] <- paste0("education_", names(education_breakdown)[-1])
  
  synth_pop <- sim_demographics %>%
    mutate(rand = runif(n()),
           age_regroup = case_when(age == "18-24" ~ "18-29",
                                   age == "25-34" & rand < 0.5 ~ "18-29",
                                   age == "25-34" & rand >= 0.5 ~ "30-44",
                                   age == "35-44" ~ "30-44",
                                   age == "45-64" ~ "45-64",
                                   age == "65+" ~ "65+"),
           id = 1:n()) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age = age_regroup, education) %>%
    mutate_if(is.factor, as.character) %>%
    left_join(gender_breakdown, by = "gender") %>%
    left_join(race_breakdown, by = "race") %>%
    left_join(age_breakdown, by = "age") %>%
    left_join(education_breakdown, by = "education") %>%
    mutate(biden_prob = gender_biden + race_biden + age_biden + education_biden,
           bloomberg_prob = gender_bloomberg + race_bloomberg + age_bloomberg + education_bloomberg,
           buttigieg_prob = gender_buttigieg + race_buttigieg + age_buttigieg + education_buttigieg,
           klobuchar_prob = gender_klobuchar + race_klobuchar + age_klobuchar + education_klobuchar,
           sanders_prob = gender_sanders + race_sanders + age_sanders + education_sanders,
           steyer_prob = gender_steyer + race_steyer + age_steyer + education_steyer,
           warren_prob = gender_warren + race_warren + age_warren + education_warren,
           yang_prob = gender_yang + race_yang + age_yang + education_yang) %>%
    mutate_at(vars(c("biden_prob", "bloomberg_prob", "buttigieg_prob", "klobuchar_prob", "sanders_prob", "steyer_prob", "warren_prob", 
                     "yang_prob")),
              function(x) pmax(x/100, 0)) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age, education, biden_prob, 
                  bloomberg_prob, buttigieg_prob, klobuchar_prob, sanders_prob, steyer_prob, warren_prob, yang_prob) %>%
    melt(id.vars = c("id", "state", "district", "primary_type", "district_delegates", "gender", "race", "age", "education"),
         variable.name = "candidate_prob", value.name = "prob") %>%
    arrange(id, candidate_prob) %>%
    group_by(id) %>%
    mutate(prob = prob / sum(prob),
           cum_prob = cumsum(prob)) %>%
    dplyr::select(-prob) %>%
    ungroup() %>%
    spread(candidate_prob, cum_prob) %>%
    mutate(rand = runif(n()),
           vote = case_when(rand <= biden_prob ~ "biden",
                            rand > biden_prob & rand <= bloomberg_prob ~ "bloomberg",
                            rand > bloomberg_prob & rand <= buttigieg_prob ~ "buttigieg",
                            rand > buttigieg_prob & rand <= klobuchar_prob ~ "klobuchar",
                            rand > klobuchar_prob & rand <= sanders_prob ~ "sanders",
                            rand > sanders_prob & rand <= steyer_prob ~ "steyer",
                            rand > steyer_prob & rand <= warren_prob ~ "warren",
                            rand > warren_prob ~ "yang")) %>%
    as.tbl()
  
  synth_district_pop <- synth_pop %>%
    group_by(state, district) %>%
    summarise(district_pop = n())
  
  district_sim_delegates <- synth_pop %>%
    group_by(state, district) %>%
    mutate(district_total = n()) %>%
    group_by(state, district, vote) %>%
    summarise(pct = n() / mean(district_total)) %>%
    ungroup() %>%
    mutate(met_threshold = pct >= 0.15) %>% 
    group_by(state, district) %>%
    mutate(pct_valid = (pct*met_threshold) / sum(pct*met_threshold)) %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(candidate_delegates = delegates * pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, district, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate = vote, pct, pct_valid, candidate_delegates) %>%
    arrange(district, candidate)
  
  state_sim_delegates <- district_sim_delegates %>%
    left_join(synth_district_pop, by = c("state", "district")) %>%
    mutate(synth_votes = pct * district_pop) %>%
    group_by(state) %>%
    mutate(state_vote = sum(synth_votes)) %>%
    group_by(state, candidate) %>%
    summarise(state_pct = sum(synth_votes) / mean(state_vote)) %>%
    mutate(district = "At-large") %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(met_threshold = state_pct >= 0.15,
           state_pct_valid = (state_pct*met_threshold) / sum(state_pct*met_threshold),
           candidate_delegates = delegates * state_pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate, pct = state_pct, pct_valid = state_pct_valid, candidate_delegates) %>%
    arrange(district, candidate) %>%
    bind_rows(district_sim_delegates) %>%
    mutate(sim_id = i)
}

# How much did the candidates overperform in Minnesota?
mn_overperformance <- mn_state_percentages_adj %>%
  left_join(state_averages %>% filter(state == "Minnesota") %>% dplyr::select(candidate, state_avg), by = c("candidate")) %>%
  mutate(overperformance = state_pct - (state_avg/100 + 0.25*overperformance*bounce_factor)) %>%
  dplyr::select(sim_id, candidate, mn_overperformance = overperformance) 


# North Carolina ####
nc_start_time <- Sys.time()
# Draw votes
sim_demographics_table <- district_demographics %>%
  filter(state == "North Carolina") %>%
  mutate(prob = predict(dem_primary_logit, newdata = ., type = "response"),
         n_vote = round(pop * prob * turnout * 0.05)) %>%
  dplyr::select(-pop, -prob)

sim_demographics <- sim_demographics_table %>%
  lapply(rep, sim_demographics_table$n_vote) %>%
  as.data.frame() 

state_marginal_demographics <- shapeStateDemo("North Carolina")

nc_averages_over_time <- state_averages_over_time_unsmoothed %>%
  filter(state == "North Carolina", as.numeric(today() - median_date) <= 1) %>%
  mutate(age = as.numeric(today() - median_date),
         nreps = 2 - age)

nc_dirichlet_params <- (nc_averages_over_time %>%
                          lapply(rep, nc_averages_over_time$nreps) %>%
                          as.data.frame() %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          dplyr::select(-median_date) %>%
                          group_by(candidate) %>%
                          mutate(i = 1:n()) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-i) %>%
                          as.matrix() %>%
                          diri.est())$param

nc_dirichlet_params <- 0.5 * nc_dirichlet_params / max(1, min(nc_dirichlet_params))
nc_dirichlet_params[8] <- 0

nc_state_percentages <- (rdirichlet(n_sims, nc_dirichlet_params) %>%
                            as.data.frame() %>%
                            dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, 
                                          steyer = V6, warren = V7, yang = V8)) %>%
  apply(MARGIN = 1, FUN = function(x) x / sum(x)) %>%
  t() %>%
  as.data.frame() %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

nc_state_percentages_adj <- nc_state_percentages %>%
  left_join(overperformance_bounce, by = c("sim_id", "candidate")) %>%
  mutate(sc_place = sc_places,
         state_pct = pmax(state_pct + bounce + 0.03 * (3.5 - sc_place), 0)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct)) %>%
  dplyr::select(-sc_place) %>%
  left_join(sc_second_choice_division, by = c("candidate", "sim_id")) %>%
  melt(id.vars = c("sim_id", "candidate", "state_pct", "overperformance", "bounce_factor", "stay_in", "bounce"), 
       variable.name = "second_choice", value.name = "pct") %>%
  mutate(pct = pct*state_pct*(!stay_in),
         state_pct = case_when(candidate == second_choice ~ state_pct,
                               candidate != second_choice ~ pct)) %>%
  arrange(sim_id, second_choice, candidate)

overperformance_stats <- nc_state_percentages_adj %>%
  group_by(sim_id, candidate) %>%
  summarise(overperformance = mean(overperformance),
            bounce_factor = mean(bounce_factor))

nc_state_percentages_adj <- nc_state_percentages_adj %>%
  group_by(sim_id, second_choice) %>%
  summarise(state_pct = sum(state_pct)) %>%
  dplyr::select(everything(), candidate = second_choice) %>%
  left_join(sc_stayin_matrix %>% melt(id.vars = NULL, value.name = "stay_in") %>%
              dplyr::select(sim_id = Var1, candidate = Var2, stay_in), by = c("sim_id", "candidate")) %>%
  mutate(state_pct = state_pct * stay_in) %>%
  left_join(overperformance_stats, by = c("sim_id", "candidate"))

nc_sims <- vector("list", n_sims)

nc_sims <- foreach(i = 1:n_sims, .packages = c("Hmisc", "dplyr", "reshape2", "tidyr")) %dopar% {
  demographic_lean_sim <- demographic_leans %>%
    mutate(lean = rnorm(n(), mean = avg_lean, sd = lean_se)) %>%
    dplyr::select(candidate, demographic_var, demographic, lean)
  
  # This simulation's national percentages
  this_sim_state_percentage <- nc_state_percentages_adj %>%
    filter(sim_id == i)
  
  # Apply simulated leans to state average
  state_demographic_sim <- demoCandidateBreakdown("North Carolina", this_sim_state_percentage, demographic_lean_sim, 
                                                  state_marginal_demographics)
  
  # Turn these into probabilities
  gender_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("men", "women")) %>%
    dplyr::select(candidate, gender = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(gender = case_when(gender == "men" ~ "Male",
                              gender == "women" ~ "Female"))
  
  names(gender_breakdown)[-1] <- paste0("gender_", names(gender_breakdown))[-1]
  
  race_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("white", "black", "latino")) %>%
    dplyr::select(candidate, race = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(race = case_when(race == "white" ~ "White",
                            race == "black" ~ "Black",
                            race == "latino" ~ "Latino"))
  
  names(race_breakdown)[-1] <- paste0("race_", names(race_breakdown))[-1]
  
  age_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("age_1829", "age_3044", "age_4564", "age_65")) %>%
    dplyr::select(candidate, age = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(age = case_when(age == "age_1829" ~ "18-29",
                           age == "age_3044" ~ "30-44",
                           age == "age_4564" ~ "45-64",
                           age == "age_65" ~ "65+"))
  
  names(age_breakdown)[-1] <- paste0("age_", names(age_breakdown)[-1])
  
  education_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("college", "no_college")) %>%
    dplyr::select(candidate, education = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(education = case_when(education == "college" ~ "College",
                                 education == "no_college" ~ "Less than college"))
  
  names(education_breakdown)[-1] <- paste0("education_", names(education_breakdown)[-1])
  
  synth_pop <- sim_demographics %>%
    mutate(rand = runif(n()),
           age_regroup = case_when(age == "18-24" ~ "18-29",
                                   age == "25-34" & rand < 0.5 ~ "18-29",
                                   age == "25-34" & rand >= 0.5 ~ "30-44",
                                   age == "35-44" ~ "30-44",
                                   age == "45-64" ~ "45-64",
                                   age == "65+" ~ "65+"),
           id = 1:n()) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age = age_regroup, education) %>%
    mutate_if(is.factor, as.character) %>%
    left_join(gender_breakdown, by = "gender") %>%
    left_join(race_breakdown, by = "race") %>%
    left_join(age_breakdown, by = "age") %>%
    left_join(education_breakdown, by = "education") %>%
    mutate(biden_prob = gender_biden + race_biden + age_biden + education_biden,
           bloomberg_prob = gender_bloomberg + race_bloomberg + age_bloomberg + education_bloomberg,
           buttigieg_prob = gender_buttigieg + race_buttigieg + age_buttigieg + education_buttigieg,
           klobuchar_prob = gender_klobuchar + race_klobuchar + age_klobuchar + education_klobuchar,
           sanders_prob = gender_sanders + race_sanders + age_sanders + education_sanders,
           steyer_prob = gender_steyer + race_steyer + age_steyer + education_steyer,
           warren_prob = gender_warren + race_warren + age_warren + education_warren,
           yang_prob = gender_yang + race_yang + age_yang + education_yang) %>%
    mutate_at(vars(c("biden_prob", "bloomberg_prob", "buttigieg_prob", "klobuchar_prob", "sanders_prob", "steyer_prob", "warren_prob", 
                     "yang_prob")),
              function(x) pmax(x/100, 0)) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age, education, biden_prob, 
                  bloomberg_prob, buttigieg_prob, klobuchar_prob, sanders_prob, steyer_prob, warren_prob, yang_prob) %>%
    melt(id.vars = c("id", "state", "district", "primary_type", "district_delegates", "gender", "race", "age", "education"),
         variable.name = "candidate_prob", value.name = "prob") %>%
    arrange(id, candidate_prob) %>%
    group_by(id) %>%
    mutate(prob = prob / sum(prob),
           cum_prob = cumsum(prob)) %>%
    dplyr::select(-prob) %>%
    ungroup() %>%
    spread(candidate_prob, cum_prob) %>%
    mutate(rand = runif(n()),
           vote = case_when(rand <= biden_prob ~ "biden",
                            rand > biden_prob & rand <= bloomberg_prob ~ "bloomberg",
                            rand > bloomberg_prob & rand <= buttigieg_prob ~ "buttigieg",
                            rand > buttigieg_prob & rand <= klobuchar_prob ~ "klobuchar",
                            rand > klobuchar_prob & rand <= sanders_prob ~ "sanders",
                            rand > sanders_prob & rand <= steyer_prob ~ "steyer",
                            rand > steyer_prob & rand <= warren_prob ~ "warren",
                            rand > warren_prob ~ "yang")) %>%
    as.tbl()
  
  synth_district_pop <- synth_pop %>%
    group_by(state, district) %>%
    summarise(district_pop = n())
  
  district_sim_delegates <- synth_pop %>%
    group_by(state, district) %>%
    mutate(district_total = n()) %>%
    group_by(state, district, vote) %>%
    summarise(pct = n() / mean(district_total)) %>%
    ungroup() %>%
    mutate(met_threshold = pct >= 0.15) %>% 
    group_by(state, district) %>%
    mutate(pct_valid = (pct*met_threshold) / sum(pct*met_threshold)) %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(candidate_delegates = delegates * pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, district, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate = vote, pct, pct_valid, candidate_delegates) %>%
    arrange(district, candidate)
  
  state_sim_delegates <- district_sim_delegates %>%
    left_join(synth_district_pop, by = c("state", "district")) %>%
    mutate(synth_votes = pct * district_pop) %>%
    group_by(state) %>%
    mutate(state_vote = sum(synth_votes)) %>%
    group_by(state, candidate) %>%
    summarise(state_pct = sum(synth_votes) / mean(state_vote)) %>%
    mutate(district = "At-large") %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(met_threshold = state_pct >= 0.15,
           state_pct_valid = (state_pct*met_threshold) / sum(state_pct*met_threshold),
           candidate_delegates = delegates * state_pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate, pct = state_pct, pct_valid = state_pct_valid, candidate_delegates) %>%
    arrange(district, candidate) %>%
    bind_rows(district_sim_delegates) %>%
    mutate(sim_id = i)
}

# How much did the candidates overperform in North Carolina?
nc_overperformance <- nc_state_percentages_adj %>%
  left_join(state_averages %>% filter(state == "North Carolina") %>% dplyr::select(candidate, state_avg), by = c("candidate")) %>%
  mutate(overperformance = state_pct - (state_avg/100 + 0.25*overperformance*bounce_factor)) %>%
  dplyr::select(sim_id, candidate, nc_overperformance = overperformance) 


# Oklahoma ####
ok_start_time <- Sys.time()
# Draw votes
sim_demographics_table <- district_demographics %>%
  filter(state == "Oklahoma") %>%
  mutate(prob = predict(dem_primary_logit, newdata = ., type = "response"),
         n_vote = round(pop * prob * turnout * 0.05)) %>%
  dplyr::select(-pop, -prob)

sim_demographics <- sim_demographics_table %>%
  lapply(rep, sim_demographics_table$n_vote) %>%
  as.data.frame() 

state_marginal_demographics <- shapeStateDemo("Oklahoma")

ok_averages_over_time <- state_averages_over_time_unsmoothed %>%
  filter(state == "Oklahoma", as.numeric(today() - median_date) <= 1) %>%
  mutate(age = as.numeric(today() - median_date),
         nreps = 2 - age) 

ok_dirichlet_params <- (ok_averages_over_time %>%
                          lapply(rep, ok_averages_over_time$nreps) %>%
                          as.data.frame() %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          dplyr::select(-median_date) %>%
                          group_by(candidate) %>%
                          mutate(i = 1:n()) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-i) %>%
                          as.matrix() %>%
                          diri.est())$param

ok_dirichlet_params <- ok_dirichlet_params / max(1, min(ok_dirichlet_params))
ok_dirichlet_params[8] <- 0

ok_state_percentages <- (rdirichlet(n_sims, ok_dirichlet_params) %>%
                            as.data.frame() %>%
                            dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, 
                                          steyer = V6, warren = V7, yang = V8)) %>%
  apply(MARGIN = 1, FUN = function(x) x / sum(x)) %>%
  t() %>%
  as.data.frame() %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

ok_state_percentages_adj <- ok_state_percentages %>%
  left_join(overperformance_bounce, by = c("sim_id", "candidate")) %>%
  mutate(nh_place = nh_places,
         nv_place = nv_places,
         sc_place = sc_places,
         state_pct = pmax(state_pct + bounce + 0.01 * (3.5 - sc_place) + 0.01 * (3.5 - nv_place), 0)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct)) %>%
  dplyr::select(-nh_place, -nv_place, -nh_place) %>%
  left_join(sc_second_choice_division, by = c("candidate", "sim_id")) %>%
  melt(id.vars = c("sim_id", "candidate", "state_pct", "overperformance", "bounce_factor", "stay_in", "bounce"), 
       variable.name = "second_choice", value.name = "pct") %>%
  mutate(pct = pct*state_pct*(!stay_in),
         state_pct = case_when(candidate == second_choice ~ state_pct,
                               candidate != second_choice ~ pct)) %>%
  arrange(sim_id, second_choice, candidate)

overperformance_stats <- ok_state_percentages_adj %>%
  group_by(sim_id, candidate) %>%
  summarise(overperformance = mean(overperformance),
            bounce_factor = mean(bounce_factor))

ok_state_percentages_adj <- ok_state_percentages_adj %>%
  group_by(sim_id, second_choice) %>%
  summarise(state_pct = sum(state_pct)) %>%
  dplyr::select(everything(), candidate = second_choice) %>%
  left_join(sc_stayin_matrix %>% melt(id.vars = NULL, value.name = "stay_in") %>%
              dplyr::select(sim_id = Var1, candidate = Var2, stay_in), by = c("sim_id", "candidate")) %>%
  mutate(state_pct = state_pct * stay_in) %>%
  left_join(overperformance_stats, by = c("sim_id", "candidate"))

ok_sims <- vector("list", n_sims)

ok_sims <- foreach(i = 1:n_sims, .packages = c("Hmisc", "dplyr", "reshape2", "tidyr")) %dopar% {
  demographic_lean_sim <- demographic_leans %>%
    mutate(lean = rnorm(n(), mean = avg_lean, sd = lean_se)) %>%
    dplyr::select(candidate, demographic_var, demographic, lean)
  
  # This simulation's national percentages
  this_sim_state_percentage <- ok_state_percentages_adj %>%
    filter(sim_id == i)
  
  # Apply simulated leans to state average
  state_demographic_sim <- demoCandidateBreakdown("Oklahoma", this_sim_state_percentage, demographic_lean_sim, 
                                                  state_marginal_demographics)
  
  # Turn these into probabilities
  gender_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("men", "women")) %>%
    dplyr::select(candidate, gender = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(gender = case_when(gender == "men" ~ "Male",
                              gender == "women" ~ "Female"))
  
  names(gender_breakdown)[-1] <- paste0("gender_", names(gender_breakdown))[-1]
  
  race_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("white", "black", "latino")) %>%
    dplyr::select(candidate, race = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(race = case_when(race == "white" ~ "White",
                            race == "black" ~ "Black",
                            race == "latino" ~ "Latino"))
  
  names(race_breakdown)[-1] <- paste0("race_", names(race_breakdown))[-1]
  
  age_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("age_1829", "age_3044", "age_4564", "age_65")) %>%
    dplyr::select(candidate, age = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(age = case_when(age == "age_1829" ~ "18-29",
                           age == "age_3044" ~ "30-44",
                           age == "age_4564" ~ "45-64",
                           age == "age_65" ~ "65+"))
  
  names(age_breakdown)[-1] <- paste0("age_", names(age_breakdown)[-1])
  
  education_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("college", "no_college")) %>%
    dplyr::select(candidate, education = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(education = case_when(education == "college" ~ "College",
                                 education == "no_college" ~ "Less than college"))
  
  names(education_breakdown)[-1] <- paste0("education_", names(education_breakdown)[-1])
  
  synth_pop <- sim_demographics %>%
    mutate(rand = runif(n()),
           age_regroup = case_when(age == "18-24" ~ "18-29",
                                   age == "25-34" & rand < 0.5 ~ "18-29",
                                   age == "25-34" & rand >= 0.5 ~ "30-44",
                                   age == "35-44" ~ "30-44",
                                   age == "45-64" ~ "45-64",
                                   age == "65+" ~ "65+"),
           id = 1:n()) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age = age_regroup, education) %>%
    mutate_if(is.factor, as.character) %>%
    left_join(gender_breakdown, by = "gender") %>%
    left_join(race_breakdown, by = "race") %>%
    left_join(age_breakdown, by = "age") %>%
    left_join(education_breakdown, by = "education") %>%
    mutate(biden_prob = gender_biden + race_biden + age_biden + education_biden,
           bloomberg_prob = gender_bloomberg + race_bloomberg + age_bloomberg + education_bloomberg,
           buttigieg_prob = gender_buttigieg + race_buttigieg + age_buttigieg + education_buttigieg,
           klobuchar_prob = gender_klobuchar + race_klobuchar + age_klobuchar + education_klobuchar,
           sanders_prob = gender_sanders + race_sanders + age_sanders + education_sanders,
           steyer_prob = gender_steyer + race_steyer + age_steyer + education_steyer,
           warren_prob = gender_warren + race_warren + age_warren + education_warren,
           yang_prob = gender_yang + race_yang + age_yang + education_yang) %>%
    mutate_at(vars(c("biden_prob", "bloomberg_prob", "buttigieg_prob", "klobuchar_prob", "sanders_prob", "steyer_prob", "warren_prob", 
                     "yang_prob")),
              function(x) pmax(x/100, 0)) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age, education, biden_prob, 
                  bloomberg_prob, buttigieg_prob, klobuchar_prob, sanders_prob, steyer_prob, warren_prob, yang_prob) %>%
    melt(id.vars = c("id", "state", "district", "primary_type", "district_delegates", "gender", "race", "age", "education"),
         variable.name = "candidate_prob", value.name = "prob") %>%
    arrange(id, candidate_prob) %>%
    group_by(id) %>%
    mutate(prob = prob / sum(prob),
           cum_prob = cumsum(prob)) %>%
    dplyr::select(-prob) %>%
    ungroup() %>%
    spread(candidate_prob, cum_prob) %>%
    mutate(rand = runif(n()),
           vote = case_when(rand <= biden_prob ~ "biden",
                            rand > biden_prob & rand <= bloomberg_prob ~ "bloomberg",
                            rand > bloomberg_prob & rand <= buttigieg_prob ~ "buttigieg",
                            rand > buttigieg_prob & rand <= klobuchar_prob ~ "klobuchar",
                            rand > klobuchar_prob & rand <= sanders_prob ~ "sanders",
                            rand > sanders_prob & rand <= steyer_prob ~ "steyer",
                            rand > steyer_prob & rand <= warren_prob ~ "warren",
                            rand > warren_prob ~ "yang")) %>%
    as.tbl()
  
  synth_district_pop <- synth_pop %>%
    group_by(state, district) %>%
    summarise(district_pop = n())
  
  district_sim_delegates <- synth_pop %>%
    group_by(state, district) %>%
    mutate(district_total = n()) %>%
    group_by(state, district, vote) %>%
    summarise(pct = n() / mean(district_total)) %>%
    ungroup() %>%
    mutate(met_threshold = pct >= 0.15) %>% 
    group_by(state, district) %>%
    mutate(pct_valid = (pct*met_threshold) / sum(pct*met_threshold)) %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(candidate_delegates = delegates * pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, district, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate = vote, pct, pct_valid, candidate_delegates) %>%
    arrange(district, candidate)
  
  state_sim_delegates <- district_sim_delegates %>%
    left_join(synth_district_pop, by = c("state", "district")) %>%
    mutate(synth_votes = pct * district_pop) %>%
    group_by(state) %>%
    mutate(state_vote = sum(synth_votes)) %>%
    group_by(state, candidate) %>%
    summarise(state_pct = sum(synth_votes) / mean(state_vote)) %>%
    mutate(district = "At-large") %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(met_threshold = state_pct >= 0.15,
           state_pct_valid = (state_pct*met_threshold) / sum(state_pct*met_threshold),
           candidate_delegates = delegates * state_pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate, pct = state_pct, pct_valid = state_pct_valid, candidate_delegates) %>%
    arrange(district, candidate) %>%
    bind_rows(district_sim_delegates) %>%
    mutate(sim_id = i)
}

# How much did the candidates overperform in Oklahoma?
ok_overperformance <- ok_state_percentages_adj %>%
  left_join(state_averages %>% filter(state == "Oklahoma") %>% dplyr::select(candidate, state_avg), by = c("candidate")) %>%
  mutate(overperformance = state_pct - (state_avg/100 + 0.25*overperformance*bounce_factor)) %>%
  dplyr::select(sim_id, candidate, ok_overperformance = overperformance) 


# Tennessee ####
tn_start_time <- Sys.time()
# Draw votes
sim_demographics_table <- district_demographics %>%
  filter(state == "Tennessee") %>%
  mutate(prob = predict(dem_primary_logit, newdata = ., type = "response"),
         n_vote = round(pop * prob * turnout * 0.05)) %>%
  dplyr::select(-pop, -prob)

sim_demographics <- sim_demographics_table %>%
  lapply(rep, sim_demographics_table$n_vote) %>%
  as.data.frame() 

state_marginal_demographics <- shapeStateDemo("Tennessee")

tn_averages_over_time <- state_averages_over_time_unsmoothed %>%
  filter(state == "Tennessee", as.numeric(today() - median_date) <= 1) %>%
  mutate(age = as.numeric(today() - median_date),
         nreps = 2 - age)

tn_dirichlet_params <- (tn_averages_over_time %>%
                          lapply(rep, tn_averages_over_time$nreps) %>%
                          as.data.frame() %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          dplyr::select(-median_date) %>%
                          group_by(candidate) %>%
                          mutate(i = 1:n()) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-i) %>%
                          as.matrix() %>%
                          diri.est())$param

tn_dirichlet_params <- 0.5 * tn_dirichlet_params / max(1, min(tn_dirichlet_params))
tn_dirichlet_params[8] <- 0

tn_state_percentages <- (rdirichlet(n_sims, tn_dirichlet_params) %>%
                            as.data.frame() %>%
                            dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, 
                                          steyer = V6, warren = V7, yang = V8)) %>%
  apply(MARGIN = 1, FUN = function(x) x / sum(x)) %>%
  t() %>%
  as.data.frame() %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

tn_state_percentages_adj <- tn_state_percentages %>%
  left_join(overperformance_bounce, by = c("sim_id", "candidate")) %>%
  mutate(sc_place = sc_places,
         state_pct = pmax(state_pct + bounce + 0.02 * (3.5 - sc_place), 0)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct)) %>%
  dplyr::select(-sc_place) %>%
  left_join(sc_second_choice_division, by = c("candidate", "sim_id")) %>%
  melt(id.vars = c("sim_id", "candidate", "state_pct", "overperformance", "bounce_factor", "stay_in", "bounce"), 
       variable.name = "second_choice", value.name = "pct") %>%
  mutate(pct = pct*state_pct*(!stay_in),
         state_pct = case_when(candidate == second_choice ~ state_pct,
                               candidate != second_choice ~ pct)) %>%
  arrange(sim_id, second_choice, candidate)

overperformance_stats <- tn_state_percentages_adj %>%
  group_by(sim_id, candidate) %>%
  summarise(overperformance = mean(overperformance),
            bounce_factor = mean(bounce_factor))

tn_state_percentages_adj <- tn_state_percentages_adj %>%
  group_by(sim_id, second_choice) %>%
  summarise(state_pct = sum(state_pct)) %>%
  dplyr::select(everything(), candidate = second_choice) %>%
  left_join(sc_stayin_matrix %>% melt(id.vars = NULL, value.name = "stay_in") %>%
              dplyr::select(sim_id = Var1, candidate = Var2, stay_in), by = c("sim_id", "candidate")) %>%
  mutate(state_pct = state_pct * stay_in) %>%
  left_join(overperformance_stats, by = c("sim_id", "candidate"))

tn_sims <- vector("list", n_sims)

tn_sims <- foreach(i = 1:n_sims, .packages = c("Hmisc", "dplyr", "reshape2", "tidyr")) %dopar% {
  demographic_lean_sim <- demographic_leans %>%
    mutate(lean = rnorm(n(), mean = avg_lean, sd = lean_se)) %>%
    dplyr::select(candidate, demographic_var, demographic, lean)
  
  # This simulation's national percentages
  this_sim_state_percentage <- tn_state_percentages_adj %>%
    filter(sim_id == i)
  
  # Apply simulated leans to state average
  state_demographic_sim <- demoCandidateBreakdown("Tennessee", this_sim_state_percentage, demographic_lean_sim, 
                                                  state_marginal_demographics)
  
  # Turn these into probabilities
  gender_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("men", "women")) %>%
    dplyr::select(candidate, gender = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(gender = case_when(gender == "men" ~ "Male",
                              gender == "women" ~ "Female"))
  
  names(gender_breakdown)[-1] <- paste0("gender_", names(gender_breakdown))[-1]
  
  race_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("white", "black", "latino")) %>%
    dplyr::select(candidate, race = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(race = case_when(race == "white" ~ "White",
                            race == "black" ~ "Black",
                            race == "latino" ~ "Latino"))
  
  names(race_breakdown)[-1] <- paste0("race_", names(race_breakdown))[-1]
  
  age_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("age_1829", "age_3044", "age_4564", "age_65")) %>%
    dplyr::select(candidate, age = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(age = case_when(age == "age_1829" ~ "18-29",
                           age == "age_3044" ~ "30-44",
                           age == "age_4564" ~ "45-64",
                           age == "age_65" ~ "65+"))
  
  names(age_breakdown)[-1] <- paste0("age_", names(age_breakdown)[-1])
  
  education_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("college", "no_college")) %>%
    dplyr::select(candidate, education = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(education = case_when(education == "college" ~ "College",
                                 education == "no_college" ~ "Less than college"))
  
  names(education_breakdown)[-1] <- paste0("education_", names(education_breakdown)[-1])
  
  synth_pop <- sim_demographics %>%
    mutate(rand = runif(n()),
           age_regroup = case_when(age == "18-24" ~ "18-29",
                                   age == "25-34" & rand < 0.5 ~ "18-29",
                                   age == "25-34" & rand >= 0.5 ~ "30-44",
                                   age == "35-44" ~ "30-44",
                                   age == "45-64" ~ "45-64",
                                   age == "65+" ~ "65+"),
           id = 1:n()) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age = age_regroup, education) %>%
    mutate_if(is.factor, as.character) %>%
    left_join(gender_breakdown, by = "gender") %>%
    left_join(race_breakdown, by = "race") %>%
    left_join(age_breakdown, by = "age") %>%
    left_join(education_breakdown, by = "education") %>%
    mutate(biden_prob = gender_biden + race_biden + age_biden + education_biden,
           bloomberg_prob = gender_bloomberg + race_bloomberg + age_bloomberg + education_bloomberg,
           buttigieg_prob = gender_buttigieg + race_buttigieg + age_buttigieg + education_buttigieg,
           klobuchar_prob = gender_klobuchar + race_klobuchar + age_klobuchar + education_klobuchar,
           sanders_prob = gender_sanders + race_sanders + age_sanders + education_sanders,
           steyer_prob = gender_steyer + race_steyer + age_steyer + education_steyer,
           warren_prob = gender_warren + race_warren + age_warren + education_warren,
           yang_prob = gender_yang + race_yang + age_yang + education_yang) %>%
    mutate_at(vars(c("biden_prob", "bloomberg_prob", "buttigieg_prob", "klobuchar_prob", "sanders_prob", "steyer_prob", "warren_prob", 
                     "yang_prob")),
              function(x) pmax(x/100, 0)) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age, education, biden_prob, 
                  bloomberg_prob, buttigieg_prob, klobuchar_prob, sanders_prob, steyer_prob, warren_prob, yang_prob) %>%
    melt(id.vars = c("id", "state", "district", "primary_type", "district_delegates", "gender", "race", "age", "education"),
         variable.name = "candidate_prob", value.name = "prob") %>%
    arrange(id, candidate_prob) %>%
    group_by(id) %>%
    mutate(prob = prob / sum(prob),
           cum_prob = cumsum(prob)) %>%
    dplyr::select(-prob) %>%
    ungroup() %>%
    spread(candidate_prob, cum_prob) %>%
    mutate(rand = runif(n()),
           vote = case_when(rand <= biden_prob ~ "biden",
                            rand > biden_prob & rand <= bloomberg_prob ~ "bloomberg",
                            rand > bloomberg_prob & rand <= buttigieg_prob ~ "buttigieg",
                            rand > buttigieg_prob & rand <= klobuchar_prob ~ "klobuchar",
                            rand > klobuchar_prob & rand <= sanders_prob ~ "sanders",
                            rand > sanders_prob & rand <= steyer_prob ~ "steyer",
                            rand > steyer_prob & rand <= warren_prob ~ "warren",
                            rand > warren_prob ~ "yang")) %>%
    as.tbl()
  
  synth_district_pop <- synth_pop %>%
    group_by(state, district) %>%
    summarise(district_pop = n())
  
  district_sim_delegates <- synth_pop %>%
    group_by(state, district) %>%
    mutate(district_total = n()) %>%
    group_by(state, district, vote) %>%
    summarise(pct = n() / mean(district_total)) %>%
    ungroup() %>%
    mutate(met_threshold = pct >= 0.15) %>% 
    group_by(state, district) %>%
    mutate(pct_valid = (pct*met_threshold) / sum(pct*met_threshold)) %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(candidate_delegates = delegates * pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, district, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate = vote, pct, pct_valid, candidate_delegates) %>%
    arrange(district, candidate)
  
  state_sim_delegates <- district_sim_delegates %>%
    left_join(synth_district_pop, by = c("state", "district")) %>%
    mutate(synth_votes = pct * district_pop) %>%
    group_by(state) %>%
    mutate(state_vote = sum(synth_votes)) %>%
    group_by(state, candidate) %>%
    summarise(state_pct = sum(synth_votes) / mean(state_vote)) %>%
    mutate(district = "At-large") %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(met_threshold = state_pct >= 0.15,
           state_pct_valid = (state_pct*met_threshold) / sum(state_pct*met_threshold),
           candidate_delegates = delegates * state_pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate, pct = state_pct, pct_valid = state_pct_valid, candidate_delegates) %>%
    arrange(district, candidate) %>%
    bind_rows(district_sim_delegates) %>%
    mutate(sim_id = i)
}

# How much did the candidates overperform in Tennessee?
tn_overperformance <- tn_state_percentages_adj %>%
  left_join(state_averages %>% filter(state == "Tennessee") %>% dplyr::select(candidate, state_avg), by = c("candidate")) %>%
  mutate(overperformance = state_pct - (state_avg/100 + 0.25*overperformance*bounce_factor)) %>%
  dplyr::select(sim_id, candidate, tn_overperformance = overperformance) 


# Texas ####
tx_start_time <- Sys.time()
# Draw votes
sim_demographics_table <- district_demographics %>%
  filter(state == "Texas") %>%
  mutate(prob = predict(dem_primary_logit, newdata = ., type = "response"),
         n_vote = round(pop * prob * turnout * 0.03)) %>%
  dplyr::select(-pop, -prob)

sim_demographics <- sim_demographics_table %>%
  lapply(rep, sim_demographics_table$n_vote) %>%
  as.data.frame() 

state_marginal_demographics <- shapeStateDemo("Texas")

tx_averages_over_time <- state_averages_over_time_unsmoothed %>%
  filter(state == "Texas", as.numeric(today() - median_date) <= 1) %>%
  mutate(age = as.numeric(today() - median_date),
         nreps = 2 - age)

tx_dirichlet_params <- (tx_averages_over_time %>%
                          lapply(rep, tx_averages_over_time$nreps) %>%
                          as.data.frame() %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          dplyr::select(-median_date) %>%
                          group_by(candidate) %>%
                          mutate(i = 1:n()) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-i) %>%
                          as.matrix() %>%
                          diri.est())$param

tx_dirichlet_params <- 0.5 * tx_dirichlet_params / max(1, min(tx_dirichlet_params))
tx_dirichlet_params[8] <- 0

tx_state_percentages <- (rdirichlet(n_sims, tx_dirichlet_params) %>%
                            as.data.frame() %>%
                            dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, 
                                          steyer = V6, warren = V7, yang = V8)) %>%
  apply(MARGIN = 1, FUN = function(x) x / sum(x)) %>%
  t() %>%
  as.data.frame() %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

tx_state_percentages_adj <- tx_state_percentages %>%
  left_join(overperformance_bounce, by = c("sim_id", "candidate")) %>%
  mutate(nv_place = nv_places,
         sc_place = sc_places,
         state_pct = pmax(state_pct + bounce + 0.02 * (3.5 - sc_place) + 0.02 * (3.5 - nv_place), 0)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct)) %>%
  dplyr::select(-nv_place, -sc_place) %>%
  left_join(sc_second_choice_division, by = c("candidate", "sim_id")) %>%
  melt(id.vars = c("sim_id", "candidate", "state_pct", "overperformance", "bounce_factor", "stay_in", "bounce"), 
       variable.name = "second_choice", value.name = "pct") %>%
  mutate(pct = pct*state_pct*(!stay_in),
         state_pct = case_when(candidate == second_choice ~ state_pct,
                               candidate != second_choice ~ pct)) %>%
  arrange(sim_id, second_choice, candidate)

overperformance_stats <- tx_state_percentages_adj %>%
  group_by(sim_id, candidate) %>%
  summarise(overperformance = mean(overperformance),
            bounce_factor = mean(bounce_factor))

tx_state_percentages_adj <- tx_state_percentages_adj %>%
  group_by(sim_id, second_choice) %>%
  summarise(state_pct = sum(state_pct)) %>%
  dplyr::select(everything(), candidate = second_choice) %>%
  left_join(sc_stayin_matrix %>% melt(id.vars = NULL, value.name = "stay_in") %>%
              dplyr::select(sim_id = Var1, candidate = Var2, stay_in), by = c("sim_id", "candidate")) %>%
  mutate(state_pct = state_pct * stay_in) %>%
  left_join(overperformance_stats, by = c("sim_id", "candidate"))

tx_sims <- vector("list", n_sims)

tx_sims <- foreach(i = 1:n_sims, .packages = c("Hmisc", "dplyr", "reshape2", "tidyr")) %dopar% {
  demographic_lean_sim <- demographic_leans %>%
    mutate(lean = rnorm(n(), mean = avg_lean, sd = lean_se)) %>%
    dplyr::select(candidate, demographic_var, demographic, lean)
  
  # This simulation's national percentages
  this_sim_state_percentage <- tx_state_percentages_adj %>%
    filter(sim_id == i)
  
  # Apply simulated leans to state average
  state_demographic_sim <- demoCandidateBreakdown("Texas", this_sim_state_percentage, demographic_lean_sim, 
                                                  state_marginal_demographics)
  
  # Turn these into probabilities
  gender_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("men", "women")) %>%
    dplyr::select(candidate, gender = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(gender = case_when(gender == "men" ~ "Male",
                              gender == "women" ~ "Female"))
  
  names(gender_breakdown)[-1] <- paste0("gender_", names(gender_breakdown))[-1]
  
  race_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("white", "black", "latino")) %>%
    dplyr::select(candidate, race = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(race = case_when(race == "white" ~ "White",
                            race == "black" ~ "Black",
                            race == "latino" ~ "Latino"))
  
  names(race_breakdown)[-1] <- paste0("race_", names(race_breakdown))[-1]
  
  age_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("age_1829", "age_3044", "age_4564", "age_65")) %>%
    dplyr::select(candidate, age = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(age = case_when(age == "age_1829" ~ "18-29",
                           age == "age_3044" ~ "30-44",
                           age == "age_4564" ~ "45-64",
                           age == "age_65" ~ "65+"))
  
  names(age_breakdown)[-1] <- paste0("age_", names(age_breakdown)[-1])
  
  education_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("college", "no_college")) %>%
    dplyr::select(candidate, education = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(education = case_when(education == "college" ~ "College",
                                 education == "no_college" ~ "Less than college"))
  
  names(education_breakdown)[-1] <- paste0("education_", names(education_breakdown)[-1])
  
  synth_pop <- sim_demographics %>%
    mutate(rand = runif(n()),
           age_regroup = case_when(age == "18-24" ~ "18-29",
                                   age == "25-34" & rand < 0.5 ~ "18-29",
                                   age == "25-34" & rand >= 0.5 ~ "30-44",
                                   age == "35-44" ~ "30-44",
                                   age == "45-64" ~ "45-64",
                                   age == "65+" ~ "65+"),
           id = 1:n()) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age = age_regroup, education) %>%
    mutate_if(is.factor, as.character) %>%
    left_join(gender_breakdown, by = "gender") %>%
    left_join(race_breakdown, by = "race") %>%
    left_join(age_breakdown, by = "age") %>%
    left_join(education_breakdown, by = "education") %>%
    mutate(biden_prob = gender_biden + race_biden + age_biden + education_biden,
           bloomberg_prob = gender_bloomberg + race_bloomberg + age_bloomberg + education_bloomberg,
           buttigieg_prob = gender_buttigieg + race_buttigieg + age_buttigieg + education_buttigieg,
           klobuchar_prob = gender_klobuchar + race_klobuchar + age_klobuchar + education_klobuchar,
           sanders_prob = gender_sanders + race_sanders + age_sanders + education_sanders,
           steyer_prob = gender_steyer + race_steyer + age_steyer + education_steyer,
           warren_prob = gender_warren + race_warren + age_warren + education_warren,
           yang_prob = gender_yang + race_yang + age_yang + education_yang) %>%
    mutate_at(vars(c("biden_prob", "bloomberg_prob", "buttigieg_prob", "klobuchar_prob", "sanders_prob", "steyer_prob", "warren_prob", 
                     "yang_prob")),
              function(x) pmax(x/100, 0)) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age, education, biden_prob, 
                  bloomberg_prob, buttigieg_prob, klobuchar_prob, sanders_prob, steyer_prob, warren_prob, yang_prob) %>%
    melt(id.vars = c("id", "state", "district", "primary_type", "district_delegates", "gender", "race", "age", "education"),
         variable.name = "candidate_prob", value.name = "prob") %>%
    arrange(id, candidate_prob) %>%
    group_by(id) %>%
    mutate(prob = prob / sum(prob),
           cum_prob = cumsum(prob)) %>%
    dplyr::select(-prob) %>%
    ungroup() %>%
    spread(candidate_prob, cum_prob) %>%
    mutate(rand = runif(n()),
           vote = case_when(rand <= biden_prob ~ "biden",
                            rand > biden_prob & rand <= bloomberg_prob ~ "bloomberg",
                            rand > bloomberg_prob & rand <= buttigieg_prob ~ "buttigieg",
                            rand > buttigieg_prob & rand <= klobuchar_prob ~ "klobuchar",
                            rand > klobuchar_prob & rand <= sanders_prob ~ "sanders",
                            rand > sanders_prob & rand <= steyer_prob ~ "steyer",
                            rand > steyer_prob & rand <= warren_prob ~ "warren",
                            rand > warren_prob ~ "yang")) %>%
    as.tbl()
  
  synth_district_pop <- synth_pop %>%
    group_by(state, district) %>%
    summarise(district_pop = n())
  
  district_sim_delegates <- synth_pop %>%
    group_by(state, district) %>%
    mutate(district_total = n()) %>%
    group_by(state, district, vote) %>%
    summarise(pct = n() / mean(district_total)) %>%
    ungroup() %>%
    mutate(met_threshold = pct >= 0.15) %>% 
    group_by(state, district) %>%
    mutate(pct_valid = (pct*met_threshold) / sum(pct*met_threshold)) %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(candidate_delegates = delegates * pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, district, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate = vote, pct, pct_valid, candidate_delegates) %>%
    arrange(district, candidate)
  
  state_sim_delegates <- district_sim_delegates %>%
    left_join(synth_district_pop, by = c("state", "district")) %>%
    mutate(synth_votes = pct * district_pop) %>%
    group_by(state) %>%
    mutate(state_vote = sum(synth_votes)) %>%
    group_by(state, candidate) %>%
    summarise(state_pct = sum(synth_votes) / mean(state_vote)) %>%
    mutate(district = "At-large") %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(met_threshold = state_pct >= 0.15,
           state_pct_valid = (state_pct*met_threshold) / sum(state_pct*met_threshold),
           candidate_delegates = delegates * state_pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate, pct = state_pct, pct_valid = state_pct_valid, candidate_delegates) %>%
    arrange(district, candidate) %>%
    bind_rows(district_sim_delegates) %>%
    mutate(sim_id = i)
}

 # How much did the candidates overperform in Texas?
tx_overperformance <- tx_state_percentages_adj %>%
  left_join(state_averages %>% filter(state == "Texas") %>% dplyr::select(candidate, state_avg), by = c("candidate")) %>%
  mutate(overperformance = state_pct - (state_avg/100 + 0.25*overperformance*bounce_factor)) %>%
  dplyr::select(sim_id, candidate, tx_overperformance = overperformance) 


# Utah ####
ut_start_time <- Sys.time()
# Draw votes
sim_demographics_table <- district_demographics %>%
  filter(state == "Utah") %>%
  mutate(prob = predict(dem_primary_logit, newdata = ., type = "response"),
         n_vote = round(pop * prob * turnout * 0.03)) %>%
  dplyr::select(-pop, -prob)

sim_demographics <- sim_demographics_table %>%
  lapply(rep, sim_demographics_table$n_vote) %>%
  as.data.frame() 

state_marginal_demographics <- shapeStateDemo("Utah")

ut_averages_over_time <- state_averages_over_time_unsmoothed %>%
  filter(state == "Utah", as.numeric(today() - median_date) <= 1) %>%
  mutate(age = as.numeric(today() - median_date),
         nreps = 2 - age)

ut_dirichlet_params <- (ut_averages_over_time %>%
                          lapply(rep, ut_averages_over_time$nreps) %>%
                          as.data.frame() %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          dplyr::select(-median_date) %>%
                          group_by(candidate) %>%
                          mutate(i = 1:n()) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-i) %>%
                          as.matrix() %>%
                          diri.est())$param

ut_dirichlet_params <- ut_dirichlet_params / 2
ut_dirichlet_params[8] <- 0

ut_state_percentages <- (rdirichlet(n_sims, ut_dirichlet_params) %>%
                            as.data.frame() %>%
                            dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, 
                                          steyer = V6, warren = V7, yang = V8)) %>%
  apply(MARGIN = 1, FUN = function(x) x / sum(x)) %>%
  t() %>%
  as.data.frame() %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

ut_state_percentages_adj <- ut_state_percentages %>%
  left_join(overperformance_bounce, by = c("sim_id", "candidate")) %>%
  mutate(nv_place = nv_places,
         state_pct = pmax(state_pct + bounce + 0.01 * (3.5 - nv_place), 0)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct)) %>%
  dplyr::select(-nv_place) %>%
  left_join(sc_second_choice_division, by = c("candidate", "sim_id")) %>%
  melt(id.vars = c("sim_id", "candidate", "state_pct", "overperformance", "bounce_factor", "stay_in", "bounce"), 
       variable.name = "second_choice", value.name = "pct") %>%
  mutate(pct = pct*state_pct*(!stay_in),
         state_pct = case_when(candidate == second_choice ~ state_pct,
                               candidate != second_choice ~ pct)) %>%
  arrange(sim_id, second_choice, candidate)

overperformance_stats <- ut_state_percentages_adj %>%
  group_by(sim_id, candidate) %>%
  summarise(overperformance = mean(overperformance),
            bounce_factor = mean(bounce_factor))

ut_state_percentages_adj <- ut_state_percentages_adj %>%
  group_by(sim_id, second_choice) %>%
  summarise(state_pct = sum(state_pct)) %>%
  dplyr::select(everything(), candidate = second_choice) %>%
  left_join(sc_stayin_matrix %>% melt(id.vars = NULL, value.name = "stay_in") %>%
              dplyr::select(sim_id = Var1, candidate = Var2, stay_in), by = c("sim_id", "candidate")) %>%
  mutate(state_pct = state_pct * stay_in) %>%
  left_join(overperformance_stats, by = c("sim_id", "candidate"))

ut_sims <- vector("list", n_sims)

ut_sims <- foreach(i = 1:n_sims, .packages = c("Hmisc", "dplyr", "reshape2", "tidyr")) %dopar% {
  demographic_lean_sim <- demographic_leans %>%
    mutate(lean = rnorm(n(), mean = avg_lean, sd = lean_se)) %>%
    dplyr::select(candidate, demographic_var, demographic, lean)
  
  # This simulation's national percentages
  this_sim_state_percentage <- ut_state_percentages_adj %>%
    filter(sim_id == i)
  
  # Apply simulated leans to state average
  state_demographic_sim <- demoCandidateBreakdown("Utah", this_sim_state_percentage, demographic_lean_sim, 
                                                  state_marginal_demographics)
  
  # Turn these into probabilities
  gender_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("men", "women")) %>%
    dplyr::select(candidate, gender = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(gender = case_when(gender == "men" ~ "Male",
                              gender == "women" ~ "Female"))
  
  names(gender_breakdown)[-1] <- paste0("gender_", names(gender_breakdown))[-1]
  
  race_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("white", "black", "latino")) %>%
    dplyr::select(candidate, race = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(race = case_when(race == "white" ~ "White",
                            race == "black" ~ "Black",
                            race == "latino" ~ "Latino"))
  
  names(race_breakdown)[-1] <- paste0("race_", names(race_breakdown))[-1]
  
  age_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("age_1829", "age_3044", "age_4564", "age_65")) %>%
    dplyr::select(candidate, age = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(age = case_when(age == "age_1829" ~ "18-29",
                           age == "age_3044" ~ "30-44",
                           age == "age_4564" ~ "45-64",
                           age == "age_65" ~ "65+"))
  
  names(age_breakdown)[-1] <- paste0("age_", names(age_breakdown)[-1])
  
  education_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("college", "no_college")) %>%
    dplyr::select(candidate, education = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(education = case_when(education == "college" ~ "College",
                                 education == "no_college" ~ "Less than college"))
  
  names(education_breakdown)[-1] <- paste0("education_", names(education_breakdown)[-1])
  
  synth_pop <- sim_demographics %>%
    mutate(rand = runif(n()),
           age_regroup = case_when(age == "18-24" ~ "18-29",
                                   age == "25-34" & rand < 0.5 ~ "18-29",
                                   age == "25-34" & rand >= 0.5 ~ "30-44",
                                   age == "35-44" ~ "30-44",
                                   age == "45-64" ~ "45-64",
                                   age == "65+" ~ "65+"),
           id = 1:n()) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age = age_regroup, education) %>%
    mutate_if(is.factor, as.character) %>%
    left_join(gender_breakdown, by = "gender") %>%
    left_join(race_breakdown, by = "race") %>%
    left_join(age_breakdown, by = "age") %>%
    left_join(education_breakdown, by = "education") %>%
    mutate(biden_prob = gender_biden + race_biden + age_biden + education_biden,
           bloomberg_prob = gender_bloomberg + race_bloomberg + age_bloomberg + education_bloomberg,
           buttigieg_prob = gender_buttigieg + race_buttigieg + age_buttigieg + education_buttigieg,
           klobuchar_prob = gender_klobuchar + race_klobuchar + age_klobuchar + education_klobuchar,
           sanders_prob = gender_sanders + race_sanders + age_sanders + education_sanders,
           steyer_prob = gender_steyer + race_steyer + age_steyer + education_steyer,
           warren_prob = gender_warren + race_warren + age_warren + education_warren,
           yang_prob = gender_yang + race_yang + age_yang + education_yang) %>%
    mutate_at(vars(c("biden_prob", "bloomberg_prob", "buttigieg_prob", "klobuchar_prob", "sanders_prob", "steyer_prob", "warren_prob", 
                     "yang_prob")),
              function(x) pmax(x/100, 0)) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age, education, biden_prob, 
                  bloomberg_prob, buttigieg_prob, klobuchar_prob, sanders_prob, steyer_prob, warren_prob, yang_prob) %>%
    melt(id.vars = c("id", "state", "district", "primary_type", "district_delegates", "gender", "race", "age", "education"),
         variable.name = "candidate_prob", value.name = "prob") %>%
    arrange(id, candidate_prob) %>%
    group_by(id) %>%
    mutate(prob = prob / sum(prob),
           cum_prob = cumsum(prob)) %>%
    dplyr::select(-prob) %>%
    ungroup() %>%
    spread(candidate_prob, cum_prob) %>%
    mutate(rand = runif(n()),
           vote = case_when(rand <= biden_prob ~ "biden",
                            rand > biden_prob & rand <= bloomberg_prob ~ "bloomberg",
                            rand > bloomberg_prob & rand <= buttigieg_prob ~ "buttigieg",
                            rand > buttigieg_prob & rand <= klobuchar_prob ~ "klobuchar",
                            rand > klobuchar_prob & rand <= sanders_prob ~ "sanders",
                            rand > sanders_prob & rand <= steyer_prob ~ "steyer",
                            rand > steyer_prob & rand <= warren_prob ~ "warren",
                            rand > warren_prob ~ "yang")) %>%
    as.tbl()
  
  synth_district_pop <- synth_pop %>%
    group_by(state, district) %>%
    summarise(district_pop = n())
  
  district_sim_delegates <- synth_pop %>%
    group_by(state, district) %>%
    mutate(district_total = n()) %>%
    group_by(state, district, vote) %>%
    summarise(pct = n() / mean(district_total)) %>%
    ungroup() %>%
    mutate(met_threshold = pct >= 0.15) %>% 
    group_by(state, district) %>%
    mutate(pct_valid = (pct*met_threshold) / sum(pct*met_threshold)) %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(candidate_delegates = delegates * pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, district, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate = vote, pct, pct_valid, candidate_delegates) %>%
    arrange(district, candidate)
  
  state_sim_delegates <- district_sim_delegates %>%
    left_join(synth_district_pop, by = c("state", "district")) %>%
    mutate(synth_votes = pct * district_pop) %>%
    group_by(state) %>%
    mutate(state_vote = sum(synth_votes)) %>%
    group_by(state, candidate) %>%
    summarise(state_pct = sum(synth_votes) / mean(state_vote)) %>%
    mutate(district = "At-large") %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(met_threshold = state_pct >= 0.15,
           state_pct_valid = (state_pct*met_threshold) / sum(state_pct*met_threshold),
           candidate_delegates = delegates * state_pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate, pct = state_pct, pct_valid = state_pct_valid, candidate_delegates) %>%
    arrange(district, candidate) %>%
    bind_rows(district_sim_delegates) %>%
    mutate(sim_id = i)
}

# How much did the candidates overperform in Utah?
ut_overperformance <- ut_state_percentages_adj %>%
  left_join(state_averages %>% filter(state == "Utah") %>% dplyr::select(candidate, state_avg), by = c("candidate")) %>%
  mutate(overperformance = state_pct - (state_avg/100 + 0.25*overperformance*bounce_factor)) %>%
  dplyr::select(sim_id, candidate, ut_overperformance = overperformance) 


# Vermont ####
vt_start_time <- Sys.time()
# Use nearby states (Maine, New Hampshire) but include a Bernie Bounce
sim_demographics_table <- district_demographics %>%
  filter(state == "Vermont") %>%
  mutate(prob = predict(dem_primary_logit, newdata = ., type = "response"),
         n_vote = round(pop * prob * turnout * 0.05)) %>%
  dplyr::select(-pop, -prob)

sim_demographics <- sim_demographics_table %>%
  lapply(rep, sim_demographics_table$n_vote) %>%
  as.data.frame() 
state_marginal_demographics <- shapeStateDemo("Vermont")

vt_averages_over_time <- state_averages_over_time_unsmoothed %>%
  filter(state %in% c("Maine", "New Hampshire"), as.numeric(today() - median_date) <= 1) %>%
  mutate(age = as.numeric(today() - median_date),
         nreps = 2 - age,
         inv_distance = case_when(state == "Maine" ~ 0.499,
                                  state == "New Hampshire" ~ 0.361)) %>%
  group_by(candidate, median_date, age, nreps) %>%
  summarise(pct = wtd.mean(pct, inv_distance)) 

vt_dirichlet_params <- ((vt_averages_over_time %>%
                           lapply(rep, vt_averages_over_time$nreps) %>%
                           as.data.frame() %>%
                           dplyr::select(candidate, median_date, pct) %>%
                           na.omit() %>%
                           dplyr::select(-median_date) %>%
                           group_by(candidate) %>%
                           mutate(i = 1:n()) %>%
                           spread(candidate, pct) %>%
                           dplyr::select(-i) %>%
                           as.matrix() %>%
                           diri.est())$param)

vt_dirichlet_params <- vt_dirichlet_params / min(vt_dirichlet_params)
vt_dirichlet_params[5] <- vt_dirichlet_params[5] * 3
vt_dirichlet_params[8] <- 0

vt_state_percentages <- (rdirichlet(n_sims, vt_dirichlet_params) %>%
                            as.data.frame() %>%
                            dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, 
                                          steyer = V6, warren = V7, yang = V8)) %>%
  apply(MARGIN = 1, FUN = function(x) x / sum(x)) %>%
  t() %>%
  as.data.frame() %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

vt_state_percentages_adj <- vt_state_percentages %>%
  left_join(overperformance_bounce, by = c("sim_id", "candidate")) %>%
  mutate(nh_place = nh_places,
         state_pct = pmax(state_pct + bounce + 0.03 * (3.5 - nh_place), 0)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct)) %>%
  dplyr::select(-nh_place) %>%
  left_join(sc_second_choice_division, by = c("candidate", "sim_id")) %>%
  melt(id.vars = c("sim_id", "candidate", "state_pct", "overperformance", "bounce_factor", "stay_in", "bounce"), 
       variable.name = "second_choice", value.name = "pct") %>%
  mutate(pct = pct*state_pct*(!stay_in),
         state_pct = case_when(candidate == second_choice ~ state_pct,
                               candidate != second_choice ~ pct)) %>%
  arrange(sim_id, second_choice, candidate)

overperformance_stats <- vt_state_percentages_adj %>%
  group_by(sim_id, candidate) %>%
  summarise(overperformance = mean(overperformance),
            bounce_factor = mean(bounce_factor))

vt_state_percentages_adj <- vt_state_percentages_adj %>%
  group_by(sim_id, second_choice) %>%
  summarise(state_pct = sum(state_pct)) %>%
  dplyr::select(everything(), candidate = second_choice) %>%
  left_join(sc_stayin_matrix %>% melt(id.vars = NULL, value.name = "stay_in") %>%
              dplyr::select(sim_id = Var1, candidate = Var2, stay_in), by = c("sim_id", "candidate")) %>%
  mutate(state_pct = state_pct * stay_in) %>%
  left_join(overperformance_stats, by = c("sim_id", "candidate"))

vt_sims <- vector("list", n_sims)

### Simulate
vt_sims <- foreach(i = 1:n_sims, .packages = c("Hmisc", "dplyr", "reshape2", "tidyr")) %dopar% {
  this_sim_state_percentage <- vt_state_percentages_adj %>%
    filter(sim_id == i)
  
  district_sim_pct <- this_sim_state_percentage %>%
    mutate(met_threshold = state_pct >= 0.15)
  
  state_sim_delegates <- district_sim_pct %>%
    mutate(pct_valid = state_pct * met_threshold) %>% 
    mutate(pct_valid = pct_valid / sum(pct_valid),
           state = "Vermont",
           district = "At-large") %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(candidate_delegates = delegates * pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, district, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate, pct = state_pct, pct_valid, candidate_delegates, sim_id) %>%
    arrange(district, candidate) 
}

# Virginia ####
va_start_time <- Sys.time()
# Draw votes
sim_demographics_table <- district_demographics %>%
  filter(state == "Virginia") %>%
  mutate(prob = predict(dem_primary_logit, newdata = ., type = "response"),
         n_vote = round(pop * prob * turnout * 0.03)) %>%
  dplyr::select(-pop, -prob)

sim_demographics <- sim_demographics_table %>%
  lapply(rep, sim_demographics_table$n_vote) %>%
  as.data.frame() 

state_marginal_demographics <- shapeStateDemo("Virginia")

va_averages_over_time <- state_averages_over_time_unsmoothed %>%
  filter(state == "Virginia", as.numeric(today() - median_date) <= 1) %>%
  mutate(age = as.numeric(today() - median_date),
         nreps = 2 - age)

va_dirichlet_params <- (va_averages_over_time %>%
                          lapply(rep, va_averages_over_time$nreps) %>%
                          as.data.frame() %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          dplyr::select(-median_date) %>%
                          group_by(candidate) %>%
                          mutate(i = 1:n()) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-i) %>%
                          as.matrix() %>%
                          diri.est())$param

va_dirichlet_params <- va_dirichlet_params / 2
va_dirichlet_params[8] <- 0

va_state_percentages <- (rdirichlet(n_sims, va_dirichlet_params) %>%
                            as.data.frame() %>%
                            dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, 
                                          steyer = V6, warren = V7, yang = V8)) %>%
  apply(MARGIN = 1, FUN = function(x) x / sum(x)) %>%
  t() %>%
  as.data.frame() %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

va_state_percentages_adj <- va_state_percentages %>%
  left_join(overperformance_bounce, by = c("sim_id", "candidate")) %>%
  mutate(nv_place = nv_places,
         sc_place = sc_places,
         state_pct = pmax(state_pct + bounce + 0.01 * (3.5 - sc_place) + 0.01 * (3.5 - nv_place), 0)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct)) %>%
  left_join(sc_second_choice_division, by = c("candidate", "sim_id")) %>%
  melt(id.vars = c("sim_id", "candidate", "state_pct", "overperformance", "bounce_factor", "stay_in", "bounce"), 
       variable.name = "second_choice", value.name = "pct") %>%
  mutate(pct = pct*state_pct*(!stay_in),
         state_pct = case_when(candidate == second_choice ~ state_pct,
                               candidate != second_choice ~ pct)) %>%
  arrange(sim_id, second_choice, candidate)

overperformance_stats <- va_state_percentages_adj %>%
  group_by(sim_id, candidate) %>%
  summarise(overperformance = mean(overperformance),
            bounce_factor = mean(bounce_factor))

va_state_percentages_adj <- va_state_percentages_adj %>%
  group_by(sim_id, second_choice) %>%
  summarise(state_pct = sum(state_pct)) %>%
  dplyr::select(everything(), candidate = second_choice) %>%
  left_join(sc_stayin_matrix %>% melt(id.vars = NULL, value.name = "stay_in") %>%
              dplyr::select(sim_id = Var1, candidate = Var2, stay_in), by = c("sim_id", "candidate")) %>%
  mutate(state_pct = state_pct * stay_in) %>%
  left_join(overperformance_stats, by = c("sim_id", "candidate"))

va_sims <- vector("list", n_sims)

va_sims <- foreach(i = 1:n_sims, .packages = c("Hmisc", "dplyr", "reshape2", "tidyr")) %dopar% {
  demographic_lean_sim <- demographic_leans %>%
    mutate(lean = rnorm(n(), mean = avg_lean, sd = lean_se)) %>%
    dplyr::select(candidate, demographic_var, demographic, lean)
  
  # This simulation's national percentages
  this_sim_state_percentage <- va_state_percentages_adj %>%
    filter(sim_id == i)
  
  # Apply simulated leans to state average
  state_demographic_sim <- demoCandidateBreakdown("Virginia", this_sim_state_percentage, demographic_lean_sim, 
                                                  state_marginal_demographics)
  
  # Turn these into probabilities
  gender_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("men", "women")) %>%
    dplyr::select(candidate, gender = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(gender = case_when(gender == "men" ~ "Male",
                              gender == "women" ~ "Female"))
  
  names(gender_breakdown)[-1] <- paste0("gender_", names(gender_breakdown))[-1]
  
  race_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("white", "black", "latino")) %>%
    dplyr::select(candidate, race = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(race = case_when(race == "white" ~ "White",
                            race == "black" ~ "Black",
                            race == "latino" ~ "Latino"))
  
  names(race_breakdown)[-1] <- paste0("race_", names(race_breakdown))[-1]
  
  age_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("age_1829", "age_3044", "age_4564", "age_65")) %>%
    dplyr::select(candidate, age = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(age = case_when(age == "age_1829" ~ "18-29",
                           age == "age_3044" ~ "30-44",
                           age == "age_4564" ~ "45-64",
                           age == "age_65" ~ "65+"))
  
  names(age_breakdown)[-1] <- paste0("age_", names(age_breakdown)[-1])
  
  education_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("college", "no_college")) %>%
    dplyr::select(candidate, education = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(education = case_when(education == "college" ~ "College",
                                 education == "no_college" ~ "Less than college"))
  
  names(education_breakdown)[-1] <- paste0("education_", names(education_breakdown)[-1])
  
  synth_pop <- sim_demographics %>%
    mutate(rand = runif(n()),
           age_regroup = case_when(age == "18-24" ~ "18-29",
                                   age == "25-34" & rand < 0.5 ~ "18-29",
                                   age == "25-34" & rand >= 0.5 ~ "30-44",
                                   age == "35-44" ~ "30-44",
                                   age == "45-64" ~ "45-64",
                                   age == "65+" ~ "65+"),
           id = 1:n()) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age = age_regroup, education) %>%
    mutate_if(is.factor, as.character) %>%
    left_join(gender_breakdown, by = "gender") %>%
    left_join(race_breakdown, by = "race") %>%
    left_join(age_breakdown, by = "age") %>%
    left_join(education_breakdown, by = "education") %>%
    mutate(biden_prob = gender_biden + race_biden + age_biden + education_biden,
           bloomberg_prob = gender_bloomberg + race_bloomberg + age_bloomberg + education_bloomberg,
           buttigieg_prob = gender_buttigieg + race_buttigieg + age_buttigieg + education_buttigieg,
           klobuchar_prob = gender_klobuchar + race_klobuchar + age_klobuchar + education_klobuchar,
           sanders_prob = gender_sanders + race_sanders + age_sanders + education_sanders,
           steyer_prob = gender_steyer + race_steyer + age_steyer + education_steyer,
           warren_prob = gender_warren + race_warren + age_warren + education_warren,
           yang_prob = gender_yang + race_yang + age_yang + education_yang) %>%
    mutate_at(vars(c("biden_prob", "bloomberg_prob", "buttigieg_prob", "klobuchar_prob", "sanders_prob", "steyer_prob", "warren_prob", 
                     "yang_prob")),
              function(x) pmax(x/100, 0)) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age, education, biden_prob, 
                  bloomberg_prob, buttigieg_prob, klobuchar_prob, sanders_prob, steyer_prob, warren_prob, yang_prob) %>%
    melt(id.vars = c("id", "state", "district", "primary_type", "district_delegates", "gender", "race", "age", "education"),
         variable.name = "candidate_prob", value.name = "prob") %>%
    arrange(id, candidate_prob) %>%
    group_by(id) %>%
    mutate(prob = prob / sum(prob),
           cum_prob = cumsum(prob)) %>%
    dplyr::select(-prob) %>%
    ungroup() %>%
    spread(candidate_prob, cum_prob) %>%
    mutate(rand = runif(n()),
           vote = case_when(rand <= biden_prob ~ "biden",
                            rand > biden_prob & rand <= bloomberg_prob ~ "bloomberg",
                            rand > bloomberg_prob & rand <= buttigieg_prob ~ "buttigieg",
                            rand > buttigieg_prob & rand <= klobuchar_prob ~ "klobuchar",
                            rand > klobuchar_prob & rand <= sanders_prob ~ "sanders",
                            rand > sanders_prob & rand <= steyer_prob ~ "steyer",
                            rand > steyer_prob & rand <= warren_prob ~ "warren",
                            rand > warren_prob ~ "yang")) %>%
    as.tbl()
  
  synth_district_pop <- synth_pop %>%
    group_by(state, district) %>%
    summarise(district_pop = n())
  
  district_sim_delegates <- synth_pop %>%
    group_by(state, district) %>%
    mutate(district_total = n()) %>%
    group_by(state, district, vote) %>%
    summarise(pct = n() / mean(district_total)) %>%
    ungroup() %>%
    mutate(met_threshold = pct >= 0.15) %>% 
    group_by(state, district) %>%
    mutate(pct_valid = (pct*met_threshold) / sum(pct*met_threshold)) %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(candidate_delegates = delegates * pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, district, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate = vote, pct, pct_valid, candidate_delegates) %>%
    arrange(district, candidate)
  
  state_sim_delegates <- district_sim_delegates %>%
    left_join(synth_district_pop, by = c("state", "district")) %>%
    mutate(synth_votes = pct * district_pop) %>%
    group_by(state) %>%
    mutate(state_vote = sum(synth_votes)) %>%
    group_by(state, candidate) %>%
    summarise(state_pct = sum(synth_votes) / mean(state_vote)) %>%
    mutate(district = "At-large") %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(met_threshold = state_pct >= 0.15,
           state_pct_valid = (state_pct*met_threshold) / sum(state_pct*met_threshold),
           candidate_delegates = delegates * state_pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate, pct = state_pct, pct_valid = state_pct_valid, candidate_delegates) %>%
    arrange(district, candidate) %>%
    bind_rows(district_sim_delegates) %>%
    mutate(sim_id = i)
}

# How much did the candidates overperform in Virginia?
va_overperformance <- va_state_percentages_adj %>%
  left_join(state_averages %>% filter(state == "Virginia") %>% dplyr::select(candidate, state_avg), by = c("candidate")) %>%
  mutate(overperformance = state_pct - (state_avg/100 + 0.5*overperformance*bounce_factor)) %>%
  dplyr::select(sim_id, candidate, va_overperformance = overperformance) 


# Super Tuesday overperformances ####
st_overperformance <- al_overperformance %>%
  left_join(ca_overperformance, by = c("sim_id", "candidate")) %>%
  left_join(co_overperformance, by = c("sim_id", "candidate")) %>%
  left_join(ma_overperformance, by = c("sim_id", "candidate")) %>%
  left_join(mn_overperformance, by = c("sim_id", "candidate")) %>%
  left_join(ok_overperformance, by = c("sim_id", "candidate")) %>%
  left_join(tn_overperformance, by = c("sim_id", "candidate")) %>%
  left_join(tx_overperformance, by = c("sim_id", "candidate")) %>%
  left_join(ut_overperformance, by = c("sim_id", "candidate")) %>%
  left_join(va_overperformance, by = c("sim_id", "candidate")) %>%
  mutate(overperformance = ((7^2)*al_overperformance + (53^2)*ca_overperformance + (7^2)*co_overperformance + (9^2)*ma_overperformance +
           (8^2)*mn_overperformance + (5^2)*ok_overperformance + (9^2)*tn_overperformance + (36^2)*tx_overperformance + 
           (4^2)*ut_overperformance + (11^2)*va_overperformance) / (7^2 + 53^2 + 7^2 + 9^2 + 8^2 + 5^2 + 9^2 + 36^2 + 4^2 + 11^2)) %>%
  dplyr::select(sim_id, candidate, overperformance) %>%
  na.omit()

st_overperformance_matrix <- st_overperformance %>%
  spread(candidate, overperformance) %>%
  ungroup() %>%
  dplyr::select(-sim_id) %>%
  as.matrix()

# Super Tuesday dropouts ####
## What share of the delegates do candidates have?
super_tuesday_sims <- bind_rows(ia_sims, nh_sims, nv_sims, sc_sims, al_sims, ca_sims, co_sims, ma_sims, mn_sims, nc_sims, ok_sims, tn_sims,
                                tx_sims, ut_sims, va_sims)

delegate_percentage <-  super_tuesday_sims %>% 
  group_by(sim_id, candidate) %>% 
  summarise(delegates = sum(candidate_delegates)) %>% 
  mutate(delegate_pct = delegates / sum(delegates)) 

low_delegates_super_tuesday <- delegate_percentage %>%
  mutate(low_delegates = delegate_pct < 0.05) %>%
  dplyr::select(-delegates, -delegate_pct) %>%
  spread(candidate, low_delegates) %>%
  ungroup()

st_stayin_matrix <- (!(low_delegates_super_tuesday %>%
  dplyr::select(-sim_id) %>%
  as.matrix())) | (st_overperformance_matrix >= 0.05) & 
  (st_overperformance_matrix > -0.15) & sc_stayin_matrix

massachusetts_winner <- ma_sims %>%
  sapply(function(df) {
    df %>% filter(district == "At-large") %>% filter(pct == max(pct)) %>% pull(candidate)
  })

minnesota_winner <- mn_sims %>%
  sapply(function(df) {
    df %>% filter(district == "At-large") %>% filter(pct == max(pct)) %>% pull(candidate)
  })

vermont_winner <- vt_sims %>%
  sapply(function(df) {
    df %>% filter(district == "At-large") %>% filter(pct == max(pct)) %>% pull(candidate)
  })

st_stayin_matrix[, "warren"] <- st_stayin_matrix[, "warren"] & (massachusetts_winner == "warren")
st_stayin_matrix[, "klobuchar"] <- st_stayin_matrix[, "klobuchar"] & (minnesota_winner == "klobuchar")
st_stayin_matrix[, "sanders"] <- st_stayin_matrix[, "sanders"] & (vermont_winner == "sanders")

st_second_choice_division <- st_stayin_matrix %>%
  as.data.frame() %>%
  melt(id.vars = NULL, variable.name = "candidate", value.name = "stay_in") %>%
  mutate(candidate = as.character(candidate),
         sim_id = rep(1:n_sims, 8)) %>%
  as.tbl() %>%
  left_join(second_choice, by = c("candidate" = "first_choice"))


# Maine (8 Mar 2020) ####
## Super Tuesday bounce--apply it to Maine
bounce_factor <- tibble(sim_id = 1:n_sims, bounce_factor = rbeta(n_sims, shape1 = 9, shape2 = 2))

overperformance_bounce <- st_overperformance %>%
  left_join(bounce_factor, by = "sim_id") %>%
  mutate(bounce = overperformance * bounce_factor)

me_start_time <- Sys.time()
# Draw votes
sim_demographics_table <- district_demographics %>%
  filter(state == "Maine") %>%
  mutate(prob = predict(dem_primary_logit, newdata = ., type = "response"),
         n_vote = round(pop * prob * turnout * 0.05)) %>%
  dplyr::select(-pop, -prob)

sim_demographics <- sim_demographics_table %>%
  lapply(rep, sim_demographics_table$n_vote) %>%
  as.data.frame() 

state_marginal_demographics <- shapeStateDemo("Maine")

me_averages_over_time <- state_averages_over_time_unsmoothed %>%
  filter(state == "Maine", as.numeric(today() - median_date) <= 1) %>%
  mutate(age = as.numeric(today() - median_date),
         nreps = 2 - age)

me_dirichlet_params <- (me_averages_over_time %>%
                          lapply(rep, me_averages_over_time$nreps) %>%
                          as.data.frame() %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          dplyr::select(-median_date) %>%
                          group_by(candidate) %>%
                          mutate(i = 1:n()) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-i) %>%
                          as.matrix() %>%
                          diri.est())$param

me_dirichlet_params <- me_dirichlet_params / 2
me_dirichlet_params[8] <- 0

me_state_percentages <- (rdirichlet(n_sims, me_dirichlet_params) %>%
                            as.data.frame() %>%
                            dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, steyer = V6, 
                                          warren = V7, yang = V8)) %>%
  apply(MARGIN = 1, FUN = function(x) x / sum(x)) %>%
  t() %>%
  as.data.frame() %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

me_state_percentages_adj <- me_state_percentages %>%
  left_join(overperformance_bounce, by = c("sim_id", "candidate")) %>%
  mutate(state_pct = pmax(state_pct + bounce, 0)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct)) %>%
  left_join(st_second_choice_division, by = c("candidate", "sim_id")) %>%
  melt(id.vars = c("sim_id", "candidate", "state_pct", "overperformance", "bounce_factor", "stay_in", "bounce"), 
       variable.name = "second_choice", value.name = "pct") %>%
  mutate(pct = pct*state_pct*(!stay_in),
         state_pct = case_when(candidate == second_choice ~ state_pct,
                               candidate != second_choice ~ pct)) %>%
  arrange(sim_id, second_choice, candidate)

overperformance_stats <- me_state_percentages_adj %>%
  group_by(sim_id, candidate) %>%
  summarise(overperformance = mean(overperformance),
            bounce_factor = mean(bounce_factor))

me_state_percentages_adj <- me_state_percentages_adj %>%
  group_by(sim_id, second_choice) %>%
  summarise(state_pct = sum(state_pct)) %>%
  dplyr::select(everything(), candidate = second_choice) %>%
  left_join(st_stayin_matrix %>% melt(id.vars = NULL, value.name = "stay_in") %>%
              dplyr::select(sim_id = Var1, candidate = Var2, stay_in), by = c("sim_id", "candidate")) %>%
  mutate(state_pct = state_pct * stay_in) %>%
  left_join(overperformance_stats, by = c("sim_id", "candidate"))

me_sims <- vector("list", n_sims)

me_sims <- foreach(i = 1:n_sims, .packages = c("Hmisc", "dplyr", "reshape2", "tidyr")) %dopar% {
  demographic_lean_sim <- demographic_leans %>%
    mutate(lean = rnorm(n(), mean = avg_lean, sd = lean_se)) %>%
    dplyr::select(candidate, demographic_var, demographic, lean)
  
  # This simulation's national percentages
  this_sim_state_percentage <- me_state_percentages_adj %>%
    filter(sim_id == i)
  
  # Apply simulated leans to state average
  state_demographic_sim <- demoCandidateBreakdown("Maine", this_sim_state_percentage, demographic_lean_sim, 
                                                  state_marginal_demographics)
  
  # Turn these into probabilities
  gender_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("men", "women")) %>%
    dplyr::select(candidate, gender = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(gender = case_when(gender == "men" ~ "Male",
                              gender == "women" ~ "Female"))
  
  names(gender_breakdown)[-1] <- paste0("gender_", names(gender_breakdown))[-1]
  
  race_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("white", "black", "latino")) %>%
    dplyr::select(candidate, race = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(race = case_when(race == "white" ~ "White",
                            race == "black" ~ "Black",
                            race == "latino" ~ "Latino"))
  
  names(race_breakdown)[-1] <- paste0("race_", names(race_breakdown))[-1]
  
  age_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("age_1829", "age_3044", "age_4564", "age_65")) %>%
    dplyr::select(candidate, age = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(age = case_when(age == "age_1829" ~ "18-29",
                           age == "age_3044" ~ "30-44",
                           age == "age_4564" ~ "45-64",
                           age == "age_65" ~ "65+"))
  
  names(age_breakdown)[-1] <- paste0("age_", names(age_breakdown)[-1])
  
  education_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("college", "no_college")) %>%
    dplyr::select(candidate, education = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(education = case_when(education == "college" ~ "College",
                                 education == "no_college" ~ "Less than college"))
  
  names(education_breakdown)[-1] <- paste0("education_", names(education_breakdown)[-1])
  
  synth_pop <- sim_demographics %>%
    mutate(rand = runif(n()),
           age_regroup = case_when(age == "18-24" ~ "18-29",
                                   age == "25-34" & rand < 0.5 ~ "18-29",
                                   age == "25-34" & rand >= 0.5 ~ "30-44",
                                   age == "35-44" ~ "30-44",
                                   age == "45-64" ~ "45-64",
                                   age == "65+" ~ "65+"),
           id = 1:n()) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age = age_regroup, education) %>%
    mutate_if(is.factor, as.character) %>%
    left_join(gender_breakdown, by = "gender") %>%
    left_join(race_breakdown, by = "race") %>%
    left_join(age_breakdown, by = "age") %>%
    left_join(education_breakdown, by = "education") %>%
    mutate(biden_prob = gender_biden + race_biden + age_biden + education_biden,
           buttigieg_prob = gender_buttigieg + race_buttigieg + age_buttigieg + education_buttigieg,
           klobuchar_prob = gender_klobuchar + race_klobuchar + age_klobuchar + education_klobuchar,
           sanders_prob = gender_sanders + race_sanders + age_sanders + education_sanders,
           steyer_prob = gender_steyer + race_steyer + age_steyer + education_steyer,
           warren_prob = gender_warren + race_warren + age_warren + education_warren,
           yang_prob = gender_yang + race_yang + age_yang + education_yang) %>%
    mutate_at(vars(c("biden_prob", "buttigieg_prob", "klobuchar_prob", "sanders_prob", "steyer_prob", "warren_prob", "yang_prob")),
              function(x) pmax(x/100, 0)) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age, education, biden_prob, buttigieg_prob,
                  klobuchar_prob, sanders_prob, steyer_prob, warren_prob, yang_prob) %>%
    melt(id.vars = c("id", "state", "district", "primary_type", "district_delegates", "gender", "race", "age", "education"),
         variable.name = "candidate_prob", value.name = "prob") %>%
    arrange(id, candidate_prob) %>%
    group_by(id) %>%
    mutate(prob = prob / sum(prob),
           cum_prob = cumsum(prob)) %>%
    dplyr::select(-prob) %>%
    ungroup() %>%
    spread(candidate_prob, cum_prob) %>%
    mutate(rand = runif(n()),
           vote = case_when(rand <= biden_prob ~ "biden",
                            rand > biden_prob & rand <= buttigieg_prob ~ "buttigieg",
                            rand > buttigieg_prob & rand <= klobuchar_prob ~ "klobuchar",
                            rand > klobuchar_prob & rand <= sanders_prob ~ "sanders",
                            rand > sanders_prob & rand <= steyer_prob ~ "steyer",
                            rand > steyer_prob & rand <= warren_prob ~ "warren",
                            rand > warren_prob ~ "yang")) %>%
    as.tbl()
  
  synth_district_pop <- synth_pop %>%
    group_by(state, district) %>%
    summarise(district_pop = n())
  
  district_sim_delegates <- synth_pop %>%
    group_by(state, district) %>%
    mutate(district_total = n()) %>%
    group_by(state, district, vote) %>%
    summarise(pct = n() / mean(district_total)) %>%
    ungroup() %>%
    mutate(met_threshold = pct >= 0.15) %>% 
    group_by(state, district) %>%
    mutate(pct_valid = (pct*met_threshold) / sum(pct*met_threshold)) %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(candidate_delegates = delegates * pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, district, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate = vote, pct, pct_valid, candidate_delegates) %>%
    arrange(district, candidate)
  
  state_sim_delegates <- district_sim_delegates %>%
    left_join(synth_district_pop, by = c("state", "district")) %>%
    mutate(synth_votes = pct * district_pop) %>%
    group_by(state) %>%
    mutate(state_vote = sum(synth_votes)) %>%
    group_by(state, candidate) %>%
    summarise(state_pct = sum(synth_votes) / mean(state_vote)) %>%
    mutate(district = "At-large") %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(met_threshold = state_pct >= 0.15,
           state_pct_valid = (state_pct*met_threshold) / sum(state_pct*met_threshold),
           candidate_delegates = delegates * state_pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate, pct = state_pct, pct_valid = state_pct_valid, candidate_delegates) %>%
    arrange(district, candidate) %>%
    bind_rows(district_sim_delegates) %>%
    mutate(sim_id = i)
}

me_stayin_matrix <- st_stayin_matrix


# Mini Tuesday (10 Mar 2020) ####
## Carry Super Tuesday bounce over, but reduced
bounce_factor <- tibble(sim_id = 1:n_sims, bounce_factor = rbeta(n_sims, shape1 = 6, shape2 = 3))

overperformance_bounce <- st_overperformance %>%
  left_join(bounce_factor, by = "sim_id") %>%
  mutate(bounce = overperformance * bounce_factor)

# Idaho ####
id_start_time <- Sys.time()
# Draw votes
sim_demographics_table <- district_demographics %>%
  filter(state == "Idaho") %>%
  mutate(prob = predict(dem_primary_logit, newdata = ., type = "response"),
         n_vote = round(pop * prob * turnout * 0.05)) %>%
  dplyr::select(-pop, -prob)

sim_demographics <- sim_demographics_table %>%
  lapply(rep, sim_demographics_table$n_vote) %>%
  as.data.frame() 
state_marginal_demographics <- shapeStateDemo("Idaho")

# Have no Idaho polls; use nearest neighbors
id_averages_over_time <- state_averages_over_time_unsmoothed %>%
  filter(state %in% c("Indiana", "Missouri", "Oklahoma", "Iowa", "Tennessee"), as.numeric(today() - median_date) <= 1) %>%
  mutate(age = as.numeric(today() - median_date),
         nreps = 2 - age,
         inv_distance = case_when(state == "Indiana" ~ 0.431,
                                  state == "Missouri" ~ 0.413,
                                  state == "Oklahoma" ~ 0.492,
                                  state == "Iowa" ~ 0.402)) %>%
  group_by(candidate, median_date, age, nreps) %>%
  summarise(pct = wtd.mean(pct, inv_distance))

id_dirichlet_params <- ((id_averages_over_time %>%
                           lapply(rep, id_averages_over_time$nreps) %>%
                           as.data.frame() %>%
                           dplyr::select(candidate, median_date, pct) %>%
                           na.omit() %>%
                           dplyr::select(-median_date) %>%
                           group_by(candidate) %>%
                           mutate(i = 1:n()) %>%
                           spread(candidate, pct) %>%
                           dplyr::select(-i) %>%
                           as.matrix() %>%
                           diri.est())$param)

id_dirichlet_params <- 0.5*id_dirichlet_params / min(id_dirichlet_params)
id_dirichlet_params[1] <- id_dirichlet_params[1] / 2
id_dirichlet_params[8] <- 0

id_state_percentages <- (rdirichlet(n_sims, id_dirichlet_params) %>%
                            as.data.frame() %>%
                            dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, 
                                          steyer = V6, warren = V7, yang = V8)) %>%
  apply(MARGIN = 1, FUN = function(x) x / sum(x)) %>%
  t() %>%
  as.data.frame() %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

id_state_percentages_adj <- id_state_percentages %>%
  left_join(overperformance_bounce, by = c("sim_id", "candidate")) %>%
  mutate(state_pct = pmax(state_pct + bounce, 0)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct)) %>%
  left_join(st_second_choice_division, by = c("candidate", "sim_id")) %>%
  melt(id.vars = c("sim_id", "candidate", "state_pct", "overperformance", "bounce_factor", "stay_in", "bounce"), 
       variable.name = "second_choice", value.name = "pct") %>%
  mutate(pct = pct*state_pct*(!stay_in),
         state_pct = case_when(candidate == second_choice ~ state_pct,
                               candidate != second_choice ~ pct)) %>%
  arrange(sim_id, second_choice, candidate)

overperformance_stats <- id_state_percentages_adj %>%
  group_by(sim_id, candidate) %>%
  summarise(overperformance = mean(overperformance),
            bounce_factor = mean(bounce_factor))

id_state_percentages_adj <- id_state_percentages_adj %>%
  group_by(sim_id, second_choice) %>%
  summarise(state_pct = sum(state_pct)) %>%
  dplyr::select(everything(), candidate = second_choice) %>%
  left_join(me_stayin_matrix %>% melt(id.vars = NULL, value.name = "stay_in") %>%
              dplyr::select(sim_id = Var1, candidate = Var2, stay_in), by = c("sim_id", "candidate")) %>%
  mutate(state_pct = state_pct * stay_in) %>%
  left_join(overperformance_stats, by = c("sim_id", "candidate"))

id_sims <- vector("list", n_sims)

id_sims <- foreach(i = 1:n_sims, .packages = c("Hmisc", "dplyr", "reshape2", "tidyr")) %dopar% {
  demographic_lean_sim <- demographic_leans %>%
    mutate(lean = rnorm(n(), mean = avg_lean, sd = lean_se)) %>%
    dplyr::select(candidate, demographic_var, demographic, lean)
  
  # This simulation's national percentages
  this_sim_state_percentage <- id_state_percentages_adj %>%
    filter(sim_id == i)
  
  # Apply simulated leans to state average
  state_demographic_sim <- demoCandidateBreakdown("Montana", this_sim_state_percentage, demographic_lean_sim, 
                                                  state_marginal_demographics)
  
  # Turn these into probabilities
  gender_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("men", "women")) %>%
    dplyr::select(candidate, gender = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(gender = case_when(gender == "men" ~ "Male",
                              gender == "women" ~ "Female"))
  
  names(gender_breakdown)[-1] <- paste0("gender_", names(gender_breakdown))[-1]
  
  race_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("white", "black", "latino")) %>%
    dplyr::select(candidate, race = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(race = case_when(race == "white" ~ "White",
                            race == "black" ~ "Black",
                            race == "latino" ~ "Latino"))
  
  names(race_breakdown)[-1] <- paste0("race_", names(race_breakdown))[-1]
  
  age_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("age_1829", "age_3044", "age_4564", "age_65")) %>%
    dplyr::select(candidate, age = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(age = case_when(age == "age_1829" ~ "18-29",
                           age == "age_3044" ~ "30-44",
                           age == "age_4564" ~ "45-64",
                           age == "age_65" ~ "65+"))
  
  names(age_breakdown)[-1] <- paste0("age_", names(age_breakdown)[-1])
  
  education_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("college", "no_college")) %>%
    dplyr::select(candidate, education = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(education = case_when(education == "college" ~ "College",
                                 education == "no_college" ~ "Less than college"))
  
  names(education_breakdown)[-1] <- paste0("education_", names(education_breakdown)[-1])
  
  synth_pop <- sim_demographics %>%
    mutate(rand = runif(n()),
           age_regroup = case_when(age == "18-24" ~ "18-29",
                                   age == "25-34" & rand < 0.5 ~ "18-29",
                                   age == "25-34" & rand >= 0.5 ~ "30-44",
                                   age == "35-44" ~ "30-44",
                                   age == "45-64" ~ "45-64",
                                   age == "65+" ~ "65+"),
           id = 1:n()) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age = age_regroup, education) %>%
    mutate_if(is.factor, as.character) %>%
    left_join(gender_breakdown, by = "gender") %>%
    left_join(race_breakdown, by = "race") %>%
    left_join(age_breakdown, by = "age") %>%
    left_join(education_breakdown, by = "education") %>%
    mutate(biden_prob = gender_biden + race_biden + age_biden + education_biden,
           bloomberg_prob = gender_bloomberg + race_bloomberg + age_bloomberg + education_bloomberg,
           buttigieg_prob = gender_buttigieg + race_buttigieg + age_buttigieg + education_buttigieg,
           klobuchar_prob = gender_klobuchar + race_klobuchar + age_klobuchar + education_klobuchar,
           sanders_prob = gender_sanders + race_sanders + age_sanders + education_sanders,
           steyer_prob = gender_steyer + race_steyer + age_steyer + education_steyer,
           warren_prob = gender_warren + race_warren + age_warren + education_warren,
           yang_prob = gender_yang + race_yang + age_yang + education_yang) %>%
    mutate_at(vars(c("biden_prob", "bloomberg_prob", "buttigieg_prob", "klobuchar_prob", "sanders_prob", "steyer_prob", "warren_prob", 
                     "yang_prob")), function(x) pmax(x/100, 0)) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age, education, biden_prob, 
                  bloomberg_prob, buttigieg_prob, klobuchar_prob, sanders_prob, steyer_prob, warren_prob, yang_prob) %>%
    melt(id.vars = c("id", "state", "district", "primary_type", "district_delegates", "gender", "race", "age", "education"),
         variable.name = "candidate_prob", value.name = "prob") %>%
    arrange(id, candidate_prob) %>%
    group_by(id) %>%
    mutate(prob = prob / sum(prob),
           cum_prob = cumsum(prob)) %>%
    dplyr::select(-prob) %>%
    ungroup() %>%
    spread(candidate_prob, cum_prob) %>%
    mutate(rand = runif(n()),
           vote = case_when(rand <= biden_prob ~ "biden",
                            rand > biden_prob & rand <= bloomberg_prob ~ "bloomberg",
                            rand > bloomberg_prob & rand <= buttigieg_prob ~ "buttigieg",
                            rand > buttigieg_prob & rand <= klobuchar_prob ~ "klobuchar",
                            rand > klobuchar_prob & rand <= sanders_prob ~ "sanders",
                            rand > sanders_prob & rand <= steyer_prob ~ "steyer",
                            rand > steyer_prob & rand <= warren_prob ~ "warren",
                            rand > warren_prob ~ "yang")) %>%
    as.tbl()
  
  
  synth_district_pop <- synth_pop %>%
    group_by(state, district) %>%
    summarise(district_pop = n())
  
  district_sim_delegates <- synth_pop %>%
    group_by(state, district) %>%
    mutate(district_total = n()) %>%
    group_by(state, district, vote) %>%
    summarise(pct = n() / mean(district_total)) %>%
    ungroup() %>%
    mutate(met_threshold = pct >= 0.15) %>% 
    group_by(state, district) %>%
    mutate(pct_valid = (pct*met_threshold) / sum(pct*met_threshold)) %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(candidate_delegates = delegates * pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, district, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate = vote, pct, pct_valid, candidate_delegates) %>%
    arrange(district, candidate)
  
  state_sim_delegates <- district_sim_delegates %>%
    left_join(synth_district_pop, by = c("state", "district")) %>%
    mutate(synth_votes = pct * district_pop) %>%
    group_by(state) %>%
    mutate(state_vote = sum(synth_votes)) %>%
    group_by(state, candidate) %>%
    summarise(state_pct = sum(synth_votes) / mean(state_vote)) %>%
    mutate(district = "At-large") %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(met_threshold = state_pct >= 0.15,
           state_pct_valid = (state_pct*met_threshold) / sum(state_pct*met_threshold),
           candidate_delegates = delegates * state_pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate, pct = state_pct, pct_valid = state_pct_valid, candidate_delegates) %>%
    arrange(district, candidate) %>%
    bind_rows(district_sim_delegates) %>%
    mutate(sim_id = i)
}

# Michigan ####
mi_start_time <- Sys.time()
# Draw votes
sim_demographics_table <- district_demographics %>%
  filter(state == "Michigan") %>%
  mutate(prob = predict(dem_primary_logit, newdata = ., type = "response"),
         n_vote = round(pop * prob * turnout * 0.05)) %>%
  dplyr::select(-pop, -prob)

sim_demographics <- sim_demographics_table %>%
  lapply(rep, sim_demographics_table$n_vote) %>%
  as.data.frame() 

state_marginal_demographics <- shapeStateDemo("Michigan")

mi_averages_over_time <- state_averages_over_time_unsmoothed %>%
  filter(state == "Michigan", as.numeric(today() - median_date) <= 1) %>%
  mutate(age = as.numeric(today() - median_date),
         nreps = 2 - age) 

mi_dirichlet_params <- (mi_averages_over_time %>%
                          lapply(rep, mi_averages_over_time$nreps) %>%
                          as.data.frame() %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          dplyr::select(-median_date) %>%
                          group_by(candidate) %>%
                          mutate(i = 1:n()) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-i) %>%
                          as.matrix() %>%
                          diri.est())$param

mi_dirichlet_params <- mi_dirichlet_params / 2
mi_dirichlet_params[8] <- 0

mi_state_percentages <- (rdirichlet(n_sims, mi_dirichlet_params) %>%
                            as.data.frame() %>%
                            dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, 
                                          steyer = V6, warren = V7, yang = V8)) %>%
  apply(MARGIN = 1, FUN = function(x) x / sum(x)) %>%
  t() %>%
  as.data.frame() %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

mi_state_percentages_adj <- mi_state_percentages %>%
  left_join(overperformance_bounce, by = c("sim_id", "candidate")) %>%
  mutate(state_pct = pmax(state_pct + bounce, 0)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct)) %>%
  left_join(st_second_choice_division, by = c("candidate", "sim_id")) %>%
  melt(id.vars = c("sim_id", "candidate", "state_pct", "overperformance", "bounce_factor", "stay_in", "bounce"), 
       variable.name = "second_choice", value.name = "pct") %>%
  mutate(pct = pct*state_pct*(!stay_in),
         state_pct = case_when(candidate == second_choice ~ state_pct,
                               candidate != second_choice ~ pct)) %>%
  arrange(sim_id, second_choice, candidate)

overperformance_stats <- mi_state_percentages_adj %>%
  group_by(sim_id, candidate) %>%
  summarise(overperformance = mean(overperformance),
            bounce_factor = mean(bounce_factor))

mi_state_percentages_adj <- mi_state_percentages_adj %>%
  group_by(sim_id, second_choice) %>%
  summarise(state_pct = sum(state_pct)) %>%
  dplyr::select(everything(), candidate = second_choice) %>%
  left_join(me_stayin_matrix %>% melt(id.vars = NULL, value.name = "stay_in") %>%
              dplyr::select(sim_id = Var1, candidate = Var2, stay_in), by = c("sim_id", "candidate")) %>%
  mutate(state_pct = state_pct * stay_in) %>%
  left_join(overperformance_stats, by = c("sim_id", "candidate"))

mi_sims <- vector("list", n_sims)

mi_sims <- foreach(i = 1:n_sims, .packages = c("Hmisc", "dplyr", "reshape2", "tidyr")) %dopar% {
  demographic_lean_sim <- demographic_leans %>%
    mutate(lean = rnorm(n(), mean = avg_lean, sd = lean_se)) %>%
    dplyr::select(candidate, demographic_var, demographic, lean)
  
  # This simulation's national percentages
  this_sim_state_percentage <- mi_state_percentages_adj %>%
    filter(sim_id == i)
  
  # Apply simulated leans to state average
  state_demographic_sim <- demoCandidateBreakdown("Michigan", this_sim_state_percentage, demographic_lean_sim, 
                                                  state_marginal_demographics)
  
  # Turn these into probabilities
  gender_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("men", "women")) %>%
    dplyr::select(candidate, gender = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(gender = case_when(gender == "men" ~ "Male",
                              gender == "women" ~ "Female"))
  
  names(gender_breakdown)[-1] <- paste0("gender_", names(gender_breakdown))[-1]
  
  race_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("white", "black", "latino")) %>%
    dplyr::select(candidate, race = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(race = case_when(race == "white" ~ "White",
                            race == "black" ~ "Black",
                            race == "latino" ~ "Latino"))
  
  names(race_breakdown)[-1] <- paste0("race_", names(race_breakdown))[-1]
  
  age_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("age_1829", "age_3044", "age_4564", "age_65")) %>%
    dplyr::select(candidate, age = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(age = case_when(age == "age_1829" ~ "18-29",
                           age == "age_3044" ~ "30-44",
                           age == "age_4564" ~ "45-64",
                           age == "age_65" ~ "65+"))
  
  names(age_breakdown)[-1] <- paste0("age_", names(age_breakdown)[-1])
  
  education_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("college", "no_college")) %>%
    dplyr::select(candidate, education = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(education = case_when(education == "college" ~ "College",
                                 education == "no_college" ~ "Less than college"))
  
  names(education_breakdown)[-1] <- paste0("education_", names(education_breakdown)[-1])
  
  synth_pop <- sim_demographics %>%
    mutate(rand = runif(n()),
           age_regroup = case_when(age == "18-24" ~ "18-29",
                                   age == "25-34" & rand < 0.5 ~ "18-29",
                                   age == "25-34" & rand >= 0.5 ~ "30-44",
                                   age == "35-44" ~ "30-44",
                                   age == "45-64" ~ "45-64",
                                   age == "65+" ~ "65+"),
           id = 1:n()) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age = age_regroup, education) %>%
    mutate_if(is.factor, as.character) %>%
    left_join(gender_breakdown, by = "gender") %>%
    left_join(race_breakdown, by = "race") %>%
    left_join(age_breakdown, by = "age") %>%
    left_join(education_breakdown, by = "education") %>%
    mutate(biden_prob = gender_biden + race_biden + age_biden + education_biden,
           bloomberg_prob = gender_bloomberg + race_bloomberg + age_bloomberg + education_bloomberg,
           buttigieg_prob = gender_buttigieg + race_buttigieg + age_buttigieg + education_buttigieg,
           klobuchar_prob = gender_klobuchar + race_klobuchar + age_klobuchar + education_klobuchar,
           sanders_prob = gender_sanders + race_sanders + age_sanders + education_sanders,
           steyer_prob = gender_steyer + race_steyer + age_steyer + education_steyer,
           warren_prob = gender_warren + race_warren + age_warren + education_warren,
           yang_prob = gender_yang + race_yang + age_yang + education_yang) %>%
    mutate_at(vars(c("biden_prob", "bloomberg_prob", "buttigieg_prob", "klobuchar_prob", "sanders_prob", "steyer_prob", "warren_prob", 
                     "yang_prob")),
              function(x) pmax(x/100, 0)) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age, education, biden_prob, 
                  bloomberg_prob, buttigieg_prob, klobuchar_prob, sanders_prob, steyer_prob, warren_prob, yang_prob) %>%
    melt(id.vars = c("id", "state", "district", "primary_type", "district_delegates", "gender", "race", "age", "education"),
         variable.name = "candidate_prob", value.name = "prob") %>%
    arrange(id, candidate_prob) %>%
    group_by(id) %>%
    mutate(prob = prob / sum(prob),
           cum_prob = cumsum(prob)) %>%
    dplyr::select(-prob) %>%
    ungroup() %>%
    spread(candidate_prob, cum_prob) %>%
    mutate(rand = runif(n()),
           vote = case_when(rand <= biden_prob ~ "biden",
                            rand > biden_prob & rand <= bloomberg_prob ~ "bloomberg",
                            rand > bloomberg_prob & rand <= buttigieg_prob ~ "buttigieg",
                            rand > buttigieg_prob & rand <= klobuchar_prob ~ "klobuchar",
                            rand > klobuchar_prob & rand <= sanders_prob ~ "sanders",
                            rand > sanders_prob & rand <= steyer_prob ~ "steyer",
                            rand > steyer_prob & rand <= warren_prob ~ "warren",
                            rand > warren_prob ~ "yang")) %>%
    as.tbl()
  
  synth_district_pop <- synth_pop %>%
    group_by(state, district) %>%
    summarise(district_pop = n())
  
  district_sim_delegates <- synth_pop %>%
    group_by(state, district) %>%
    mutate(district_total = n()) %>%
    group_by(state, district, vote) %>%
    summarise(pct = n() / mean(district_total)) %>%
    ungroup() %>%
    mutate(met_threshold = pct >= 0.15) %>% 
    group_by(state, district) %>%
    mutate(pct_valid = (pct*met_threshold) / sum(pct*met_threshold)) %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(candidate_delegates = delegates * pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, district, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate = vote, pct, pct_valid, candidate_delegates) %>%
    arrange(district, candidate)
  
  state_sim_delegates <- district_sim_delegates %>%
    left_join(synth_district_pop, by = c("state", "district")) %>%
    mutate(synth_votes = pct * district_pop) %>%
    group_by(state) %>%
    mutate(state_vote = sum(synth_votes)) %>%
    group_by(state, candidate) %>%
    summarise(state_pct = sum(synth_votes) / mean(state_vote)) %>%
    mutate(district = "At-large") %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(met_threshold = state_pct >= 0.15,
           state_pct_valid = (state_pct*met_threshold) / sum(state_pct*met_threshold),
           candidate_delegates = delegates * state_pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate, pct = state_pct, pct_valid = state_pct_valid, candidate_delegates) %>%
    arrange(district, candidate) %>%
    bind_rows(district_sim_delegates) %>%
    mutate(sim_id = i)
}

# How much did the candidates overperform in Michigan?
mi_overperformance <- mi_state_percentages_adj %>%
  left_join(state_averages %>% filter(state == "Michigan") %>% dplyr::select(candidate, state_avg), by = c("candidate")) %>%
  mutate(overperformance = state_pct - (state_avg/100 + 0.25*overperformance*bounce_factor)) %>%
  dplyr::select(sim_id, candidate, mi_overperformance = overperformance) 


# Mississippi ####
ms_start_time <- Sys.time()
# Draw votes
sim_demographics_table <- district_demographics %>%
  filter(state == "Mississippi") %>%
  mutate(prob = predict(dem_primary_logit, newdata = ., type = "response"),
         n_vote = round(pop * prob * turnout * 0.05)) %>%
  dplyr::select(-pop, -prob)

sim_demographics <- sim_demographics_table %>%
  lapply(rep, sim_demographics_table$n_vote) %>%
  as.data.frame() 

state_marginal_demographics <- shapeStateDemo("Mississippi")

ms_averages_over_time <- state_averages_over_time_unsmoothed %>%
  filter(state == "Mississippi", as.numeric(today() - median_date) <= 1) %>%
  mutate(age = as.numeric(today() - median_date),
         nreps = 2 - age) 

ms_dirichlet_params <- (ms_averages_over_time %>%
                          lapply(rep, ms_averages_over_time$nreps) %>%
                          as.data.frame() %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          mutate(pct = pct / 100) %>%
                          dplyr::select(-median_date) %>%
                          group_by(candidate) %>%
                          mutate(i = 1:n()) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-i) %>%
                          as.matrix() %>%
                          diri.est())$param

ms_dirichlet_params <- ms_dirichlet_params / 2
ms_dirichlet_params[8] <- 0

ms_state_percentages <- (rdirichlet(n_sims, ms_dirichlet_params) %>%
                            as.data.frame() %>%
                            dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, 
                                          steyer = V6, warren = V7, yang = V8)) %>%
  apply(MARGIN = 1, FUN = function(x) x / sum(x)) %>%
  t() %>%
  as.data.frame() %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

ms_state_percentages_adj <- ms_state_percentages %>%
  left_join(overperformance_bounce, by = c("sim_id", "candidate")) %>%
  mutate(state_pct = pmax(state_pct + bounce, 0)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct)) %>%
  left_join(st_second_choice_division, by = c("candidate", "sim_id")) %>%
  melt(id.vars = c("sim_id", "candidate", "state_pct", "overperformance", "bounce_factor", "stay_in", "bounce"), 
       variable.name = "second_choice", value.name = "pct") %>%
  mutate(pct = pct*state_pct*(!stay_in),
         state_pct = case_when(candidate == second_choice ~ state_pct,
                               candidate != second_choice ~ pct)) %>%
  arrange(sim_id, second_choice, candidate)

overperformance_stats <- ms_state_percentages_adj %>%
  group_by(sim_id, candidate) %>%
  summarise(overperformance = mean(overperformance),
            bounce_factor = mean(bounce_factor))

ms_state_percentages_adj <- ms_state_percentages_adj %>%
  group_by(sim_id, second_choice) %>%
  summarise(state_pct = sum(state_pct)) %>%
  dplyr::select(everything(), candidate = second_choice) %>%
  left_join(me_stayin_matrix %>% melt(id.vars = NULL, value.name = "stay_in") %>%
              dplyr::select(sim_id = Var1, candidate = Var2, stay_in), by = c("sim_id", "candidate")) %>%
  mutate(state_pct = state_pct * stay_in) %>%
  left_join(overperformance_stats, by = c("sim_id", "candidate"))

ms_sims <- vector("list", n_sims)

ms_sims <- foreach(i = 1:n_sims, .packages = c("Hmisc", "dplyr", "reshape2", "tidyr")) %dopar% {
  demographic_lean_sim <- demographic_leans %>%
    mutate(lean = rnorm(n(), mean = avg_lean, sd = lean_se)) %>%
    dplyr::select(candidate, demographic_var, demographic, lean)
  
  # This simulation's national percentages
  this_sim_state_percentage <- ms_state_percentages_adj %>%
    filter(sim_id == i)
  
  # Apply simulated leans to state average
  state_demographic_sim <- demoCandidateBreakdown("Mississippi", this_sim_state_percentage, demographic_lean_sim, 
                                                  state_marginal_demographics)
  
  # Turn these into probabilities
  gender_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("men", "women")) %>%
    dplyr::select(candidate, gender = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(gender = case_when(gender == "men" ~ "Male",
                              gender == "women" ~ "Female"))
  
  names(gender_breakdown)[-1] <- paste0("gender_", names(gender_breakdown))[-1]
  
  race_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("white", "black", "latino")) %>%
    dplyr::select(candidate, race = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(race = case_when(race == "white" ~ "White",
                            race == "black" ~ "Black",
                            race == "latino" ~ "Latino"))
  
  names(race_breakdown)[-1] <- paste0("race_", names(race_breakdown))[-1]
  
  age_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("age_1829", "age_3044", "age_4564", "age_65")) %>%
    dplyr::select(candidate, age = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(age = case_when(age == "age_1829" ~ "18-29",
                           age == "age_3044" ~ "30-44",
                           age == "age_4564" ~ "45-64",
                           age == "age_65" ~ "65+"))
  
  names(age_breakdown)[-1] <- paste0("age_", names(age_breakdown)[-1])
  
  education_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("college", "no_college")) %>%
    dplyr::select(candidate, education = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(education = case_when(education == "college" ~ "College",
                                 education == "no_college" ~ "Less than college"))
  
  names(education_breakdown)[-1] <- paste0("education_", names(education_breakdown)[-1])
  
  synth_pop <- sim_demographics %>%
    mutate(rand = runif(n()),
           age_regroup = case_when(age == "18-24" ~ "18-29",
                                   age == "25-34" & rand < 0.5 ~ "18-29",
                                   age == "25-34" & rand >= 0.5 ~ "30-44",
                                   age == "35-44" ~ "30-44",
                                   age == "45-64" ~ "45-64",
                                   age == "65+" ~ "65+"),
           id = 1:n()) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age = age_regroup, education) %>%
    mutate_if(is.factor, as.character) %>%
    left_join(gender_breakdown, by = "gender") %>%
    left_join(race_breakdown, by = "race") %>%
    left_join(age_breakdown, by = "age") %>%
    left_join(education_breakdown, by = "education") %>%
    mutate(biden_prob = gender_biden + race_biden + age_biden + education_biden,
           bloomberg_prob = gender_bloomberg + race_bloomberg + age_bloomberg + education_bloomberg,
           buttigieg_prob = gender_buttigieg + race_buttigieg + age_buttigieg + education_buttigieg,
           klobuchar_prob = gender_klobuchar + race_klobuchar + age_klobuchar + education_klobuchar,
           sanders_prob = gender_sanders + race_sanders + age_sanders + education_sanders,
           steyer_prob = gender_steyer + race_steyer + age_steyer + education_steyer,
           warren_prob = gender_warren + race_warren + age_warren + education_warren,
           yang_prob = gender_yang + race_yang + age_yang + education_yang) %>%
    mutate_at(vars(c("biden_prob", "bloomberg_prob", "buttigieg_prob", "klobuchar_prob", "sanders_prob", "steyer_prob", "warren_prob", 
                     "yang_prob")),
              function(x) pmax(x/100, 0)) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age, education, biden_prob, 
                  bloomberg_prob, buttigieg_prob, klobuchar_prob, sanders_prob, steyer_prob, warren_prob, yang_prob) %>%
    melt(id.vars = c("id", "state", "district", "primary_type", "district_delegates", "gender", "race", "age", "education"),
         variable.name = "candidate_prob", value.name = "prob") %>%
    arrange(id, candidate_prob) %>%
    group_by(id) %>%
    mutate(prob = prob / sum(prob),
           cum_prob = cumsum(prob)) %>%
    dplyr::select(-prob) %>%
    ungroup() %>%
    spread(candidate_prob, cum_prob) %>%
    mutate(rand = runif(n()),
           vote = case_when(rand <= biden_prob ~ "biden",
                            rand > biden_prob & rand <= bloomberg_prob ~ "bloomberg",
                            rand > bloomberg_prob & rand <= buttigieg_prob ~ "buttigieg",
                            rand > buttigieg_prob & rand <= klobuchar_prob ~ "klobuchar",
                            rand > klobuchar_prob & rand <= sanders_prob ~ "sanders",
                            rand > sanders_prob & rand <= steyer_prob ~ "steyer",
                            rand > steyer_prob & rand <= warren_prob ~ "warren",
                            rand > warren_prob ~ "yang")) %>%
    as.tbl()
  
  synth_district_pop <- synth_pop %>%
    group_by(state, district) %>%
    summarise(district_pop = n())
  
  district_sim_delegates <- synth_pop %>%
    group_by(state, district) %>%
    mutate(district_total = n()) %>%
    group_by(state, district, vote) %>%
    summarise(pct = n() / mean(district_total)) %>%
    ungroup() %>%
    mutate(met_threshold = pct >= 0.15) %>% 
    group_by(state, district) %>%
    mutate(pct_valid = (pct*met_threshold) / sum(pct*met_threshold)) %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(candidate_delegates = delegates * pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, district, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate = vote, pct, pct_valid, candidate_delegates) %>%
    arrange(district, candidate)
  
  state_sim_delegates <- district_sim_delegates %>%
    left_join(synth_district_pop, by = c("state", "district")) %>%
    mutate(synth_votes = pct * district_pop) %>%
    group_by(state) %>%
    mutate(state_vote = sum(synth_votes)) %>%
    group_by(state, candidate) %>%
    summarise(state_pct = sum(synth_votes) / mean(state_vote)) %>%
    mutate(district = "At-large") %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(met_threshold = state_pct >= 0.15,
           state_pct_valid = (state_pct*met_threshold) / sum(state_pct*met_threshold),
           candidate_delegates = delegates * state_pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate, pct = state_pct, pct_valid = state_pct_valid, candidate_delegates) %>%
    arrange(district, candidate) %>%
    bind_rows(district_sim_delegates) %>%
    mutate(sim_id = i)
}

# How much did the candidates overperform in Mississippi?
ms_overperformance <- ms_state_percentages_adj %>%
  left_join(state_averages %>% filter(state == "Mississippi") %>% dplyr::select(candidate, state_avg), by = c("candidate")) %>%
  mutate(overperformance = state_pct - (state_avg/100 + 0.25*overperformance*bounce_factor)) %>%
  dplyr::select(sim_id, candidate, ms_overperformance = overperformance) 

# Missouri ####
mo_start_time <- Sys.time()
# Draw votes
sim_demographics_table <- district_demographics %>%
  filter(state == "Missouri") %>%
  mutate(prob = predict(dem_primary_logit, newdata = ., type = "response"),
         n_vote = round(pop * prob * turnout * 0.05)) %>%
  dplyr::select(-pop, -prob)

sim_demographics <- sim_demographics_table %>%
  lapply(rep, sim_demographics_table$n_vote) %>%
  as.data.frame() 

state_marginal_demographics <- shapeStateDemo("Missouri")

mo_averages_over_time <- state_averages_over_time_unsmoothed %>%
  filter(state == "Missouri", as.numeric(today() - median_date) <= 1) %>%
  mutate(age = as.numeric(today() - median_date),
         nreps = 2 - age) 

mo_dirichlet_params <- (mo_averages_over_time %>%
                          lapply(rep, mo_averages_over_time$nreps) %>%
                          as.data.frame() %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          dplyr::select(-median_date) %>%
                          group_by(candidate) %>%
                          mutate(i = 1:n()) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-i) %>%
                          as.matrix() %>%
                          diri.est())$param

mo_dirichlet_params <- mo_dirichlet_params / 2
mo_dirichlet_params[8] <- 0

mo_state_percentages <- (rdirichlet(n_sims, mo_dirichlet_params) %>%
                            as.data.frame() %>%
                            dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, 
                                          steyer = V6, warren = V7, yang = V8)) %>%
  apply(MARGIN = 1, FUN = function(x) x / sum(x)) %>%
  t() %>%
  as.data.frame() %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

mo_state_percentages_adj <- mo_state_percentages %>%
  left_join(overperformance_bounce, by = c("sim_id", "candidate")) %>%
  mutate(state_pct = pmax(state_pct + bounce, 0)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct)) %>%
  left_join(st_second_choice_division, by = c("candidate", "sim_id")) %>%
  melt(id.vars = c("sim_id", "candidate", "state_pct", "overperformance", "bounce_factor", "stay_in", "bounce"), 
       variable.name = "second_choice", value.name = "pct") %>%
  mutate(pct = pct*state_pct*(!stay_in),
         state_pct = case_when(candidate == second_choice ~ state_pct,
                               candidate != second_choice ~ pct)) %>%
  arrange(sim_id, second_choice, candidate)

overperformance_stats <- mo_state_percentages_adj %>%
  group_by(sim_id, candidate) %>%
  summarise(overperformance = mean(overperformance),
            bounce_factor = mean(bounce_factor))

mo_state_percentages_adj <- mo_state_percentages_adj %>%
  group_by(sim_id, second_choice) %>%
  summarise(state_pct = sum(state_pct)) %>%
  dplyr::select(everything(), candidate = second_choice) %>%
  left_join(me_stayin_matrix %>% melt(id.vars = NULL, value.name = "stay_in") %>%
              dplyr::select(sim_id = Var1, candidate = Var2, stay_in), by = c("sim_id", "candidate")) %>%
  mutate(state_pct = state_pct * stay_in) %>%
  left_join(overperformance_stats, by = c("sim_id", "candidate"))

mo_sims <- vector("list", n_sims)

mo_sims <- foreach(i = 1:n_sims, .packages = c("Hmisc", "dplyr", "reshape2", "tidyr")) %dopar% {
  demographic_lean_sim <- demographic_leans %>%
    mutate(lean = rnorm(n(), mean = avg_lean, sd = lean_se)) %>%
    dplyr::select(candidate, demographic_var, demographic, lean)
  
  # This simulation's national percentages
  this_sim_state_percentage <- mo_state_percentages_adj %>%
    filter(sim_id == i)
  
  # Apply simulated leans to state average
  state_demographic_sim <- demoCandidateBreakdown("Missouri", this_sim_state_percentage, demographic_lean_sim, 
                                                  state_marginal_demographics)
  
  # Turn these into probabilities
  gender_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("men", "women")) %>%
    dplyr::select(candidate, gender = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(gender = case_when(gender == "men" ~ "Male",
                              gender == "women" ~ "Female"))
  
  names(gender_breakdown)[-1] <- paste0("gender_", names(gender_breakdown))[-1]
  
  race_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("white", "black", "latino")) %>%
    dplyr::select(candidate, race = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(race = case_when(race == "white" ~ "White",
                            race == "black" ~ "Black",
                            race == "latino" ~ "Latino"))
  
  names(race_breakdown)[-1] <- paste0("race_", names(race_breakdown))[-1]
  
  age_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("age_1829", "age_3044", "age_4564", "age_65")) %>%
    dplyr::select(candidate, age = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(age = case_when(age == "age_1829" ~ "18-29",
                           age == "age_3044" ~ "30-44",
                           age == "age_4564" ~ "45-64",
                           age == "age_65" ~ "65+"))
  
  names(age_breakdown)[-1] <- paste0("age_", names(age_breakdown)[-1])
  
  education_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("college", "no_college")) %>%
    dplyr::select(candidate, education = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(education = case_when(education == "college" ~ "College",
                                 education == "no_college" ~ "Less than college"))
  
  names(education_breakdown)[-1] <- paste0("education_", names(education_breakdown)[-1])
  
  synth_pop <- sim_demographics %>%
    mutate(rand = runif(n()),
           age_regroup = case_when(age == "18-24" ~ "18-29",
                                   age == "25-34" & rand < 0.5 ~ "18-29",
                                   age == "25-34" & rand >= 0.5 ~ "30-44",
                                   age == "35-44" ~ "30-44",
                                   age == "45-64" ~ "45-64",
                                   age == "65+" ~ "65+"),
           id = 1:n()) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age = age_regroup, education) %>%
    mutate_if(is.factor, as.character) %>%
    left_join(gender_breakdown, by = "gender") %>%
    left_join(race_breakdown, by = "race") %>%
    left_join(age_breakdown, by = "age") %>%
    left_join(education_breakdown, by = "education") %>%
    mutate(biden_prob = gender_biden + race_biden + age_biden + education_biden,
           bloomberg_prob = gender_bloomberg + race_bloomberg + age_bloomberg + education_bloomberg,
           buttigieg_prob = gender_buttigieg + race_buttigieg + age_buttigieg + education_buttigieg,
           klobuchar_prob = gender_klobuchar + race_klobuchar + age_klobuchar + education_klobuchar,
           sanders_prob = gender_sanders + race_sanders + age_sanders + education_sanders,
           steyer_prob = gender_steyer + race_steyer + age_steyer + education_steyer,
           warren_prob = gender_warren + race_warren + age_warren + education_warren,
           yang_prob = gender_yang + race_yang + age_yang + education_yang) %>%
    mutate_at(vars(c("biden_prob", "bloomberg_prob", "buttigieg_prob", "klobuchar_prob", "sanders_prob", "steyer_prob", "warren_prob", 
                     "yang_prob")),
              function(x) pmax(x/100, 0)) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age, education, biden_prob, 
                  bloomberg_prob, buttigieg_prob, klobuchar_prob, sanders_prob, steyer_prob, warren_prob, yang_prob) %>%
    melt(id.vars = c("id", "state", "district", "primary_type", "district_delegates", "gender", "race", "age", "education"),
         variable.name = "candidate_prob", value.name = "prob") %>%
    arrange(id, candidate_prob) %>%
    group_by(id) %>%
    mutate(prob = prob / sum(prob),
           cum_prob = cumsum(prob)) %>%
    dplyr::select(-prob) %>%
    ungroup() %>%
    spread(candidate_prob, cum_prob) %>%
    mutate(rand = runif(n()),
           vote = case_when(rand <= biden_prob ~ "biden",
                            rand > biden_prob & rand <= bloomberg_prob ~ "bloomberg",
                            rand > bloomberg_prob & rand <= buttigieg_prob ~ "buttigieg",
                            rand > buttigieg_prob & rand <= klobuchar_prob ~ "klobuchar",
                            rand > klobuchar_prob & rand <= sanders_prob ~ "sanders",
                            rand > sanders_prob & rand <= steyer_prob ~ "steyer",
                            rand > steyer_prob & rand <= warren_prob ~ "warren",
                            rand > warren_prob ~ "yang")) %>%
    as.tbl()
  
  synth_district_pop <- synth_pop %>%
    group_by(state, district) %>%
    summarise(district_pop = n())
  
  district_sim_delegates <- synth_pop %>%
    group_by(state, district) %>%
    mutate(district_total = n()) %>%
    group_by(state, district, vote) %>%
    summarise(pct = n() / mean(district_total)) %>%
    ungroup() %>%
    mutate(met_threshold = pct >= 0.15) %>% 
    group_by(state, district) %>%
    mutate(pct_valid = (pct*met_threshold) / sum(pct*met_threshold)) %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(candidate_delegates = delegates * pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, district, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate = vote, pct, pct_valid, candidate_delegates) %>%
    arrange(district, candidate)
  
  state_sim_delegates <- district_sim_delegates %>%
    left_join(synth_district_pop, by = c("state", "district")) %>%
    mutate(synth_votes = pct * district_pop) %>%
    group_by(state) %>%
    mutate(state_vote = sum(synth_votes)) %>%
    group_by(state, candidate) %>%
    summarise(state_pct = sum(synth_votes) / mean(state_vote)) %>%
    mutate(district = "At-large") %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(met_threshold = state_pct >= 0.15,
           state_pct_valid = (state_pct*met_threshold) / sum(state_pct*met_threshold),
           candidate_delegates = delegates * state_pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate, pct = state_pct, pct_valid = state_pct_valid, candidate_delegates) %>%
    arrange(district, candidate) %>%
    bind_rows(district_sim_delegates) %>%
    mutate(sim_id = i)
}

# How much did the candidates overperform in Missouri?
mo_overperformance <- mo_state_percentages_adj %>%
  left_join(state_averages %>% filter(state == "Missouri") %>% dplyr::select(candidate, state_avg), by = c("candidate")) %>%
  mutate(overperformance = state_pct - (state_avg/100 + 0.25*overperformance*bounce_factor)) %>%
  dplyr::select(sim_id, candidate, mo_overperformance = overperformance) 


# North Dakota ####
nd_start_time <- Sys.time()
# Shortcut since I know nothing about what the situation looks like on the ground: 

# Have no North Dakota polls; use nearest neighbors
nd_averages_over_time <- state_averages_over_time_unsmoothed %>%
  filter(state %in% c("Iowa", "Minnesota", "Oklahoma", "Utah", "Wisconsin"), as.numeric(today() - median_date) <= 1) %>%
  mutate(age = as.numeric(today() - median_date),
         nreps = 2 - age,
         inv_distance = case_when(state == "Iowa" ~ 0.395,
                                  state == "Minnesota" ~ 0.342,
                                  state == "Oklahoma" ~ 0.316,
                                  state == "Utah" ~ 0.337,
                                  state == "Wisconsin" ~ 0.317)) %>%
  group_by(candidate, median_date, age, nreps) %>%
  summarise(pct = wtd.mean(pct, inv_distance))

nd_dirichlet_params <- ((nd_averages_over_time %>%
                           lapply(rep, nd_averages_over_time$nreps) %>%
                           as.data.frame() %>%
                           dplyr::select(candidate, median_date, pct) %>%
                           na.omit() %>%
                           dplyr::select(-median_date) %>%
                           group_by(candidate) %>%
                           mutate(i = 1:n()) %>%
                           spread(candidate, pct) %>%
                           dplyr::select(-i) %>%
                           as.matrix() %>%
                           diri.est())$param)

nd_dirichlet_params <- 0.5*nd_dirichlet_params / min(nd_dirichlet_params)
nd_dirichlet_params[1] <- nd_dirichlet_params[1] / 2
nd_dirichlet_params[8] <- 0

nd_state_percentages <- (rdirichlet(n_sims, nd_dirichlet_params) %>%
                            as.data.frame() %>%
                            dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, 
                                          steyer = V6, warren = V7, yang = V8)) %>%
  apply(MARGIN = 1, FUN = function(x) x / sum(x)) %>%
  t() %>%
  as.data.frame() %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

nd_state_percentages_adj <- nd_state_percentages %>%
  left_join(overperformance_bounce, by = c("sim_id", "candidate")) %>%
  mutate(state_pct = pmax(state_pct + bounce, 0)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct)) %>%
  left_join(st_second_choice_division, by = c("candidate", "sim_id")) %>%
  melt(id.vars = c("sim_id", "candidate", "state_pct", "overperformance", "bounce_factor", "stay_in", "bounce"), 
       variable.name = "second_choice", value.name = "pct") %>%
  mutate(pct = pct*state_pct*(!stay_in),
         state_pct = case_when(candidate == second_choice ~ state_pct,
                               candidate != second_choice ~ pct)) %>%
  arrange(sim_id, second_choice, candidate)

overperformance_stats <- nd_state_percentages_adj %>%
  group_by(sim_id, candidate) %>%
  summarise(overperformance = mean(overperformance),
            bounce_factor = mean(bounce_factor))

nd_state_percentages_adj <- nd_state_percentages_adj %>%
  group_by(sim_id, second_choice) %>%
  summarise(state_pct = sum(state_pct)) %>%
  dplyr::select(everything(), candidate = second_choice) %>%
  left_join(me_stayin_matrix %>% melt(id.vars = NULL, value.name = "stay_in") %>%
              dplyr::select(sim_id = Var1, candidate = Var2, stay_in), by = c("sim_id", "candidate")) %>%
  mutate(state_pct = state_pct * stay_in) %>%
  left_join(overperformance_stats, by = c("sim_id", "candidate"))

nd_sims <- vector("list", n_sims)

### Simulate
nd_sims <- foreach(i = 1:n_sims, .packages = c("Hmisc", "dplyr", "reshape2", "tidyr")) %dopar% {
  this_sim_state_percentage <- nd_state_percentages_adj %>%
    filter(sim_id == i)
  
  second_choice_sim <- second_choice %>%
    melt(id.vars = "first_choice", variable.name = "second_choice", value.name = "pct") %>%
    mutate(alpha = 50 * pct / (1 - pct),
           second_pct_sim = rbeta(n(), shape1 = alpha, shape2 = 30)) %>%
    dplyr::select(first_choice, second_choice, second_pct_sim)
  
  district_sim_pct <- this_sim_state_percentage %>%
    mutate(met_threshold = state_pct >= 0.15)
  
  state_sim_delegates <- district_sim_pct %>%
    merge(second_choice_sim, by.x = "candidate", by.y = "first_choice", all.x = TRUE) %>%
    merge(district_sim_pct, by.x = "second_choice", by.y = "candidate") %>%
    mutate(pct_from_second_choice = state_pct.y * second_pct_sim * (second_choice != candidate) * (met_threshold.x) * (!met_threshold.y)) %>%
    group_by(candidate, state_pct.x, met_threshold = met_threshold.x) %>%
    summarise(pct_from_second_choice = sum(pct_from_second_choice)) %>%
    ungroup() %>%
    mutate(pct_valid = (state_pct.x + pct_from_second_choice)*met_threshold) %>% 
    mutate(pct_valid = pct_valid / sum(pct_valid),
           state = "North Dakota",
           district = "At-large") %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(candidate_delegates = delegates * pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, district, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate, pct = state_pct.x, pct_valid, candidate_delegates) %>%
    arrange(district, candidate) %>%
    mutate(sim_id = i)
}


# Ohio ####
oh_start_time <- Sys.time()
# Draw votes
sim_demographics_table <- district_demographics %>%
  filter(state == "Ohio") %>%
  mutate(prob = predict(dem_primary_logit, newdata = ., type = "response"),
         n_vote = round(pop * prob * turnout * 0.05)) %>%
  dplyr::select(-pop, -prob)

sim_demographics <- sim_demographics_table %>%
  lapply(rep, sim_demographics_table$n_vote) %>%
  as.data.frame() 

state_marginal_demographics <- shapeStateDemo("Ohio")

oh_averages_over_time <- state_averages_over_time_unsmoothed %>%
  filter(state == "Ohio", as.numeric(today() - median_date) <= 1) %>%
  mutate(age = as.numeric(today() - median_date),
         nreps = 2 - age) 

oh_dirichlet_params <- (oh_averages_over_time %>%
                          lapply(rep, oh_averages_over_time$nreps) %>%
                          as.data.frame() %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          dplyr::select(-median_date) %>%
                          group_by(candidate) %>%
                          mutate(i = 1:n()) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-i) %>%
                          as.matrix() %>%
                          diri.est())$param

oh_dirichlet_params <- oh_dirichlet_params / 2
oh_dirichlet_params[8] <- 0

oh_state_percentages <- (rdirichlet(n_sims, oh_dirichlet_params) %>%
                            as.data.frame() %>%
                            dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, 
                                          steyer = V6, warren = V7, yang = V8)) %>%
  apply(MARGIN = 1, FUN = function(x) x / sum(x)) %>%
  t() %>%
  as.data.frame() %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

oh_state_percentages_adj <- oh_state_percentages %>%
  left_join(overperformance_bounce, by = c("sim_id", "candidate")) %>%
  mutate(state_pct = pmax(state_pct + bounce, 0)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct)) %>%
  left_join(st_second_choice_division, by = c("candidate", "sim_id")) %>%
  melt(id.vars = c("sim_id", "candidate", "state_pct", "overperformance", "bounce_factor", "stay_in", "bounce"), 
       variable.name = "second_choice", value.name = "pct") %>%
  mutate(pct = pct*state_pct*(!stay_in),
         state_pct = case_when(candidate == second_choice ~ state_pct,
                               candidate != second_choice ~ pct)) %>%
  arrange(sim_id, second_choice, candidate)

overperformance_stats <- oh_state_percentages_adj %>%
  group_by(sim_id, candidate) %>%
  summarise(overperformance = mean(overperformance),
            bounce_factor = mean(bounce_factor))

oh_state_percentages_adj <- oh_state_percentages_adj %>%
  group_by(sim_id, second_choice) %>%
  summarise(state_pct = sum(state_pct)) %>%
  dplyr::select(everything(), candidate = second_choice) %>%
  left_join(me_stayin_matrix %>% melt(id.vars = NULL, value.name = "stay_in") %>%
              dplyr::select(sim_id = Var1, candidate = Var2, stay_in), by = c("sim_id", "candidate")) %>%
  mutate(state_pct = state_pct * stay_in) %>%
  left_join(overperformance_stats, by = c("sim_id", "candidate"))

oh_sims <- vector("list", n_sims)

oh_sims <- foreach(i = 1:n_sims, .packages = c("Hmisc", "dplyr", "reshape2", "tidyr")) %dopar% {
  demographic_lean_sim <- demographic_leans %>%
    mutate(lean = rnorm(n(), mean = avg_lean, sd = lean_se)) %>%
    dplyr::select(candidate, demographic_var, demographic, lean)
  
  # This simulation's national percentages
  this_sim_state_percentage <- oh_state_percentages_adj %>%
    filter(sim_id == i)
  
  # Apply simulated leans to state average
  state_demographic_sim <- demoCandidateBreakdown("Ohio", this_sim_state_percentage, demographic_lean_sim, 
                                                  state_marginal_demographics)
  
  # Turn these into probabilities
  gender_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("men", "women")) %>%
    dplyr::select(candidate, gender = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(gender = case_when(gender == "men" ~ "Male",
                              gender == "women" ~ "Female"))
  
  names(gender_breakdown)[-1] <- paste0("gender_", names(gender_breakdown))[-1]
  
  race_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("white", "black", "latino")) %>%
    dplyr::select(candidate, race = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(race = case_when(race == "white" ~ "White",
                            race == "black" ~ "Black",
                            race == "latino" ~ "Latino"))
  
  names(race_breakdown)[-1] <- paste0("race_", names(race_breakdown))[-1]
  
  age_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("age_1829", "age_3044", "age_4564", "age_65")) %>%
    dplyr::select(candidate, age = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(age = case_when(age == "age_1829" ~ "18-29",
                           age == "age_3044" ~ "30-44",
                           age == "age_4564" ~ "45-64",
                           age == "age_65" ~ "65+"))
  
  names(age_breakdown)[-1] <- paste0("age_", names(age_breakdown)[-1])
  
  education_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("college", "no_college")) %>%
    dplyr::select(candidate, education = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(education = case_when(education == "college" ~ "College",
                                 education == "no_college" ~ "Less than college"))
  
  names(education_breakdown)[-1] <- paste0("education_", names(education_breakdown)[-1])
  
  synth_pop <- sim_demographics %>%
    mutate(rand = runif(n()),
           age_regroup = case_when(age == "18-24" ~ "18-29",
                                   age == "25-34" & rand < 0.5 ~ "18-29",
                                   age == "25-34" & rand >= 0.5 ~ "30-44",
                                   age == "35-44" ~ "30-44",
                                   age == "45-64" ~ "45-64",
                                   age == "65+" ~ "65+"),
           id = 1:n()) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age = age_regroup, education) %>%
    mutate_if(is.factor, as.character) %>%
    left_join(gender_breakdown, by = "gender") %>%
    left_join(race_breakdown, by = "race") %>%
    left_join(age_breakdown, by = "age") %>%
    left_join(education_breakdown, by = "education") %>%
    mutate(biden_prob = gender_biden + race_biden + age_biden + education_biden,
           bloomberg_prob = gender_bloomberg + race_bloomberg + age_bloomberg + education_bloomberg,
           buttigieg_prob = gender_buttigieg + race_buttigieg + age_buttigieg + education_buttigieg,
           klobuchar_prob = gender_klobuchar + race_klobuchar + age_klobuchar + education_klobuchar,
           sanders_prob = gender_sanders + race_sanders + age_sanders + education_sanders,
           steyer_prob = gender_steyer + race_steyer + age_steyer + education_steyer,
           warren_prob = gender_warren + race_warren + age_warren + education_warren,
           yang_prob = gender_yang + race_yang + age_yang + education_yang) %>%
    mutate_at(vars(c("biden_prob", "bloomberg_prob", "buttigieg_prob", "klobuchar_prob", "sanders_prob", "steyer_prob", "warren_prob", 
                     "yang_prob")),
              function(x) pmax(x/100, 0)) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age, education, biden_prob, 
                  bloomberg_prob, buttigieg_prob, klobuchar_prob, sanders_prob, steyer_prob, warren_prob, yang_prob) %>%
    melt(id.vars = c("id", "state", "district", "primary_type", "district_delegates", "gender", "race", "age", "education"),
         variable.name = "candidate_prob", value.name = "prob") %>%
    arrange(id, candidate_prob) %>%
    group_by(id) %>%
    mutate(prob = prob / sum(prob),
           cum_prob = cumsum(prob)) %>%
    dplyr::select(-prob) %>%
    ungroup() %>%
    spread(candidate_prob, cum_prob) %>%
    mutate(rand = runif(n()),
           vote = case_when(rand <= biden_prob ~ "biden",
                            rand > biden_prob & rand <= bloomberg_prob ~ "bloomberg",
                            rand > bloomberg_prob & rand <= buttigieg_prob ~ "buttigieg",
                            rand > buttigieg_prob & rand <= klobuchar_prob ~ "klobuchar",
                            rand > klobuchar_prob & rand <= sanders_prob ~ "sanders",
                            rand > sanders_prob & rand <= steyer_prob ~ "steyer",
                            rand > steyer_prob & rand <= warren_prob ~ "warren",
                            rand > warren_prob ~ "yang")) %>%
    as.tbl()
  
  synth_district_pop <- synth_pop %>%
    group_by(state, district) %>%
    summarise(district_pop = n())
  
  district_sim_delegates <- synth_pop %>%
    group_by(state, district) %>%
    mutate(district_total = n()) %>%
    group_by(state, district, vote) %>%
    summarise(pct = n() / mean(district_total)) %>%
    ungroup() %>%
    mutate(met_threshold = pct >= 0.15) %>% 
    group_by(state, district) %>%
    mutate(pct_valid = (pct*met_threshold) / sum(pct*met_threshold)) %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(candidate_delegates = delegates * pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, district, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate = vote, pct, pct_valid, candidate_delegates) %>%
    arrange(district, candidate)
  
  state_sim_delegates <- district_sim_delegates %>%
    left_join(synth_district_pop, by = c("state", "district")) %>%
    mutate(synth_votes = pct * district_pop) %>%
    group_by(state) %>%
    mutate(state_vote = sum(synth_votes)) %>%
    group_by(state, candidate) %>%
    summarise(state_pct = sum(synth_votes) / mean(state_vote)) %>%
    mutate(district = "At-large") %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(met_threshold = state_pct >= 0.15,
           state_pct_valid = (state_pct*met_threshold) / sum(state_pct*met_threshold),
           candidate_delegates = delegates * state_pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate, pct = state_pct, pct_valid = state_pct_valid, candidate_delegates) %>%
    arrange(district, candidate) %>%
    bind_rows(district_sim_delegates) %>%
    mutate(sim_id = i)
}

# How much did the candidates overperform in Ohio?
oh_overperformance <- oh_state_percentages_adj %>%
  left_join(state_averages %>% filter(state == "Ohio") %>% dplyr::select(candidate, state_avg), by = c("candidate")) %>%
  mutate(overperformance = state_pct - (state_avg/100 + 0.25*overperformance*bounce_factor)) %>%
  dplyr::select(sim_id, candidate, oh_overperformance = overperformance) 

# Washington ####
wa_start_time <- Sys.time()
# Draw votes
sim_demographics_table <- district_demographics %>%
  filter(state == "Washington") %>%
  mutate(prob = predict(dem_primary_logit, newdata = ., type = "response"),
         n_vote = round(pop * prob * turnout * 0.05)) %>%
  dplyr::select(-pop, -prob)

sim_demographics <- sim_demographics_table %>%
  lapply(rep, sim_demographics_table$n_vote) %>%
  as.data.frame() 

state_marginal_demographics <- shapeStateDemo("Washington")

wa_averages_over_time <- state_averages_over_time_unsmoothed %>%
  filter(state == "Washington", as.numeric(today() - median_date) <= 1) %>%
  mutate(age = as.numeric(today() - median_date),
         nreps = 2 - age)

wa_dirichlet_params <- (wa_averages_over_time %>%
                          lapply(rep, wa_averages_over_time$nreps) %>%
                          as.data.frame() %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          dplyr::select(-median_date) %>%
                          group_by(candidate) %>%
                          mutate(i = 1:n()) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-i) %>%
                          na.omit() %>%
                          as.matrix() %>%
                          diri.est())$param

wa_dirichlet_params <- wa_dirichlet_params / 2
wa_dirichlet_params[8] <- 0

wa_state_percentages <- (rdirichlet(n_sims, wa_dirichlet_params) %>%
                            as.data.frame() %>%
                            dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, 
                                          steyer = V6, warren = V7, yang = V8)) %>%
  apply(MARGIN = 1, FUN = function(x) x / sum(x)) %>%
  t() %>%
  as.data.frame() %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

wa_state_percentages_adj <- wa_state_percentages %>%
  left_join(overperformance_bounce, by = c("sim_id", "candidate")) %>%
  mutate(state_pct = pmax(state_pct + bounce, 0)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct)) %>%
  left_join(st_second_choice_division, by = c("candidate", "sim_id")) %>%
  melt(id.vars = c("sim_id", "candidate", "state_pct", "overperformance", "bounce_factor", "stay_in", "bounce"), 
       variable.name = "second_choice", value.name = "pct") %>%
  mutate(pct = pct*state_pct*(!stay_in),
         state_pct = case_when(candidate == second_choice ~ state_pct,
                               candidate != second_choice ~ pct)) %>%
  arrange(sim_id, second_choice, candidate)

overperformance_stats <- wa_state_percentages_adj %>%
  group_by(sim_id, candidate) %>%
  summarise(overperformance = mean(overperformance),
            bounce_factor = mean(bounce_factor))

wa_state_percentages_adj <- wa_state_percentages_adj %>%
  group_by(sim_id, second_choice) %>%
  summarise(state_pct = sum(state_pct)) %>%
  dplyr::select(everything(), candidate = second_choice) %>%
  left_join(me_stayin_matrix %>% melt(id.vars = NULL, value.name = "stay_in") %>%
              dplyr::select(sim_id = Var1, candidate = Var2, stay_in), by = c("sim_id", "candidate")) %>%
  mutate(state_pct = state_pct * stay_in) %>%
  left_join(overperformance_stats, by = c("sim_id", "candidate"))

wa_sims <- vector("list", n_sims)

wa_sims <- foreach(i = 1:n_sims, .packages = c("Hmisc", "dplyr", "reshape2", "tidyr")) %dopar% {
  demographic_lean_sim <- demographic_leans %>%
    mutate(lean = rnorm(n(), mean = avg_lean, sd = lean_se)) %>%
    dplyr::select(candidate, demographic_var, demographic, lean)
  
  # This simulation's national percentages
  this_sim_state_percentage <- wa_state_percentages_adj %>%
    filter(sim_id == i)
  
  # Apply simulated leans to state average
  state_demographic_sim <- demoCandidateBreakdown("Washington", this_sim_state_percentage, demographic_lean_sim, 
                                                  state_marginal_demographics)
  
  # Turn these into probabilities
  gender_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("men", "women")) %>%
    dplyr::select(candidate, gender = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(gender = case_when(gender == "men" ~ "Male",
                              gender == "women" ~ "Female"))
  
  names(gender_breakdown)[-1] <- paste0("gender_", names(gender_breakdown))[-1]
  
  race_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("white", "black", "latino")) %>%
    dplyr::select(candidate, race = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(race = case_when(race == "white" ~ "White",
                            race == "black" ~ "Black",
                            race == "latino" ~ "Latino"))
  
  names(race_breakdown)[-1] <- paste0("race_", names(race_breakdown))[-1]
  
  age_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("age_1829", "age_3044", "age_4564", "age_65")) %>%
    dplyr::select(candidate, age = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(age = case_when(age == "age_1829" ~ "18-29",
                           age == "age_3044" ~ "30-44",
                           age == "age_4564" ~ "45-64",
                           age == "age_65" ~ "65+"))
  
  names(age_breakdown)[-1] <- paste0("age_", names(age_breakdown)[-1])
  
  education_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("college", "no_college")) %>%
    dplyr::select(candidate, education = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(education = case_when(education == "college" ~ "College",
                                 education == "no_college" ~ "Less than college"))
  
  names(education_breakdown)[-1] <- paste0("education_", names(education_breakdown)[-1])
  
  synth_pop <- sim_demographics %>%
    mutate(rand = runif(n()),
           age_regroup = case_when(age == "18-24" ~ "18-29",
                                   age == "25-34" & rand < 0.5 ~ "18-29",
                                   age == "25-34" & rand >= 0.5 ~ "30-44",
                                   age == "35-44" ~ "30-44",
                                   age == "45-64" ~ "45-64",
                                   age == "65+" ~ "65+"),
           id = 1:n()) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age = age_regroup, education) %>%
    mutate_if(is.factor, as.character) %>%
    left_join(gender_breakdown, by = "gender") %>%
    left_join(race_breakdown, by = "race") %>%
    left_join(age_breakdown, by = "age") %>%
    left_join(education_breakdown, by = "education") %>%
    mutate(biden_prob = gender_biden + race_biden + age_biden + education_biden,
           bloomberg_prob = gender_bloomberg + race_bloomberg + age_bloomberg + education_bloomberg,
           buttigieg_prob = gender_buttigieg + race_buttigieg + age_buttigieg + education_buttigieg,
           klobuchar_prob = gender_klobuchar + race_klobuchar + age_klobuchar + education_klobuchar,
           sanders_prob = gender_sanders + race_sanders + age_sanders + education_sanders,
           steyer_prob = gender_steyer + race_steyer + age_steyer + education_steyer,
           warren_prob = gender_warren + race_warren + age_warren + education_warren,
           yang_prob = gender_yang + race_yang + age_yang + education_yang) %>%
    mutate_at(vars(c("biden_prob", "bloomberg_prob", "buttigieg_prob", "klobuchar_prob", "sanders_prob", "steyer_prob", "warren_prob", 
                     "yang_prob")),
              function(x) pmax(x/100, 0)) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age, education, biden_prob, 
                  bloomberg_prob, buttigieg_prob, klobuchar_prob, sanders_prob, steyer_prob, warren_prob, yang_prob) %>%
    melt(id.vars = c("id", "state", "district", "primary_type", "district_delegates", "gender", "race", "age", "education"),
         variable.name = "candidate_prob", value.name = "prob") %>%
    arrange(id, candidate_prob) %>%
    group_by(id) %>%
    mutate(prob = prob / sum(prob),
           cum_prob = cumsum(prob)) %>%
    dplyr::select(-prob) %>%
    ungroup() %>%
    spread(candidate_prob, cum_prob) %>%
    mutate(rand = runif(n()),
           vote = case_when(rand <= biden_prob ~ "biden",
                            rand > biden_prob & rand <= bloomberg_prob ~ "bloomberg",
                            rand > bloomberg_prob & rand <= buttigieg_prob ~ "buttigieg",
                            rand > buttigieg_prob & rand <= klobuchar_prob ~ "klobuchar",
                            rand > klobuchar_prob & rand <= sanders_prob ~ "sanders",
                            rand > sanders_prob & rand <= steyer_prob ~ "steyer",
                            rand > steyer_prob & rand <= warren_prob ~ "warren",
                            rand > warren_prob ~ "yang")) %>%
    as.tbl()
  
  synth_district_pop <- synth_pop %>%
    group_by(state, district) %>%
    summarise(district_pop = n())
  
  district_sim_delegates <- synth_pop %>%
    group_by(state, district) %>%
    mutate(district_total = n()) %>%
    group_by(state, district, vote) %>%
    summarise(pct = n() / mean(district_total)) %>%
    ungroup() %>%
    mutate(met_threshold = pct >= 0.15) %>% 
    group_by(state, district) %>%
    mutate(pct_valid = (pct*met_threshold) / sum(pct*met_threshold)) %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(candidate_delegates = delegates * pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, district, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate = vote, pct, pct_valid, candidate_delegates) %>%
    arrange(district, candidate)
  
  state_sim_delegates <- district_sim_delegates %>%
    left_join(synth_district_pop, by = c("state", "district")) %>%
    mutate(synth_votes = pct * district_pop) %>%
    group_by(state) %>%
    mutate(state_vote = sum(synth_votes)) %>%
    group_by(state, candidate) %>%
    summarise(state_pct = sum(synth_votes) / mean(state_vote)) %>%
    mutate(district = "At-large") %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(met_threshold = state_pct >= 0.15,
           state_pct_valid = (state_pct*met_threshold) / sum(state_pct*met_threshold),
           candidate_delegates = delegates * state_pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate, pct = state_pct, pct_valid = state_pct_valid, candidate_delegates) %>%
    arrange(district, candidate) %>%
    bind_rows(district_sim_delegates) %>%
    mutate(sim_id = i)
}

# How much did the candidates overperform in Washington?
wa_overperformance <- wa_state_percentages_adj %>%
  left_join(state_averages %>% filter(state == "Washington") %>% dplyr::select(candidate, state_avg), by = c("candidate")) %>%
  mutate(overperformance = state_pct - (state_avg/100 + 0.25*overperformance*bounce_factor)) %>%
  dplyr::select(sim_id, candidate, wa_overperformance = overperformance) 

# Mini Tuesday overperformances ####
minitues_overperformance <- mi_overperformance %>%
  left_join(ms_overperformance, by = c("sim_id", "candidate")) %>%
  left_join(mo_overperformance, by = c("sim_id", "candidate")) %>%
  left_join(oh_overperformance, by = c("sim_id", "candidate")) %>%
  left_join(wa_overperformance, by = c("sim_id", "candidate")) %>%
  mutate(overperformance = ((14^2)*mi_overperformance + (4^2)*ms_overperformance + (8^2)*mo_overperformance + (16^2)*oh_overperformance +
                              (10^2)*wa_overperformance) / (14^2 + 4^2 + 8^2 + 16^2 + 10^2)) %>%
  dplyr::select(sim_id, candidate, overperformance) %>%
  na.omit()

minitues_overperformance_matrix <- minitues_overperformance %>%
  spread(candidate, overperformance) %>%
  ungroup() %>%
  dplyr::select(-sim_id) %>%
  as.matrix()

# Mini Tuesday dropouts ####
## What share of the delegates do candidates have?
mini_tuesday_sims <- bind_rows(ia_sims, nh_sims, nv_sims, sc_sims, al_sims, ca_sims, co_sims, ma_sims, mn_sims, nc_sims, ok_sims, tn_sims,
                                tx_sims, ut_sims, va_sims, me_sims, id_sims, mi_sims, ms_sims, mo_sims, nd_sims, oh_sims, wa_sims)

delegate_percentage <- mini_tuesday_sims %>% 
  group_by(sim_id, candidate) %>% 
  summarise(delegates = sum(candidate_delegates)) %>% 
  mutate(delegate_pct = delegates / sum(delegates)) 

low_delegates_mini_tuesday <- delegate_percentage %>%
  mutate(low_delegates = delegate_pct < 0.07) %>%
  dplyr::select(-delegates, -delegate_pct) %>%
  spread(candidate, low_delegates) %>%
  ungroup()

minitues_stayin_matrix <- (!(low_delegates_mini_tuesday %>%
                               dplyr::select(-sim_id) %>%
                               as.matrix()) | (minitues_overperformance_matrix >= 0.05)) &
  (minitues_overperformance_matrix > -0.2) & (st_stayin_matrix)

minitues_second_choice_division <- minitues_stayin_matrix %>%
  as.data.frame() %>%
  melt(id.vars = NULL, variable.name = "candidate", value.name = "stay_in") %>%
  mutate(candidate = as.character(candidate),
         sim_id = rep(1:n_sims, 8)) %>%
  as.tbl() %>%
  left_join(second_choice, by = c("candidate" = "first_choice"))


# Northern Marianas (14 Mar 2020) ####
na_start_time <- Sys.time()
# Shortcut since I know nothing about what the situation looks like on the ground: 
### Draw vote shares
na_dirichlet_params <- as_dirichlet_params

na_state_percentages <- rdirichlet(n_sims, na_dirichlet_params) %>%
  as.data.frame() %>%
  dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, steyer = V6, warren = V7, yang = V8) %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

na_sims <- vector("list", n_sims)

### Simulate
na_sims <- foreach(i = 1:n_sims, .packages = c("Hmisc", "dplyr", "reshape2", "tidyr")) %dopar% {
  this_sim_state_percentage <- na_state_percentages %>%
    filter(sim_id == i)
  
  second_choice_sim <- second_choice %>%
    melt(id.vars = "first_choice", variable.name = "second_choice", value.name = "pct") %>%
    mutate(alpha = 50 * pct / (1 - pct),
           second_pct_sim = rbeta(n(), shape1 = alpha, shape2 = 30)) %>%
    dplyr::select(first_choice, second_choice, second_pct_sim)
  
  district_sim_pct <- this_sim_state_percentage %>%
    mutate(met_threshold = state_pct >= 0.15)
  
  state_sim_delegates <- district_sim_pct %>%
    merge(second_choice_sim, by.x = "candidate", by.y = "first_choice", all.x = TRUE) %>%
    merge(district_sim_pct, by.x = "second_choice", by.y = "candidate") %>%
    mutate(pct_from_second_choice = state_pct.y * second_pct_sim * (second_choice != candidate) * (met_threshold.x) * (!met_threshold.y)) %>%
    group_by(candidate, state_pct.x, met_threshold = met_threshold.x) %>%
    summarise(pct_from_second_choice = sum(pct_from_second_choice)) %>%
    ungroup() %>%
    mutate(pct_valid = (state_pct.x + pct_from_second_choice)*met_threshold) %>% 
    mutate(pct_valid = pct_valid / sum(pct_valid),
           state = "Northern Marianas",
           district = "At-large") %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(candidate_delegates = delegates * pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, district, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate, pct = state_pct.x, pct_valid, candidate_delegates) %>%
    arrange(district, candidate) %>%
    mutate(sim_id = i)
}

# St. Paddy's Day primary (17 Mar 2020) ####
bounce_factor <- tibble(sim_id = 1:n_sims, bounce_factor = rbeta(n_sims, shape1 = 6, shape2 = 4))

overperformance_bounce <- minitues_overperformance %>%
  left_join(bounce_factor, by = "sim_id") %>%
  mutate(bounce = overperformance * bounce_factor)

# Arizona ####
az_start_time <- Sys.time()
# Draw votes
sim_demographics_table <- district_demographics %>%
  filter(state == "Arizona") %>%
  mutate(prob = predict(dem_primary_logit, newdata = ., type = "response"),
         n_vote = round(pop * prob * turnout * 0.05)) %>%
  dplyr::select(-pop, -prob)

sim_demographics <- sim_demographics_table %>%
  lapply(rep, sim_demographics_table$n_vote) %>%
  as.data.frame() 

state_marginal_demographics <- shapeStateDemo("Arizona")

az_averages_over_time <- state_averages_over_time_unsmoothed %>%
  filter(state == "Arizona", as.numeric(today() - median_date) <= 1) %>%
  mutate(age = as.numeric(today() - median_date),
         nreps = 2 - age) 

az_dirichlet_params <- (az_averages_over_time %>%
                          lapply(rep, az_averages_over_time$nreps) %>%
                          as.data.frame() %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          dplyr::select(-median_date) %>%
                          group_by(candidate) %>%
                          mutate(i = 1:n()) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-i) %>%
                          as.matrix() %>%
                          diri.est())$param

az_dirichlet_params <- az_dirichlet_params / 2
az_dirichlet_params[8] <- 0

az_state_percentages <- (rdirichlet(n_sims, az_dirichlet_params) %>%
                            as.data.frame() %>%
                            dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, 
                                          steyer = V6, warren = V7, yang = V8)) %>%
  apply(MARGIN = 1, FUN = function(x) x / sum(x)) %>%
  t() %>%
  as.data.frame() %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

az_state_percentages_adj <- az_state_percentages %>%
  left_join(overperformance_bounce, by = c("sim_id", "candidate")) %>%
  mutate(state_pct = pmax(state_pct + bounce, 0)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct)) %>%
  left_join(minitues_second_choice_division, by = c("candidate", "sim_id")) %>%
  melt(id.vars = c("sim_id", "candidate", "state_pct", "overperformance", "bounce_factor", "stay_in", "bounce"), 
       variable.name = "second_choice", value.name = "pct") %>%
  mutate(pct = pct*state_pct*(!stay_in),
         state_pct = case_when(candidate == second_choice ~ state_pct,
                               candidate != second_choice ~ pct)) %>%
  arrange(sim_id, second_choice, candidate)

overperformance_stats <- az_state_percentages_adj %>%
  group_by(sim_id, candidate) %>%
  summarise(overperformance = mean(overperformance),
            bounce_factor = mean(bounce_factor))

az_state_percentages_adj <- az_state_percentages_adj %>%
  group_by(sim_id, second_choice) %>%
  summarise(state_pct = sum(state_pct)) %>%
  dplyr::select(everything(), candidate = second_choice) %>%
  left_join(minitues_stayin_matrix %>% melt(id.vars = NULL, value.name = "stay_in") %>%
              dplyr::select(sim_id = Var1, candidate = Var2, stay_in), by = c("sim_id", "candidate")) %>%
  mutate(state_pct = state_pct * stay_in) %>%
  left_join(overperformance_stats, by = c("sim_id", "candidate"))

az_sims <- vector("list", n_sims)

az_sims <- foreach(i = 1:n_sims, .packages = c("Hmisc", "dplyr", "reshape2", "tidyr")) %dopar% {
  demographic_lean_sim <- demographic_leans %>%
    mutate(lean = rnorm(n(), mean = avg_lean, sd = lean_se)) %>%
    dplyr::select(candidate, demographic_var, demographic, lean)
  
  # This simulation's national percentages
  this_sim_state_percentage <- az_state_percentages_adj %>%
    filter(sim_id == i)
  
  # Apply simulated leans to state average
  state_demographic_sim <- demoCandidateBreakdown("Arizona", this_sim_state_percentage, demographic_lean_sim, 
                                                  state_marginal_demographics)
  
  # Turn these into probabilities
  gender_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("men", "women")) %>%
    dplyr::select(candidate, gender = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(gender = case_when(gender == "men" ~ "Male",
                              gender == "women" ~ "Female"))
  
  names(gender_breakdown)[-1] <- paste0("gender_", names(gender_breakdown))[-1]
  
  race_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("white", "black", "latino")) %>%
    dplyr::select(candidate, race = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(race = case_when(race == "white" ~ "White",
                            race == "black" ~ "Black",
                            race == "latino" ~ "Latino"))
  
  names(race_breakdown)[-1] <- paste0("race_", names(race_breakdown))[-1]
  
  age_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("age_1829", "age_3044", "age_4564", "age_65")) %>%
    dplyr::select(candidate, age = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(age = case_when(age == "age_1829" ~ "18-29",
                           age == "age_3044" ~ "30-44",
                           age == "age_4564" ~ "45-64",
                           age == "age_65" ~ "65+"))
  
  names(age_breakdown)[-1] <- paste0("age_", names(age_breakdown)[-1])
  
  education_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("college", "no_college")) %>%
    dplyr::select(candidate, education = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(education = case_when(education == "college" ~ "College",
                                 education == "no_college" ~ "Less than college"))
  
  names(education_breakdown)[-1] <- paste0("education_", names(education_breakdown)[-1])
  
  synth_pop <- sim_demographics %>%
    mutate(rand = runif(n()),
           age_regroup = case_when(age == "18-24" ~ "18-29",
                                   age == "25-34" & rand < 0.5 ~ "18-29",
                                   age == "25-34" & rand >= 0.5 ~ "30-44",
                                   age == "35-44" ~ "30-44",
                                   age == "45-64" ~ "45-64",
                                   age == "65+" ~ "65+"),
           id = 1:n()) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age = age_regroup, education) %>%
    mutate_if(is.factor, as.character) %>%
    left_join(gender_breakdown, by = "gender") %>%
    left_join(race_breakdown, by = "race") %>%
    left_join(age_breakdown, by = "age") %>%
    left_join(education_breakdown, by = "education") %>%
    mutate(biden_prob = gender_biden + race_biden + age_biden + education_biden,
           bloomberg_prob = gender_bloomberg + race_bloomberg + age_bloomberg + education_bloomberg,
           buttigieg_prob = gender_buttigieg + race_buttigieg + age_buttigieg + education_buttigieg,
           klobuchar_prob = gender_klobuchar + race_klobuchar + age_klobuchar + education_klobuchar,
           sanders_prob = gender_sanders + race_sanders + age_sanders + education_sanders,
           steyer_prob = gender_steyer + race_steyer + age_steyer + education_steyer,
           warren_prob = gender_warren + race_warren + age_warren + education_warren,
           yang_prob = gender_yang + race_yang + age_yang + education_yang) %>%
    mutate_at(vars(c("biden_prob", "bloomberg_prob", "buttigieg_prob", "klobuchar_prob", "sanders_prob", "steyer_prob", "warren_prob", 
                     "yang_prob")),
              function(x) pmax(x/100, 0)) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age, education, biden_prob, 
                  bloomberg_prob, buttigieg_prob, klobuchar_prob, sanders_prob, steyer_prob, warren_prob, yang_prob) %>%
    melt(id.vars = c("id", "state", "district", "primary_type", "district_delegates", "gender", "race", "age", "education"),
         variable.name = "candidate_prob", value.name = "prob") %>%
    arrange(id, candidate_prob) %>%
    group_by(id) %>%
    mutate(prob = prob / sum(prob),
           cum_prob = cumsum(prob)) %>%
    dplyr::select(-prob) %>%
    ungroup() %>%
    spread(candidate_prob, cum_prob) %>%
    mutate(rand = runif(n()),
           vote = case_when(rand <= biden_prob ~ "biden",
                            rand > biden_prob & rand <= bloomberg_prob ~ "bloomberg",
                            rand > bloomberg_prob & rand <= buttigieg_prob ~ "buttigieg",
                            rand > buttigieg_prob & rand <= klobuchar_prob ~ "klobuchar",
                            rand > klobuchar_prob & rand <= sanders_prob ~ "sanders",
                            rand > sanders_prob & rand <= steyer_prob ~ "steyer",
                            rand > steyer_prob & rand <= warren_prob ~ "warren",
                            rand > warren_prob ~ "yang")) %>%
    as.tbl()
  
  synth_district_pop <- synth_pop %>%
    group_by(state, district) %>%
    summarise(district_pop = n())
  
  district_sim_delegates <- synth_pop %>%
    group_by(state, district) %>%
    mutate(district_total = n()) %>%
    group_by(state, district, vote) %>%
    summarise(pct = n() / mean(district_total)) %>%
    ungroup() %>%
    mutate(met_threshold = pct >= 0.15) %>% 
    group_by(state, district) %>%
    mutate(pct_valid = (pct*met_threshold) / sum(pct*met_threshold)) %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(candidate_delegates = delegates * pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, district, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate = vote, pct, pct_valid, candidate_delegates) %>%
    arrange(district, candidate)
  
  state_sim_delegates <- district_sim_delegates %>%
    left_join(synth_district_pop, by = c("state", "district")) %>%
    mutate(synth_votes = pct * district_pop) %>%
    group_by(state) %>%
    mutate(state_vote = sum(synth_votes)) %>%
    group_by(state, candidate) %>%
    summarise(state_pct = sum(synth_votes) / mean(state_vote)) %>%
    mutate(district = "At-large") %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(met_threshold = state_pct >= 0.15,
           state_pct_valid = (state_pct*met_threshold) / sum(state_pct*met_threshold),
           candidate_delegates = delegates * state_pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate, pct = state_pct, pct_valid = state_pct_valid, candidate_delegates) %>%
    arrange(district, candidate) %>%
    bind_rows(district_sim_delegates) %>%
    mutate(sim_id = i)
}

# How much did the candidates overperform in Arizona?
az_overperformance <- az_state_percentages_adj %>%
  left_join(state_averages %>% filter(state == "Arizona") %>% dplyr::select(candidate, state_avg), by = c("candidate")) %>%
  mutate(overperformance = state_pct - (state_avg/100 + 0.25*overperformance*bounce_factor)) %>%
  dplyr::select(sim_id, candidate, az_overperformance = overperformance) 

# Florida ####
fl_start_time <- Sys.time()
# Draw votes
sim_demographics_table <- district_demographics %>%
  filter(state == "Florida") %>%
  mutate(prob = predict(dem_primary_logit, newdata = ., type = "response"),
         n_vote = round(pop * prob * turnout * 0.05)) %>%
  dplyr::select(-pop, -prob)

sim_demographics <- sim_demographics_table %>%
  lapply(rep, sim_demographics_table$n_vote) %>%
  as.data.frame() 

state_marginal_demographics <- shapeStateDemo("Florida")

fl_averages_over_time <- state_averages_over_time_unsmoothed %>%
  filter(state == "Florida", as.numeric(today() - median_date) <= 1) %>%
  mutate(age = as.numeric(today() - median_date),
         nreps = 2 - age)

fl_dirichlet_params <- (fl_averages_over_time %>%
                          lapply(rep, fl_averages_over_time$nreps) %>%
                          as.data.frame() %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          dplyr::select(-median_date) %>%
                          group_by(candidate) %>%
                          mutate(i = 1:n()) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-i) %>%
                          as.matrix() %>%
                          diri.est())$param

fl_dirichlet_params <- fl_dirichlet_params / 2
fl_dirichlet_params[8] <- 0

fl_state_percentages <- (rdirichlet(n_sims, fl_dirichlet_params) %>%
                            as.data.frame() %>%
                            dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, 
                                          steyer = V6, warren = V7, yang = V8)) %>%
  apply(MARGIN = 1, FUN = function(x) x / sum(x)) %>%
  t() %>%
  as.data.frame() %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

fl_state_percentages_adj <- fl_state_percentages %>%
  left_join(overperformance_bounce, by = c("sim_id", "candidate")) %>%
  mutate(state_pct = pmax(state_pct + bounce, 0)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct)) %>%
  left_join(minitues_second_choice_division, by = c("candidate", "sim_id")) %>%
  melt(id.vars = c("sim_id", "candidate", "state_pct", "overperformance", "bounce_factor", "stay_in", "bounce"), 
       variable.name = "second_choice", value.name = "pct") %>%
  mutate(pct = pct*state_pct*(!stay_in),
         state_pct = case_when(candidate == second_choice ~ state_pct,
                               candidate != second_choice ~ pct)) %>%
  arrange(sim_id, second_choice, candidate)

overperformance_stats <- fl_state_percentages_adj %>%
  group_by(sim_id, candidate) %>%
  summarise(overperformance = mean(overperformance),
            bounce_factor = mean(bounce_factor))

fl_state_percentages_adj <- fl_state_percentages_adj %>%
  group_by(sim_id, second_choice) %>%
  summarise(state_pct = sum(state_pct)) %>%
  dplyr::select(everything(), candidate = second_choice) %>%
  left_join(minitues_stayin_matrix %>% melt(id.vars = NULL, value.name = "stay_in") %>%
              dplyr::select(sim_id = Var1, candidate = Var2, stay_in), by = c("sim_id", "candidate")) %>%
  mutate(state_pct = state_pct * stay_in) %>%
  left_join(overperformance_stats, by = c("sim_id", "candidate"))

fl_sims <- vector("list", n_sims)

fl_sims <- foreach(i = 1:n_sims, .packages = c("Hmisc", "dplyr", "reshape2", "tidyr")) %dopar% {
  demographic_lean_sim <- demographic_leans %>%
    mutate(lean = rnorm(n(), mean = avg_lean, sd = lean_se)) %>%
    dplyr::select(candidate, demographic_var, demographic, lean)
  
  # This simulation's national percentages
  this_sim_state_percentage <- fl_state_percentages_adj %>%
    filter(sim_id == i)
  
  # Apply simulated leans to state average
  state_demographic_sim <- demoCandidateBreakdown("Florida", this_sim_state_percentage, demographic_lean_sim, 
                                                  state_marginal_demographics)
  
  # Turn these into probabilities
  gender_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("men", "women")) %>%
    dplyr::select(candidate, gender = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(gender = case_when(gender == "men" ~ "Male",
                              gender == "women" ~ "Female"))
  
  names(gender_breakdown)[-1] <- paste0("gender_", names(gender_breakdown))[-1]
  
  race_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("white", "black", "latino")) %>%
    dplyr::select(candidate, race = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(race = case_when(race == "white" ~ "White",
                            race == "black" ~ "Black",
                            race == "latino" ~ "Latino"))
  
  names(race_breakdown)[-1] <- paste0("race_", names(race_breakdown))[-1]
  
  age_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("age_1829", "age_3044", "age_4564", "age_65")) %>%
    dplyr::select(candidate, age = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(age = case_when(age == "age_1829" ~ "18-29",
                           age == "age_3044" ~ "30-44",
                           age == "age_4564" ~ "45-64",
                           age == "age_65" ~ "65+"))
  
  names(age_breakdown)[-1] <- paste0("age_", names(age_breakdown)[-1])
  
  education_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("college", "no_college")) %>%
    dplyr::select(candidate, education = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(education = case_when(education == "college" ~ "College",
                                 education == "no_college" ~ "Less than college"))
  
  names(education_breakdown)[-1] <- paste0("education_", names(education_breakdown)[-1])
  
  synth_pop <- sim_demographics %>%
    mutate(rand = runif(n()),
           age_regroup = case_when(age == "18-24" ~ "18-29",
                                   age == "25-34" & rand < 0.5 ~ "18-29",
                                   age == "25-34" & rand >= 0.5 ~ "30-44",
                                   age == "35-44" ~ "30-44",
                                   age == "45-64" ~ "45-64",
                                   age == "65+" ~ "65+"),
           id = 1:n()) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age = age_regroup, education) %>%
    mutate_if(is.factor, as.character) %>%
    left_join(gender_breakdown, by = "gender") %>%
    left_join(race_breakdown, by = "race") %>%
    left_join(age_breakdown, by = "age") %>%
    left_join(education_breakdown, by = "education") %>%
    mutate(biden_prob = gender_biden + race_biden + age_biden + education_biden,
           bloomberg_prob = gender_bloomberg + race_bloomberg + age_bloomberg + education_bloomberg,
           buttigieg_prob = gender_buttigieg + race_buttigieg + age_buttigieg + education_buttigieg,
           klobuchar_prob = gender_klobuchar + race_klobuchar + age_klobuchar + education_klobuchar,
           sanders_prob = gender_sanders + race_sanders + age_sanders + education_sanders,
           steyer_prob = gender_steyer + race_steyer + age_steyer + education_steyer,
           warren_prob = gender_warren + race_warren + age_warren + education_warren,
           yang_prob = gender_yang + race_yang + age_yang + education_yang) %>%
    mutate_at(vars(c("biden_prob", "bloomberg_prob", "buttigieg_prob", "klobuchar_prob", "sanders_prob", "steyer_prob", "warren_prob", 
                     "yang_prob")),
              function(x) pmax(x/100, 0)) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age, education, biden_prob, 
                  bloomberg_prob, buttigieg_prob, klobuchar_prob, sanders_prob, steyer_prob, warren_prob, yang_prob) %>%
    melt(id.vars = c("id", "state", "district", "primary_type", "district_delegates", "gender", "race", "age", "education"),
         variable.name = "candidate_prob", value.name = "prob") %>%
    arrange(id, candidate_prob) %>%
    group_by(id) %>%
    mutate(prob = prob / sum(prob),
           cum_prob = cumsum(prob)) %>%
    dplyr::select(-prob) %>%
    ungroup() %>%
    spread(candidate_prob, cum_prob) %>%
    mutate(rand = runif(n()),
           vote = case_when(rand <= biden_prob ~ "biden",
                            rand > biden_prob & rand <= bloomberg_prob ~ "bloomberg",
                            rand > bloomberg_prob & rand <= buttigieg_prob ~ "buttigieg",
                            rand > buttigieg_prob & rand <= klobuchar_prob ~ "klobuchar",
                            rand > klobuchar_prob & rand <= sanders_prob ~ "sanders",
                            rand > sanders_prob & rand <= steyer_prob ~ "steyer",
                            rand > steyer_prob & rand <= warren_prob ~ "warren",
                            rand > warren_prob ~ "yang")) %>%
    as.tbl()
  
  synth_district_pop <- synth_pop %>%
    group_by(state, district) %>%
    summarise(district_pop = n())
  
  district_sim_delegates <- synth_pop %>%
    group_by(state, district) %>%
    mutate(district_total = n()) %>%
    group_by(state, district, vote) %>%
    summarise(pct = n() / mean(district_total)) %>%
    ungroup() %>%
    mutate(met_threshold = pct >= 0.15) %>% 
    group_by(state, district) %>%
    mutate(pct_valid = (pct*met_threshold) / sum(pct*met_threshold)) %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(candidate_delegates = delegates * pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, district, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate = vote, pct, pct_valid, candidate_delegates) %>%
    arrange(district, candidate)
  
  state_sim_delegates <- district_sim_delegates %>%
    left_join(synth_district_pop, by = c("state", "district")) %>%
    mutate(synth_votes = pct * district_pop) %>%
    group_by(state) %>%
    mutate(state_vote = sum(synth_votes)) %>%
    group_by(state, candidate) %>%
    summarise(state_pct = sum(synth_votes) / mean(state_vote)) %>%
    mutate(district = "At-large") %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(met_threshold = state_pct >= 0.15,
           state_pct_valid = (state_pct*met_threshold) / sum(state_pct*met_threshold),
           candidate_delegates = delegates * state_pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate, pct = state_pct, pct_valid = state_pct_valid, candidate_delegates) %>%
    arrange(district, candidate) %>%
    bind_rows(district_sim_delegates) %>%
    mutate(sim_id = i)
}

# How much did the candidates overperform in Florida?
fl_overperformance <- fl_state_percentages_adj %>%
  left_join(state_averages %>% filter(state == "Florida") %>% dplyr::select(candidate, state_avg), by = c("candidate")) %>%
  mutate(overperformance = state_pct - (state_avg/100 + 0.25*overperformance*bounce_factor)) %>%
  dplyr::select(sim_id, candidate, fl_overperformance = overperformance)


# Illinois ####
il_start_time <- Sys.time()
# Draw votes
sim_demographics_table <- district_demographics %>%
  filter(state == "Illinois") %>%
  mutate(prob = predict(dem_primary_logit, newdata = ., type = "response"),
         n_vote = round(pop * prob * turnout * 0.05)) %>%
  dplyr::select(-pop, -prob)

sim_demographics <- sim_demographics_table %>%
  lapply(rep, sim_demographics_table$n_vote) %>%
  as.data.frame() 

state_marginal_demographics <- shapeStateDemo("Illinois")

il_averages_over_time <- state_averages_over_time_unsmoothed %>%
  filter(state == "Illinois", as.numeric(today() - median_date) <= 1) %>%
  mutate(age = as.numeric(today() - median_date),
         nreps = 2 - age)

il_dirichlet_params <- (il_averages_over_time %>%
                          lapply(rep, il_averages_over_time$nreps) %>%
                          as.data.frame() %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          dplyr::select(-median_date) %>%
                          group_by(candidate) %>%
                          mutate(i = 1:n()) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-i) %>%
                          as.matrix() %>%
                          diri.est())$param

il_dirichlet_params <- il_dirichlet_params / 2
il_dirichlet_params[8] <- 0

il_state_percentages <- (rdirichlet(n_sims, il_dirichlet_params) %>%
                            as.data.frame() %>%
                            dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, 
                                          steyer = V6, warren = V7, yang = V8)) %>%
  apply(MARGIN = 1, FUN = function(x) x / sum(x)) %>%
  t() %>%
  as.data.frame() %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

il_state_percentages_adj <- il_state_percentages %>%
  left_join(overperformance_bounce, by = c("sim_id", "candidate")) %>%
  mutate(state_pct = pmax(state_pct + bounce, 0)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct)) %>%
  left_join(minitues_second_choice_division, by = c("candidate", "sim_id")) %>%
  melt(id.vars = c("sim_id", "candidate", "state_pct", "overperformance", "bounce_factor", "stay_in", "bounce"), 
       variable.name = "second_choice", value.name = "pct") %>%
  mutate(pct = pct*state_pct*(!stay_in),
         state_pct = case_when(candidate == second_choice ~ state_pct,
                               candidate != second_choice ~ pct)) %>%
  arrange(sim_id, second_choice, candidate)

overperformance_stats <- il_state_percentages_adj %>%
  group_by(sim_id, candidate) %>%
  summarise(overperformance = mean(overperformance),
            bounce_factor = mean(bounce_factor))

il_state_percentages_adj <- il_state_percentages_adj %>%
  group_by(sim_id, second_choice) %>%
  summarise(state_pct = sum(state_pct)) %>%
  dplyr::select(everything(), candidate = second_choice) %>%
  left_join(minitues_stayin_matrix %>% melt(id.vars = NULL, value.name = "stay_in") %>%
              dplyr::select(sim_id = Var1, candidate = Var2, stay_in), by = c("sim_id", "candidate")) %>%
  mutate(state_pct = state_pct * stay_in) %>%
  left_join(overperformance_stats, by = c("sim_id", "candidate"))

il_sims <- vector("list", n_sims)

il_sims <- foreach(i = 1:n_sims, .packages = c("Hmisc", "dplyr", "reshape2", "tidyr")) %dopar% {
  demographic_lean_sim <- demographic_leans %>%
    mutate(lean = rnorm(n(), mean = avg_lean, sd = lean_se)) %>%
    dplyr::select(candidate, demographic_var, demographic, lean)
  
  # This simulation's national percentages
  this_sim_state_percentage <- il_state_percentages_adj %>%
    filter(sim_id == i)
  
  # Apply simulated leans to state average
  state_demographic_sim <- demoCandidateBreakdown("Illinois", this_sim_state_percentage, demographic_lean_sim, 
                                                  state_marginal_demographics)
  
  # Turn these into probabilities
  gender_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("men", "women")) %>%
    dplyr::select(candidate, gender = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(gender = case_when(gender == "men" ~ "Male",
                              gender == "women" ~ "Female"))
  
  names(gender_breakdown)[-1] <- paste0("gender_", names(gender_breakdown))[-1]
  
  race_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("white", "black", "latino")) %>%
    dplyr::select(candidate, race = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(race = case_when(race == "white" ~ "White",
                            race == "black" ~ "Black",
                            race == "latino" ~ "Latino"))
  
  names(race_breakdown)[-1] <- paste0("race_", names(race_breakdown))[-1]
  
  age_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("age_1829", "age_3044", "age_4564", "age_65")) %>%
    dplyr::select(candidate, age = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(age = case_when(age == "age_1829" ~ "18-29",
                           age == "age_3044" ~ "30-44",
                           age == "age_4564" ~ "45-64",
                           age == "age_65" ~ "65+"))
  
  names(age_breakdown)[-1] <- paste0("age_", names(age_breakdown)[-1])
  
  education_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("college", "no_college")) %>%
    dplyr::select(candidate, education = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(education = case_when(education == "college" ~ "College",
                                 education == "no_college" ~ "Less than college"))
  
  names(education_breakdown)[-1] <- paste0("education_", names(education_breakdown)[-1])
  
  synth_pop <- sim_demographics %>%
    mutate(rand = runif(n()),
           age_regroup = case_when(age == "18-24" ~ "18-29",
                                   age == "25-34" & rand < 0.5 ~ "18-29",
                                   age == "25-34" & rand >= 0.5 ~ "30-44",
                                   age == "35-44" ~ "30-44",
                                   age == "45-64" ~ "45-64",
                                   age == "65+" ~ "65+"),
           id = 1:n()) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age = age_regroup, education) %>%
    mutate_if(is.factor, as.character) %>%
    left_join(gender_breakdown, by = "gender") %>%
    left_join(race_breakdown, by = "race") %>%
    left_join(age_breakdown, by = "age") %>%
    left_join(education_breakdown, by = "education") %>%
    mutate(biden_prob = gender_biden + race_biden + age_biden + education_biden,
           bloomberg_prob = gender_bloomberg + race_bloomberg + age_bloomberg + education_bloomberg,
           buttigieg_prob = gender_buttigieg + race_buttigieg + age_buttigieg + education_buttigieg,
           klobuchar_prob = gender_klobuchar + race_klobuchar + age_klobuchar + education_klobuchar,
           sanders_prob = gender_sanders + race_sanders + age_sanders + education_sanders,
           steyer_prob = gender_steyer + race_steyer + age_steyer + education_steyer,
           warren_prob = gender_warren + race_warren + age_warren + education_warren,
           yang_prob = gender_yang + race_yang + age_yang + education_yang) %>%
    mutate_at(vars(c("biden_prob", "bloomberg_prob", "buttigieg_prob", "klobuchar_prob", "sanders_prob", "steyer_prob", "warren_prob", 
                     "yang_prob")),
              function(x) pmax(x/100, 0)) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age, education, biden_prob, 
                  bloomberg_prob, buttigieg_prob, klobuchar_prob, sanders_prob, steyer_prob, warren_prob, yang_prob) %>%
    melt(id.vars = c("id", "state", "district", "primary_type", "district_delegates", "gender", "race", "age", "education"),
         variable.name = "candidate_prob", value.name = "prob") %>%
    arrange(id, candidate_prob) %>%
    group_by(id) %>%
    mutate(prob = prob / sum(prob),
           cum_prob = cumsum(prob)) %>%
    dplyr::select(-prob) %>%
    ungroup() %>%
    spread(candidate_prob, cum_prob) %>%
    mutate(rand = runif(n()),
           vote = case_when(rand <= biden_prob ~ "biden",
                            rand > biden_prob & rand <= bloomberg_prob ~ "bloomberg",
                            rand > bloomberg_prob & rand <= buttigieg_prob ~ "buttigieg",
                            rand > buttigieg_prob & rand <= klobuchar_prob ~ "klobuchar",
                            rand > klobuchar_prob & rand <= sanders_prob ~ "sanders",
                            rand > sanders_prob & rand <= steyer_prob ~ "steyer",
                            rand > steyer_prob & rand <= warren_prob ~ "warren",
                            rand > warren_prob ~ "yang")) %>%
    as.tbl()
  
  synth_district_pop <- synth_pop %>%
    group_by(state, district) %>%
    summarise(district_pop = n())
  
  district_sim_delegates <- synth_pop %>%
    group_by(state, district) %>%
    mutate(district_total = n()) %>%
    group_by(state, district, vote) %>%
    summarise(pct = n() / mean(district_total)) %>%
    ungroup() %>%
    mutate(met_threshold = pct >= 0.15) %>% 
    group_by(state, district) %>%
    mutate(pct_valid = (pct*met_threshold) / sum(pct*met_threshold)) %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(candidate_delegates = delegates * pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, district, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate = vote, pct, pct_valid, candidate_delegates) %>%
    arrange(district, candidate)
  
  state_sim_delegates <- district_sim_delegates %>%
    left_join(synth_district_pop, by = c("state", "district")) %>%
    mutate(synth_votes = pct * district_pop) %>%
    group_by(state) %>%
    mutate(state_vote = sum(synth_votes)) %>%
    group_by(state, candidate) %>%
    summarise(state_pct = sum(synth_votes) / mean(state_vote)) %>%
    mutate(district = "At-large") %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(met_threshold = state_pct >= 0.15,
           state_pct_valid = (state_pct*met_threshold) / sum(state_pct*met_threshold),
           candidate_delegates = delegates * state_pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate, pct = state_pct, pct_valid = state_pct_valid, candidate_delegates) %>%
    arrange(district, candidate) %>%
    bind_rows(district_sim_delegates) %>%
    mutate(sim_id = i)
}

# How much did the candidates overperform in Illinois?
il_overperformance <- il_state_percentages_adj %>%
  left_join(state_averages %>% filter(state == "Illinois") %>% dplyr::select(candidate, state_avg), by = c("candidate")) %>%
  mutate(overperformance = state_pct - (state_avg/100 + 0.25*overperformance*bounce_factor)) %>%
  dplyr::select(sim_id, candidate, il_overperformance = overperformance) 


# St. Paddy's Day overperformances ####
st_paddy_overperformance <- az_overperformance %>%
  left_join(fl_overperformance, by = c("sim_id", "candidate")) %>%
  left_join(il_overperformance, by = c("sim_id", "candidate")) %>%
  mutate(overperformance = ((9^2)*az_overperformance + (27^2)*fl_overperformance + (18^2)*il_overperformance) / (9^2 + 27^2 + 18^2)) %>%
  dplyr::select(sim_id, candidate, overperformance)

st_paddy_overperformance_matrix <- st_paddy_overperformance %>%
  spread(candidate, overperformance) %>%
  ungroup() %>%
  dplyr::select(-sim_id) %>%
  as.matrix()

# St. Paddy's Day dropouts ####
## What share of the delegates do candidates have?
st_paddy_sims <- bind_rows(ia_sims, nh_sims, nv_sims, sc_sims, al_sims, ca_sims, co_sims, ma_sims, mn_sims, nc_sims, ok_sims, tn_sims,
                           tx_sims, ut_sims, va_sims, me_sims, id_sims, mi_sims, ms_sims, mo_sims, nd_sims, oh_sims, wa_sims, na_sims,
                           az_sims, fl_sims, il_sims)

delegate_percentage <- st_paddy_sims %>% 
  group_by(sim_id, candidate) %>% 
  summarise(delegates = sum(candidate_delegates)) %>% 
  mutate(delegate_pct = delegates / sum(delegates)) 

low_delegates_st_paddy <- delegate_percentage %>%
  mutate(low_delegates = delegate_pct < 0.09) %>%
  dplyr::select(-delegates, -delegate_pct) %>%
  spread(candidate, low_delegates) %>%
  ungroup()

st_paddy_stayin_matrix <- (!(low_delegates_st_paddy %>%
                               dplyr::select(-sim_id) %>%
                               as.matrix()) | (st_paddy_overperformance_matrix >= 0.05)) &
  (st_paddy_overperformance_matrix > -0.2) & minitues_stayin_matrix

st_paddy_second_choice_division <- st_paddy_stayin_matrix %>%
  as.data.frame() %>%
  melt(id.vars = NULL, variable.name = "candidate", value.name = "stay_in") %>%
  mutate(candidate = as.character(candidate),
         sim_id = rep(1:n_sims, 8)) %>%
  as.tbl() %>%
  left_join(second_choice, by = c("candidate" = "first_choice"))

# Georgia (24 Mar 2020) ####
bounce_factor <- tibble(sim_id = 1:n_sims, bounce_factor = rbeta(n_sims, shape1 = 5, shape2 = 3))

overperformance_bounce <- st_paddy_overperformance %>%
  left_join(bounce_factor, by = "sim_id") %>%
  mutate(bounce = overperformance * bounce_factor)

ga_start_time <- Sys.time()
# Draw votes
sim_demographics_table <- district_demographics %>%
  filter(state == "Georgia") %>%
  mutate(prob = predict(dem_primary_logit, newdata = ., type = "response"),
         n_vote = round(pop * prob * turnout * 0.05)) %>%
  dplyr::select(-pop, -prob)

sim_demographics <- sim_demographics_table %>%
  lapply(rep, sim_demographics_table$n_vote) %>%
  as.data.frame() 

state_marginal_demographics <- shapeStateDemo("Georgia")

ga_averages_over_time <- state_averages_over_time_unsmoothed %>%
  filter(state == "Georgia", as.numeric(today() - median_date) <= 1) %>%
  mutate(age = as.numeric(today() - median_date),
         nreps = 2 - age)

ga_dirichlet_params <- (ga_averages_over_time %>%
                          lapply(rep, ga_averages_over_time$nreps) %>%
                          as.data.frame() %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          dplyr::select(-median_date) %>%
                          group_by(candidate) %>%
                          mutate(i = 1:n()) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-i) %>%
                          as.matrix() %>%
                          diri.est())$param

ga_dirichlet_params <- ga_dirichlet_params / 2
ga_dirichlet_params[8] <- 0

ga_state_percentages <- (rdirichlet(n_sims, ga_dirichlet_params) %>%
                            as.data.frame() %>%
                            dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, 
                                          steyer = V6, warren = V7, yang = V8)) %>%
  apply(MARGIN = 1, FUN = function(x) x / sum(x)) %>%
  t() %>%
  as.data.frame() %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

ga_state_percentages_adj <- ga_state_percentages %>%
  left_join(overperformance_bounce, by = c("sim_id", "candidate")) %>%
  mutate(state_pct = pmax(state_pct + bounce, 0)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct)) %>%
  left_join(st_paddy_second_choice_division, by = c("candidate", "sim_id")) %>%
  melt(id.vars = c("sim_id", "candidate", "state_pct", "overperformance", "bounce_factor", "stay_in", "bounce"), 
       variable.name = "second_choice", value.name = "pct") %>%
  mutate(pct = pct*state_pct*(!stay_in),
         state_pct = case_when(candidate == second_choice ~ state_pct,
                               candidate != second_choice ~ pct)) %>%
  arrange(sim_id, second_choice, candidate)

overperformance_stats <- ga_state_percentages_adj %>%
  group_by(sim_id, candidate) %>%
  summarise(overperformance = mean(overperformance),
            bounce_factor = mean(bounce_factor))

ga_state_percentages_adj <- ga_state_percentages_adj %>%
  group_by(sim_id, second_choice) %>%
  summarise(state_pct = sum(state_pct)) %>%
  dplyr::select(everything(), candidate = second_choice) %>%
  left_join(st_paddy_stayin_matrix %>% melt(id.vars = NULL, value.name = "stay_in") %>%
              dplyr::select(sim_id = Var1, candidate = Var2, stay_in), by = c("sim_id", "candidate")) %>%
  mutate(state_pct = state_pct * stay_in) %>%
  left_join(overperformance_stats, by = c("sim_id", "candidate"))

ga_sims <- vector("list", n_sims)

ga_sims <- foreach(i = 1:n_sims, .packages = c("Hmisc", "dplyr", "reshape2", "tidyr")) %dopar% {
  demographic_lean_sim <- demographic_leans %>%
    mutate(lean = rnorm(n(), mean = avg_lean, sd = lean_se)) %>%
    dplyr::select(candidate, demographic_var, demographic, lean)
  
  # This simulation's national percentages
  this_sim_state_percentage <- ga_state_percentages_adj %>%
    filter(sim_id == i)
  
  # Apply simulated leans to state average
  state_demographic_sim <- demoCandidateBreakdown("Georgia", this_sim_state_percentage, demographic_lean_sim, 
                                                  state_marginal_demographics)
  
  # Turn these into probabilities
  gender_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("men", "women")) %>%
    dplyr::select(candidate, gender = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(gender = case_when(gender == "men" ~ "Male",
                              gender == "women" ~ "Female"))
  
  names(gender_breakdown)[-1] <- paste0("gender_", names(gender_breakdown))[-1]
  
  race_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("white", "black", "latino")) %>%
    dplyr::select(candidate, race = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(race = case_when(race == "white" ~ "White",
                            race == "black" ~ "Black",
                            race == "latino" ~ "Latino"))
  
  names(race_breakdown)[-1] <- paste0("race_", names(race_breakdown))[-1]
  
  age_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("age_1829", "age_3044", "age_4564", "age_65")) %>%
    dplyr::select(candidate, age = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(age = case_when(age == "age_1829" ~ "18-29",
                           age == "age_3044" ~ "30-44",
                           age == "age_4564" ~ "45-64",
                           age == "age_65" ~ "65+"))
  
  names(age_breakdown)[-1] <- paste0("age_", names(age_breakdown)[-1])
  
  education_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("college", "no_college")) %>%
    dplyr::select(candidate, education = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(education = case_when(education == "college" ~ "College",
                                 education == "no_college" ~ "Less than college"))
  
  names(education_breakdown)[-1] <- paste0("education_", names(education_breakdown)[-1])
  
  synth_pop <- sim_demographics %>%
    mutate(rand = runif(n()),
           age_regroup = case_when(age == "18-24" ~ "18-29",
                                   age == "25-34" & rand < 0.5 ~ "18-29",
                                   age == "25-34" & rand >= 0.5 ~ "30-44",
                                   age == "35-44" ~ "30-44",
                                   age == "45-64" ~ "45-64",
                                   age == "65+" ~ "65+"),
           id = 1:n()) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age = age_regroup, education) %>%
    mutate_if(is.factor, as.character) %>%
    left_join(gender_breakdown, by = "gender") %>%
    left_join(race_breakdown, by = "race") %>%
    left_join(age_breakdown, by = "age") %>%
    left_join(education_breakdown, by = "education") %>%
    mutate(biden_prob = gender_biden + race_biden + age_biden + education_biden,
           bloomberg_prob = gender_bloomberg + race_bloomberg + age_bloomberg + education_bloomberg,
           buttigieg_prob = gender_buttigieg + race_buttigieg + age_buttigieg + education_buttigieg,
           klobuchar_prob = gender_klobuchar + race_klobuchar + age_klobuchar + education_klobuchar,
           sanders_prob = gender_sanders + race_sanders + age_sanders + education_sanders,
           steyer_prob = gender_steyer + race_steyer + age_steyer + education_steyer,
           warren_prob = gender_warren + race_warren + age_warren + education_warren,
           yang_prob = gender_yang + race_yang + age_yang + education_yang) %>%
    mutate_at(vars(c("biden_prob", "bloomberg_prob", "buttigieg_prob", "klobuchar_prob", "sanders_prob", "steyer_prob", "warren_prob", 
                     "yang_prob")),
              function(x) pmax(x/100, 0)) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age, education, biden_prob, 
                  bloomberg_prob, buttigieg_prob, klobuchar_prob, sanders_prob, steyer_prob, warren_prob, yang_prob) %>%
    melt(id.vars = c("id", "state", "district", "primary_type", "district_delegates", "gender", "race", "age", "education"),
         variable.name = "candidate_prob", value.name = "prob") %>%
    arrange(id, candidate_prob) %>%
    group_by(id) %>%
    mutate(prob = prob / sum(prob),
           cum_prob = cumsum(prob)) %>%
    dplyr::select(-prob) %>%
    ungroup() %>%
    spread(candidate_prob, cum_prob) %>%
    mutate(rand = runif(n()),
           vote = case_when(rand <= biden_prob ~ "biden",
                            rand > biden_prob & rand <= bloomberg_prob ~ "bloomberg",
                            rand > bloomberg_prob & rand <= buttigieg_prob ~ "buttigieg",
                            rand > buttigieg_prob & rand <= klobuchar_prob ~ "klobuchar",
                            rand > klobuchar_prob & rand <= sanders_prob ~ "sanders",
                            rand > sanders_prob & rand <= steyer_prob ~ "steyer",
                            rand > steyer_prob & rand <= warren_prob ~ "warren",
                            rand > warren_prob ~ "yang")) %>%
    as.tbl()
  
  synth_district_pop <- synth_pop %>%
    group_by(state, district) %>%
    summarise(district_pop = n())
  
  district_sim_delegates <- synth_pop %>%
    group_by(state, district) %>%
    mutate(district_total = n()) %>%
    group_by(state, district, vote) %>%
    summarise(pct = n() / mean(district_total)) %>%
    ungroup() %>%
    mutate(met_threshold = pct >= 0.15) %>% 
    group_by(state, district) %>%
    mutate(pct_valid = (pct*met_threshold) / sum(pct*met_threshold)) %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(candidate_delegates = delegates * pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, district, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate = vote, pct, pct_valid, candidate_delegates) %>%
    arrange(district, candidate)
  
  state_sim_delegates <- district_sim_delegates %>%
    left_join(synth_district_pop, by = c("state", "district")) %>%
    mutate(synth_votes = pct * district_pop) %>%
    group_by(state) %>%
    mutate(state_vote = sum(synth_votes)) %>%
    group_by(state, candidate) %>%
    summarise(state_pct = sum(synth_votes) / mean(state_vote)) %>%
    mutate(district = "At-large") %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(met_threshold = state_pct >= 0.15,
           state_pct_valid = (state_pct*met_threshold) / sum(state_pct*met_threshold),
           candidate_delegates = delegates * state_pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate, pct = state_pct, pct_valid = state_pct_valid, candidate_delegates) %>%
    arrange(district, candidate) %>%
    bind_rows(district_sim_delegates) %>%
    mutate(sim_id = i)
}

# How much did the candidates overperform in Georgia?
ga_overperformance <- ga_state_percentages_adj %>%
  left_join(state_averages %>% filter(state == "Georgia") %>% dplyr::select(candidate, state_avg), by = c("candidate")) %>%
  mutate(overperformance = state_pct - (state_avg/100 + 0.25*overperformance*bounce_factor)) %>%
  dplyr::select(sim_id, candidate, ga_overperformance = overperformance) 


# Puerto Rico (29 Mar 2020) ####
pr_start_time <- Sys.time()

bounce_factor <- tibble(sim_id = 1:n_sims, bounce_factor = rbeta(n_sims, shape1 = 4, shape2 = 4))

overperformance_bounce <- st_paddy_overperformance %>%
  left_join(bounce_factor, by = "sim_id") %>%
  mutate(bounce = overperformance * bounce_factor)

# Draw votes
sim_demographics_table <- district_demographics %>%
  filter(state == "Puerto Rico") %>%
  mutate(prob = predict(dem_primary_logit, newdata = ., type = "response"),
         n_vote = round(pop * prob * turnout * 0.05)) %>%
  dplyr::select(-pop, -prob) %>%
  filter(!is.na(district))

sim_demographics <- sim_demographics_table %>%
  lapply(rep, sim_demographics_table$n_vote) %>%
  as.data.frame() 
state_marginal_demographics <- shapeStateDemo("Puerto Rico")

# Hear me out--we're gonna use national polls first
pr_averages_over_time <- national_averages_adjusted %>%
  filter(as.numeric(today() - median_date) <= 1) %>%
  mutate(age = as.numeric(today() - median_date),
         nreps = 2 - age) %>%
  group_by(median_date) %>%
  mutate(pct = pct / sum(pct))

pr_dirichlet_params <- ((pr_averages_over_time %>%
                           lapply(rep, pr_averages_over_time$nreps) %>%
                           as.data.frame() %>%
                           dplyr::select(candidate, median_date, pct) %>%
                           na.omit() %>%
                           dplyr::select(-median_date) %>%
                           group_by(candidate) %>%
                           mutate(i = 1:n()) %>%
                           spread(candidate, pct) %>%
                           dplyr::select(-i) %>%
                           as.matrix() %>%
                           diri.est())$param)

pr_dirichlet_params <- 0.5 * pr_dirichlet_params / min(pr_dirichlet_params)
pr_dirichlet_params[8] <- 0

pr_state_percentages <- (rdirichlet(n_sims, pr_dirichlet_params) %>%
                            as.data.frame() %>%
                            dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, 
                                          steyer = V6, warren = V7, yang = V8)) %>%
  apply(MARGIN = 1, FUN = function(x) x / sum(x)) %>%
  t() %>%
  as.data.frame() %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

pr_state_percentages_adj <- pr_state_percentages %>%
  left_join(overperformance_bounce, by = c("sim_id", "candidate")) %>%
  mutate(state_pct = pmax(state_pct + bounce, 0)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct)) %>%
  left_join(st_paddy_second_choice_division, by = c("candidate", "sim_id")) %>%
  melt(id.vars = c("sim_id", "candidate", "state_pct", "overperformance", "bounce_factor", "stay_in", "bounce"), 
       variable.name = "second_choice", value.name = "pct") %>%
  mutate(pct = pct*state_pct*(!stay_in),
         state_pct = case_when(candidate == second_choice ~ state_pct,
                               candidate != second_choice ~ pct)) %>%
  arrange(sim_id, second_choice, candidate)

overperformance_stats <- pr_state_percentages_adj %>%
  group_by(sim_id, candidate) %>%
  summarise(overperformance = mean(overperformance),
            bounce_factor = mean(bounce_factor))

pr_state_percentages_adj <- pr_state_percentages_adj %>%
  group_by(sim_id, second_choice) %>%
  summarise(state_pct = sum(state_pct)) %>%
  dplyr::select(everything(), candidate = second_choice) %>%
  left_join(st_paddy_stayin_matrix %>% melt(id.vars = NULL, value.name = "stay_in") %>%
              dplyr::select(sim_id = Var1, candidate = Var2, stay_in), by = c("sim_id", "candidate")) %>%
  mutate(state_pct = state_pct * stay_in) %>%
  left_join(overperformance_stats, by = c("sim_id", "candidate"))

pr_sims <- vector("list", n_sims)

pr_sims <- foreach(i = 1:n_sims, .packages = c("Hmisc", "dplyr", "reshape2", "tidyr")) %dopar% {
  demographic_lean_sim <- demographic_leans %>%
    mutate(lean = rnorm(n(), mean = avg_lean, sd = lean_se)) %>%
    dplyr::select(candidate, demographic_var, demographic, lean)
  
  # This simulation's national percentages
  this_sim_state_percentage <- pr_state_percentages_adj %>%
    filter(sim_id == i)
  
  # Apply simulated leans to state average
  state_demographic_sim <- demoCandidateBreakdown("New Mexico", this_sim_state_percentage, demographic_lean_sim, 
                                                  state_marginal_demographics)
  
  # Turn these into probabilities
  gender_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("men", "women")) %>%
    dplyr::select(candidate, gender = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(gender = case_when(gender == "men" ~ "Male",
                              gender == "women" ~ "Female"))
  
  names(gender_breakdown)[-1] <- paste0("gender_", names(gender_breakdown))[-1]
  
  race_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("white", "black", "latino")) %>%
    dplyr::select(candidate, race = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(race = case_when(race == "white" ~ "White",
                            race == "black" ~ "Black",
                            race == "latino" ~ "Latino"))
  
  names(race_breakdown)[-1] <- paste0("race_", names(race_breakdown))[-1]
  
  age_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("age_1829", "age_3044", "age_4564", "age_65")) %>%
    dplyr::select(candidate, age = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(age = case_when(age == "age_1829" ~ "18-29",
                           age == "age_3044" ~ "30-44",
                           age == "age_4564" ~ "45-64",
                           age == "age_65" ~ "65+"))
  
  names(age_breakdown)[-1] <- paste0("age_", names(age_breakdown)[-1])
  
  education_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("college", "no_college")) %>%
    dplyr::select(candidate, education = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(education = case_when(education == "college" ~ "College",
                                 education == "no_college" ~ "Less than college"))
  
  names(education_breakdown)[-1] <- paste0("education_", names(education_breakdown)[-1])
  
  synth_pop <- sim_demographics %>%
    mutate(rand = runif(n()),
           age_regroup = case_when(age == "18-24" ~ "18-29",
                                   age == "25-34" & rand < 0.5 ~ "18-29",
                                   age == "25-34" & rand >= 0.5 ~ "30-44",
                                   age == "35-44" ~ "30-44",
                                   age == "45-64" ~ "45-64",
                                   age == "65+" ~ "65+"),
           id = 1:n()) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age = age_regroup, education) %>%
    mutate_if(is.factor, as.character) %>%
    left_join(gender_breakdown, by = "gender") %>%
    left_join(race_breakdown, by = "race") %>%
    left_join(age_breakdown, by = "age") %>%
    left_join(education_breakdown, by = "education") %>%
    mutate(biden_prob = gender_biden + race_biden + age_biden + education_biden,
           bloomberg_prob = gender_bloomberg + race_bloomberg + age_bloomberg + education_bloomberg,
           buttigieg_prob = gender_buttigieg + race_buttigieg + age_buttigieg + education_buttigieg,
           klobuchar_prob = gender_klobuchar + race_klobuchar + age_klobuchar + education_klobuchar,
           sanders_prob = gender_sanders + race_sanders + age_sanders + education_sanders,
           steyer_prob = gender_steyer + race_steyer + age_steyer + education_steyer,
           warren_prob = gender_warren + race_warren + age_warren + education_warren,
           yang_prob = gender_yang + race_yang + age_yang + education_yang) %>%
    mutate_at(vars(c("biden_prob", "bloomberg_prob", "buttigieg_prob", "klobuchar_prob", "sanders_prob", "steyer_prob", "warren_prob", 
                     "yang_prob")), function(x) pmax(x/100, 0)) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age, education, biden_prob, 
                  bloomberg_prob, buttigieg_prob, klobuchar_prob, sanders_prob, steyer_prob, warren_prob, yang_prob) %>%
    melt(id.vars = c("id", "state", "district", "primary_type", "district_delegates", "gender", "race", "age", "education"),
         variable.name = "candidate_prob", value.name = "prob") %>%
    arrange(id, candidate_prob) %>%
    group_by(id) %>%
    mutate(prob = prob / sum(prob),
           cum_prob = cumsum(prob)) %>%
    dplyr::select(-prob) %>%
    ungroup() %>%
    spread(candidate_prob, cum_prob) %>%
    mutate(rand = runif(n()),
           vote = case_when(rand <= biden_prob ~ "biden",
                            rand > biden_prob & rand <= bloomberg_prob ~ "bloomberg",
                            rand > bloomberg_prob & rand <= buttigieg_prob ~ "buttigieg",
                            rand > buttigieg_prob & rand <= klobuchar_prob ~ "klobuchar",
                            rand > klobuchar_prob & rand <= sanders_prob ~ "sanders",
                            rand > sanders_prob & rand <= steyer_prob ~ "steyer",
                            rand > steyer_prob & rand <= warren_prob ~ "warren",
                            rand > warren_prob ~ "yang")) %>%
    as.tbl()
  
  
  synth_district_pop <- synth_pop %>%
    group_by(state, district) %>%
    summarise(district_pop = n())
  
  district_sim_delegates <- synth_pop %>%
    group_by(state, district) %>%
    mutate(district_total = n()) %>%
    group_by(state, district, vote) %>%
    summarise(pct = n() / mean(district_total)) %>%
    ungroup() %>%
    mutate(met_threshold = pct >= 0.15) %>% 
    group_by(state, district) %>%
    mutate(pct_valid = (pct*met_threshold) / sum(pct*met_threshold)) %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(candidate_delegates = delegates * pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, district, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate = vote, pct, pct_valid, candidate_delegates) %>%
    arrange(district, candidate)
  
  state_sim_delegates <- district_sim_delegates %>%
    left_join(synth_district_pop, by = c("state", "district")) %>%
    mutate(synth_votes = pct * district_pop) %>%
    group_by(state) %>%
    mutate(state_vote = sum(synth_votes)) %>%
    group_by(state, candidate) %>%
    summarise(state_pct = sum(synth_votes) / mean(state_vote)) %>%
    mutate(district = "At-large") %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(met_threshold = state_pct >= 0.15,
           state_pct_valid = (state_pct*met_threshold) / sum(state_pct*met_threshold),
           candidate_delegates = delegates * state_pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate, pct = state_pct, pct_valid = state_pct_valid, candidate_delegates) %>%
    arrange(district, candidate) %>%
    bind_rows(district_sim_delegates) %>%
    mutate(sim_id = i)
}

# Four-on-the-Four-of-Four primary (4 Apr 2020) ####
bounce_factor <- tibble(sim_id = 1:n_sims, bounce_factor = rbeta(n_sims, shape1 = 3, shape2 = 5))

overperformance_bounce <- st_paddy_overperformance %>%
  left_join(bounce_factor, by = "sim_id") %>%
  mutate(bounce = overperformance * bounce_factor)

# Alaska ####
ak_start_time <- Sys.time()
# Draw votes
sim_demographics_table <- district_demographics %>%
  filter(state == "Alaska") %>%
  mutate(prob = predict(dem_primary_logit, newdata = ., type = "response"),
         n_vote = round(pop * prob * turnout * 0.05)) %>%
  dplyr::select(-pop, -prob)

sim_demographics <- sim_demographics_table %>%
  lapply(rep, sim_demographics_table$n_vote) %>%
  as.data.frame() 

state_marginal_demographics <- shapeStateDemo("Alaska")

# Have no Alaska polls; use nearest neighbors
ak_averages_over_time <- state_averages_over_time_unsmoothed %>%
  filter(state %in% c("Colorado", "Montana", "Oregon", "Washington"), as.numeric(today() - median_date) <= 1) %>%
  mutate(age = as.numeric(today() - median_date),
         nreps = 2 - age,
         inv_distance = case_when(state == "Colorado" ~ 0.159,
                                  state == "Montana" ~ 0.152,
                                  state == "Oregon" ~ 0.150,
                                  state == "Washington" ~ 0.150)) %>%
  group_by(candidate, median_date, age, nreps) %>%
  summarise(pct = wtd.mean(pct, inv_distance)) %>%
  bind_rows(natl_bloomberg_steyer %>% filter(candidate == "bloomberg")) 

ak_dirichlet_params <- ((ak_averages_over_time %>%
                           lapply(rep, ak_averages_over_time$nreps) %>%
                           as.data.frame() %>%
                           dplyr::select(candidate, median_date, pct) %>%
                           na.omit() %>%
                           mutate(pct = pct / 100) %>%
                           dplyr::select(-median_date) %>%
                           group_by(candidate) %>%
                           mutate(i = 1:n()) %>%
                           filter(i == 1) %>%
                           spread(candidate, pct) %>%
                           dplyr::select(-i) %>%
                           as.matrix() %>%
                           diri.est())$param)

ak_dirichlet_params <- 0.5 * ak_dirichlet_params / max(1, min(ak_dirichlet_params))
ak_dirichlet_params[1] <- ak_dirichlet_params[1] / 2
ak_dirichlet_params[8] <- 0

ak_state_percentages <- (rdirichlet(n_sims, ak_dirichlet_params) %>%
                            as.data.frame() %>%
                            dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, 
                                          steyer = V6, warren = V7, yang = V8)) %>%
  apply(MARGIN = 1, FUN = function(x) x / sum(x)) %>%
  t() %>%
  as.data.frame() %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

ak_state_percentages_adj <- ak_state_percentages %>%
  left_join(overperformance_bounce, by = c("sim_id", "candidate")) %>%
  mutate(state_pct = pmax(state_pct + bounce, 0)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct)) %>%
  left_join(st_paddy_second_choice_division, by = c("candidate", "sim_id")) %>%
  melt(id.vars = c("sim_id", "candidate", "state_pct", "overperformance", "bounce_factor", "stay_in", "bounce"), 
       variable.name = "second_choice", value.name = "pct") %>%
  mutate(pct = pct*state_pct*(!stay_in),
         state_pct = case_when(candidate == second_choice ~ state_pct,
                               candidate != second_choice ~ pct)) %>%
  arrange(sim_id, second_choice, candidate)

overperformance_stats <- ak_state_percentages_adj %>%
  group_by(sim_id, candidate) %>%
  summarise(overperformance = mean(overperformance),
            bounce_factor = mean(bounce_factor))

ak_state_percentages_adj <- ak_state_percentages_adj %>%
  group_by(sim_id, second_choice) %>%
  summarise(state_pct = sum(state_pct)) %>%
  dplyr::select(everything(), candidate = second_choice) %>%
  left_join(st_paddy_stayin_matrix %>% melt(id.vars = NULL, value.name = "stay_in") %>%
              dplyr::select(sim_id = Var1, candidate = Var2, stay_in), by = c("sim_id", "candidate")) %>%
  mutate(state_pct = state_pct * stay_in) %>%
  left_join(overperformance_stats, by = c("sim_id", "candidate"))

ak_sims <- vector("list", n_sims)

ak_sims <- foreach(i = 1:n_sims, .packages = c("Hmisc", "dplyr", "reshape2", "tidyr")) %dopar% {
  this_sim_state_percentage <- ak_state_percentages_adj %>%
    filter(sim_id == i)
  
  district_sim_pct <- this_sim_state_percentage %>%
    mutate(state_pct = state_pct / sum(state_pct),
           met_threshold = state_pct >= 0.15)
  
  state_sim_delegates <- district_sim_pct %>%
    mutate(pct_valid = state_pct * met_threshold) %>% 
    mutate(pct_valid = pct_valid / sum(pct_valid),
           state = "Alaska",
           district = "At-large") %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(candidate_delegates = delegates * pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, district, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate, pct = state_pct, pct_valid, candidate_delegates, sim_id) %>%
    arrange(district, candidate) 
}

# Hawaii ####
hi_start_time <- Sys.time()
# Draw votes
sim_demographics_table <- district_demographics %>%
  filter(state == "Hawaii") %>%
  mutate(prob = predict(dem_primary_logit, newdata = ., type = "response"),
         n_vote = round(pop * prob * turnout * 0.05)) %>%
  dplyr::select(-pop, -prob)

sim_demographics <- sim_demographics_table %>%
  lapply(rep, sim_demographics_table$n_vote) %>%
  as.data.frame() 
state_marginal_demographics <- shapeStateDemo("Hawaii")

# Have no Hawaii polls; use nearest neighbors
hi_averages_over_time <- state_averages_over_time_unsmoothed %>%
  filter(state %in% c("New Jersey", "Virginia", "Washington"), as.numeric(today() - median_date) <= 1) %>%
  mutate(age = as.numeric(today() - median_date),
         nreps = 2 - age,
         inv_distance = case_when(state == "New Jersey" ~ 0.283,
                                  state == "Virginia" ~ 0.251,
                                  state == "Washington" ~ 0.277)) %>%
  group_by(candidate, median_date, age, nreps) %>%
  summarise(pct = wtd.mean(pct, inv_distance))

hi_dirichlet_params <- ((hi_averages_over_time %>%
                           lapply(rep, hi_averages_over_time$nreps) %>%
                           as.data.frame() %>%
                           dplyr::select(candidate, median_date, pct) %>%
                           na.omit() %>%
                           dplyr::select(-median_date) %>%
                           group_by(candidate) %>%
                           mutate(i = 1:n()) %>%
                           spread(candidate, pct) %>%
                           dplyr::select(-i) %>%
                           as.matrix() %>%
                           diri.est())$param)

hi_dirichlet_params <- 0.5 * hi_dirichlet_params / max(1, min(hi_dirichlet_params))
hi_dirichlet_params[8] <- 0

hi_state_percentages <- (rdirichlet(n_sims, hi_dirichlet_params) %>%
                            as.data.frame() %>%
                            dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, 
                                          steyer = V6, warren = V7, yang = V8)) %>%
  apply(MARGIN = 1, FUN = function(x) x / sum(x)) %>%
  t() %>%
  as.data.frame() %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

hi_state_percentages_adj <- hi_state_percentages %>%
  left_join(overperformance_bounce, by = c("sim_id", "candidate")) %>%
  mutate(state_pct = pmax(state_pct + bounce, 0)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct)) %>%
  left_join(st_paddy_second_choice_division, by = c("candidate", "sim_id")) %>%
  melt(id.vars = c("sim_id", "candidate", "state_pct", "overperformance", "bounce_factor", "stay_in", "bounce"), 
       variable.name = "second_choice", value.name = "pct") %>%
  mutate(pct = pct*state_pct*(!stay_in),
         state_pct = case_when(candidate == second_choice ~ state_pct,
                               candidate != second_choice ~ pct)) %>%
  arrange(sim_id, second_choice, candidate)

overperformance_stats <- hi_state_percentages_adj %>%
  group_by(sim_id, candidate) %>%
  summarise(overperformance = mean(overperformance),
            bounce_factor = mean(bounce_factor))

hi_state_percentages_adj <- hi_state_percentages_adj %>%
  group_by(sim_id, second_choice) %>%
  summarise(state_pct = sum(state_pct)) %>%
  dplyr::select(everything(), candidate = second_choice) %>%
  left_join(st_paddy_stayin_matrix %>% melt(id.vars = NULL, value.name = "stay_in") %>%
              dplyr::select(sim_id = Var1, candidate = Var2, stay_in), by = c("sim_id", "candidate")) %>%
  mutate(state_pct = state_pct * stay_in) %>%
  left_join(overperformance_stats, by = c("sim_id", "candidate"))

hi_sims <- vector("list", n_sims)

hi_sims <- foreach(i = 1:n_sims, .packages = c("Hmisc", "dplyr", "reshape2", "tidyr")) %dopar% {
  demographic_lean_sim <- demographic_leans %>%
    mutate(lean = rnorm(n(), mean = avg_lean, sd = lean_se)) %>%
    dplyr::select(candidate, demographic_var, demographic, lean)
  
  # This simulation's national percentages
  this_sim_state_percentage <- hi_state_percentages_adj %>%
    filter(sim_id == i)
  
  # Apply simulated leans to state average
  state_demographic_sim <- demoCandidateBreakdown("California", this_sim_state_percentage, demographic_lean_sim, 
                                                  state_marginal_demographics)
  
  # Turn these into probabilities
  gender_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("men", "women")) %>%
    dplyr::select(candidate, gender = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(gender = case_when(gender == "men" ~ "Male",
                              gender == "women" ~ "Female"))
  
  names(gender_breakdown)[-1] <- paste0("gender_", names(gender_breakdown))[-1]
  
  race_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("white", "black", "latino")) %>%
    dplyr::select(candidate, race = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(race = case_when(race == "white" ~ "White",
                            race == "black" ~ "Black",
                            race == "latino" ~ "Latino"))
  
  names(race_breakdown)[-1] <- paste0("race_", names(race_breakdown))[-1]
  
  age_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("age_1829", "age_3044", "age_4564", "age_65")) %>%
    dplyr::select(candidate, age = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(age = case_when(age == "age_1829" ~ "18-29",
                           age == "age_3044" ~ "30-44",
                           age == "age_4564" ~ "45-64",
                           age == "age_65" ~ "65+"))
  
  names(age_breakdown)[-1] <- paste0("age_", names(age_breakdown)[-1])
  
  education_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("college", "no_college")) %>%
    dplyr::select(candidate, education = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(education = case_when(education == "college" ~ "College",
                                 education == "no_college" ~ "Less than college"))
  
  names(education_breakdown)[-1] <- paste0("education_", names(education_breakdown)[-1])
  
  synth_pop <- sim_demographics %>%
    mutate(rand = runif(n()),
           age_regroup = case_when(age == "18-24" ~ "18-29",
                                   age == "25-34" & rand < 0.5 ~ "18-29",
                                   age == "25-34" & rand >= 0.5 ~ "30-44",
                                   age == "35-44" ~ "30-44",
                                   age == "45-64" ~ "45-64",
                                   age == "65+" ~ "65+"),
           id = 1:n()) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age = age_regroup, education) %>%
    mutate_if(is.factor, as.character) %>%
    left_join(gender_breakdown, by = "gender") %>%
    left_join(race_breakdown, by = "race") %>%
    left_join(age_breakdown, by = "age") %>%
    left_join(education_breakdown, by = "education") %>%
    mutate(biden_prob = gender_biden + race_biden + age_biden + education_biden,
           bloomberg_prob = gender_bloomberg + race_bloomberg + age_bloomberg + education_bloomberg,
           buttigieg_prob = gender_buttigieg + race_buttigieg + age_buttigieg + education_buttigieg,
           klobuchar_prob = gender_klobuchar + race_klobuchar + age_klobuchar + education_klobuchar,
           sanders_prob = gender_sanders + race_sanders + age_sanders + education_sanders,
           steyer_prob = gender_steyer + race_steyer + age_steyer + education_steyer,
           warren_prob = gender_warren + race_warren + age_warren + education_warren,
           yang_prob = gender_yang + race_yang + age_yang + education_yang) %>%
    mutate_at(vars(c("biden_prob", "bloomberg_prob", "buttigieg_prob", "klobuchar_prob", "sanders_prob", "steyer_prob", "warren_prob", 
                     "yang_prob")), function(x) pmax(x/100, 0)) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age, education, biden_prob, 
                  bloomberg_prob, buttigieg_prob, klobuchar_prob, sanders_prob, steyer_prob, warren_prob, yang_prob) %>%
    melt(id.vars = c("id", "state", "district", "primary_type", "district_delegates", "gender", "race", "age", "education"),
         variable.name = "candidate_prob", value.name = "prob") %>%
    arrange(id, candidate_prob) %>%
    group_by(id) %>%
    mutate(prob = prob / sum(prob),
           cum_prob = cumsum(prob)) %>%
    dplyr::select(-prob) %>%
    ungroup() %>%
    spread(candidate_prob, cum_prob) %>%
    mutate(rand = runif(n()),
           vote = case_when(rand <= biden_prob ~ "biden",
                            rand > biden_prob & rand <= bloomberg_prob ~ "bloomberg",
                            rand > bloomberg_prob & rand <= buttigieg_prob ~ "buttigieg",
                            rand > buttigieg_prob & rand <= klobuchar_prob ~ "klobuchar",
                            rand > klobuchar_prob & rand <= sanders_prob ~ "sanders",
                            rand > sanders_prob & rand <= steyer_prob ~ "steyer",
                            rand > steyer_prob & rand <= warren_prob ~ "warren",
                            rand > warren_prob ~ "yang")) %>%
    as.tbl()
  
  
  synth_district_pop <- synth_pop %>%
    group_by(state, district) %>%
    summarise(district_pop = n())
  
  district_sim_delegates <- synth_pop %>%
    group_by(state, district) %>%
    mutate(district_total = n()) %>%
    group_by(state, district, vote) %>%
    summarise(pct = n() / mean(district_total)) %>%
    ungroup() %>%
    mutate(met_threshold = pct >= 0.15) %>% 
    group_by(state, district) %>%
    mutate(pct_valid = (pct*met_threshold) / sum(pct*met_threshold)) %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(candidate_delegates = delegates * pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, district, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate = vote, pct, pct_valid, candidate_delegates) %>%
    arrange(district, candidate)
  
  state_sim_delegates <- district_sim_delegates %>%
    left_join(synth_district_pop, by = c("state", "district")) %>%
    mutate(synth_votes = pct * district_pop) %>%
    group_by(state) %>%
    mutate(state_vote = sum(synth_votes)) %>%
    group_by(state, candidate) %>%
    summarise(state_pct = sum(synth_votes) / mean(state_vote)) %>%
    mutate(district = "At-large") %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(met_threshold = state_pct >= 0.15,
           state_pct_valid = (state_pct*met_threshold) / sum(state_pct*met_threshold),
           candidate_delegates = delegates * state_pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate, pct = state_pct, pct_valid = state_pct_valid, candidate_delegates) %>%
    arrange(district, candidate) %>%
    bind_rows(district_sim_delegates) %>%
    mutate(sim_id = i)
}

# Louisiana ####
la_start_time <- Sys.time()
# Draw votes
sim_demographics_table <- district_demographics %>%
  filter(state == "Louisiana") %>%
  mutate(prob = predict(dem_primary_logit, newdata = ., type = "response"),
         n_vote = round(pop * prob * turnout * 0.05)) %>%
  dplyr::select(-pop, -prob)

sim_demographics <- sim_demographics_table %>%
  lapply(rep, sim_demographics_table$n_vote) %>%
  as.data.frame() 
state_marginal_demographics <- shapeStateDemo("Louisiana")

# Have no Louisiana polls; use nearest neighbors
la_averages_over_time <- state_averages_over_time_unsmoothed %>%
  filter(state %in% c("Alabama", "Mississippi", "South Carolina"), as.numeric(today() - median_date) <= 1) %>%
  mutate(age = as.numeric(today() - median_date),
         nreps = 2 - age,
         inv_distance = case_when(state == "Mississippi" ~ 0.982,
                                  state == "Alabama" ~ 0.530,
                                  state == "South Carolina" ~ 0.400)) %>%
  group_by(candidate, median_date, age, nreps) %>%
  summarise(pct = wtd.mean(pct, inv_distance)) %>%
  bind_rows(natl_bloomberg_steyer %>% filter(candidate == "bloomberg"))

la_dirichlet_params <- ((la_averages_over_time %>%
                           lapply(rep, la_averages_over_time$nreps) %>%
                           as.data.frame() %>%
                           dplyr::select(candidate, median_date, pct) %>%
                           na.omit() %>%
                           dplyr::select(-median_date) %>%
                           group_by(candidate) %>%
                           mutate(i = 1:n()) %>%
                           filter(i == 1) %>%
                           spread(candidate, pct) %>%
                           dplyr::select(-i) %>%
                           as.matrix() %>%
                           diri.est())$param)

la_dirichlet_params <- 0.5 * la_dirichlet_params / max(1, min(la_dirichlet_params))
la_dirichlet_params[8] <- 0

la_state_percentages <- (rdirichlet(n_sims, la_dirichlet_params) %>%
                            as.data.frame() %>%
                            dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, 
                                          steyer = V6, warren = V7, yang = V8)) %>%
  apply(MARGIN = 1, FUN = function(x) x / sum(x)) %>%
  t() %>%
  as.data.frame() %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

la_state_percentages_adj <- la_state_percentages %>%
  left_join(overperformance_bounce, by = c("sim_id", "candidate")) %>%
  mutate(state_pct = pmax(state_pct + bounce, 0)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct)) %>%
  left_join(st_paddy_second_choice_division, by = c("candidate", "sim_id")) %>%
  melt(id.vars = c("sim_id", "candidate", "state_pct", "overperformance", "bounce_factor", "stay_in", "bounce"), 
       variable.name = "second_choice", value.name = "pct") %>%
  mutate(pct = pct*state_pct*(!stay_in),
         state_pct = case_when(candidate == second_choice ~ state_pct,
                               candidate != second_choice ~ pct)) %>%
  arrange(sim_id, second_choice, candidate)

overperformance_stats <- la_state_percentages_adj %>%
  group_by(sim_id, candidate) %>%
  summarise(overperformance = mean(overperformance),
            bounce_factor = mean(bounce_factor))

la_state_percentages_adj <- la_state_percentages_adj %>%
  group_by(sim_id, second_choice) %>%
  summarise(state_pct = sum(state_pct)) %>%
  dplyr::select(everything(), candidate = second_choice) %>%
  left_join(st_paddy_stayin_matrix %>% melt(id.vars = NULL, value.name = "stay_in") %>%
              dplyr::select(sim_id = Var1, candidate = Var2, stay_in), by = c("sim_id", "candidate")) %>%
  mutate(state_pct = state_pct * stay_in) %>%
  left_join(overperformance_stats, by = c("sim_id", "candidate"))

la_sims <- vector("list", n_sims)

la_sims <- foreach(i = 1:n_sims, .packages = c("Hmisc", "dplyr", "reshape2", "tidyr")) %dopar% {
  demographic_lean_sim <- demographic_leans %>%
    mutate(lean = rnorm(n(), mean = avg_lean, sd = lean_se)) %>%
    dplyr::select(candidate, demographic_var, demographic, lean)
  
  # This simulation's national percentages
  this_sim_state_percentage <- la_state_percentages_adj %>%
    filter(sim_id == i)
  
  # Apply simulated leans to state average
  state_demographic_sim <- demoCandidateBreakdown("Mississippi", this_sim_state_percentage, demographic_lean_sim, 
                                                  state_marginal_demographics)
  
  # Turn these into probabilities
  gender_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("men", "women")) %>%
    dplyr::select(candidate, gender = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(gender = case_when(gender == "men" ~ "Male",
                              gender == "women" ~ "Female"))
  
  names(gender_breakdown)[-1] <- paste0("gender_", names(gender_breakdown))[-1]
  
  race_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("white", "black", "latino")) %>%
    dplyr::select(candidate, race = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(race = case_when(race == "white" ~ "White",
                            race == "black" ~ "Black",
                            race == "latino" ~ "Latino"))
  
  names(race_breakdown)[-1] <- paste0("race_", names(race_breakdown))[-1]
  
  age_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("age_1829", "age_3044", "age_4564", "age_65")) %>%
    dplyr::select(candidate, age = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(age = case_when(age == "age_1829" ~ "18-29",
                           age == "age_3044" ~ "30-44",
                           age == "age_4564" ~ "45-64",
                           age == "age_65" ~ "65+"))
  
  names(age_breakdown)[-1] <- paste0("age_", names(age_breakdown)[-1])
  
  education_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("college", "no_college")) %>%
    dplyr::select(candidate, education = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(education = case_when(education == "college" ~ "College",
                                 education == "no_college" ~ "Less than college"))
  
  names(education_breakdown)[-1] <- paste0("education_", names(education_breakdown)[-1])
  
  synth_pop <- sim_demographics %>%
    mutate(rand = runif(n()),
           age_regroup = case_when(age == "18-24" ~ "18-29",
                                   age == "25-34" & rand < 0.5 ~ "18-29",
                                   age == "25-34" & rand >= 0.5 ~ "30-44",
                                   age == "35-44" ~ "30-44",
                                   age == "45-64" ~ "45-64",
                                   age == "65+" ~ "65+"),
           id = 1:n()) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age = age_regroup, education) %>%
    mutate_if(is.factor, as.character) %>%
    left_join(gender_breakdown, by = "gender") %>%
    left_join(race_breakdown, by = "race") %>%
    left_join(age_breakdown, by = "age") %>%
    left_join(education_breakdown, by = "education") %>%
    mutate(biden_prob = gender_biden + race_biden + age_biden + education_biden,
           bloomberg_prob = gender_bloomberg + race_bloomberg + age_bloomberg + education_bloomberg,
           buttigieg_prob = gender_buttigieg + race_buttigieg + age_buttigieg + education_buttigieg,
           klobuchar_prob = gender_klobuchar + race_klobuchar + age_klobuchar + education_klobuchar,
           sanders_prob = gender_sanders + race_sanders + age_sanders + education_sanders,
           steyer_prob = gender_steyer + race_steyer + age_steyer + education_steyer,
           warren_prob = gender_warren + race_warren + age_warren + education_warren,
           yang_prob = gender_yang + race_yang + age_yang + education_yang) %>%
    mutate_at(vars(c("biden_prob", "bloomberg_prob", "buttigieg_prob", "klobuchar_prob", "sanders_prob", "steyer_prob", "warren_prob", 
                     "yang_prob")), function(x) pmax(x/100, 0)) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age, education, biden_prob, 
                  bloomberg_prob, buttigieg_prob, klobuchar_prob, sanders_prob, steyer_prob, warren_prob, yang_prob) %>%
    melt(id.vars = c("id", "state", "district", "primary_type", "district_delegates", "gender", "race", "age", "education"),
         variable.name = "candidate_prob", value.name = "prob") %>%
    arrange(id, candidate_prob) %>%
    group_by(id) %>%
    mutate(prob = prob / sum(prob),
           cum_prob = cumsum(prob)) %>%
    dplyr::select(-prob) %>%
    ungroup() %>%
    spread(candidate_prob, cum_prob) %>%
    mutate(rand = runif(n()),
           vote = case_when(rand <= biden_prob ~ "biden",
                            rand > biden_prob & rand <= bloomberg_prob ~ "bloomberg",
                            rand > bloomberg_prob & rand <= buttigieg_prob ~ "buttigieg",
                            rand > buttigieg_prob & rand <= klobuchar_prob ~ "klobuchar",
                            rand > klobuchar_prob & rand <= sanders_prob ~ "sanders",
                            rand > sanders_prob & rand <= steyer_prob ~ "steyer",
                            rand > steyer_prob & rand <= warren_prob ~ "warren",
                            rand > warren_prob ~ "yang")) %>%
    as.tbl()
  
  
  synth_district_pop <- synth_pop %>%
    group_by(state, district) %>%
    summarise(district_pop = n())
  
  district_sim_delegates <- synth_pop %>%
    group_by(state, district) %>%
    mutate(district_total = n()) %>%
    group_by(state, district, vote) %>%
    summarise(pct = n() / mean(district_total)) %>%
    ungroup() %>%
    mutate(met_threshold = pct >= 0.15) %>% 
    group_by(state, district) %>%
    mutate(pct_valid = (pct*met_threshold) / sum(pct*met_threshold)) %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(candidate_delegates = delegates * pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, district, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate = vote, pct, pct_valid, candidate_delegates) %>%
    arrange(district, candidate)
  
  state_sim_delegates <- district_sim_delegates %>%
    left_join(synth_district_pop, by = c("state", "district")) %>%
    mutate(synth_votes = pct * district_pop) %>%
    group_by(state) %>%
    mutate(state_vote = sum(synth_votes)) %>%
    group_by(state, candidate) %>%
    summarise(state_pct = sum(synth_votes) / mean(state_vote)) %>%
    mutate(district = "At-large") %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(met_threshold = state_pct >= 0.15,
           state_pct_valid = (state_pct*met_threshold) / sum(state_pct*met_threshold),
           candidate_delegates = delegates * state_pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate, pct = state_pct, pct_valid = state_pct_valid, candidate_delegates) %>%
    arrange(district, candidate) %>%
    bind_rows(district_sim_delegates) %>%
    mutate(sim_id = i)
}

# Wyoming ####
wy_start_time <- Sys.time()

# Have no Wyoming polls; use nearest neighbors
wy_averages_over_time <- state_averages_over_time_unsmoothed %>%
  filter(state %in% c("Montana", "Oklahoma", "Iowa"), as.numeric(today() - median_date) <= 1) %>%
  mutate(age = as.numeric(today() - median_date),
         nreps = 2 - age,
         inv_distance = case_when(state == "Iowa" ~ 0.321,
                                  state == "Montana" ~ 0.389,
                                  state == "Oklahoma" ~ 0.287)) %>%
  group_by(candidate, median_date, age, nreps) %>%
  summarise(pct = wtd.mean(pct, inv_distance)) %>%
  bind_rows(natl_bloomberg_steyer %>% filter(candidate == "bloomberg"))

wy_dirichlet_params <- ((wy_averages_over_time %>%
                           lapply(rep, wy_averages_over_time$nreps) %>%
                           as.data.frame() %>%
                           dplyr::select(candidate, median_date, pct) %>%
                           na.omit() %>%
                           dplyr::select(-median_date) %>%
                           group_by(candidate) %>%
                           mutate(i = 1:n()) %>%
                           filter(i == 1) %>%
                           spread(candidate, pct) %>%
                           dplyr::select(-i) %>%
                           as.matrix() %>%
                           diri.est())$param)

wy_dirichlet_params <- 0.5 * wy_dirichlet_params / max(1, min(wy_dirichlet_params))
wy_dirichlet_params[1] <- wy_dirichlet_params[1] / 2
wy_dirichlet_params[8] <- 0

wy_state_percentages <- (rdirichlet(n_sims, wy_dirichlet_params) %>%
                            as.data.frame() %>%
                            dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, 
                                          steyer = V6, warren = V7, yang = V8)) %>%
  apply(MARGIN = 1, FUN = function(x) x / sum(x)) %>%
  t() %>%
  as.data.frame() %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

wy_state_percentages_adj <- wy_state_percentages %>%
  left_join(overperformance_bounce, by = c("sim_id", "candidate")) %>%
  mutate(state_pct = pmax(state_pct + bounce, 0)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct)) %>%
  left_join(st_paddy_second_choice_division, by = c("candidate", "sim_id")) %>%
  melt(id.vars = c("sim_id", "candidate", "state_pct", "overperformance", "bounce_factor", "stay_in", "bounce"), 
       variable.name = "second_choice", value.name = "pct") %>%
  mutate(pct = pct*state_pct*(!stay_in),
         state_pct = case_when(candidate == second_choice ~ state_pct,
                               candidate != second_choice ~ pct)) %>%
  arrange(sim_id, second_choice, candidate)

overperformance_stats <- wy_state_percentages_adj %>%
  group_by(sim_id, candidate) %>%
  summarise(overperformance = mean(overperformance),
            bounce_factor = mean(bounce_factor))

wy_state_percentages_adj <- wy_state_percentages_adj %>%
  group_by(sim_id, second_choice) %>%
  summarise(state_pct = sum(state_pct)) %>%
  dplyr::select(everything(), candidate = second_choice) %>%
  left_join(st_paddy_stayin_matrix %>% melt(id.vars = NULL, value.name = "stay_in") %>%
              dplyr::select(sim_id = Var1, candidate = Var2, stay_in), by = c("sim_id", "candidate")) %>%
  mutate(state_pct = state_pct * stay_in) %>%
  left_join(overperformance_stats, by = c("sim_id", "candidate"))

wy_sims <- vector("list", n_sims)

### Simulate
wy_sims <- foreach(i = 1:n_sims, .packages = c("Hmisc", "dplyr", "reshape2", "tidyr")) %dopar% {
  this_sim_state_percentage <- wy_state_percentages %>%
    filter(sim_id == i)
  
  second_choice_sim <- second_choice %>%
    melt(id.vars = "first_choice", variable.name = "second_choice", value.name = "pct") %>%
    mutate(alpha = 50 * pct / (1 - pct),
           second_pct_sim = rbeta(n(), shape1 = alpha, shape2 = 30)) %>%
    dplyr::select(first_choice, second_choice, second_pct_sim)
  
  district_sim_pct <- this_sim_state_percentage %>%
    mutate(met_threshold = state_pct >= 0.15)
  
  state_sim_delegates <- district_sim_pct %>%
    merge(second_choice_sim, by.x = "candidate", by.y = "first_choice", all.x = TRUE) %>%
    merge(district_sim_pct, by.x = "second_choice", by.y = "candidate") %>%
    mutate(pct_from_second_choice = state_pct.y * second_pct_sim * (second_choice != candidate) * (met_threshold.x) * (!met_threshold.y)) %>%
    group_by(candidate, state_pct.x, met_threshold = met_threshold.x) %>%
    summarise(pct_from_second_choice = sum(pct_from_second_choice)) %>%
    ungroup() %>%
    mutate(pct_valid = (state_pct.x + pct_from_second_choice)*met_threshold) %>% 
    mutate(pct_valid = pct_valid / sum(pct_valid),
           state = "Wyoming",
           district = "At-large") %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(candidate_delegates = delegates * pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, district, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate, pct = state_pct.x, pct_valid, candidate_delegates) %>%
    arrange(district, candidate) %>%
    mutate(sim_id = i)
  
}

# Wisconsin (7 Apr 2020) ####
bounce_factor <- tibble(sim_id = 1:n_sims, bounce_factor = rbeta(n_sims, shape1 = 3, shape2 = 6))

overperformance_bounce <- st_paddy_overperformance %>%
  left_join(bounce_factor, by = "sim_id") %>%
  mutate(bounce = overperformance * bounce_factor)

wi_start_time <- Sys.time()
# Draw votes
sim_demographics_table <- district_demographics %>%
  filter(state == "Wisconsin") %>%
  mutate(prob = predict(dem_primary_logit, newdata = ., type = "response"),
         n_vote = round(pop * prob * turnout * 0.05)) %>%
  dplyr::select(-pop, -prob)

sim_demographics <- sim_demographics_table %>%
  lapply(rep, sim_demographics_table$n_vote) %>%
  as.data.frame() 

state_marginal_demographics <- shapeStateDemo("Wisconsin")

wi_averages_over_time <- state_averages_over_time_unsmoothed %>%
  filter(state == "Wisconsin", as.numeric(today() - median_date) <= 1) %>%
  mutate(age = as.numeric(today() - median_date),
         nreps = 2 - age)

wi_dirichlet_params <- (wi_averages_over_time %>%
                          lapply(rep, wi_averages_over_time$nreps) %>%
                          as.data.frame() %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          dplyr::select(-median_date) %>%
                          group_by(candidate) %>%
                          mutate(i = 1:n()) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-i) %>%
                          as.matrix() %>%
                          diri.est())$param

wi_dirichlet_params <- wi_dirichlet_params / 2
wi_dirichlet_params[8] <- 0

wi_state_percentages <- (rdirichlet(n_sims, wi_dirichlet_params) %>%
                            as.data.frame() %>%
                            dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, 
                                          steyer = V6, warren = V7, yang = V8)) %>%
  apply(MARGIN = 1, FUN = function(x) x / sum(x)) %>%
  t() %>%
  as.data.frame() %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

wi_state_percentages_adj <- wi_state_percentages %>%
  left_join(overperformance_bounce, by = c("sim_id", "candidate")) %>%
  mutate(state_pct = pmax(state_pct + bounce, 0)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct)) %>%
  left_join(st_paddy_second_choice_division, by = c("candidate", "sim_id")) %>%
  melt(id.vars = c("sim_id", "candidate", "state_pct", "overperformance", "bounce_factor", "stay_in", "bounce"), 
       variable.name = "second_choice", value.name = "pct") %>%
  mutate(pct = pct*state_pct*(!stay_in),
         state_pct = case_when(candidate == second_choice ~ state_pct,
                               candidate != second_choice ~ pct)) %>%
  arrange(sim_id, second_choice, candidate)

overperformance_stats <- wi_state_percentages_adj %>%
  group_by(sim_id, candidate) %>%
  summarise(overperformance = mean(overperformance),
            bounce_factor = mean(bounce_factor))

wi_state_percentages_adj <- wi_state_percentages_adj %>%
  group_by(sim_id, second_choice) %>%
  summarise(state_pct = sum(state_pct)) %>%
  dplyr::select(everything(), candidate = second_choice) %>%
  left_join(st_paddy_stayin_matrix %>% melt(id.vars = NULL, value.name = "stay_in") %>%
              dplyr::select(sim_id = Var1, candidate = Var2, stay_in), by = c("sim_id", "candidate")) %>%
  mutate(state_pct = state_pct * stay_in) %>%
  left_join(overperformance_stats, by = c("sim_id", "candidate"))

wi_sims <- vector("list", n_sims)

wi_sims <- foreach(i = 1:n_sims, .packages = c("Hmisc", "dplyr", "reshape2", "tidyr")) %dopar% {
  demographic_lean_sim <- demographic_leans %>%
    mutate(lean = rnorm(n(), mean = avg_lean, sd = lean_se)) %>%
    dplyr::select(candidate, demographic_var, demographic, lean)
  
  # This simulation's national percentages
  this_sim_state_percentage <- wi_state_percentages_adj %>%
    filter(sim_id == i)
  
  # Apply simulated leans to state average
  state_demographic_sim <- demoCandidateBreakdown("Wisconsin", this_sim_state_percentage, demographic_lean_sim, 
                                                  state_marginal_demographics)
  
  # Turn these into probabilities
  gender_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("men", "women")) %>%
    dplyr::select(candidate, gender = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(gender = case_when(gender == "men" ~ "Male",
                              gender == "women" ~ "Female"))
  
  names(gender_breakdown)[-1] <- paste0("gender_", names(gender_breakdown))[-1]
  
  race_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("white", "black", "latino")) %>%
    dplyr::select(candidate, race = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(race = case_when(race == "white" ~ "White",
                            race == "black" ~ "Black",
                            race == "latino" ~ "Latino"))
  
  names(race_breakdown)[-1] <- paste0("race_", names(race_breakdown))[-1]
  
  age_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("age_1829", "age_3044", "age_4564", "age_65")) %>%
    dplyr::select(candidate, age = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(age = case_when(age == "age_1829" ~ "18-29",
                           age == "age_3044" ~ "30-44",
                           age == "age_4564" ~ "45-64",
                           age == "age_65" ~ "65+"))
  
  names(age_breakdown)[-1] <- paste0("age_", names(age_breakdown)[-1])
  
  education_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("college", "no_college")) %>%
    dplyr::select(candidate, education = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(education = case_when(education == "college" ~ "College",
                                 education == "no_college" ~ "Less than college"))
  
  names(education_breakdown)[-1] <- paste0("education_", names(education_breakdown)[-1])
  
  synth_pop <- sim_demographics %>%
    mutate(rand = runif(n()),
           age_regroup = case_when(age == "18-24" ~ "18-29",
                                   age == "25-34" & rand < 0.5 ~ "18-29",
                                   age == "25-34" & rand >= 0.5 ~ "30-44",
                                   age == "35-44" ~ "30-44",
                                   age == "45-64" ~ "45-64",
                                   age == "65+" ~ "65+"),
           id = 1:n()) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age = age_regroup, education) %>%
    mutate_if(is.factor, as.character) %>%
    left_join(gender_breakdown, by = "gender") %>%
    left_join(race_breakdown, by = "race") %>%
    left_join(age_breakdown, by = "age") %>%
    left_join(education_breakdown, by = "education") %>%
    mutate(biden_prob = gender_biden + race_biden + age_biden + education_biden,
           bloomberg_prob = gender_bloomberg + race_bloomberg + age_bloomberg + education_bloomberg,
           buttigieg_prob = gender_buttigieg + race_buttigieg + age_buttigieg + education_buttigieg,
           klobuchar_prob = gender_klobuchar + race_klobuchar + age_klobuchar + education_klobuchar,
           sanders_prob = gender_sanders + race_sanders + age_sanders + education_sanders,
           steyer_prob = gender_steyer + race_steyer + age_steyer + education_steyer,
           warren_prob = gender_warren + race_warren + age_warren + education_warren,
           yang_prob = gender_yang + race_yang + age_yang + education_yang) %>%
    mutate_at(vars(c("biden_prob", "bloomberg_prob", "buttigieg_prob", "klobuchar_prob", "sanders_prob", "steyer_prob", "warren_prob", 
                     "yang_prob")),
              function(x) pmax(x/100, 0)) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age, education, biden_prob, 
                  bloomberg_prob, buttigieg_prob, klobuchar_prob, sanders_prob, steyer_prob, warren_prob, yang_prob) %>%
    melt(id.vars = c("id", "state", "district", "primary_type", "district_delegates", "gender", "race", "age", "education"),
         variable.name = "candidate_prob", value.name = "prob") %>%
    arrange(id, candidate_prob) %>%
    group_by(id) %>%
    mutate(prob = prob / sum(prob),
           cum_prob = cumsum(prob)) %>%
    dplyr::select(-prob) %>%
    ungroup() %>%
    spread(candidate_prob, cum_prob) %>%
    mutate(rand = runif(n()),
           vote = case_when(rand <= biden_prob ~ "biden",
                            rand > biden_prob & rand <= bloomberg_prob ~ "bloomberg",
                            rand > bloomberg_prob & rand <= buttigieg_prob ~ "buttigieg",
                            rand > buttigieg_prob & rand <= klobuchar_prob ~ "klobuchar",
                            rand > klobuchar_prob & rand <= sanders_prob ~ "sanders",
                            rand > sanders_prob & rand <= steyer_prob ~ "steyer",
                            rand > steyer_prob & rand <= warren_prob ~ "warren",
                            rand > warren_prob ~ "yang")) %>%
    as.tbl()
  
  synth_district_pop <- synth_pop %>%
    group_by(state, district) %>%
    summarise(district_pop = n())
  
  district_sim_delegates <- synth_pop %>%
    group_by(state, district) %>%
    mutate(district_total = n()) %>%
    group_by(state, district, vote) %>%
    summarise(pct = n() / mean(district_total)) %>%
    ungroup() %>%
    mutate(met_threshold = pct >= 0.15) %>% 
    group_by(state, district) %>%
    mutate(pct_valid = (pct*met_threshold) / sum(pct*met_threshold)) %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(candidate_delegates = delegates * pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, district, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate = vote, pct, pct_valid, candidate_delegates) %>%
    arrange(district, candidate)
  
  state_sim_delegates <- district_sim_delegates %>%
    left_join(synth_district_pop, by = c("state", "district")) %>%
    mutate(synth_votes = pct * district_pop) %>%
    group_by(state) %>%
    mutate(state_vote = sum(synth_votes)) %>%
    group_by(state, candidate) %>%
    summarise(state_pct = sum(synth_votes) / mean(state_vote)) %>%
    mutate(district = "At-large") %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(met_threshold = state_pct >= 0.15,
           state_pct_valid = (state_pct*met_threshold) / sum(state_pct*met_threshold),
           candidate_delegates = delegates * state_pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate, pct = state_pct, pct_valid = state_pct_valid, candidate_delegates) %>%
    arrange(district, candidate) %>%
    bind_rows(district_sim_delegates) %>%
    mutate(sim_id = i)
}

# How much did the candidates overperform in Wisconsin?
wi_overperformance <- wi_state_percentages_adj %>%
  left_join(state_averages %>% filter(state == "Wisconsin") %>% dplyr::select(candidate, state_avg), by = c("candidate")) %>%
  mutate(overperformance = state_pct - (state_avg/100 + 0.25*overperformance*bounce_factor)) %>%
  dplyr::select(sim_id, candidate, overperformance) 

# Post-Wisconsin dropouts
wi_overperformance_matrix <- wi_overperformance %>%
  spread(candidate, overperformance) %>%
  ungroup() %>%
  dplyr::select(-sim_id) %>%
  as.matrix()

wi_stayin_matrix <- st_paddy_stayin_matrix & (wi_overperformance_matrix > -0.2)

wi_second_choice_division <- wi_stayin_matrix %>%
  as.data.frame() %>%
  melt(id.vars = NULL, variable.name = "candidate", value.name = "stay_in") %>%
  mutate(candidate = as.character(candidate),
         sim_id = rep(1:n_sims, 8)) %>%
  as.tbl() %>%
  left_join(second_choice, by = c("candidate" = "first_choice"))

# Acela primary (28 Apr 2020) ####
bounce_factor <- tibble(sim_id = 1:n_sims, bounce_factor = rbeta(n_sims, shape1 = 4, shape2 = 3))

overperformance_bounce <- wi_overperformance %>%
  left_join(bounce_factor, by = "sim_id") %>%
  mutate(bounce = overperformance * bounce_factor)

# Connecticut ####
ct_start_time <- Sys.time()
# Draw votes
sim_demographics_table <- district_demographics %>%
  filter(state == "Connecticut") %>%
  mutate(prob = predict(dem_primary_logit, newdata = ., type = "response"),
         n_vote = round(pop * prob * turnout * 0.05)) %>%
  dplyr::select(-pop, -prob)

sim_demographics <- sim_demographics_table %>%
  lapply(rep, sim_demographics_table$n_vote) %>%
  as.data.frame() 

state_marginal_demographics <- shapeStateDemo("Connecticut")

manj_averages_over_time <- state_averages_over_time_unsmoothed %>%
  filter(state %in% c("Massachusetts", "New Jersey"), as.numeric(today() - median_date) <= 1) %>%
  mutate(age = as.numeric(today() - median_date),
         nreps = 2 - age,
         inv_distance = case_when(state == "Massachusetts" ~ 0.684,
                                  state == "New Jersey" ~ 0.557)) %>%
  group_by(candidate, median_date, age, nreps) %>%
  summarise(pct = wtd.mean(pct, inv_distance)) %>%
  filter(candidate %in% c("bloomberg", "klobuchar", "steyer", "yang"))

ct_averages_over_time <- state_averages_over_time_unsmoothed %>%
  filter(state == "Connecticut", as.numeric(today() - median_date) <= 1) %>%
  mutate(age = as.numeric(today() - median_date),
         nreps = 2 - age)

ct_dirichlet_params <- (ct_averages_over_time %>%
                          lapply(rep, ct_averages_over_time$nreps) %>%
                          as.data.frame() %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          dplyr::select(-median_date) %>%
                          group_by(candidate) %>%
                          mutate(i = 1:n()) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-i) %>%
                          as.matrix() %>%
                          diri.est())$param

ct_dirichlet_params <- 0.5 * ct_dirichlet_params / max(1, min(ct_dirichlet_params))
ct_dirichlet_params[8] <- 0

ct_state_percentages <- (rdirichlet(n_sims, ct_dirichlet_params) %>%
                            as.data.frame() %>%
                            dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, 
                                          steyer = V6, warren = V7, yang = V8)) %>%
  apply(MARGIN = 1, FUN = function(x) x / sum(x)) %>%
  t() %>%
  as.data.frame() %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

ct_state_percentages_adj <- ct_state_percentages %>%
  left_join(overperformance_bounce, by = c("sim_id", "candidate")) %>%
  mutate(state_pct = pmax(state_pct + bounce, 0)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct)) %>%
  left_join(wi_second_choice_division, by = c("candidate", "sim_id")) %>%
  melt(id.vars = c("sim_id", "candidate", "state_pct", "overperformance", "bounce_factor", "stay_in", "bounce"), 
       variable.name = "second_choice", value.name = "pct") %>%
  mutate(pct = pct*state_pct*(!stay_in),
         state_pct = case_when(candidate == second_choice ~ state_pct,
                               candidate != second_choice ~ pct)) %>%
  arrange(sim_id, second_choice, candidate)

overperformance_stats <- ct_state_percentages_adj %>%
  group_by(sim_id, candidate) %>%
  summarise(overperformance = mean(overperformance),
            bounce_factor = mean(bounce_factor))

ct_state_percentages_adj <- ct_state_percentages_adj %>%
  group_by(sim_id, second_choice) %>%
  summarise(state_pct = sum(state_pct)) %>%
  dplyr::select(everything(), candidate = second_choice) %>%
  left_join(wi_stayin_matrix %>% melt(id.vars = NULL, value.name = "stay_in") %>%
              dplyr::select(sim_id = Var1, candidate = Var2, stay_in), by = c("sim_id", "candidate")) %>%
  mutate(state_pct = state_pct * stay_in) %>%
  left_join(overperformance_stats, by = c("sim_id", "candidate"))

ct_sims <- vector("list", n_sims)

ct_sims <- foreach(i = 1:n_sims, .packages = c("Hmisc", "dplyr", "reshape2", "tidyr")) %dopar% {
  demographic_lean_sim <- demographic_leans %>%
    mutate(lean = rnorm(n(), mean = avg_lean, sd = lean_se)) %>%
    dplyr::select(candidate, demographic_var, demographic, lean)
  
  # This simulation's national percentages
  this_sim_state_percentage <- ct_state_percentages_adj %>%
    filter(sim_id == i)
  
  # Apply simulated leans to state average
  state_demographic_sim <- demoCandidateBreakdown("Connecticut", this_sim_state_percentage, demographic_lean_sim, 
                                                  state_marginal_demographics)
  
  # Turn these into probabilities
  gender_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("men", "women")) %>%
    dplyr::select(candidate, gender = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(gender = case_when(gender == "men" ~ "Male",
                              gender == "women" ~ "Female"))
  
  names(gender_breakdown)[-1] <- paste0("gender_", names(gender_breakdown))[-1]
  
  race_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("white", "black", "latino")) %>%
    dplyr::select(candidate, race = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(race = case_when(race == "white" ~ "White",
                            race == "black" ~ "Black",
                            race == "latino" ~ "Latino"))
  
  names(race_breakdown)[-1] <- paste0("race_", names(race_breakdown))[-1]
  
  age_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("age_1829", "age_3044", "age_4564", "age_65")) %>%
    dplyr::select(candidate, age = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(age = case_when(age == "age_1829" ~ "18-29",
                           age == "age_3044" ~ "30-44",
                           age == "age_4564" ~ "45-64",
                           age == "age_65" ~ "65+"))
  
  names(age_breakdown)[-1] <- paste0("age_", names(age_breakdown)[-1])
  
  education_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("college", "no_college")) %>%
    dplyr::select(candidate, education = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(education = case_when(education == "college" ~ "College",
                                 education == "no_college" ~ "Less than college"))
  
  names(education_breakdown)[-1] <- paste0("education_", names(education_breakdown)[-1])
  
  synth_pop <- sim_demographics %>%
    mutate(rand = runif(n()),
           age_regroup = case_when(age == "18-24" ~ "18-29",
                                   age == "25-34" & rand < 0.5 ~ "18-29",
                                   age == "25-34" & rand >= 0.5 ~ "30-44",
                                   age == "35-44" ~ "30-44",
                                   age == "45-64" ~ "45-64",
                                   age == "65+" ~ "65+"),
           id = 1:n()) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age = age_regroup, education) %>%
    mutate_if(is.factor, as.character) %>%
    left_join(gender_breakdown, by = "gender") %>%
    left_join(race_breakdown, by = "race") %>%
    left_join(age_breakdown, by = "age") %>%
    left_join(education_breakdown, by = "education") %>%
    mutate(biden_prob = gender_biden + race_biden + age_biden + education_biden,
           bloomberg_prob = gender_bloomberg + race_bloomberg + age_bloomberg + education_bloomberg,
           buttigieg_prob = gender_buttigieg + race_buttigieg + age_buttigieg + education_buttigieg,
           klobuchar_prob = gender_klobuchar + race_klobuchar + age_klobuchar + education_klobuchar,
           sanders_prob = gender_sanders + race_sanders + age_sanders + education_sanders,
           steyer_prob = gender_steyer + race_steyer + age_steyer + education_steyer,
           warren_prob = gender_warren + race_warren + age_warren + education_warren,
           yang_prob = gender_yang + race_yang + age_yang + education_yang) %>%
    mutate_at(vars(c("biden_prob", "bloomberg_prob", "buttigieg_prob", "klobuchar_prob", "sanders_prob", "steyer_prob", "warren_prob", 
                     "yang_prob")),
              function(x) pmax(x/100, 0)) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age, education, biden_prob, 
                  bloomberg_prob, buttigieg_prob, klobuchar_prob, sanders_prob, steyer_prob, warren_prob, yang_prob) %>%
    melt(id.vars = c("id", "state", "district", "primary_type", "district_delegates", "gender", "race", "age", "education"),
         variable.name = "candidate_prob", value.name = "prob") %>%
    arrange(id, candidate_prob) %>%
    group_by(id) %>%
    mutate(prob = prob / sum(prob),
           cum_prob = cumsum(prob)) %>%
    dplyr::select(-prob) %>%
    ungroup() %>%
    spread(candidate_prob, cum_prob) %>%
    mutate(rand = runif(n()),
           vote = case_when(rand <= biden_prob ~ "biden",
                            rand > biden_prob & rand <= bloomberg_prob ~ "bloomberg",
                            rand > bloomberg_prob & rand <= buttigieg_prob ~ "buttigieg",
                            rand > buttigieg_prob & rand <= klobuchar_prob ~ "klobuchar",
                            rand > klobuchar_prob & rand <= sanders_prob ~ "sanders",
                            rand > sanders_prob & rand <= steyer_prob ~ "steyer",
                            rand > steyer_prob & rand <= warren_prob ~ "warren",
                            rand > warren_prob ~ "yang")) %>%
    as.tbl()
  
  synth_district_pop <- synth_pop %>%
    group_by(state, district) %>%
    summarise(district_pop = n())
  
  district_sim_delegates <- synth_pop %>%
    group_by(state, district) %>%
    mutate(district_total = n()) %>%
    group_by(state, district, vote) %>%
    summarise(pct = n() / mean(district_total)) %>%
    ungroup() %>%
    mutate(met_threshold = pct >= 0.15) %>% 
    group_by(state, district) %>%
    mutate(pct_valid = (pct*met_threshold) / sum(pct*met_threshold)) %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(candidate_delegates = delegates * pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, district, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate = vote, pct, pct_valid, candidate_delegates) %>%
    arrange(district, candidate)
  
  state_sim_delegates <- district_sim_delegates %>%
    left_join(synth_district_pop, by = c("state", "district")) %>%
    mutate(synth_votes = pct * district_pop) %>%
    group_by(state) %>%
    mutate(state_vote = sum(synth_votes)) %>%
    group_by(state, candidate) %>%
    summarise(state_pct = sum(synth_votes) / mean(state_vote)) %>%
    mutate(district = "At-large") %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(met_threshold = state_pct >= 0.15,
           state_pct_valid = (state_pct*met_threshold) / sum(state_pct*met_threshold),
           candidate_delegates = delegates * state_pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate, pct = state_pct, pct_valid = state_pct_valid, candidate_delegates) %>%
    arrange(district, candidate) %>%
    bind_rows(district_sim_delegates) %>%
    mutate(sim_id = i)
}

# Delaware ####
de_start_time <- Sys.time()

sim_demographics_table <- district_demographics %>%
  filter(state == "Delaware") %>%
  mutate(prob = predict(dem_primary_logit, newdata = ., type = "response"),
         n_vote = round(pop * prob * turnout * 0.05)) %>%
  dplyr::select(-pop, -prob)

sim_demographics <- sim_demographics_table %>%
  lapply(rep, sim_demographics_table$n_vote) %>%
  as.data.frame() 
state_marginal_demographics <- shapeStateDemo("Delaware")

de_averages_over_time <- state_averages_over_time_unsmoothed %>%
  filter(state == "Delaware", as.numeric(today() - median_date) <= 1) %>%
  mutate(age = as.numeric(today() - median_date),
         nreps = 2 - age) 

de_dirichlet_params <- (de_averages_over_time %>%
                          lapply(rep, de_averages_over_time$nreps) %>%
                          as.data.frame() %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          dplyr::select(-median_date) %>%
                          group_by(candidate) %>%
                          mutate(i = 1:n()) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-i) %>%
                          as.matrix() %>%
                          diri.est())$param

de_dirichlet_params <- de_dirichlet_params / 2
de_dirichlet_params[8] <- 0

de_state_percentages <- (rdirichlet(n_sims, de_dirichlet_params) %>%
                            as.data.frame() %>%
                            dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, 
                                          steyer = V6, warren = V7, yang = V8)) %>%
  apply(MARGIN = 1, FUN = function(x) x / sum(x)) %>%
  t() %>%
  as.data.frame() %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

de_state_percentages_adj <- de_state_percentages  %>%
  left_join(overperformance_bounce, by = c("sim_id", "candidate")) %>%
  mutate(state_pct = pmax(state_pct + bounce, 0)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct)) %>%
  left_join(wi_second_choice_division, by = c("candidate", "sim_id")) %>%
  melt(id.vars = c("sim_id", "candidate", "state_pct", "overperformance", "bounce_factor", "stay_in", "bounce"), 
       variable.name = "second_choice", value.name = "pct") %>%
  mutate(pct = pct*state_pct*(!stay_in),
         state_pct = case_when(candidate == second_choice ~ state_pct,
                               candidate != second_choice ~ pct)) %>%
  arrange(sim_id, second_choice, candidate)

overperformance_stats <- de_state_percentages_adj %>%
  group_by(sim_id, candidate) %>%
  summarise(overperformance = mean(overperformance),
            bounce_factor = mean(bounce_factor))

de_state_percentages_adj <- de_state_percentages_adj %>%
  group_by(sim_id, second_choice) %>%
  summarise(state_pct = sum(state_pct)) %>%
  dplyr::select(everything(), candidate = second_choice) %>%
  left_join(wi_stayin_matrix %>% melt(id.vars = NULL, value.name = "stay_in") %>%
              dplyr::select(sim_id = Var1, candidate = Var2, stay_in), by = c("sim_id", "candidate")) %>%
  mutate(state_pct = state_pct * stay_in) %>%
  left_join(overperformance_stats, by = c("sim_id", "candidate"))

de_sims <- vector("list", n_sims)

### Simulate
de_sims <- foreach(i = 1:n_sims, .packages = c("Hmisc", "dplyr", "reshape2", "tidyr")) %dopar% {
  demographic_lean_sim <- demographic_leans %>%
    mutate(lean = rnorm(n(), mean = avg_lean, sd = lean_se)) %>%
    dplyr::select(candidate, demographic_var, demographic, lean)
  
  # This simulation's national percentages
  this_sim_state_percentage <- de_state_percentages_adj %>%
    filter(sim_id == i)
  
  # Apply simulated leans to state average
  state_demographic_sim <- demoCandidateBreakdown("Delaware", this_sim_state_percentage, demographic_lean_sim, 
                                                  state_marginal_demographics)
  
  # Turn these into probabilities
  gender_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("men", "women")) %>%
    dplyr::select(candidate, gender = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(gender = case_when(gender == "men" ~ "Male",
                              gender == "women" ~ "Female"))
  
  names(gender_breakdown)[-1] <- paste0("gender_", names(gender_breakdown))[-1]
  
  race_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("white", "black", "latino")) %>%
    dplyr::select(candidate, race = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(race = case_when(race == "white" ~ "White",
                            race == "black" ~ "Black",
                            race == "latino" ~ "Latino"))
  
  names(race_breakdown)[-1] <- paste0("race_", names(race_breakdown))[-1]
  
  age_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("age_1829", "age_3044", "age_4564", "age_65")) %>%
    dplyr::select(candidate, age = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(age = case_when(age == "age_1829" ~ "18-29",
                           age == "age_3044" ~ "30-44",
                           age == "age_4564" ~ "45-64",
                           age == "age_65" ~ "65+"))
  
  names(age_breakdown)[-1] <- paste0("age_", names(age_breakdown)[-1])
  
  education_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("college", "no_college")) %>%
    dplyr::select(candidate, education = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(education = case_when(education == "college" ~ "College",
                                 education == "no_college" ~ "Less than college"))
  
  names(education_breakdown)[-1] <- paste0("education_", names(education_breakdown)[-1])
  
  synth_pop <- sim_demographics %>%
    mutate(rand = runif(n()),
           age_regroup = case_when(age == "18-24" ~ "18-29",
                                   age == "25-34" & rand < 0.5 ~ "18-29",
                                   age == "25-34" & rand >= 0.5 ~ "30-44",
                                   age == "35-44" ~ "30-44",
                                   age == "45-64" ~ "45-64",
                                   age == "65+" ~ "65+"),
           id = 1:n()) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age = age_regroup, education) %>%
    mutate_if(is.factor, as.character) %>%
    left_join(gender_breakdown, by = "gender") %>%
    left_join(race_breakdown, by = "race") %>%
    left_join(age_breakdown, by = "age") %>%
    left_join(education_breakdown, by = "education") %>%
    mutate(biden_prob = gender_biden + race_biden + age_biden + education_biden,
           bloomberg_prob = gender_bloomberg + race_bloomberg + age_bloomberg + education_bloomberg,
           buttigieg_prob = gender_buttigieg + race_buttigieg + age_buttigieg + education_buttigieg,
           klobuchar_prob = gender_klobuchar + race_klobuchar + age_klobuchar + education_klobuchar,
           sanders_prob = gender_sanders + race_sanders + age_sanders + education_sanders,
           steyer_prob = gender_steyer + race_steyer + age_steyer + education_steyer,
           warren_prob = gender_warren + race_warren + age_warren + education_warren,
           yang_prob = gender_yang + race_yang + age_yang + education_yang) %>%
    mutate_at(vars(c("biden_prob", "bloomberg_prob", "buttigieg_prob", "klobuchar_prob", "sanders_prob", "steyer_prob", "warren_prob", 
                     "yang_prob")),
              function(x) pmax(x/100, 0)) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age, education, biden_prob, 
                  bloomberg_prob, buttigieg_prob, klobuchar_prob, sanders_prob, steyer_prob, warren_prob, yang_prob) %>%
    melt(id.vars = c("id", "state", "district", "primary_type", "district_delegates", "gender", "race", "age", "education"),
         variable.name = "candidate_prob", value.name = "prob") %>%
    arrange(id, candidate_prob) %>%
    group_by(id) %>%
    mutate(prob = prob / sum(prob),
           cum_prob = cumsum(prob)) %>%
    dplyr::select(-prob) %>%
    ungroup() %>%
    spread(candidate_prob, cum_prob) %>%
    mutate(rand = runif(n()),
           vote = case_when(rand <= biden_prob ~ "biden",
                            rand > biden_prob & rand <= bloomberg_prob ~ "bloomberg",
                            rand > bloomberg_prob & rand <= buttigieg_prob ~ "buttigieg",
                            rand > buttigieg_prob & rand <= klobuchar_prob ~ "klobuchar",
                            rand > klobuchar_prob & rand <= sanders_prob ~ "sanders",
                            rand > sanders_prob & rand <= steyer_prob ~ "steyer",
                            rand > steyer_prob & rand <= warren_prob ~ "warren",
                            rand > warren_prob ~ "yang")) %>%
    as.tbl()
  
  synth_district_pop <- synth_pop %>%
    group_by(state, district) %>%
    summarise(district_pop = n())
  
  district_sim_delegates <- synth_pop %>%
    group_by(state, district) %>%
    mutate(district_total = n()) %>%
    group_by(state, district, vote) %>%
    summarise(pct = n() / mean(district_total)) %>%
    ungroup() %>%
    mutate(met_threshold = pct >= 0.15) %>% 
    group_by(state, district) %>%
    mutate(pct_valid = (pct*met_threshold) / sum(pct*met_threshold)) %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(candidate_delegates = delegates * pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, district, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate = vote, pct, pct_valid, candidate_delegates) %>%
    arrange(district, candidate)
  
  state_sim_delegates <- district_sim_delegates %>%
    left_join(synth_district_pop, by = c("state", "district")) %>%
    mutate(synth_votes = pct * district_pop) %>%
    group_by(state) %>%
    mutate(state_vote = sum(synth_votes)) %>%
    group_by(state, candidate) %>%
    summarise(state_pct = sum(synth_votes) / mean(state_vote)) %>%
    mutate(district = "At-large") %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(met_threshold = state_pct >= 0.15,
           state_pct_valid = (state_pct*met_threshold) / sum(state_pct*met_threshold),
           candidate_delegates = delegates * state_pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate, pct = state_pct, pct_valid = state_pct_valid, candidate_delegates) %>%
    arrange(district, candidate) %>%
    bind_rows(district_sim_delegates) %>%
    mutate(sim_id = i)
}

de_overperformance <- de_state_percentages_adj %>%
  left_join(state_averages %>% filter(state == "Delaware") %>% dplyr::select(candidate, state_avg), by = c("candidate")) %>%
  mutate(overperformance = state_pct - (state_avg/100 + 0.25*overperformance*bounce_factor)) %>%
  dplyr::select(sim_id, candidate, de_overperformance = overperformance) 


# Maryland ####
md_start_time <- Sys.time()

sim_demographics_table <- district_demographics %>%
  filter(state == "Maryland") %>%
  mutate(prob = predict(dem_primary_logit, newdata = ., type = "response"),
         n_vote = round(pop * prob * turnout * 0.05)) %>%
  dplyr::select(-pop, -prob)

sim_demographics <- sim_demographics_table %>%
  lapply(rep, sim_demographics_table$n_vote) %>%
  as.data.frame() 
state_marginal_demographics <- shapeStateDemo("Maryland")

md_averages_over_time <- state_averages_over_time_unsmoothed %>%
  filter(state == "Maryland", as.numeric(today() - median_date) <= 1) %>%
  mutate(age = as.numeric(today() - median_date),
         nreps = 2 - age) 

md_dirichlet_params <- (md_averages_over_time %>%
                          lapply(rep, md_averages_over_time$nreps) %>%
                          as.data.frame() %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          dplyr::select(-median_date) %>%
                          group_by(candidate) %>%
                          mutate(i = 1:n()) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-i) %>%
                          as.matrix() %>%
                          diri.est())$param

md_dirichlet_params <- md_dirichlet_params / 2
md_dirichlet_params[8] <- 0

md_state_percentages <- (rdirichlet(n_sims, md_dirichlet_params) %>%
                            as.data.frame() %>%
                            dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, 
                                          steyer = V6, warren = V7, yang = V8)) %>%
  apply(MARGIN = 1, FUN = function(x) x / sum(x)) %>%
  t() %>%
  as.data.frame() %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

md_state_percentages_adj <- md_state_percentages  %>%
  left_join(overperformance_bounce, by = c("sim_id", "candidate")) %>%
  mutate(state_pct = pmax(state_pct + bounce, 0)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct)) %>%
  left_join(wi_second_choice_division, by = c("candidate", "sim_id")) %>%
  melt(id.vars = c("sim_id", "candidate", "state_pct", "overperformance", "bounce_factor", "stay_in", "bounce"), 
       variable.name = "second_choice", value.name = "pct") %>%
  mutate(pct = pct*state_pct*(!stay_in),
         state_pct = case_when(candidate == second_choice ~ state_pct,
                               candidate != second_choice ~ pct)) %>%
  arrange(sim_id, second_choice, candidate)

overperformance_stats <- md_state_percentages_adj %>%
  group_by(sim_id, candidate) %>%
  summarise(overperformance = mean(overperformance),
            bounce_factor = mean(bounce_factor))

md_state_percentages_adj <- md_state_percentages_adj %>%
  group_by(sim_id, second_choice) %>%
  summarise(state_pct = sum(state_pct)) %>%
  dplyr::select(everything(), candidate = second_choice) %>%
  left_join(wi_stayin_matrix %>% melt(id.vars = NULL, value.name = "stay_in") %>%
              dplyr::select(sim_id = Var1, candidate = Var2, stay_in), by = c("sim_id", "candidate")) %>%
  mutate(state_pct = state_pct * stay_in) %>%
  left_join(overperformance_stats, by = c("sim_id", "candidate"))

md_sims <- vector("list", n_sims)

### Simulate
md_sims <- foreach(i = 1:n_sims, .packages = c("Hmisc", "dplyr", "reshape2", "tidyr")) %dopar% {
  demographic_lean_sim <- demographic_leans %>%
    mutate(lean = rnorm(n(), mean = avg_lean, sd = lean_se)) %>%
    dplyr::select(candidate, demographic_var, demographic, lean)
  
  # This simulation's national percentages
  this_sim_state_percentage <- md_state_percentages_adj %>%
    filter(sim_id == i)
  
  # Apply simulated leans to state average
  state_demographic_sim <- demoCandidateBreakdown("Maryland", this_sim_state_percentage, demographic_lean_sim, 
                                                  state_marginal_demographics)
  
  # Turn these into probabilities
  gender_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("men", "women")) %>%
    dplyr::select(candidate, gender = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(gender = case_when(gender == "men" ~ "Male",
                              gender == "women" ~ "Female"))
  
  names(gender_breakdown)[-1] <- paste0("gender_", names(gender_breakdown))[-1]
  
  race_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("white", "black", "latino")) %>%
    dplyr::select(candidate, race = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(race = case_when(race == "white" ~ "White",
                            race == "black" ~ "Black",
                            race == "latino" ~ "Latino"))
  
  names(race_breakdown)[-1] <- paste0("race_", names(race_breakdown))[-1]
  
  age_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("age_1829", "age_3044", "age_4564", "age_65")) %>%
    dplyr::select(candidate, age = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(age = case_when(age == "age_1829" ~ "18-29",
                           age == "age_3044" ~ "30-44",
                           age == "age_4564" ~ "45-64",
                           age == "age_65" ~ "65+"))
  
  names(age_breakdown)[-1] <- paste0("age_", names(age_breakdown)[-1])
  
  education_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("college", "no_college")) %>%
    dplyr::select(candidate, education = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(education = case_when(education == "college" ~ "College",
                                 education == "no_college" ~ "Less than college"))
  
  names(education_breakdown)[-1] <- paste0("education_", names(education_breakdown)[-1])
  
  synth_pop <- sim_demographics %>%
    mutate(rand = runif(n()),
           age_regroup = case_when(age == "18-24" ~ "18-29",
                                   age == "25-34" & rand < 0.5 ~ "18-29",
                                   age == "25-34" & rand >= 0.5 ~ "30-44",
                                   age == "35-44" ~ "30-44",
                                   age == "45-64" ~ "45-64",
                                   age == "65+" ~ "65+"),
           id = 1:n()) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age = age_regroup, education) %>%
    mutate_if(is.factor, as.character) %>%
    left_join(gender_breakdown, by = "gender") %>%
    left_join(race_breakdown, by = "race") %>%
    left_join(age_breakdown, by = "age") %>%
    left_join(education_breakdown, by = "education") %>%
    mutate(biden_prob = gender_biden + race_biden + age_biden + education_biden,
           bloomberg_prob = gender_bloomberg + race_bloomberg + age_bloomberg + education_bloomberg,
           buttigieg_prob = gender_buttigieg + race_buttigieg + age_buttigieg + education_buttigieg,
           klobuchar_prob = gender_klobuchar + race_klobuchar + age_klobuchar + education_klobuchar,
           sanders_prob = gender_sanders + race_sanders + age_sanders + education_sanders,
           steyer_prob = gender_steyer + race_steyer + age_steyer + education_steyer,
           warren_prob = gender_warren + race_warren + age_warren + education_warren,
           yang_prob = gender_yang + race_yang + age_yang + education_yang) %>%
    mutate_at(vars(c("biden_prob", "bloomberg_prob", "buttigieg_prob", "klobuchar_prob", "sanders_prob", "steyer_prob", "warren_prob", 
                     "yang_prob")),
              function(x) pmax(x/100, 0)) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age, education, biden_prob, 
                  bloomberg_prob, buttigieg_prob, klobuchar_prob, sanders_prob, steyer_prob, warren_prob, yang_prob) %>%
    melt(id.vars = c("id", "state", "district", "primary_type", "district_delegates", "gender", "race", "age", "education"),
         variable.name = "candidate_prob", value.name = "prob") %>%
    arrange(id, candidate_prob) %>%
    group_by(id) %>%
    mutate(prob = prob / sum(prob),
           cum_prob = cumsum(prob)) %>%
    dplyr::select(-prob) %>%
    ungroup() %>%
    spread(candidate_prob, cum_prob) %>%
    mutate(rand = runif(n()),
           vote = case_when(rand <= biden_prob ~ "biden",
                            rand > biden_prob & rand <= bloomberg_prob ~ "bloomberg",
                            rand > bloomberg_prob & rand <= buttigieg_prob ~ "buttigieg",
                            rand > buttigieg_prob & rand <= klobuchar_prob ~ "klobuchar",
                            rand > klobuchar_prob & rand <= sanders_prob ~ "sanders",
                            rand > sanders_prob & rand <= steyer_prob ~ "steyer",
                            rand > steyer_prob & rand <= warren_prob ~ "warren",
                            rand > warren_prob ~ "yang")) %>%
    as.tbl()
  
  synth_district_pop <- synth_pop %>%
    group_by(state, district) %>%
    summarise(district_pop = n())
  
  district_sim_delegates <- synth_pop %>%
    group_by(state, district) %>%
    mutate(district_total = n()) %>%
    group_by(state, district, vote) %>%
    summarise(pct = n() / mean(district_total)) %>%
    ungroup() %>%
    mutate(met_threshold = pct >= 0.15) %>% 
    group_by(state, district) %>%
    mutate(pct_valid = (pct*met_threshold) / sum(pct*met_threshold)) %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(candidate_delegates = delegates * pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, district, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate = vote, pct, pct_valid, candidate_delegates) %>%
    arrange(district, candidate)
  
  state_sim_delegates <- district_sim_delegates %>%
    left_join(synth_district_pop, by = c("state", "district")) %>%
    mutate(synth_votes = pct * district_pop) %>%
    group_by(state) %>%
    mutate(state_vote = sum(synth_votes)) %>%
    group_by(state, candidate) %>%
    summarise(state_pct = sum(synth_votes) / mean(state_vote)) %>%
    mutate(district = "At-large") %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(met_threshold = state_pct >= 0.15,
           state_pct_valid = (state_pct*met_threshold) / sum(state_pct*met_threshold),
           candidate_delegates = delegates * state_pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate, pct = state_pct, pct_valid = state_pct_valid, candidate_delegates) %>%
    arrange(district, candidate) %>%
    bind_rows(district_sim_delegates) %>%
    mutate(sim_id = i)
}

md_overperformance <- md_state_percentages_adj %>%
  left_join(state_averages %>% filter(state == "Maryland") %>% dplyr::select(candidate, state_avg), by = c("candidate")) %>%
  mutate(overperformance = state_pct - (state_avg/100 + 0.25*overperformance*bounce_factor)) %>%
  dplyr::select(sim_id, candidate, md_overperformance = overperformance) 

# New York ####
ny_start_time <- Sys.time()

sim_demographics_table <- district_demographics %>%
  filter(state == "New York") %>%
  mutate(prob = predict(dem_primary_logit, newdata = ., type = "response"),
         n_vote = round(pop * prob * turnout * 0.05)) %>%
  dplyr::select(-pop, -prob)

sim_demographics <- sim_demographics_table %>%
  lapply(rep, sim_demographics_table$n_vote) %>%
  as.data.frame() 
state_marginal_demographics <- shapeStateDemo("New York")

ny_averages_over_time <- state_averages_over_time_unsmoothed %>%
  filter(state == "New York", as.numeric(today() - median_date) <= 1) %>%
  mutate(age = as.numeric(today() - median_date),
         nreps = 2 - age)

ny_dirichlet_params <- (ny_averages_over_time %>%
                          lapply(rep, ny_averages_over_time$nreps) %>%
                          as.data.frame() %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          dplyr::select(-median_date) %>%
                          group_by(candidate) %>%
                          mutate(i = 1:n()) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-i) %>%
                          as.matrix() %>%
                          diri.est())$param

ny_dirichlet_params <- ny_dirichlet_params / 4
ny_dirichlet_params[8] <- 0

ny_state_percentages <- (rdirichlet(n_sims, ny_dirichlet_params) %>%
                            as.data.frame() %>%
                            dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, 
                                          steyer = V6, warren = V7, yang = V8)) %>%
  apply(MARGIN = 1, FUN = function(x) x / sum(x)) %>%
  t() %>%
  as.data.frame() %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

ny_state_percentages_adj <- ny_state_percentages %>%
  left_join(overperformance_bounce, by = c("sim_id", "candidate")) %>%
  mutate(state_pct = pmax(state_pct + bounce, 0)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct)) %>%
  left_join(wi_second_choice_division, by = c("candidate", "sim_id")) %>%
  melt(id.vars = c("sim_id", "candidate", "state_pct", "overperformance", "bounce_factor", "stay_in", "bounce"), 
       variable.name = "second_choice", value.name = "pct") %>%
  mutate(pct = pct*state_pct*(!stay_in),
         state_pct = case_when(candidate == second_choice ~ state_pct,
                               candidate != second_choice ~ pct)) %>%
  arrange(sim_id, second_choice, candidate)

overperformance_stats <- ny_state_percentages_adj %>%
  group_by(sim_id, candidate) %>%
  summarise(overperformance = mean(overperformance),
            bounce_factor = mean(bounce_factor))

ny_state_percentages_adj <- ny_state_percentages_adj %>%
  group_by(sim_id, second_choice) %>%
  summarise(state_pct = sum(state_pct)) %>%
  dplyr::select(everything(), candidate = second_choice) %>%
  left_join(wi_stayin_matrix %>% melt(id.vars = NULL, value.name = "stay_in") %>%
              dplyr::select(sim_id = Var1, candidate = Var2, stay_in), by = c("sim_id", "candidate")) %>%
  mutate(state_pct = state_pct * stay_in) %>%
  left_join(overperformance_stats, by = c("sim_id", "candidate"))

ny_sims <- vector("list", n_sims)

### Simulate
ny_sims <- foreach(i = 1:n_sims, .packages = c("Hmisc", "dplyr", "reshape2", "tidyr")) %dopar% {
  demographic_lean_sim <- demographic_leans %>%
    mutate(lean = rnorm(n(), mean = avg_lean, sd = lean_se)) %>%
    dplyr::select(candidate, demographic_var, demographic, lean)
  
  # This simulation's national percentages
  this_sim_state_percentage <- ny_state_percentages_adj %>%
    filter(sim_id == i)
  
  # Apply simulated leans to state average
  state_demographic_sim <- demoCandidateBreakdown("New York", this_sim_state_percentage, demographic_lean_sim, 
                                                  state_marginal_demographics)
  
  # Turn these into probabilities
  gender_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("men", "women")) %>%
    dplyr::select(candidate, gender = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(gender = case_when(gender == "men" ~ "Male",
                              gender == "women" ~ "Female"))
  
  names(gender_breakdown)[-1] <- paste0("gender_", names(gender_breakdown))[-1]
  
  race_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("white", "black", "latino")) %>%
    dplyr::select(candidate, race = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(race = case_when(race == "white" ~ "White",
                            race == "black" ~ "Black",
                            race == "latino" ~ "Latino"))
  
  names(race_breakdown)[-1] <- paste0("race_", names(race_breakdown))[-1]
  
  age_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("age_1829", "age_3044", "age_4564", "age_65")) %>%
    dplyr::select(candidate, age = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(age = case_when(age == "age_1829" ~ "18-29",
                           age == "age_3044" ~ "30-44",
                           age == "age_4564" ~ "45-64",
                           age == "age_65" ~ "65+"))
  
  names(age_breakdown)[-1] <- paste0("age_", names(age_breakdown)[-1])
  
  education_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("college", "no_college")) %>%
    dplyr::select(candidate, education = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(education = case_when(education == "college" ~ "College",
                                 education == "no_college" ~ "Less than college"))
  
  names(education_breakdown)[-1] <- paste0("education_", names(education_breakdown)[-1])
  
  synth_pop <- sim_demographics %>%
    mutate(rand = runif(n()),
           age_regroup = case_when(age == "18-24" ~ "18-29",
                                   age == "25-34" & rand < 0.5 ~ "18-29",
                                   age == "25-34" & rand >= 0.5 ~ "30-44",
                                   age == "35-44" ~ "30-44",
                                   age == "45-64" ~ "45-64",
                                   age == "65+" ~ "65+"),
           id = 1:n()) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age = age_regroup, education) %>%
    mutate_if(is.factor, as.character) %>%
    left_join(gender_breakdown, by = "gender") %>%
    left_join(race_breakdown, by = "race") %>%
    left_join(age_breakdown, by = "age") %>%
    left_join(education_breakdown, by = "education") %>%
    mutate(biden_prob = gender_biden + race_biden + age_biden + education_biden,
           bloomberg_prob = gender_bloomberg + race_bloomberg + age_bloomberg + education_bloomberg,
           buttigieg_prob = gender_buttigieg + race_buttigieg + age_buttigieg + education_buttigieg,
           klobuchar_prob = gender_klobuchar + race_klobuchar + age_klobuchar + education_klobuchar,
           sanders_prob = gender_sanders + race_sanders + age_sanders + education_sanders,
           steyer_prob = gender_steyer + race_steyer + age_steyer + education_steyer,
           warren_prob = gender_warren + race_warren + age_warren + education_warren,
           yang_prob = gender_yang + race_yang + age_yang + education_yang) %>%
    mutate_at(vars(c("biden_prob", "bloomberg_prob", "buttigieg_prob", "klobuchar_prob", "sanders_prob", "steyer_prob", "warren_prob", 
                     "yang_prob")),
              function(x) pmax(x/100, 0)) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age, education, biden_prob, 
                  bloomberg_prob, buttigieg_prob, klobuchar_prob, sanders_prob, steyer_prob, warren_prob, yang_prob) %>%
    melt(id.vars = c("id", "state", "district", "primary_type", "district_delegates", "gender", "race", "age", "education"),
         variable.name = "candidate_prob", value.name = "prob") %>%
    arrange(id, candidate_prob) %>%
    group_by(id) %>%
    mutate(prob = prob / sum(prob),
           cum_prob = cumsum(prob)) %>%
    dplyr::select(-prob) %>%
    ungroup() %>%
    spread(candidate_prob, cum_prob) %>%
    mutate(rand = runif(n()),
           vote = case_when(rand <= biden_prob ~ "biden",
                            rand > biden_prob & rand <= bloomberg_prob ~ "bloomberg",
                            rand > bloomberg_prob & rand <= buttigieg_prob ~ "buttigieg",
                            rand > buttigieg_prob & rand <= klobuchar_prob ~ "klobuchar",
                            rand > klobuchar_prob & rand <= sanders_prob ~ "sanders",
                            rand > sanders_prob & rand <= steyer_prob ~ "steyer",
                            rand > steyer_prob & rand <= warren_prob ~ "warren",
                            rand > warren_prob ~ "yang")) %>%
    as.tbl()
  
  synth_district_pop <- synth_pop %>%
    group_by(state, district) %>%
    summarise(district_pop = n())
  
  district_sim_delegates <- synth_pop %>%
    group_by(state, district) %>%
    mutate(district_total = n()) %>%
    group_by(state, district, vote) %>%
    summarise(pct = n() / mean(district_total)) %>%
    ungroup() %>%
    mutate(met_threshold = pct >= 0.15) %>% 
    group_by(state, district) %>%
    mutate(pct_valid = (pct*met_threshold) / sum(pct*met_threshold)) %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(candidate_delegates = delegates * pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, district, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate = vote, pct, pct_valid, candidate_delegates) %>%
    arrange(district, candidate)
  
  state_sim_delegates <- district_sim_delegates %>%
    left_join(synth_district_pop, by = c("state", "district")) %>%
    mutate(synth_votes = pct * district_pop) %>%
    group_by(state) %>%
    mutate(state_vote = sum(synth_votes)) %>%
    group_by(state, candidate) %>%
    summarise(state_pct = sum(synth_votes) / mean(state_vote)) %>%
    mutate(district = "At-large") %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(met_threshold = state_pct >= 0.15,
           state_pct_valid = (state_pct*met_threshold) / sum(state_pct*met_threshold),
           candidate_delegates = delegates * state_pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate, pct = state_pct, pct_valid = state_pct_valid, candidate_delegates) %>%
    arrange(district, candidate) %>%
    bind_rows(district_sim_delegates) %>%
    mutate(sim_id = i)
}

ny_overperformance <- ny_state_percentages_adj %>%
  left_join(state_averages %>% filter(state == "New York") %>% dplyr::select(candidate, state_avg), by = c("candidate")) %>%
  mutate(overperformance = state_pct - (state_avg/100 + 0.25*overperformance*bounce_factor)) %>%
  dplyr::select(sim_id, candidate, ny_overperformance = overperformance) 


# Pennsylvania ####
pa_start_time <- Sys.time()

sim_demographics_table <- district_demographics %>%
  filter(state == "Pennsylvania") %>%
  mutate(prob = predict(dem_primary_logit, newdata = ., type = "response"),
         n_vote = round(pop * prob * turnout * 0.05)) %>%
  dplyr::select(-pop, -prob)

sim_demographics <- sim_demographics_table %>%
  lapply(rep, sim_demographics_table$n_vote) %>%
  as.data.frame() 
state_marginal_demographics <- shapeStateDemo("Pennsylvania")

pa_averages_over_time <- state_averages_over_time_unsmoothed %>%
  filter(state == "Pennsylvania", as.numeric(today() - median_date) <= 1) %>%
  mutate(age = as.numeric(today() - median_date),
         nreps = 2 - age)

pa_dirichlet_params <- (pa_averages_over_time %>%
                          lapply(rep, pa_averages_over_time$nreps) %>%
                          as.data.frame() %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          dplyr::select(-median_date) %>%
                          group_by(candidate) %>%
                          mutate(i = 1:n()) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-i) %>%
                          as.matrix() %>%
                          diri.est())$param

pa_dirichlet_params <- pa_dirichlet_params / 2
pa_dirichlet_params[8] <- 0

pa_state_percentages <- (rdirichlet(n_sims, pa_dirichlet_params) %>%
                            as.data.frame() %>%
                            dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, 
                                          steyer = V6, warren = V7, yang = V8)) %>%
  apply(MARGIN = 1, FUN = function(x) x / sum(x)) %>%
  t() %>%
  as.data.frame() %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

pa_state_percentages_adj <- pa_state_percentages  %>%
  left_join(overperformance_bounce, by = c("sim_id", "candidate")) %>%
  mutate(state_pct = pmax(state_pct + bounce, 0)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct)) %>%
  left_join(wi_second_choice_division, by = c("candidate", "sim_id")) %>%
  melt(id.vars = c("sim_id", "candidate", "state_pct", "overperformance", "bounce_factor", "stay_in", "bounce"), 
       variable.name = "second_choice", value.name = "pct") %>%
  mutate(pct = pct*state_pct*(!stay_in),
         state_pct = case_when(candidate == second_choice ~ state_pct,
                               candidate != second_choice ~ pct)) %>%
  arrange(sim_id, second_choice, candidate)

overperformance_stats <- pa_state_percentages_adj %>%
  group_by(sim_id, candidate) %>%
  summarise(overperformance = mean(overperformance),
            bounce_factor = mean(bounce_factor))

pa_state_percentages_adj <- pa_state_percentages_adj %>%
  group_by(sim_id, second_choice) %>%
  summarise(state_pct = sum(state_pct)) %>%
  dplyr::select(everything(), candidate = second_choice) %>%
  left_join(wi_stayin_matrix %>% melt(id.vars = NULL, value.name = "stay_in") %>%
              dplyr::select(sim_id = Var1, candidate = Var2, stay_in), by = c("sim_id", "candidate")) %>%
  mutate(state_pct = state_pct * stay_in) %>%
  left_join(overperformance_stats, by = c("sim_id", "candidate"))

pa_sims <- vector("list", n_sims)

### Simulate
pa_sims <- foreach(i = 1:n_sims, .packages = c("Hmisc", "dplyr", "reshape2", "tidyr")) %dopar% {
  demographic_lean_sim <- demographic_leans %>%
    mutate(lean = rnorm(n(), mean = avg_lean, sd = lean_se)) %>%
    dplyr::select(candidate, demographic_var, demographic, lean)
  
  # This simulation's national percentages
  this_sim_state_percentage <- pa_state_percentages_adj %>%
    filter(sim_id == i)
  
  # Apply simulated leans to state average
  state_demographic_sim <- demoCandidateBreakdown("Pennsylvania", this_sim_state_percentage, demographic_lean_sim, 
                                                  state_marginal_demographics)
  
  # Turn these into probabilities
  gender_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("men", "women")) %>%
    dplyr::select(candidate, gender = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(gender = case_when(gender == "men" ~ "Male",
                              gender == "women" ~ "Female"))
  
  names(gender_breakdown)[-1] <- paste0("gender_", names(gender_breakdown))[-1]
  
  race_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("white", "black", "latino")) %>%
    dplyr::select(candidate, race = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(race = case_when(race == "white" ~ "White",
                            race == "black" ~ "Black",
                            race == "latino" ~ "Latino"))
  
  names(race_breakdown)[-1] <- paste0("race_", names(race_breakdown))[-1]
  
  age_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("age_1829", "age_3044", "age_4564", "age_65")) %>%
    dplyr::select(candidate, age = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(age = case_when(age == "age_1829" ~ "18-29",
                           age == "age_3044" ~ "30-44",
                           age == "age_4564" ~ "45-64",
                           age == "age_65" ~ "65+"))
  
  names(age_breakdown)[-1] <- paste0("age_", names(age_breakdown)[-1])
  
  education_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("college", "no_college")) %>%
    dplyr::select(candidate, education = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(education = case_when(education == "college" ~ "College",
                                 education == "no_college" ~ "Less than college"))
  
  names(education_breakdown)[-1] <- paste0("education_", names(education_breakdown)[-1])
  
  synth_pop <- sim_demographics %>%
    mutate(rand = runif(n()),
           age_regroup = case_when(age == "18-24" ~ "18-29",
                                   age == "25-34" & rand < 0.5 ~ "18-29",
                                   age == "25-34" & rand >= 0.5 ~ "30-44",
                                   age == "35-44" ~ "30-44",
                                   age == "45-64" ~ "45-64",
                                   age == "65+" ~ "65+"),
           id = 1:n()) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age = age_regroup, education) %>%
    mutate_if(is.factor, as.character) %>%
    left_join(gender_breakdown, by = "gender") %>%
    left_join(race_breakdown, by = "race") %>%
    left_join(age_breakdown, by = "age") %>%
    left_join(education_breakdown, by = "education") %>%
    mutate(biden_prob = gender_biden + race_biden + age_biden + education_biden,
           bloomberg_prob = gender_bloomberg + race_bloomberg + age_bloomberg + education_bloomberg,
           buttigieg_prob = gender_buttigieg + race_buttigieg + age_buttigieg + education_buttigieg,
           klobuchar_prob = gender_klobuchar + race_klobuchar + age_klobuchar + education_klobuchar,
           sanders_prob = gender_sanders + race_sanders + age_sanders + education_sanders,
           steyer_prob = gender_steyer + race_steyer + age_steyer + education_steyer,
           warren_prob = gender_warren + race_warren + age_warren + education_warren,
           yang_prob = gender_yang + race_yang + age_yang + education_yang) %>%
    mutate_at(vars(c("biden_prob", "bloomberg_prob", "buttigieg_prob", "klobuchar_prob", "sanders_prob", "steyer_prob", "warren_prob", 
                     "yang_prob")),
              function(x) pmax(x/100, 0)) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age, education, biden_prob, 
                  bloomberg_prob, buttigieg_prob, klobuchar_prob, sanders_prob, steyer_prob, warren_prob, yang_prob) %>%
    melt(id.vars = c("id", "state", "district", "primary_type", "district_delegates", "gender", "race", "age", "education"),
         variable.name = "candidate_prob", value.name = "prob") %>%
    arrange(id, candidate_prob) %>%
    group_by(id) %>%
    mutate(prob = prob / sum(prob),
           cum_prob = cumsum(prob)) %>%
    dplyr::select(-prob) %>%
    ungroup() %>%
    spread(candidate_prob, cum_prob) %>%
    mutate(rand = runif(n()),
           vote = case_when(rand <= biden_prob ~ "biden",
                            rand > biden_prob & rand <= bloomberg_prob ~ "bloomberg",
                            rand > bloomberg_prob & rand <= buttigieg_prob ~ "buttigieg",
                            rand > buttigieg_prob & rand <= klobuchar_prob ~ "klobuchar",
                            rand > klobuchar_prob & rand <= sanders_prob ~ "sanders",
                            rand > sanders_prob & rand <= steyer_prob ~ "steyer",
                            rand > steyer_prob & rand <= warren_prob ~ "warren",
                            rand > warren_prob ~ "yang")) %>%
    as.tbl()
  
  synth_district_pop <- synth_pop %>%
    group_by(state, district) %>%
    summarise(district_pop = n())
  
  district_sim_delegates <- synth_pop %>%
    group_by(state, district) %>%
    mutate(district_total = n()) %>%
    group_by(state, district, vote) %>%
    summarise(pct = n() / mean(district_total)) %>%
    ungroup() %>%
    mutate(met_threshold = pct >= 0.15) %>% 
    group_by(state, district) %>%
    mutate(pct_valid = (pct*met_threshold) / sum(pct*met_threshold)) %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(candidate_delegates = delegates * pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, district, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate = vote, pct, pct_valid, candidate_delegates) %>%
    arrange(district, candidate)
  
  state_sim_delegates <- district_sim_delegates %>%
    left_join(synth_district_pop, by = c("state", "district")) %>%
    mutate(synth_votes = pct * district_pop) %>%
    group_by(state) %>%
    mutate(state_vote = sum(synth_votes)) %>%
    group_by(state, candidate) %>%
    summarise(state_pct = sum(synth_votes) / mean(state_vote)) %>%
    mutate(district = "At-large") %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(met_threshold = state_pct >= 0.15,
           state_pct_valid = (state_pct*met_threshold) / sum(state_pct*met_threshold),
           candidate_delegates = delegates * state_pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate, pct = state_pct, pct_valid = state_pct_valid, candidate_delegates) %>%
    arrange(district, candidate) %>%
    bind_rows(district_sim_delegates) %>%
    mutate(sim_id = i)
}

pa_overperformance <- pa_state_percentages_adj %>%
  left_join(state_averages %>% filter(state == "Pennsylvania") %>% dplyr::select(candidate, state_avg), by = c("candidate")) %>%
  mutate(overperformance = state_pct - (state_avg/100 + 0.25*overperformance*bounce_factor)) %>%
  dplyr::select(sim_id, candidate, pa_overperformance = overperformance) 

# Rhode Island ####
ri_start_time <- Sys.time()
# Draw votes
sim_demographics_table <- district_demographics %>%
  filter(state == "Rhode Island") %>%
  mutate(prob = predict(dem_primary_logit, newdata = ., type = "response"),
         n_vote = round(pop * prob * turnout * 0.05)) %>%
  dplyr::select(-pop, -prob)

sim_demographics <- sim_demographics_table %>%
  lapply(rep, sim_demographics_table$n_vote) %>%
  as.data.frame() 
state_marginal_demographics <- shapeStateDemo("Rhode Island")

# Have no Rhode Island polls; use nearest neighbors
ri_averages_over_time <- state_averages_over_time_unsmoothed %>%
  filter(state %in% c("Illinois", "Massachusetts", "New York"), as.numeric(today() - median_date) <= 1) %>%
  mutate(age = as.numeric(today() - median_date),
         nreps = 2 - age,
         inv_distance = case_when(state == "Illinois" ~ 0.508,
                                  state == "Massachusetts" ~ 0.412,
                                  state == "New York" ~ 0.415)) %>%
  group_by(candidate, median_date, age, nreps) %>%
  summarise(pct = wtd.mean(pct, inv_distance))

ri_dirichlet_params <- ((ri_averages_over_time %>%
                           lapply(rep, ri_averages_over_time$nreps) %>%
                           as.data.frame() %>%
                           dplyr::select(candidate, median_date, pct) %>%
                           na.omit() %>%
                           dplyr::select(-median_date) %>%
                           group_by(candidate) %>%
                           mutate(i = 1:n()) %>%
                           spread(candidate, pct) %>%
                           dplyr::select(-i) %>%
                           as.matrix() %>%
                           diri.est())$param)

ri_dirichlet_params <- 0.5 * ri_dirichlet_params / max(1, min(ri_dirichlet_params))
ri_dirichlet_params[8] <- 0

ri_state_percentages <- (rdirichlet(n_sims, ri_dirichlet_params) %>%
                            as.data.frame() %>%
                            dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, 
                                          steyer = V6, warren = V7, yang = V8)) %>%
  apply(MARGIN = 1, FUN = function(x) x / sum(x)) %>%
  t() %>%
  as.data.frame() %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

ri_state_percentages_adj <- ri_state_percentages  %>%
  left_join(overperformance_bounce, by = c("sim_id", "candidate")) %>%
  mutate(state_pct = pmax(state_pct + bounce, 0)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct)) %>%
  left_join(wi_second_choice_division, by = c("candidate", "sim_id")) %>%
  melt(id.vars = c("sim_id", "candidate", "state_pct", "overperformance", "bounce_factor", "stay_in", "bounce"), 
       variable.name = "second_choice", value.name = "pct") %>%
  mutate(pct = pct*state_pct*(!stay_in),
         state_pct = case_when(candidate == second_choice ~ state_pct,
                               candidate != second_choice ~ pct)) %>%
  arrange(sim_id, second_choice, candidate)

overperformance_stats <- ri_state_percentages_adj %>%
  group_by(sim_id, candidate) %>%
  summarise(overperformance = mean(overperformance),
            bounce_factor = mean(bounce_factor))

ri_state_percentages_adj <- ri_state_percentages_adj %>%
  group_by(sim_id, second_choice) %>%
  summarise(state_pct = sum(state_pct)) %>%
  dplyr::select(everything(), candidate = second_choice) %>%
  left_join(wi_stayin_matrix %>% melt(id.vars = NULL, value.name = "stay_in") %>%
              dplyr::select(sim_id = Var1, candidate = Var2, stay_in), by = c("sim_id", "candidate")) %>%
  mutate(state_pct = state_pct * stay_in) %>%
  left_join(overperformance_stats, by = c("sim_id", "candidate"))

ri_sims <- vector("list", n_sims)

ri_sims <- foreach(i = 1:n_sims, .packages = c("Hmisc", "dplyr", "reshape2", "tidyr")) %dopar% {
  demographic_lean_sim <- demographic_leans %>%
    mutate(lean = rnorm(n(), mean = avg_lean, sd = lean_se)) %>%
    dplyr::select(candidate, demographic_var, demographic, lean)
  
  # This simulation's national percentages
  this_sim_state_percentage <- ri_state_percentages_adj %>%
    filter(sim_id == i)
  
  # Apply simulated leans to state average
  state_demographic_sim <- demoCandidateBreakdown("Massachusetts", this_sim_state_percentage, demographic_lean_sim, 
                                                  state_marginal_demographics)
  
  # Turn these into probabilities
  gender_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("men", "women")) %>%
    dplyr::select(candidate, gender = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(gender = case_when(gender == "men" ~ "Male",
                              gender == "women" ~ "Female"))
  
  names(gender_breakdown)[-1] <- paste0("gender_", names(gender_breakdown))[-1]
  
  race_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("white", "black", "latino")) %>%
    dplyr::select(candidate, race = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(race = case_when(race == "white" ~ "White",
                            race == "black" ~ "Black",
                            race == "latino" ~ "Latino"))
  
  names(race_breakdown)[-1] <- paste0("race_", names(race_breakdown))[-1]
  
  age_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("age_1829", "age_3044", "age_4564", "age_65")) %>%
    dplyr::select(candidate, age = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(age = case_when(age == "age_1829" ~ "18-29",
                           age == "age_3044" ~ "30-44",
                           age == "age_4564" ~ "45-64",
                           age == "age_65" ~ "65+"))
  
  names(age_breakdown)[-1] <- paste0("age_", names(age_breakdown)[-1])
  
  education_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("college", "no_college")) %>%
    dplyr::select(candidate, education = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(education = case_when(education == "college" ~ "College",
                                 education == "no_college" ~ "Less than college"))
  
  names(education_breakdown)[-1] <- paste0("education_", names(education_breakdown)[-1])
  
  synth_pop <- sim_demographics %>%
    mutate(rand = runif(n()),
           age_regroup = case_when(age == "18-24" ~ "18-29",
                                   age == "25-34" & rand < 0.5 ~ "18-29",
                                   age == "25-34" & rand >= 0.5 ~ "30-44",
                                   age == "35-44" ~ "30-44",
                                   age == "45-64" ~ "45-64",
                                   age == "65+" ~ "65+"),
           id = 1:n()) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age = age_regroup, education) %>%
    mutate_if(is.factor, as.character) %>%
    left_join(gender_breakdown, by = "gender") %>%
    left_join(race_breakdown, by = "race") %>%
    left_join(age_breakdown, by = "age") %>%
    left_join(education_breakdown, by = "education") %>%
    mutate(biden_prob = gender_biden + race_biden + age_biden + education_biden,
           bloomberg_prob = gender_bloomberg + race_bloomberg + age_bloomberg + education_bloomberg,
           buttigieg_prob = gender_buttigieg + race_buttigieg + age_buttigieg + education_buttigieg,
           klobuchar_prob = gender_klobuchar + race_klobuchar + age_klobuchar + education_klobuchar,
           sanders_prob = gender_sanders + race_sanders + age_sanders + education_sanders,
           steyer_prob = gender_steyer + race_steyer + age_steyer + education_steyer,
           warren_prob = gender_warren + race_warren + age_warren + education_warren,
           yang_prob = gender_yang + race_yang + age_yang + education_yang) %>%
    mutate_at(vars(c("biden_prob", "bloomberg_prob", "buttigieg_prob", "klobuchar_prob", "sanders_prob", "steyer_prob", "warren_prob", 
                     "yang_prob")), function(x) pmax(x/100, 0)) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age, education, biden_prob, 
                  bloomberg_prob, buttigieg_prob, klobuchar_prob, sanders_prob, steyer_prob, warren_prob, yang_prob) %>%
    melt(id.vars = c("id", "state", "district", "primary_type", "district_delegates", "gender", "race", "age", "education"),
         variable.name = "candidate_prob", value.name = "prob") %>%
    arrange(id, candidate_prob) %>%
    group_by(id) %>%
    mutate(prob = prob / sum(prob),
           cum_prob = cumsum(prob)) %>%
    dplyr::select(-prob) %>%
    ungroup() %>%
    spread(candidate_prob, cum_prob) %>%
    mutate(rand = runif(n()),
           vote = case_when(rand <= biden_prob ~ "biden",
                            rand > biden_prob & rand <= bloomberg_prob ~ "bloomberg",
                            rand > bloomberg_prob & rand <= buttigieg_prob ~ "buttigieg",
                            rand > buttigieg_prob & rand <= klobuchar_prob ~ "klobuchar",
                            rand > klobuchar_prob & rand <= sanders_prob ~ "sanders",
                            rand > sanders_prob & rand <= steyer_prob ~ "steyer",
                            rand > steyer_prob & rand <= warren_prob ~ "warren",
                            rand > warren_prob ~ "yang")) %>%
    as.tbl()
  
  
  synth_district_pop <- synth_pop %>%
    group_by(state, district) %>%
    summarise(district_pop = n())
  
  district_sim_delegates <- synth_pop %>%
    group_by(state, district) %>%
    mutate(district_total = n()) %>%
    group_by(state, district, vote) %>%
    summarise(pct = n() / mean(district_total)) %>%
    ungroup() %>%
    mutate(met_threshold = pct >= 0.15) %>% 
    group_by(state, district) %>%
    mutate(pct_valid = (pct*met_threshold) / sum(pct*met_threshold)) %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(candidate_delegates = delegates * pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, district, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate = vote, pct, pct_valid, candidate_delegates) %>%
    arrange(district, candidate)
  
  state_sim_delegates <- district_sim_delegates %>%
    left_join(synth_district_pop, by = c("state", "district")) %>%
    mutate(synth_votes = pct * district_pop) %>%
    group_by(state) %>%
    mutate(state_vote = sum(synth_votes)) %>%
    group_by(state, candidate) %>%
    summarise(state_pct = sum(synth_votes) / mean(state_vote)) %>%
    mutate(district = "At-large") %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(met_threshold = state_pct >= 0.15,
           state_pct_valid = (state_pct*met_threshold) / sum(state_pct*met_threshold),
           candidate_delegates = delegates * state_pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate, pct = state_pct, pct_valid = state_pct_valid, candidate_delegates) %>%
    arrange(district, candidate) %>%
    bind_rows(district_sim_delegates) %>%
    mutate(sim_id = i)
}

# Acela primary overperformances ####
acela_overperformance <- de_overperformance %>%
  left_join(md_overperformance, by = c("sim_id", "candidate")) %>%
  left_join(ny_overperformance, by = c("sim_id", "candidate")) %>%
  left_join(pa_overperformance, by = c("sim_id", "candidate")) %>%
  mutate(overperformance = ((1^2)*de_overperformance + (8^2)*md_overperformance + (27^2)*ny_overperformance + (18^2)*pa_overperformance) / 
           (1^2 + 8^2 + 27^2 + 18^2)) %>%
  dplyr::select(sim_id, candidate, overperformance)

acela_overperformance_matrix <- acela_overperformance %>%
  spread(candidate, overperformance) %>%
  ungroup() %>%
  dplyr::select(-sim_id) %>%
  as.matrix()

# Acela primary dropouts ####
## What share of the delegates do candidates have?
delegate_percentage <-  bind_rows(ia_sims, nh_sims, nv_sims, sc_sims, al_sims, ca_sims, co_sims, ma_sims, mn_sims, nc_sims, ok_sims, tn_sims,
                                  tx_sims, ut_sims, va_sims, me_sims, id_sims, mi_sims, ms_sims, mo_sims, nd_sims, oh_sims, wa_sims, na_sims,
                                  az_sims, fl_sims, il_sims, ga_sims, pr_sims, ak_sims, hi_sims, la_sims, wy_sims, wi_sims, ct_sims, de_sims,
                                  md_sims, ny_sims, pa_sims, ri_sims) %>% 
  group_by(sim_id, candidate) %>% 
  summarise(delegates = sum(candidate_delegates)) %>% 
  mutate(delegate_pct = delegates / sum(delegates)) 

low_delegates_acela <- delegate_percentage %>%
  mutate(low_delegates = delegate_pct < 0.15) %>%
  dplyr::select(-delegates, -delegate_pct) %>%
  spread(candidate, low_delegates) %>%
  ungroup()

acela_stayin_matrix <- (!(low_delegates_acela %>%
                            dplyr::select(-sim_id) %>%
                            as.matrix()) | (acela_overperformance_matrix >= 0.10)) &
  (acela_overperformance_matrix > -0.15) & (wi_stayin_matrix)

acela_second_choice_division <- acela_stayin_matrix %>%
  as.data.frame() %>%
  melt(id.vars = NULL, variable.name = "candidate", value.name = "stay_in") %>%
  mutate(candidate = as.character(candidate),
         sim_id = rep(1:n_sims, 8)) %>%
  as.tbl() %>%
  left_join(second_choice, by = c("candidate" = "first_choice"))

# Guam (2 May 2020) ####
bounce_factor <- tibble(sim_id = 1:n_sims, bounce_factor = rbeta(n_sims, shape1 = 8, shape2 = 3))

overperformance_bounce <- acela_overperformance %>%
  left_join(bounce_factor, by = "sim_id") %>%
  mutate(bounce = overperformance * bounce_factor)

gu_start_time <- Sys.time()
# Shortcut since I know nothing about what the situation looks like on the ground: 
### Draw vote shares
gu_averages_over_time <- national_averages_adjusted %>%
  filter(as.numeric(today() - median_date) <= 1) %>%
  mutate(age = as.numeric(today() - median_date),
         nreps = 2 - age) %>%
  group_by(median_date) %>%
  mutate(pct = pct / sum(pct))

gu_dirichlet_params <- (gu_averages_over_time %>%
                          lapply(rep, gu_averages_over_time$nreps) %>%
                          as.data.frame() %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          dplyr::select(-median_date) %>%
                          group_by(candidate) %>%
                          mutate(i = 1:n()) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-i) %>%
                          as.matrix() %>%
                          diri.est())$param

gu_dirichlet_params <- 0.5 * gu_dirichlet_params / max(1, min(gu_dirichlet_params))
gu_dirichlet_params[8] <- 0

gu_state_percentages <- (rdirichlet(n_sims, gu_dirichlet_params) %>%
                            as.data.frame() %>%
                            dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, 
                                          steyer = V6, warren = V7, yang = V8)) %>%
  apply(MARGIN = 1, FUN = function(x) x / sum(x)) %>%
  t() %>%
  as.data.frame() %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

gu_state_percentages_adj <- gu_state_percentages %>%
  left_join(overperformance_bounce, by = c("sim_id", "candidate")) %>%
  mutate(state_pct = pmax(state_pct + bounce, 0)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct)) %>%
  left_join(acela_second_choice_division, by = c("candidate", "sim_id")) %>%
  melt(id.vars = c("sim_id", "candidate", "state_pct", "overperformance", "bounce_factor", "stay_in", "bounce"), 
       variable.name = "second_choice", value.name = "pct") %>%
  mutate(pct = pct*state_pct*(!stay_in),
         state_pct = case_when(candidate == second_choice ~ state_pct,
                               candidate != second_choice ~ pct)) %>%
  arrange(sim_id, second_choice, candidate)

overperformance_stats <- gu_state_percentages_adj %>%
  group_by(sim_id, candidate) %>%
  summarise(overperformance = mean(overperformance),
            bounce_factor = mean(bounce_factor))

gu_state_percentages_adj <- gu_state_percentages_adj %>%
  group_by(sim_id, second_choice) %>%
  summarise(state_pct = sum(state_pct)) %>%
  dplyr::select(everything(), candidate = second_choice) %>%
  left_join(acela_stayin_matrix %>% melt(id.vars = NULL, value.name = "stay_in") %>%
              dplyr::select(sim_id = Var1, candidate = Var2, stay_in), by = c("sim_id", "candidate")) %>%
  mutate(state_pct = state_pct * stay_in) %>%
  left_join(overperformance_stats, by = c("sim_id", "candidate"))

gu_sims <- vector("list", n_sims)

### Simulate
gu_sims <- foreach(i = 1:n_sims, .packages = c("Hmisc", "dplyr", "reshape2", "tidyr")) %dopar% {
  this_sim_state_percentage <- gu_state_percentages %>%
    filter(sim_id == i)
  
  second_choice_sim <- second_choice %>%
    melt(id.vars = "first_choice", variable.name = "second_choice", value.name = "pct") %>%
    mutate(alpha = 50 * pct / (1 - pct),
           second_pct_sim = rbeta(n(), shape1 = alpha, shape2 = 30)) %>%
    dplyr::select(first_choice, second_choice, second_pct_sim)
  
  district_sim_pct <- this_sim_state_percentage %>%
    mutate(met_threshold = state_pct >= 0.15)
  
  state_sim_delegates <- district_sim_pct %>%
    merge(second_choice_sim, by.x = "candidate", by.y = "first_choice", all.x = TRUE) %>%
    merge(district_sim_pct, by.x = "second_choice", by.y = "candidate") %>%
    mutate(pct_from_second_choice = state_pct.y * second_pct_sim * (second_choice != candidate) * (met_threshold.x) * (!met_threshold.y)) %>%
    group_by(candidate, state_pct.x, met_threshold = met_threshold.x) %>%
    summarise(pct_from_second_choice = sum(pct_from_second_choice)) %>%
    ungroup() %>%
    mutate(pct_valid = (state_pct.x + pct_from_second_choice)*met_threshold) %>% 
    mutate(pct_valid = pct_valid / sum(pct_valid),
           state = "Guam",
           district = "At-large") %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(candidate_delegates = delegates * pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, district, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate, pct = state_pct.x, pct_valid, candidate_delegates) %>%
    arrange(district, candidate) %>%
    mutate(sim_id = i)
}

# Kansas (2 May 2020) ####
ks_start_time <- Sys.time()
# Draw votes
sim_demographics_table <- district_demographics %>%
  filter(state == "Kansas") %>%
  mutate(prob = predict(dem_primary_logit, newdata = ., type = "response"),
         n_vote = round(pop * prob * turnout * 0.05)) %>%
  dplyr::select(-pop, -prob)

sim_demographics <- sim_demographics_table %>%
  lapply(rep, sim_demographics_table$n_vote) %>%
  as.data.frame() 
state_marginal_demographics <- shapeStateDemo("Kansas")

# Have no Kansas polls; use nearest neighbors
ks_averages_over_time <- state_averages_over_time_unsmoothed %>%
  filter(state %in% c("Missouri", "Wisconsin", "Iowa"), as.numeric(today() - median_date) <= 1) %>%
  mutate(age = as.numeric(today() - median_date),
         nreps = 2 - age,
         inv_distance = case_when(state == "Iowa" ~ 0.527,
                                  state == "Missouri" ~ 0.536,
                                  state == "Wisconsin" ~ 0.468)) %>%
  group_by(candidate, median_date, age, nreps) %>%
  summarise(pct = wtd.mean(pct, inv_distance))

ks_dirichlet_params <- ((ks_averages_over_time %>%
                           lapply(rep, ks_averages_over_time$nreps) %>%
                           as.data.frame() %>%
                           dplyr::select(candidate, median_date, pct) %>%
                           na.omit() %>%
                           dplyr::select(-median_date) %>%
                           group_by(candidate) %>%
                           mutate(i = 1:n()) %>%
                           spread(candidate, pct) %>%
                           dplyr::select(-i) %>%
                           as.matrix() %>%
                           diri.est())$param)

ks_dirichlet_params <- 0.5 * ks_dirichlet_params / max(1, min(ks_dirichlet_params))
ks_dirichlet_params[8] <- 0

ks_state_percentages <- (rdirichlet(n_sims, ks_dirichlet_params) %>%
                            as.data.frame() %>%
                            dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, 
                                          steyer = V6, warren = V7, yang = V8)) %>%
  apply(MARGIN = 1, FUN = function(x) x / sum(x)) %>%
  t() %>%
  as.data.frame() %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

ks_state_percentages_adj <- ks_state_percentages %>%
  left_join(overperformance_bounce, by = c("sim_id", "candidate")) %>%
  mutate(state_pct = pmax(state_pct + bounce, 0)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct)) %>%
  left_join(acela_second_choice_division, by = c("candidate", "sim_id")) %>%
  melt(id.vars = c("sim_id", "candidate", "state_pct", "overperformance", "bounce_factor", "stay_in", "bounce"), 
       variable.name = "second_choice", value.name = "pct") %>%
  mutate(pct = pct*state_pct*(!stay_in),
         state_pct = case_when(candidate == second_choice ~ state_pct,
                               candidate != second_choice ~ pct)) %>%
  arrange(sim_id, second_choice, candidate)

overperformance_stats <- ks_state_percentages_adj %>%
  group_by(sim_id, candidate) %>%
  summarise(overperformance = mean(overperformance),
            bounce_factor = mean(bounce_factor))

ks_state_percentages_adj <- ks_state_percentages_adj %>%
  group_by(sim_id, second_choice) %>%
  summarise(state_pct = sum(state_pct)) %>%
  dplyr::select(everything(), candidate = second_choice) %>%
  left_join(acela_stayin_matrix %>% melt(id.vars = NULL, value.name = "stay_in") %>%
              dplyr::select(sim_id = Var1, candidate = Var2, stay_in), by = c("sim_id", "candidate")) %>%
  mutate(state_pct = state_pct * stay_in) %>%
  left_join(overperformance_stats, by = c("sim_id", "candidate"))

ks_sims <- vector("list", n_sims)

ks_sims <- foreach(i = 1:n_sims, .packages = c("Hmisc", "dplyr", "reshape2", "tidyr")) %dopar% {
  demographic_lean_sim <- demographic_leans %>%
    mutate(lean = rnorm(n(), mean = avg_lean, sd = lean_se)) %>%
    dplyr::select(candidate, demographic_var, demographic, lean)
  
  # This simulation's national percentages
  this_sim_state_percentage <- ks_state_percentages_adj %>%
    filter(sim_id == i)
  
  # Apply simulated leans to state average
  state_demographic_sim <- demoCandidateBreakdown("Missouri", this_sim_state_percentage, demographic_lean_sim, 
                                                  state_marginal_demographics)
  
  # Turn these into probabilities
  gender_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("men", "women")) %>%
    dplyr::select(candidate, gender = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(gender = case_when(gender == "men" ~ "Male",
                              gender == "women" ~ "Female"))
  
  names(gender_breakdown)[-1] <- paste0("gender_", names(gender_breakdown))[-1]
  
  race_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("white", "black", "latino")) %>%
    dplyr::select(candidate, race = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(race = case_when(race == "white" ~ "White",
                            race == "black" ~ "Black",
                            race == "latino" ~ "Latino"))
  
  names(race_breakdown)[-1] <- paste0("race_", names(race_breakdown))[-1]
  
  age_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("age_1829", "age_3044", "age_4564", "age_65")) %>%
    dplyr::select(candidate, age = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(age = case_when(age == "age_1829" ~ "18-29",
                           age == "age_3044" ~ "30-44",
                           age == "age_4564" ~ "45-64",
                           age == "age_65" ~ "65+"))
  
  names(age_breakdown)[-1] <- paste0("age_", names(age_breakdown)[-1])
  
  education_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("college", "no_college")) %>%
    dplyr::select(candidate, education = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(education = case_when(education == "college" ~ "College",
                                 education == "no_college" ~ "Less than college"))
  
  names(education_breakdown)[-1] <- paste0("education_", names(education_breakdown)[-1])
  
  synth_pop <- sim_demographics %>%
    mutate(rand = runif(n()),
           age_regroup = case_when(age == "18-24" ~ "18-29",
                                   age == "25-34" & rand < 0.5 ~ "18-29",
                                   age == "25-34" & rand >= 0.5 ~ "30-44",
                                   age == "35-44" ~ "30-44",
                                   age == "45-64" ~ "45-64",
                                   age == "65+" ~ "65+"),
           id = 1:n()) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age = age_regroup, education) %>%
    mutate_if(is.factor, as.character) %>%
    left_join(gender_breakdown, by = "gender") %>%
    left_join(race_breakdown, by = "race") %>%
    left_join(age_breakdown, by = "age") %>%
    left_join(education_breakdown, by = "education") %>%
    mutate(biden_prob = gender_biden + race_biden + age_biden + education_biden,
           bloomberg_prob = gender_bloomberg + race_bloomberg + age_bloomberg + education_bloomberg,
           buttigieg_prob = gender_buttigieg + race_buttigieg + age_buttigieg + education_buttigieg,
           klobuchar_prob = gender_klobuchar + race_klobuchar + age_klobuchar + education_klobuchar,
           sanders_prob = gender_sanders + race_sanders + age_sanders + education_sanders,
           steyer_prob = gender_steyer + race_steyer + age_steyer + education_steyer,
           warren_prob = gender_warren + race_warren + age_warren + education_warren,
           yang_prob = gender_yang + race_yang + age_yang + education_yang) %>%
    mutate_at(vars(c("biden_prob", "bloomberg_prob", "buttigieg_prob", "klobuchar_prob", "sanders_prob", "steyer_prob", "warren_prob", 
                     "yang_prob")), function(x) pmax(x/100, 0)) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age, education, biden_prob, 
                  bloomberg_prob, buttigieg_prob, klobuchar_prob, sanders_prob, steyer_prob, warren_prob, yang_prob) %>%
    melt(id.vars = c("id", "state", "district", "primary_type", "district_delegates", "gender", "race", "age", "education"),
         variable.name = "candidate_prob", value.name = "prob") %>%
    arrange(id, candidate_prob) %>%
    group_by(id) %>%
    mutate(prob = prob / sum(prob),
           cum_prob = cumsum(prob)) %>%
    dplyr::select(-prob) %>%
    ungroup() %>%
    spread(candidate_prob, cum_prob) %>%
    mutate(rand = runif(n()),
           vote = case_when(rand <= biden_prob ~ "biden",
                            rand > biden_prob & rand <= bloomberg_prob ~ "bloomberg",
                            rand > bloomberg_prob & rand <= buttigieg_prob ~ "buttigieg",
                            rand > buttigieg_prob & rand <= klobuchar_prob ~ "klobuchar",
                            rand > klobuchar_prob & rand <= sanders_prob ~ "sanders",
                            rand > sanders_prob & rand <= steyer_prob ~ "steyer",
                            rand > steyer_prob & rand <= warren_prob ~ "warren",
                            rand > warren_prob ~ "yang")) %>%
    as.tbl()
  
  
  synth_district_pop <- synth_pop %>%
    group_by(state, district) %>%
    summarise(district_pop = n())
  
  district_sim_delegates <- synth_pop %>%
    group_by(state, district) %>%
    mutate(district_total = n()) %>%
    group_by(state, district, vote) %>%
    summarise(pct = n() / mean(district_total)) %>%
    ungroup() %>%
    mutate(met_threshold = pct >= 0.15) %>% 
    group_by(state, district) %>%
    mutate(pct_valid = (pct*met_threshold) / sum(pct*met_threshold)) %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(candidate_delegates = delegates * pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, district, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate = vote, pct, pct_valid, candidate_delegates) %>%
    arrange(district, candidate)
  
  state_sim_delegates <- district_sim_delegates %>%
    left_join(synth_district_pop, by = c("state", "district")) %>%
    mutate(synth_votes = pct * district_pop) %>%
    group_by(state) %>%
    mutate(state_vote = sum(synth_votes)) %>%
    group_by(state, candidate) %>%
    summarise(state_pct = sum(synth_votes) / mean(state_vote)) %>%
    mutate(district = "At-large") %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(met_threshold = state_pct >= 0.15,
           state_pct_valid = (state_pct*met_threshold) / sum(state_pct*met_threshold),
           candidate_delegates = delegates * state_pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate, pct = state_pct, pct_valid = state_pct_valid, candidate_delegates) %>%
    arrange(district, candidate) %>%
    bind_rows(district_sim_delegates) %>%
    mutate(sim_id = i)
}

# Indiana (5 May 2020) ####
bounce_factor <- tibble(sim_id = 1:n_sims, bounce_factor = rbeta(n_sims, shape1 = 6, shape2 = 3))

overperformance_bounce <- acela_overperformance %>%
  left_join(bounce_factor, by = "sim_id") %>%
  mutate(bounce = overperformance * bounce_factor)

in_start_time <- Sys.time()

sim_demographics_table <- district_demographics %>%
  filter(state == "Indiana") %>%
  mutate(prob = predict(dem_primary_logit, newdata = ., type = "response"),
         n_vote = round(pop * prob * turnout * 0.05)) %>%
  dplyr::select(-pop, -prob)

sim_demographics <- sim_demographics_table %>%
  lapply(rep, sim_demographics_table$n_vote) %>%
  as.data.frame() 
state_marginal_demographics <- shapeStateDemo("Indiana")

mimooh_averages_over_time <- state_averages_over_time_unsmoothed %>%
  filter(state %in% c("Michigan", "Missouri", "Ohio"), as.numeric(today() - median_date) <= 1) %>%
  mutate(age = as.numeric(today() - median_date),
         nreps = 2 - age,
         inv_distance = case_when(state == "Michigan" ~ 0.606,
                                  state == "Missouri" ~ 1.07,
                                  state == "Ohio" ~ 0.820)) %>%
  group_by(candidate, median_date, age, nreps) %>%
  summarise(pct = wtd.mean(pct, inv_distance)) %>%
  filter(candidate %in% c("bloomberg", "steyer", "yang"))

in_averages_over_time <- state_averages_over_time_unsmoothed %>%
  filter(state == "Indiana", as.numeric(today() - median_date) <= 1) %>%
  mutate(age = as.numeric(today() - median_date),
         nreps = 2 - age)

in_dirichlet_params <- (in_averages_over_time %>%
                          lapply(rep, in_averages_over_time$nreps) %>%
                          as.data.frame() %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          dplyr::select(-median_date) %>%
                          group_by(candidate) %>%
                          mutate(i = 1:n()) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-i) %>%
                          as.matrix() %>%
                          diri.est())$param

in_dirichlet_params <- 0.5 * in_dirichlet_params / max(1, min(in_dirichlet_params))
in_dirichlet_params[8] <- 0

in_state_percentages <- (rdirichlet(n_sims, in_dirichlet_params) %>%
                            as.data.frame() %>%
                            dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, 
                                          steyer = V6, warren = V7, yang = V8)) %>%
  apply(MARGIN = 1, FUN = function(x) x / sum(x)) %>%
  t() %>%
  as.data.frame() %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

in_state_percentages_adj <- in_state_percentages %>%
  left_join(overperformance_bounce, by = c("sim_id", "candidate")) %>%
  mutate(state_pct = pmax(state_pct + bounce, 0)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct)) %>%
  left_join(acela_second_choice_division, by = c("candidate", "sim_id")) %>%
  melt(id.vars = c("sim_id", "candidate", "state_pct", "overperformance", "bounce_factor", "stay_in", "bounce"), 
       variable.name = "second_choice", value.name = "pct") %>%
  mutate(pct = pct*state_pct*(!stay_in),
         state_pct = case_when(candidate == second_choice ~ state_pct,
                               candidate != second_choice ~ pct)) %>%
  arrange(sim_id, second_choice, candidate)

overperformance_stats <- in_state_percentages_adj %>%
  group_by(sim_id, candidate) %>%
  summarise(overperformance = mean(overperformance),
            bounce_factor = mean(bounce_factor))

in_state_percentages_adj <- in_state_percentages_adj %>%
  group_by(sim_id, second_choice) %>%
  summarise(state_pct = sum(state_pct)) %>%
  dplyr::select(everything(), candidate = second_choice) %>%
  left_join(acela_stayin_matrix %>% melt(id.vars = NULL, value.name = "stay_in") %>%
              dplyr::select(sim_id = Var1, candidate = Var2, stay_in), by = c("sim_id", "candidate")) %>%
  mutate(state_pct = state_pct * stay_in) %>%
  left_join(overperformance_stats, by = c("sim_id", "candidate"))

in_sims <- vector("list", n_sims)

### Simulate
in_sims <- foreach(i = 1:n_sims, .packages = c("Hmisc", "dplyr", "reshape2", "tidyr")) %dopar% {
  demographic_lean_sim <- demographic_leans %>%
    mutate(lean = rnorm(n(), mean = avg_lean, sd = lean_se)) %>%
    dplyr::select(candidate, demographic_var, demographic, lean)
  
  # This simulation's national percentages
  this_sim_state_percentage <- in_state_percentages_adj %>%
    filter(sim_id == i)
  
  # Apply simulated leans to state average
  state_demographic_sim <- demoCandidateBreakdown("Missouri", this_sim_state_percentage, demographic_lean_sim, 
                                                  state_marginal_demographics)
  
  # Turn these into probabilities
  gender_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("men", "women")) %>%
    dplyr::select(candidate, gender = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(gender = case_when(gender == "men" ~ "Male",
                              gender == "women" ~ "Female"))
  
  names(gender_breakdown)[-1] <- paste0("gender_", names(gender_breakdown))[-1]
  
  race_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("white", "black", "latino")) %>%
    dplyr::select(candidate, race = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(race = case_when(race == "white" ~ "White",
                            race == "black" ~ "Black",
                            race == "latino" ~ "Latino"))
  
  names(race_breakdown)[-1] <- paste0("race_", names(race_breakdown))[-1]
  
  age_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("age_1829", "age_3044", "age_4564", "age_65")) %>%
    dplyr::select(candidate, age = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(age = case_when(age == "age_1829" ~ "18-29",
                           age == "age_3044" ~ "30-44",
                           age == "age_4564" ~ "45-64",
                           age == "age_65" ~ "65+"))
  
  names(age_breakdown)[-1] <- paste0("age_", names(age_breakdown)[-1])
  
  education_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("college", "no_college")) %>%
    dplyr::select(candidate, education = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(education = case_when(education == "college" ~ "College",
                                 education == "no_college" ~ "Less than college"))
  
  names(education_breakdown)[-1] <- paste0("education_", names(education_breakdown)[-1])
  
  synth_pop <- sim_demographics %>%
    mutate(rand = runif(n()),
           age_regroup = case_when(age == "18-24" ~ "18-29",
                                   age == "25-34" & rand < 0.5 ~ "18-29",
                                   age == "25-34" & rand >= 0.5 ~ "30-44",
                                   age == "35-44" ~ "30-44",
                                   age == "45-64" ~ "45-64",
                                   age == "65+" ~ "65+"),
           id = 1:n()) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age = age_regroup, education) %>%
    mutate_if(is.factor, as.character) %>%
    left_join(gender_breakdown, by = "gender") %>%
    left_join(race_breakdown, by = "race") %>%
    left_join(age_breakdown, by = "age") %>%
    left_join(education_breakdown, by = "education") %>%
    mutate(biden_prob = gender_biden + race_biden + age_biden + education_biden,
           bloomberg_prob = gender_bloomberg + race_bloomberg + age_bloomberg + education_bloomberg,
           buttigieg_prob = gender_buttigieg + race_buttigieg + age_buttigieg + education_buttigieg,
           klobuchar_prob = gender_klobuchar + race_klobuchar + age_klobuchar + education_klobuchar,
           sanders_prob = gender_sanders + race_sanders + age_sanders + education_sanders,
           steyer_prob = gender_steyer + race_steyer + age_steyer + education_steyer,
           warren_prob = gender_warren + race_warren + age_warren + education_warren,
           yang_prob = gender_yang + race_yang + age_yang + education_yang) %>%
    mutate_at(vars(c("biden_prob", "bloomberg_prob", "buttigieg_prob", "klobuchar_prob", "sanders_prob", "steyer_prob", "warren_prob", 
                     "yang_prob")),
              function(x) pmax(x/100, 0)) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age, education, biden_prob, 
                  bloomberg_prob, buttigieg_prob, klobuchar_prob, sanders_prob, steyer_prob, warren_prob, yang_prob) %>%
    melt(id.vars = c("id", "state", "district", "primary_type", "district_delegates", "gender", "race", "age", "education"),
         variable.name = "candidate_prob", value.name = "prob") %>%
    arrange(id, candidate_prob) %>%
    group_by(id) %>%
    mutate(prob = prob / sum(prob),
           cum_prob = cumsum(prob)) %>%
    dplyr::select(-prob) %>%
    ungroup() %>%
    spread(candidate_prob, cum_prob) %>%
    mutate(rand = runif(n()),
           vote = case_when(rand <= biden_prob ~ "biden",
                            rand > biden_prob & rand <= bloomberg_prob ~ "bloomberg",
                            rand > bloomberg_prob & rand <= buttigieg_prob ~ "buttigieg",
                            rand > buttigieg_prob & rand <= klobuchar_prob ~ "klobuchar",
                            rand > klobuchar_prob & rand <= sanders_prob ~ "sanders",
                            rand > sanders_prob & rand <= steyer_prob ~ "steyer",
                            rand > steyer_prob & rand <= warren_prob ~ "warren",
                            rand > warren_prob ~ "yang")) %>%
    as.tbl()
  
  synth_district_pop <- synth_pop %>%
    group_by(state, district) %>%
    summarise(district_pop = n())
  
  district_sim_delegates <- synth_pop %>%
    group_by(state, district) %>%
    mutate(district_total = n()) %>%
    group_by(state, district, vote) %>%
    summarise(pct = n() / mean(district_total)) %>%
    ungroup() %>%
    mutate(met_threshold = pct >= 0.15) %>% 
    group_by(state, district) %>%
    mutate(pct_valid = (pct*met_threshold) / sum(pct*met_threshold)) %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(candidate_delegates = delegates * pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, district, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate = vote, pct, pct_valid, candidate_delegates) %>%
    arrange(district, candidate)
  
  state_sim_delegates <- district_sim_delegates %>%
    left_join(synth_district_pop, by = c("state", "district")) %>%
    mutate(synth_votes = pct * district_pop) %>%
    group_by(state) %>%
    mutate(state_vote = sum(synth_votes)) %>%
    group_by(state, candidate) %>%
    summarise(state_pct = sum(synth_votes) / mean(state_vote)) %>%
    mutate(district = "At-large") %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(met_threshold = state_pct >= 0.15,
           state_pct_valid = (state_pct*met_threshold) / sum(state_pct*met_threshold),
           candidate_delegates = delegates * state_pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate, pct = state_pct, pct_valid = state_pct_valid, candidate_delegates) %>%
    arrange(district, candidate) %>%
    bind_rows(district_sim_delegates) %>%
    mutate(sim_id = i)
}

indiana_winner <- in_sims %>%
  sapply(function(df) {
    df %>% filter(district == "At-large") %>% filter(pct == max(pct)) %>% pull(candidate)
  })

in_stayin_matrix <- acela_stayin_matrix
in_stayin_matrix[, "buttigieg"] <- in_stayin_matrix[, "buttigieg"] & (indiana_winner == "buttigieg")

in_second_choice_division <- in_stayin_matrix %>%
  as.data.frame() %>%
  melt(id.vars = NULL, variable.name = "candidate", value.name = "stay_in") %>%
  mutate(candidate = as.character(candidate),
         sim_id = rep(1:n_sims, 8)) %>%
  as.tbl() %>%
  left_join(second_choice, by = c("candidate" = "first_choice"))


# Nebraska (12 May 2020) ####
bounce_factor <- tibble(sim_id = 1:n_sims, bounce_factor = rbeta(n_sims, shape1 = 5, shape2 = 4))

overperformance_bounce <- acela_overperformance %>%
  left_join(bounce_factor, by = "sim_id") %>%
  mutate(bounce = overperformance * bounce_factor)

ne_start_time <- Sys.time()
# Draw votes
sim_demographics_table <- district_demographics %>%
  filter(state == "Nebraska") %>%
  mutate(prob = predict(dem_primary_logit, newdata = ., type = "response"),
         n_vote = round(pop * prob * turnout * 0.05)) %>%
  dplyr::select(-pop, -prob)

sim_demographics <- sim_demographics_table %>%
  lapply(rep, sim_demographics_table$n_vote) %>%
  as.data.frame() 
state_marginal_demographics <- shapeStateDemo("Nebraska")

# Have no Nebraska polls; use nearest neighbors
ne_averages_over_time <- state_averages_over_time_unsmoothed %>%
  filter(state %in% c("Missouri", "Wisconsin", "Iowa"), as.numeric(today() - median_date) <= 1) %>%
  mutate(age = as.numeric(today() - median_date),
         nreps = 2 - age,
         inv_distance = case_when(state == "Missouri" ~ 0.487,
                                  state == "Wisconsin" ~ 0.463,
                                  state == "Iowa" ~ 0.563)) %>%
  group_by(candidate, median_date, age, nreps) %>%
  summarise(pct = wtd.mean(pct, inv_distance))

ne_dirichlet_params <- ((ne_averages_over_time %>%
                           lapply(rep, ne_averages_over_time$nreps) %>%
                           as.data.frame() %>%
                           dplyr::select(candidate, median_date, pct) %>%
                           na.omit() %>%
                           dplyr::select(-median_date) %>%
                           group_by(candidate) %>%
                           mutate(i = 1:n()) %>%
                           spread(candidate, pct) %>%
                           dplyr::select(-i) %>%
                           as.matrix() %>%
                           diri.est())$param)

ne_dirichlet_params <- 0.5 * ne_dirichlet_params / max(1, min(ne_dirichlet_params))
ne_dirichlet_params[8] <- 0

ne_state_percentages <- (rdirichlet(n_sims, ne_dirichlet_params) %>%
                            as.data.frame() %>%
                            dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, 
                                          steyer = V6, warren = V7, yang = V8)) %>%
  apply(MARGIN = 1, FUN = function(x) x / sum(x)) %>%
  t() %>%
  as.data.frame() %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

ne_state_percentages_adj <- ne_state_percentages %>%
  left_join(overperformance_bounce, by = c("sim_id", "candidate")) %>%
  mutate(state_pct = pmax(state_pct + bounce, 0)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct)) %>%
  left_join(in_second_choice_division, by = c("candidate", "sim_id")) %>%
  melt(id.vars = c("sim_id", "candidate", "state_pct", "overperformance", "bounce_factor", "stay_in", "bounce"), 
       variable.name = "second_choice", value.name = "pct") %>%
  mutate(pct = pct*state_pct*(!stay_in),
         state_pct = case_when(candidate == second_choice ~ state_pct,
                               candidate != second_choice ~ pct)) %>%
  arrange(sim_id, second_choice, candidate)

overperformance_stats <- ne_state_percentages_adj %>%
  group_by(sim_id, candidate) %>%
  summarise(overperformance = mean(overperformance),
            bounce_factor = mean(bounce_factor))

ne_state_percentages_adj <- ne_state_percentages_adj %>%
  group_by(sim_id, second_choice) %>%
  summarise(state_pct = sum(state_pct)) %>%
  dplyr::select(everything(), candidate = second_choice) %>%
  left_join(in_stayin_matrix %>% melt(id.vars = NULL, value.name = "stay_in") %>%
              dplyr::select(sim_id = Var1, candidate = Var2, stay_in), by = c("sim_id", "candidate")) %>%
  mutate(state_pct = state_pct * stay_in) %>%
  left_join(overperformance_stats, by = c("sim_id", "candidate"))

ne_sims <- vector("list", n_sims)

ne_sims <- foreach(i = 1:n_sims, .packages = c("Hmisc", "dplyr", "reshape2", "tidyr")) %dopar% {
  demographic_lean_sim <- demographic_leans %>%
    mutate(lean = rnorm(n(), mean = avg_lean, sd = lean_se)) %>%
    dplyr::select(candidate, demographic_var, demographic, lean)
  
  # This simulation's national percentages
  this_sim_state_percentage <- ne_state_percentages_adj %>%
    filter(sim_id == i)
  
  # Apply simulated leans to state average
  state_demographic_sim <- demoCandidateBreakdown("Missouri", this_sim_state_percentage, demographic_lean_sim, 
                                                  state_marginal_demographics)
  
  # Turn these into probabilities
  gender_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("men", "women")) %>%
    dplyr::select(candidate, gender = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(gender = case_when(gender == "men" ~ "Male",
                              gender == "women" ~ "Female"))
  
  names(gender_breakdown)[-1] <- paste0("gender_", names(gender_breakdown))[-1]
  
  race_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("white", "black", "latino")) %>%
    dplyr::select(candidate, race = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(race = case_when(race == "white" ~ "White",
                            race == "black" ~ "Black",
                            race == "latino" ~ "Latino"))
  
  names(race_breakdown)[-1] <- paste0("race_", names(race_breakdown))[-1]
  
  age_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("age_1829", "age_3044", "age_4564", "age_65")) %>%
    dplyr::select(candidate, age = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(age = case_when(age == "age_1829" ~ "18-29",
                           age == "age_3044" ~ "30-44",
                           age == "age_4564" ~ "45-64",
                           age == "age_65" ~ "65+"))
  
  names(age_breakdown)[-1] <- paste0("age_", names(age_breakdown)[-1])
  
  education_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("college", "no_college")) %>%
    dplyr::select(candidate, education = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(education = case_when(education == "college" ~ "College",
                                 education == "no_college" ~ "Less than college"))
  
  names(education_breakdown)[-1] <- paste0("education_", names(education_breakdown)[-1])
  
  synth_pop <- sim_demographics %>%
    mutate(rand = runif(n()),
           age_regroup = case_when(age == "18-24" ~ "18-29",
                                   age == "25-34" & rand < 0.5 ~ "18-29",
                                   age == "25-34" & rand >= 0.5 ~ "30-44",
                                   age == "35-44" ~ "30-44",
                                   age == "45-64" ~ "45-64",
                                   age == "65+" ~ "65+"),
           id = 1:n()) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age = age_regroup, education) %>%
    mutate_if(is.factor, as.character) %>%
    left_join(gender_breakdown, by = "gender") %>%
    left_join(race_breakdown, by = "race") %>%
    left_join(age_breakdown, by = "age") %>%
    left_join(education_breakdown, by = "education") %>%
    mutate(biden_prob = gender_biden + race_biden + age_biden + education_biden,
           bloomberg_prob = gender_bloomberg + race_bloomberg + age_bloomberg + education_bloomberg,
           buttigieg_prob = gender_buttigieg + race_buttigieg + age_buttigieg + education_buttigieg,
           klobuchar_prob = gender_klobuchar + race_klobuchar + age_klobuchar + education_klobuchar,
           sanders_prob = gender_sanders + race_sanders + age_sanders + education_sanders,
           steyer_prob = gender_steyer + race_steyer + age_steyer + education_steyer,
           warren_prob = gender_warren + race_warren + age_warren + education_warren,
           yang_prob = gender_yang + race_yang + age_yang + education_yang) %>%
    mutate_at(vars(c("biden_prob", "bloomberg_prob", "buttigieg_prob", "klobuchar_prob", "sanders_prob", "steyer_prob", "warren_prob", 
                     "yang_prob")), function(x) pmax(x/100, 0)) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age, education, biden_prob, 
                  bloomberg_prob, buttigieg_prob, klobuchar_prob, sanders_prob, steyer_prob, warren_prob, yang_prob) %>%
    melt(id.vars = c("id", "state", "district", "primary_type", "district_delegates", "gender", "race", "age", "education"),
         variable.name = "candidate_prob", value.name = "prob") %>%
    arrange(id, candidate_prob) %>%
    group_by(id) %>%
    mutate(prob = prob / sum(prob),
           cum_prob = cumsum(prob)) %>%
    dplyr::select(-prob) %>%
    ungroup() %>%
    spread(candidate_prob, cum_prob) %>%
    mutate(rand = runif(n()),
           vote = case_when(rand <= biden_prob ~ "biden",
                            rand > biden_prob & rand <= bloomberg_prob ~ "bloomberg",
                            rand > bloomberg_prob & rand <= buttigieg_prob ~ "buttigieg",
                            rand > buttigieg_prob & rand <= klobuchar_prob ~ "klobuchar",
                            rand > klobuchar_prob & rand <= sanders_prob ~ "sanders",
                            rand > sanders_prob & rand <= steyer_prob ~ "steyer",
                            rand > steyer_prob & rand <= warren_prob ~ "warren",
                            rand > warren_prob ~ "yang")) %>%
    as.tbl()
  
  
  synth_district_pop <- synth_pop %>%
    group_by(state, district) %>%
    summarise(district_pop = n())
  
  district_sim_delegates <- synth_pop %>%
    group_by(state, district) %>%
    mutate(district_total = n()) %>%
    group_by(state, district, vote) %>%
    summarise(pct = n() / mean(district_total)) %>%
    ungroup() %>%
    mutate(met_threshold = pct >= 0.15) %>% 
    group_by(state, district) %>%
    mutate(pct_valid = (pct*met_threshold) / sum(pct*met_threshold)) %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(candidate_delegates = delegates * pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, district, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate = vote, pct, pct_valid, candidate_delegates) %>%
    arrange(district, candidate)
  
  state_sim_delegates <- district_sim_delegates %>%
    left_join(synth_district_pop, by = c("state", "district")) %>%
    mutate(synth_votes = pct * district_pop) %>%
    group_by(state) %>%
    mutate(state_vote = sum(synth_votes)) %>%
    group_by(state, candidate) %>%
    summarise(state_pct = sum(synth_votes) / mean(state_vote)) %>%
    mutate(district = "At-large") %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(met_threshold = state_pct >= 0.15,
           state_pct_valid = (state_pct*met_threshold) / sum(state_pct*met_threshold),
           candidate_delegates = delegates * state_pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate, pct = state_pct, pct_valid = state_pct_valid, candidate_delegates) %>%
    arrange(district, candidate) %>%
    bind_rows(district_sim_delegates) %>%
    mutate(sim_id = i)
}

# West Virginia (12 May 2020) ####
wv_start_time <- Sys.time()
# Draw votes
sim_demographics_table <- district_demographics %>%
  filter(state == "West Virginia") %>%
  mutate(prob = predict(dem_primary_logit, newdata = ., type = "response"),
         n_vote = round(pop * prob * turnout * 0.05)) %>%
  dplyr::select(-pop, -prob)

sim_demographics <- sim_demographics_table %>%
  lapply(rep, sim_demographics_table$n_vote) %>%
  as.data.frame() 
state_marginal_demographics <- shapeStateDemo("West Virginia")

# Have no West Virginia polls; use nearest neighbors
wv_averages_over_time <- state_averages_over_time_unsmoothed %>%
  filter(state %in% c("Ohio", "Missouri"), as.numeric(today() - median_date) <= 1) %>%
  mutate(age = as.numeric(today() - median_date),
         nreps = 2 - age,
         inv_distance = case_when(state == "Missouri" ~ 0.304,
                                  state == "Ohio" ~ 0.315)) %>%
  group_by(candidate, median_date, age, nreps) %>%
  summarise(pct = wtd.mean(pct, inv_distance))

wv_dirichlet_params <- ((wv_averages_over_time %>%
                           lapply(rep, wv_averages_over_time$nreps) %>%
                           as.data.frame() %>%
                           dplyr::select(candidate, median_date, pct) %>%
                           na.omit() %>%
                           dplyr::select(-median_date) %>%
                           group_by(candidate) %>%
                           mutate(i = 1:n()) %>%
                           spread(candidate, pct) %>%
                           dplyr::select(-i) %>%
                           as.matrix() %>%
                           diri.est())$param)

wv_dirichlet_params <- 0.5 * wv_dirichlet_params / max(1, min(wv_dirichlet_params))
wv_dirichlet_params[8] <- 0

wv_state_percentages <- (rdirichlet(n_sims, wv_dirichlet_params) %>%
                            as.data.frame() %>%
                            dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, 
                                          steyer = V6, warren = V7, yang = V8)) %>%
  apply(MARGIN = 1, FUN = function(x) x / sum(x)) %>%
  t() %>%
  as.data.frame() %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

wv_state_percentages_adj <- wv_state_percentages %>%
  left_join(overperformance_bounce, by = c("sim_id", "candidate")) %>%
  mutate(state_pct = pmax(state_pct + bounce, 0)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct)) %>%
  left_join(in_second_choice_division, by = c("candidate", "sim_id")) %>%
  melt(id.vars = c("sim_id", "candidate", "state_pct", "overperformance", "bounce_factor", "stay_in", "bounce"), 
       variable.name = "second_choice", value.name = "pct") %>%
  mutate(pct = pct*state_pct*(!stay_in),
         state_pct = case_when(candidate == second_choice ~ state_pct,
                               candidate != second_choice ~ pct)) %>%
  arrange(sim_id, second_choice, candidate)

overperformance_stats <- wv_state_percentages_adj %>%
  group_by(sim_id, candidate) %>%
  summarise(overperformance = mean(overperformance),
            bounce_factor = mean(bounce_factor))

wv_state_percentages_adj <- wv_state_percentages_adj %>%
  group_by(sim_id, second_choice) %>%
  summarise(state_pct = sum(state_pct)) %>%
  dplyr::select(everything(), candidate = second_choice) %>%
  left_join(in_stayin_matrix %>% melt(id.vars = NULL, value.name = "stay_in") %>%
              dplyr::select(sim_id = Var1, candidate = Var2, stay_in), by = c("sim_id", "candidate")) %>%
  mutate(state_pct = state_pct * stay_in) %>%
  left_join(overperformance_stats, by = c("sim_id", "candidate"))

wv_sims <- vector("list", n_sims)

wv_sims <- foreach(i = 1:n_sims, .packages = c("Hmisc", "dplyr", "reshape2", "tidyr")) %dopar% {
  demographic_lean_sim <- demographic_leans %>%
    mutate(lean = rnorm(n(), mean = avg_lean, sd = lean_se)) %>%
    dplyr::select(candidate, demographic_var, demographic, lean)
  
  # This simulation's national percentages
  this_sim_state_percentage <- wv_state_percentages_adj %>%
    filter(sim_id == i)
  
  # Apply simulated leans to state average
  state_demographic_sim <- demoCandidateBreakdown("Maine", this_sim_state_percentage, demographic_lean_sim, 
                                                  state_marginal_demographics)
  
  # Turn these into probabilities
  gender_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("men", "women")) %>%
    dplyr::select(candidate, gender = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(gender = case_when(gender == "men" ~ "Male",
                              gender == "women" ~ "Female"))
  
  names(gender_breakdown)[-1] <- paste0("gender_", names(gender_breakdown))[-1]
  
  race_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("white", "black", "latino")) %>%
    dplyr::select(candidate, race = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(race = case_when(race == "white" ~ "White",
                            race == "black" ~ "Black",
                            race == "latino" ~ "Latino"))
  
  names(race_breakdown)[-1] <- paste0("race_", names(race_breakdown))[-1]
  
  age_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("age_1829", "age_3044", "age_4564", "age_65")) %>%
    dplyr::select(candidate, age = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(age = case_when(age == "age_1829" ~ "18-29",
                           age == "age_3044" ~ "30-44",
                           age == "age_4564" ~ "45-64",
                           age == "age_65" ~ "65+"))
  
  names(age_breakdown)[-1] <- paste0("age_", names(age_breakdown)[-1])
  
  education_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("college", "no_college")) %>%
    dplyr::select(candidate, education = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(education = case_when(education == "college" ~ "College",
                                 education == "no_college" ~ "Less than college"))
  
  names(education_breakdown)[-1] <- paste0("education_", names(education_breakdown)[-1])
  
  synth_pop <- sim_demographics %>%
    mutate(rand = runif(n()),
           age_regroup = case_when(age == "18-24" ~ "18-29",
                                   age == "25-34" & rand < 0.5 ~ "18-29",
                                   age == "25-34" & rand >= 0.5 ~ "30-44",
                                   age == "35-44" ~ "30-44",
                                   age == "45-64" ~ "45-64",
                                   age == "65+" ~ "65+"),
           id = 1:n()) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age = age_regroup, education) %>%
    mutate_if(is.factor, as.character) %>%
    left_join(gender_breakdown, by = "gender") %>%
    left_join(race_breakdown, by = "race") %>%
    left_join(age_breakdown, by = "age") %>%
    left_join(education_breakdown, by = "education") %>%
    mutate(biden_prob = gender_biden + race_biden + age_biden + education_biden,
           bloomberg_prob = gender_bloomberg + race_bloomberg + age_bloomberg + education_bloomberg,
           buttigieg_prob = gender_buttigieg + race_buttigieg + age_buttigieg + education_buttigieg,
           klobuchar_prob = gender_klobuchar + race_klobuchar + age_klobuchar + education_klobuchar,
           sanders_prob = gender_sanders + race_sanders + age_sanders + education_sanders,
           steyer_prob = gender_steyer + race_steyer + age_steyer + education_steyer,
           warren_prob = gender_warren + race_warren + age_warren + education_warren,
           yang_prob = gender_yang + race_yang + age_yang + education_yang) %>%
    mutate_at(vars(c("biden_prob", "bloomberg_prob", "buttigieg_prob", "klobuchar_prob", "sanders_prob", "steyer_prob", "warren_prob", 
                     "yang_prob")), function(x) pmax(x/100, 0)) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age, education, biden_prob, 
                  bloomberg_prob, buttigieg_prob, klobuchar_prob, sanders_prob, steyer_prob, warren_prob, yang_prob) %>%
    melt(id.vars = c("id", "state", "district", "primary_type", "district_delegates", "gender", "race", "age", "education"),
         variable.name = "candidate_prob", value.name = "prob") %>%
    arrange(id, candidate_prob) %>%
    group_by(id) %>%
    mutate(prob = prob / sum(prob),
           cum_prob = cumsum(prob)) %>%
    dplyr::select(-prob) %>%
    ungroup() %>%
    spread(candidate_prob, cum_prob) %>%
    mutate(rand = runif(n()),
           vote = case_when(rand <= biden_prob ~ "biden",
                            rand > biden_prob & rand <= bloomberg_prob ~ "bloomberg",
                            rand > bloomberg_prob & rand <= buttigieg_prob ~ "buttigieg",
                            rand > buttigieg_prob & rand <= klobuchar_prob ~ "klobuchar",
                            rand > klobuchar_prob & rand <= sanders_prob ~ "sanders",
                            rand > sanders_prob & rand <= steyer_prob ~ "steyer",
                            rand > steyer_prob & rand <= warren_prob ~ "warren",
                            rand > warren_prob ~ "yang")) %>%
    as.tbl()
  
  
  synth_district_pop <- synth_pop %>%
    group_by(state, district) %>%
    summarise(district_pop = n())
  
  district_sim_delegates <- synth_pop %>%
    group_by(state, district) %>%
    mutate(district_total = n()) %>%
    group_by(state, district, vote) %>%
    summarise(pct = n() / mean(district_total)) %>%
    ungroup() %>%
    mutate(met_threshold = pct >= 0.15) %>% 
    group_by(state, district) %>%
    mutate(pct_valid = (pct*met_threshold) / sum(pct*met_threshold)) %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(candidate_delegates = delegates * pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, district, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate = vote, pct, pct_valid, candidate_delegates) %>%
    arrange(district, candidate)
  
  state_sim_delegates <- district_sim_delegates %>%
    left_join(synth_district_pop, by = c("state", "district")) %>%
    mutate(synth_votes = pct * district_pop) %>%
    group_by(state) %>%
    mutate(state_vote = sum(synth_votes)) %>%
    group_by(state, candidate) %>%
    summarise(state_pct = sum(synth_votes) / mean(state_vote)) %>%
    mutate(district = "At-large") %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(met_threshold = state_pct >= 0.15,
           state_pct_valid = (state_pct*met_threshold) / sum(state_pct*met_threshold),
           candidate_delegates = delegates * state_pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate, pct = state_pct, pct_valid = state_pct_valid, candidate_delegates) %>%
    arrange(district, candidate) %>%
    bind_rows(district_sim_delegates) %>%
    mutate(sim_id = i)
}

# Kentucky (19 May 2020) ####
bounce_factor <- tibble(sim_id = 1:n_sims, bounce_factor = rbeta(n_sims, shape1 = 3, shape2 = 4))

overperformance_bounce <- acela_overperformance %>%
  left_join(bounce_factor, by = "sim_id") %>%
  mutate(bounce = overperformance * bounce_factor)

ky_start_time <- Sys.time()
# Draw votes
sim_demographics_table <- district_demographics %>%
  filter(state == "Kentucky") %>%
  mutate(prob = predict(dem_primary_logit, newdata = ., type = "response"),
         n_vote = round(pop * prob * turnout * 0.05)) %>%
  dplyr::select(-pop, -prob)

sim_demographics <- sim_demographics_table %>%
  lapply(rep, sim_demographics_table$n_vote) %>%
  as.data.frame() 
state_marginal_demographics <- shapeStateDemo("Kentucky")

# Have no Kentucky polls; use nearest neighbors
ky_averages_over_time <- state_averages_over_time_unsmoothed %>%
  filter(state %in% c("Tennessee", "Ohio", "Missouri"), as.numeric(today() - median_date) <= 1) %>%
  mutate(age = as.numeric(today() - median_date),
         nreps = 2 - age,
         inv_distance = case_when(state == "Tennessee" ~ 0.659,
                                  state == "Missouri" ~ 0.488,
                                  state == "Ohio" ~ 0.436)) %>%
  group_by(candidate, median_date, age, nreps) %>%
  summarise(pct = wtd.mean(pct, inv_distance))

ky_dirichlet_params <- ((ky_averages_over_time %>%
                           lapply(rep, ky_averages_over_time$nreps) %>%
                           as.data.frame() %>%
                           dplyr::select(candidate, median_date, pct) %>%
                           na.omit() %>%
                           dplyr::select(-median_date) %>%
                           group_by(candidate) %>%
                           mutate(i = 1:n()) %>%
                           spread(candidate, pct) %>%
                           dplyr::select(-i) %>%
                           as.matrix() %>%
                           diri.est())$param)

ky_dirichlet_params <- 0.5 * ky_dirichlet_params / max(1, min(ky_dirichlet_params))
ky_dirichlet_params[8] <- 0

ky_state_percentages <- (rdirichlet(n_sims, ky_dirichlet_params) %>%
                            as.data.frame() %>%
                            dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, 
                                          steyer = V6, warren = V7, yang = V8)) %>%
  apply(MARGIN = 1, FUN = function(x) x / sum(x)) %>%
  t() %>%
  as.data.frame() %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

ky_state_percentages_adj <- ky_state_percentages %>%
  left_join(overperformance_bounce, by = c("sim_id", "candidate")) %>%
  mutate(state_pct = pmax(state_pct + bounce, 0)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct)) %>%
  left_join(in_second_choice_division, by = c("candidate", "sim_id")) %>%
  melt(id.vars = c("sim_id", "candidate", "state_pct", "overperformance", "bounce_factor", "stay_in", "bounce"), 
       variable.name = "second_choice", value.name = "pct") %>%
  mutate(pct = pct*state_pct*(!stay_in),
         state_pct = case_when(candidate == second_choice ~ state_pct,
                               candidate != second_choice ~ pct)) %>%
  arrange(sim_id, second_choice, candidate)

overperformance_stats <- ky_state_percentages_adj %>%
  group_by(sim_id, candidate) %>%
  summarise(overperformance = mean(overperformance),
            bounce_factor = mean(bounce_factor))

ky_state_percentages_adj <- ky_state_percentages_adj %>%
  group_by(sim_id, second_choice) %>%
  summarise(state_pct = sum(state_pct)) %>%
  dplyr::select(everything(), candidate = second_choice) %>%
  left_join(in_stayin_matrix %>% melt(id.vars = NULL, value.name = "stay_in") %>%
              dplyr::select(sim_id = Var1, candidate = Var2, stay_in), by = c("sim_id", "candidate")) %>%
  mutate(state_pct = state_pct * stay_in) %>%
  left_join(overperformance_stats, by = c("sim_id", "candidate"))

ky_sims <- vector("list", n_sims)

ky_sims <- foreach(i = 1:n_sims, .packages = c("Hmisc", "dplyr", "reshape2", "tidyr")) %dopar% {
  demographic_lean_sim <- demographic_leans %>%
    mutate(lean = rnorm(n(), mean = avg_lean, sd = lean_se)) %>%
    dplyr::select(candidate, demographic_var, demographic, lean)
  
  # This simulation's national percentages
  this_sim_state_percentage <- ky_state_percentages_adj %>%
    filter(sim_id == i)
  
  # Apply simulated leans to state average
  state_demographic_sim <- demoCandidateBreakdown("Tennessee", this_sim_state_percentage, demographic_lean_sim, 
                                                  state_marginal_demographics)
  
  # Turn these into probabilities
  gender_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("men", "women")) %>%
    dplyr::select(candidate, gender = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(gender = case_when(gender == "men" ~ "Male",
                              gender == "women" ~ "Female"))
  
  names(gender_breakdown)[-1] <- paste0("gender_", names(gender_breakdown))[-1]
  
  race_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("white", "black", "latino")) %>%
    dplyr::select(candidate, race = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(race = case_when(race == "white" ~ "White",
                            race == "black" ~ "Black",
                            race == "latino" ~ "Latino"))
  
  names(race_breakdown)[-1] <- paste0("race_", names(race_breakdown))[-1]
  
  age_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("age_1829", "age_3044", "age_4564", "age_65")) %>%
    dplyr::select(candidate, age = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(age = case_when(age == "age_1829" ~ "18-29",
                           age == "age_3044" ~ "30-44",
                           age == "age_4564" ~ "45-64",
                           age == "age_65" ~ "65+"))
  
  names(age_breakdown)[-1] <- paste0("age_", names(age_breakdown)[-1])
  
  education_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("college", "no_college")) %>%
    dplyr::select(candidate, education = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(education = case_when(education == "college" ~ "College",
                                 education == "no_college" ~ "Less than college"))
  
  names(education_breakdown)[-1] <- paste0("education_", names(education_breakdown)[-1])
  
  synth_pop <- sim_demographics %>%
    mutate(rand = runif(n()),
           age_regroup = case_when(age == "18-24" ~ "18-29",
                                   age == "25-34" & rand < 0.5 ~ "18-29",
                                   age == "25-34" & rand >= 0.5 ~ "30-44",
                                   age == "35-44" ~ "30-44",
                                   age == "45-64" ~ "45-64",
                                   age == "65+" ~ "65+"),
           id = 1:n()) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age = age_regroup, education) %>%
    mutate_if(is.factor, as.character) %>%
    left_join(gender_breakdown, by = "gender") %>%
    left_join(race_breakdown, by = "race") %>%
    left_join(age_breakdown, by = "age") %>%
    left_join(education_breakdown, by = "education") %>%
    mutate(biden_prob = gender_biden + race_biden + age_biden + education_biden,
           bloomberg_prob = gender_bloomberg + race_bloomberg + age_bloomberg + education_bloomberg,
           buttigieg_prob = gender_buttigieg + race_buttigieg + age_buttigieg + education_buttigieg,
           klobuchar_prob = gender_klobuchar + race_klobuchar + age_klobuchar + education_klobuchar,
           sanders_prob = gender_sanders + race_sanders + age_sanders + education_sanders,
           steyer_prob = gender_steyer + race_steyer + age_steyer + education_steyer,
           warren_prob = gender_warren + race_warren + age_warren + education_warren,
           yang_prob = gender_yang + race_yang + age_yang + education_yang) %>%
    mutate_at(vars(c("biden_prob", "bloomberg_prob", "buttigieg_prob", "klobuchar_prob", "sanders_prob", "steyer_prob", "warren_prob", 
                     "yang_prob")), function(x) pmax(x/100, 0)) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age, education, biden_prob, 
                  bloomberg_prob, buttigieg_prob, klobuchar_prob, sanders_prob, steyer_prob, warren_prob, yang_prob) %>%
    melt(id.vars = c("id", "state", "district", "primary_type", "district_delegates", "gender", "race", "age", "education"),
         variable.name = "candidate_prob", value.name = "prob") %>%
    arrange(id, candidate_prob) %>%
    group_by(id) %>%
    mutate(prob = prob / sum(prob),
           cum_prob = cumsum(prob)) %>%
    dplyr::select(-prob) %>%
    ungroup() %>%
    spread(candidate_prob, cum_prob) %>%
    mutate(rand = runif(n()),
           vote = case_when(rand <= biden_prob ~ "biden",
                            rand > biden_prob & rand <= bloomberg_prob ~ "bloomberg",
                            rand > bloomberg_prob & rand <= buttigieg_prob ~ "buttigieg",
                            rand > buttigieg_prob & rand <= klobuchar_prob ~ "klobuchar",
                            rand > klobuchar_prob & rand <= sanders_prob ~ "sanders",
                            rand > sanders_prob & rand <= steyer_prob ~ "steyer",
                            rand > steyer_prob & rand <= warren_prob ~ "warren",
                            rand > warren_prob ~ "yang")) %>%
    as.tbl()
  
  
  synth_district_pop <- synth_pop %>%
    group_by(state, district) %>%
    summarise(district_pop = n())
  
  district_sim_delegates <- synth_pop %>%
    group_by(state, district) %>%
    mutate(district_total = n()) %>%
    group_by(state, district, vote) %>%
    summarise(pct = n() / mean(district_total)) %>%
    ungroup() %>%
    mutate(met_threshold = pct >= 0.15) %>% 
    group_by(state, district) %>%
    mutate(pct_valid = (pct*met_threshold) / sum(pct*met_threshold)) %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(candidate_delegates = delegates * pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, district, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate = vote, pct, pct_valid, candidate_delegates) %>%
    arrange(district, candidate)
  
  state_sim_delegates <- district_sim_delegates %>%
    left_join(synth_district_pop, by = c("state", "district")) %>%
    mutate(synth_votes = pct * district_pop) %>%
    group_by(state) %>%
    mutate(state_vote = sum(synth_votes)) %>%
    group_by(state, candidate) %>%
    summarise(state_pct = sum(synth_votes) / mean(state_vote)) %>%
    mutate(district = "At-large") %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(met_threshold = state_pct >= 0.15,
           state_pct_valid = (state_pct*met_threshold) / sum(state_pct*met_threshold),
           candidate_delegates = delegates * state_pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate, pct = state_pct, pct_valid = state_pct_valid, candidate_delegates) %>%
    arrange(district, candidate) %>%
    bind_rows(district_sim_delegates) %>%
    mutate(sim_id = i)
}

# Oregon (19 May 2020) ####
or_start_time <- Sys.time()

sim_demographics_table <- district_demographics %>%
  filter(state == "Oregon") %>%
  mutate(prob = predict(dem_primary_logit, newdata = ., type = "response"),
         n_vote = round(pop * prob * turnout * 0.05)) %>%
  dplyr::select(-pop, -prob)

sim_demographics <- sim_demographics_table %>%
  lapply(rep, sim_demographics_table$n_vote) %>%
  as.data.frame() 
state_marginal_demographics <- shapeStateDemo("Oregon")

comiohwawi_averages_over_time <- state_averages_over_time_unsmoothed %>%
  filter(state %in% c("Michigan", "Washington", "Colorado", "Ohio", "Wisconsin"), as.numeric(today() - median_date) <= 1) %>%
  mutate(age = as.numeric(today() - median_date),
         nreps = 4 - age,
         inv_distance = case_when(state == "Michigan" ~ 0.453,
                                  state == "Colorado" ~ 0.436,
                                  state == "Ohio" ~ 0.380,
                                  state == "Washington" ~ 0.587,
                                  state == "Wisconsin" ~ 0.378)) %>%
  group_by(candidate, median_date, age, nreps) %>%
  summarise(pct = wtd.mean(pct, inv_distance)) %>%
  filter(candidate %in% c("bloomberg", "klobuchar", "steyer"))

or_averages_over_time <- state_averages_over_time_unsmoothed %>%
  filter(state == "Oregon", as.numeric(today() - median_date) <= 1) %>%
  mutate(age = as.numeric(today() - median_date),
         nreps = 4 - age)

or_dirichlet_params <- (or_averages_over_time %>%
                          lapply(rep, or_averages_over_time$nreps) %>%
                          as.data.frame() %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          dplyr::select(-median_date) %>%
                          group_by(candidate) %>%
                          mutate(i = 1:n()) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-i) %>%
                          as.matrix() %>%
                          diri.est())$param

or_dirichlet_params <- 0.5 * or_dirichlet_params / max(1, min(or_dirichlet_params))
or_dirichlet_params[8] <- 0

or_state_percentages <- (rdirichlet(n_sims, or_dirichlet_params) %>%
                            as.data.frame() %>%
                            dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, 
                                          steyer = V6, warren = V7, yang = V8)) %>%
  apply(MARGIN = 1, FUN = function(x) x / sum(x)) %>%
  t() %>%
  as.data.frame() %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

or_state_percentages_adj <- or_state_percentages %>%
  left_join(overperformance_bounce, by = c("sim_id", "candidate")) %>%
  mutate(state_pct = pmax(state_pct + bounce, 0)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct)) %>%
  left_join(in_second_choice_division, by = c("candidate", "sim_id")) %>%
  melt(id.vars = c("sim_id", "candidate", "state_pct", "overperformance", "bounce_factor", "stay_in", "bounce"), 
       variable.name = "second_choice", value.name = "pct") %>%
  mutate(pct = pct*state_pct*(!stay_in),
         state_pct = case_when(candidate == second_choice ~ state_pct,
                               candidate != second_choice ~ pct)) %>%
  arrange(sim_id, second_choice, candidate)

overperformance_stats <- or_state_percentages_adj %>%
  group_by(sim_id, candidate) %>%
  summarise(overperformance = mean(overperformance),
            bounce_factor = mean(bounce_factor))

or_state_percentages_adj <- or_state_percentages_adj %>%
  group_by(sim_id, second_choice) %>%
  summarise(state_pct = sum(state_pct)) %>%
  dplyr::select(everything(), candidate = second_choice) %>%
  left_join(in_stayin_matrix %>% melt(id.vars = NULL, value.name = "stay_in") %>%
              dplyr::select(sim_id = Var1, candidate = Var2, stay_in), by = c("sim_id", "candidate")) %>%
  mutate(state_pct = state_pct * stay_in) %>%
  left_join(overperformance_stats, by = c("sim_id", "candidate"))

or_sims <- vector("list", n_sims)

### Simulate
or_sims <- foreach(i = 1:n_sims, .packages = c("Hmisc", "dplyr", "reshape2", "tidyr")) %dopar% {
  demographic_lean_sim <- demographic_leans %>%
    mutate(lean = rnorm(n(), mean = avg_lean, sd = lean_se)) %>%
    dplyr::select(candidate, demographic_var, demographic, lean)
  
  # This simulation's national percentages
  this_sim_state_percentage <- or_state_percentages_adj %>%
    filter(sim_id == i)
  
  # Apply simulated leans to state average
  state_demographic_sim <- demoCandidateBreakdown("Oregon", this_sim_state_percentage, demographic_lean_sim, 
                                                  state_marginal_demographics)
  
  # Turn these into probabilities
  gender_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("men", "women")) %>%
    dplyr::select(candidate, gender = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(gender = case_when(gender == "men" ~ "Male",
                              gender == "women" ~ "Female"))
  
  names(gender_breakdown)[-1] <- paste0("gender_", names(gender_breakdown))[-1]
  
  race_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("white", "black", "latino")) %>%
    dplyr::select(candidate, race = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(race = case_when(race == "white" ~ "White",
                            race == "black" ~ "Black",
                            race == "latino" ~ "Latino"))
  
  names(race_breakdown)[-1] <- paste0("race_", names(race_breakdown))[-1]
  
  age_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("age_1829", "age_3044", "age_4564", "age_65")) %>%
    dplyr::select(candidate, age = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(age = case_when(age == "age_1829" ~ "18-29",
                           age == "age_3044" ~ "30-44",
                           age == "age_4564" ~ "45-64",
                           age == "age_65" ~ "65+"))
  
  names(age_breakdown)[-1] <- paste0("age_", names(age_breakdown)[-1])
  
  education_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("college", "no_college")) %>%
    dplyr::select(candidate, education = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(education = case_when(education == "college" ~ "College",
                                 education == "no_college" ~ "Less than college"))
  
  names(education_breakdown)[-1] <- paste0("education_", names(education_breakdown)[-1])
  
  synth_pop <- sim_demographics %>%
    mutate(rand = runif(n()),
           age_regroup = case_when(age == "18-24" ~ "18-29",
                                   age == "25-34" & rand < 0.5 ~ "18-29",
                                   age == "25-34" & rand >= 0.5 ~ "30-44",
                                   age == "35-44" ~ "30-44",
                                   age == "45-64" ~ "45-64",
                                   age == "65+" ~ "65+"),
           id = 1:n()) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age = age_regroup, education) %>%
    mutate_if(is.factor, as.character) %>%
    left_join(gender_breakdown, by = "gender") %>%
    left_join(race_breakdown, by = "race") %>%
    left_join(age_breakdown, by = "age") %>%
    left_join(education_breakdown, by = "education") %>%
    mutate(biden_prob = gender_biden + race_biden + age_biden + education_biden,
           bloomberg_prob = gender_bloomberg + race_bloomberg + age_bloomberg + education_bloomberg,
           buttigieg_prob = gender_buttigieg + race_buttigieg + age_buttigieg + education_buttigieg,
           klobuchar_prob = gender_klobuchar + race_klobuchar + age_klobuchar + education_klobuchar,
           sanders_prob = gender_sanders + race_sanders + age_sanders + education_sanders,
           steyer_prob = gender_steyer + race_steyer + age_steyer + education_steyer,
           warren_prob = gender_warren + race_warren + age_warren + education_warren,
           yang_prob = gender_yang + race_yang + age_yang + education_yang) %>%
    mutate_at(vars(c("biden_prob", "bloomberg_prob", "buttigieg_prob", "klobuchar_prob", "sanders_prob", "steyer_prob", "warren_prob", 
                     "yang_prob")),
              function(x) pmax(x/100, 0)) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age, education, biden_prob, 
                  bloomberg_prob, buttigieg_prob, klobuchar_prob, sanders_prob, steyer_prob, warren_prob, yang_prob) %>%
    melt(id.vars = c("id", "state", "district", "primary_type", "district_delegates", "gender", "race", "age", "education"),
         variable.name = "candidate_prob", value.name = "prob") %>%
    arrange(id, candidate_prob) %>%
    group_by(id) %>%
    mutate(prob = prob / sum(prob),
           cum_prob = cumsum(prob)) %>%
    dplyr::select(-prob) %>%
    ungroup() %>%
    spread(candidate_prob, cum_prob) %>%
    mutate(rand = runif(n()),
           vote = case_when(rand <= biden_prob ~ "biden",
                            rand > biden_prob & rand <= bloomberg_prob ~ "bloomberg",
                            rand > bloomberg_prob & rand <= buttigieg_prob ~ "buttigieg",
                            rand > buttigieg_prob & rand <= klobuchar_prob ~ "klobuchar",
                            rand > klobuchar_prob & rand <= sanders_prob ~ "sanders",
                            rand > sanders_prob & rand <= steyer_prob ~ "steyer",
                            rand > steyer_prob & rand <= warren_prob ~ "warren",
                            rand > warren_prob ~ "yang")) %>%
    as.tbl()
  
  synth_district_pop <- synth_pop %>%
    group_by(state, district) %>%
    summarise(district_pop = n())
  
  district_sim_delegates <- synth_pop %>%
    group_by(state, district) %>%
    mutate(district_total = n()) %>%
    group_by(state, district, vote) %>%
    summarise(pct = n() / mean(district_total)) %>%
    ungroup() %>%
    mutate(met_threshold = pct >= 0.15) %>% 
    group_by(state, district) %>%
    mutate(pct_valid = (pct*met_threshold) / sum(pct*met_threshold)) %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(candidate_delegates = delegates * pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, district, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate = vote, pct, pct_valid, candidate_delegates) %>%
    arrange(district, candidate)
  
  state_sim_delegates <- district_sim_delegates %>%
    left_join(synth_district_pop, by = c("state", "district")) %>%
    mutate(synth_votes = pct * district_pop) %>%
    group_by(state) %>%
    mutate(state_vote = sum(synth_votes)) %>%
    group_by(state, candidate) %>%
    summarise(state_pct = sum(synth_votes) / mean(state_vote)) %>%
    mutate(district = "At-large") %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(met_threshold = state_pct >= 0.15,
           state_pct_valid = (state_pct*met_threshold) / sum(state_pct*met_threshold),
           candidate_delegates = delegates * state_pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate, pct = state_pct, pct_valid = state_pct_valid, candidate_delegates) %>%
    arrange(district, candidate) %>%
    bind_rows(district_sim_delegates) %>%
    mutate(sim_id = i)
}

# The Final Five (2 June 2020) ####
bounce_factor <- tibble(sim_id = 1:n_sims, bounce_factor = rbeta(n_sims, shape1 = 3, shape2 = 3))

overperformance_bounce <- acela_overperformance %>%
  left_join(bounce_factor, by = "sim_id") %>%
  mutate(bounce = overperformance * bounce_factor)

# District of Columbia ####
dc_start_time <- Sys.time()

dc_averages_over_time <- state_averages_over_time_unsmoothed %>%
  filter(state %in% c("Maryland", "New York", "New Jersey", "Illinois"), as.numeric(today() - median_date) <= 1) %>%
  mutate(age = as.numeric(today() - median_date),
         nreps = 4 - age,
         inv_distance = case_when(state == "Maryland" ~ 0.100,
                                  state == "New York" ~ 0.0979,
                                  state == "New Jersey" ~ 0.0944,
                                  state == "Illinois" ~ 0.0919)) %>%
  group_by(candidate, median_date, age, nreps) %>%
  summarise(pct = wtd.mean(pct, inv_distance)) 

dc_dirichlet_params <- (dc_averages_over_time %>%
                          lapply(rep, dc_averages_over_time$nreps) %>%
                          as.data.frame() %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          dplyr::select(-median_date) %>%
                          group_by(candidate) %>%
                          mutate(i = 1:n()) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-i) %>%
                          as.matrix() %>%
                          diri.est())$param

dc_dirichlet_params <- 0.25 * dc_dirichlet_params / max(1, min(dc_dirichlet_params))
dc_dirichlet_params[8] <- 0

dc_state_percentages <- (rdirichlet(n_sims, dc_dirichlet_params) %>%
                            as.data.frame() %>%
                            dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, 
                                          steyer = V6, warren = V7, yang = V8)) %>%
  apply(MARGIN = 1, FUN = function(x) x / sum(x)) %>%
  t() %>%
  as.data.frame() %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

dc_state_percentages_adj <- dc_state_percentages %>%
  left_join(overperformance_bounce, by = c("sim_id", "candidate")) %>%
  mutate(state_pct = pmax(state_pct + bounce, 0)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct)) %>%
  left_join(in_second_choice_division, by = c("candidate", "sim_id")) %>%
  melt(id.vars = c("sim_id", "candidate", "state_pct", "overperformance", "bounce_factor", "stay_in", "bounce"), 
       variable.name = "second_choice", value.name = "pct") %>%
  mutate(pct = pct*state_pct*(!stay_in),
         state_pct = case_when(candidate == second_choice ~ state_pct,
                               candidate != second_choice ~ pct)) %>%
  arrange(sim_id, second_choice, candidate)

overperformance_stats <- dc_state_percentages_adj %>%
  group_by(sim_id, candidate) %>%
  summarise(overperformance = mean(overperformance),
            bounce_factor = mean(bounce_factor))

dc_state_percentages_adj <- dc_state_percentages_adj %>%
  group_by(sim_id, second_choice) %>%
  summarise(state_pct = sum(state_pct)) %>%
  dplyr::select(everything(), candidate = second_choice) %>%
  left_join(in_stayin_matrix %>% melt(id.vars = NULL, value.name = "stay_in") %>%
              dplyr::select(sim_id = Var1, candidate = Var2, stay_in), by = c("sim_id", "candidate")) %>%
  mutate(state_pct = state_pct * stay_in) %>%
  left_join(overperformance_stats, by = c("sim_id", "candidate"))

dc_sims <- vector("list", n_sims)

### Simulate
dc_sims <- foreach(i = 1:n_sims, .packages = c("Hmisc", "dplyr", "reshape2", "tidyr")) %dopar% {
  this_sim_state_percentage <- dc_state_percentages_adj %>%
    filter(sim_id == i)
  
  district_sim_pct <- this_sim_state_percentage %>%
    mutate(met_threshold = state_pct >= 0.15)
  
  state_sim_delegates <- district_sim_pct %>%
    mutate(pct_valid = state_pct * met_threshold) %>% 
    mutate(pct_valid = pct_valid / sum(pct_valid),
           state = "District of Columbia",
           district = "At-large") %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(candidate_delegates = delegates * pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, district, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate, pct = state_pct, pct_valid, candidate_delegates, sim_id) %>%
    arrange(district, candidate) 
  
  md1_sim_delegates <- district_sim_pct %>%
    mutate(pct_valid = state_pct * met_threshold) %>% 
    mutate(pct_valid = pct_valid / sum(pct_valid),
           state = "District of Columbia",
           district = "MD01") %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(candidate_delegates = delegates * pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, district, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate, pct = state_pct, pct_valid, candidate_delegates, sim_id) %>%
    arrange(district, candidate) 
  
  md2_sim_delegates <- district_sim_pct %>%
    mutate(pct_valid = state_pct * met_threshold) %>% 
    mutate(pct_valid = pct_valid / sum(pct_valid),
           state = "District of Columbia",
           district = "MD02") %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(candidate_delegates = delegates * pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, district, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate, pct = state_pct, pct_valid, candidate_delegates, sim_id) %>%
    arrange(district, candidate) 
  
  dc_delegates <- bind_rows(state_sim_delegates, md1_sim_delegates, md2_sim_delegates)
}

# Montana ####
mt_start_time <- Sys.time()

sim_demographics_table <- district_demographics %>%
  filter(state == "Montana") %>%
  mutate(prob = predict(dem_primary_logit, newdata = ., type = "response"),
         n_vote = round(pop * prob * turnout * 0.05)) %>%
  dplyr::select(-pop, -prob)

sim_demographics <- sim_demographics_table %>%
  lapply(rep, sim_demographics_table$n_vote) %>%
  as.data.frame() 
state_marginal_demographics <- shapeStateDemo("Montana")

mt_averages_over_time <- state_averages_over_time_unsmoothed %>%
  filter(state == "Montana", as.numeric(today() - median_date) <= 1) %>%
  mutate(age = as.numeric(today() - median_date),
         nreps = 2 - age)

mt_dirichlet_params <- (mt_averages_over_time %>%
                          lapply(rep, mt_averages_over_time$nreps) %>%
                          as.data.frame() %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          dplyr::select(-median_date) %>%
                          group_by(candidate) %>%
                          mutate(i = 1:n()) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-i) %>%
                          as.matrix() %>%
                          diri.est())$param

mt_dirichlet_params <- mt_dirichlet_params / 2
mt_dirichlet_params[8] <- 0

mt_state_percentages <- (rdirichlet(n_sims, mt_dirichlet_params) %>%
                            as.data.frame() %>%
                            dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, 
                                          steyer = V6, warren = V7, yang = V8)) %>%
  apply(MARGIN = 1, FUN = function(x) x / sum(x)) %>%
  t() %>%
  as.data.frame() %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

mt_state_percentages_adj <- mt_state_percentages %>%
  left_join(overperformance_bounce, by = c("sim_id", "candidate")) %>%
  mutate(state_pct = pmax(state_pct + bounce, 0)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct)) %>%
  left_join(in_second_choice_division, by = c("candidate", "sim_id")) %>%
  melt(id.vars = c("sim_id", "candidate", "state_pct", "overperformance", "bounce_factor", "stay_in", "bounce"), 
       variable.name = "second_choice", value.name = "pct") %>%
  mutate(pct = pct*state_pct*(!stay_in),
         state_pct = case_when(candidate == second_choice ~ state_pct,
                               candidate != second_choice ~ pct)) %>%
  arrange(sim_id, second_choice, candidate)

overperformance_stats <- mt_state_percentages_adj %>%
  group_by(sim_id, candidate) %>%
  summarise(overperformance = mean(overperformance),
            bounce_factor = mean(bounce_factor))

mt_state_percentages_adj <- mt_state_percentages_adj %>%
  group_by(sim_id, second_choice) %>%
  summarise(state_pct = sum(state_pct)) %>%
  dplyr::select(everything(), candidate = second_choice) %>%
  left_join(in_stayin_matrix %>% melt(id.vars = NULL, value.name = "stay_in") %>%
              dplyr::select(sim_id = Var1, candidate = Var2, stay_in), by = c("sim_id", "candidate")) %>%
  mutate(state_pct = state_pct * stay_in) %>%
  left_join(overperformance_stats, by = c("sim_id", "candidate"))

mt_sims <- vector("list", n_sims)

### Simulate
mt_sims <- foreach(i = 1:n_sims, .packages = c("Hmisc", "dplyr", "reshape2", "tidyr")) %dopar% {
  demographic_lean_sim <- demographic_leans %>%
    mutate(lean = rnorm(n(), mean = avg_lean, sd = lean_se)) %>%
    dplyr::select(candidate, demographic_var, demographic, lean)
  
  # This simulation's national percentages
  this_sim_state_percentage <- mt_state_percentages_adj %>%
    filter(sim_id == i)
  
  # Apply simulated leans to state average
  state_demographic_sim <- demoCandidateBreakdown("Montana", this_sim_state_percentage, demographic_lean_sim, 
                                                  state_marginal_demographics)
  
  # Turn these into probabilities
  gender_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("men", "women")) %>%
    dplyr::select(candidate, gender = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(gender = case_when(gender == "men" ~ "Male",
                              gender == "women" ~ "Female"))
  
  names(gender_breakdown)[-1] <- paste0("gender_", names(gender_breakdown))[-1]
  
  race_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("white", "black", "latino")) %>%
    dplyr::select(candidate, race = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(race = case_when(race == "white" ~ "White",
                            race == "black" ~ "Black",
                            race == "latino" ~ "Latino"))
  
  names(race_breakdown)[-1] <- paste0("race_", names(race_breakdown))[-1]
  
  age_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("age_1829", "age_3044", "age_4564", "age_65")) %>%
    dplyr::select(candidate, age = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(age = case_when(age == "age_1829" ~ "18-29",
                           age == "age_3044" ~ "30-44",
                           age == "age_4564" ~ "45-64",
                           age == "age_65" ~ "65+"))
  
  names(age_breakdown)[-1] <- paste0("age_", names(age_breakdown)[-1])
  
  education_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("college", "no_college")) %>%
    dplyr::select(candidate, education = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(education = case_when(education == "college" ~ "College",
                                 education == "no_college" ~ "Less than college"))
  
  names(education_breakdown)[-1] <- paste0("education_", names(education_breakdown)[-1])
  
  synth_pop <- sim_demographics %>%
    mutate(rand = runif(n()),
           age_regroup = case_when(age == "18-24" ~ "18-29",
                                   age == "25-34" & rand < 0.5 ~ "18-29",
                                   age == "25-34" & rand >= 0.5 ~ "30-44",
                                   age == "35-44" ~ "30-44",
                                   age == "45-64" ~ "45-64",
                                   age == "65+" ~ "65+"),
           id = 1:n()) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age = age_regroup, education) %>%
    mutate_if(is.factor, as.character) %>%
    left_join(gender_breakdown, by = "gender") %>%
    left_join(race_breakdown, by = "race") %>%
    left_join(age_breakdown, by = "age") %>%
    left_join(education_breakdown, by = "education") %>%
    mutate(biden_prob = gender_biden + race_biden + age_biden + education_biden,
           bloomberg_prob = gender_bloomberg + race_bloomberg + age_bloomberg + education_bloomberg,
           buttigieg_prob = gender_buttigieg + race_buttigieg + age_buttigieg + education_buttigieg,
           klobuchar_prob = gender_klobuchar + race_klobuchar + age_klobuchar + education_klobuchar,
           sanders_prob = gender_sanders + race_sanders + age_sanders + education_sanders,
           steyer_prob = gender_steyer + race_steyer + age_steyer + education_steyer,
           warren_prob = gender_warren + race_warren + age_warren + education_warren,
           yang_prob = gender_yang + race_yang + age_yang + education_yang) %>%
    mutate_at(vars(c("biden_prob", "bloomberg_prob", "buttigieg_prob", "klobuchar_prob", "sanders_prob", "steyer_prob", "warren_prob", 
                     "yang_prob")),
              function(x) pmax(x/100, 0)) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age, education, biden_prob, 
                  bloomberg_prob, buttigieg_prob, klobuchar_prob, sanders_prob, steyer_prob, warren_prob, yang_prob) %>%
    melt(id.vars = c("id", "state", "district", "primary_type", "district_delegates", "gender", "race", "age", "education"),
         variable.name = "candidate_prob", value.name = "prob") %>%
    arrange(id, candidate_prob) %>%
    group_by(id) %>%
    mutate(prob = prob / sum(prob),
           cum_prob = cumsum(prob)) %>%
    dplyr::select(-prob) %>%
    ungroup() %>%
    spread(candidate_prob, cum_prob) %>%
    mutate(rand = runif(n()),
           vote = case_when(rand <= biden_prob ~ "biden",
                            rand > biden_prob & rand <= bloomberg_prob ~ "bloomberg",
                            rand > bloomberg_prob & rand <= buttigieg_prob ~ "buttigieg",
                            rand > buttigieg_prob & rand <= klobuchar_prob ~ "klobuchar",
                            rand > klobuchar_prob & rand <= sanders_prob ~ "sanders",
                            rand > sanders_prob & rand <= steyer_prob ~ "steyer",
                            rand > steyer_prob & rand <= warren_prob ~ "warren",
                            rand > warren_prob ~ "yang")) %>%
    as.tbl()
  
  synth_district_pop <- synth_pop %>%
    group_by(state, district) %>%
    summarise(district_pop = n())
  
  district_sim_delegates <- synth_pop %>%
    group_by(state, district) %>%
    mutate(district_total = n()) %>%
    group_by(state, district, vote) %>%
    summarise(pct = n() / mean(district_total)) %>%
    ungroup() %>%
    mutate(met_threshold = pct >= 0.15) %>% 
    group_by(state, district) %>%
    mutate(pct_valid = (pct*met_threshold) / sum(pct*met_threshold)) %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(candidate_delegates = delegates * pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, district, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate = vote, pct, pct_valid, candidate_delegates) %>%
    arrange(district, candidate)
  
  state_sim_delegates <- district_sim_delegates %>%
    left_join(synth_district_pop, by = c("state", "district")) %>%
    mutate(synth_votes = pct * district_pop) %>%
    group_by(state) %>%
    mutate(state_vote = sum(synth_votes)) %>%
    group_by(state, candidate) %>%
    summarise(state_pct = sum(synth_votes) / mean(state_vote)) %>%
    mutate(district = "At-large") %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(met_threshold = state_pct >= 0.15,
           state_pct_valid = (state_pct*met_threshold) / sum(state_pct*met_threshold),
           candidate_delegates = delegates * state_pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate, pct = state_pct, pct_valid = state_pct_valid, candidate_delegates) %>%
    arrange(district, candidate) %>%
    bind_rows(district_sim_delegates) %>%
    mutate(sim_id = i)
}

# New Jersey ####
nj_start_time <- Sys.time()

sim_demographics_table <- district_demographics %>%
  filter(state == "New Jersey") %>%
  mutate(prob = predict(dem_primary_logit, newdata = ., type = "response"),
         n_vote = round(pop * prob * turnout * 0.05)) %>%
  dplyr::select(-pop, -prob)

sim_demographics <- sim_demographics_table %>%
  lapply(rep, sim_demographics_table$n_vote) %>%
  as.data.frame() 
state_marginal_demographics <- shapeStateDemo("New Jersey")

nj_averages_over_time <- state_averages_over_time_unsmoothed %>%
  filter(state == "New Jersey", as.numeric(today() - median_date) <= 1) %>%
  mutate(age = as.numeric(today() - median_date),
         nreps = 2 - age)

nj_dirichlet_params <- (nj_averages_over_time %>%
                          lapply(rep, nj_averages_over_time$nreps) %>%
                          as.data.frame() %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          dplyr::select(-median_date) %>%
                          group_by(candidate) %>%
                          mutate(i = 1:n()) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-i) %>%
                          as.matrix() %>%
                          diri.est())$param

nj_dirichlet_params <- nj_dirichlet_params / 2
nj_dirichlet_params[8] <- 0

nj_state_percentages <- (rdirichlet(n_sims, nj_dirichlet_params) %>%
                            as.data.frame() %>%
                            dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, 
                                          steyer = V6, warren = V7, yang = V8)) %>%
  apply(MARGIN = 1, FUN = function(x) x / sum(x)) %>%
  t() %>%
  as.data.frame() %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

nj_state_percentages_adj <- nj_state_percentages %>%
  left_join(overperformance_bounce, by = c("sim_id", "candidate")) %>%
  mutate(state_pct = pmax(state_pct + bounce, 0)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct)) %>%
  left_join(in_second_choice_division, by = c("candidate", "sim_id")) %>%
  melt(id.vars = c("sim_id", "candidate", "state_pct", "overperformance", "bounce_factor", "stay_in", "bounce"), 
       variable.name = "second_choice", value.name = "pct") %>%
  mutate(pct = pct*state_pct*(!stay_in),
         state_pct = case_when(candidate == second_choice ~ state_pct,
                               candidate != second_choice ~ pct)) %>%
  arrange(sim_id, second_choice, candidate)

overperformance_stats <- nj_state_percentages_adj %>%
  group_by(sim_id, candidate) %>%
  summarise(overperformance = mean(overperformance),
            bounce_factor = mean(bounce_factor))

nj_state_percentages_adj <- nj_state_percentages_adj %>%
  group_by(sim_id, second_choice) %>%
  summarise(state_pct = sum(state_pct)) %>%
  dplyr::select(everything(), candidate = second_choice) %>%
  left_join(in_stayin_matrix %>% melt(id.vars = NULL, value.name = "stay_in") %>%
              dplyr::select(sim_id = Var1, candidate = Var2, stay_in), by = c("sim_id", "candidate")) %>%
  mutate(state_pct = state_pct * stay_in) %>%
  left_join(overperformance_stats, by = c("sim_id", "candidate"))

nj_sims <- vector("list", n_sims)

### Simulate
nj_sims <- foreach(i = 1:n_sims, .packages = c("Hmisc", "dplyr", "reshape2", "tidyr")) %dopar% {
  demographic_lean_sim <- demographic_leans %>%
    mutate(lean = rnorm(n(), mean = avg_lean, sd = lean_se)) %>%
    dplyr::select(candidate, demographic_var, demographic, lean)
  
  # This simulation's national percentages
  this_sim_state_percentage <- nj_state_percentages_adj %>%
    filter(sim_id == i)
  
  # Apply simulated leans to state average
  state_demographic_sim <- demoCandidateBreakdown("New Jersey", this_sim_state_percentage, demographic_lean_sim, 
                                                  state_marginal_demographics)
  
  # Turn these into probabilities
  gender_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("men", "women")) %>%
    dplyr::select(candidate, gender = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(gender = case_when(gender == "men" ~ "Male",
                              gender == "women" ~ "Female"))
  
  names(gender_breakdown)[-1] <- paste0("gender_", names(gender_breakdown))[-1]
  
  race_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("white", "black", "latino")) %>%
    dplyr::select(candidate, race = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(race = case_when(race == "white" ~ "White",
                            race == "black" ~ "Black",
                            race == "latino" ~ "Latino"))
  
  names(race_breakdown)[-1] <- paste0("race_", names(race_breakdown))[-1]
  
  age_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("age_1829", "age_3044", "age_4564", "age_65")) %>%
    dplyr::select(candidate, age = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(age = case_when(age == "age_1829" ~ "18-29",
                           age == "age_3044" ~ "30-44",
                           age == "age_4564" ~ "45-64",
                           age == "age_65" ~ "65+"))
  
  names(age_breakdown)[-1] <- paste0("age_", names(age_breakdown)[-1])
  
  education_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("college", "no_college")) %>%
    dplyr::select(candidate, education = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(education = case_when(education == "college" ~ "College",
                                 education == "no_college" ~ "Less than college"))
  
  names(education_breakdown)[-1] <- paste0("education_", names(education_breakdown)[-1])
  
  synth_pop <- sim_demographics %>%
    mutate(rand = runif(n()),
           age_regroup = case_when(age == "18-24" ~ "18-29",
                                   age == "25-34" & rand < 0.5 ~ "18-29",
                                   age == "25-34" & rand >= 0.5 ~ "30-44",
                                   age == "35-44" ~ "30-44",
                                   age == "45-64" ~ "45-64",
                                   age == "65+" ~ "65+"),
           id = 1:n()) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age = age_regroup, education) %>%
    mutate_if(is.factor, as.character) %>%
    left_join(gender_breakdown, by = "gender") %>%
    left_join(race_breakdown, by = "race") %>%
    left_join(age_breakdown, by = "age") %>%
    left_join(education_breakdown, by = "education") %>%
    mutate(biden_prob = gender_biden + race_biden + age_biden + education_biden,
           bloomberg_prob = gender_bloomberg + race_bloomberg + age_bloomberg + education_bloomberg,
           buttigieg_prob = gender_buttigieg + race_buttigieg + age_buttigieg + education_buttigieg,
           klobuchar_prob = gender_klobuchar + race_klobuchar + age_klobuchar + education_klobuchar,
           sanders_prob = gender_sanders + race_sanders + age_sanders + education_sanders,
           steyer_prob = gender_steyer + race_steyer + age_steyer + education_steyer,
           warren_prob = gender_warren + race_warren + age_warren + education_warren,
           yang_prob = gender_yang + race_yang + age_yang + education_yang) %>%
    mutate_at(vars(c("biden_prob", "bloomberg_prob", "buttigieg_prob", "klobuchar_prob", "sanders_prob", "steyer_prob", "warren_prob", 
                     "yang_prob")),
              function(x) pmax(x/100, 0)) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age, education, biden_prob, 
                  bloomberg_prob, buttigieg_prob, klobuchar_prob, sanders_prob, steyer_prob, warren_prob, yang_prob) %>%
    melt(id.vars = c("id", "state", "district", "primary_type", "district_delegates", "gender", "race", "age", "education"),
         variable.name = "candidate_prob", value.name = "prob") %>%
    arrange(id, candidate_prob) %>%
    group_by(id) %>%
    mutate(prob = prob / sum(prob),
           cum_prob = cumsum(prob)) %>%
    dplyr::select(-prob) %>%
    ungroup() %>%
    spread(candidate_prob, cum_prob) %>%
    mutate(rand = runif(n()),
           vote = case_when(rand <= biden_prob ~ "biden",
                            rand > biden_prob & rand <= bloomberg_prob ~ "bloomberg",
                            rand > bloomberg_prob & rand <= buttigieg_prob ~ "buttigieg",
                            rand > buttigieg_prob & rand <= klobuchar_prob ~ "klobuchar",
                            rand > klobuchar_prob & rand <= sanders_prob ~ "sanders",
                            rand > sanders_prob & rand <= steyer_prob ~ "steyer",
                            rand > steyer_prob & rand <= warren_prob ~ "warren",
                            rand > warren_prob ~ "yang")) %>%
    as.tbl()
  
  synth_district_pop <- synth_pop %>%
    group_by(state, district) %>%
    summarise(district_pop = n())
  
  district_sim_delegates <- synth_pop %>%
    group_by(state, district) %>%
    mutate(district_total = n()) %>%
    group_by(state, district, vote) %>%
    summarise(pct = n() / mean(district_total)) %>%
    ungroup() %>%
    mutate(met_threshold = pct >= 0.15) %>% 
    group_by(state, district) %>%
    mutate(pct_valid = (pct*met_threshold) / sum(pct*met_threshold)) %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(candidate_delegates = delegates * pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, district, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate = vote, pct, pct_valid, candidate_delegates) %>%
    arrange(district, candidate)
  
  state_sim_delegates <- district_sim_delegates %>%
    left_join(synth_district_pop, by = c("state", "district")) %>%
    mutate(synth_votes = pct * district_pop) %>%
    group_by(state) %>%
    mutate(state_vote = sum(synth_votes)) %>%
    group_by(state, candidate) %>%
    summarise(state_pct = sum(synth_votes) / mean(state_vote)) %>%
    mutate(district = "At-large") %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(met_threshold = state_pct >= 0.15,
           state_pct_valid = (state_pct*met_threshold) / sum(state_pct*met_threshold),
           candidate_delegates = delegates * state_pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate, pct = state_pct, pct_valid = state_pct_valid, candidate_delegates) %>%
    arrange(district, candidate) %>%
    bind_rows(district_sim_delegates) %>%
    mutate(sim_id = i)
}

# New Mexico ####
nm_start_time <- Sys.time()

sim_demographics_table <- district_demographics %>%
  filter(state == "New Mexico") %>%
  mutate(prob = predict(dem_primary_logit, newdata = ., type = "response"),
         n_vote = round(pop * prob * turnout * 0.05)) %>%
  dplyr::select(-pop, -prob)

sim_demographics <- sim_demographics_table %>%
  lapply(rep, sim_demographics_table$n_vote) %>%
  as.data.frame() 
state_marginal_demographics <- shapeStateDemo("New Mexico")

nm_averages_over_time <- state_averages_over_time_unsmoothed %>%
  filter(state == "New Mexico", as.numeric(today() - median_date) <= 1) %>%
  mutate(age = as.numeric(today() - median_date),
         nreps = 2 - age)

nm_dirichlet_params <- (nm_averages_over_time %>%
                          lapply(rep, nm_averages_over_time$nreps) %>%
                          as.data.frame() %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          dplyr::select(-median_date) %>%
                          group_by(candidate) %>%
                          mutate(i = 1:n()) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-i) %>%
                          as.matrix() %>%
                          diri.est())$param

nm_dirichlet_params <- nm_dirichlet_params / 2
nm_dirichlet_params[8] <- 0

nm_state_percentages <- (rdirichlet(n_sims, nm_dirichlet_params) %>%
                            as.data.frame() %>%
                            dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, 
                                          steyer = V6, warren = V7, yang = V8)) %>%
  apply(MARGIN = 1, FUN = function(x) x / sum(x)) %>%
  t() %>%
  as.data.frame() %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

nm_state_percentages_adj <- nm_state_percentages %>%
  left_join(overperformance_bounce, by = c("sim_id", "candidate")) %>%
  mutate(state_pct = pmax(state_pct + bounce, 0)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct)) %>%
  left_join(in_second_choice_division, by = c("candidate", "sim_id")) %>%
  melt(id.vars = c("sim_id", "candidate", "state_pct", "overperformance", "bounce_factor", "stay_in", "bounce"), 
       variable.name = "second_choice", value.name = "pct") %>%
  mutate(pct = pct*state_pct*(!stay_in),
         state_pct = case_when(candidate == second_choice ~ state_pct,
                               candidate != second_choice ~ pct)) %>%
  arrange(sim_id, second_choice, candidate)

overperformance_stats <- nm_state_percentages_adj %>%
  group_by(sim_id, candidate) %>%
  summarise(overperformance = mean(overperformance),
            bounce_factor = mean(bounce_factor))

nm_state_percentages_adj <- nm_state_percentages_adj %>%
  group_by(sim_id, second_choice) %>%
  summarise(state_pct = sum(state_pct)) %>%
  dplyr::select(everything(), candidate = second_choice) %>%
  left_join(in_stayin_matrix %>% melt(id.vars = NULL, value.name = "stay_in") %>%
              dplyr::select(sim_id = Var1, candidate = Var2, stay_in), by = c("sim_id", "candidate")) %>%
  mutate(state_pct = state_pct * stay_in) %>%
  left_join(overperformance_stats, by = c("sim_id", "candidate"))

nm_sims <- vector("list", n_sims)

### Simulate
nm_sims <- foreach(i = 1:n_sims, .packages = c("Hmisc", "dplyr", "reshape2", "tidyr")) %dopar% {
  demographic_lean_sim <- demographic_leans %>%
    mutate(lean = rnorm(n(), mean = avg_lean, sd = lean_se)) %>%
    dplyr::select(candidate, demographic_var, demographic, lean)
  
  # This simulation's national percentages
  this_sim_state_percentage <- nm_state_percentages_adj %>%
    filter(sim_id == i)
  
  # Apply simulated leans to state average
  state_demographic_sim <- demoCandidateBreakdown("New Mexico", this_sim_state_percentage, demographic_lean_sim, 
                                                  state_marginal_demographics)
  
  # Turn these into probabilities
  gender_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("men", "women")) %>%
    dplyr::select(candidate, gender = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(gender = case_when(gender == "men" ~ "Male",
                              gender == "women" ~ "Female"))
  
  names(gender_breakdown)[-1] <- paste0("gender_", names(gender_breakdown))[-1]
  
  race_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("white", "black", "latino")) %>%
    dplyr::select(candidate, race = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(race = case_when(race == "white" ~ "White",
                            race == "black" ~ "Black",
                            race == "latino" ~ "Latino"))
  
  names(race_breakdown)[-1] <- paste0("race_", names(race_breakdown))[-1]
  
  age_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("age_1829", "age_3044", "age_4564", "age_65")) %>%
    dplyr::select(candidate, age = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(age = case_when(age == "age_1829" ~ "18-29",
                           age == "age_3044" ~ "30-44",
                           age == "age_4564" ~ "45-64",
                           age == "age_65" ~ "65+"))
  
  names(age_breakdown)[-1] <- paste0("age_", names(age_breakdown)[-1])
  
  education_breakdown <- state_demographic_sim %>%
    filter(demographic %in% c("college", "no_college")) %>%
    dplyr::select(candidate, education = demographic, demo_pct) %>%
    spread(candidate, demo_pct) %>%
    mutate(education = case_when(education == "college" ~ "College",
                                 education == "no_college" ~ "Less than college"))
  
  names(education_breakdown)[-1] <- paste0("education_", names(education_breakdown)[-1])
  
  synth_pop <- sim_demographics %>%
    mutate(rand = runif(n()),
           age_regroup = case_when(age == "18-24" ~ "18-29",
                                   age == "25-34" & rand < 0.5 ~ "18-29",
                                   age == "25-34" & rand >= 0.5 ~ "30-44",
                                   age == "35-44" ~ "30-44",
                                   age == "45-64" ~ "45-64",
                                   age == "65+" ~ "65+"),
           id = 1:n()) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age = age_regroup, education) %>%
    mutate_if(is.factor, as.character) %>%
    left_join(gender_breakdown, by = "gender") %>%
    left_join(race_breakdown, by = "race") %>%
    left_join(age_breakdown, by = "age") %>%
    left_join(education_breakdown, by = "education") %>%
    mutate(biden_prob = gender_biden + race_biden + age_biden + education_biden,
           bloomberg_prob = gender_bloomberg + race_bloomberg + age_bloomberg + education_bloomberg,
           buttigieg_prob = gender_buttigieg + race_buttigieg + age_buttigieg + education_buttigieg,
           klobuchar_prob = gender_klobuchar + race_klobuchar + age_klobuchar + education_klobuchar,
           sanders_prob = gender_sanders + race_sanders + age_sanders + education_sanders,
           steyer_prob = gender_steyer + race_steyer + age_steyer + education_steyer,
           warren_prob = gender_warren + race_warren + age_warren + education_warren,
           yang_prob = gender_yang + race_yang + age_yang + education_yang) %>%
    mutate_at(vars(c("biden_prob", "bloomberg_prob", "buttigieg_prob", "klobuchar_prob", "sanders_prob", "steyer_prob", "warren_prob", 
                     "yang_prob")),
              function(x) pmax(x/100, 0)) %>%
    dplyr::select(id, state, district, primary_type, district_delegates, gender, race, age, education, biden_prob, 
                  bloomberg_prob, buttigieg_prob, klobuchar_prob, sanders_prob, steyer_prob, warren_prob, yang_prob) %>%
    melt(id.vars = c("id", "state", "district", "primary_type", "district_delegates", "gender", "race", "age", "education"),
         variable.name = "candidate_prob", value.name = "prob") %>%
    arrange(id, candidate_prob) %>%
    group_by(id) %>%
    mutate(prob = prob / sum(prob),
           cum_prob = cumsum(prob)) %>%
    dplyr::select(-prob) %>%
    ungroup() %>%
    spread(candidate_prob, cum_prob) %>%
    mutate(rand = runif(n()),
           vote = case_when(rand <= biden_prob ~ "biden",
                            rand > biden_prob & rand <= bloomberg_prob ~ "bloomberg",
                            rand > bloomberg_prob & rand <= buttigieg_prob ~ "buttigieg",
                            rand > buttigieg_prob & rand <= klobuchar_prob ~ "klobuchar",
                            rand > klobuchar_prob & rand <= sanders_prob ~ "sanders",
                            rand > sanders_prob & rand <= steyer_prob ~ "steyer",
                            rand > steyer_prob & rand <= warren_prob ~ "warren",
                            rand > warren_prob ~ "yang")) %>%
    as.tbl()
  
  synth_district_pop <- synth_pop %>%
    group_by(state, district) %>%
    summarise(district_pop = n())
  
  district_sim_delegates <- synth_pop %>%
    group_by(state, district) %>%
    mutate(district_total = n()) %>%
    group_by(state, district, vote) %>%
    summarise(pct = n() / mean(district_total)) %>%
    ungroup() %>%
    mutate(met_threshold = pct >= 0.15) %>% 
    group_by(state, district) %>%
    mutate(pct_valid = (pct*met_threshold) / sum(pct*met_threshold)) %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(candidate_delegates = delegates * pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, district, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate = vote, pct, pct_valid, candidate_delegates) %>%
    arrange(district, candidate)
  
  state_sim_delegates <- district_sim_delegates %>%
    left_join(synth_district_pop, by = c("state", "district")) %>%
    mutate(synth_votes = pct * district_pop) %>%
    group_by(state) %>%
    mutate(state_vote = sum(synth_votes)) %>%
    group_by(state, candidate) %>%
    summarise(state_pct = sum(synth_votes) / mean(state_vote)) %>%
    mutate(district = "At-large") %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(met_threshold = state_pct >= 0.15,
           state_pct_valid = (state_pct*met_threshold) / sum(state_pct*met_threshold),
           candidate_delegates = delegates * state_pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate, pct = state_pct, pct_valid = state_pct_valid, candidate_delegates) %>%
    arrange(district, candidate) %>%
    bind_rows(district_sim_delegates) %>%
    mutate(sim_id = i)
}

# South Dakota ####
sd_start_time <- Sys.time()

# Have no South Dakota polls; use nearest neighbors
sd_averages_over_time <- state_averages_over_time_unsmoothed %>%
  filter(state %in% c("Missouri", "Wisconsin", "Oklahoma", "Iowa"), as.numeric(today() - median_date) <= 1) %>%
  mutate(age = as.numeric(today() - median_date),
         nreps = 2 - age,
         inv_distance = case_when(state == "Missouri" ~ 0.556,
                                  state == "Oklahoma" ~ 0.465,
                                  state == "Wisconsin" ~ 0.474,
                                  state == "Iowa" ~ 0.62)) %>%
  group_by(candidate, median_date, age, nreps) %>%
  summarise(pct = wtd.mean(pct, inv_distance))

sd_dirichlet_params <- ((sd_averages_over_time %>%
                           lapply(rep, sd_averages_over_time$nreps) %>%
                           as.data.frame() %>%
                           dplyr::select(candidate, median_date, pct) %>%
                           na.omit() %>%
                           dplyr::select(-median_date) %>%
                           group_by(candidate) %>%
                           mutate(i = 1:n()) %>%
                           spread(candidate, pct) %>%
                           dplyr::select(-i) %>%
                           as.matrix() %>%
                           diri.est())$param)

sd_dirichlet_params <- 0.5 * sd_dirichlet_params / min(sd_dirichlet_params)
sd_dirichlet_params[8] <- 0

sd_state_percentages <- (rdirichlet(n_sims, sd_dirichlet_params) %>%
                            as.data.frame() %>%
                            dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, 
                                          steyer = V6, warren = V7, yang = V8)) %>%
  apply(MARGIN = 1, FUN = function(x) x / sum(x)) %>%
  t() %>%
  as.data.frame() %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

sd_state_percentages_adj <- sd_state_percentages %>%
  left_join(overperformance_bounce, by = c("sim_id", "candidate")) %>%
  mutate(state_pct = pmax(state_pct + bounce, 0)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct)) %>%
  left_join(in_second_choice_division, by = c("candidate", "sim_id")) %>%
  melt(id.vars = c("sim_id", "candidate", "state_pct", "overperformance", "bounce_factor", "stay_in", "bounce"), 
       variable.name = "second_choice", value.name = "pct") %>%
  mutate(pct = pct*state_pct*(!stay_in),
         state_pct = case_when(candidate == second_choice ~ state_pct,
                               candidate != second_choice ~ pct)) %>%
  arrange(sim_id, second_choice, candidate)

overperformance_stats <- sd_state_percentages_adj %>%
  group_by(sim_id, candidate) %>%
  summarise(overperformance = mean(overperformance),
            bounce_factor = mean(bounce_factor))

sd_state_percentages_adj <- sd_state_percentages_adj %>%
  group_by(sim_id, second_choice) %>%
  summarise(state_pct = sum(state_pct)) %>%
  dplyr::select(everything(), candidate = second_choice) %>%
  left_join(in_stayin_matrix %>% melt(id.vars = NULL, value.name = "stay_in") %>%
              dplyr::select(sim_id = Var1, candidate = Var2, stay_in), by = c("sim_id", "candidate")) %>%
  mutate(state_pct = state_pct * stay_in) %>%
  left_join(overperformance_stats, by = c("sim_id", "candidate"))

sd_sims <- vector("list", n_sims)

### Simulate
sd_sims <- foreach(i = 1:n_sims, .packages = c("Hmisc", "dplyr", "reshape2", "tidyr")) %dopar% {
  this_sim_state_percentage <- sd_state_percentages_adj %>%
    filter(sim_id == i)
  
  district_sim_pct <- this_sim_state_percentage %>%
    mutate(met_threshold = state_pct >= 0.15)
  
  state_sim_delegates <- district_sim_pct %>%
    mutate(pct_valid = state_pct * met_threshold) %>% 
    mutate(pct_valid = pct_valid / sum(pct_valid),
           state = "South Dakota",
           district = "At-large") %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(candidate_delegates = delegates * pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, district, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate, pct = state_pct, pct_valid, candidate_delegates, sim_id) %>%
    arrange(district, candidate)
}

# Virgin Islands (6 June 2020) ####
vi_start_time <- Sys.time()
# Shortcut since I know nothing about what the situation looks like on the ground: 
### Draw vote shares
vi_averages_over_time <- national_averages_adjusted %>%
  filter(as.numeric(today() - median_date) <= 1) %>%
  mutate(age = as.numeric(today() - median_date),
         nreps = 2 - age) %>%
  group_by(median_date) %>%
  mutate(pct = pct / sum(pct))

vi_dirichlet_params <- (vi_averages_over_time %>%
                          lapply(rep, vi_averages_over_time$nreps) %>%
                          as.data.frame() %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          dplyr::select(-median_date) %>%
                          group_by(candidate) %>%
                          mutate(i = 1:n()) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-i) %>%
                          as.matrix() %>%
                          diri.est())$param

vi_dirichlet_params <- 0.5 * vi_dirichlet_params / max(1, min(vi_dirichlet_params))
vi_dirichlet_params[8] <- 0

vi_state_percentages <-(rdirichlet(n_sims, vi_dirichlet_params) %>%
                           as.data.frame() %>%
                           dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, 
                                         steyer = V6, warren = V7, yang = V8)) %>%
  apply(MARGIN = 1, FUN = function(x) x / sum(x)) %>%
  t() %>%
  as.data.frame() %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

vi_state_percentages_adj <- vi_state_percentages %>%
  left_join(overperformance_bounce, by = c("sim_id", "candidate")) %>%
  mutate(state_pct = pmax(state_pct + bounce, 0)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct)) %>%
  left_join(in_second_choice_division, by = c("candidate", "sim_id")) %>%
  melt(id.vars = c("sim_id", "candidate", "state_pct", "overperformance", "bounce_factor", "stay_in", "bounce"), 
       variable.name = "second_choice", value.name = "pct") %>%
  mutate(pct = pct*state_pct*(!stay_in),
         state_pct = case_when(candidate == second_choice ~ state_pct,
                               candidate != second_choice ~ pct)) %>%
  arrange(sim_id, second_choice, candidate)

overperformance_stats <- vi_state_percentages_adj %>%
  group_by(sim_id, candidate) %>%
  summarise(overperformance = mean(overperformance),
            bounce_factor = mean(bounce_factor))

vi_state_percentages_adj <- vi_state_percentages_adj %>%
  group_by(sim_id, second_choice) %>%
  summarise(state_pct = sum(state_pct)) %>%
  dplyr::select(everything(), candidate = second_choice) %>%
  left_join(in_stayin_matrix %>% melt(id.vars = NULL, value.name = "stay_in") %>%
              dplyr::select(sim_id = Var1, candidate = Var2, stay_in), by = c("sim_id", "candidate")) %>%
  mutate(state_pct = state_pct * stay_in) %>%
  left_join(overperformance_stats, by = c("sim_id", "candidate"))

vi_sims <- vector("list", n_sims)

### Simulate
vi_sims <- foreach(i = 1:n_sims, .packages = c("Hmisc", "dplyr", "reshape2", "tidyr")) %dopar% {
  this_sim_state_percentage <- vi_state_percentages_adj %>%
    filter(sim_id == i) %>%
    mutate(state_pct = state_pct / sum(state_pct))
  
  district_sim_pct <- this_sim_state_percentage %>%
    mutate(met_threshold = state_pct >= 0.15)
  
  st_thomas_sim_delegates <- district_sim_pct %>%
    mutate(pct_valid = state_pct * met_threshold) %>% 
    mutate(pct_valid = pct_valid / sum(pct_valid),
           state = "Virgin Islands",
           district = "St. Thomas/St. John") %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(candidate_delegates = delegates * pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, district, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate, pct = state_pct, pct_valid, candidate_delegates, sim_id) %>%
    arrange(district, candidate) 
  
  st_croix_sim_delegates <- district_sim_pct %>%
    mutate(pct_valid = state_pct * met_threshold) %>% 
    mutate(pct_valid = pct_valid / sum(pct_valid),
           state = "Virgin Islands",
           district = "St. Croix") %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(candidate_delegates = delegates * pct_valid,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, district, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate, pct = state_pct, pct_valid, candidate_delegates, sim_id) %>%
    arrange(district, candidate) 
  
  vi_delegates <- bind_rows(st_thomas_sim_delegates, st_croix_sim_delegates)
}

# Wrap-up ####
end_time <- Sys.time()
all_sims <- bind_rows(ia_sims, nh_sims, nv_sims, sc_sims, al_sims, as_sims, ar_sims, ca_sims, co_sims, da_sims, ma_sims, mn_sims, nc_sims, 
                      ok_sims, tn_sims, tx_sims, ut_sims, vt_sims, va_sims, me_sims, id_sims, mi_sims, ms_sims, mo_sims, nd_sims, oh_sims,
                      wa_sims, na_sims, az_sims, fl_sims, il_sims, ga_sims, pr_sims, ak_sims, hi_sims, la_sims, wy_sims, wi_sims, ct_sims,
                      de_sims, md_sims, ny_sims, pa_sims, ri_sims, gu_sims, ks_sims, in_sims, ne_sims, wv_sims, ky_sims, or_sims, dc_sims,
                      mt_sims, nj_sims, nm_sims, sd_sims, vi_sims) 

all_sims_by_state <- all_sims %>%
  arrange(sim_id, state, candidate, district) %>%
  group_by(sim_id, state, candidate) %>%
  summarise(pct = first(pct),
            delegates = sum(candidate_delegates))

all_sims %>%
  write_csv("output/sims_by_district.csv")

all_sims_by_state %>%
  write_csv("output/sims_by_state.csv")

state_timer <- data.frame(Stage = c("Setup", "Iowa", "New Hampshire", "Nevada", "South Carolina", "Alabama", "American Samoa", "Arkansas",
                                    "California", "Colorado", "Democrats Abroad", "Massachusetts", "Minnesota", "North Carolina", "Oklahoma",
                                    "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Maine", "Idaho", "Michigan", "Mississippi",
                                    "Missouri", "North Dakota", "Ohio", "Washington", "Northern Marianas", "Arizona", "Florida", "Illinois",
                                    "Georgia", "Puerto Rico", "Alaska", "Hawaii", "Louisiana", "Wyoming", "Wisconsin", "Connecticut",
                                    "Delaware", "Maryland", "New York", "Pennsylvania", "Rhode Island", "Guam", "Kansas", "Indiana",
                                    "Nebraska", "West Virginia", "Kentucky", "Oregon", "District of Columbia", "Montana", "New Jersey",
                                    "New Mexico", "South Dakota", "Virgin Islands"),
                          Time = c(ia_start_time - start_time,
                                   nh_start_time - ia_start_time,
                                   nv_start_time - nh_start_time,
                                   sc_start_time - nv_start_time,
                                   al_start_time - sc_start_time,
                                   as_start_time - al_start_time,
                                   ar_start_time - as_start_time,
                                   ca_start_time - ar_start_time,
                                   co_start_time - ca_start_time,
                                   da_start_time - co_start_time,
                                   ma_start_time - da_start_time,
                                   mn_start_time - ma_start_time,
                                   nc_start_time - mn_start_time,
                                   ok_start_time - nc_start_time,
                                   tn_start_time - ok_start_time,
                                   tx_start_time - tn_start_time,
                                   ut_start_time - tx_start_time,
                                   vt_start_time - ut_start_time,
                                   va_start_time - vt_start_time,
                                   me_start_time - va_start_time,
                                   id_start_time - me_start_time,
                                   mi_start_time - id_start_time,
                                   ms_start_time - mi_start_time,
                                   mo_start_time - ms_start_time,
                                   nd_start_time - mo_start_time,
                                   oh_start_time - nd_start_time,
                                   wa_start_time - oh_start_time,
                                   na_start_time - wa_start_time,
                                   az_start_time - na_start_time,
                                   fl_start_time - az_start_time,
                                   il_start_time - fl_start_time,
                                   ga_start_time - il_start_time,
                                   pr_start_time - ga_start_time,
                                   ak_start_time - pr_start_time,
                                   hi_start_time - ak_start_time,
                                   la_start_time - hi_start_time,
                                   wy_start_time - la_start_time,
                                   wi_start_time - wy_start_time,
                                   ct_start_time - wi_start_time,
                                   de_start_time - ct_start_time,
                                   md_start_time - de_start_time,
                                   ny_start_time - md_start_time,
                                   pa_start_time - ny_start_time,
                                   ri_start_time - pa_start_time,
                                   gu_start_time - ri_start_time,
                                   ks_start_time - gu_start_time,
                                   in_start_time - ks_start_time,
                                   ne_start_time - in_start_time,
                                   wv_start_time - ne_start_time,
                                   ky_start_time - wv_start_time,
                                   or_start_time - ky_start_time,
                                   dc_start_time - or_start_time,
                                   mt_start_time - dc_start_time,
                                   nj_start_time - mt_start_time,
                                   nm_start_time - nj_start_time,
                                   sd_start_time - nm_start_time,
                                   vi_start_time - sd_start_time,
                                   end_time - vi_start_time)) %>%
  mutate(Time = round(Time))

stopCluster(cl)

# rm(list = c("polls_df_list", grep("^.{2}_sims$", ls(), value = TRUE)))
