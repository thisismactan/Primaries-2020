# Simulation for February 3, 2020 (Iowa caucuses)

## Simulate for the state

# For one state
state_marginal_demographics <- district_demographics %>%
  filter(state == sim_state) %>%
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

demographic_lean_sim <- demographic_leans %>%
  mutate(lean = rnorm(n(), mean = avg_lean, sd = lean_se)) %>%
  dplyr::select(candidate, demographic_var, demographic, lean)

state_sim_list <- vector("list", n_sims)

for(i in 1:n_sims) {
  # This simulation's national percentages
  this_sim_state_percentage <- sim_state_percentages %>%
    filter(sim_id == i)
  
  # Simulate for a state
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
  
  # Apply simulated leans to state average
  state_demographic_sim <- state_lean_sim %>%
    mutate(demo_pct = pmax(state_pct + scaled_lean/100, 0))
  
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
  
  # Draw votes
  sim_demographics <- district_demographics %>%
    filter(state == sim_state) %>%
    mutate(prob = predict(dem_primary_logit, newdata = ., type = "response"),
           n_vote = round(pop * prob * turnout * 0.1)) %>%
    dplyr::select(-pop, -prob)
  
  if(sim_state %in% c("Iowa", "New Hampshire", "Nevada", "South Carolina")) {
    synth_pop <- sim_demographics %>%
      lapply(rep, sim_demographics$n_vote) %>%
      as.data.frame() %>%
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
  } else {
    synth_pop <- sim_demographics %>%
      lapply(rep, sim_demographics$n_vote) %>%
      as.data.frame() %>%
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
  }
  
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
    mutate(pct = pct.x + pct_from_second_choice) %>% 
    filter(met_threshold) %>%
    group_by(state, district) %>%
    mutate(pct = pct / sum(pct)) %>%
    left_join(district_delegates, by = c("state", "district")) %>%
    mutate(candidate_delegates = delegates * pct,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, district, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate = vote, pct, candidate_delegates) %>%
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
    filter(state_pct >= 0.15) %>%
    mutate(state_pct = state_pct / sum(state_pct),
           candidate_delegates = delegates * state_pct,
           floor_delegates = floor(candidate_delegates),
           remainder = candidate_delegates - floor_delegates) %>%
    arrange(state, desc(remainder)) %>%
    mutate(rank = 1:n(),
           total_remaining_delegates = delegates - sum(floor_delegates),
           candidate_delegates = floor_delegates + (rank <= total_remaining_delegates)) %>%
    dplyr::select(state, district, candidate, pct = state_pct, candidate_delegates) %>%
    arrange(district, candidate) %>%
    bind_rows(district_sim_delegates) %>%
    mutate(sim_id = i)
  
  state_sim_list[[i]] <- state_sim_delegates
}
