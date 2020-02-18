# Iowa (3 Feb 2020) ####
ia_dirichlet_params <- (state_averages_over_time %>%
                            filter(state == "Iowa", as.numeric(today() - median_date) <= 7) %>%
                            mutate(age = as.numeric(today() - median_date)) %>%
                            dplyr::select(candidate, median_date, pct) %>%
                            na.omit() %>%
                            mutate(pct = pct / 100) %>%
                            spread(candidate, pct) %>%
                            dplyr::select(-median_date) %>%
                            as.matrix() %>%
                            diri.est())$param

ia_state_percentages <- rdirichlet(n_sims, ia_dirichlet_params) %>%
  as.data.frame() %>%
  dplyr::select(biden = V1, buttigieg = V2, klobuchar = V3, sanders = V4, steyer = V5, warren = V6, yang = V7) %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")


# New Hampshire (11 Feb 2020) ####
nh_dirichlet_params <- (state_averages_over_time %>%
                          filter(state == "New Hampshire", as.numeric(today() - median_date) <= 7) %>%
                          mutate(age = as.numeric(today() - median_date)) %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          mutate(pct = pct / 100) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-median_date) %>%
                          as.matrix() %>%
                          diri.est())$param

nh_state_percentages <- rdirichlet(n_sims, nh_dirichlet_params) %>%
  as.data.frame() %>%
  dplyr::select(biden = V1, buttigieg = V2, klobuchar = V3, sanders = V4, steyer = V5, warren = V6, yang = V7) %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

# Nevada (22 Feb 2020) ####
nv_dirichlet_params <- (state_averages_over_time %>%
                          filter(state == "Nevada", as.numeric(today() - median_date) <= 7) %>%
                          mutate(age = as.numeric(today() - median_date)) %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          mutate(pct = pct / 100) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-median_date) %>%
                          as.matrix() %>%
                          diri.est())$param

nv_state_percentages <- rdirichlet(n_sims, nv_dirichlet_params) %>%
  as.data.frame() %>%
  dplyr::select(biden = V1, buttigieg = V2, klobuchar = V3, sanders = V4, steyer = V5, warren = V6, yang = V7) %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

# South Carolina (29 Feb 2020) ####
sc_dirichlet_params <- (state_averages_over_time %>%
                          filter(state == "South Carolina", as.numeric(today() - median_date) <= 7) %>%
                          mutate(age = as.numeric(today() - median_date)) %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          mutate(pct = pct / 100) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-median_date) %>%
                          as.matrix() %>%
                          diri.est())$param

sc_state_percentages <- rdirichlet(n_sims, sc_dirichlet_params) %>%
  as.data.frame() %>%
  dplyr::select(biden = V1, buttigieg = V2, klobuchar = V3, sanders = V4, steyer = V5, warren = V6, yang = V7) %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

# Alabama (3 Mar 2020) ####
al_dirichlet_params <- (state_averages_over_time %>%
                            filter(state == "Alabama", as.numeric(today() - median_date) <= 7) %>%
                            mutate(age = as.numeric(today() - median_date)) %>%
                            dplyr::select(candidate, median_date, pct) %>%
                            na.omit() %>%
                            mutate(pct = pct / 100) %>%
                            spread(candidate, pct) %>%
                            dplyr::select(-median_date) %>%
                            as.matrix() %>%
                            diri.est())$param

al_state_percentages <- rdirichlet(n_sims, al_dirichlet_params) %>%
  as.data.frame() %>%
  dplyr::select(biden = V1, buttigieg = V2, klobuchar = V3, sanders = V4, warren = V5, yang = V6) %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

# American Samoa (3 Mar 2020) ####
as_state_percentages <- rdirichlet(n_sims, c(2, 1, 1, 1, 2, 1, 1, 1)) %>%
  as.data.frame() %>%
  dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, steyer = V6, warren = V7, yang = V8) %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

# California (3 Mar 2020) ####
ca_dirichlet_params <- (state_averages_over_time %>%
                          filter(state == "California", as.numeric(today() - median_date) <= 7) %>%
                          mutate(age = as.numeric(today() - median_date)) %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          mutate(pct = pct / 100) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-median_date) %>%
                          as.matrix() %>%
                          diri.est())$param

ca_state_percentages <- rdirichlet(n_sims, ca_dirichlet_params) %>%
  as.data.frame() %>%
  dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, steyer = V6, warren = V7, yang = V8) %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

# Colorado (3 Mar 2020) ####
co_dirichlet_params <- (state_averages_over_time %>%
                          filter(state == "Colorado", as.numeric(today() - median_date) <= 7) %>%
                          mutate(age = as.numeric(today() - median_date)) %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          mutate(pct = pct / 100) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-median_date) %>%
                          as.matrix() %>%
                          diri.est())$param

co_state_percentages <- rdirichlet(n_sims, co_dirichlet_params) %>%
  as.data.frame() %>%
  dplyr::select(biden = V1, buttigieg = V2, klobuchar = V3, sanders = V4, steyer = V5, warren = V6, yang = V7) %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

# Democrats Abroad (3 Mar 2020) ####
da_state_percentages <- rdirichlet(n_sims, c(2, 1, 1, 1, 2, 1, 1, 1)) %>%
  as.data.frame() %>%
  dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, steyer = V6, warren = V7, yang = V8) %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

# Massachusetts (3 Mar 2020) ####
ma_dirichlet_params <- (state_averages_over_time %>%
                          filter(state == "Massachusetts", as.numeric(today() - median_date) <= 7) %>%
                          mutate(age = as.numeric(today() - median_date)) %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          mutate(pct = pct / 100) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-median_date) %>%
                          as.matrix() %>%
                          diri.est())$param

ma_state_percentages <- rdirichlet(n_sims, ma_dirichlet_params) %>%
  as.data.frame() %>%
  dplyr::select(biden = V1, buttigieg = V2, klobuchar = V3, sanders = V4, steyer = V5, warren = V6, yang = V7) %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

# Minnesota (3 Mar 2020) ####
mn_dirichlet_params <- (state_averages_over_time %>%
                          filter(state == "Minnesota", as.numeric(today() - median_date) <= 7) %>%
                          mutate(age = as.numeric(today() - median_date)) %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          mutate(pct = pct / 100) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-median_date) %>%
                          as.matrix() %>%
                          diri.est())$param

mn_state_percentages <- rdirichlet(n_sims, mn_dirichlet_params) %>%
  as.data.frame() %>%
  dplyr::select(biden = V1, buttigieg = V2, klobuchar = V3, sanders = V4, steyer = V5, warren = V6, yang = V7) %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

# North Carolina (3 Mar 2020) ####
nc_dirichlet_params <- (state_averages_over_time %>%
                          filter(state == "North Carolina", as.numeric(today() - median_date) <= 7) %>%
                          mutate(age = as.numeric(today() - median_date)) %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          mutate(pct = pct / 100) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-median_date) %>%
                          na.omit() %>%
                          as.matrix() %>%
                          diri.est())$param

nc_state_percentages <- rdirichlet(n_sims, nc_dirichlet_params) %>%
  as.data.frame() %>%
  dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, steyer = V6, warren = V7, yang = V8) %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

# Oklahoma (3 Mar 2020) ####
ok_dirichlet_params <- (state_averages_over_time %>%
                          filter(state == "Oklahoma", as.numeric(today() - median_date) <= 7) %>%
                          mutate(age = as.numeric(today() - median_date)) %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          mutate(pct = pct / 100) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-median_date) %>%
                          na.omit() %>%
                          as.matrix() %>%
                          diri.est())$param

ok_state_percentages <- rdirichlet(n_sims, ok_dirichlet_params) %>%
  as.data.frame() %>%
  dplyr::select(biden = V1, buttigieg = V2, klobuchar = V3, sanders = V4, steyer = V5, warren = V6, yang = V7) %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

# Tennessee (3 Mar 2020) ####
tn_dirichlet_params <- (state_averages_over_time %>%
                          filter(state == "Tennessee", as.numeric(today() - median_date) <= 7) %>%
                          mutate(age = as.numeric(today() - median_date)) %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          mutate(pct = pct / 100) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-median_date) %>%
                          na.omit() %>%
                          as.matrix() %>%
                          diri.est())$param

tn_state_percentages <- rdirichlet(n_sims, tn_dirichlet_params) %>%
  as.data.frame() %>%
  dplyr::select(biden = V1, buttigieg = V2, klobuchar = V3, sanders = V4, warren = V5, yang = V6) %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

# Texas (3 Mar 2020) ####
tx_dirichlet_params <- (state_averages_over_time %>%
                          filter(state == "Texas", as.numeric(today() - median_date) <= 7) %>%
                          mutate(age = as.numeric(today() - median_date)) %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          mutate(pct = pct / 100) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-median_date) %>%
                          na.omit() %>%
                          as.matrix() %>%
                          diri.est())$param

tx_state_percentages <- rdirichlet(n_sims, tx_dirichlet_params) %>%
  as.data.frame() %>%
  dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, steyer = V6, warren = V7, yang = V8) %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

# Virginia (3 Mar 2020) ####
va_dirichlet_params <- (state_averages_over_time %>%
                          filter(state == "Virginia", as.numeric(today() - median_date) <= 7) %>%
                          mutate(age = as.numeric(today() - median_date)) %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          mutate(pct = pct / 100) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-median_date) %>%
                          na.omit() %>%
                          as.matrix() %>%
                          diri.est())$param

va_state_percentages <- rdirichlet(n_sims, va_dirichlet_params) %>%
  as.data.frame() %>%
  dplyr::select(biden = V1, buttigieg = V2, klobuchar = V3, sanders = V4, warren = V5, yang = V6) %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

# Arkansas (3 Mar 2020) ####
ar_state_percentages <- ok_state_percentages %>%
  left_join(tx_state_percentages, by = c("sim_id", "candidate")) %>%
  left_join(tn_state_percentages, by = c("sim_id", "candidate")) %>%
  mutate(state_pct = case_when(candidate %in% c("biden", "buttigieg", "klobuchar", "sanders", "warren", "yang") ~ 
                                 (2*state_pct.x + 2*state_pct.y + state_pct) / 5,
                               candidate == "bloomberg" ~ state_pct.y,
                               candidate == "steyer" ~ (state_pct.x + state_pct.y) / 2)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct)) %>%
  ungroup() %>%
  dplyr::select(sim_id, candidate, state_pct)

# Utah (3 Mar 2020) ####
ut_state_percentages <- rdirichlet(n_sims, c(1, 1, 1, 1, 2, 1, 2, 1)) %>%
  as.data.frame() %>%
  dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, steyer = V6, warren = V7, yang = V8) %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

# Vermont (3 Mar 2020) ####
vt_state_percentages <- rdirichlet(n_sims, c(3, 1, 4, 2, 16, 1, 3, 1)) %>%
  as.data.frame() %>%
  dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, steyer = V6, warren = V7, yang = V8) %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

# Maine (8 Mar 2020) ####
me_dirichlet_params <- (state_averages_over_time %>%
                          filter(state == "Maine", as.numeric(today() - median_date) <= 7) %>%
                          mutate(age = as.numeric(today() - median_date)) %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          mutate(pct = pct / 100) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-median_date) %>%
                          na.omit() %>%
                          as.matrix() %>%
                          diri.est())$param

me_state_percentages <- rdirichlet(n_sims, me_dirichlet_params) %>%
  as.data.frame() %>%
  dplyr::select(biden = V1, buttigieg = V2, klobuchar = V3, sanders = V4, warren = V5, yang = V6) %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

# Michigan (10 Mar 2020) ####
mi_dirichlet_params <- (state_averages_over_time %>%
                          filter(state == "Michigan", as.numeric(today() - median_date) <= 7) %>%
                          mutate(age = as.numeric(today() - median_date)) %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          mutate(pct = pct / 100) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-median_date) %>%
                          na.omit() %>%
                          as.matrix() %>%
                          diri.est())$param

mi_state_percentages <- rdirichlet(n_sims, mi_dirichlet_params) %>%
  as.data.frame() %>%
  dplyr::select(biden = V1, buttigieg = V2, klobuchar = V3, sanders = V4, steyer = V5, warren = V6, yang = V7) %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

# Mississippi (10 Mar 2020) ####
ms_dirichlet_params <- (state_averages_over_time %>%
                          filter(state == "Mississippi", as.numeric(today() - median_date) <= 7) %>%
                          mutate(age = as.numeric(today() - median_date)) %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          mutate(pct = pct / 100) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-median_date) %>%
                          na.omit() %>%
                          as.matrix() %>%
                          diri.est())$param

ms_state_percentages <- rdirichlet(n_sims, ms_dirichlet_params) %>%
  as.data.frame() %>%
  dplyr::select(biden = V1, buttigieg = V2, klobuchar = V3, sanders = V4, warren = V5, yang = V6) %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

# Missouri (10 Mar 2020) ####
mo_dirichlet_params <- (state_averages_over_time %>%
                          filter(state == "Missouri", as.numeric(today() - median_date) <= 7) %>%
                          mutate(age = as.numeric(today() - median_date)) %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          mutate(pct = pct / 100) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-median_date) %>%
                          na.omit() %>%
                          as.matrix() %>%
                          diri.est())$param

mo_state_percentages <- rdirichlet(n_sims, mo_dirichlet_params) %>%
  as.data.frame() %>%
  dplyr::select(biden = V1, buttigieg = V2, klobuchar = V3, sanders = V4, warren = V5, yang = V6) %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

# Ohio (10 Mar 2020) ####
oh_dirichlet_params <- (state_averages_over_time %>%
                          filter(state == "Ohio", as.numeric(today() - median_date) <= 7) %>%
                          mutate(age = as.numeric(today() - median_date)) %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          mutate(pct = pct / 100) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-median_date) %>%
                          na.omit() %>%
                          as.matrix() %>%
                          diri.est())$param

oh_state_percentages <- rdirichlet(n_sims, oh_dirichlet_params) %>%
  as.data.frame() %>%
  dplyr::select(biden = V1, buttigieg = V2, klobuchar = V3, sanders = V4, steyer = V5, warren = V6, yang = V7) %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

# Washington (10 Mar 2020) ####
wa_dirichlet_params <- (state_averages_over_time %>%
                          filter(state == "Washington", as.numeric(today() - median_date) <= 7) %>%
                          mutate(age = as.numeric(today() - median_date)) %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          mutate(pct = pct / 100) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-median_date) %>%
                          na.omit() %>%
                          as.matrix() %>%
                          diri.est())$param

wa_state_percentages <- rdirichlet(n_sims, wa_dirichlet_params) %>%
  as.data.frame() %>%
  dplyr::select(biden = V1, buttigieg = V2, klobuchar = V3, sanders = V4, warren = V5, yang = V6) %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

# Idaho (10 Mar 2020) ####
id_state_percentages <- rdirichlet(n_sims, c(1, 1, 1, 1, 2, 1, 2, 1)) %>%
  as.data.frame() %>%
  dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, steyer = V6, warren = V7, yang = V8) %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

# North Dakota (10 Mar 2020) ####
nd_state_percentages <- mn_state_percentages %>%
  left_join(ia_state_percentages, by = c("sim_id", "candidate")) %>%
  mutate(state_pct = case_when(candidate != "bloomberg" ~ (state_pct.x + state_pct.y) / 2,
                               candidate == "bloomberg" ~ state_pct.x)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct)) %>%
  ungroup() %>%
  dplyr::select(sim_id, candidate, state_pct)

# Northern Mariana Islands (14 Mar 2020) ####
ni_state_percentages <- as_state_percentages

# Arizona (17 Mar 2020) ####
az_dirichlet_params <- (state_averages_over_time %>%
                           filter(state == "Arizona", as.numeric(today() - median_date) <= 7) %>%
                           mutate(age = as.numeric(today() - median_date)) %>%
                           dplyr::select(candidate, median_date, pct) %>%
                           na.omit() %>%
                           mutate(pct = pct / 100) %>%
                           spread(candidate, pct) %>%
                           dplyr::select(-median_date) %>%
                           na.omit() %>%
                           as.matrix() %>%
                           diri.est())$param

az_state_percentages <- rdirichlet(n_sims, az_dirichlet_params) %>%
  as.data.frame() %>%
  dplyr::select(biden = V1, buttigieg = V2, klobuchar = V3, sanders = V4, steyer = V5, warren = V6, yang = V7) %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

# Florida (17 Mar 2020) ####
fl_dirichlet_params <- (state_averages_over_time %>%
                          filter(state == "Florida", as.numeric(today() - median_date) <= 7) %>%
                          mutate(age = as.numeric(today() - median_date)) %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          mutate(pct = pct / 100) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-median_date) %>%
                          na.omit() %>%
                          as.matrix() %>%
                          diri.est())$param

fl_state_percentages <- rdirichlet(n_sims, fl_dirichlet_params) %>%
  as.data.frame() %>%
  dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, steyer = V6, warren = V7, yang = V8) %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

# Illinois (17 Mar 2020) ####
il_dirichlet_params <- (state_averages_over_time %>%
                          filter(state == "Illinois", as.numeric(today() - median_date) <= 7) %>%
                          mutate(age = as.numeric(today() - median_date)) %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          mutate(pct = pct / 100) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-median_date) %>%
                          na.omit() %>%
                          as.matrix() %>%
                          diri.est())$param

il_state_percentages <- rdirichlet(n_sims, il_dirichlet_params) %>%
  as.data.frame() %>%
  dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, steyer = V6, warren = V7, yang = V8) %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

# Georgia (24 Mar 2020) ####
ga_dirichlet_params <- (state_averages_over_time %>%
                          filter(state == "Georgia", as.numeric(today() - median_date) <= 7) %>%
                          mutate(age = as.numeric(today() - median_date)) %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          mutate(pct = pct / 100) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-median_date) %>%
                          na.omit() %>%
                          as.matrix() %>%
                          diri.est())$param

ga_state_percentages <- rdirichlet(n_sims, ga_dirichlet_params) %>%
  as.data.frame() %>%
  dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, steyer = V6, warren = V7, yang = V8) %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

# Puerto Rico (29 Mar 2020) ####
pr_state_percentages <- nv_state_percentages %>%
  left_join(fl_state_percentages, by = c("sim_id", "candidate")) %>%
  mutate(state_pct = case_when(candidate != "bloomberg" ~ (state_pct.x + state_pct.y) / 2,
                               candidate == "bloomberg" ~ state_pct.y)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct)) %>%
  ungroup() %>%
  dplyr::select(sim_id, candidate, state_pct)

# Alaska (4 Apr 2020) ####
ak_state_percentages <- id_state_percentages

# Hawaii (4 Apr 2020) ####
hi_state_percentages <- ca_state_percentages

# Louisiana (4 Apr 2020) ####
la_state_percentages <- ms_state_percentages %>%
  left_join(al_state_percentages, by = c("sim_id", "candidate")) %>%
  left_join(ar_state_percentages, by = c("sim_id", "candidate"))  %>%
  mutate(state_pct = case_when(candidate != "bloomberg" ~ (2*state_pct.x + state_pct.y + 2*state_pct) / 5,
                               candidate == "bloomberg" ~ state_pct)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct)) %>%
  ungroup() %>%
  dplyr::select(sim_id, candidate, state_pct)

# Wyoming (4 Apr 2020) ####
wy_state_percentages <- id_state_percentages %>%
  left_join(ut_state_percentages, by = c("sim_id", "candidate")) %>%
  left_join(co_state_percentages, by = c("sim_id", "candidate"))  %>%
  mutate(state_pct = case_when(candidate != "bloomberg" ~ (2*state_pct.x + 2*state_pct.y + state_pct) / 5,
                               candidate == "bloomberg" ~ (state_pct.x + state_pct.y) / 2)) %>%
  group_by(sim_id) %>%
  dplyr::mutate(state_pct = state_pct / sum(state_pct)) %>%
  ungroup() %>%
  dplyr::select(sim_id, candidate, state_pct)

# Wisconsin (7 Apr 2020) ####
wi_dirichlet_params <- (state_averages_over_time %>%
                          filter(state == "Wisconsin", as.numeric(today() - median_date) <= 7) %>%
                          mutate(age = as.numeric(today() - median_date)) %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          mutate(pct = pct / 100) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-median_date) %>%
                          na.omit() %>%
                          as.matrix() %>%
                          diri.est())$param

wi_state_percentages <- rdirichlet(n_sims, wi_dirichlet_params) %>%
  as.data.frame() %>%
  dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, steyer = V6, warren = V7, yang = V8) %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

# Connecticut (28 Apr 2020) ####
ct_dirichlet_params <- (state_averages_over_time %>%
                          filter(state == "Connecticut", as.numeric(today() - median_date) <= 7) %>%
                          mutate(age = as.numeric(today() - median_date)) %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          mutate(pct = pct / 100) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-median_date) %>%
                          na.omit() %>%
                          as.matrix() %>%
                          diri.est())$param

ct_state_percentages <- rdirichlet(n_sims, ct_dirichlet_params) %>%
  as.data.frame() %>%
  dplyr::select(biden = V1, buttigieg = V2, sanders = V3, warren = V4) %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

# Delaware (28 Apr 2020) ####
de_dirichlet_params <- (state_averages_over_time %>%
                          filter(state == "Delaware", as.numeric(today() - median_date) <= 7) %>%
                          mutate(age = as.numeric(today() - median_date)) %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          mutate(pct = pct / 100) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-median_date) %>%
                          na.omit() %>%
                          as.matrix() %>%
                          diri.est())$param

de_state_percentages <- rdirichlet(n_sims, de_dirichlet_params) %>%
  as.data.frame() %>%
  dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, steyer = V6, warren = V7, yang = V8) %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

# Maryland (28 Apr 2020) ####
md_dirichlet_params <- (state_averages_over_time %>%
                          filter(state == "Maryland", as.numeric(today() - median_date) <= 7) %>%
                          mutate(age = as.numeric(today() - median_date)) %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          mutate(pct = pct / 100) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-median_date) %>%
                          na.omit() %>%
                          as.matrix() %>%
                          diri.est())$param

md_state_percentages <- rdirichlet(n_sims, md_dirichlet_params) %>%
  as.data.frame() %>%
  dplyr::select(biden = V1, buttigieg = V2, klobuchar = V3, sanders = V4, steyer = V5, warren = V6, yang = V7) %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

# New York (28 Apr 2020) ####
ny_dirichlet_params <- (state_averages_over_time %>%
                          filter(state == "New York", as.numeric(today() - median_date) <= 7) %>%
                          mutate(age = as.numeric(today() - median_date)) %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          mutate(pct = pct / 100) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-median_date) %>%
                          na.omit() %>%
                          as.matrix() %>%
                          diri.est())$param

ny_state_percentages <- rdirichlet(n_sims, ny_dirichlet_params) %>%
  as.data.frame() %>%
  dplyr::select(biden = V1, buttigieg = V2, klobuchar = V3, sanders = V4, steyer = V5, warren = V6, yang = V7) %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

# Pennsylvania (28 Apr 2020) ####
pa_dirichlet_params <- (state_averages_over_time %>%
                          filter(state == "Pennsylvania", as.numeric(today() - median_date) <= 7) %>%
                          mutate(age = as.numeric(today() - median_date)) %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          mutate(pct = pct / 100) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-median_date) %>%
                          na.omit() %>%
                          as.matrix() %>%
                          diri.est())$param

pa_state_percentages <- rdirichlet(n_sims, pa_dirichlet_params) %>%
  as.data.frame() %>%
  dplyr::select(biden = V1, buttigieg = V2, klobuchar = V3, sanders = V4, steyer = V5, warren = V6, yang = V7) %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

# Rhode Island (28 Apr 2020) ####
ri_state_percentages <- ct_state_percentages %>%
  left_join(ma_state_percentages, by = c("sim_id", "candidate")) %>%
  mutate(state_pct = case_when(candidate %in% c("biden", "buttigieg", "sanders", "warren") ~ (state_pct.x + state_pct.y) / 2,
                               candidate %in% c("klobuchar", "steyer", "yang") ~ state_pct.y,
                               candidate == "bloomberg" ~ 0.02)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct)) %>%
  ungroup() %>%
  dplyr::select(sim_id, candidate, state_pct)

# Guam (2 May 2020) ####
gu_state_percentages <- as_state_percentages

# Kansas (2 May 2020) ####
ks_state_percentages <- mo_state_percentages %>%
  left_join(ok_state_percentages, by = c("sim_id", "candidate"))  %>%
  mutate(state_pct = case_when(candidate != "steyer" ~ (state_pct.x + state_pct.y) / 2,
                               candidate == "steyer" ~ state_pct.y)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct)) %>%
  ungroup() %>%
  dplyr::select(sim_id, candidate, state_pct)

# Indiana (5 May 2020) ####
in_dirichlet_params <- (state_averages_over_time %>%
                          filter(state == "Indiana", as.numeric(today() - median_date) <= 7) %>%
                          mutate(age = as.numeric(today() - median_date)) %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          mutate(pct = pct / 100) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-median_date) %>%
                          na.omit() %>%
                          as.matrix() %>%
                          diri.est())$param

in_state_percentages <- rdirichlet(n_sims, in_dirichlet_params) %>%
  as.data.frame() %>%
  dplyr::select(biden = V1, buttigieg = V2, klobuchar = V3, sanders = V4, warren = V5) %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

# Nebraska (12 May 2020) ####
ne_state_percentages <- ks_state_percentages %>%
  left_join(ia_state_percentages, by = c("sim_id", "candidate"))  %>%
  mutate(state_pct = case_when(!(candidate %in% c("bloomberg", "steyer")) ~ (state_pct.x + state_pct.y) / 2,
                               candidate == "steyer" ~ state_pct.y,
                               candidate == "bloomberg" ~ 0.01)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct)) %>%
  ungroup() %>%
  dplyr::select(sim_id, candidate, state_pct)

# West Virginia (12 May 2020) ####
wv_state_percentages <- wy_state_percentages %>%
  left_join(pa_state_percentages, by = c("sim_id", "candidate")) %>%
  mutate(state_pct = case_when(candidate != "bloomberg" ~ (state_pct.x + 2*state_pct.y) / 3,
                               candidate == "bloomberg" ~ state_pct.y)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct, na.rm = TRUE)) %>%
  ungroup() %>%
  dplyr::select(sim_id, candidate, state_pct)

# Kentucky (19 May 2020) ####
ky_state_percentages <- wv_state_percentages %>%
  left_join(tn_state_percentages, by = c("sim_id", "candidate")) %>%
  left_join(oh_state_percentages, by = c("sim_id", "candidate")) %>%
  mutate(state_pct = case_when(!(candidate %in% c("bloomberg", "steyer")) ~ (2*state_pct.x + 3*state_pct.y + state_pct) / 6,
                               candidate == "bloomberg" ~ state_pct.x,
                               candidate == "steyer" ~ (2*state_pct.x + state_pct) / 3)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct, na.rm = TRUE)) %>%
  ungroup() %>%
  dplyr::select(sim_id, candidate, state_pct)

# Oregon (19 May 2020) ####
or_dirichlet_params <- (state_averages_over_time %>%
                          filter(state == "Oregon", as.numeric(today() - median_date) <= 7) %>%
                          mutate(age = as.numeric(today() - median_date)) %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          mutate(pct = pct / 100) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-median_date) %>%
                          na.omit() %>%
                          as.matrix() %>%
                          diri.est())$param

or_state_percentages <- rdirichlet(n_sims, or_dirichlet_params) %>%
  as.data.frame() %>%
  dplyr::select(biden = V1, buttigieg = V2, sanders = V3, warren = V4, yang = V5) %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

# District of Columbia (2 June 2020) ####
dc_state_percentages <- rdirichlet(n_sims, c(7, 1, 1, 1, 3, 1, 2, 1)) %>%
  as.data.frame() %>%
  dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, steyer = V6, warren = V7, yang = V8) %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

# Montana (2 June 2020) ####
mt_dirichlet_params <- (state_averages_over_time %>%
                          filter(state == "Montana", as.numeric(today() - median_date) <= 7) %>%
                          mutate(age = as.numeric(today() - median_date)) %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          mutate(pct = pct / 100) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-median_date) %>%
                          na.omit() %>%
                          as.matrix() %>%
                          diri.est())$param

mt_state_percentages <- rdirichlet(n_sims, mt_dirichlet_params) %>%
  as.data.frame() %>%
  dplyr::select(biden = V1, buttigieg = V2, klobuchar = V3, sanders = V4, steyer = V5, warren = V6, yang = V7) %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

# New Jersey (2 June 2020) ####
nj_dirichlet_params <- (state_averages_over_time %>%
                          filter(state == "New Jersey", as.numeric(today() - median_date) <= 7) %>%
                          mutate(age = as.numeric(today() - median_date)) %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          mutate(pct = pct / 100) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-median_date) %>%
                          na.omit() %>%
                          as.matrix() %>%
                          diri.est())$param

nj_state_percentages <- rdirichlet(n_sims, nj_dirichlet_params) %>%
  as.data.frame() %>%
  dplyr::select(biden = V1, buttigieg = V2, klobuchar = V3, sanders = V4, steyer = V5, warren = V6, yang = V7) %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

# New Mexico (2 June 2020) ####
nm_dirichlet_params <- (state_averages_over_time %>%
                          filter(state == "New Mexico", as.numeric(today() - median_date) <= 7) %>%
                          mutate(age = as.numeric(today() - median_date)) %>%
                          dplyr::select(candidate, median_date, pct) %>%
                          na.omit() %>%
                          mutate(pct = pct / 100) %>%
                          spread(candidate, pct) %>%
                          dplyr::select(-median_date) %>%
                          na.omit() %>%
                          as.matrix() %>%
                          diri.est())$param

nm_state_percentages <- rdirichlet(n_sims, nm_dirichlet_params) %>%
  as.data.frame() %>%
  dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, steyer = V6, warren = V7, yang = V8) %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

# South Dakota (2 June 2020) ####
sd_state_percentages <- nd_state_percentages %>%
  left_join(ne_state_percentages, by = c("sim_id", "candidate")) %>%
  left_join(mn_state_percentages, by = c("sim_id", "candidate")) %>%
  left_join(mt_state_percentages, by = c("sim_id", "candidate")) %>%
  mutate(state_pct = case_when(candidate != "steyer" ~ (3*state_pct.x + 2*state_pct.y + state_pct.x.x + state_pct.y.y) / 7,
                               candidate == "steyer" ~ (3*state_pct.x + state_pct.x.x + state_pct.y.y) / 5)) %>%
  group_by(sim_id) %>%
  mutate(state_pct = state_pct / sum(state_pct, na.rm = TRUE)) %>%
  ungroup() %>%
  dplyr::select(sim_id, candidate, state_pct)

# U.S. Virgin Islands (6 June 2020) ####
vi_state_percentages <- rdirichlet(n_sims, c(10, 1, 1, 1, 2, 1, 2, 2)) %>%
  as.data.frame() %>%
  dplyr::select(biden = V1, bloomberg = V2, buttigieg = V3, klobuchar = V4, sanders = V5, steyer = V6, warren = V7, yang = V8) %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "state_pct")

#### SIMULATION LISTS ####
ia_sims <- nh_sims <- nv_sims <- sc_sims <- al_sims <- as_sims <- ar_sims <- ca_sims <- co_sims <- da_sims <- ma_sims <-
  mn_sims <- nc_sims <- ok_sims <- tn_sims <- tx_sims <- ut_sims <- vt_sims <- va_sims <- me_sims <- id_sims <- mi_sims <-
  ms_sims <- mo_sims <- nd_sims <- oh_sims <- wa_sims <- ni_sims <- az_sims <- fl_sims <- il_sims <- ga_sims <- pr_sims <-
  ak_sims <- hi_sims <- la_sims <- wy_sims <- wi_sims <- ct_sims <- de_sims <- md_sims <- ny_sims <- pa_sims <- ri_sims <-
  gu_sims <- ks_sims <- in_sims <- ne_sims <- wv_sims <- ky_sims <- or_sims <- dc_sims <- mt_sims <- nj_sims <- nm_sims <- 
  sd_sims <- vi_sims <- vector("list", n_sims)
