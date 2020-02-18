library(censusapi)
source("code/census-mining/api_key.R")
Sys.setenv(CENSUS_KEY = api_key)
Sys.getenv("CENSUS_KEY")

varstoget <- c("DP05_0028E", "DP02_0032PE", "DP05_0072PE", "DP05_0033PE", "DP05_0066PE", "DP02_0092PE", 
               "DP02_0067PE", "DP03_0062E", "DP03_0119PE", "DP05_0017E")

states2017 <- getCensus(name = "acs/acs1/profile", vintage = 2016, vars = varstoget, region = "state:*")
names(states2017) <- c("State", "Population", "PctMarried", "PctWhite", "PctBlack", "PctLatino", "PctImm", 
                       "PctColl", "MedInc", "Poverty", "MedAge")

states2017 <- states2017 %>%
  mutate(State = as.numeric(State))

statefips <- read_csv("data/auxiliary-demographics/state_fips.csv")
presresults <- read_csv("data/auxiliary-demographics/state_pres_results_2012_2016.csv")
statearea <- read_csv("data/auxiliary-demographics/state_area.csv")
statereligion <- read_csv("data/auxiliary-demographics/state_religion_2010.csv")

statefeatures <- statefips %>%
  merge(states2017, by.x = "FIPS", by.y = "State", all.x = TRUE) %>%
  merge(statearea, by = "State", all.x = TRUE) %>%
  mutate(Density = Population/Area) %>%
  merge(presresults, by = "State", all.x = TRUE) %>%
  merge(statereligion %>% 
          dplyr::select(State, PctReligious), 
        by = "State", all.x = TRUE) %>%
  as.tbl()

state_pca <- prcomp(~PctMarried+PctWhite+PctBlack+PctLatino+PctImm+PctColl+MedInc+Poverty+MedAge+Area+Density+PctReligious+Dem2012+Dem2016,
                    data = statefeatures, scale = TRUE)

state_dist <- dist(state_pca$x, upper = TRUE) %>% 
  as.matrix() %>%
  as.data.frame()
names(state_dist) <- statefeatures$State
state_dist <- bind_cols(state1 = statefeatures$State, state_dist) %>%
  melt(id.vars = "state1", variable.name = "state2", value.name = "distance") %>%
  as.tbl() %>%
  mutate(distance = case_when(distance > 0 ~ distance,
                              distance == 0 ~ Inf))

state_neighbors_with_polls <- state_dist %>%
  filter(state2 %in% state_averages_over_time$state) %>%
  mutate(inv_distance = 1/distance) %>%
  arrange(state1, distance) %>%
  group_by(state1) %>%
  mutate(rank = 1:n()) %>%
  filter(rank <= 5) %>%
  dplyr::select(state1, state2, inv_distance)
