source("code/library.R")
source("code/census-mining/api_key.R") 

census_api_key(api_key, install = TRUE, overwrite = TRUE)

# Inspect variables
vars_2017_acs5 <- load_variables(2017, "acs5", cache = TRUE)

# Congressional districts ####
#### Sex, race, education ####
cd_sre_2017 <- get_acs(geography = "congressional district",
                      variables = c(`Male / white` = "C15002H_002",
                                    `Male / black` = "C15002B_002",
                                    `Male / Latino` = "C15002I_002",
                                    `Female / white` = "C15002H_007",
                                    `Female / black` = "C15002B_007",
                                    `Female / Latina` = "C15002I_007",
                                    
                                    # Sex / race / education
                                    `Male / white / college` = "C15002H_006",
                                    `Male / black / college` = "C15002B_006",
                                    `Male / Latino / college` = "C15002I_006",
                                    `Female / white / college` = "C15002H_011",
                                    `Female / black / college` = "C15002B_011",
                                    `Female / Latina / college` = "C15002I_011")) %>%
  mutate(gender = case_when(grepl("Male", variable) ~ "Male",
                            grepl("Female", variable) ~ "Female"),
         race = case_when(grepl("white", variable, ignore.case = TRUE) ~ "White",
                          grepl("black", variable, ignore.case = TRUE) ~ "Black",
                          grepl("Latin", variable, ignore.case = TRUE) ~ "Latino",
                          !grepl("white|black|latin", variable, ignore.case = TRUE) ~ "Total"),
         education = case_when(grepl("college", variable) ~ "College only",
                               !grepl("college", variable) ~ "Total")) %>%
  group_by(id = GEOID, name = NAME, gender, race, education) %>%
  summarise(pop = sum(estimate)) %>%
  ungroup() %>%
  spread(education, pop) %>%
  mutate(`Less than college` = Total - `College only`) %>%
  dplyr::select(-Total) %>%
  melt(id.vars = c("id", "name", "gender", "race"), variable.name = "education", value.name = "pop") %>%
  filter(!grepl("Delaware|Montana|New Jersey|Texas|District of Columbia|Puerto Rico", name)) %>%
  mutate(state = str_split(name, ", ") %>% sapply(tail, 1)) %>%
  filter(!grepl("districts not defined", name, ignore.case = TRUE))

#### Sex, age, and education ####
cd_sae_2017 <- get_acs(geography = "congressional district",
                       variables = c(`Male / 18-24` = "B15001_003",
                                    `Male / 25-34` = "B15001_011",
                                    `Male / 35-44` = "B15001_019",
                                    `Male / 45-64` = "B15001_027",
                                    `Male / 65+`   = "B15001_035",
                                    `Female / 18-24` = "B15001_044",
                                    `Female / 25-34` = "B15001_052",
                                    `Female / 35-44` = "B15001_060",
                                    `Female / 45-64` = "B15001_068",
                                    `Female / 65+` = "B15001_076",
                                    
                                    # Sex / race / bachelor's
                                    `Male / 18-24 / bachelor's` = "B15001_009",
                                    `Male / 25-34 / bachelor's` = "B15001_017",
                                    `Male / 35-44 / bachelor's` = "B15001_025",
                                    `Male / 45-64 / bachelor's` = "B15001_033",
                                    `Male / 65+ / bachelor's`   = "B15001_041",
                                    `Female / 18-24 / bachelor's` = "B15001_050",
                                    `Female / 25-34 / bachelor's` = "B15001_058",
                                    `Female / 35-44 / bachelor's` = "B15001_066",
                                    `Female / 45-64 / bachelor's` = "B15001_074",
                                    `Female / 65+ / bachelor's` = "B15001_082",
                                    
                                    # Sex / race / grad
                                    `Male / 18-24 / graduate` = "B15001_010",
                                    `Male / 25-34 / graduate` = "B15001_018",
                                    `Male / 35-44 / graduate` = "B15001_026",
                                    `Male / 45-64 / graduate` = "B15001_034",
                                    `Male / 65+ / graduate`   = "B15001_042",
                                    `Female / 18-24 / graduate` = "B15001_051",
                                    `Female / 25-34 / graduate` = "B15001_059",
                                    `Female / 35-44 / graduate` = "B15001_067",
                                    `Female / 45-64 / graduate` = "B15001_075",
                                    `Female / 65+ / graduate` = "B15001_083")) %>%
  mutate(gender = case_when(grepl("Male", variable) ~ "Male",
                            grepl("Female", variable) ~ "Female"),
         education = case_when(grepl("bachelor|graduate", variable) ~ "College only",
                               !grepl("bachelor|graduate", variable) ~ "Total"),
         age = case_when(grepl("18|24", variable) ~ "18-24",
                         grepl("25|34", variable) ~ "25-34",
                         grepl("30|44", variable) ~ "35-44",
                         grepl("45|64", variable) ~ "45-64",
                         grepl("65|75|85", variable) ~ "65+",
                         !grepl("[[:digit:]]", variable) ~ "Total")) %>%
  group_by(id = GEOID, name = NAME, gender, age, education) %>%
  summarise(pop = sum(estimate)) %>%
  ungroup() %>%
  spread(education, pop) %>%
  mutate(`Less than college` = Total - `College only`) %>%
  dplyr::select(-Total) %>%
  melt(id.vars = c("id", "name", "gender", "age"), variable.name = "education", value.name = "pop") %>%
  filter(!grepl("Delaware|Montana|New Jersey|Texas|District of Columbia|Puerto Rico", name)) %>%
  mutate(state = str_split(name, ", ") %>% sapply(tail, 1)) %>%
  filter(!grepl("districts not defined", name, ignore.case = TRUE))

#### Sex, race, age ####
cd_sra_2017 <- get_acs(geography = "congressional district",
                      variables = c(`Male / white / 18-19` = "B01001H_007",
                                    `Male / white / 20-24` = "B01001H_008",
                                    `Male / white / 25-29` = "B01001H_009",
                                    `Male / white / 30-34` = "B01001H_010",
                                    `Male / white / 35-44` = "B01001H_011",
                                    `Male / white / 45-54` = "B01001H_012",
                                    `Male / white / 55-64` = "B01001H_013",
                                    `Male / white / 65-74` = "B01001H_014",
                                    `Male / white / 75-84` = "B01001H_015",
                                    `Male / white / 85+`   = "B01001H_016",
                                    `Female / white / 18-19` = "B01001H_022",
                                    `Female / white / 20-24` = "B01001H_023",
                                    `Female / white / 25-29` = "B01001H_024",
                                    `Female / white / 30-34` = "B01001H_025",
                                    `Female / white / 35-44` = "B01001H_026",
                                    `Female / white / 45-54` = "B01001H_027",
                                    `Female / white / 55-64` = "B01001H_028",
                                    `Female / white / 65-74` = "B01001H_029",
                                    `Female / white / 75-84` = "B01001H_030",
                                    `Female / white / 85+`   = "B01001H_031",
                                    `Male / black / 18-19` = "B01001B_007",
                                    `Male / black / 20-24` = "B01001B_008",
                                    `Male / black / 25-29` = "B01001B_009",
                                    `Male / black / 30-34` = "B01001B_010",
                                    `Male / black / 35-44` = "B01001B_011",
                                    `Male / black / 45-54` = "B01001B_012",
                                    `Male / black / 55-64` = "B01001B_013",
                                    `Male / black / 65-74` = "B01001B_014",
                                    `Male / black / 75-84` = "B01001B_015",
                                    `Male / black / 85+`   = "B01001B_016",
                                    `Female / black / 18-19` = "B01001B_022",
                                    `Female / black / 20-24` = "B01001B_023",
                                    `Female / black / 25-29` = "B01001B_024",
                                    `Female / black / 30-34` = "B01001B_025",
                                    `Female / black / 35-44` = "B01001B_026",
                                    `Female / black / 45-54` = "B01001B_027",
                                    `Female / black / 55-64` = "B01001B_028",
                                    `Female / black / 65-74` = "B01001B_029",
                                    `Female / black / 75-84` = "B01001B_030",
                                    `Female / black / 85+`   = "B01001B_031",
                                    `Male / Latino / 18-19` = "B01001I_007",
                                    `Male / Latino / 20-24` = "B01001I_008",
                                    `Male / Latino / 25-29` = "B01001I_009",
                                    `Male / Latino / 30-34` = "B01001I_010",
                                    `Male / Latino / 35-44` = "B01001I_011",
                                    `Male / Latino / 45-54` = "B01001I_012",
                                    `Male / Latino / 55-64` = "B01001I_013",
                                    `Male / Latino / 65-74` = "B01001I_014",
                                    `Male / Latino / 75-84` = "B01001I_015",
                                    `Male / Latino / 85+`   = "B01001I_016",
                                    `Female / Latina / 18-19` = "B01001I_022",
                                    `Female / Latina / 20-24` = "B01001I_023",
                                    `Female / Latina / 25-29` = "B01001I_024",
                                    `Female / Latina / 30-34` = "B01001I_025",
                                    `Female / Latina / 35-44` = "B01001I_026",
                                    `Female / Latina / 45-54` = "B01001I_027",
                                    `Female / Latina / 55-64` = "B01001I_028",
                                    `Female / Latina / 65-74` = "B01001I_029",
                                    `Female / Latina / 75-84` = "B01001I_030",
                                    `Female / Latina / 85+`   = "B01001I_031")) %>%
  mutate(gender = case_when(grepl("Male", variable) ~ "Male",
                            grepl("Female", variable) ~ "Female"),
         race = case_when(grepl("white", variable, ignore.case = TRUE) ~ "White",
                          grepl("black", variable, ignore.case = TRUE) ~ "Black",
                          grepl("Latin", variable, ignore.case = TRUE) ~ "Latino",
                          !grepl("white|black|latin", variable, ignore.case = TRUE) ~ "Total"),
         age = case_when(grepl("18|24", variable) ~ "18-24",
                         grepl("25|34", variable) ~ "25-34",
                         grepl("35|44", variable) ~ "35-44",
                         grepl("45|64", variable) ~ "45-64",
                         grepl("65|75|85", variable) ~ "65+",
                         !grepl("[[:digit:]]", variable) ~ "Total")) %>%
  group_by(id = GEOID, name = NAME, gender, race, age) %>% 
  summarise(pop = sum(estimate)) %>%
  ungroup() %>%
  filter(!grepl("Delaware|Montana|New Jersey|Texas|District of Columbia|Puerto Rico", name)) %>%
  mutate(state = str_split(name, ", ") %>% sapply(tail, 1)) %>%
  filter(!grepl("districts not defined", name, ignore.case = TRUE))

# New Jersey Assembly districts ####
#### Sex, race, education ####
nj_dd_sre_2017 <- get_acs(geography = "state legislative district (lower chamber)",
                          variables = c(`Male / white` = "C15002H_002",
                                        `Male / black` = "C15002B_002",
                                        `Male / Latino` = "C15002I_002",
                                        `Female / white` = "C15002H_007",
                                        `Female / black` = "C15002B_007",
                                        `Female / Latina` = "C15002I_007",
                                        
                                        # Sex / race / education
                                        `Male / white / college` = "C15002H_006",
                                        `Male / black / college` = "C15002B_006",
                                        `Male / Latino / college` = "C15002I_006",
                                        `Female / white / college` = "C15002H_011",
                                        `Female / black / college` = "C15002B_011",
                                        `Female / Latina / college` = "C15002I_011"),
                          state = "NJ") %>%
  mutate(gender = case_when(grepl("Male", variable) ~ "Male",
                            grepl("Female", variable) ~ "Female"),
         race = case_when(grepl("white", variable, ignore.case = TRUE) ~ "White",
                          grepl("black", variable, ignore.case = TRUE) ~ "Black",
                          grepl("Latin", variable, ignore.case = TRUE) ~ "Latino",
                          !grepl("white|black|latin", variable, ignore.case = TRUE) ~ "Total"),
         education = case_when(grepl("college", variable) ~ "College only",
                               !grepl("college", variable) ~ "Total")) %>%
  group_by(id = GEOID, ld = NAME, gender, race, education) %>%
  summarise(pop = sum(estimate)) %>%
  ungroup() %>%
  spread(education, pop) %>%
  mutate(`Less than college` = Total - `College only`) %>%
  dplyr::select(-Total) %>%
  melt(id.vars = c("id", "ld", "gender", "race"), variable.name = "education", value.name = "pop") %>%
  mutate(name = case_when(grepl("District 1 |District 3 ", ld) ~ "DD1",
                          grepl("District 2 |District 9 ", ld) ~ "DD2",
                          grepl("District 4 |District 5 ", ld) ~ "DD3",
                          grepl("District 6 |District 7 ", ld) ~ "DD4",
                          grepl("District 8 |District 12", ld) ~ "DD5",
                          grepl("District 10|District 30", ld) ~ "DD6",
                          grepl("District 11|District 13", ld) ~ "DD7",
                          grepl("District 14|District 15", ld) ~ "DD8",
                          grepl("District 16|District 17", ld) ~ "DD9",
                          grepl("District 18|District 19", ld) ~ "DD10",
                          grepl("District 23|District 24", ld) ~ "DD11",
                          grepl("District 21|District 27", ld) ~ "DD12",
                          grepl("District 20|District 22", ld) ~ "DD13",
                          grepl("District 25|District 26", ld) ~ "DD14",
                          grepl("District 28|District 29", ld) ~ "DD15",
                          grepl("District 31|District 33", ld) ~ "DD16",
                          grepl("District 32|District 36", ld) ~ "DD17",
                          grepl("District 34|District 35", ld) ~ "DD18",
                          grepl("District 37|District 38", ld) ~ "DD19",
                          grepl("District 39|District 40", ld) ~ "DD20"),
         state = "New Jersey") %>%
  dplyr::select(state, id, name, gender, race, education, pop)

#### Sex, age, and education ####
nj_dd_sae_2017 <- get_acs(geography = "state legislative district (upper chamber)",
                          variables = c(`Male / 18-24` = "B15001_003",
                                        `Male / 25-34` = "B15001_011",
                                        `Male / 35-44` = "B15001_019",
                                        `Male / 45-64` = "B15001_027",
                                        `Male / 65+`   = "B15001_035",
                                        `Female / 18-24` = "B15001_044",
                                        `Female / 25-34` = "B15001_052",
                                        `Female / 35-44` = "B15001_060",
                                        `Female / 45-64` = "B15001_068",
                                        `Female / 65+` = "B15001_076",
                                        
                                        # Sex / race / bachelor's
                                        `Male / 18-24 / bachelor's` = "B15001_009",
                                        `Male / 25-34 / bachelor's` = "B15001_017",
                                        `Male / 35-44 / bachelor's` = "B15001_025",
                                        `Male / 45-64 / bachelor's` = "B15001_033",
                                        `Male / 65+ / bachelor's`   = "B15001_041",
                                        `Female / 18-24 / bachelor's` = "B15001_050",
                                        `Female / 25-34 / bachelor's` = "B15001_058",
                                        `Female / 35-44 / bachelor's` = "B15001_066",
                                        `Female / 45-64 / bachelor's` = "B15001_074",
                                        `Female / 65+ / bachelor's` = "B15001_082",
                                        
                                        # Sex / race / grad
                                        `Male / 18-24 / graduate` = "B15001_010",
                                        `Male / 25-34 / graduate` = "B15001_018",
                                        `Male / 35-44 / graduate` = "B15001_026",
                                        `Male / 45-64 / graduate` = "B15001_034",
                                        `Male / 65+ / graduate`   = "B15001_042",
                                        `Female / 18-24 / graduate` = "B15001_051",
                                        `Female / 25-34 / graduate` = "B15001_059",
                                        `Female / 35-44 / graduate` = "B15001_067",
                                        `Female / 45-64 / graduate` = "B15001_075",
                                        `Female / 65+ / graduate` = "B15001_083"),
                          state = "NJ") %>%
  mutate(gender = case_when(grepl("Male", variable) ~ "Male",
                            grepl("Female", variable) ~ "Female"),
         education = case_when(grepl("bachelor|graduate", variable) ~ "College only",
                               !grepl("bachelor|graduate", variable) ~ "Total"),
         age = case_when(grepl("18|24", variable) ~ "18-24",
                         grepl("25|34", variable) ~ "25-34",
                         grepl("35|44", variable) ~ "35-44",
                         grepl("45|64", variable) ~ "45-64",
                         grepl("65|75|85", variable) ~ "65+",
                         !grepl("[[:digit:]]", variable) ~ "Total")) %>%
  group_by(id = GEOID, ld = NAME, gender, age, education) %>%
  summarise(pop = sum(estimate)) %>%
  ungroup() %>%
  spread(education, pop) %>%
  mutate(`Less than college` = Total - `College only`) %>%
  dplyr::select(-Total) %>%
  melt(id.vars = c("id", "ld", "gender", "age"), variable.name = "education", value.name = "pop") %>%
  mutate(name = case_when(grepl("District 1 |District 3 ", ld) ~ "DD1",
                          grepl("District 2 |District 9 ", ld) ~ "DD2",
                          grepl("District 4 |District 5 ", ld) ~ "DD3",
                          grepl("District 6 |District 7 ", ld) ~ "DD4",
                          grepl("District 8 |District 12", ld) ~ "DD5",
                          grepl("District 10|District 30", ld) ~ "DD6",
                          grepl("District 11|District 13", ld) ~ "DD7",
                          grepl("District 14|District 15", ld) ~ "DD8",
                          grepl("District 16|District 17", ld) ~ "DD9",
                          grepl("District 18|District 19", ld) ~ "DD10",
                          grepl("District 23|District 24", ld) ~ "DD11",
                          grepl("District 21|District 27", ld) ~ "DD12",
                          grepl("District 20|District 22", ld) ~ "DD13",
                          grepl("District 25|District 26", ld) ~ "DD14",
                          grepl("District 28|District 29", ld) ~ "DD15",
                          grepl("District 31|District 33", ld) ~ "DD16",
                          grepl("District 32|District 36", ld) ~ "DD17",
                          grepl("District 34|District 35", ld) ~ "DD18",
                          grepl("District 37|District 38", ld) ~ "DD19",
                          grepl("District 39|District 40", ld) ~ "DD20"),
         state = "New Jersey") %>%
  dplyr::select(state, id, name, gender, age, education, pop)

#### Sex, race, age ####
nj_dd_sra_2017 <- get_acs(geography = "state legislative district (upper chamber)",
                          variables = c(`Male / white / 18-19` = "B01001H_007",
                                        `Male / white / 20-24` = "B01001H_008",
                                        `Male / white / 25-29` = "B01001H_009",
                                        `Male / white / 30-34` = "B01001H_010",
                                        `Male / white / 35-44` = "B01001H_011",
                                        `Male / white / 45-54` = "B01001H_012",
                                        `Male / white / 55-64` = "B01001H_013",
                                        `Male / white / 65-74` = "B01001H_014",
                                        `Male / white / 75-84` = "B01001H_015",
                                        `Male / white / 85+`   = "B01001H_016",
                                        `Female / white / 18-19` = "B01001H_022",
                                        `Female / white / 20-24` = "B01001H_023",
                                        `Female / white / 25-29` = "B01001H_024",
                                        `Female / white / 30-34` = "B01001H_025",
                                        `Female / white / 35-44` = "B01001H_026",
                                        `Female / white / 45-54` = "B01001H_027",
                                        `Female / white / 55-64` = "B01001H_028",
                                        `Female / white / 65-74` = "B01001H_029",
                                        `Female / white / 75-84` = "B01001H_030",
                                        `Female / white / 85+`   = "B01001H_031",
                                        `Male / black / 18-19` = "B01001B_007",
                                        `Male / black / 20-24` = "B01001B_008",
                                        `Male / black / 25-29` = "B01001B_009",
                                        `Male / black / 30-34` = "B01001B_010",
                                        `Male / black / 35-44` = "B01001B_011",
                                        `Male / black / 45-54` = "B01001B_012",
                                        `Male / black / 55-64` = "B01001B_013",
                                        `Male / black / 65-74` = "B01001B_014",
                                        `Male / black / 75-84` = "B01001B_015",
                                        `Male / black / 85+`   = "B01001B_016",
                                        `Female / black / 18-19` = "B01001B_022",
                                        `Female / black / 20-24` = "B01001B_023",
                                        `Female / black / 25-29` = "B01001B_024",
                                        `Female / black / 30-34` = "B01001B_025",
                                        `Female / black / 35-44` = "B01001B_026",
                                        `Female / black / 45-54` = "B01001B_027",
                                        `Female / black / 55-64` = "B01001B_028",
                                        `Female / black / 65-74` = "B01001B_029",
                                        `Female / black / 75-84` = "B01001B_030",
                                        `Female / black / 85+`   = "B01001B_031",
                                        `Male / Latino / 18-19` = "B01001I_007",
                                        `Male / Latino / 20-24` = "B01001I_008",
                                        `Male / Latino / 25-29` = "B01001I_009",
                                        `Male / Latino / 30-34` = "B01001I_010",
                                        `Male / Latino / 35-44` = "B01001I_011",
                                        `Male / Latino / 45-54` = "B01001I_012",
                                        `Male / Latino / 55-64` = "B01001I_013",
                                        `Male / Latino / 65-74` = "B01001I_014",
                                        `Male / Latino / 75-84` = "B01001I_015",
                                        `Male / Latino / 85+`   = "B01001I_016",
                                        `Female / Latina / 18-19` = "B01001I_022",
                                        `Female / Latina / 20-24` = "B01001I_023",
                                        `Female / Latina / 25-29` = "B01001I_024",
                                        `Female / Latina / 30-34` = "B01001I_025",
                                        `Female / Latina / 35-44` = "B01001I_026",
                                        `Female / Latina / 45-54` = "B01001I_027",
                                        `Female / Latina / 55-64` = "B01001I_028",
                                        `Female / Latina / 65-74` = "B01001I_029",
                                        `Female / Latina / 75-84` = "B01001I_030",
                                        `Female / Latina / 85+`   = "B01001I_031"),
                          state = "NJ") %>%
  mutate(gender = case_when(grepl("Male", variable) ~ "Male",
                            grepl("Female", variable) ~ "Female"),
         race = case_when(grepl("white", variable, ignore.case = TRUE) ~ "White",
                          grepl("black", variable, ignore.case = TRUE) ~ "Black",
                          grepl("Latin", variable, ignore.case = TRUE) ~ "Latino",
                          !grepl("white|black|latin", variable, ignore.case = TRUE) ~ "Total"),
         age = case_when(grepl("18|24", variable) ~ "18-24",
                         grepl("25|34", variable) ~ "25-34",
                         grepl("35|44", variable) ~ "35-44",
                         grepl("45|64", variable) ~ "45-64",
                         grepl("65|75|85", variable) ~ "65+",
                         !grepl("[[:digit:]]", variable) ~ "Total"),
         ld = NAME,
         name = case_when(grepl("District 1 |District 3 ", ld) ~ "DD1",
                          grepl("District 2 |District 9 ", ld) ~ "DD2",
                          grepl("District 4 |District 5 ", ld) ~ "DD3",
                          grepl("District 6 |District 7 ", ld) ~ "DD4",
                          grepl("District 8 |District 12", ld) ~ "DD5",
                          grepl("District 10|District 30", ld) ~ "DD6",
                          grepl("District 11|District 13", ld) ~ "DD7",
                          grepl("District 14|District 15", ld) ~ "DD8",
                          grepl("District 16|District 17", ld) ~ "DD9",
                          grepl("District 18|District 19", ld) ~ "DD10",
                          grepl("District 23|District 24", ld) ~ "DD11",
                          grepl("District 21|District 27", ld) ~ "DD12",
                          grepl("District 20|District 22", ld) ~ "DD13",
                          grepl("District 25|District 26", ld) ~ "DD14",
                          grepl("District 28|District 29", ld) ~ "DD15",
                          grepl("District 31|District 33", ld) ~ "DD16",
                          grepl("District 32|District 36", ld) ~ "DD17",
                          grepl("District 34|District 35", ld) ~ "DD18",
                          grepl("District 37|District 38", ld) ~ "DD19",
                          grepl("District 39|District 40", ld) ~ "DD20"),
         state = "New Jersey") %>%
  group_by(state, id = GEOID, name, gender, race, age) %>% 
  summarise(pop = sum(estimate)) %>%
  ungroup()

# Texas Senate districts ####
#### Sex, race, education ####
tx_sd_sre_2017 <- get_acs(geography = "state legislative district (upper chamber)",
                          variables = c(`Male / white` = "C15002H_002",
                                        `Male / black` = "C15002B_002",
                                        `Male / Latino` = "C15002I_002",
                                        `Female / white` = "C15002H_007",
                                        `Female / black` = "C15002B_007",
                                        `Female / Latina` = "C15002I_007",
                                        
                                        # Sex / race / education
                                        `Male / white / college` = "C15002H_006",
                                        `Male / black / college` = "C15002B_006",
                                        `Male / Latino / college` = "C15002I_006",
                                        `Female / white / college` = "C15002H_011",
                                        `Female / black / college` = "C15002B_011",
                                        `Female / Latina / college` = "C15002I_011"),
                          state = "TX") %>%
  mutate(gender = case_when(grepl("Male", variable) ~ "Male",
                            grepl("Female", variable) ~ "Female"),
         race = case_when(grepl("white", variable, ignore.case = TRUE) ~ "White",
                          grepl("black", variable, ignore.case = TRUE) ~ "Black",
                          grepl("Latin", variable, ignore.case = TRUE) ~ "Latino",
                          !grepl("white|black|latin", variable, ignore.case = TRUE) ~ "Total"),
         education = case_when(grepl("college", variable) ~ "College only",
                               !grepl("college", variable) ~ "Total")) %>%
  group_by(id = GEOID, name = NAME, gender, race, education) %>%
  summarise(pop = sum(estimate)) %>%
  ungroup() %>%
  spread(education, pop) %>%
  mutate(`Less than college` = Total - `College only`) %>%
  dplyr::select(-Total) %>%
  melt(id.vars = c("id", "name", "gender", "race"), variable.name = "education", value.name = "pop") %>%
  mutate(state = "Texas")

#### Sex, age, and education ####
tx_sd_sae_2017 <- get_acs(geography = "state legislative district (upper chamber)",
                          variables = c(`Male / 18-24` = "B15001_003",
                                        `Male / 25-34` = "B15001_011",
                                        `Male / 35-44` = "B15001_019",
                                        `Male / 45-64` = "B15001_027",
                                        `Male / 65+`   = "B15001_035",
                                        `Female / 18-24` = "B15001_044",
                                        `Female / 25-34` = "B15001_052",
                                        `Female / 35-44` = "B15001_060",
                                        `Female / 45-64` = "B15001_068",
                                        `Female / 65+` = "B15001_076",
                                        
                                        # Sex / race / bachelor's
                                        `Male / 18-24 / bachelor's` = "B15001_009",
                                        `Male / 25-34 / bachelor's` = "B15001_017",
                                        `Male / 35-44 / bachelor's` = "B15001_025",
                                        `Male / 45-64 / bachelor's` = "B15001_033",
                                        `Male / 65+ / bachelor's`   = "B15001_041",
                                        `Female / 18-24 / bachelor's` = "B15001_050",
                                        `Female / 25-34 / bachelor's` = "B15001_058",
                                        `Female / 35-44 / bachelor's` = "B15001_066",
                                        `Female / 45-64 / bachelor's` = "B15001_074",
                                        `Female / 65+ / bachelor's` = "B15001_082",
                                        
                                        # Sex / race / grad
                                        `Male / 18-24 / graduate` = "B15001_010",
                                        `Male / 25-34 / graduate` = "B15001_018",
                                        `Male / 35-44 / graduate` = "B15001_026",
                                        `Male / 45-64 / graduate` = "B15001_034",
                                        `Male / 65+ / graduate`   = "B15001_042",
                                        `Female / 18-24 / graduate` = "B15001_051",
                                        `Female / 25-34 / graduate` = "B15001_059",
                                        `Female / 35-44 / graduate` = "B15001_067",
                                        `Female / 45-64 / graduate` = "B15001_075",
                                        `Female / 65+ / graduate` = "B15001_083"),
                          state = "TX") %>%
  mutate(gender = case_when(grepl("Male", variable) ~ "Male",
                            grepl("Female", variable) ~ "Female"),
         education = case_when(grepl("bachelor|graduate", variable) ~ "College only",
                               !grepl("bachelor|graduate", variable) ~ "Total"),
         age = case_when(grepl("18|24", variable) ~ "18-24",
                         grepl("25|34", variable) ~ "25-34",
                         grepl("35|44", variable) ~ "35-44",
                         grepl("45|64", variable) ~ "45-64",
                         grepl("65|75|85", variable) ~ "65+",
                         !grepl("[[:digit:]]", variable) ~ "Total")) %>%
  group_by(id = GEOID, name = NAME, gender, age, education) %>%
  summarise(pop = sum(estimate)) %>%
  ungroup() %>%
  spread(education, pop) %>%
  mutate(`Less than college` = Total - `College only`) %>%
  dplyr::select(-Total) %>%
  melt(id.vars = c("id", "name", "gender", "age"), variable.name = "education", value.name = "pop") %>%
  mutate(state = "Texas")

#### Sex, race, age ####
tx_sd_sra_2017 <- get_acs(geography = "state legislative district (upper chamber)",
                          variables = c(`Male / white / 18-19` = "B01001H_007",
                                        `Male / white / 20-24` = "B01001H_008",
                                        `Male / white / 25-29` = "B01001H_009",
                                        `Male / white / 30-34` = "B01001H_010",
                                        `Male / white / 35-44` = "B01001H_011",
                                        `Male / white / 45-54` = "B01001H_012",
                                        `Male / white / 55-64` = "B01001H_013",
                                        `Male / white / 65-74` = "B01001H_014",
                                        `Male / white / 75-84` = "B01001H_015",
                                        `Male / white / 85+`   = "B01001H_016",
                                        `Female / white / 18-19` = "B01001H_022",
                                        `Female / white / 20-24` = "B01001H_023",
                                        `Female / white / 25-29` = "B01001H_024",
                                        `Female / white / 30-34` = "B01001H_025",
                                        `Female / white / 35-44` = "B01001H_026",
                                        `Female / white / 45-54` = "B01001H_027",
                                        `Female / white / 55-64` = "B01001H_028",
                                        `Female / white / 65-74` = "B01001H_029",
                                        `Female / white / 75-84` = "B01001H_030",
                                        `Female / white / 85+`   = "B01001H_031",
                                        `Male / black / 18-19` = "B01001B_007",
                                        `Male / black / 20-24` = "B01001B_008",
                                        `Male / black / 25-29` = "B01001B_009",
                                        `Male / black / 30-34` = "B01001B_010",
                                        `Male / black / 35-44` = "B01001B_011",
                                        `Male / black / 45-54` = "B01001B_012",
                                        `Male / black / 55-64` = "B01001B_013",
                                        `Male / black / 65-74` = "B01001B_014",
                                        `Male / black / 75-84` = "B01001B_015",
                                        `Male / black / 85+`   = "B01001B_016",
                                        `Female / black / 18-19` = "B01001B_022",
                                        `Female / black / 20-24` = "B01001B_023",
                                        `Female / black / 25-29` = "B01001B_024",
                                        `Female / black / 30-34` = "B01001B_025",
                                        `Female / black / 35-44` = "B01001B_026",
                                        `Female / black / 45-54` = "B01001B_027",
                                        `Female / black / 55-64` = "B01001B_028",
                                        `Female / black / 65-74` = "B01001B_029",
                                        `Female / black / 75-84` = "B01001B_030",
                                        `Female / black / 85+`   = "B01001B_031",
                                        `Male / Latino / 18-19` = "B01001I_007",
                                        `Male / Latino / 20-24` = "B01001I_008",
                                        `Male / Latino / 25-29` = "B01001I_009",
                                        `Male / Latino / 30-34` = "B01001I_010",
                                        `Male / Latino / 35-44` = "B01001I_011",
                                        `Male / Latino / 45-54` = "B01001I_012",
                                        `Male / Latino / 55-64` = "B01001I_013",
                                        `Male / Latino / 65-74` = "B01001I_014",
                                        `Male / Latino / 75-84` = "B01001I_015",
                                        `Male / Latino / 85+`   = "B01001I_016",
                                        `Female / Latina / 18-19` = "B01001I_022",
                                        `Female / Latina / 20-24` = "B01001I_023",
                                        `Female / Latina / 25-29` = "B01001I_024",
                                        `Female / Latina / 30-34` = "B01001I_025",
                                        `Female / Latina / 35-44` = "B01001I_026",
                                        `Female / Latina / 45-54` = "B01001I_027",
                                        `Female / Latina / 55-64` = "B01001I_028",
                                        `Female / Latina / 65-74` = "B01001I_029",
                                        `Female / Latina / 75-84` = "B01001I_030",
                                        `Female / Latina / 85+`   = "B01001I_031"),
                          state = "TX") %>%
  mutate(gender = case_when(grepl("Male", variable) ~ "Male",
                            grepl("Female", variable) ~ "Female"),
         race = case_when(grepl("white", variable, ignore.case = TRUE) ~ "White",
                          grepl("black", variable, ignore.case = TRUE) ~ "Black",
                          grepl("Latin", variable, ignore.case = TRUE) ~ "Latino",
                          !grepl("white|black|latin", variable, ignore.case = TRUE) ~ "Total"),
         age = case_when(grepl("18|24", variable) ~ "18-24",
                         grepl("25|34", variable) ~ "25-34",
                         grepl("35|44", variable) ~ "35-44",
                         grepl("45|64", variable) ~ "45-64",
                         grepl("65|75|85", variable) ~ "65+",
                         !grepl("[[:digit:]]", variable) ~ "Total"),
         state = "Texas") %>%
  group_by(state, id = GEOID, name = NAME, gender, race, age) %>% 
  summarise(pop = sum(estimate)) %>%
  ungroup()

# Delaware counties ####
#### Sex, race, and education ####
de_county_sre_2017 <- get_acs(geography = "county",
                              variables = c(`Male / white` = "C15002H_002",
                                            `Male / black` = "C15002B_002",
                                            `Male / Latino` = "C15002I_002",
                                            `Female / white` = "C15002H_007",
                                            `Female / black` = "C15002B_007",
                                            `Female / Latina` = "C15002I_007",
                                            
                                            # Sex / race / education
                                            `Male / white / college` = "C15002H_006",
                                            `Male / black / college` = "C15002B_006",
                                            `Male / Latino / college` = "C15002I_006",
                                            `Female / white / college` = "C15002H_011",
                                            `Female / black / college` = "C15002B_011",
                                            `Female / Latina / college` = "C15002I_011"),
                              state = "DE") %>%
  mutate(gender = case_when(grepl("Male", variable) ~ "Male",
                            grepl("Female", variable) ~ "Female"),
         race = case_when(grepl("white", variable, ignore.case = TRUE) ~ "White",
                          grepl("black", variable, ignore.case = TRUE) ~ "Black",
                          grepl("Latin", variable, ignore.case = TRUE) ~ "Latino",
                          !grepl("white|black|latin", variable, ignore.case = TRUE) ~ "Total"),
         education = case_when(grepl("college", variable) ~ "College only",
                               !grepl("college", variable) ~ "Total")) %>%
  group_by(id = GEOID, name = NAME, gender, race, education) %>%
  summarise(pop = sum(estimate)) %>%
  ungroup() %>%
  spread(education, pop) %>%
  mutate(`Less than college` = Total - `College only`) %>%
  dplyr::select(-Total) %>%
  melt(id.vars = c("id", "name", "gender", "race"), variable.name = "education", value.name = "pop") %>%
  mutate(state = "Delaware")

#### Sex, age, and education ####
de_county_sae_2017 <- get_acs(geography = "county",
                              variables = c(`Male / 18-24` = "B15001_003",
                                            `Male / 25-34` = "B15001_011",
                                            `Male / 35-44` = "B15001_019",
                                            `Male / 45-64` = "B15001_027",
                                            `Male / 65+`   = "B15001_035",
                                            `Female / 18-24` = "B15001_044",
                                            `Female / 25-34` = "B15001_052",
                                            `Female / 35-44` = "B15001_060",
                                            `Female / 45-64` = "B15001_068",
                                            `Female / 65+` = "B15001_076",
                                            
                                            # Sex / race / bachelor's
                                            `Male / 18-24 / bachelor's` = "B15001_009",
                                            `Male / 25-34 / bachelor's` = "B15001_017",
                                            `Male / 35-44 / bachelor's` = "B15001_025",
                                            `Male / 45-64 / bachelor's` = "B15001_033",
                                            `Male / 65+ / bachelor's`   = "B15001_041",
                                            `Female / 18-24 / bachelor's` = "B15001_050",
                                            `Female / 25-34 / bachelor's` = "B15001_058",
                                            `Female / 35-44 / bachelor's` = "B15001_066",
                                            `Female / 45-64 / bachelor's` = "B15001_074",
                                            `Female / 65+ / bachelor's` = "B15001_082",
                                            
                                            # Sex / race / grad
                                            `Male / 18-24 / graduate` = "B15001_010",
                                            `Male / 25-34 / graduate` = "B15001_018",
                                            `Male / 35-44 / graduate` = "B15001_026",
                                            `Male / 45-64 / graduate` = "B15001_034",
                                            `Male / 65+ / graduate`   = "B15001_042",
                                            `Female / 18-24 / graduate` = "B15001_051",
                                            `Female / 25-34 / graduate` = "B15001_059",
                                            `Female / 35-44 / graduate` = "B15001_067",
                                            `Female / 45-64 / graduate` = "B15001_075",
                                            `Female / 65+ / graduate` = "B15001_083"),
                              state = "DE") %>%
  mutate(gender = case_when(grepl("Male", variable) ~ "Male",
                            grepl("Female", variable) ~ "Female"),
         education = case_when(grepl("bachelor|graduate", variable) ~ "College only",
                               !grepl("bachelor|graduate", variable) ~ "Total"),
         age = case_when(grepl("18|24", variable) ~ "18-24",
                         grepl("25|34", variable) ~ "25-34",
                         grepl("35|44", variable) ~ "35-44",
                         grepl("45|64", variable) ~ "45-64",
                         grepl("65|75|85", variable) ~ "65+",
                         !grepl("[[:digit:]]", variable) ~ "Total")) %>%
  group_by(id = GEOID, name = NAME, gender, age, education) %>%
  summarise(pop = sum(estimate)) %>%
  ungroup() %>%
  spread(education, pop) %>%
  mutate(`Less than college` = Total - `College only`) %>%
  dplyr::select(-Total) %>%
  melt(id.vars = c("id", "name", "gender", "age"), variable.name = "education", value.name = "pop") %>%
  mutate(state = "Delaware")

#### Sex, race, age ####
de_county_sra_2017 <- get_acs(geography = "county",
                              variables = c(`Male / white / 18-19` = "B01001H_007",
                                            `Male / white / 20-24` = "B01001H_008",
                                            `Male / white / 25-29` = "B01001H_009",
                                            `Male / white / 30-34` = "B01001H_010",
                                            `Male / white / 35-44` = "B01001H_011",
                                            `Male / white / 45-54` = "B01001H_012",
                                            `Male / white / 55-64` = "B01001H_013",
                                            `Male / white / 65-74` = "B01001H_014",
                                            `Male / white / 75-84` = "B01001H_015",
                                            `Male / white / 85+`   = "B01001H_016",
                                            `Female / white / 18-19` = "B01001H_022",
                                            `Female / white / 20-24` = "B01001H_023",
                                            `Female / white / 25-29` = "B01001H_024",
                                            `Female / white / 30-34` = "B01001H_025",
                                            `Female / white / 35-44` = "B01001H_026",
                                            `Female / white / 45-54` = "B01001H_027",
                                            `Female / white / 55-64` = "B01001H_028",
                                            `Female / white / 65-74` = "B01001H_029",
                                            `Female / white / 75-84` = "B01001H_030",
                                            `Female / white / 85+`   = "B01001H_031",
                                            `Male / black / 18-19` = "B01001B_007",
                                            `Male / black / 20-24` = "B01001B_008",
                                            `Male / black / 25-29` = "B01001B_009",
                                            `Male / black / 30-34` = "B01001B_010",
                                            `Male / black / 35-44` = "B01001B_011",
                                            `Male / black / 45-54` = "B01001B_012",
                                            `Male / black / 55-64` = "B01001B_013",
                                            `Male / black / 65-74` = "B01001B_014",
                                            `Male / black / 75-84` = "B01001B_015",
                                            `Male / black / 85+`   = "B01001B_016",
                                            `Female / black / 18-19` = "B01001B_022",
                                            `Female / black / 20-24` = "B01001B_023",
                                            `Female / black / 25-29` = "B01001B_024",
                                            `Female / black / 30-34` = "B01001B_025",
                                            `Female / black / 35-44` = "B01001B_026",
                                            `Female / black / 45-54` = "B01001B_027",
                                            `Female / black / 55-64` = "B01001B_028",
                                            `Female / black / 65-74` = "B01001B_029",
                                            `Female / black / 75-84` = "B01001B_030",
                                            `Female / black / 85+`   = "B01001B_031",
                                            `Male / Latino / 18-19` = "B01001I_007",
                                            `Male / Latino / 20-24` = "B01001I_008",
                                            `Male / Latino / 25-29` = "B01001I_009",
                                            `Male / Latino / 30-34` = "B01001I_010",
                                            `Male / Latino / 35-44` = "B01001I_011",
                                            `Male / Latino / 45-54` = "B01001I_012",
                                            `Male / Latino / 55-64` = "B01001I_013",
                                            `Male / Latino / 65-74` = "B01001I_014",
                                            `Male / Latino / 75-84` = "B01001I_015",
                                            `Male / Latino / 85+`   = "B01001I_016",
                                            `Female / Latina / 18-19` = "B01001I_022",
                                            `Female / Latina / 20-24` = "B01001I_023",
                                            `Female / Latina / 25-29` = "B01001I_024",
                                            `Female / Latina / 30-34` = "B01001I_025",
                                            `Female / Latina / 35-44` = "B01001I_026",
                                            `Female / Latina / 45-54` = "B01001I_027",
                                            `Female / Latina / 55-64` = "B01001I_028",
                                            `Female / Latina / 65-74` = "B01001I_029",
                                            `Female / Latina / 75-84` = "B01001I_030",
                                            `Female / Latina / 85+`   = "B01001I_031"),
                              state = "DE") %>%
  mutate(gender = case_when(grepl("Male", variable) ~ "Male",
                            grepl("Female", variable) ~ "Female"),
         race = case_when(grepl("white", variable, ignore.case = TRUE) ~ "White",
                          grepl("black", variable, ignore.case = TRUE) ~ "Black",
                          grepl("Latin", variable, ignore.case = TRUE) ~ "Latino",
                          !grepl("white|black|latin", variable, ignore.case = TRUE) ~ "Total"),
         age = case_when(grepl("18|24", variable) ~ "18-24",
                         grepl("25|34", variable) ~ "25-34",
                         grepl("35|44", variable) ~ "35-44",
                         grepl("45|64", variable) ~ "45-64",
                         grepl("65|75|85", variable) ~ "65+",
                         !grepl("[[:digit:]]", variable) ~ "Total"),
         state = "Delaware") %>%
  group_by(state, id = GEOID, name = NAME, gender, race, age) %>% 
  summarise(pop = sum(estimate)) %>%
  ungroup()

# Montana (east/west) ####
#### Sex, race, and education ####
mt_region_sre_2017 <- get_acs(geography = "county",
                              variables = c(`Male / white` = "C15002H_002",
                                            `Male / black` = "C15002B_002",
                                            `Male / Latino` = "C15002I_002",
                                            `Female / white` = "C15002H_007",
                                            `Female / black` = "C15002B_007",
                                            `Female / Latina` = "C15002I_007",
                                            
                                            # Sex / race / education
                                            `Male / white / college` = "C15002H_006",
                                            `Male / black / college` = "C15002B_006",
                                            `Male / Latino / college` = "C15002I_006",
                                            `Female / white / college` = "C15002H_011",
                                            `Female / black / college` = "C15002B_011",
                                            `Female / Latina / college` = "C15002I_011"),
                              state = "MT") %>%
  mutate(gender = case_when(grepl("Male", variable) ~ "Male",
                            grepl("Female", variable) ~ "Female"),
         race = case_when(grepl("white", variable, ignore.case = TRUE) ~ "White",
                          grepl("black", variable, ignore.case = TRUE) ~ "Black",
                          grepl("Latin", variable, ignore.case = TRUE) ~ "Latino",
                          !grepl("white|black|latin", variable, ignore.case = TRUE) ~ "Total"),
         education = case_when(grepl("college", variable) ~ "College only",
                               !grepl("college", variable) ~ "Total")) %>%
  group_by(id = GEOID, name = NAME, gender, race, education) %>%
  summarise(pop = sum(estimate)) %>%
  ungroup() %>%
  spread(education, pop) %>%
  mutate(`Less than college` = Total - `College only`) %>%
  dplyr::select(-Total) %>%
  melt(id.vars = c("id", "name", "gender", "race"), variable.name = "education", value.name = "pop") %>%
  mutate(region = case_when(grepl("Beav|Broa|Deer|Flat|Glac|Gran|Jeff|Lake|Lewi|Linc|Madi|Mine|Miss|Pond|Powe|Rava|Sand|Silv", name) ~ "Western",
                            !grepl("Beav|Broa|Deer|Flat|Glac|Gran|Jeff|Lake|Lewi|Linc|Madi|Mine|Miss|Pond|Powe|Rava|Sand|Silv", name) ~ "Eastern"),
         state = "Montana") %>%
  group_by(state, name = region, gender, race, education) %>%
  summarise(pop = sum(pop)) %>%
  ungroup()

#### Sex, race, and education ####
mt_region_sae_2017 <- get_acs(geography = "county",
                              variables = c(`Male / 18-24` = "B15001_003",
                                            `Male / 25-34` = "B15001_011",
                                            `Male / 35-44` = "B15001_019",
                                            `Male / 45-64` = "B15001_027",
                                            `Male / 65+`   = "B15001_035",
                                            `Female / 18-24` = "B15001_044",
                                            `Female / 25-34` = "B15001_052",
                                            `Female / 35-44` = "B15001_060",
                                            `Female / 45-64` = "B15001_068",
                                            `Female / 65+` = "B15001_076",
                                            
                                            # Sex / race / bachelor's
                                            `Male / 18-24 / bachelor's` = "B15001_009",
                                            `Male / 25-34 / bachelor's` = "B15001_017",
                                            `Male / 35-44 / bachelor's` = "B15001_025",
                                            `Male / 45-64 / bachelor's` = "B15001_033",
                                            `Male / 65+ / bachelor's`   = "B15001_041",
                                            `Female / 18-24 / bachelor's` = "B15001_050",
                                            `Female / 25-34 / bachelor's` = "B15001_058",
                                            `Female / 35-44 / bachelor's` = "B15001_066",
                                            `Female / 45-64 / bachelor's` = "B15001_074",
                                            `Female / 65+ / bachelor's` = "B15001_082",
                                            
                                            # Sex / race / grad
                                            `Male / 18-24 / graduate` = "B15001_010",
                                            `Male / 25-34 / graduate` = "B15001_018",
                                            `Male / 35-44 / graduate` = "B15001_026",
                                            `Male / 45-64 / graduate` = "B15001_034",
                                            `Male / 65+ / graduate`   = "B15001_042",
                                            `Female / 18-24 / graduate` = "B15001_051",
                                            `Female / 25-34 / graduate` = "B15001_059",
                                            `Female / 35-44 / graduate` = "B15001_067",
                                            `Female / 45-64 / graduate` = "B15001_075",
                                            `Female / 65+ / graduate` = "B15001_083"),
                              state = "MT") %>%
  mutate(gender = case_when(grepl("Male", variable) ~ "Male",
                            grepl("Female", variable) ~ "Female"),
         education = case_when(grepl("bachelor|graduate", variable) ~ "College only",
                               !grepl("bachelor|graduate", variable) ~ "Total"),
         age = case_when(grepl("18|24", variable) ~ "18-24",
                         grepl("25|34", variable) ~ "25-34",
                         grepl("35|44", variable) ~ "35-44",
                         grepl("45|64", variable) ~ "45-64",
                         grepl("65|75|85", variable) ~ "65+",
                         !grepl("[[:digit:]]", variable) ~ "Total")) %>%
  group_by(id = GEOID, name = NAME, gender, age, education) %>%
  summarise(pop = sum(estimate)) %>%
  ungroup() %>%
  spread(education, pop) %>%
  mutate(`Less than college` = Total - `College only`) %>%
  dplyr::select(-Total) %>%
  melt(id.vars = c("id", "name", "gender", "age"), variable.name = "education", value.name = "pop") %>%
  mutate(region = case_when(grepl("Beav|Broa|Deer|Flat|Glac|Gran|Jeff|Lake|Lewi|Linc|Madi|Mine|Miss|Pond|Powe|Rava|Sand|Silv", name) ~ "Western",
                            !grepl("Beav|Broa|Deer|Flat|Glac|Gran|Jeff|Lake|Lewi|Linc|Madi|Mine|Miss|Pond|Powe|Rava|Sand|Silv", name) ~ "Eastern"),
         state = "Montana") %>%
  group_by(state, name = region, gender, age, education) %>%
  summarise(pop = sum(pop)) %>%
  ungroup()

#### Sex, race, age ####
mt_region_sra_2017 <- get_acs(geography = "county",
                              variables = c(`Male / white / 18-19` = "B01001H_007",
                                            `Male / white / 20-24` = "B01001H_008",
                                            `Male / white / 25-29` = "B01001H_009",
                                            `Male / white / 30-34` = "B01001H_010",
                                            `Male / white / 35-44` = "B01001H_011",
                                            `Male / white / 45-54` = "B01001H_012",
                                            `Male / white / 55-64` = "B01001H_013",
                                            `Male / white / 65-74` = "B01001H_014",
                                            `Male / white / 75-84` = "B01001H_015",
                                            `Male / white / 85+`   = "B01001H_016",
                                            `Female / white / 18-19` = "B01001H_022",
                                            `Female / white / 20-24` = "B01001H_023",
                                            `Female / white / 25-29` = "B01001H_024",
                                            `Female / white / 30-34` = "B01001H_025",
                                            `Female / white / 35-44` = "B01001H_026",
                                            `Female / white / 45-54` = "B01001H_027",
                                            `Female / white / 55-64` = "B01001H_028",
                                            `Female / white / 65-74` = "B01001H_029",
                                            `Female / white / 75-84` = "B01001H_030",
                                            `Female / white / 85+`   = "B01001H_031",
                                            `Male / black / 18-19` = "B01001B_007",
                                            `Male / black / 20-24` = "B01001B_008",
                                            `Male / black / 25-29` = "B01001B_009",
                                            `Male / black / 30-34` = "B01001B_010",
                                            `Male / black / 35-44` = "B01001B_011",
                                            `Male / black / 45-54` = "B01001B_012",
                                            `Male / black / 55-64` = "B01001B_013",
                                            `Male / black / 65-74` = "B01001B_014",
                                            `Male / black / 75-84` = "B01001B_015",
                                            `Male / black / 85+`   = "B01001B_016",
                                            `Female / black / 18-19` = "B01001B_022",
                                            `Female / black / 20-24` = "B01001B_023",
                                            `Female / black / 25-29` = "B01001B_024",
                                            `Female / black / 30-34` = "B01001B_025",
                                            `Female / black / 35-44` = "B01001B_026",
                                            `Female / black / 45-54` = "B01001B_027",
                                            `Female / black / 55-64` = "B01001B_028",
                                            `Female / black / 65-74` = "B01001B_029",
                                            `Female / black / 75-84` = "B01001B_030",
                                            `Female / black / 85+`   = "B01001B_031",
                                            `Male / Latino / 18-19` = "B01001I_007",
                                            `Male / Latino / 20-24` = "B01001I_008",
                                            `Male / Latino / 25-29` = "B01001I_009",
                                            `Male / Latino / 30-34` = "B01001I_010",
                                            `Male / Latino / 35-44` = "B01001I_011",
                                            `Male / Latino / 45-54` = "B01001I_012",
                                            `Male / Latino / 55-64` = "B01001I_013",
                                            `Male / Latino / 65-74` = "B01001I_014",
                                            `Male / Latino / 75-84` = "B01001I_015",
                                            `Male / Latino / 85+`   = "B01001I_016",
                                            `Female / Latina / 18-19` = "B01001I_022",
                                            `Female / Latina / 20-24` = "B01001I_023",
                                            `Female / Latina / 25-29` = "B01001I_024",
                                            `Female / Latina / 30-34` = "B01001I_025",
                                            `Female / Latina / 35-44` = "B01001I_026",
                                            `Female / Latina / 45-54` = "B01001I_027",
                                            `Female / Latina / 55-64` = "B01001I_028",
                                            `Female / Latina / 65-74` = "B01001I_029",
                                            `Female / Latina / 75-84` = "B01001I_030",
                                            `Female / Latina / 85+`   = "B01001I_031"),
                              state = "MT") %>%
  mutate(gender = case_when(grepl("Male", variable) ~ "Male",
                            grepl("Female", variable) ~ "Female"),
         race = case_when(grepl("white", variable, ignore.case = TRUE) ~ "White",
                          grepl("black", variable, ignore.case = TRUE) ~ "Black",
                          grepl("Latin", variable, ignore.case = TRUE) ~ "Latino",
                          !grepl("white|black|latin", variable, ignore.case = TRUE) ~ "Total"),
         age = case_when(grepl("18|24", variable) ~ "18-24",
                         grepl("25|34", variable) ~ "25-34",
                         grepl("35|44", variable) ~ "35-44",
                         grepl("45|64", variable) ~ "45-64",
                         grepl("65|75|85", variable) ~ "65+",
                         !grepl("[[:digit:]]", variable) ~ "Total")) %>%
  group_by(id = GEOID, county = NAME, gender, race, age) %>% 
  summarise(pop = sum(estimate)) %>%
  mutate(region = case_when(grepl("Beav|Broa|Deer|Flat|Glac|Gran|Jeff|Lake|Lewi|Linc|Madi|Mine|Miss|Pond|Powe|Rava|Sand|Silv", county) ~ "Western",
                            !grepl("Beav|Broa|Deer|Flat|Glac|Gran|Jeff|Lake|Lewi|Linc|Madi|Mine|Miss|Pond|Powe|Rava|Sand|Silv", county) ~ "Eastern"),
         state = "Montana") %>%
  group_by(state, name = region, gender, race, age) %>%
  summarise(pop = sum(pop)) %>%
  ungroup()

# Puerto Rico Senate districts ####
#### Sex, race, and education ####
pr_sd_sre_2017 <- get_acs(geography = "state legislative district (upper chamber)",
                          variables = c(`Male / white` = "C15002H_002",
                                        `Male / black` = "C15002B_002",
                                        `Male / Latino` = "C15002I_002",
                                        `Female / white` = "C15002H_007",
                                        `Female / black` = "C15002B_007",
                                        `Female / Latina` = "C15002I_007",
                                        
                                        # Sex / race / education
                                        `Male / white / college` = "C15002H_006",
                                        `Male / black / college` = "C15002B_006",
                                        `Male / Latino / college` = "C15002I_006",
                                        `Female / white / college` = "C15002H_011",
                                        `Female / black / college` = "C15002B_011",
                                        `Female / Latina / college` = "C15002I_011"),
                          state = "PR") %>%
  mutate(gender = case_when(grepl("Male", variable) ~ "Male",
                            grepl("Female", variable) ~ "Female"),
         race = case_when(grepl("white", variable, ignore.case = TRUE) ~ "White",
                          grepl("black", variable, ignore.case = TRUE) ~ "Black",
                          grepl("Latin", variable, ignore.case = TRUE) ~ "Latino",
                          !grepl("white|black|latin", variable, ignore.case = TRUE) ~ "Total"),
         education = case_when(grepl("college", variable) ~ "College only",
                               !grepl("college", variable) ~ "Total")) %>%
  group_by(id = GEOID, name = NAME, gender, race, education) %>%
  summarise(pop = sum(estimate)) %>%
  ungroup() %>%
  spread(education, pop) %>%
  mutate(`Less than college` = Total - `College only`) %>%
  dplyr::select(-Total) %>%
  melt(id.vars = c("id", "name", "gender", "race"), variable.name = "education", value.name = "pop") %>%
  mutate(state = "Puerto Rico")

#### Sex, age, and education ####
pr_sd_sae_2017 <- get_acs(geography = "state legislative district (upper chamber)",
                          variables = c(`Male / 18-24` = "B15001_003",
                                        `Male / 25-34` = "B15001_011",
                                        `Male / 35-44` = "B15001_019",
                                        `Male / 45-64` = "B15001_027",
                                        `Male / 65+`   = "B15001_035",
                                        `Female / 18-24` = "B15001_044",
                                        `Female / 25-34` = "B15001_052",
                                        `Female / 35-44` = "B15001_060",
                                        `Female / 45-64` = "B15001_068",
                                        `Female / 65+` = "B15001_076",
                                        
                                        # Sex / race / bachelor's
                                        `Male / 18-24 / bachelor's` = "B15001_009",
                                        `Male / 25-34 / bachelor's` = "B15001_017",
                                        `Male / 35-44 / bachelor's` = "B15001_025",
                                        `Male / 45-64 / bachelor's` = "B15001_033",
                                        `Male / 65+ / bachelor's`   = "B15001_041",
                                        `Female / 18-24 / bachelor's` = "B15001_050",
                                        `Female / 25-34 / bachelor's` = "B15001_058",
                                        `Female / 35-44 / bachelor's` = "B15001_066",
                                        `Female / 45-64 / bachelor's` = "B15001_074",
                                        `Female / 65+ / bachelor's` = "B15001_082",
                                        
                                        # Sex / race / grad
                                        `Male / 18-24 / graduate` = "B15001_010",
                                        `Male / 25-34 / graduate` = "B15001_018",
                                        `Male / 35-44 / graduate` = "B15001_026",
                                        `Male / 45-64 / graduate` = "B15001_034",
                                        `Male / 65+ / graduate`   = "B15001_042",
                                        `Female / 18-24 / graduate` = "B15001_051",
                                        `Female / 25-34 / graduate` = "B15001_059",
                                        `Female / 35-44 / graduate` = "B15001_067",
                                        `Female / 45-64 / graduate` = "B15001_075",
                                        `Female / 65+ / graduate` = "B15001_083"),
                          state = "PR") %>%
  mutate(gender = case_when(grepl("Male", variable) ~ "Male",
                            grepl("Female", variable) ~ "Female"),
         education = case_when(grepl("bachelor|graduate", variable) ~ "College only",
                               !grepl("bachelor|graduate", variable) ~ "Total"),
         age = case_when(grepl("18|24", variable) ~ "18-24",
                         grepl("25|34", variable) ~ "25-34",
                         grepl("35|44", variable) ~ "35-44",
                         grepl("45|64", variable) ~ "45-64",
                         grepl("65|75|85", variable) ~ "65+",
                         !grepl("[[:digit:]]", variable) ~ "Total")) %>%
  group_by(id = GEOID, name = NAME, gender, age, education) %>%
  summarise(pop = sum(estimate)) %>%
  ungroup() %>%
  spread(education, pop) %>%
  mutate(`Less than college` = Total - `College only`) %>%
  dplyr::select(-Total) %>%
  melt(id.vars = c("id", "name", "gender", "age"), variable.name = "education", value.name = "pop") %>%
  mutate(state = "Puerto Rico")

#### Sex, race, age ####
pr_sd_sra_2017 <- get_acs(geography = "state legislative district (upper chamber)",
                          variables = c(`Male / white / 18-19` = "B01001H_007",
                                        `Male / white / 20-24` = "B01001H_008",
                                        `Male / white / 25-29` = "B01001H_009",
                                        `Male / white / 30-34` = "B01001H_010",
                                        `Male / white / 35-44` = "B01001H_011",
                                        `Male / white / 45-54` = "B01001H_012",
                                        `Male / white / 55-64` = "B01001H_013",
                                        `Male / white / 65-74` = "B01001H_014",
                                        `Male / white / 75-84` = "B01001H_015",
                                        `Male / white / 85+`   = "B01001H_016",
                                        `Female / white / 18-19` = "B01001H_022",
                                        `Female / white / 20-24` = "B01001H_023",
                                        `Female / white / 25-29` = "B01001H_024",
                                        `Female / white / 30-34` = "B01001H_025",
                                        `Female / white / 35-44` = "B01001H_026",
                                        `Female / white / 45-54` = "B01001H_027",
                                        `Female / white / 55-64` = "B01001H_028",
                                        `Female / white / 65-74` = "B01001H_029",
                                        `Female / white / 75-84` = "B01001H_030",
                                        `Female / white / 85+`   = "B01001H_031",
                                        `Male / black / 18-19` = "B01001B_007",
                                        `Male / black / 20-24` = "B01001B_008",
                                        `Male / black / 25-29` = "B01001B_009",
                                        `Male / black / 30-34` = "B01001B_010",
                                        `Male / black / 35-44` = "B01001B_011",
                                        `Male / black / 45-54` = "B01001B_012",
                                        `Male / black / 55-64` = "B01001B_013",
                                        `Male / black / 65-74` = "B01001B_014",
                                        `Male / black / 75-84` = "B01001B_015",
                                        `Male / black / 85+`   = "B01001B_016",
                                        `Female / black / 18-19` = "B01001B_022",
                                        `Female / black / 20-24` = "B01001B_023",
                                        `Female / black / 25-29` = "B01001B_024",
                                        `Female / black / 30-34` = "B01001B_025",
                                        `Female / black / 35-44` = "B01001B_026",
                                        `Female / black / 45-54` = "B01001B_027",
                                        `Female / black / 55-64` = "B01001B_028",
                                        `Female / black / 65-74` = "B01001B_029",
                                        `Female / black / 75-84` = "B01001B_030",
                                        `Female / black / 85+`   = "B01001B_031",
                                        `Male / Latino / 18-19` = "B01001I_007",
                                        `Male / Latino / 20-24` = "B01001I_008",
                                        `Male / Latino / 25-29` = "B01001I_009",
                                        `Male / Latino / 30-34` = "B01001I_010",
                                        `Male / Latino / 35-44` = "B01001I_011",
                                        `Male / Latino / 45-54` = "B01001I_012",
                                        `Male / Latino / 55-64` = "B01001I_013",
                                        `Male / Latino / 65-74` = "B01001I_014",
                                        `Male / Latino / 75-84` = "B01001I_015",
                                        `Male / Latino / 85+`   = "B01001I_016",
                                        `Female / Latina / 18-19` = "B01001I_022",
                                        `Female / Latina / 20-24` = "B01001I_023",
                                        `Female / Latina / 25-29` = "B01001I_024",
                                        `Female / Latina / 30-34` = "B01001I_025",
                                        `Female / Latina / 35-44` = "B01001I_026",
                                        `Female / Latina / 45-54` = "B01001I_027",
                                        `Female / Latina / 55-64` = "B01001I_028",
                                        `Female / Latina / 65-74` = "B01001I_029",
                                        `Female / Latina / 75-84` = "B01001I_030",
                                        `Female / Latina / 85+`   = "B01001I_031"),
                          state = "PR") %>%
  mutate(gender = case_when(grepl("Male", variable) ~ "Male",
                            grepl("Female", variable) ~ "Female"),
         race = case_when(grepl("white", variable, ignore.case = TRUE) ~ "White",
                          grepl("black", variable, ignore.case = TRUE) ~ "Black",
                          grepl("Latin", variable, ignore.case = TRUE) ~ "Latino",
                          !grepl("white|black|latin", variable, ignore.case = TRUE) ~ "Total"),
         age = case_when(grepl("18|24", variable) ~ "18-24",
                         grepl("25|34", variable) ~ "25-34",
                         grepl("35|44", variable) ~ "35-44",
                         grepl("45|64", variable) ~ "45-64",
                         grepl("65|75|85", variable) ~ "65+",
                         !grepl("[[:digit:]]", variable) ~ "Total"),
         state = "Puerto Rico") %>%
  group_by(state, id = GEOID, name = NAME, gender, race, age) %>% 
  summarise(pop = sum(estimate)) %>%
  ungroup()

# District of Columbia ####
#### Sex, race, and education ####
dc_md_sre_2017 <- get_acs(geography = "state legislative district (upper chamber)",
                          variables = c(`Male / white` = "C15002H_002",
                                        `Male / black` = "C15002B_002",
                                        `Male / Latino` = "C15002I_002",
                                        `Female / white` = "C15002H_007",
                                        `Female / black` = "C15002B_007",
                                        `Female / Latina` = "C15002I_007",
                                        
                                        # Sex / race / education
                                        `Male / white / college` = "C15002H_006",
                                        `Male / black / college` = "C15002B_006",
                                        `Male / Latino / college` = "C15002I_006",
                                        `Female / white / college` = "C15002H_011",
                                        `Female / black / college` = "C15002B_011",
                                        `Female / Latina / college` = "C15002I_011"),
                          state = "DC") %>%
  mutate(gender = case_when(grepl("Male", variable) ~ "Male",
                            grepl("Female", variable) ~ "Female"),
         race = case_when(grepl("white", variable, ignore.case = TRUE) ~ "White",
                          grepl("black", variable, ignore.case = TRUE) ~ "Black",
                          grepl("Latin", variable, ignore.case = TRUE) ~ "Latino",
                          !grepl("white|black|latin", variable, ignore.case = TRUE) ~ "Total"),
         education = case_when(grepl("college", variable) ~ "College only",
                               !grepl("college", variable) ~ "Total")) %>%
  group_by(id = GEOID, ward = NAME, gender, race, education) %>%
  summarise(pop = sum(estimate)) %>%
  ungroup() %>%
  spread(education, pop) %>%
  mutate(`Less than college` = Total - `College only`) %>%
  dplyr::select(-Total) %>%
  melt(id.vars = c("id", "ward", "gender", "race"), variable.name = "education", value.name = "pop") %>%
  mutate(state = "District of Columbia",
         name = case_when(grepl("Ward 1|Ward 2|Ward 6|Ward 8", ward) ~ "MD1",
                          grepl("Ward 3|Ward 4|Ward 5|Ward 7", ward) ~ "MD2")) %>%
  dplyr::select(state, name, gender, race, education, pop)

#### Sex, age, and education ####
dc_md_sae_2017 <- get_acs(geography = "state legislative district (upper chamber)",
                          variables = c(`Male / 18-24` = "B15001_003",
                                        `Male / 25-34` = "B15001_011",
                                        `Male / 35-44` = "B15001_019",
                                        `Male / 45-64` = "B15001_027",
                                        `Male / 65+`   = "B15001_035",
                                        `Female / 18-24` = "B15001_044",
                                        `Female / 25-34` = "B15001_052",
                                        `Female / 35-44` = "B15001_060",
                                        `Female / 45-64` = "B15001_068",
                                        `Female / 65+` = "B15001_076",
                                        
                                        # Sex / race / bachelor's
                                        `Male / 18-24 / bachelor's` = "B15001_009",
                                        `Male / 25-34 / bachelor's` = "B15001_017",
                                        `Male / 35-44 / bachelor's` = "B15001_025",
                                        `Male / 45-64 / bachelor's` = "B15001_033",
                                        `Male / 65+ / bachelor's`   = "B15001_041",
                                        `Female / 18-24 / bachelor's` = "B15001_050",
                                        `Female / 25-34 / bachelor's` = "B15001_058",
                                        `Female / 35-44 / bachelor's` = "B15001_066",
                                        `Female / 45-64 / bachelor's` = "B15001_074",
                                        `Female / 65+ / bachelor's` = "B15001_082",
                                        
                                        # Sex / race / grad
                                        `Male / 18-24 / graduate` = "B15001_010",
                                        `Male / 25-34 / graduate` = "B15001_018",
                                        `Male / 35-44 / graduate` = "B15001_026",
                                        `Male / 45-64 / graduate` = "B15001_034",
                                        `Male / 65+ / graduate`   = "B15001_042",
                                        `Female / 18-24 / graduate` = "B15001_051",
                                        `Female / 25-34 / graduate` = "B15001_059",
                                        `Female / 35-44 / graduate` = "B15001_067",
                                        `Female / 45-64 / graduate` = "B15001_075",
                                        `Female / 65+ / graduate` = "B15001_083"),
                          state = "DC") %>%
  mutate(gender = case_when(grepl("Male", variable) ~ "Male",
                            grepl("Female", variable) ~ "Female"),
         education = case_when(grepl("bachelor|graduate", variable) ~ "College only",
                               !grepl("bachelor|graduate", variable) ~ "Total"),
         age = case_when(grepl("18|24", variable) ~ "18-24",
                         grepl("25|34", variable) ~ "25-34",
                         grepl("35|44", variable) ~ "35-44",
                         grepl("45|64", variable) ~ "45-64",
                         grepl("65|75|85", variable) ~ "65+",
                         !grepl("[[:digit:]]", variable) ~ "Total")) %>%
  group_by(id = GEOID, ward = NAME, gender, age, education) %>%
  summarise(pop = sum(estimate)) %>%
  ungroup() %>%
  spread(education, pop) %>%
  mutate(`Less than college` = Total - `College only`) %>%
  dplyr::select(-Total) %>%
  melt(id.vars = c("id", "ward", "gender", "age"), variable.name = "education", value.name = "pop") %>%
  mutate(state = "District of Columbia",
         name = case_when(grepl("Ward 1|Ward 2|Ward 6|Ward 8", ward) ~ "MD1",
                          grepl("Ward 3|Ward 4|Ward 5|Ward 7", ward) ~ "MD2")) %>%
  dplyr::select(state, name, gender, age, education, pop)

#### Sex, race, age ####
dc_md_sra_2017 <- get_acs(geography = "state legislative district (upper chamber)",
                          variables = c(`Male / white / 18-19` = "B01001H_007",
                                        `Male / white / 20-24` = "B01001H_008",
                                        `Male / white / 25-29` = "B01001H_009",
                                        `Male / white / 30-34` = "B01001H_010",
                                        `Male / white / 35-44` = "B01001H_011",
                                        `Male / white / 45-54` = "B01001H_012",
                                        `Male / white / 55-64` = "B01001H_013",
                                        `Male / white / 65-74` = "B01001H_014",
                                        `Male / white / 75-84` = "B01001H_015",
                                        `Male / white / 85+`   = "B01001H_016",
                                        `Female / white / 18-19` = "B01001H_022",
                                        `Female / white / 20-24` = "B01001H_023",
                                        `Female / white / 25-29` = "B01001H_024",
                                        `Female / white / 30-34` = "B01001H_025",
                                        `Female / white / 35-44` = "B01001H_026",
                                        `Female / white / 45-54` = "B01001H_027",
                                        `Female / white / 55-64` = "B01001H_028",
                                        `Female / white / 65-74` = "B01001H_029",
                                        `Female / white / 75-84` = "B01001H_030",
                                        `Female / white / 85+`   = "B01001H_031",
                                        `Male / black / 18-19` = "B01001B_007",
                                        `Male / black / 20-24` = "B01001B_008",
                                        `Male / black / 25-29` = "B01001B_009",
                                        `Male / black / 30-34` = "B01001B_010",
                                        `Male / black / 35-44` = "B01001B_011",
                                        `Male / black / 45-54` = "B01001B_012",
                                        `Male / black / 55-64` = "B01001B_013",
                                        `Male / black / 65-74` = "B01001B_014",
                                        `Male / black / 75-84` = "B01001B_015",
                                        `Male / black / 85+`   = "B01001B_016",
                                        `Female / black / 18-19` = "B01001B_022",
                                        `Female / black / 20-24` = "B01001B_023",
                                        `Female / black / 25-29` = "B01001B_024",
                                        `Female / black / 30-34` = "B01001B_025",
                                        `Female / black / 35-44` = "B01001B_026",
                                        `Female / black / 45-54` = "B01001B_027",
                                        `Female / black / 55-64` = "B01001B_028",
                                        `Female / black / 65-74` = "B01001B_029",
                                        `Female / black / 75-84` = "B01001B_030",
                                        `Female / black / 85+`   = "B01001B_031",
                                        `Male / Latino / 18-19` = "B01001I_007",
                                        `Male / Latino / 20-24` = "B01001I_008",
                                        `Male / Latino / 25-29` = "B01001I_009",
                                        `Male / Latino / 30-34` = "B01001I_010",
                                        `Male / Latino / 35-44` = "B01001I_011",
                                        `Male / Latino / 45-54` = "B01001I_012",
                                        `Male / Latino / 55-64` = "B01001I_013",
                                        `Male / Latino / 65-74` = "B01001I_014",
                                        `Male / Latino / 75-84` = "B01001I_015",
                                        `Male / Latino / 85+`   = "B01001I_016",
                                        `Female / Latina / 18-19` = "B01001I_022",
                                        `Female / Latina / 20-24` = "B01001I_023",
                                        `Female / Latina / 25-29` = "B01001I_024",
                                        `Female / Latina / 30-34` = "B01001I_025",
                                        `Female / Latina / 35-44` = "B01001I_026",
                                        `Female / Latina / 45-54` = "B01001I_027",
                                        `Female / Latina / 55-64` = "B01001I_028",
                                        `Female / Latina / 65-74` = "B01001I_029",
                                        `Female / Latina / 75-84` = "B01001I_030",
                                        `Female / Latina / 85+`   = "B01001I_031"),
                          state = "DC") %>%
  mutate(gender = case_when(grepl("Male", variable) ~ "Male",
                            grepl("Female", variable) ~ "Female"),
         race = case_when(grepl("white", variable, ignore.case = TRUE) ~ "White",
                          grepl("black", variable, ignore.case = TRUE) ~ "Black",
                          grepl("Latin", variable, ignore.case = TRUE) ~ "Latino",
                          !grepl("white|black|latin", variable, ignore.case = TRUE) ~ "Total"),
         age = case_when(grepl("18|24", variable) ~ "18-24",
                         grepl("25|34", variable) ~ "25-34",
                         grepl("35|44", variable) ~ "35-44",
                         grepl("45|64", variable) ~ "45-64",
                         grepl("65|75|85", variable) ~ "65+",
                         !grepl("[[:digit:]]", variable) ~ "Total"),
         state = "District of Columbia") %>%
  group_by(state, id = GEOID, ward = NAME, gender, race, age) %>% 
  summarise(pop = sum(estimate)) %>%
  ungroup() %>%
  mutate(state = "District of Columbia",
         name = case_when(grepl("Ward 1|Ward 2|Ward 6|Ward 8", ward) ~ "MD1",
                          grepl("Ward 3|Ward 4|Ward 5|Ward 7", ward) ~ "MD2")) %>%
  dplyr::select(state, name, gender, race, age, pop)

# Stick datasets together ####
## First calculate deviation from average college rate within gender/race cross-sections
sre_2017 <- bind_rows(cd_sre_2017, nj_dd_sre_2017, tx_sd_sre_2017, de_county_sre_2017, mt_region_sre_2017, dc_md_sre_2017, pr_sd_sre_2017) %>%
  as.tbl() %>%
  ungroup() %>%
  dplyr::select(state, name, gender, race, education, pop) 

sre_dev_2017 <- sre_2017 %>%
  group_by(name, gender) %>%
  mutate(district_gender_pop = sum(pop)) %>%
  group_by(name, gender, education) %>%
  mutate(group_pct = sum(pop) / district_gender_pop) %>%
  group_by(name, gender, race) %>%
  mutate(pct = pop / sum(pop),
         logit_dev = logit(pct) - logit(group_pct)) %>%
  ungroup() %>%
  dplyr::select(state, name, gender, race, education, logit_dev)

## Then calculate college rate within gender/age cross-sections
sae_2017 <- bind_rows(cd_sae_2017, nj_dd_sae_2017, tx_sd_sae_2017, de_county_sae_2017, mt_region_sae_2017, dc_md_sae_2017, pr_sd_sae_2017) %>%
  as.tbl() %>%
  ungroup() %>%
  dplyr::select(state, name, gender, age, education, pop) 

sae_pct_2017 <- sae_2017 %>%
  group_by(name, gender, age) %>%
  mutate(group_pct = pop / sum(pop)) %>%
  ungroup() %>%
  dplyr::select(state, name, gender, age, education, group_pct)

## Then apply race-based deviations to the gender-age college rates
sra_2017 <- bind_rows(cd_sra_2017, nj_dd_sra_2017, tx_sd_sra_2017, de_county_sra_2017, mt_region_sra_2017, dc_md_sra_2017, pr_sd_sra_2017) %>%
  as.tbl() %>%
  ungroup() %>%
  dplyr::select(state, name, gender, race, age, pop)

sra_pct_2017 <- sra_2017 %>%
  group_by(name, gender) %>%
  mutate(district_gender_pop = sum(pop)) %>%
  group_by(name, gender, age) %>%
  mutate(group_pct = sum(pop) / district_gender_pop)

srae_2017 <- bind_rows(cd_sra_2017, nj_dd_sra_2017, tx_sd_sra_2017, de_county_sra_2017, mt_region_sra_2017, dc_md_sra_2017, pr_sd_sra_2017) %>%
  as.tbl() %>%
  ungroup() %>%
  dplyr::select(state, name, gender, race, age, pop) %>%
  left_join(sae_pct_2017, by = c("state", "name", "gender", "age")) %>%
  left_join(sre_dev_2017, by = c("state", "name", "gender", "race", "education")) %>%
  mutate(pct = invlogit(logit(group_pct) + logit_dev)) %>%
  # Convert all the names
  mutate(district = case_when(grepl("at Large", name) ~ "At-large",
                              grepl("Congressional District 1 ", name) ~ "CD01",
                              grepl("Congressional District 2 ", name) ~ "CD02",
                              grepl("Congressional District 3 ", name) ~ "CD03",
                              grepl("Congressional District 4 ", name) ~ "CD04",
                              grepl("Congressional District 5 ", name) ~ "CD05",
                              grepl("Congressional District 6 ", name) ~ "CD06",
                              grepl("Congressional District 7 ", name) ~ "CD07",
                              grepl("Congressional District 8 ", name) ~ "CD08",
                              grepl("Congressional District 9 ", name) ~ "CD09",
                              grepl("Congressional District 10", name) ~ "CD10",
                              grepl("Congressional District 11", name) ~ "CD11",
                              grepl("Congressional District 12", name) ~ "CD12",
                              grepl("Congressional District 13", name) ~ "CD13",
                              grepl("Congressional District 14", name) ~ "CD14",
                              grepl("Congressional District 15", name) ~ "CD15",
                              grepl("Congressional District 16", name) ~ "CD16",
                              grepl("Congressional District 17", name) ~ "CD17",
                              grepl("Congressional District 18", name) ~ "CD18",
                              grepl("Congressional District 19", name) ~ "CD19",
                              grepl("Congressional District 20", name) ~ "CD20",
                              grepl("Congressional District 21", name) ~ "CD21",
                              grepl("Congressional District 22", name) ~ "CD22",
                              grepl("Congressional District 23", name) ~ "CD23",
                              grepl("Congressional District 24", name) ~ "CD24",
                              grepl("Congressional District 25", name) ~ "CD25",
                              grepl("Congressional District 26", name) ~ "CD26",
                              grepl("Congressional District 27", name) ~ "CD27",
                              grepl("Congressional District 28", name) ~ "CD28",
                              grepl("Congressional District 29", name) ~ "CD29",
                              grepl("Congressional District 30", name) ~ "CD30",
                              grepl("Congressional District 31", name) ~ "CD31",
                              grepl("Congressional District 32", name) ~ "CD32",
                              grepl("Congressional District 33", name) ~ "CD33",
                              grepl("Congressional District 34", name) ~ "CD34",
                              grepl("Congressional District 35", name) ~ "CD35",
                              grepl("Congressional District 36", name) ~ "CD36",
                              grepl("Congressional District 37", name) ~ "CD37",
                              grepl("Congressional District 38", name) ~ "CD38",
                              grepl("Congressional District 39", name) ~ "CD39",
                              grepl("Congressional District 40", name) ~ "CD40",
                              grepl("Congressional District 41", name) ~ "CD41",
                              grepl("Congressional District 42", name) ~ "CD42",
                              grepl("Congressional District 43", name) ~ "CD43",
                              grepl("Congressional District 44", name) ~ "CD44",
                              grepl("Congressional District 45", name) ~ "CD45",
                              grepl("Congressional District 46", name) ~ "CD46",
                              grepl("Congressional District 47", name) ~ "CD47",
                              grepl("Congressional District 48", name) ~ "CD48",
                              grepl("Congressional District 49", name) ~ "CD49",
                              grepl("Congressional District 50", name) ~ "CD50",
                              grepl("Congressional District 51", name) ~ "CD51",
                              grepl("Congressional District 52", name) ~ "CD52",
                              grepl("Congressional District 53", name) ~ "CD53",
                              grepl("State Senate District I \\(", name) ~ "San Juan",
                              grepl("State Senate District II \\(", name) ~ "Bayamón",
                              grepl("State Senate District III \\(", name) ~ "Arecibo",
                              grepl("State Senate District IV \\(", name) ~ "Mayagüez",
                              grepl("State Senate District V \\(", name) ~ "Ponce",
                              grepl("State Senate District VI \\(", name) ~ "Guayama",
                              grepl("State Senate District VII \\(", name) ~ "Humacao",
                              grepl("State Senate District VIII \\(", name) ~ "Carolina",
                              grepl("State Senate District 1 \\(2018\\)", name) & grepl("Texas", name) ~ "SD01",
                              grepl("State Senate District 2 \\(2018\\)", name) & grepl("Texas", name)  ~ "SD02",
                              grepl("State Senate District 3 \\(2018\\)", name) & grepl("Texas", name)  ~ "SD03",
                              grepl("State Senate District 4", name) & grepl("Texas", name)  ~ "SD04",
                              grepl("State Senate District 5", name) & grepl("Texas", name)  ~ "SD05",
                              grepl("State Senate District 6", name) & grepl("Texas", name)  ~ "SD06",
                              grepl("State Senate District 7", name) & grepl("Texas", name)  ~ "SD07",
                              grepl("State Senate District 8", name) & grepl("Texas", name)  ~ "SD08",
                              grepl("State Senate District 9", name) & grepl("Texas", name)  ~ "SD09",
                              grepl("State Senate District 10", name) & grepl("Texas", name)  ~ "SD10",
                              grepl("State Senate District 11", name) & grepl("Texas", name)  ~ "SD11",
                              grepl("State Senate District 12", name) & grepl("Texas", name)  ~ "SD12",
                              grepl("State Senate District 13", name) & grepl("Texas", name)  ~ "SD13",
                              grepl("State Senate District 14", name) & grepl("Texas", name)  ~ "SD14",
                              grepl("State Senate District 15", name) & grepl("Texas", name)  ~ "SD15",
                              grepl("State Senate District 16", name) & grepl("Texas", name)  ~ "SD16",
                              grepl("State Senate District 17", name) & grepl("Texas", name)  ~ "SD17",
                              grepl("State Senate District 18", name) & grepl("Texas", name)  ~ "SD18",
                              grepl("State Senate District 19", name) & grepl("Texas", name)  ~ "SD19",
                              grepl("State Senate District 20", name) & grepl("Texas", name)  ~ "SD20",
                              grepl("State Senate District 21", name) & grepl("Texas", name)  ~ "SD21",
                              grepl("State Senate District 22", name) & grepl("Texas", name)  ~ "SD22",
                              grepl("State Senate District 23", name) & grepl("Texas", name)  ~ "SD23",
                              grepl("State Senate District 24", name) & grepl("Texas", name)  ~ "SD24",
                              grepl("State Senate District 25", name) & grepl("Texas", name)  ~ "SD25",
                              grepl("State Senate District 26", name) & grepl("Texas", name)  ~ "SD26",
                              grepl("State Senate District 27", name) & grepl("Texas", name)  ~ "SD27",
                              grepl("State Senate District 28", name) & grepl("Texas", name)  ~ "SD28",
                              grepl("State Senate District 29", name) & grepl("Texas", name)  ~ "SD29",
                              grepl("State Senate District 30", name) & grepl("Texas", name)  ~ "SD30",
                              grepl("State Senate District 31", name) & grepl("Texas", name)  ~ "SD31",
                              grepl("MD1", name) ~ "MD01",
                              grepl("MD2", name) ~ "MD02",
                              grepl("Eastern", name) ~ "Eastern",
                              grepl("Western", name) ~ "Western",
                              grepl("Kent County", name) ~ "Kent County",
                              grepl("New Castle County", name) ~ "New Castle County",
                              grepl("Sussex County", name) ~ "Sussex County",
                              grepl("DD", name) ~ name),
         education = case_when(education == "College only" ~ "College",
                               education == "Less than college" ~ "Less than college")) %>%
  dplyr::select(state, district, gender, race, age, education, pop, pct) %>%
  # Merge delegate numbers in
  left_join(read_csv("data/district_delegates.csv") %>% dplyr::select(-clinton_2016, -sanders_2016), by = c("state", "district")) %>%
  mutate(pop = round(pop*pct)) %>%
  dplyr::select(state, district, primary_type, district_delegates = delegates, gender, race, age, education, pop)

write_csv(srae_2017, "data/district_demographics.csv")
