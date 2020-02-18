source("code/library.R")

state_polls_url <- paste0("https://projects.fivethirtyeight.com/2020-primaries/democratic/", tolowerSpacefill(state), "/")

header_string <- '/html/body/div[5]/div/table[1]'

state_polls_table <- httr::GET(state_polls_url) %>%
  httr::content(as = "text", encoding = "UTF-8") %>%
  xml2::read_html() %>%
  rvest::html_nodes(xpath = header_string) %>%
  rvest::html_table(fill = TRUE)

state_polls.dirty <- state_polls_table[[1]]

names(state_polls.dirty)[1] <- "garbage"
names_length <- length(names(state_polls.dirty))
names(state_polls.dirty) <- c("dot", tolowerSpacefill(names(state_polls.dirty))[-names_length])
state_polls.dirty <- state_polls.dirty[, -c(names_length - 1, names_length)] %>%
  as_tibble(.name_repair = "unique") %>%
  select(dates, pollster, n = sample, pop = sample.1, biden, booker, bloomberg, buttigieg, harris, klobuchar, sanders, steyer, warren, yang)

## Cleanup time
state_polls.clean <- state_polls.dirty %>%
  mutate_at(vars(c("n", "biden", "bloomberg", "booker", "buttigieg", "harris", "klobuchar", "sanders", "steyer", "warren", "yang")), 
            function(x) gsub("[[:space:]]|[[:alpha:]]|[[:punct:]]|–", "", x) %>% as.numeric())

### Figuring out dates
state_poll_dates <- state_polls.clean$dates %>%
  str_split("–| – |[[:space:]]|, |-")

single_day <- lapply(state_poll_dates, length) == 3
single_month <- lapply(state_poll_dates, length) == 4
single_year <- lapply(state_poll_dates, length) == 5

start_dates <- end_dates <- rep(NA, nrow(state_polls.clean))
start_dates[single_day] <- state_poll_dates[single_day] %>% lapply(function(x) paste(x[3], x[1], x[2]))
start_dates[single_month] <- state_poll_dates[single_month] %>% lapply(function(x) paste(x[4], x[1], x[2]))
start_dates[single_year] <- state_poll_dates[single_year] %>% lapply(function(x) paste(x[5], x[1], x[2]))

end_dates[single_day] <- state_poll_dates[single_day] %>% lapply(function(x) paste(x[3], x[1], x[2]))
end_dates[single_month] <- state_poll_dates[single_month] %>% lapply(function(x) paste(x[4], x[1], x[3]))
end_dates[single_year] <- state_poll_dates[single_year] %>% lapply(function(x) paste(x[5], x[3], x[4]))

start_dates <- start_dates %>%
  unlist() %>%
  as.Date(format = "%Y %b %e")

end_dates <- end_dates %>%
  unlist() %>%
  as.Date(format = "%Y %b %e")

### Final cleanup
state_polls <- state_polls.clean %>%
  mutate(start_date = start_dates,
         end_date = end_dates,
         date_spread = as.numeric(end_date - start_date) + 1,
         median_date = start_date + floor(date_spread/2),
         age = as.numeric(today() - median_date),
         weight = 4*ifelse(pop == "LV", 1.5, 1)*(n^0.4)*(age <= 90)/
           (exp(age^(1/3))*ifelse(date_spread == 1, 4, 1)*ifelse(grepl("Harris", pollster), 9, 1)),
         loess_weight = 4*ifelse(pop == "LV", 1.5, 1)*(n^0.3)/(ifelse(date_spread == 1, 4, 1)*ifelse(grepl("Harris", pollster), 9, 1))) %>%
  arrange(age) %>%
  filter(pop != "A") %>%
  dplyr::select(pollster, median_date, age, date_spread, n, pop, weight, loess_weight, biden, bloomberg, booker, buttigieg, harris, klobuchar, sanders, 
                steyer, warren, yang) %>%
  filter(!grepl("McLaughlin|WPA", pollster, ignore.case = FALSE), biden + sanders >= 25,
         !is.na(biden), !is.na(sanders), !is.na(warren), !is.na(buttigieg))

state_polls.long <- state_polls %>%
  melt(id.vars = c("pollster", "median_date", "weight", "loess_weight", "age", "date_spread", "n", "pop"), variable.name = "candidate", value.name = "pct")

source("code/poll_average_over_time.R")

state_poll_leans <- state_polls.long %>%
  left_join(averages %>% select(candidate, median_date, pct_natl = pct), by = c("candidate", "median_date")) %>%
  mutate(state_lean = pct - pct_natl) %>%
  select(-pct, -pct_natl)
