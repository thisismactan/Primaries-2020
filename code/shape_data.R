source("code/library.R")

## Read in, filter to 2008+
primaries <- read_csv("data/primaries_data.csv") %>%
  mutate(date = as.Date(date, format = "%d-%b-%y")) %>%
  filter(year(date) >= 2008)

gop_2016 <- primaries %>%
  filter(party == "Republican", year(date) == 2016)

## Create dates for Republican primary
startdate_gop2016 <- as.Date("2016-02-02")
enddate_gop2016 <- as.Date("2016-05-04")
dates_gop2016 <- seq(from = startdate_gop2016, to = enddate_gop2016, by = 7)

candidates_gop2016 <- unique(gop_2016$candidate)

gop_2016_dates <- expand.grid(date = dates_gop2016, candidate = candidates_gop2016) %>%
  as.tbl() %>%
  dplyr::select(candidate, date)

primaries %>%
  group_by(party, date, state) %>%
  mutate(total_votes = sum(votes)) %>%
  group_by(party, candidate, dropout_date, home_state, also_ran, date, candidates_running) %>%
  summarise(total_delegates = sum(total_delegates),
            delegates_won = sum(delegates_won),
            votes_won = sum(votes),
            contests_won = sum(won, na.rm = TRUE)) %>%
  mutate(dropout14 = case_when(!is.na(dropout_date) ~ dropout_date - date < 14,
                               is.na(dropout_date) ~ FALSE)) %>%
  View()

