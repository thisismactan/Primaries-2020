source("code/library.R")
primary_schedule <- read_csv("data/primary_schedule.csv")

# Delegates by day
primary_schedule %>%
  group_by(date) %>%
  summarise(pledged = sum(pledged)) %>%
  mutate(cum_delegates = cumsum(pledged),
         cum_pct = cum_delegates/sum(pledged)) %>%
  print(n = Inf)

