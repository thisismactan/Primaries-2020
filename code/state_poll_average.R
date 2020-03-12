source("code/state_poll_average_over_time.R")

state_primary_schedule <- read_csv("data/primary_schedule.csv")
  
state_name <- "Ohio"
contest_type <- ifelse(
  state_name %in% c("Iowa", "Nevada", "American Samoa", "North Dakota", "Wyoming", "Virgin Islands"),
  "caucus", "primary"
)
contest_date <- state_primary_schedule %>%
  filter(state == state_name) %>%
  pull(date)

## Column plot
state_averages %>%
  filter(state == state_name, candidate %in% c("biden", "sanders")) %>%
  ggplot(aes(x = candidate, fill = candidate)) +
  geom_col(aes(y = state_avg)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), col = "#555555") +
  geom_text(aes(y = state_avg + 1, label = paste0(round(state_avg, 1), "%")), size = 3.5) +
  scale_fill_manual(name = "Candidate", labels = candidate_labels, values = candidate_colors) +
  scale_x_discrete(labels = candidate_lastnames) +
  theme(axis.ticks.x = element_blank()) +
  labs(title = paste0("2020 Democratic ", state_name, " ", contest_type, " polling average"), x = "", y = "%", 
       caption = "Error bars indicate 90% CIs", subtitle = paste0(month(today(), label = TRUE, abbr = FALSE), " ", day(today()), ", ", year(today())))

## Over time
state_polls %>%
  filter(state == state_name, candidate %in% c("biden", "sanders")) %>%
  ggplot(aes(x = median_date, col = candidate)) +
  geom_vline(xintercept = contest_date) +
  geom_ribbon(data = state_averages_over_time %>% filter(state == state_name, candidate %in% c("biden", "sanders")), 
              aes(ymin = lower, ymax = upper, fill = candidate), col = NA, alpha = 1/7) +
  geom_point(aes(y = pct_adjusted), alpha = 3/4, size = 1) +
  geom_line(data = state_averages_over_time %>% filter(state == state_name, candidate %in% c("biden", "sanders")), 
            aes(y = pct), lwd = 1.3) +
  scale_fill_manual(name = "Candidate", labels = candidate_labels, values = candidate_colors) +
  scale_colour_manual(name = "Candidate", labels = candidate_labels, values = candidate_colors) +
  scale_x_date(breaks = "1 month", date_labels = "%e %b %Y", limits = as.Date(c("2019-09-01", "2020-06-16"))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5)) +
  labs(title = paste0("2020 Democratic ", state_name, " ", contest_type, " polling"), x = "Date", y = "%",
       subtitle = paste0(month(today(), label = TRUE, abbr = FALSE), " ", day(today()), ", ", year(today())),
       caption = "Averages smoothed over five days")
