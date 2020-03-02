source("code/poll_average_over_time.R")

contest_date <- as.Date("2020-06-16")

## Column plot
national_average %>%
  filter(!(candidate %in% c("yang", "steyer", "buttigieg"))) %>%
  ggplot(aes(x = candidate, fill = candidate)) +
  geom_col(aes(y = avg)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), col = "#555555") +
  geom_text(aes(y = avg + 1, label = paste0(round(avg, 1), "%")), size = 3.5) +
  scale_fill_manual(name = "Candidate", labels = candidate_labels, values = candidate_colors) +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
  labs(title = "2020 Democratic primary national polling", x = "Candidate", y = "%", caption = "Error bars indicate 90% CIs", 
       subtitle = paste0(month(today(), label = TRUE, abbr = FALSE), " ", day(today()), ", ", year(today())))

## Over time
national_polls_adjusted %>%
  filter(!(candidate %in% c("yang", "steyer", "buttigieg"))) %>%
  ggplot(aes(x = median_date, col = candidate)) +
  geom_vline(xintercept = contest_date) +
  geom_ribbon(data = national_averages_adjusted %>% filter(!(candidate %in% c("yang", "steyer", "buttigieg"))), 
              aes(ymin = lower, ymax = upper, fill = candidate), col = NA, alpha = 1/7) +
  geom_point(aes(y = 100*pct), alpha = 1/2, size = 1) +
  geom_line(data = national_averages_adjusted %>% 
              filter(!(candidate %in% c("yang", "steyer", "buttigieg"))), aes(y = pct), lwd = 1.3) +
  scale_fill_manual(name = "Candidate", labels = candidate_labels, values = candidate_colors) +
  scale_colour_manual(name = "Candidate", labels = candidate_labels, values = candidate_colors) +
  scale_x_date(breaks = "1 month", date_labels = "%e %b %Y", limits = as.Date(c("2019-06-01", "2020-06-16"))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5)) +
  labs(title = "2020 Democratic primary national polling", x = "Candidate", y = "%",
       subtitle = paste0(month(today(), label = TRUE, abbr = FALSE), " ", day(today()), ", ", year(today())),
       caption = "Averages smoothed over five days") +
  lims(y = c(0, 50))
