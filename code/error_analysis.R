pred_comparison <- tibble(candidate = rep(c("Biden", "Buttigieg", "Klobuchar", "Sanders", "Steyer", "Warren"), 2),
                          type = ordered(rep(c("Forecast", "Actual"), each = 6), levels = c("Forecast", "Actual")),
                          pct = c(c(0.367, 0.108, 0.063, 0.224, 0.145, 0.087), c(0.484, 0.082, 0.031, 0.199, 0.113, 0.071)),
                          lower = c(c(0.24, 0.034, 0.016, 0.116, 0.060, 0.026), rep(NA, 6)),
                          upper = c(c(0.507, 0.203, 0.139, 0.352, 0.257, 0.180), rep(NA, 6)))

ggplot(pred_comparison, aes(x = candidate, y = pct, fill = candidate)) +
  geom_col(aes(alpha = type), position = "dodge2") +
  geom_errorbar(aes(ymin = lower, ymax = upper), col = "#666666", width = 0.5, position = position_dodge(width = 1)) +
  geom_text(aes(y = pct + 0.01, label = scales::percent(pct, accuracy = 1L), group = type), size = 3, position = position_dodge(width = 0.85)) +
  scale_alpha_manual(name = "", values = c("Forecast" = 0.5, "Actual" = 1)) +
  scale_fill_manual(name = "Candidate", 
                    values = c("Biden" = "red", "Buttigieg" = "darkorange1", "Klobuchar" = "gold2", "Sanders" = "blue", "Steyer" = "deeppink4",
                               "Warren" = "green4"), 
                    labels = c("Biden" = "Joe Biden", "Buttigieg" = "Pete Buttigieg", "Klobuchar" = "Amy Klobuchar", "Sanders" = "Bernie Sanders", 
                               "Steyer" = "Tom Steyer", "Warren" = "Elizabeth Warren")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L)) +
  theme(axis.ticks.x = element_blank()) +
  labs(title = "2020 South Carolina Democratic primary results", subtitle = "Forecast vs. actual",
       x = "", y = "Vote share", caption = "Error bars indicate 90% CI on forecast")
