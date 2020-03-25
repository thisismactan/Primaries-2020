library(tidyverse)

exit_polls <- read_csv("data/exit_polls.csv") %>%
  left_join(primary_schedule %>% dplyr::select(state, date), by = "state") %>%
  mutate(demo_group_desc = case_when(demo_group %in% c("gender", "race", "age", "education", "ideology", "urbanicity") ~ str_to_sentence(demo_group),
                                     demo_group == "education_race" ~ "Education and race",
                                     demo_group == "education_race_gender" ~ "Education and gender, whites only",
                                     demo_group == "prior_primary" ~ "Primary voting history",
                                     demo_group == "mfa" ~ "Medicare for All?",
                                     demo_group == "priority" ~ "More important that nominee..."),
         group_voters = group_pct * total_votes,
         cand_voters = cand_pct * group_voters,
         demo = case_when(demo == "Moderate/conservative" ~ "Mod./cons.",
                          demo == "Somewhat liberal" ~ "Smwt. liberal",
                          !(demo %in% c("Moderate/conservative", "Somewhat liberal")) ~ demo))

# Percent of all voters
demo_vote_estimates <- exit_polls %>%
  filter(!is.na(cand_voters), demo != "All nonwhite") %>%
  group_by(candidate, demo_group, demo_group_desc, demo) %>%
  summarise(group_votes = sum(group_voters), 
            cand_votes = sum(cand_voters)) %>%
  group_by(candidate, demo_group) %>%
  mutate(demo_pct = group_votes / sum(group_votes)) %>%
  ungroup() %>%
  mutate(pct = cand_votes / group_votes)

# All candidates' table
demo_vote_estimates %>%
  filter(candidate %in% c("biden", "bloomberg", "sanders", "warren")) %>%
  dplyr::select(candidate, demo, demo_pct, pct) %>%
  spread(candidate, pct) %>%
  print(n = Inf)

## Demographics
demo_vote_estimates %>%
  filter(candidate %in% c("biden", "sanders"), demo_group %in% c("age", "education", "gender", "race")) %>%
  ggplot(aes(x = demo, y = pct, fill = candidate)) +
  facet_wrap(~demo_group_desc, scales = "free_x") +
  geom_col(position = "dodge") +
  scale_fill_manual(name = "Candidate", values = candidate_colors, labels = candidate_labels) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Breakdown of the 2020 primary vote by demographic group", subtitle = "Estimates from exit polls",
       x = "", y = "Share of vote")

demo_vote_estimates %>%
  filter(candidate %in% c("biden", "sanders"), demo_group %in% c("prior_primary", "ideology", "mfa", "priority")) %>%
  ggplot(aes(x = demo, y = pct, fill = candidate)) +
  facet_wrap(~demo_group_desc, scales = "free_x") +
  geom_col(position = "dodge") +
  scale_fill_manual(name = "Candidate", values = candidate_colors, labels = candidate_labels) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Breakdown of the 2020 primary vote by political segment", subtitle = "Estimates from exit polls",
       x = "", y = "Share of vote")

# Percentages over time
group_cum_pct <- exit_polls %>%
  group_by(candidate, date, demo, demo_group) %>%
  summarise(total_votes = sum(group_voters),
            cand_votes = sum(cand_voters, na.rm = TRUE)) %>%
  arrange(candidate, demo, date) %>%
  group_by(candidate, demo, demo_group) %>%
  mutate(cum_total_votes = cumsum(total_votes),
         cum_cand_votes = cumsum(cand_votes),
         cum_pct = cum_cand_votes / cum_total_votes) %>%
  group_by(date, demo) %>%
  mutate(cum_pct = cum_pct / sum(cum_pct))

group_cum_pct %>%
  filter(candidate %in% c("biden", "sanders"), demo_group == "age") %>%
  ggplot(aes(x = date, y = cum_pct, col = candidate)) +
  facet_wrap(~str_to_title(demo)) +
  geom_line()
