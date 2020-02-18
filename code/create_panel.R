source("code/library.R")

# Read primary data going back to 2004
primaries <- read_csv("data/primaries_data.csv")

# Create panels

## Total across candidates (for denominators)
primaries_state_panel <- primaries %>%
  arrange(party, date, candidate) %>%
  group_by(party, date) %>%
  summarise(total_votes = sum(votes, na.rm = TRUE),
            states_on_date = sum(won, na.rm = TRUE),
            delegates_on_date = sum(delegates_won, na.rm = TRUE),
            candidates_running = first(candidates_running)) %>%
  group_by(party, year = year(date)) %>%
  mutate(candidates_running = first(candidates_running),
         total_states_to_date = cumsum(states_on_date),
         total_votes_to_date = cumsum(total_votes),
         total_delegates_to_date = cumsum(delegates_on_date))

## Total within dates
primaries_candidate_panel <- primaries %>%
  filter(!is.na(won)) %>%
  group_by(party, date, candidate, dropout_date, nomination_decided_date) %>%
  summarise(votes_won = sum(votes, na.rm = TRUE),
            states_won = sum(won, na.rm = TRUE),
            delegates_won = sum(delegates_won, na.rm = TRUE)) %>%
  arrange(party, candidate, date) %>%
  group_by(party, candidate, year = year(date), nomination_decided_date) %>%
  mutate(cum_votes = cumsum(votes_won),
         cum_states = cumsum(states_won),
         cum_delegates = cumsum(delegates_won))

## By candidate
primaries_panel <- primaries_candidate_panel %>%
  left_join(primaries_state_panel, by = c("year", "party", "date")) %>%
  mutate(cum_vote_share = cum_votes / total_votes_to_date,
         cum_state_share = cum_states / total_states_to_date,
         cum_delegate_share = cum_delegates / total_delegates_to_date,
         days_to_end = pmin(nomination_decided_date - date, dropout_date - date, na.rm = TRUE),
         dropped_out = !is.na(dropout_date) | candidate == "Bernie Sanders") %>%
  ungroup() %>%
  filter(days_to_end > 0)

dem_panel <- primaries_panel %>%
  filter(party == "Democratic")

gop_panel <- primaries_panel %>%
  filter(party == "Republican")
