# NUMBERS ####
all_sims <- read_csv("output/sims_by_district.csv") %>%
  group_by(sim_id) %>%
  mutate(any_missing = any(is.na(candidate))) %>%
  filter(!any_missing)

candidate_stats <- all_sims %>%
  group_by(sim_id, candidate) %>%
  summarise(delegates = sum(candidate_delegates, na.rm = TRUE)) %>%
  group_by(candidate) %>% summarise(pct05 = quantile(delegates, 0.05),
                                    mean = mean(delegates),
                                    median = median(delegates),
                                    pct95 = quantile(delegates, 0.95))

# Sanity check: how many delegates total? (Should be 3,979)
all_sims %>%
  group_by(sim_id) %>% 
  summarise(delegates = sum(candidate_delegates)) %>%
  group_by(delegates) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

# Total delegates expected
all_sims %>%
  group_by(sim_id, candidate) %>%
  summarise(delegates = sum(candidate_delegates)) %>%
  group_by(candidate) %>%
  summarise(n = n(),
            pct05 = quantile(delegates, 0.05, na.rm = TRUE),
            avg = mean(delegates, na.rm = TRUE),
            median = median(delegates, na.rm = TRUE),
            pct95 = quantile(delegates, 0.95, na.rm = TRUE))

# Probability of a majority
all_sims %>%
  group_by(sim_id, candidate) %>%
  summarise(delegates = sum(candidate_delegates)) %>%
  mutate(majority = delegates > 3979/2) %>%
  group_by(candidate) %>%
  summarise(prob_majority = mean(majority, na.rm = TRUE))

# Probability of a plurality
all_sims %>%
  group_by(sim_id, candidate) %>%
  summarise(delegates = sum(candidate_delegates)) %>%
  group_by(sim_id) %>%
  filter(delegates == max(delegates)) %>%
  group_by(candidate) %>%
  summarise(prob_plurality = n() / n_sims)

# Overall outcomes (distinguishing majority from plurality)
all_sims %>%
  group_by(sim_id, candidate) %>%
  summarise(delegates = sum(candidate_delegates, na.rm = TRUE)) %>%
  filter(delegates == max(delegates)) %>%
  mutate(outcome = case_when(delegates >= 1990 ~ paste(str_to_title(candidate), "majority"),
                             delegates < 1990 ~ paste(str_to_title(candidate), "plurality"))) %>%
  group_by(outcome) %>%
  summarise(prob = n() / n_sims) %>%
  arrange(desc(prob))

# Expected delegates in all early states
all_sims %>%
  filter(state %in% c("Iowa", "New Hampshire", "Nevada", "South Carolina")) %>%
  group_by(candidate, sim_id) %>%
  summarise(delegates = sum(candidate_delegates)) %>%
  group_by(candidate) %>%
  summarise(avg = mean(delegates),
            pct05 = quantile(delegates, prob = 0.05),
            pct50 = quantile(delegates, prob = 0.5),
            pct95 = quantile(delegates, prob = 0.95))


# Expected delegates by early state
all_sims %>%
  filter(state %in% c("Iowa", "New Hampshire", "Nevada", "South Carolina")) %>%
  group_by(state, candidate, sim_id) %>%
  summarise(delegates = sum(candidate_delegates)) %>%
  group_by(state, Candidate = str_to_title(candidate)) %>%
  summarise(avg = mean(delegates)) %>%
  spread(state, avg) %>%
  dplyr::select(Candidate, IA = Iowa, NH = `New Hampshire`, NV = Nevada, SC = `South Carolina`)

# All states
all_sims %>%
  group_by(state, candidate, sim_id) %>%
  summarise(delegates = sum(candidate_delegates)) %>%
  group_by(State = state, Candidate = str_to_title(candidate)) %>%
  summarise(avg = round(mean(delegates), 1)) %>%
  spread(Candidate, avg) %>%
  as.data.frame()


# Probability of winning in each state
all_sims %>%
  filter(district == "At-large") %>%
  group_by(sim_id, state) %>%
  filter(pct == max(pct)) %>%
  group_by(State = state, Candidate = str_to_title(candidate)) %>%
  summarise(n = n() / n_sims) %>%
  spread(Candidate, n) %>%
  as.data.frame()


### PLOTS ####
candidates_to_plot <- c("biden", "bloomberg", "buttigieg", "sanders", "warren")
candidates_to_plot2 <- c("buttigieg", "klobuchar", "steyer", "yang")
all_candidates <- c("biden", "bloomberg", "buttigieg", "klobuchar", "sanders", "steyer", "warren", "yang")

# Iowa only
all_sims %>%
  filter(candidate %in% candidates_to_plot, state == "New Hampshire") %>%
  group_by(sim_id, candidate) %>%
  summarise(delegates = sum(candidate_delegates)) %>%
  ggplot(aes(x = delegates, y = ..density.., fill = candidate)) +
  facet_wrap(~str_to_title(candidate), nrow = length(candidates_to_plot)) +
  geom_vline(data = all_sims %>%
               filter(candidate %in% candidates_to_plot, state == "Iowa") %>%
               group_by(sim_id, candidate) %>% 
               summarise(delegates = sum(candidate_delegates)) %>% 
               group_by(candidate) %>%
               summarise(avg_delegates = mean(delegates)),
             aes(xintercept = avg_delegates, col = candidate), show.legend = FALSE) +
  geom_histogram(col = "black", binwidth = 1) +
  scale_fill_manual(name = "Candidate", values = candidate_colors, labels = candidate_labels) +
  scale_colour_manual(name = "Candidate", values = candidate_colors, labels = candidate_labels) + 
  labs(title = "2020 Democratic New Hampshire primary forecast", x = "Delegates", y = "Probability",
       subtitle = paste0(month(today(), label = TRUE, abbr = FALSE), " ", day(today()), ", ", year(today())))

# Early states
all_sims %>%
  filter(state %in% c("Iowa", "New Hampshire", "Nevada", "South Carolina")) %>%
  filter(candidate %in% candidates_to_plot) %>%
  group_by(sim_id, candidate) %>%
  summarise(delegates = sum(candidate_delegates)) %>%
  ggplot(aes(x = delegates, y = ..density.., fill = candidate)) +
  facet_wrap(~str_to_title(candidate), nrow = length(candidates_to_plot)) +
  geom_vline(data = all_sims %>%
               filter(state %in% c("Iowa", "New Hampshire", "Nevada", "South Carolina")) %>%
               filter(candidate %in% candidates_to_plot) %>%
               group_by(sim_id, candidate) %>% 
               summarise(delegates = sum(candidate_delegates)) %>% 
               group_by(candidate) %>%
               summarise(avg_delegates = mean(delegates)),
             aes(xintercept = avg_delegates, col = candidate), show.legend = FALSE) +
  geom_histogram(col = "black", binwidth = 2) +
  scale_fill_manual(name = "Candidate", values = candidate_colors, labels = candidate_labels) +
  scale_colour_manual(name = "Candidate", values = candidate_colors, labels = candidate_labels) + 
  labs(title = "2020 Democratic early states delegate forecast", x = "Delegates", y = "Probability density",
       subtitle = paste0(month(today(), label = TRUE, abbr = FALSE), " ", day(today()), ", ", year(today())),
       caption = "Early states: IA, NH, NV, and SC")

# Super Tuesday


# All states
all_sims %>%
  filter(candidate %in% candidates_to_plot) %>%
  group_by(sim_id, candidate) %>%
  summarise(delegates = sum(candidate_delegates)) %>%
  ggplot(aes(x = delegates, y = ..density.., fill = candidate)) +
  facet_wrap(~str_to_title(candidate), nrow = length(candidates_to_plot)) +
  geom_vline(aes(xintercept = ceiling(3979/2)), size = 1) +
  geom_vline(data = candidate_stats %>% filter(candidate %in% candidates_to_plot),
             aes(xintercept = mean, col = candidate), show.legend = FALSE) +
  geom_histogram(col = "black", binwidth = 59) +
  scale_fill_manual(name = "Candidate", values = candidate_colors, labels = candidate_labels) +
  scale_colour_manual(name = "Candidate", values = candidate_colors, labels = candidate_labels) + 
  labs(title = "2020 Democratic primary delegate forecast", x = "Delegates", y = "Probability density",
       subtitle = paste0(month(today(), label = TRUE, abbr = FALSE), " ", day(today()), ", ", year(today())),
       caption = "1,990 pledged delegates required for first-ballot majority") +
  lims(x = c(0, 3500))

# Delegate totals over time
all_sims_by_state <- all_sims %>%
  group_by(sim_id, state, candidate) %>%
  summarise(delegates = sum(candidate_delegates)) %>%
  left_join(primary_schedule %>% dplyr::select(date, state), by = "state") 

all_sims_by_date <- all_sims_by_state %>%
  group_by(sim_id, date, candidate) %>%
  summarise(delegates = sum(delegates)) 

delegate_timeline <- all_sims_by_date %>%
  arrange(sim_id, candidate, date) %>%
  group_by(sim_id, candidate) %>%
  mutate(delegates_to_date = cumsum(delegates)) %>%
  group_by(candidate, date) %>%
  summarise(pct05 = quantile(delegates_to_date, 0.05, na.rm = TRUE),
            mean = mean(delegates_to_date, na.rm = TRUE),
            median = median(delegates_to_date, na.rm = TRUE),
            pct95 = quantile(delegates_to_date, 0.95, na.rm = TRUE))

delegate_timeline %>%
  filter(candidate %in% candidates_to_plot) %>%
  ggplot(aes(x = date, y = mean)) +
  geom_hline(yintercept = ceiling(3979/2)) +
  geom_stepribbon(aes(ymin = pct05, ymax = pct95, fill = candidate), show.legend = FALSE, alpha = 0.2, size = 0) +
  geom_step(aes(col = candidate)) +
  scale_colour_manual(name = "Candidate", values = candidate_colors, labels = candidate_labels) +
  scale_fill_manual(name = "Candidate", values = candidate_colors, labels = candidate_labels)

## Maps
all_sims_by_state %>%
  group_by(sim_id, state, candidate) 