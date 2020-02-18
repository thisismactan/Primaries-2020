source("code/library.R")

# Nationwide polling
national_polls_url <- "https://projects.fivethirtyeight.com/2020-primaries/democratic/national/"

header_string <- '/html/body/div[5]/div/table[1]'

national_polls_table <- httr::GET(national_polls_url) %>%
  httr::content(as = "text", encoding = "UTF-8") %>%
  xml2::read_html() %>%
  rvest::html_nodes(xpath = header_string) %>%
  rvest::html_table(fill = TRUE)

names(national_polls_table[[1]]) <- c("blank", "garbage", "dates", "pollster", "n", "pop", "biden", "sanders", "warren", "buttigieg", "harris",
                                      "booker", "yang", "klobuchar", "gabbard", "castro", "steyer", "bloomberg", "bennet", "delaney", "williamson",
                                      "bullock", "patrick", "orourke", "ryan", "gillibrand", "clinton", "deblasio", "hickenlooper", "inslee", "kerry",
                                      "sestak", "messam", "obama", "gravel", "moulton", "brown", "swalwell", "abrams", "holder", "mcauliffe", 
                                      "winfrey", "ojeda", "trump", "cuomo", "avenatti", "kennedy", "zuckerberg", "pelosi", "garcetti", "newsom", 
                                      "schultz", "kaine", "johnson", "kucinich", "lee", "scott", "sinema", "warner", "blank2")

national_polls.dirty <- national_polls_table[[1]] %>%
  dplyr::select(dates, pollster, n, pop, biden, booker, bloomberg, buttigieg, harris, klobuchar, sanders, steyer, warren, yang)

## Cleanup time
national_polls.clean <- national_polls.dirty %>%
  mutate_at(vars(c("n", "biden", "booker", "bloomberg", "buttigieg", "harris", "klobuchar", "sanders", "steyer", "warren", "yang")), 
                 function(x) gsub("[[:space:]]|[[:alpha:]]|[[:punct:]]|–", "", x) %>% as.numeric()) %>%
  as.tbl()

### Figuring out dates
poll_dates <- national_polls.clean$dates %>%
  str_split("–| – |[[:space:]]|, |-")

single_day <- lapply(poll_dates, length) == 3
single_month <- lapply(poll_dates, length) == 4
single_year <- lapply(poll_dates, length) == 5

start_dates <- end_dates <- rep(NA, nrow(national_polls.clean))
start_dates[single_day] <- poll_dates[single_day] %>% lapply(function(x) paste(x[3], x[1], x[2]))
start_dates[single_month] <- poll_dates[single_month] %>% lapply(function(x) paste(x[4], x[1], x[2]))
start_dates[single_year] <- poll_dates[single_year] %>% lapply(function(x) paste(x[5], x[1], x[2]))

end_dates[single_day] <- poll_dates[single_day] %>% lapply(function(x) paste(x[3], x[1], x[2]))
end_dates[single_month] <- poll_dates[single_month] %>% lapply(function(x) paste(x[4], x[1], x[3]))
end_dates[single_year] <- poll_dates[single_year] %>% lapply(function(x) paste(x[5], x[3], x[4]))

start_dates <- start_dates %>%
  unlist() %>%
  as.Date(format = "%Y %b %e")

end_dates <- end_dates %>%
  unlist() %>%
  as.Date(format = "%Y %b %e")

### Final cleanup
national_polls <- national_polls.clean %>%
  mutate(start_date = start_dates,
         end_date = end_dates,
         date_spread = as.numeric(end_date - start_date) + 1,
         median_date = start_date + floor(date_spread/2),
         age = as.numeric(today() - median_date),
         weight = 4*ifelse(pop == "LV", 1.5, 1)*(n^0.4)*(age <= 90)/
           (exp(age^(1/3))*ifelse(date_spread == 1, 4, 1)*ifelse(grepl("Harris", pollster), 9, 1)),
         loess_weight = 4*ifelse(pop == "LV", 1.5, 1)*(n^0.2)/(ifelse(date_spread == 1, 4, 1)*ifelse(grepl("Harris", pollster), 9, 1))) %>%
  arrange(age) %>%
  filter(pop != "A") %>%
  dplyr::select(pollster, median_date, age, date_spread, n, pop, weight, loess_weight, biden, booker, bloomberg, buttigieg, harris, klobuchar, 
                sanders, steyer, warren, yang) %>%
  filter(!grepl("McLaughlin|WPA", pollster, ignore.case = FALSE), biden + sanders >= 25,
         !is.na(biden), !is.na(sanders), !is.na(warren), !is.na(buttigieg))

national_polls.long <- national_polls %>%
  melt(id.vars = c("pollster", "median_date", "weight", "loess_weight", "age", "date_spread", "n", "pop"), variable.name = "candidate", value.name = "pct")

national_polls_dld <- read_csv("data/fivethirtyeight-data/primary_polls_2020.csv")
