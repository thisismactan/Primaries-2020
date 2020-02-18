source("code/library.R")

cces_2016 <- read_csv("data/cces_2016.csv")

# Random forest
## Train/test split
set.seed(2020)
frac_train <- 0.7
n_train <- floor(frac_train*nrow(cces_2016))
train <- sample(nrow(cces_2016), n_train, replace = FALSE)
dem_primary.train <- cces_2016[train, ]
dem_primary.test <- cces_2016[-train, ]

dem_primary_logit_train <- glm(formula = democratic_primary == "Democrat" ~ gender*age + race*education + primary_type, 
                               data = dem_primary.train, family = binomial)
summary(dem_primary_logit_train)
train_preds <- predict(dem_primary_logit_train, type = "response")

dem_primary.test <- dem_primary.test %>%
  mutate(prob = predict(dem_primary_logit_train, newdata = dem_primary.test, type = "response"))

## How well calibrated is it?
dem_primary.test %>%
  group_by(prob_bucket = cut(prob, breaks = quantile(prob, probs = (0:10)/10))) %>%
  summarise(n = n(),
            observed = mean(democratic_primary == "Democrat"),
            lower = min(prob),
            avg = mean(prob),
            upper = max(prob)) %>%
  na.omit()

## On full dataset
dem_primary_logit <- glm(formula = democratic_primary == "Democrat" ~ gender*age + race*education + primary_type, 
                         data = cces_2016, family = binomial)

summary(dem_primary_logit)

write_rds(dem_primary_logit, "code/dem_primary_logit.rds")
