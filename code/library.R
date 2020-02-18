## LIBRARIES

#### Data manipulation ####
library(Hmisc)
library(lubridate)
library(reshape2)
library(scales)
library(tidyverse)

#### Modeling ####
library(coxme)
library(lme4)
library(MCMCpack)
library(randomForest)
library(survival)
library(survminer)
library(xgboost)

#### Scraping and data collection ####
library(httr)
library(tidycensus)
library(rvest)
library(utils)
library(xml2)

#### Simulation ####
library(Compositional)
library(doParallel)
library(foreach)
library(mvnfast)

#### Mapping, potentially ####
library(leaflet)
library(rgdal)
library(sf)

## CUSTOM FUNCTIONS
betaMOM <- function(x = NULL, mu = NULL, v = NULL) {
  # Method of moments estimation for beta distribution parameters alpha and beta
  if(!is.null(x)) {
    v <- var(x)
    mu <- mean(x)
  }
  
  if(mu == 0) {
    return(c("alpha" = NaN, "beta" = NaN))
  }
  
  alpha <- ((1 - mu) / v - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  
  estimates <- c("alpha" = alpha, "beta" = beta)
  return(estimates)
}

estBetaParams <- function(mu, var) {
  return(estimates)
}

logit <- function(x) {
  logit_x <- log(x/(1-x))
  return(logit_x)
}

invlogit <- function(x) {
  invlogit_x <- exp(x)/(1 + exp(x))
  return(invlogit_x)
}

tolowerSpacefill <- function(word, sep = "-") {
  small_word <- tolower(word)
  hyphenated_word <- gsub(" ", sep, small_word)
  return(hyphenated_word)
}

## COLORS YAY
candidate_labels <- c("biden" = "Joe Biden",
                      "bloomberg" = "Michael Bloomberg",
                      "booker" = "Cory Booker",
                      "buttigieg" = "Pete Buttigieg",
                      "klobuchar" = "Amy Klobuchar",
                      "sanders" = "Bernie Sanders",
                      "steyer" = "Tom Steyer",
                      "warren" = "Elizabeth Warren",
                      "yang" = "Andrew Yang")

candidate_colors <- c("biden" = "red",
                      "bloomberg" = "darkgray",
                      "booker" = "lawngreen",
                      "buttigieg" = "darkorange1",
                      "klobuchar" = "gold2",
                      "sanders" = "blue",
                      "steyer" = "deeppink4",
                      "warren" = "green4",
                      "yang" = "sienna4")

candidate_lastnames <- c("biden" = "Biden",
                         "bloomberg" = "Bloomberg",
                         "booker" = "Booker",
                         "buttigieg" = "Buttigieg",
                         "klobuchar" = "Klobuchar",
                         "sanders" = "Sanders",
                         "steyer" = "Steyer",
                         "warren" = "Warren",
                         "yang" = "Yang")

# Primaries schedule
primary_schedule <- read_csv("data/primary_schedule.csv")
