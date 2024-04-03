#### Preamble ####
# Purpose: Simulates the data for deaths attributed to cancer for 5 hospitals
# Author: Alaina Hu 
# Date: 1 April 2024 
# Contact: alaina.hu@utoronto.ca 
# License: MIT
# Pre-requisites: NA
# Any other information needed? NA


#### Workspace setup ####

library(tidyverse)
library(ggplot2)


#### Simulate data ####
set.seed(853)

age_groups <- c("<40", "40-60", ">60")
cancer_types <- c("Breast", "Lung", "Prostate", "Colorectal", "Skin")
number_of_years <- 20
start_mean <- 80 
decrease_per_year <- 2 
sd_deaths <- 2 

mean_deaths_per_year <- start_mean - (0:(number_of_years - 1)) * decrease_per_year

simulated_data <-
  tibble(
    hospital = rep(paste("Hospital", 1:5), each = number_of_years),
    year = rep(2003:(2002 + number_of_years), times = 5),
    age_group = sample(age_groups, size = 100, replace = TRUE),
    cancer_type = sample(cancer_types, size = 100, replace = TRUE),
    deaths = round(rnorm(n = number_of_years * 5, mean = rep(mean_deaths_per_year, each = 5), sd = sd_deaths))
  )

simulated_data_sorted <- simulated_data %>%
  arrange(year)

head(simulated_data_sorted)

#### Plot data ####
simulated_data_sorted |>
  ggplot(aes(x = year, y = deaths, color = hospital)) +
  geom_point() +
  theme_minimal() +
  labs(x = "Year", y = "Cancer Deaths", color = "Hospital") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "bottom")

#### Test data ####
hospital_names <- c("Hospital 1", "Hospital 2", "Hospital 3", "Hospital 4", "Hospital 5")

simulated_data_sorted$hospital |>
  unique() %in% hospital_names

simulated_data_sorted$hospital |> class() == "character"

simulated_data_sorted$age_group |> unique() %in% age_groups

simulated_data_sorted$cancer_type |> unique() %in% cancer_types

simulated_data_sorted$year |> class() == "integer"
simulated_data_sorted$year |> min() == 2003
simulated_data_sorted$year |> max() == 2023

simulated_data_sorted$year |> unique() |> length() == 20

simulated_data_sorted$deaths |> class() == "numeric"
simulated_data_sorted$deaths |> min() >= 0
simulated_data_sorted$deaths |> max() <= 200

simulated_data_sorted$hospital |> length() == 100

#### Model ####
library(rstanarm)

model <-
  stan_glm(
    deaths ~ age_group,
    data = simulated_data_sorted,
    family = neg_binomial_2(link = "log"),
    prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
    prior_intercept = normal(location = 0, scale = 2.5, autoscale = TRUE),
    seed = 853
  )

saveRDS(
  model,
  file = "model.rds"
)

