# 5 - Hierarchical Models

library(cmdstanr)
library(MCMCvis)
library(shinystan)
library(ggplot2)
library(dplyr)

### Introduction ####

#' The goal of today's lesson is to look at how the spring arrival timing of migratory birds
#' is changing over time. It has been shown that arrival dates are generally advancing
#' for most species, but there is considerable variation across species. 

# Read in our dataset. Data adapted from: https://github.com/bentonelli/Nonstationarity_pheno_climate

spring_arrivals <- readRDS("data/spring_arrivals.rds")

#Take a look at the structure
head(spring_arrivals)

#' Our dataset consists of estimated arrival dates, represented as ordinal dates 
#' (1 = Jan. 1, 2= Jan. 2, etc.)
#' 
#' Note that these are estimated arrival dates, so they end up being numbers with decimals.
#' 
#' Our dataset has 12 species, each with arrival dates from different areas (represented numerically)

# Look at how many data points each species has
table(spring_arrivals$species)

#' We're going to start with a simpler version of the model, with a paired down version of our
#' data. Let's first filter just to one species

species_data <- spring_arrivals %>% 
  filter(species == "Acadian Flycatcher") 

#' Because areas are defined by large, non-sequential numbers, we're going to go ahead
#' and reformat them so that each area gets a number between 1 and the number of areas
#' in our dataset.

species_data$rf_area <- as.numeric(as.factor(species_data$area))

# Let's also add a standardized year column, where 0 is the first year of the study
species_data$std_year <- species_data$year-min(species_data$year)

#Take a look at the new data
head(species_data)

### Prepare data ####
input_data <- list(
  N = nrow(species_data),
  yy = species_data$arr_est,
  year = species_data$std_year,
  N_areas = length(unique(species_data$rf_area)),
  area = as.numeric(species_data$rf_area)
)

### In-class to-dos ####
#' TODO (1): Before running the model, look at the stan code, and convert the code
#' to handwritten equations.
#' 
#' TODO (2): Write out the interpretation of each parameter in the model.
#' 
#' TODO (3): Run the model and plot out the alphas. Talk to a neighbor about what the alphas 
#' represent, then check in with Ben.

### Homework to-dos ####
#' TODO (4): Next, adapt your written equations from step 1-2 to add a new parameter
#' that represents the effect of year on arrival date. Should you have one slope,
#' or one slope for each location? There is no right answer, but explain the 
#' advantages of the approach you choose. If you choose to model a slope for each location,
#' make sure to model those slopes hierarchically.
#' 
#' TODO (5): Update your model code and run again. Plot out your new results.
#' 
#' TODO (6): Change the target species from "Acadian Flycatcher" to another
#' species in the dataset. Rerun your model. Do you get the similar results for the
#' effect of year? Repeat this process for at least 4 species. Report the species 
#' you tried and your results for the year effect below.

### Run model ####
mod <- cmdstan_model("Stan_models/S5_HierarchicalModels.stan")

stan_model <- mod$sample(
  data = input_data,
  iter_warmup = 3000,
  iter_sampling = 5000,
  adapt_delta = .8,
  chains = 4,
  parallel_chains = 4,
  refresh = 500 
)

MCMCsummary(stan_model)

MCMCplot(stan_model,params = "alpha")
