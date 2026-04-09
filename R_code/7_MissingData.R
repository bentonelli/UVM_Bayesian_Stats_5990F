# Missing Data

library(cmdstanr)
library(MCMCvis)
library(shinystan)
library(dplyr)


#### INTRODUCTION ####

#' Missing data can be a huge pain. Often, the easiest way out when faced with
#' missing data is simply to throw out observations. Today, we are going to learn how to 
#' model missing data as if it is a parameter. This can open up possibilities if your
#' data is incomplete. Although this worksheet will focus on missing dependent variables (yys)
#' there are also ways to model missing independent variables.
#' 
#' This week we will return to simulating data. This simulated dataset will represent
#' a time series of insect populations. Insect populations can be density-dependent, 
#' with large populations in one year often followed by small populations in the next.
#' 
#' Let's imagine we've been surveying moth populations for the last 50 or so years.
#' Our moths tend to do worse when minimum winter temperatures are lower, and that populations 
#' oscillate due to density effects. Let's imagine we survey populations in the summer.
#' 
#' 

### CREATE DATA - RUN THIS SECTION ####
#'Feel free to ignore the details of this section, it is not important you understand
#'what is going on here.
ts_length <- 51

set.seed(200)
min_temp <- rnorm(ts_length,0,5)

min_temp_eff <- 5
carrying_capacity <- 275
auto_regressive_effect <- -1.75

insect_abundance_record <- rep(NA,ts_length)
starting_abundance <- 280
for(nn in 0:(ts_length-1)){
  if(nn == 0){
    insect_abundance <- starting_abundance
  } else {
    insect_abundance <- insect_abundance + min_temp_eff*min_temp[nn] + (insect_abundance-275)*auto_regressive_effect + rnorm(1,0,10)
  }
  insect_abundance_record[nn+1] <- insect_abundance
}

plot(1976:2026,insect_abundance_record,type="l")
plot(min_temp,insect_abundance_record[1:ts_length])


insect_df <- data.frame(year = 1976:2026,min_temp=c(min_temp),insect_abd = insect_abundance_record)

### DATA VIZ ####

#Let's visualize the data
plot(insect_df$year,insect_df$insect_abd,type="l",col="skyblue4",lwd=4,xlab="Year",ylab="Insect Abundance")
plot(insect_df$min_temp,insect_df$insect_abd,col="firebrick3",cex=1,pch=19,
     xlab="Minimum Temperature",ylab="Insect Abundance")
plot(insect_abundance_record[1:(ts_length-1)],insect_abundance_record[2:(ts_length)],
     col="forestgreen",cex=1,pch=19,xlab="Minimum Temperature",ylab="Insect Abundance")

#' Before we run our model, let's adapt our data frame to pretend we don't have
#' data from the year before we started sampling(1976), during covid (2020), or the current 
#' year (2026). 
#' 

missing_year_ind <- c(1,45,51)
known_abd_vals <- insect_df$insect_abd[missing_year_ind] # Save population values for later
insect_df$insect_abd[missing_year_ind] <- NA # Replace these years' data with NA

# TODO(1) - What do the next two lines do in general terms? Write your answer below.

abd_mis <- which(is.na(insect_df$insect_abd))
abd_obs <- which(!is.na(insect_df$insect_abd))

#' TODO(2) - To the right of each item below, write a comment briefly describing 
#' what each of these data components represent. Feel free to look at the stan
#' model for context.

### Prepare data ####
input_data <- list(
  N = nrow(insect_df), # Number of years of data to model (known and unknown)
  N_obs = length(abd_obs),
  N_mis = length(abd_mis),
  
  abd_obs = abd_obs,
  abd_mis = abd_mis, 
  
  temp = insect_df$min_temp,
  yy_obs = insect_df$insect_abd[!is.na(insect_df$insect_abd)]
)

### Run model ####

#' TODO (3) If you haven't already, take a look at the stan model. What does yy[1]
#' represent? Why are we modeling it separately? Does the current prior make sense?
#' Discuss with a neighbor and then check in with Ben.

# CHANGE
mod <- cmdstan_model("Stan_models/S7_MissingData.stan")

stan_model <- mod$sample(
  data = input_data,
  iter_warmup = 4000,
  iter_sampling = 8000,
  adapt_delta = .8,
  chains = 4,
  parallel_chains = 4,
  refresh = 500 
)

# Plot the regression coeffecient estimates
MCMCsummary(stan_model,params = c("alpha","beta","theta","sigma"))

#Plot the abundance estimates by year
MCMCplot(stan_model,params="yy",horiz = FALSE,labels=1976:2026)

#' TODO (4) Why are most of these yys represented with dots, while three have credible intervals (CrI)?
#' 
#' TODO (5) Compare the estimated yys with the actual values (saved above as "known_abd_vals").
#' How does the model do at estimating these values? Why is the CrI for the first point not representative 
#' of the prior value we set up in the model (in other words, why doesn't it have a larger CrI)?
#' 
#' TODO (6) Replace 10 consecutive abundance values in the middle of the time series, and run the model again.
#' How does the model do at estimating these abundance values?
