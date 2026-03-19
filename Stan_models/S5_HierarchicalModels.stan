// Hierarchical Model

data {
  int<lower=0> N;
  vector[N] yy; // Arrival date
  vector[N] year; // Year - not needed until todo #4
  int<lower=0> N_areas; // The total number of areas
  array[N] int<lower=1,upper=N_areas> area; // Area index (1:N_areas)
}

parameters {
  real mu_alpha; 
  real<lower=0> sigma_alpha;
  
  vector[N_areas] alpha;
  real<lower=0> sigma;
}

model {
  
  mu_alpha ~ normal(125,50); // Think about this prior, why is it set up like this?
  sigma_alpha ~ lognormal(2,1); // Lognormal distributions are another postivie only distribution.
  
  alpha ~ normal(mu_alpha,sigma_alpha);
  sigma ~ lognormal(1,1);
  
  yy ~ normal(alpha[area], sigma); // Note the indexing here.
  
}

