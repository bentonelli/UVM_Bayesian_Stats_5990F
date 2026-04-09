// Autoregressive model. Adapted from: https://mc-stan.org/docs/stan-users-guide/time-series.html#autoregressive.section

data {
  int<lower=0> N;
  
  int<lower=0> N_obs;
  int<lower=0> N_mis;
  array[N_obs] int<lower=1, upper=N_obs + N_mis> abd_obs;
  array[N_mis] int<lower=1, upper=N_obs + N_mis> abd_mis;
  
  vector[N] temp;
  array[N_obs] real yy_obs;
}
parameters {
  real alpha;
  real beta;
  real theta;
  real<lower=0> sigma;
  array[N_mis] real yy_mis; //Note that we do not set a prior on these
}

transformed parameters {
  vector[N] yy; //We create a vector of our abundance values here
  yy[abd_obs] = to_vector(yy_obs);  //Puts observed values where they belong
  yy[abd_mis] = to_vector(yy_mis); // Uses a vector of unknown values as a placeholder
}

model {
  
  alpha ~ normal(300,200);
  beta ~ normal(0,10);
  theta ~ normal(0,10);
  
  yy[1] ~ normal(250,50); //First abundance value (unknown)
  yy[2:N] ~ normal(alpha + beta * temp[2:N] + theta * yy[1:(N-1)], sigma);
}

