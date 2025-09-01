data {
  int<lower=1> N; // number of games
  int<lower=1> P; // number of unique pitchers
  array[N] int<lower=1,upper=P> pid; // pitcher id for each game
  vector[N] playoff_game; // regular season or playoff game
  vector[N] fip_presep; // pre september fip
  vector[N] fip; // fip for each game
  vector[N] offense; // pre september offense runs per game
  vector[N] home; // home or away game
  int<lower=0,upper=1> priorOnly; // for prior predictvie
}


parameters {
  // fixed effects
  real beta0; // population level intercept
  real beta_fip_presep; // pre sep fip beta
  real beta_offense; // pre sep offense r/g beta
  real beta_home; // home or away beta
  
  // random intercept
  real player_mu_intercept; // global mean
  real<lower=0> player_sigma_intercept; // global sd
  vector[P] z_intercept; // non-centering
  
  // random slope for playoff effect
  real player_playoff_mu_slope; // global mean 
  real<lower=0> player_playoff_sigma_slope; // global sd
  vector[P] z_slope; // non-centering
  
  real<lower=0> sigma; // residual SD
  real<lower=1> nu; // deg freedom for t distribution
}


transformed parameters {
  vector[P] player_intercept = player_mu_intercept + z_intercept*player_sigma_intercept; // random intercept
  vector[P] player_playoff_slope = player_playoff_mu_slope + z_slope*player_playoff_sigma_slope; // random slopes for playoff effect
}


model {
  // Hyperpriors for random effects
  player_playoff_mu_slope ~ normal(0, 0.5);
  player_playoff_sigma_slope ~ normal(0, 1);
  z_slope ~ normal(0, 1);
  
  player_mu_intercept ~ normal(0, 0.5);
  player_sigma_intercept ~ normal(0, 1);
  z_intercept ~ normal(0, 1);
  
  // Priors
  beta0 ~ normal(3, 1.5);
  beta_fip_presep ~ normal(0, 1);
  beta_offense ~ normal(0, 1);
  beta_home ~ normal(0, 1);
  sigma ~ normal(0, 1.5);
  nu ~ gamma(2, 0.1);
  
  // Likelihood
  if (!priorOnly){
    vector[N] mu = beta0 + beta_fip_presep*fip_presep + beta_offense*offense + beta_home*home + player_intercept[pid] + player_playoff_slope[pid].*playoff_game;
    fip ~ student_t(nu, mu, sigma);
  }
}


generated quantities{
  vector[N] mu = beta0 + beta_fip_presep*fip_presep + beta_offense*offense + beta_home*home + player_intercept[pid]+ player_playoff_slope[pid].*playoff_game;
  vector[N] fip_rep; // predictive
  vector[N] log_lik; // for loo
  
  for (n in 1:N){
    fip_rep[n] = student_t_rng(nu, mu[n], sigma);
    log_lik[n] = student_t_lpdf(fip[n] | nu, mu[n], sigma); // for loo
  }
}
