library(cmdstanr)
library(posterior)
library(bayesplot)
library(dplyr)
library(stringi)
library(ggplot2)

# load data
df_gamelogs <- read.csv('df_gamelogs.csv')
# standardize data, factor ids
df_gamelogs <- df_gamelogs %>%
  mutate(fip_presep_std = scale(fip_presep)[,1], 
         id = as.numeric(factor(mlbam_playerid)))

# load stan models
mod_tdist <- cmdstan_model('model_tdist.stan')
mod_skew <- cmdstan_model('model_skew.stan')

# ---- Prior only ----
data_list <- list(
  N = nrow(df_gamelogs), 
  P = length(unique(df_gamelogs$id)),
  pid = df_gamelogs$id, 
  playoff_game = df_gamelogs$playoff_game, 
  fip_presep = df_gamelogs$fip_presep_std,
  fip = df_gamelogs$fip,
  priorOnly = 1
)
prior_fit_tdist <- mod_tdist$sample(
  data = data_list, 
  chains = 4, 
  iter_sampling = 1000, 
  iter_warmup = 1000, 
  parallel_chains = 4
)

# prior predictive check
yrep_prior <- prior_fit_tdist$draws("fip_rep", format = "matrix")
y_obs <- df_gamelogs$fip
# sample 100 time from each observation
pp_check(y_obs, yrep_prior[1:100, ], fun = "dens_overlay") +
  ggtitle("Prior Predictive Check: Density Overlay") +
  xlim(-5, 15) +
  ylim(0, 1) 





# ---- Fitting with fip_presep and playoff game ----
data_list <- list(
  N = nrow(df_gamelogs), 
  P = length(unique(df_gamelogs$id)),
  pid = df_gamelogs$id, 
  playoff_game = df_gamelogs$playoff_game, 
  fip_presep = df_gamelogs$fip_presep_std,
  fip = df_gamelogs$fip,
  priorOnly = 0
)
posterior_fit_skew <- mod_skew$sample(
  data = data_list, 
  chains = 4, 
  iter_sampling = 1000, 
  iter_warmup = 1000, 
  parallel_chains = 4
)


posterior_fit_tdist <- mod_tdist$sample(
  data = data_list, 
  chains = 4, 
  iter_sampling = 1000, 
  iter_warmup = 1000, 
  parallel_chains = 4
)

# Calculate LOO for each model
loo_tdist <- posterior_fit_tdist$loo()$estimates['elpd_loo', 'Estimate']
loo_skew <- posterior_fit_skew$loo()$estimates['elpd_loo', 'Estimate']
elpd_tdist <- loo_tdist$estimates['elpd_loo', 'Estimate'] # -18706
elpd_skew <- loo_skew$estimates['elpd_loo', 'Estimate'] # -19398




# checking parameter fit
posterior_fit_tdist$summary(c("beta0", "beta_fip_presep", "player_mu_intercept", 'player_sigma_intercept', 
                        "player_playoff_mu_slope", 'player_playoff_sigma_slope', 'sigma', 'alpha'))

# posterior predictive check
yrep_post <- posterior_fit_tdist$draws("fip_rep", format = "matrix")
y_obs <- df_gamelogs$fip
# sample 100 time from each observation
pp_check(y_obs, yrep_post[1:100, ], fun = "dens_overlay") +
  ggtitle("Posterior Predictive Check: Density Overlay") +
  xlim(-5, 15) +
  ylim(0, 0.5)

# Get summary of all random intercepts
player_intercept_summary <- posterior_fit_tdist$summary("player_intercept")
player_intercept_summary <- player_intercept_summary %>%
  mutate(pitcher_id = 1:nrow(player_intercept_summary)) %>%
  left_join(df_gamelogs %>% select(c(pitcher_name, id)) %>% distinct(pitcher_name, id), 
            by = c('pitcher_id' = 'id'))
# Find 5 highest mean slopes (pitch worse than FIP)
highest_slopes <- player_intercept_summary %>%
  arrange(desc(mean)) %>%
  head(5)
print(highest_slopes[, c("variable", "pitcher_id", 'pitcher_name', "mean", "q5", "q95")])
# Find 5 lowest mean slopes (pitch better than FIP)  
lowest_slopes <- player_intercept_summary %>%
  arrange(mean) %>%
  head(5)
print(lowest_slopes[, c("variable", "pitcher_id", 'pitcher_name', "mean", "q5", "q95")])

# Get summary of all random slopes
player_playoff_slope_summary <- posterior_fit_tdist$summary("player_playoff_slope")
player_playoff_slope_summary <- player_playoff_slope_summary %>%
  mutate(pitcher_id = 1:nrow(player_playoff_slope_summary)) %>%
  left_join(df_gamelogs %>% select(c(pitcher_name, id)) %>% distinct(pitcher_name, id), 
            by = c('pitcher_id' = 'id'))
# Find 5 highest mean slopes (pitch worse in playoffs)
highest_slopes <- player_playoff_slope_summary %>%
  arrange(desc(mean)) %>%
  head(5)
print(highest_slopes[, c("variable", "pitcher_id", 'pitcher_name', "mean", "q5", "q95")])
# Find 5 lowest mean slopes (pitch better in playoffs)  
lowest_slopes <- player_playoff_slope_summary %>%
  arrange(mean) %>%
  head(5)
print(lowest_slopes[, c("variable", "pitcher_id", 'pitcher_name', "mean", "q5", "q95")])


# ---- adding new offense and home/away ----

# load offense df
df_offense_presep <- read.csv('df_offense_presep.csv')
# merge offense, standardize, create home col
df_gamelogs <- merge(df_gamelogs, df_offense_presep, on='season_batting_team_id', all.x = TRUE) %>%
  mutate(r_per_g_presep_std = scale(r_per_g_presep)[,1], 
         home = ifelse(home_away == 'H', 1, 0))

# load stan model
mod_tdist_off_home <- cmdstan_model('model_tdist_off_home.stan')

# prior data list
data_list <- list(
  N = nrow(df_gamelogs), 
  P = length(unique(df_gamelogs$id)),
  pid = df_gamelogs$id, 
  playoff_game = df_gamelogs$playoff_game, 
  fip_presep = df_gamelogs$fip_presep_std,
  fip = df_gamelogs$fip,
  offense = df_gamelogs$r_per_g_presep_std, 
  home = df_gamelogs$home,
  priorOnly = 1
)

# data list
prior_fit_tdist_off_home <- mod_tdist_off_home$sample(
  data = data_list, 
  chains = 4, 
  iter_sampling = 1000, 
  iter_warmup = 1000, 
  parallel_chains = 4
)

# prior predictive check
yrep_prior <- prior_fit_tdist_off_home$draws("fip_rep", format = "matrix")
y_obs <- df_gamelogs$fip
# sample 100 time from each observation
pp_check(y_obs, yrep_prior[1:100, ], fun = "dens_overlay") +
  ggtitle("Prior Predictive Check: Density Overlay") +
  xlim(-5, 15) +
  ylim(0, 1) 

# posterior data list
data_list <- list(
  N = nrow(df_gamelogs), 
  P = length(unique(df_gamelogs$id)),
  pid = df_gamelogs$id, 
  playoff_game = df_gamelogs$playoff_game, 
  fip_presep = df_gamelogs$fip_presep_std,
  fip = df_gamelogs$fip,
  offense = df_gamelogs$r_per_g_presep_std, 
  home = df_gamelogs$home,
  priorOnly = 0
  
)

# posterior
posterior_fit_tdist_off_home <- mod_tdist_off_home$sample(
  data = data_list, 
  chains = 4, 
  iter_sampling = 1000, 
  iter_warmup = 1000, 
  parallel_chains = 4
)


# checking parameter fit
posterior_fit_tdist_off_home$summary(c("beta0", "beta_fip_presep", 'beta_offense', 'beta_home', "player_mu_intercept", 'player_sigma_intercept', 
                              "player_playoff_mu_slope", 'player_playoff_sigma_slope', 'sigma', 'nu'))

draws_array <- posterior::as_draws_array(posterior_fit_tdist_off_home)
mcmc_trace(draws_array, pars = (c("beta0", "beta_fip_presep", 'beta_offense', 'beta_home', "player_mu_intercept", 'player_sigma_intercept', 
                                  "player_playoff_mu_slope", 'player_playoff_sigma_slope', 'sigma', 'nu')))

# posterior predictive check
yrep_post <- posterior_fit_tdist_off_home$draws("fip_rep", format = "matrix")
y_obs <- df_gamelogs$fip
# sample 100 time from each observation
pp_check(y_obs, yrep_post[1:100, ], fun = "dens_overlay") +
  ggtitle("Posterior Predictive Check: Density Overlay") +
  xlim(-5, 15) +
  ylim(0, 0.5)


# Get summary of all random intercepts (pitch better/worse than pre sep FIP)
player_intercept_summary <- posterior_fit_tdist_off_home$summary("player_intercept")
player_intercept_summary <- player_intercept_summary %>%
  mutate(pitcher_id = 1:nrow(player_intercept_summary)) %>%
  left_join(df_gamelogs %>% select(c(pitcher_name, id)) %>% distinct(pitcher_name, id), 
            by = c('pitcher_id' = 'id'))
# Find 5 highest mean slopes (pitch worse than FIP)
highest_slopes <- player_intercept_summary %>%
  arrange(desc(mean)) %>%
  head(5)
print(highest_slopes[, c("variable", "pitcher_id", 'pitcher_name', "mean", "q5", "q95")])
# Find 5 lowest mean slopes (pitch better than FIP)  
lowest_slopes <- player_intercept_summary %>%
  arrange(mean) %>%
  head(5)
print(lowest_slopes[, c("variable", "pitcher_id", 'pitcher_name', "mean", "q5", "q95")])


# Get summary of all random slopes (pitch better/worse in playoffs)
player_playoff_slope_summary <- posterior_fit_tdist_off_home$summary("player_playoff_slope")
player_playoff_slope_summary <- player_playoff_slope_summary %>%
  mutate(pitcher_id = 1:nrow(player_playoff_slope_summary)) %>%
  left_join(df_gamelogs %>% select(c(pitcher_name, id)) %>% distinct(pitcher_name, id), 
            by = c('pitcher_id' = 'id'))
# Find 5 highest mean slopes (pitch worse in playoffs)
highest_slopes <- player_playoff_slope_summary %>%
  arrange(desc(mean)) %>%
  head(5)
print(highest_slopes[, c("variable", "pitcher_id", 'pitcher_name', "mean", "q5", "q95")])
# Find 5 lowest mean slopes (pitch better in playoffs)  
lowest_slopes <- player_playoff_slope_summary %>%
  arrange(mean) %>%
  head(5)
print(lowest_slopes[, c("variable", "pitcher_id", 'pitcher_name', "mean", "q5", "q95")])


# global posterior distribution of playoff effect
posterior <- as_draws_df(posterior_fit_tdist_off_home)
mu_playoff <- posterior$player_playoff_mu_slope
sigma_playoff <- posterior$player_playoff_sigma_slope
set.seed(123)
new_slopes <- rnorm(length(mu_playoff), mu_playoff, sigma_playoff)
# Compute 95% credible interval
ci <- quantile(new_slopes, probs = c(0.05, 0.95))
# plot posterior predictive distribution of slopes
ggplot(data.frame(new_slopes), aes(x=new_slopes)) +
  geom_density(alpha=0.7, fill = 'skyblue') +
  geom_vline(xintercept = ci, linetype = "dashed", color = "red") +
  labs(title="Global posterior predictive density distribution of playoff slopes",
       subtitle = paste0("90% CI: [", round(ci[1], 3), ", ", round(ci[2], 3), "]"),
       x="Global Playoff Slope", y="Density") +
  theme_classic()

# best/worst pitchers
playoff_slopes <- rbind(highest_slopes, lowest_slopes) %>%
  arrange(mean, ascending = TRUE)
ggplot(playoff_slopes, aes(x = mean, y = reorder(pitcher_name, mean))) +
  geom_point(color = "blue", size = 2) +
  geom_errorbarh(aes(xmin = q5, xmax = q95), height = 0.2, color = "gray40") +
  labs(
    title = "2013-2024 (minus 2020) Five Worst/Best Pitcher Playoff Slopes",
    subtitle = "90% Credible Intervals",
    x = "Posterior Mean (Playoff Slope)",
    y = "Pitcher"
  ) +
  theme_classic()

# kershaw
kersh <- df_gamelogs %>%
  filter(pitcher_name == 'Clayton Kershaw')

kersh_draws <- posterior$`player_playoff_slope[111]`
ci <- quantile(kersh_draws, probs = c(0.05, 0.95))
ggplot(data.frame(slope = kersh_draws), aes(x = slope)) +
  geom_density(fill = "skyblue", alpha = 0.7) +
  geom_vline(xintercept = ci, linetype = "dashed", color = "red") +
  labs(
    title = "2013-2024 (minus 2020) Clayton Kershaw Posterior Playoff Slope Distribution",
    subtitle = paste0("90% CI: [", round(ci[1], 3), ", ", round(ci[2], 3), "]"),
    x = "Playoff Slope",
    y = "Density"
  ) +
  theme_classic()
  
  
  
  
  
  
  

