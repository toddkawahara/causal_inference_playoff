library(tidyverse)
library(baseballr)

# get runs per game pre sep
df_offense_presep <- tibble('season_batting_team_id' = character(), 'r_per_g_presep' = numeric())

# 2013
for (division in c('AL East', 'AL Central', 'AL West', 'NL East', 'NL Central', 'NL West')){
  year_date = paste0(as.character(2013), '-08-31')
  # Division status
  print(paste0('Division: ', (division)))
  
  df_standings <- bref_standings_on_date(date = year_date, division = division)
  df_standings1 <- df_standings %>%
    mutate(G = W + L,
           r_per_g_presep = RS/G,
           season_batting_team_id = paste0('2013_', Tm)) %>%
    select(season_batting_team_id, r_per_g_presep)
  
  df_offense_presep <- rbind(df_offense_presep, df_standings1)
}

write.csv(df_offense_presep, 'df_offense_presep.csv', row.names=FALSE)
df_offense_presep <- read.csv('df_offense_presep.csv', row.names = NULL)

# 2014
for (division in c('AL East', 'AL Central', 'AL West', 'NL East', 'NL Central', 'NL West')){
  year_date = paste0(as.character(2014), '-08-31')
  
  # Division status
  print(paste0('Division: ', (division)))
  
  df_standings <- bref_standings_on_date(date = year_date, division = division)
  df_standings1 <- df_standings %>%
    mutate(G = W + L,
           r_per_g_presep = RS/G,
           season_batting_team_id = paste0('2014_', Tm)) %>%
    select(season_batting_team_id, r_per_g_presep)
  
  df_offense_presep <- rbind(df_offense_presep, df_standings1)
}

write.csv(df_offense_presep, 'df_offense_presep.csv', row.names=FALSE)
df_offense_presep <- read.csv('df_offense_presep.csv', row.names = NULL)

# 2015
for (division in c('AL East', 'AL Central', 'AL West', 'NL East', 'NL Central', 'NL West')){
  year_date = paste0(as.character(2015), '-08-31')
  # Division status
  print(paste0('Division: ', (division)))
  
  df_standings <- bref_standings_on_date(date = year_date, division = division)
  df_standings1 <- df_standings %>%
    mutate(G = W + L,
           r_per_g_presep = RS/G,
           season_batting_team_id = paste0('2015_', Tm)) %>%
    select(season_batting_team_id, r_per_g_presep)
  
  df_offense_presep <- rbind(df_offense_presep, df_standings1)
}

write.csv(df_offense_presep, 'df_offense_presep.csv', row.names=FALSE)
df_offense_presep <- read.csv('df_offense_presep.csv', row.names = NULL)

# 2016
for (division in c('AL East', 'AL Central', 'AL West', 'NL East', 'NL Central', 'NL West')){
  year_date = paste0(as.character(2016), '-08-31')
  # Division status
  print(paste0('Division: ', (division)))
  
  df_standings <- bref_standings_on_date(date = year_date, division = division)
  df_standings1 <- df_standings %>%
    mutate(G = W + L,
           r_per_g_presep = RS/G,
           season_batting_team_id = paste0('2016_', Tm)) %>%
    select(season_batting_team_id, r_per_g_presep)
  
  df_offense_presep <- rbind(df_offense_presep, df_standings1)
}

write.csv(df_offense_presep, 'df_offense_presep.csv', row.names=FALSE)
df_offense_presep <- read.csv('df_offense_presep.csv', row.names = NULL)

# 2017
for (division in c('AL East', 'AL Central', 'AL West', 'NL East', 'NL Central', 'NL West')){
  year_date = paste0(as.character(2017), '-08-31')
  # Division status
  print(paste0('Division: ', (division)))
  
  df_standings <- bref_standings_on_date(date = year_date, division = division)
  df_standings1 <- df_standings %>%
    mutate(G = W + L,
           r_per_g_presep = RS/G,
           season_batting_team_id = paste0('2017_', Tm)) %>%
    select(season_batting_team_id, r_per_g_presep)
  
  df_offense_presep <- rbind(df_offense_presep, df_standings1)
}

write.csv(df_offense_presep, 'df_offense_presep.csv', row.names=FALSE)
df_offense_presep <- read.csv('df_offense_presep.csv', row.names = NULL)

# 2018
for (division in c('AL East', 'AL Central', 'AL West', 'NL East', 'NL Central', 'NL West')){
  year_date = paste0(as.character(2018), '-08-31')
  # Division status
  print(paste0('Division: ', (division)))
  
  df_standings <- bref_standings_on_date(date = year_date, division = division)
  df_standings1 <- df_standings %>%
    mutate(G = W + L,
           r_per_g_presep = RS/G,
           season_batting_team_id = paste0('2018_', Tm)) %>%
    select(season_batting_team_id, r_per_g_presep)
  
  df_offense_presep <- rbind(df_offense_presep, df_standings1)
}

write.csv(df_offense_presep, 'df_offense_presep.csv', row.names=FALSE)
df_offense_presep <- read.csv('df_offense_presep.csv', row.names = NULL)

# 2019
for (division in c('AL East', 'AL Central', 'AL West', 'NL East', 'NL Central', 'NL West')){
  year_date = paste0(as.character(2019), '-08-31')
  # Division status
  print(paste0('Division: ', (division)))
  
  df_standings <- bref_standings_on_date(date = year_date, division = division)
  df_standings1 <- df_standings %>%
    mutate(G = W + L,
           r_per_g_presep = RS/G,
           season_batting_team_id = paste0('2019_', Tm)) %>%
    select(season_batting_team_id, r_per_g_presep)
  
  df_offense_presep <- rbind(df_offense_presep, df_standings1)
}

write.csv(df_offense_presep, 'df_offense_presep.csv', row.names=FALSE)
df_offense_presep <- read.csv('df_offense_presep.csv', row.names = NULL)

# 2021
for (division in c('AL East', 'AL Central', 'AL West', 'NL East', 'NL Central', 'NL West')){
  year_date = paste0(as.character(2021), '-08-31')
  # Division status
  print(paste0('Division: ', (division)))
  
  df_standings <- bref_standings_on_date(date = year_date, division = division)
  df_standings1 <- df_standings %>%
    mutate(G = W + L,
           r_per_g_presep = RS/G,
           season_batting_team_id = paste0('2021_', Tm)) %>%
    select(season_batting_team_id, r_per_g_presep)
  
  df_offense_presep <- rbind(df_offense_presep, df_standings1)
}

write.csv(df_offense_presep, 'df_offense_presep.csv', row.names=FALSE)
df_offense_presep <- read.csv('df_offense_presep.csv', row.names = NULL)
  
# 2022
for (division in c('AL East', 'AL Central', 'AL West', 'NL East', 'NL Central', 'NL West')){
  year_date = paste0(as.character(2022), '-08-31')
  # Division status
  print(paste0('Division: ', (division)))
  
  df_standings <- bref_standings_on_date(date = year_date, division = division)
  df_standings1 <- df_standings %>%
    mutate(G = W + L,
           r_per_g_presep = RS/G,
           season_batting_team_id = paste0('2022_', Tm)) %>%
    select(season_batting_team_id, r_per_g_presep)
  
  df_offense_presep <- rbind(df_offense_presep, df_standings1)
}

write.csv(df_offense_presep, 'df_offense_presep.csv', row.names=FALSE)
df_offense_presep <- read.csv('df_offense_presep.csv', row.names = NULL)

# 2023
for (division in c('AL East', 'AL Central', 'AL West', 'NL East', 'NL Central', 'NL West')){
  year_date = paste0(as.character(2023), '-08-31')
  # Division status
  print(paste0('Division: ', (division)))
  
  df_standings <- bref_standings_on_date(date = year_date, division = division)
  df_standings1 <- df_standings %>%
    mutate(G = W + L,
           r_per_g_presep = RS/G,
           season_batting_team_id = paste0('2023_', Tm)) %>%
    select(season_batting_team_id, r_per_g_presep)
  
  df_offense_presep <- rbind(df_offense_presep, df_standings1)
}

write.csv(df_offense_presep, 'df_offense_presep.csv', row.names=FALSE)
df_offense_presep <- read.csv('df_offense_presep.csv', row.names = NULL)

# 2024
for (division in c('AL East', 'AL Central', 'AL West', 'NL East', 'NL Central', 'NL West')){
  year_date = paste0(as.character(2024), '-08-31')
  # Division status
  print(paste0('Division: ', (division)))
  
  df_standings <- bref_standings_on_date(date = year_date, division = division)
  df_standings1 <- df_standings %>%
    mutate(G = W + L,
           r_per_g_presep = RS/G,
           season_batting_team_id = paste0('2024_', Tm)) %>%
    select(season_batting_team_id, r_per_g_presep)
  
  df_offense_presep <- rbind(df_offense_presep, df_standings1)
}

write.csv(df_offense_presep, 'df_offense_presep.csv', row.names=FALSE)
df_offense_presep <- read.csv('df_offense_presep.csv', row.names = NULL)

