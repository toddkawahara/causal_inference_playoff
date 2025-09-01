library(tidyverse)
library(baseballr)

# Gets all regular season game logs for pitchers with at least 75 IP
df_regseason_games <- tibble(game_date = character(), 
                             home_team = character(),
                             team = character(), 
                             batting_team = character(), 
                             pitcher_name = character(), 
                             fg_playerid = integer(),
                             hr = numeric(),
                             bb = numeric(), 
                             hbp = numeric(), 
                             so = numeric(), 
                             ip = numeric(), 
                             year = integer(), 
                             fip = numeric(), 
                             playoff_game = numeric(),
                             home_away = character())

for (year_iter in 2013:2024){
  if (year_iter == 2020){
    next
  }
  # Year status
  print(paste0('Year: ', (year_iter)))
  
  # Get qualifiers in year
  active_p <- fg_pitcher_leaders(startseason = year_iter, endseason = year_iter, qual = 75)
  
  # Get game logs for qualifying pitchers
  df_all_pitcher_logs <- data.frame()
  for (i in 1:nrow(active_p)) {
    df_individ_pitcher_logs <- fg_pitcher_game_logs(active_p$playerid[i], year_iter) %>%
      filter(GS == 1) # only query starts
    df_all_pitcher_logs <- rbind(df_all_pitcher_logs, df_individ_pitcher_logs, fill=TRUE)
  }
  df_all_pitcher_logs <- df_all_pitcher_logs[df_all_pitcher_logs$PlayerName != TRUE, ]
  
  # Clean df
  df_all_pitcher_logs <- df_all_pitcher_logs %>%
    rename(fg_playerid = playerid) %>%
    mutate(Opp = str_replace_all(Opp, "@", ""), 
           BB = BB + IBB, 
           playoff_game = 0) %>%
    mutate(home_team = case_when(
      HomeAway == 'H' ~ Team, 
      TRUE ~ Opp
    )) %>%
    select(c(Date, home_team, Team, Opp, PlayerName, fg_playerid, HR, BB, HBP, SO, IP, season, FIP, playoff_game, HomeAway)) %>%
    rename(game_date = Date, 
           team = Team, 
           batting_team = Opp, 
           pitcher_name = PlayerName, 
           hr = HR,
           bb = BB, 
           hbp = HBP, 
           so = SO, 
           ip = IP, 
           year = season, 
           fip = FIP, 
           home_away = HomeAway)
  
  # Append
  df_regseason_games <- rbind(df_regseason_games, df_all_pitcher_logs)
}
write.csv(df_regseason_games, 'df_regseason_games.csv')




