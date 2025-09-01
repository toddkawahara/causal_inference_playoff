library(tidyverse)
library(baseballr)

# Generating postseason game logs
df_playoff_games <- tibble('game_pk' = numeric(), 'game_date' = character(), 'fielding_team' = character(), 
                           'batting_team' = character(), 'matchup.pitcher.fullName' = character(), 
                           'matchup.pitcher.id' = integer(), 'outs' = numeric(), 'hr' = numeric(), 
                           'bb' = numeric(), 'hbp' = numeric(), 'so' = numeric(), 'ip' = numeric(), 
                           'year' = character(), 'fip_c' = numeric(), 'fip' = numeric(), 'playoff_game' = numeric())
# Loop through 2013 - 2024 seasons (excluding 2020)
for (year_iter in 2013:2024){
  if (year_iter == 2020){
    next
  }
  # Year status
  print(paste0('Year: ', (year_iter)))
  # get postseason games in year
  df_post <- mlb_schedule_postseason(year_iter)
  game_pks <- df_post$game_pk
  
  # Loop through all games in postseason year
  for (game in game_pks){
    # Get probable starters from game pk
    df_prob_starters <- mlb_probables(game)
    
    # Get pbp data
    df_pbp <- mlb_pbp(game)
    
    # Generate game log from pbp data
    df_game <- df_pbp %>%
      filter(matchup.pitcher.id %in% df_prob_starters$id) %>%
      distinct(atBatIndex, .keep_all = TRUE) %>%
      select(c('game_pk', 'game_date', 'playId', 'matchup.batter.id', 'home_team',
               'away_team', 'result.awayScore', 'home_team', 'result.homeScore', 'batting_team', 'fielding_team',
               'matchup.batter.fullName', 'matchup.pitcher.id', 'matchup.pitcher.fullName', 'result.eventType', 'result.description')) %>%
      # generate event cols
      mutate(
        outs = case_when(
          str_detect(result.eventType, 'triple_play') ~ 3, 
          str_detect(result.eventType, 'double_play') ~ 2, 
          str_detect(result.eventType, 'out') ~ 1, 
          str_detect(result.eventType, 'fielders_choice') ~ 1, 
          str_detect(result.eventType, 'caught_stealing') ~ 1, 
          str_detect(result.eventType, 'sac') ~ 1,
          str_detect(result.eventType, 'pickoff') ~ 1, 
          TRUE ~ 0
        ), 
        hr = case_when(
          result.eventType == 'home_run' ~ 1,
          TRUE ~ 0
        ), 
        bb = case_when(
          str_detect(result.eventType, 'walk') ~ 1, 
          TRUE ~ 0
        ), 
        hbp = case_when(
          result.eventType == 'hit_by_pitch' ~ 1, 
          TRUE ~ 0
        ),
        so = case_when(
          str_detect(result.eventType, 'strikeout') ~ 1, 
          TRUE ~ 0
        )
      ) %>%
      # summarize game logs
      group_by(game_pk, game_date, home_team, fielding_team, batting_team, matchup.pitcher.fullName, matchup.pitcher.id) %>%
      summarise(outs = sum(outs), 
                hr = sum(hr), 
                bb = sum(bb), 
                hbp = sum(hbp),
                so = sum(so), 
                .groups = 'keep') %>%
      # generate fip
      mutate(ip = floor(outs/3) +
               case_when(
                 outs %% 3 == 1 ~ .33, 
                 outs %% 3 == 2 ~ .67, 
                 TRUE ~ 0
               ), 
             year = substr(game_date, 1, 4), 
             fip_c = case_when(
               year == '2024' ~ 3.166, 
               year == '2023' ~ 3.255, 
               year == '2022' ~ 3.112, 
               year == '2021' ~ 3.170, 
               year == '2019' ~ 3.214, 
               year == '2018' ~ 3.160, 
               year == '2017' ~ 3.158, 
               year == '2016' ~ 3.147, 
               year == '2015' ~ 3.134, 
               year == '2014' ~ 3.132, 
               year == '2013' ~ 3.048, 
             ),
             fip = (13*hr + 3*(bb+hbp) - 2*so)/ip + fip_c,
             playoff_game = 1
             )
    
    # concat to playoff gamelog df
    df_playoff_games <- rbind(df_playoff_games, df_game)
    
    # Game status
    print(paste0(as.character(game), ' game log collected'))
  }
}

# Cleaning df, adding cols
df_playoff_games <- df_playoff_games %>%
  mutate(HomeAway = case_when(
    fielding_team == home_team ~ 'H', 
    TRUE ~ 'A'
  )) %>%
  ungroup() %>%
  select(-c(fip_c, game_pk, outs)) %>%
  rename(team = fielding_team, 
         pitcher_name = matchup.pitcher.fullName, 
         mlbam_playerid = matchup.pitcher.id, 
         home_away = HomeAway)
  
write.csv(df_playoff_games, 'df_playoff_games.csv')
  
  
  