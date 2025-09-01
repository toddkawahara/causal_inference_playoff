#!/usr/bin/env python
# coding: utf-8

# In[2]:


import pandas as pd
import numpy as np
import polars as pl
from unidecode import unidecode
import pybaseball
from pybaseball import playerid_reverse_lookup


# In[3]:


df_playoffs = pl.read_csv('/Users/toddk/OneDrive/Desktop/R_projects/causal_inference_playoff/df_playoff_games.csv')
df_regseason = pl.read_csv('/Users/toddk/OneDrive/Desktop/R_projects/causal_inference_playoff/df_regseason_games.csv', ignore_errors = True)


# In[4]:


# getting fg ids needed
fg_ids = df_regseason.select(pl.col('fg_playerid')).unique().to_series().to_list()

# finding bam ids to fg ids
id_matches = playerid_reverse_lookup(fg_ids, key_type='fangraphs')
id_matches = pl.from_pandas(id_matches).select(pl.col('key_mlbam'), pl.col('key_fangraphs'))

# joining mlbams to fg ids
df_regseason = df_regseason.join(id_matches, how='left', left_on = 'fg_playerid', right_on = 'key_fangraphs')


# In[5]:


# mapping teams to abbreviation
team_mapping = {
    'Arizona Diamondbacks': 'ARI', 
    'Atlanta Braves': 'ATL',
     'Baltimore Orioles': 'BAL',
     'Boston Red Sox': 'BOS',
     'Chicago Cubs': 'CHC',
     'Chicago White Sox': 'CHW',
     'Cleveland Guardians': 'CLE',
     'Cleveland Indians': 'CLE',
     'Cincinnati Reds': 'CIN',
     'Colorado Rockies': 'COL',
     'Detroit Tigers': 'DET',
     'Houston Astros': 'HOU',
     'Kansas City Royals': 'KCR',
     'Los Angeles Angels': 'LAA',
     'Los Angeles Dodgers': 'LAD',
     'Miami Marlins': 'MIA',
     'Milwaukee Brewers': 'MIL',
     'Minnesota Twins': 'MIN',
     'New York Mets': 'NYM',
     'New York Yankees': 'NYY',
     'Oakland Athletics': 'OAK',
     'Philadelphia Phillies': 'PHI',
     'Pittsburgh Pirates': 'PIT',
     'San Diego Padres': 'SDP',
     'San Francisco Giants': 'SFG',
     'Seattle Mariners': 'SEA',
     'St. Louis Cardinals': 'STL',
     'Tampa Bay Rays': 'TBR',
     'Texas Rangers': 'TEX',
     'Toronto Blue Jays': 'TOR',
     'Washington Nationals': 'WSN'
}
df_playoffs = df_playoffs.with_columns(
    home_team = pl.col('home_team').replace(team_mapping), 
    team = pl.col('team').replace(team_mapping), 
    batting_team = pl.col('batting_team').replace(team_mapping)
)

# renaming mlbam cols
df_regseason = df_regseason.drop('fg_playerid').rename({'key_mlbam': 'mlbam_playerid'})


# In[6]:


# creating season player ids
df_regseason = df_regseason.with_columns(
    season_player_id = pl.col('year').cast(pl.String) + '_' + pl.col('mlbam_playerid').cast(pl.String)
)

df_playoffs = df_playoffs.with_columns(
    season_player_id = pl.col('year').cast(pl.String) + '_' + pl.col('mlbam_playerid').cast(pl.String)
)

# creating season batting team ids
df_regseason = df_regseason.with_columns(
    season_batting_team_id = pl.col('year').cast(pl.String) + '_' + pl.col('batting_team').cast(pl.String)
)

df_playoffs = df_playoffs.with_columns(
    season_batting_team_id = pl.col('year').cast(pl.String) + '_' + pl.col('batting_team').cast(pl.String)
)

# filtering playoff players to those with >75 IP in reg season
regseason_players = df_regseason.select(pl.col('season_player_id')).unique().to_series().to_list()
df_playoffs = df_playoffs.filter(df_playoffs['season_player_id'].is_in(regseason_players))


# In[7]:


# split reg season into pre sep and sep dfs
df_regseason = df_regseason.with_columns(
    month = pl.col('game_date').str.strptime(pl.Date, '%Y-%m-%d').dt.month()
)
df_regseason_sep = df_regseason.filter(pl.col('month') == 9)
df_regseason_presep = df_regseason.filter(pl.col('month') < 9)

# get presep fip
df_presep_fips = (df_regseason_presep
    .group_by('season_player_id')
    .agg((pl.col('fip')*pl.col('ip')).sum()/pl.col('ip').sum())
                 .rename({'fip': 'fip_presep'}))

# join presep fips to sep and playoff dfs
df_regseason_sep = df_regseason_sep.join(df_presep_fips, how = 'left', on='season_player_id')
df_playoffs = df_playoffs.join(df_presep_fips, how = 'left', on='season_player_id')


# In[8]:


# aligining cols
df_regseason_sep = df_regseason_sep.select(['game_date', 'home_team', 'team', 'batting_team', 'season_batting_team_id', 'pitcher_name','year', 'mlbam_playerid', 'season_player_id', 'hr', 'bb', 'hbp', 'so', 'ip', 'fip', 'fip_presep', 'playoff_game', 'home_away'])
df_playoffs = df_playoffs.select(['game_date', 'home_team', 'team', 'batting_team', 'season_batting_team_id', 'pitcher_name','year', 'mlbam_playerid', 'season_player_id', 'hr', 'bb', 'hbp', 'so', 'ip', 'fip', 'fip_presep', 'playoff_game', 'home_away'])

# concating dfs
df_gamelogs = pl.concat([df_playoffs, df_regseason_sep], how = 'vertical')

# fill infinite fips with 99
df_gamelogs = df_gamelogs.with_columns(
    pl.when(pl.col('fip').is_infinite())
        .then(99)
        .otherwise(pl.col('fip'))
        .alias('fip')
)

# remove accents from names
df_gamelogs = df_gamelogs.with_columns(
    pl.col('pitcher_name').map_elements(unidecode, return_dtype = pl.Utf8)
)

# drop null bam ids for players that debuted in 2024, one null fip, players that started in sep but not before
df_gamelogs = df_gamelogs.drop_nulls(subset=['mlbam_playerid', 'fip', 'fip_presep'])


# In[11]:


df_gamelogs.write_csv('/Users/toddk/OneDrive/Desktop/R_projects/causal_inference_playoff/df_gamelogs.csv')

