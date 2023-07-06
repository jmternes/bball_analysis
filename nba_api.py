from nba_api.stats.static import players

player_info = players.get_players('LeBron James')
print(player_info)

'''

from nba_api.stats.endpoints import leagueleaders
import pandas as pd

# pulls data for top 500 scorers by pts column

top_500 = leagueleaders.LeagueLeaders(
    per_mode_simple='PerGame',
    season='2022-23',
    season_type_all_star='Regular Season',
    stat_category_abbreviation='PTS'
).get_data_frames()[0][:500]

# group players by name and playert ID and calculate averages

top_500_avg = top_500.groupby(['PLAYER_NAME', 'PLAYER_ID']).mean()[['min', 'fgm', 'fga', 'fg3m', 'fg3a', 'ftm', 'fta', 'turnover', 'pts']]
'''
