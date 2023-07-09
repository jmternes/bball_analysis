from nba_api.stats.endpoints import playercareerstats
from nba_api.stats.static import players
from nba_api.stats.endpoints import playergamelog
from nba_api.stats.endpoints import leagueleaders
import pandas
from pandas import DataFrame


# players dictionary

players_dict = players.get_players()

'''
swift = [player for player in players_dict if player['full_name'] == 'Stromile Swift'][0]

swift_name = swift['full_name']

swift_game_log = playergamelog.PlayerGameLog(player_id='2031', season = '2004')

swift_game_log_df = swift_game_log.get_data_frames()[0]

swift_ppg = swift_game_log_df['PTS'].mean()

swift_avg_mins = swift_game_log_df['MIN'].mean()

swift_ppg_per_36 = swift_ppg * 36 / swift_avg_mins

print("Stromile Swift averaged " + str(swift_ppg_per_36) + " points per 36 minutes in the 2004-2005 season.")

print("Stromile Swift averaged " + str(swift_avg_mins) + " minutes per game in the 2004-2005 season.")

print(swift_name + " averaged " + str(swift_ppg) + " points per game in the 2004-2005 season.")

print(swift_game_log_df)

# singel asdfvlmasdlvkmaedsf sdfasdfasdf



myPlayer_id = myPlayer['id']

print(myPlayer_id)


# Joe Johnson as an object

isojoe = [player for player in players_dict if player['full_name'] == 'Joe Johnson'][0]
isojoe_id = isojoe['id']
isojoe_name = isojoe['full_name']

print (isojoe_name + "'s ID is: " + str(isojoe_id))



# antawn jamison

antawn = [player for player in players_dict if player['full_name'] == 'Antawn Jamison'][0]
antawn_id = antawn['id']
antawn_name = antawn['full_name']
print(antawn_name + "'s ID is: " + str(antawn_id))

'''

# Allen Iverson as an object

ai = [player for player in players_dict if player['full_name'] == 'Allen Iverson'][0]
ai_id = ai['id']
ai_name = ai['full_name']

print (ai_name + "'s ID is: " + str(ai_id))

# ai ppg in 2004-2005 season

ai_game_log = playergamelog.PlayerGameLog(player_id='947', season = '2005')

ai_game_log_df = ai_game_log.get_data_frames()[0]

ai_ppg = ai_game_log_df['PTS'].mean()

print(ai_name + " averaged " + str(ai_ppg) + " points per game in the 2004-2005 season.")

ai_mins = ai_game_log_df['MIN'].mean()

ai_ppg_per_36 = ai_ppg * 48 / ai_mins

print("AI averaged " + str(ai_ppg_per_36) + " points per 48 minutes in the 2004-2005 season.")

# Joe Johnson's game logs, specifically 07-08 season

gamelog_isojoe = playergamelog.PlayerGameLog(player_id='2207', season = '2007')

# Converts game logs to Pandas DataFrame

gamelog_isojoe_df = gamelog_isojoe.get_data_frames()[0]
print(gamelog_isojoe_df)

# Joe Johnson

career = playercareerstats.PlayerCareerStats(player_id='2207')
career.get_data_frames()[0]

# points per game in 2004-2005 season
