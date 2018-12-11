from db import db, dict_to_table, create_table
from web_scrape import TeamPages, GamePages
from datetime import date

# team_pages = TeamPages()
# data = team_pages.get_data()
# dict_to_table(db, 'teams', data)

game_data_table = {
    'game_stats_id': 'integer','team_id': 'integer','date': 'integer', 'opp_id': 'integer', 'loc': 'integer', 'home_away_neutral': 'varchar', 'result': 'varchar',
    'fg': 'integer','fga': 'integer','fg2': 'integer','fg2a': 'integer','fg3': 'integer','fg3a': 'integer',
    'ft': 'integer', 'fta': 'integer','orb': 'integer', 'drb': 'integer','trb': 'integer','ast': 'integer',
    'stl': 'integer','blk': 'integer','tov': 'integer','pf': 'integer','pts': 'integer'
}
#
# response = input("About to delete game_data table. Continue? [Yes/No]")
# if response == 'Yes':
#     i, data_dict = 0, {}
#     # db.execute("DROP TABLE game_data")
#     # create_table(db, 'game_data', **game_data_table)
#     game_pages = GamePages()
#
#     for i, row in enumerate(game_pages.get_data(date(2011, 11, 1), date(2018, 10, 1)), start=1):
#         if data_dict == {}:
#             data_dict = {k: [v] for k, v in row.items()}
#         else:
#             for k, v in row.items():
#                 data_dict[k].append(v)
#         if i % 100 == 0:
#             print(i, '-', row['date'])
#             dict_to_table(db, 'game_data', data_dict)
#             data_dict = {}
#     dict_to_table(db, 'game_data', data_dict)

    # game_data = game_pages.get_data(date(2012, 11, 1), date(2018, 10, 1))
    # dict_to_table(db, 'game_data', game_data)


def make_from_scratch():
    1/0 # I HAVE THE DATA I NEED
    #db.execute("DROP TABLE IF EXISTS game_data")
    create_table(db, 'game_data', **game_data_table)
    game_pages = GamePages()

    data_dict = {}
    for i, row in enumerate(game_pages.get_data(date(2011, 11, 1), date(2018, 10, 1)), start=1):
        if data_dict == {}:
            data_dict = {k: [v] for k, v in row.items()}
        else:
            for k, v in row.items():
                data_dict[k].append(v)
        if i % 100 == 0:
            print(i, '-', row['date'])
            dict_to_table(db, 'game_data', data_dict)
            data_dict = {}
    dict_to_table(db, 'game_data', data_dict)

# make_from_scratch()

