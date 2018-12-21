import sqlite3
from pupyt.pupyt import PuPyT
from datetime import datetime, timedelta
from collections import Counter
from itertools import product
import numpy as np
import pickle



def pickle_cache(pickle_path):


    def wrap(f):
        def wrapped_f(*args):
            f(*args)
        return wrapped_f

    return wrap


def mean(*nums, weights=None, weight_fun=None):
    if weights:
        if len(weights) != len(nums):
            raise ValueError("Weights and Values not the same length")
        return np.average(nums, weights=weights)
    if weight_fun and len(nums) > 0:
        weights = list(map(weight_fun, range(len(nums))))
        return np.average(nums, weights=weights)
    if len(nums) == 0:
        return None
    return sum(nums) / len(nums)


def most_common(elements):
    counts = Counter(elements)
    max_count = max(counts.values())
    for element, count in counts.items():
        if count == max_count:
            return element


def floor_date(date: datetime, unit='week'):
    if unit == 'week':
        return date - timedelta(days=date.weekday())
    raise ValueError(f'{unit} is not a valid unit')


def div(a, b):
    if b == 0:
        return None
    else:
        return a / b


def dataset():
    db = sqlite3.connect('/Users/db/pprojects/ncaa_basketball/db')
    data = db.execute("SELECT * FROM game_data WHERE opp_id is NOT NULL;").fetchall()
    columns = [x[0] for x in db.execute("SELECT * FROM game_data").description]

    data = PuPyT(dict(zip(columns, list(zip(*data)))))
    data['date'] = [datetime.strptime(d, '%Y-%m-%d').date() for d in data['date']]
    data['season'] = [d.year - (0 if d.month > 10 else 1) for d in data['date']]

    home_arena = {}
    home_arenas = {}
    for r in data:
        home_arena.setdefault(r['season'], {}).setdefault(r['team_id'], []).append(r['loc'])
    for season_id, season_data in home_arena.items():
        for team_id, team_locs in season_data.items():
            ha = most_common(team_locs)
            home_arenas.setdefault(season_id, {})[team_id] = ha

    data['win'] = [1 if r=='W' else 0 for r in data['result']]
    data['home_arena'] = [home_arenas[r['season']][r['team_id']] for r in data]
    opp_lookup = data.group_by('team_id', 'date')

    for col in data.keys():
        if col not in ('opp_id', 'team_id', 'result', 'loc', 'home_away_neutral', 'season', 'date'):
            data[f'{col}.opp'] = [opp_lookup.get(r['opp_id'], {}).get(r['date'], {}).get(col, [None])[0] for r in data]
    data = data.filter(lambda x: x['pts.opp'])
    data['HAN'] = [
        'H' if r['home_arena'] == r['loc'] else
        'A' if r['home_arena.opp'] == r['loc'] else 'N'
        for r in data
    ]

    data['posses'] = [((0.96 * (r['fga.opp'] + r['tov.opp'] + 0.44 * r['fta.opp'] - r['orb.opp'])) + (
                0.96 * (r['fga'] + r['tov'] + 0.44 * r['fta'] - r['orb']))) / 2 for r in data]
    data['eOFF'] = [r['pts'] / r['posses'] * 100 for r in data]
    data['eDEF'] = [r['pts.opp'] / r['posses'] * 100 for r in data]
    data['game_id'] = [int(str(r['game_stats_id'])[:-1]) for r in data]
    data['TOV%'] = [div(r['tov'], r['posses']) for r in data]
    data['FT%'] = [div(r['ft'], r['fta']) for r in data]
    data['ORB%'] = [r['orb'] / (r['orb'] + r['drb.opp']) for r in data]
    data['DRB%'] = [r['drb'] / (r['drb'] + r['orb.opp']) for r in data]
    data['off_eFG%'] = [(r['fg'] + 0.5 * r['fg3']) / r['fga'] for r in data]
    data['def_eFG%'] = [(r['fg.opp'] + 0.5 * r['fg3.opp']) / r['fga.opp'] for r in data]\

    season_groups = data.group_by('season')
    data['wk'] = [
        (floor_date(r['date']) - floor_date(min(season_groups[r['season']]['date']))).days / 7 + 1
        for r in data]
    data['wk_adj'] = [min((r['wk'], 19)) for r in data]

    return data


def as_array(items):
    return np.asarray(items, dtype='float32').reshape(-1, 1)


def opp_data(data, opponent_columns):
    opp_lookup = data.group_by('team_id', 'date')
    for col in data.keys():
        if col in opponent_columns:
            data[f'{col}.opp'] = [opp_lookup.get(r['opp_id'], {}).get(r['date'], {}).get(col, [None])[0] for r in data]
    return data


def map_data(data, data_map):
    first_key = list(data_map.keys())[0]
    data_keys = data_map[first_key].keys()
    for k in data_keys:
        data[k] = [data_map[r['game_stats_id']][k] for r in data]
    return data


def mean_where(data, key=lambda x: True, target='pts'):
    return mean(*[r[target] for r in data if key(r)])


def cum_mean(data, target, lookback=22, weight_fun=lambda x: 1):
    game_id_data_map = {}

    team_group = data.group_by('team_id')
    for team_id, team_data in team_group.items():
        ordered_data = team_data.sort_on('date')
        game_stats_ids = ordered_data['game_stats_id']
        vals = [None] + ordered_data[target]
        print(vals)
        for i in range(len(game_stats_ids)):
            gs_id = game_stats_ids[i]
            lb = max(i - lookback, 0)
            ub = i + 1
            m = mean(*[x for x in vals[lb:ub] if x], weight_fun=weight_fun)
            print(m)
            game_id_data_map[gs_id] = {f'{target}.mean': m}
    data = map_data(data, game_id_data_map)
    return data


def col_type(col):
    vals = [x for x in col if x is not None]
    all_type = lambda type_, som_list: all(type(x) is type_ for x in some_list)
    if all_type(int, vals):
        return 'integer'
    if all_type(float, vals):
        return 'float'
    if all_type(bool, vals):
        return 'boolean'


def pupyt_to_db(data, path, table_name):
    db = sqlite3.connect(path)
    pass





