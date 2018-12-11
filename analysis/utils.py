import sqlite3
from pupyt.pupyt import PuPyT
from datetime import datetime, timedelta
from collections import Counter
import numpy as np


def mean(*nums):
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