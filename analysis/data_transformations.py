import numpy as np
from sklearn import linear_model
from analysis.utils import mean, as_array


def adjust_eOFF_eDEF(data, lookback=10):
    wk_adj = np.asarray(data['wk_adj'], dtype='float32').reshape(-1, 1)
    eOFF = np.asarray(data['eOFF'], dtype='float32').reshape(-1, 1)

    reg = linear_model.LinearRegression()
    reg.fit(X=wk_adj, y=eOFF)

    tempo_lm = linear_model.LinearRegression()
    tempo_lm.fit(X=wk_adj, y=as_array(data['posses']))

    def weekly_pred(model):
        return {wk: model.predict(np.array(([wk])).reshape(-1, 1))[0][0] for wk in range(1, 20)}

    weekly_eff = weekly_pred(reg)
    weekly_tempo = weekly_pred(tempo_lm)

    team_eff = {t: {'eOFF': [], 'eDEF': [], 'tempo': []} for t in set(data['team_id'])}

    games = data.group_by('game_id')
    game_id_data_map = {}

    for game_id, game_stats in games.items():
        assert len(game_stats) == 2
        t1, t2 = game_stats[0], game_stats[1]
        eff_add = {
            't1': {'eOFF': None, 'eDEF': None, 'avg_eOFF': None, 'avg_eDEF': None,
                   'avg_eOFF.opp': None, 'avg_eDEF.opp': None},
            't2': {'eOFF': None, 'eDEF': None, 'avg_eOFF': None, 'avg_eDEF': None,
                   'avg_eOFF.opp': None, 'avg_eDEF.opp': None}
        }
        for i, t in enumerate((t1, t2)):
            opp = t2 if i == 0 else t1

            eOFF = t['eOFF']
            eDEF = t['eDEF']
            tempo = t['posses']

            eOFF_opp = mean(*team_eff[opp['team_id']]['eOFF'][-lookback:])
            eDEF_opp = mean(*team_eff[opp['team_id']]['eDEF'][-lookback:])
            avg_adj_tempo_opp = mean(*team_eff[opp['team_id']]['tempo'][-lookback:])

            avg_adj_eOFF = mean(*team_eff[t['team_id']]['eOFF'][-lookback:])
            avg_adj_eDEF = mean(*team_eff[t['team_id']]['eDEF'][-lookback:])
            avg_adj_tempo = mean(*team_eff[t['team_id']]['tempo'][-lookback:])

            avg_eOFF = weekly_eff[t['wk_adj']]
            avg_eDEF = weekly_eff[t['wk_adj']]
            avg_tempo = weekly_tempo[t['wk_adj']]

            adj_eOFF = eOFF if not eDEF_opp else eOFF + (avg_eDEF - eDEF_opp)
            adj_eDEF = eDEF if not eOFF_opp else eDEF + (avg_eOFF - eOFF_opp)
            adj_tempo = tempo if not avg_adj_tempo_opp else \
                tempo + (avg_tempo - avg_adj_tempo_opp)

            tkey = 't1' if i == 0 else 't2'
            eff_add[tkey]['eOFF'] = adj_eOFF
            eff_add[tkey]['eDEF'] = adj_eDEF
            eff_add[tkey]['tempo'] = adj_tempo

            game_id_data_map[t['game_stats_id']] = {
                'adj_eOFF': adj_eOFF,
                'adj_eDEF': adj_eDEF,
                'adj_tempo': adj_tempo,
                'avg_adj_eOFF': avg_adj_eOFF,
                'avg_adj_eDEF': avg_adj_eDEF,
                'avg_adj_tempo': avg_adj_tempo,
                'avg_adj_eOFF.opp': eOFF_opp,
                'avg_adj_eDEF.opp': eDEF_opp,
                'avg_adj_tempo.opp': avg_adj_tempo_opp
            }

        team_eff[t1['team_id']]['eOFF'].append(eff_add['t1']['eOFF'])
        team_eff[t1['team_id']]['eDEF'].append(eff_add['t1']['eDEF'])
        team_eff[t1['team_id']]['tempo'].append(eff_add['t1']['tempo'])
        team_eff[t2['team_id']]['eOFF'].append(eff_add['t2']['eOFF'])
        team_eff[t2['team_id']]['eDEF'].append(eff_add['t2']['eDEF'])
        team_eff[t2['team_id']]['tempo'].append(eff_add['t2']['tempo'])

    for k in ['adj_eOFF', 'adj_eDEF', 'adj_tempo',
              'avg_adj_eOFF','avg_adj_eDEF', 'avg_adj_tempo',
              'avg_adj_eOFF.opp','avg_adj_eDEF.opp', 'avg_adj_tempo.opp']:
        data[k] = [game_id_data_map[r['game_stats_id']][k] for r in data]

    return data

