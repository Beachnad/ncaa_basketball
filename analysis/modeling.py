from analysis.data_transformations import adjust_data, elo_score
from analysis.utils import dataset, opp_data, mean, cum_mean
from itertools import product
import numpy as np


data = dataset()


def linear_wf_generator(slope, intercept):
    def wfun(i):
        return (i * slope) + intercept
    return wfun


def optimize_adjustment_paramaters(data, optimized=None):
    if not optimized:
        optimized = {
            'accuracy': 0,
            'slope': None,
            'intercept': None,
            'lookback': None,
            'eOFF_home_adv': None
        }

    slopes = (1, 3, 5, 10, 15)
    intercept = 100
    eOFF_home_adv = (mean(*[r['eOFF'] for r in data if r['HAN']=='H']) -
                     mean(*[r['eOFF'] for r in data if r['HAN']=='A'])) / 100


    for lb in (10, 15, 20, 22):
        print('================================================================')
        for slope in slopes:
            wf = linear_wf_generator(slope=slope, intercept=intercept)
            sample_weghts = list(map(wf, range(lb)))
            # print('Sample Weights:', sample_weghts)
            data = adjust_data(data, lookback=lb, weight_fun=wf, eOFF_home_adv=eOFF_home_adv)

            home_adv_fun = lambda han: eOFF_home_adv if han=='H' else -eOFF_home_adv if han=='A' else 0

            data['pred_pts'] = [
                (((eOFF + eDEF_opp) / 2 + home_adv_fun(HAN)) / 100) * ((tempo + tempo_opp) / 2) if all((eOFF, eDEF_opp, tempo, tempo_opp)) else None
                for eOFF, eDEF_opp, tempo, tempo_opp, HAN in
                zip(data['avg_adj_eOFF'], data['avg_adj_eDEF.opp'], data['avg_adj_tempo'], data['avg_adj_tempo.opp'], data['HAN'])
            ]
            data = opp_data(data, 'pred_pts')

            tmp = [[r['pred_pts'], r['pts']] for r in data if r['pred_pts']]
            preds = [x[0] for x in tmp]
            labels = [x[1] for x in tmp]
            sum(preds) / len(preds)
            sum(labels) / len(labels)

            game_pred = [('W'==y) if pred_pts > pred_pts_opp else ('L'==y) for pred_pts, pred_pts_opp, y in
                         zip(data['pred_pts'], data['pred_pts.opp'], data['result']) if all((pred_pts, pred_pts_opp))]
            correct = game_pred.count(True)
            wrong = game_pred.count(False)
            accuracy = correct / (wrong + correct)
            if accuracy > optimized['accuracy']:
                optimized = {
                    'accuracy': accuracy,
                    'slope': slope,
                    'intercept': intercept,
                    'lookback': lb,
                    'eOFF_home_adv': eOFF_home_adv
                }
                print('NEW BEST!', optimized)

            print(f'LOOKBACK: {lb},SLOPE: {slope},PRED RATE: {correct / (wrong + correct)}')
    return optimized


# CURRENT BEST:
opt_params = {'accuracy': 0.7077251501000668, 'slope': 10,
              'intercept': 100, 'lookback': 22, 'eOFF_home_adv': 0.06729469695672904}
opt_params = optimize_adjustment_paramaters(data, opt_params)

data = adjust_data(
    data,
    lookback=opt_params['lookback'],
    weight_fun=linear_wf_generator(opt_params['slope'], opt_params['intercept']),
    eOFF_home_adv=opt_params['eOFF_home_adv']
)


def test_elo_fun(data, fun, home_adv=3.7, verbose=False):
    data = elo_score(data, fun, home_adv, verbose)
    data['pred'] = ['W' if r['pre_game_elo'] > r['pre_game_elo.opp'] else
                    'L' if r['pre_game_elo'] < r['pre_game_elo.opp'] else
                    'T' for r in data]
    correct = [1 if r['pred'] == r['result'] else
               0.5 if r['pred'] == 'T' else 0 for r in data if r['season'] > 2013]
    accuracy = sum(correct) / len(correct)
    return accuracy


def optimize_elo_parameters(data, opt_params=None):
    def generate_elo_adj_function(angle, slope):
        return lambda x: x**(slope)/angle

    angles = (5.8, 5.9, 6, 6.1, 6.2)
    slopes = (0.79, .8, .81)
    home_advs = (0.025, 0.05, 0.075)

    if not opt_params:
        opt_params = {'accuracy': 0}

    for params in product(angles, slopes, home_advs):
        f = generate_elo_adj_function(params[0], params[1])
        acc = test_elo_fun(data, f, home_adv=params[2])
        print(acc, params)
        if acc > opt_params['accuracy']:
            opt_params = {
                'accuracy': acc,
                'angle': params[0],
                'slope': params[1],
                'home_adv': params[2],
                'elo_fun': f
            }
            print('NEW BEST:', opt_params)
    return opt_params


# BEST = 0.602555431784426 w/ lambda x: min(x / 12, 1.5)
opt_elo_params = {'accuracy': 0.7089206274877078, 'angle': 6, 'slope': 0.8, 'home_adv': 0.05}
opt_elo_params = optimize_elo_parameters(data, opt_elo_params)

data = elo_score(
    data,
    lambda x: x**(opt_elo_params['slope'])/opt_elo_params['angle'],
    home_advantage=opt_elo_params['home_adv']
)

data = cum_mean(data, 'TOV%', weight_fun=linear_wf_generator(10, 100))
data = cum_mean(data, 'FT%', weight_fun=linear_wf_generator(10, 100))
data = cum_mean(data, 'off_eFG%', weight_fun=linear_wf_generator(10, 100))
data = cum_mean(data, 'def_eFG%', weight_fun=linear_wf_generator(10, 100))
data = cum_mean(data, 'ORB%', weight_fun=linear_wf_generator(10, 100))
data = cum_mean(data, 'DRB%', weight_fun=linear_wf_generator(10, 100))


features = (
    'wk', 'pre_game_elo',
    'avg_adj_eOFF', 'avg_adj_eDEF', 'TOV%.mean', 'FT%.mean', 'off_eFG%.mean', 'def_eFG%.mean',
    'ORB%.mean', 'DRB%.mean'
)
identifiers = ('game_stats_id', 'opp_game_stats_id', 'team_id', 'date', 'opp_id', 'win')

df = data.select(*identifiers, *features)
df = opp_data(df, features).filter(lambda r: False if None in r else True)

features = np.matrix([
    v for k, v in df.items() if k not in identifiers
])

labels = np.array(df['win'])