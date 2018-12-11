from analysis.data_transformations import adjust_eOFF_eDEF
from analysis.utils import dataset, opp_data


data = dataset()

for lb in range(12, 18):
    data = adjust_eOFF_eDEF(data, lookback=lb)

    data['pred_pts'] = [
        ((eOFF + eDEF_opp) / 2 / 100) * ((tempo + tempo_opp) / 2) if all((eOFF, eDEF_opp, tempo, tempo_opp)) else None
        for eOFF, eDEF_opp, tempo, tempo_opp in
        zip(data['avg_adj_eOFF'], data['avg_adj_eDEF.opp'], data['avg_adj_tempo'], data['avg_adj_tempo.opp'])
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

    print(f'LOOKBACK: {lb},PRED RATE: {correct / (wrong + correct)}')

