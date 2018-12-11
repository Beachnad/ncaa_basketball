import sqlite3
from pupyt.pupyt import PuPyT
from datetime import datetime, timedelta
from collections import Counter
from sklearn import linear_model
import numpy as np


def dataset():
    db = sqlite3.connect('/Users/db/pprojects/ncaa_basketball/db')
    data = db.execute("SELECT * FROM game_data WHERE opp_id is NOT NULL;").fetchall()
    columns = [x[0] for x in db.execute("SELECT * FROM game_data").description]
    return PuPyT(dict(zip(columns, list(zip(*data)))))





def grid_dict(*items,default=None):
    if len(items) == 1:
        return {it: default for it in items[0]}
    else:
        rem_items = items[1:]
        return {it: grid_dict(*rem_items, default=default) for it in items[0]}

data = dataset()


# Linear Regression Model, to find expected eOFF and eDEF for an
# avg team in any given week of the season



