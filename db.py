import sqlite3

db = sqlite3.connect('db')


def create_table(db, table_name, **columns):
    sql_statement = "CREATE TABLE {} ({});".format(
        table_name, ', '.join([k + ' ' + v for k, v in columns.items()])
    )

    db.execute(sql_statement)
    db.commit()


def dict_to_table(db, table_name, data_dict):
    sql_statement = "INSERT INTO {} ({}) VALUES ({})".format(
        table_name,
        ', '.join([k for k in data_dict.keys()]),
        ', '.join(['?'] * len(data_dict.keys()))
    )

    values = list(zip(*[v for v in data_dict.values()]))
    print(values[:5])
    print(sql_statement)
    db.executemany(sql_statement, values)
    db.commit()

def batch_data_insert(db, table_name, data_generator):
    pass


