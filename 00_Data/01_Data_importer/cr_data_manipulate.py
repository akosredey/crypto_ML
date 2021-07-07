import pandas as pd
import sqlalchemy as db
from cr_configurations import workdir


def check_items(list1, list2):
    check = all(item in list1 for item in list2)
    return check


def value_mapper(df, col_name_to_map, new_col_name, mapping_dict):
    df[new_col_name] = df[col_name_to_map].str.upper().map(mapping_dict)
    return df


def df_subset(df, df_name, mapping_dict):
    columns_mapped = [i for i in mapping_dict[df_name][0]]
    df_sub = df[[column for column in df.columns if column in columns_mapped]]
    return df_sub


def column_rename(df, df_name, mapping_dict):
    df_renamed = df.rename(columns=mapping_dict[df_name][0])
    return df_renamed


def df_date_to_col(df):
    df_new_index = df.reset_index()
    df_date_col = df_new_index.rename(columns={'index': 'Date', 'date': 'Date'})
    return df_date_col


def df_preprocess(df, df_name, mapping_dict):
    df_sub = df_subset(df, df_name, mapping_dict)
    df_ren = column_rename(df_sub, df_name, mapping_dict)
    df_prep = df_date_to_col(df_ren)
    return df_prep


def load_to_sql(table_name, df):
    engine = db.create_engine('sqlite:///crypto.db', echo=True)
    meta = db.MetaData(engine)

    with engine.connect() as con:
        df.to_sql(table_name, con=con, if_exists='replace', index=False)
        con.close()


def create_date_table(start='1900-01-01', end='2099-12-31'):
    df = pd.DataFrame({"Date": pd.date_range(start, end)})
    df["Week_day"] = df.Date.dt.day_name()
    df["Day"] = df.Date.dt.day
    df["Month"] = df.Date.dt.month
    df["Week"] = df.Date.dt.isocalendar().week
    df["Quarter"] = df.Date.dt.quarter
    df["Year"] = df.Date.dt.year
    df.insert(0, 'DateID',
              (df.Year.astype(str) + df.Month.astype(str).str.zfill(2) + df.Day.astype(str).str.zfill(2)).astype(int))
    return df


def df_save_to_csv(df_mapping_dict):
    dir_to_cache = f"{workdir}\\02_Resources\\CSV\\"
    for df_name, df in df_mapping_dict.items():
        df.to_csv(dir_to_cache + df_name.replace(" ", "_") + '.csv')
