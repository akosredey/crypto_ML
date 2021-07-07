import json
import re
import requests
import numpy as np
import pandas as pd
import pandas_datareader as pdr
from cr_configurations import workdir, quandl_key, eia_key
import yfinance as yf
import quandl
from datetime import datetime

quandl.ApiConfig.api_key = quandl_key


def jprint(obj):
    text = json.dumps(obj, sort_keys=True, indent=4)
    print(text)


def get_json_data(data_id, json_path, provider):
    """Download and cache JSON data, return as a dataframe."""

    cache_path = f"{workdir}\\02_Resources\\{data_id}.json"
    try:
        f = open(cache_path, 'rb')
        df = pd.read_json(f)
        print(f"Loaded {cache_path} from cache")
    except (OSError, IOError) as e:
        print(f"Downloading {data_id} from {provider}")
        df = pd.read_json(json_path)
        df.to_json(cache_path)
        print(f"Cached {data_id} at {cache_path}")
    return df


# Quandl Data import
# Bitcoin Charts Exchange Rate Data

def get_quandl_data(quandl_id):
    """" Download quandl data and load to json """

    data_id = quandl_id.replace('/', '-')
    provider = "Quandl"
    df = quandl.get(quandl_id, returns="pandas")
    json_path = df.to_json()
    return get_json_data(data_id, json_path, provider)


# Yahoo Finance Data import

def get_yfinance_data(ticker, start):
    """" Download yahoo finance data and load to json """

    data_id = re.sub(r'\W+', '_', ticker)
    provider = "Yahoo"
    df = yf.download(ticker, start)
    json_path = df.to_json()
    return get_json_data(data_id, json_path, provider)


# Data Reader Data import


def get_datareader_data(datareader_id, provider):
    """" Download datareader data and load to json """
    data_id = datareader_id
    df = pdr.DataReader(datareader_id, provider)
    if df.shape[0] != df.index.nunique():
        idx = np.unique(df.index.values, return_index=True)[1]
        df = df.iloc[idx]

    json_path = df.to_json()
    return get_json_data(data_id, json_path, provider)


# Poloniex Data import

base_polo_url = 'https://poloniex.com/public?command=returnChartData&currencyPair={}&start={}&end={}&period={}'
start_date = datetime.strptime('2014-01-01', '%Y-%m-%d')  # get data from the start of 2014
end_date = datetime.now()  # up until today
period = 86400  # pull daily data (86,400 seconds per day)


def get_poloniex_data(poloniex_pair):
    """Retrieve cryptocurrency data from poloniex"""

    data_id = poloniex_pair
    provider = "Poloniex"
    json_url = base_polo_url.format(poloniex_pair, start_date.timestamp(), end_date.timestamp(), period)
    df = get_json_data(data_id, json_url, provider)
    df = df.set_index('date')
    return df


# EIA Data import


eia_url = "http://api.eia.gov/series/?api_key={}&series_id={}"


def get_eia_data(eia_id):
    response = requests.get(eia_url.format(eia_key, eia_id))
    df = pd.DataFrame(response.json()['series'][0]['data'])
    df = df.set_index(0).rename_axis('Year').sort_index()
    df = df.rename(columns={1: 'CO2_Em'})
    return df
