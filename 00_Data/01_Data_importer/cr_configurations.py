import quandl
from pycoingecko import CoinGeckoAPI
import twitter
import pathlib

# Quandl API
quandl_key = "f6Y8avFQZwXp37ftC1_6"
quandl.ApiConfig.api_key = quandl_key
cg = CoinGeckoAPI()

# EIA API
eia_key = "320c16a416c0cb06ed1a737576ec3ee0"

# Twitter API
tw_consumer_key = ""
tw_consumer_secret = ""
tw_token = ""
tw_token_secret = ""

# api = twitter.Api(consumer_key=tw_consumer_key,
#                 consumer_secret=tw_consumer_secret,
#                  access_token_key=tw_token,
#                  access_token_secret=tw_token_secret)

twitter_search_url = 'https://twitter.com/search?q=(bitcoin%20OR%20ethereum%20OR%20cryptocurrency)&src=typed_query'
# results = api.GetSearch(twitter_search_url)

project_dir = pathlib.Path().absolute()
workdir = project_dir.parent
# print(f"{project_dir}\n{workdir}")
