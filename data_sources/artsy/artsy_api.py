

import requests
import subprocess
import json
import gzip
import os
import time
import pandas as pd


import ibis
from ibis import _, desc
ibis.options.interactive = True


def get_secret(secret):
    return(
        subprocess.run("pass show " + secret, shell = True,
                       stdout=subprocess.PIPE, text = True).stdout.strip())

def lmap (fun, iterable):
    return (list(map(fun, iterable)))

def flatten_list (list_of_lists):
    return([x for xs in list_of_lists for x in xs])


def get_artsy_token() :

    url = "https://api.artsy.net/api/tokens/xapp_token"
    payload = {
        "client_id": get_secret('artsy-client-id'),
        "client_secret": get_secret('artsy-client-secret')
    }

    r = requests.post(url, data=payload)

    token = r.json()['token']
    return(token)


def dl_sale (url):
    """download the , write to file, return next auction"""

    headers = {
        "X-XAPP-Token" : artsy_token}

    r = requests.get(url, headers = headers)
    rjson = r.json()

    id_sale = 'start'
    if 'cursor' in url:     
        id_sale = url.split('cursor=')[1]

    print(id_sale)

    with gzip.open(os.path.join(DIR_SALES, id_sale) + '.json.gz', 'wt') as f:
        json.dump(rjson, f)

    next_id = rjson['_links']['next']['href']
    
    return(next_id)


def dl_sale_artworks (url):
    """download artworks in a """
    
    # url = 'https://api.artsy.net/api/sale_artworks?sale_id=5ce556e6d42611000e74fbc1{&published}'
    # url = 'https://api.artsy.net/api/sale_artworks?sale_id=5ce556e6d42611000e74fbc1'
    url_proc = url.replace("{&published}", "")
    
    id_sale = url_proc.split("sale_id=")[1]

    headers = {
        "X-XAPP-Token" : artsy_token}

    r = requests.get(url_proc, headers = headers)
    rjson = r.json()

    DIR_ARTWORK = "/home/johannes/Dropbox/phd/pmdata/data_sources/artsy/data_artworks/"

    pagenbr = 0

    # write initial page
    with gzip.open(os.path.join(DIR_ARTWORK, id_sale) + '_p' + str(pagenbr) + '.json.gz', 'wt') as f:
        json.dump(rjson, f)

    
    while True:
        # if there are additional pages, download them
        if dict_url := rjson['_links'].get('next'):
        
            r = requests.get(dict_url['href'], headers = headers)
            rjson = r.json()

            with gzip.open(os.path.join(DIR_ARTWORK, id_sale) + '_p' + str(pagenbr) + '.json.gz', 'wt') as f:
                json.dump(rjson, f)
        else:
            break

        pagenbr += 1
        time.sleep(0.3)
            
# dl_sale_artworks('https://api.artsy.net/api/sale_artworks?sale_id=5ce556e6d42611000e74fbc1{&published}')            
        

    
    


def get_latest_modified_file(directory):
    # List all files in the directory and get their full paths
    
    files = [os.path.join(directory, f) for f in os.listdir(directory)]
    
    # Check if there are no files
    if not files:
        return None

    # Use max() to find the file with the latest modification time
    latest_file = max(files, key=os.path.getmtime)
    return(latest_file)

if __name__ == "__main__":

    DIR_SALES = "/home/johannes/Dropbox/phd/pmdata/data_sources/artsy/sales_data/"
    artsy_token = get_artsy_token()

    ## get url to start
    if len(os.listdir(DIR_SALES)) == 0:
        url = "https://api.artsy.net/api/sales"
    else:
        latest_file = get_latest_modified_file(DIR_SALES)
        with gzip.open(latest_file, 'rt') as f:
            rjson = json.load(f)
        url = rjson['_links']['next']['href']

    cntr = 0
    
    while True:

        url = dl_sale(url = url)

        
        time.sleep(0.3)
        cntr += 1

    
def read_sale (filex):
    with gzip.open(os.path.join(DIR_SALES, filex), 'rt') as f:
        rjson = json.load(f)
    
    return(rjson)

def gd_auctions ():

    l_files = os.listdir(DIR_SALES)

    l_dicts = lmap(read_sale, l_files)
    l_sales = lmap(lambda x : x['_embedded']['sales'], l_dicts)
    l_flat = flatten_list(l_sales)

    dt_auctions = ibis.memtable(pd.DataFrame(l_flat))
    return(dt_auctions)

dt_auctions = gd_auctions()

dt_auctions.info().to_pandas()
dt_auctions[['start_at', 'end_at']]

ibis.options.repr

xx = l_flat[300]

(dt_auctions.select(start_year = _.start_at[0:4])
 .group_by(_.start_year)
 .aggregate(cnt = _.start_year.count())
 .order_by(_.start_year)).execute()
# starts to have ok coverage in 2017...
# but probably is also there not stable, quite some fluctuations again
# start_year  cnt
# 0        2013    8
# 1        2014   29
# 2        2015   20
# 3        2016   54
# 4        2017  200
# 5        2018  413
# 6        2019  426
# 7        2020  263
# 8        2021  320
# 9        2022  235
# 10       2023  196
# 11       2024  204
# 12       None    0
# >>> 

dt_auctions.name.split(':')[0]

(dt_auctions.select(auction_house = _.name.split(':')[0])
 .group_by(_.auction_house)
 .aggregate(cnt = _.auction_house.count())
 .order_by(_.cnt.desc())).limit(20)



# ┏━━━━━━━━━━━━━━━━┳━━━━━━━┓
# ┃ auction_house  ┃ cnt   ┃
# ┡━━━━━━━━━━━━━━━━╇━━━━━━━┩
# │ string         │ int64 │
# ├────────────────┼───────┤
# │ Heritage       │   291 │
# │ Phillips       │   144 │
# │ Forum Auctions │   130 │
# │ Rago Auctions  │   112 │
# │ Wright         │    86 │
# │ Bonhams        │    57 │
# │ Doyle          │    46 │
# │ Tate Ward      │    46 │
# │ LAMA           │    40 │
# │ Roseberys      │    35 │

# dt_auctions.filter(_.name.ilike('%sotheby%'))
# super little from big houses



# rjson['_embedded']['sales']

# get artists of 

# url = "https://api.artsy.net/api/artists?sale_id=5177449d082cd050f3000221"

# r2 = requests.get(url, headers = headers)
# r2json = r2.json()
# len(r2json['_embedded']['artists'])
