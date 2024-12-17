

import requests
import subprocess
import json
import gzip
import os
import time

def get_secret(secret):
    return(
        subprocess.run("pass show " + secret, shell = True,
                       stdout=subprocess.PIPE, text = True).stdout.strip())


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
    """download the sale, write to file, return next auction"""

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
        with gzip.open(latest_file) as f:
            rjson = json.load(f)
        url = rjson['_links']['next']['href']

    cntr = 0
    
    while True:

        url = dl_sale(url = url)

        
        time.sleep(0.3)
        cntr += 1

    



# rjson['_embedded']['sales']

# get artists of sale

# url = "https://api.artsy.net/api/artists?sale_id=5177449d082cd050f3000221"

# r2 = requests.get(url, headers = headers)
# r2json = r2.json()
# len(r2json['_embedded']['artists'])
