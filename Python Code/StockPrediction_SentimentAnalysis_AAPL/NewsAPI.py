# -*- coding: utf-8 -*-
"""
Created on Tue Oct  3 11:06:31 2017

@author: Mayur
"""

########## News API ########################################################
#from newsapi import sources
from newsapi.articles import Articles
key = 'd4ccd418a6f74aac9a81a0915958af23'
#params = {}
api = Articles(API_KEY = key)
#sources = api.sources(params)
#articles = api.articles(sources[0]['id'], params)

################ NY Times API #############################################


import csv, json
#reload(sys)
#sys.setdefaultencoding('utf8')


import urllib.request
"""
About:
Python wrapper for the New York Times Archive API 
https://developer.nytimes.com/article_search_v2.json
"""

class APIKeyException(Exception):
    def __init__(self, message): self.message = message 

class InvalidQueryException(Exception):
    def __init__(self, message): self.message = message 

class ArchiveAPI(object):
    def __init__(self, key=None):
        """
        Initializes the ArchiveAPI class. Raises an exception if no API key is given.
        :param key: New York Times API Key
        """
        self.key = key
        self.root = 'http://api.nytimes.com/svc/archive/v1/{}/{}.json?api-key={}' 
        if not self.key:
            nyt_dev_page = 'http://developer.nytimes.com/docs/reference/keys'
            exception_str = 'Warning: API Key required. Please visit {}'
            raise APIKeyException(exception_str.format(nyt_dev_page))

    def query(self, year=None, month=None, key=None,):
        """
        Calls the archive API and returns the results as a dictionary.
        :param key: Defaults to the API key used to initialize the ArchiveAPI class.
        """
        print("year"+ year)
        print("month" + month)
        print("apikey" + key)
        if not key: key = 'd4ccd418a6f74aac9a81a0915958af23'
        month = month
        year = year
        if (year < 1882) or not (0 < month < 13):
            # currently the Archive API only supports year >= 1882
            exception_str = 'Invalid query: See http://developer.nytimes.com/archive_api.json'
            raise InvalidQueryException(exception_str)
        url = self.root.format(year, month, key)
        #return url
        #url = 'http://api.nytimes.com/svc/archive/v1/2017/10.json?api-key=d4ccd418a6f74aac9a81a0915958af23'
        r = urllib.request.Request(url)
        r1 = urllib.request.urlopen(r)
        return r1.json()


#api = Articles('d4ccd418a6f74aac9a81a0915958af23')
#a = ArchiveAPI()
key = 'd4ccd418a6f74aac9a81a0915958af23'
years = [2017,2016, 2015, 2014, 2013, 2012, 2011, 2010, 2009, 2008, 2007]
months = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]
root = 'http://api.nytimes.com/svc/archive/v1/{}/{}.json?api-key={}'
for year in years:
    for month in months:
        
        #if year == 2017 and month < 11:
        #mydict = a.query(year, month, key)
        url = root.format(year, month, key)
        r = urllib.request.Request(url)
        r1 = urllib.request.urlopen(r)
        mydict = r1.read().decode('utf-8')
        file_str = '/Users/Mayur/Documents/Advance Data Science/Python Code/NYTimesData/Apple_json_files/' + str(year) + '-' + '{:02}'.format(month) + '.json'
        with open(file_str, 'w') as fout:
            json.dump(mydict, fout)
        fout.close()
        

    