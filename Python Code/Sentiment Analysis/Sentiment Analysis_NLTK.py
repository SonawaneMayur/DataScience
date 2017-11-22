# -*- coding: utf-8 -*-
"""
Created on Thu Sep 21 18:10:28 2017

@author: Mayur
"""


from tweepy import Stream
from tweepy import OAuthHandler
from tweepy.streaming import StreamListener
import json
from statistics import mode as s

#consumer key, consumer secret, access token, access secret.
ckey="wD9XB3nCUR7EKQmT9qyQRASn2"
csecret="9sNLGekY6G6QuubDoU8wBIHhw36HIZjfGo3a1wBmRA21J7NgXl"
atoken="864455585286299648-SnWXOgAa775L2WluyIuSI5ysCA5uOom"
asecret="NdQfLtSHBWmN6suu9l16yW4FYZymSJRZG4T5lRtWCZSaS"

from twitterapistuff import *

class listener(StreamListener):

    def on_data(self, data):

        all_data = json.loads(data)

        tweet = all_data["text"]
        sentiment_value, confidence =s.sentiment(tweet)
        print(tweet, sentiment_value, confidence)
        if confidence*100 >= 80:
            output = open("twitter-out.txt","a")
            output.write(sentiment_value)
            output.write('\n')
            output.close()

        return True

    def on_error(self, status):
        print(status)

auth = OAuthHandler(ckey, csecret)
auth.set_access_token(atoken, asecret)

twitterStream = Stream(auth, listener())
twitterStream.filter(track=["happy"])