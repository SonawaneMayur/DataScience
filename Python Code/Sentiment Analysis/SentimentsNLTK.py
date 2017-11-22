# -*- coding: utf-8 -*-
"""
Created on Fri Sep 22 19:06:23 2017

@author: Mayur
"""

import nltk
import random
from nltk.classify.scikitlearn import SklearnClassifier
import pickle
from sklearn.naive_bayes import MultinomialNB, BernoulliNB
from sklearn.linear_model import LogisticRegression, SGDClassifier
from sklearn.svm import SVC, LinearSVC, NuSVC
from nltk.classify import ClassifierI
from statistics import mode
from nltk.tokenize import word_tokenize



class VoteClassifier(ClassifierI):
    def __init__(self, *classifiers):
        self._classifiers = classifiers

    def classify(self, features):
        votes = []
        for c in self._classifiers:
            v = c.classify(features)
            votes.append(v)
        return mode(votes)

    def confidence(self, features):
        votes = []
        for c in self._classifiers:
            v = c.classify(features)
            votes.append(v)

        choice_votes = votes.count(mode(votes))
        conf = choice_votes / len(votes)
        return conf


documents_f = open("C:\\Users\\Mayur\\Documents\\Advance Data Science\\Python Code\\Pickle Files\\documents.pickle", "rb")
documents = pickle.load(documents_f)
documents_f.close()




word_features5k_f = open("C:\\Users\\Mayur\\Documents\\Advance Data Science\\Python Code\\Pickle Files\\word_features5k.pickle", "rb")
word_features = pickle.load(word_features5k_f)
word_features5k_f.close()


def find_features(document):
    words = word_tokenize(document)
    features = {}
    for w in word_features:
        features[w] = (w in words)

    return features



featuresets_f = open("C:\\Users\\Mayur\\Documents\\Advance Data Science\\Python Code\\Pickle Files\\featuresets.pickle", "rb")
featuresets = pickle.load(featuresets_f, encoding='bytes')
featuresets_f.close()

random.shuffle(featuresets)
print(len(featuresets))

testing_set = featuresets[10000:]
training_set = featuresets[:10000]



open_file = open("C:\\Users\\Mayur\\Documents\\Advance Data Science\\Python Code\\Pickle Files\\originalnaivebayes5k.pickle", "rb")
classifier = pickle.load(open_file)
open_file.close()


open_file = open("C:\\Users\\Mayur\\Documents\\Advance Data Science\\Python Code\\Pickle Files\\MNB_classifier5k.pickle", "rb")
MNB_classifier = pickle.load(open_file)
open_file.close()



open_file = open("C:\\Users\\Mayur\\Documents\\Advance Data Science\\Python Code\\Pickle Files\\BernoulliNB_classifier5k.pickle", "rb")
BernoulliNB_classifier = pickle.load(open_file)
open_file.close()


open_file = open("C:\\Users\\Mayur\\Documents\\Advance Data Science\\Python Code\\Pickle Files\\LogisticRegression_classifier5k.pickle", "rb")
LogisticRegression_classifier = pickle.load(open_file)
open_file.close()


open_file = open("C:\\Users\\Mayur\\Documents\\Advance Data Science\\Python Code\\Pickle Files\\LinearSVC_classifier5k.pickle", "rb")
LinearSVC_classifier = pickle.load(open_file)
open_file.close()


open_file = open("C:\\Users\\Mayur\\Documents\\Advance Data Science\\Python Code\\Pickle Files\\SGDC_classifier5k.pickle", "rb")
SGDC_classifier = pickle.load(open_file)
open_file.close()




voted_classifier = VoteClassifier(
                                  classifier,
                                  LinearSVC_classifier,
                                  MNB_classifier,
                                  BernoulliNB_classifier,
                                  LogisticRegression_classifier)



# Sentiment function only takes one parameter text.
# From there, we break down the features with the find_features function.
def sentiment(text):
    feats = find_features(text)
    return voted_classifier.classify(feats),voted_classifier.confidence(feats)

#sentiment("Happy")