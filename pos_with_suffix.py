#Identifying POS tags based on the suffix of a word
import nltk
from nltk.corpus import brown 
#Starting with a blank FreqDist
suffix_fdist = nltk.FreqDist() 
for word in brown.words():
  word = word.lower() 
  #incrementing the counts one by one for each 1-suffix, 2-suffix etc
  suffix_fdist.inc(word[-1:]) 
  suffix_fdist.inc(word[-2:]) 
  suffix_fdist.inc(word[-3:])
common_suffixes = suffix_fdist.keys()[:100]
#print common_suffixes

def pos_features(word): 
   features = {}
   for suffix in common_suffixes:
     #Tells whether the input word ends with a given suffix among common_suffixes.
     #features is a dict with string keys and boolean values.
     features['endswith(%s)' % suffix] = word.lower().endswith(suffix)
   #print features
   #print features.__class__.__name__
   return features

tagged_words = brown.tagged_words(categories='news')

featuresets = [(pos_features(n), g) for (n,g) in tagged_words]
#Each entry in featuresets is a feature vector with POS tags
#print featuresets[1]
size = int(len(featuresets) * 0.1)
train_set, test_set = featuresets[size:], featuresets[:size]
classifier = nltk.DecisionTreeClassifier.train(train_set)
nltk.classify.accuracy(classifier, test_set)
classifier.classify(pos_features('cats'))
