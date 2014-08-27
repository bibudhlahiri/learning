import nltk
from nltk.corpus import brown

def pos_features(sentence, i):
  features = {"suffix(1)": sentence[i][-1:], "suffix(2)": sentence[i][-2:], "suffix(3)": sentence[i][-3:]} 
  if i == 0:
    features["prev-word"] = "<START>" 
  else:
    features["prev-word"] = sentence[i-1] 
  return features

pos_features(brown.sents()[0], 8)
tagged_sents = brown.tagged_sents(categories='news')
featuresets = []
for tagged_sent in tagged_sents:
  untagged_sent = nltk.tag.untag(tagged_sent)
  for i, (word, tag) in enumerate(tagged_sent):
    featuresets.append((pos_features(untagged_sent, i), tag))
print featuresets[1]
#Entries in featuresets are like ({'suffix(3)': 'ton', 'prev-word': 'The', 'suffix(2)': 'on', 'suffix(1)': 'n'}, 'NP-TL')

size = int(len(featuresets) * 0.1)
train_set, test_set = featuresets[size:], featuresets[:size]
classifier = nltk.NaiveBayesClassifier.train(train_set)
print nltk.classify.accuracy(classifier, test_set) 
