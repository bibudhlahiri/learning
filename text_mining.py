import nltk
from nltk.corpus import brown
from nltk.probability import FreqDist
from nltk.book import * #For text corpora


'''
days = ['Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday']
cfd = nltk.ConditionalFreqDist(
          ('news', word)
          for word in brown.words(categories='news'))
cfd.tabulate(samples=days)
'''

#Checking basic FreqDist
fdist1 = FreqDist(text1)
vocabulary1 = fdist1.keys()
vocabulary1[:50]
fdist1['whale']

#Checking basic ConditionalFreqDist
cfd = nltk.ConditionalFreqDist((genre, word) for genre in brown.categories() for word in brown.words(categories=genre))
genres = ['news', 'religion', 'hobbies', 'science_fiction', 'romance', 'humor'] 
modals = ['can', 'could', 'may', 'might', 'must', 'will'] 
cfd.tabulate(conditions=genres, samples=modals)

#The Lookup Tagger
fd = nltk.FreqDist(brown.words(categories='news'))
#One word can have multiple tags. brown.tagged_words() returns object of class ConcatenatedCorpusView.
#We are creating the baseline tagger by tagging each word from the brown news corpus 
brown_sents = brown.sents(categories='news')
#brown_tagged_sents is of class ConcatenatedCorpusView
brown_tagged_sents = brown.tagged_sents(categories='news')
cfd = nltk.ConditionalFreqDist(brown.tagged_words(categories='news'))
#cfd['of'] is a FreqDist with 3 samples and 2849 outcomes. The entries are {('IN', 2716), ('IN-TL', 128), ('IN-HL', 5)}.
most_freq_words = fd.keys()[:100]
#word comes from most_freq_words, which comes from fd, which comes from freq distn of brown.words in news corpus.
#Basically for each word in the news corpus, we are keeping the most likely tag as derived from 
likely_tags = dict((word, cfd[word].max()) for word in most_freq_words)
#baseline_tagger is an object of class UnigramTagger.

#The UnigramTagger finds the most likely tag for each word in a training corpus, 
#and then uses that information to assign tags to new tokens. 
baseline_tagger = nltk.UnigramTagger(model=likely_tags)

'''
evaluate() scores the accuracy of the tagger against the gold standard. In the following statement,
strip the tags from the brown_tagged_sents, retag it using the tagger, then compute the accuracy score.
'''
baseline_tagger.evaluate(brown_tagged_sents)

#POS ambiguity
cfd = nltk.ConditionalFreqDist(((x[1], y[1], z[0]), z[1]) for sent in brown_tagged_sents for x,y,z in nltk.trigrams(sent))
ambiguous_contexts = [c for c in cfd.conditions() if len(cfd[c]) > 1]
sum(cfd[c].N() for c in ambiguous_contexts)/cfd.N()

tg = [(x,y,z) for sent in brown_tagged_sents for x,y,z in nltk.trigrams(sent)]
#tg[1] = (('Fulton', 'NP-TL'), ('County', 'NN-TL'), ('Grand', 'JJ-TL')) : trigram of tagged words

size = int(len(brown_tagged_sents) * 0.9)
train_sents = brown_tagged_sents[:size]
test_sents = brown_tagged_sents[size:]

t0 = nltk.DefaultTagger('NN')
t1 = nltk.UnigramTagger(train_sents, backoff=t0)
t2 = nltk.BigramTagger(train_sents, backoff=t1)
t2.evaluate(test_sents)

test_tags = [tag for sent in brown.sents(categories='editorial') for (word, tag) in t2.tag(sent)]
gold_tags = [tag for (word, tag) in brown.tagged_words(categories='editorial')]
cm = nltk.ConfusionMatrix(gold_tags, test_tags)
print cm

#The Regular Expression Tagger
patterns = [(r'.*ing$', 'VBG'), (r'.*ed$', 'VBD'), (r'.*es$', 'VBZ'), (r'.*ould$', 'MD'), (r'.*\'s$', 'NN$'), (r'.*s$', 'NNS'), 
            (r'^-?[0-9]+(.[0-9]+)?$', 'CD'),(r'.*', 'NN')]
regexp_tagger = nltk.RegexpTagger(patterns)
regexp_tagger.tag(brown_sents[3])
print regexp_tagger.evaluate(brown_tagged_sents)

 
