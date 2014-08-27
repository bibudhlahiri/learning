#Use reload(textproc) after any changes in file
#from textproc import <function_name> to import functions

def plural(word):
  if word.endswith('y'):
   return word[:-1] + 'ies'
  elif word[-1] in 'sx' or word[-2:] in ['sh', 'ch']:
   return word + 'es' 
  elif word.endswith('an'):
    return word[:-2] + 'en' 
  else:
    return word + 's'

import nltk

def unusual_words(text):
  text_vocab = set(w.lower() for w in text if w.isalpha()) 
  english_vocab = set(w.lower() for w in nltk.corpus.words.words()) 
  unusual = text_vocab.difference(english_vocab)
  return sorted(unusual)
