import nltk
from nltk.corpus import conll2000
from nltk.chunk.util import conlltags2tree

#Basically, maps POS tags to IOB tags
#The chunks are the unigrams, instead of the words
class UnigramChunker(nltk.ChunkParserI):
 def __init__(self, train_sents):
   #train_sents is a complete tree of POS-tagged words (chunk tree).
   print train_sents
   #tree2conlltags returns a list of 3-tuples containing (word, tag, IOB-tag). 
   train_data = [[(t,c) for w,t,c in nltk.chunk.tree2conlltags(sent)] 
                 for sent in train_sents]
   #Entries in train_data are like ('NNS', 'B-NP'), ('IN', 'O') etc 
   #print train_data
   self.tagger = nltk.UnigramTagger(train_data)

#The parse() method has to be implemented as it's part of the nltk.ChunkParserI interface
 def parse(self, sentence):
   pos_tags = [pos for (word,pos) in sentence]
   #Tagging the PoS tags with IOB chunk tags
   tagged_pos_tags = self.tagger.tag(pos_tags)
   chunktags = [chunktag for (pos, chunktag) in tagged_pos_tags]
   #print chunktags
   #chunktags are 'B-NP', 'I-NP' or 'O'
   #zip combines two lists, creating tuples taking elements from each list 
   conlltags = [(word, pos, chunktag) for ((word,pos),chunktag)
                in zip(sentence, chunktags)] 
   return nltk.chunk.conlltags2tree(conlltags)

test_sents = conll2000.chunked_sents('test.txt', chunk_types=['NP'])
#Sentences in train.txt are already chunk-tagged, e.g., first line is Confidence NN B-NP. So are
#sentences in test.txt. 
train_sents = conll2000.chunked_sents('train.txt', chunk_types=['NP'])
unigram_chunker = UnigramChunker(train_sents)
print unigram_chunker.evaluate(test_sents)
