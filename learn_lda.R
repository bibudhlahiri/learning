library(lda)


data(cora.documents) #2410 documents
data(cora.vocab)
K <- 10 #Number of topics in the model
result <- lda.collapsed.gibbs.sampler(cora.documents, K, cora.vocab, 
                                      25, #The number of sweeps of Gibbs sampling over the entire corpus to make
                                      0.1, #alpha: The scalar value of the Dirichlet hyperparameter for topic proportions
                                      0.1 #eta: The scalar value of the Dirichlet hyperparamater for topic multinomials
                                     )
#result$assignments is a list of length D (2,410).
#length(result$assignments[[1]]) = 64

#cora.documents[[1]] has 64 words, and the numbers in the first row indicate the IDs of the words (that occur at least once) and the numbers in the second row indicate 
#the frequencies of the words in document 1. result$assignments[[1]] has 64 numbers, which indicate the topic assignment of each of the 64 words in document 1. Numbers vary
#between 0 and 9 since the number of topics is 10 (K = 10).

#result$topics is a 10×2961 matrix where each entry indicates the number of times a word (column)
#was assigned to a topic (row)

#result$document_sums is a 10 × 2410 matrix where each entry is an integer indicating the number of times
#words in each document (column) were assigned to each topic (column)
 
predictions <- predictive.distribution(result$document_sums[,1:2], result$topics, 0.1, 0.1) #This function takes a ﬁtted LDA-type model and 
#computes a predictive distribution for new words in a document.

#predictions is a 2961 x 2 matrix, since we chose 2 documents from result$document_sums. It gives the probability of seeing a word in a document.
top.topic.words(t(predictions), 5) #to show the top 5 predictions in each document.

top.topic.documents(result$document_sums[,1:2], 5)
