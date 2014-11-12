library(CRF)

#n.nodes = 4
#n.edges = 3
data(Small)

#Computing the most likely configuration for CRF for small graphs with brute-force search
decode.exact(Small$crf)

#Computing the partition function and marginal probabilities. Returns 
#node.bel, which is the node belief; edge.bel, which is the edge belief;
#and logZ, the logarithmic value of CRF normalization factor Z.
#node.bel is a 4 x2 matrix where each row adds up to 1. It is a matrix with crf$n.nodes rows and crf$max.state columns.
#edge.bel is a list with 3 matrices.
#Belief propagation calculates the marginal distribution for each unobserved node, conditional on any observed nodes.
infer.exact(Small$crf)

#Generating samples from the distribution. Exact sampling for small graphs with brute-force inverse cumulative distribution.
#This function will return a matrix with size rows and crf$n.nodes columns, in which each row is
#a sampled configuration.
sample.exact(Small$crf, 100)
