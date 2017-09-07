## Simulate the true DAG
set.seed(123)
p <- 7
#Generate a random Directed Acyclic Graph (DAG). The resulting graph is topologically ordered from low to high node numbers. 
#prob is the probability of connecting a node to another node with higher topological ordering.
myDAG <- randomDAG(p, prob = 0.2) ## true DAG
#Convert a DAG (Directed Acyclic Graph) to a Completed Partially Directed Acyclic Graph (CPDAG) as follows:
#because every DAG in the Markov equivalence class described by a CPDAG shares the same skeleton and the same v-structures, 
#this function takes the skeleton and the v-structures of the given DAG g. Afterwards it simply uses the 3 orientation rules of the 
#PC algorithm to orient as many of the remaining undirected edges as possible. 
myCPDAG <- dag2cpdag(myDAG) ## true CPDAG
#Compute the (true) covariance matrix of a generated DAG. 
covTrue <- trueCov(myDAG) ## true covariance matrix

## simulate Gaussian data from the true DAG
n <- 10000
#Generate multivariate data with dependency structure specified by a (given) DAG with nodes corresponding to random variables. 
#The DAG has to be topologically ordered. 
dat <- rmvDAG(n, myDAG)

## estimate CPDAG -- see  help(pc)
suffStat <- list(C = cor(dat), n = n)
#gaussCItest: Using Fisher's z-transformation of the partial correlation, test for zero partial correlation of sets of 
#normally / Gaussian distributed random variables. 
pc.fit <- pc(suffStat, indepTest = gaussCItest, p=p, alpha = 0.01)

if (require(Rgraphviz)) {
  ## plot the true and estimated graphs
  par(mfrow = c(1,2))
  plot(myDAG, main = "True DAG")
  plot(pc.fit, main = "Estimated CPDAG")
}

## Supppose that we know the true CPDAG and covariance matrix
#ida() estimates the multiset of possible total causal effects of one variable (x) onto another variable (y) from observational data. 
(l.ida <- ida(x.pos = 2, y.pos = 5, mcov = covTrue, graphEst = myCPDAG, method = "local"))
(g.ida <- ida(2,5, covTrue, myCPDAG, method = "global"))
## The global and local method produce the same unique values.
stopifnot(all.equal(sort(unique(g.ida)),
                    sort(unique(l.ida))))

## From the true DAG, we can compute the true causal effect of 2 on 5.
#We estimate a set of possible total causal effects instead of the unique total causal effect, 
#since it is typically impossible to identify the latter when the true underlying causal DAG is 
#unknown (even with an infinite amount of data). Conceptually, the method works as follows. 
#First, we estimate the equivalence class of DAGs that describe the conditional independence 
#relationships in the data, using the function pc(). For each DAG G in the equivalence class, 
#we apply Pearl's do-calculus to estimate the total causal effect of x on y. 
#This can be done via a simple linear regression: if y is not a parent of x, 
#we take the regression coefficient of x in the regression lm(y ~ x + pa(x)), 
#where pa(x) denotes the parents of x in the DAG G; 
#if y is a parent of x in G, we set the estimated causal effect to zero. 
(ce.2.5 <- causalEffect(myDAG, 5, 2))




