library(gbm)
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
 
boost.boston <- gbm(medv ~., data = Boston[train, ], distribution = "gaussian", n.trees = 5000, interaction.depth = 4)
 
