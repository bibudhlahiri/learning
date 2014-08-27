library(e1071)

svc_learn <- function()
{
 set.seed(1)
 x = matrix(rnorm(20*2), ncol = 2)
 y = c(rep(-1, 10), rep(1, 10))
 x[y == 1, ] = x[y == 1, ] + 1
 plot(x, col = (3-y))
}

svm_learn <- function()
{
 set.seed(1)
 x = matrix(rnorm(200*2), ncol = 2)
 x[1:100, ] = x[1:100, ] + 2
 x[101:150, ] = x[101:150, ] - 2
 y = c(rep(1, 150), rep(2, 50))
 dat = data.frame(x = x, y = as.factor(y))
 plot(x, col = y)
 train = sample(200, 100)
 svmfit = svm(y~., data = dat[train, ], kernel = "radial", gamma = 1, cost = 1e5)
 plot(svmfit, dat[train, ])
}
