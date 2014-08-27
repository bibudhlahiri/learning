if ("package:h2o" %in% search()) { detach("package:h2o", unload = TRUE)}
if ("h2o" %in% rownames(installed.packages())) {remove.packages("h2o")}
#Had to run twice
install.packages("h2o", repos = (c(file = "/Users/blahiri/h2o/h2o-2.4.3.11/R", getOption("repos"))))
library(h2o)
localH2O = h2o.init(ip = "localhost", port = 54321, startH2O = TRUE)
irisPath = system.file("extdata", "iris.csv", package="h2o")
iris.hex = h2o.importFile(localH2O, path = irisPath, key = "iris.hex")
names(iris.hex)
class(iris.hex)
myPath = system.file("extdata", "prostate_folder", package = "h2o")
prostate_all.hex = h2o.importFolder(localH2O, path = myPath)
class(prostate_all.hex)
summary(prostate_all.hex)

irisPath = system.file("extdata", "iris_wheader.csv", package="h2o")
iris.hex = h2o.importFile(localH2O, path = irisPath)
h2o.anyFactor(iris.hex)

#Data sets that are easily and quickly handled by H2O are often too large to be treated equivalently well in R.
