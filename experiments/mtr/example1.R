# example 1 dataset

# EDM
# The Electrical Discharge Machining dataset (Karalic and Bratko 1997) represents a two-target
# regression problem. The task is to shorten the machining time by reproducing the behaviour
# of a human operator that controls the values of two variables. Each of the target variables
# takes 3 distinct numeric values ({−1, 0, 1}) and there are 16 continuous input variables.


# repro
set.seed(1)

# libs
require(mrf)

# source
source("./experiments/mtr/helpers.R")

# load the data
d <- loadMTRdata(path = "~/Downloads/mtr-datasets/", dataset.name = "example1")

# hyper-param selection 
res_hyper_param <- hyperParamSelection(Y=d$Y, X.knn = d$X.gauss, X.gauss = d$X.gauss, k = 5)

# best parameters
selected.k <- c(5, 10, 20, sqrt(nrow(d$X.knn)))[which.min(lapply(res_hyper_param$knn, function(res) mean(res)))]
selected.sigma <- c(0.1, 0.5, 1, 2)[which.min(lapply(res_hyper_param$gauss, function(res) mean(res)))]

# run pinball analysis (l)
res_pinball <- runRandomPinballAnalysis(param.knn = selected.k, param.gauss = selected.sigma, k = 5,
                                        X=d$X, Y=d$Y, X.knn = d$X.gauss, X.gauss = d$X.gauss, num_features = 100)

# run pinball analysis (nl)
res_pinball_nl <- runRandomPinballNLAnalysis(param.knn = selected.k, param.gauss = selected.sigma, k = 5,
                                             X=d$X, Y=d$Y, X.knn = d$X.gauss, X.gauss = d$X.gauss, num_features = 100)

# run coverage analysis 
res_coverage <- runNormalCoverage(param.knn = selected.k, param.gauss = selected.sigma, k = 5,
                                  X=d$X, Y=d$Y, X.knn = d$X.gauss, X.gauss = d$X.gauss, num_features = 100)


save(d, res_pinball, res_pinball_nl, res_coverage, file = "./experiments/mtr/data/example1.Rdata")
