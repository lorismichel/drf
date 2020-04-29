# enb dataset

# ENB
# The Energy Building dataset (Tsanas and Xifara 2012) concerns the prediction of the heating
# load and cooling load requirements of buildings (i.e. energy efficiency) as a function of eight
# building parameters such as glazing area, roof area, and overall height, amongst others.

# repro
set.seed(1)

# libs
require(mrf)

# source
source("./experiments/mtr/helpers.R")

# load the data
d <- loadMTRdata(path = "~/Downloads/mtr-datasets/", dataset.name = "enb")

# hyper-param selection 
res_hyper_param <- hyperParamSelection(Y=d$Y, X.knn = d$X.gauss, X.gauss = d$X.gauss, k = 10)

# best parameters
selected.k <- c(5, 10, 20, sqrt(nrow(d$X.knn)))[which.min(lapply(res_hyper_param$knn, function(res) mean(res)))]
selected.sigma <- c(0.1, 0.5, 1, 2)[which.min(lapply(res_hyper_param$gauss, function(res) mean(res)))]

# run pinball analysis (l)
res_pinball <- runRandomPinballAnalysis(param.knn = selected.k, param.gauss = selected.sigma, k = 10,
                                        X=d$X, Y=d$Y, X.knn = d$X.knn, X.gauss = d$X.gauss, num_features = 100, 
                                        nb_random_directions = 20)

# # run pinball analysis (nl)
# res_pinball_nl <- runRandomPinballNLAnalysis(param.knn = selected.k, param.gauss = selected.sigma, k = 10,
#                                              X=d$X, Y=d$Y, X.knn = d$X.knn, X.gauss = d$X.gauss, num_features = 100)
# 
# # run coverage analysis 
# res_coverage <- runNormalCoverage(param.knn = selected.k, param.gauss = selected.sigma, k = 10,
#                                   X=d$X, Y=d$Y, X.knn = d$X.knn, X.gauss = d$X.gauss, num_features = 100)

# save results  res_pinball_nl, res_coverage,
save(d, res_pinball, file = "./experiments/mtr/data/enb.Rdata")
