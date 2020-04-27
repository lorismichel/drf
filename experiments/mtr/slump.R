# slump dataset

# slump
# The Concrete Slump dataset (Yeh 2007) concerns the prediction of three properties of concrete (slump, flow and compressive strength) as a function of the content of seven concrete
# ingredients: cement,fly ash, blast furnace slag, water, superplasticizer, coarse aggregate, and
# fine aggregate.

# repro
set.seed(1)

# libs
require(mrf)

# source
source("./experiments/mtr/helpers.R")

# load the data
d <- loadMTRdata(path = "~/Downloads/mtr-datasets/", dataset.name = "slump")

# hyper-param selection 
res_hyper_param <- hyperParamSelection(Y=d$Y, X.knn = d$X.gauss, X.gauss = d$X.gauss, k = 2)

# best parameters
selected.k <- c(5, 10, 20, sqrt(nrow(d$X.knn)))[which.min(lapply(res_hyper_param$knn, function(res) mean(res)))]
selected.sigma <- c(0.1, 0.5, 1, 2)[which.min(lapply(res_hyper_param$gauss, function(res) mean(res)))]

# run pinball analysis (l)
res_pinball <- runRandomPinballAnalysis(param.knn = selected.k, param.gauss = selected.sigma, k = 2,
                                        X=d$X, Y=d$Y, X.knn = d$X.knn, X.gauss = d$X.gauss, num_features = 100)

# run pinball analysis (nl)
res_pinball_nl <- runRandomPinballNLAnalysis(param.knn = selected.k, param.gauss = selected.sigma, k = 2,
                                             X=d$X, Y=d$Y, X.knn = d$X.knn, X.gauss = d$X.gauss, num_features = 100)

# run coverage analysis 
res_coverage <- runNormalCoverage(param.knn = selected.k, param.gauss = selected.sigma, k = 2,
                                  X=d$X, Y=d$Y, X.knn = d$X.knn, X.gauss = d$X.gauss, num_features = 100)

# save results
save(d, res_pinball, res_pinball_nl, res_coverage, file = "./experiments/mtr/data/slump.Rdata")