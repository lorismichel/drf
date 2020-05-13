# wq dataset

# WQ
# The Water Quality dataset (Dzeroski et al. 2000) has 14 target attributes that refer to the
# relative representation of plant and animal species in Slovenian rivers and 16 input attributes
# that refer to physical and chemical water quality parameters.

# repro
set.seed(1)

# libs
require(mrf)

# params
USE.RES <- TRUE

# source
source("./experiments/mtr/helpers.R")

# load the data
d <- loadMTRdata(path = "~/Downloads/mtr-datasets/", dataset.name = "wq")

if (USE.RES) {
  res <- ResRF(X = d$X, Y = d$Y)
  Y <- res$residuals
} else {
  Y <- d$Y
}


# hyper-param selection 
res_hyper_param <- hyperParamSelection(Y=Y, X.knn = d$X.gauss, X.gauss = d$X.gauss, k = 2)

# best parameters
selected.k <- c(5, 10, sqrt(nrow(d$X.knn)), nrow(d$X.knn))[which.min(lapply(res_hyper_param$knn, function(res) mean(res)))]
selected.sigma <- c(0.1, 0.5, 1, 2, 10)[which.min(lapply(res_hyper_param$gauss, function(res) mean(res)))]

# run pinball analysis (l)
res_pinball <- runRandomPinballAnalysis(param.knn = selected.k, param.gauss = selected.sigma, k = 2,
                                        X=d$X, Y=Y, X.knn = d$X.knn, X.gauss = d$X.gauss, nb_random_directions = 100,
                                        num_features = 10)

# # run pinball analysis (nl)
# res_pinball_nl <- runRandomPinballNLAnalysis(param.knn = selected.k, param.gauss = selected.sigma, k = 2,
#                                              X=d$X, Y=d$Y, X.knn = d$X.knn, X.gauss = d$X.gauss, num_features = 100)
# 
# # run coverage analysis 
# res_coverage <- runNormalCoverage(param.knn = selected.k, param.gauss = selected.sigma, k = 2,
#                                   X=d$X, Y=d$Y, X.knn = d$X.knn, X.gauss = d$X.gauss, num_features = 100)

# save results
#res_pinball_nl, res_coverage
save(d, res_pinball, selected.k, selected.sigma, file = "./experiments/mtr/data/wq.Rdata")
