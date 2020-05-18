# andro dataset

# ANDRO
# The Andromeda dataset (Hatzikos et al. 2008) concerns the prediction of future values for
# six water quality variables (temperature, pH, conductivity, salinity, oxygen, turbidity) in
# Thermaikos Gulf of Thessaloniki, Greece. Measurements of the target variables are taken
# from under-water sensors with a sampling interval of 9 seconds and then averaged to get a
# single measurement for each variable over each day. The specific dataset that we use here
# corresponds to using a window of 5 days (i.e. features attributes correspond to the values of
# the six water quality variables up to 5 days in the past) and a lead of 5 days (i.e. we predict

# repro
set.seed(1)

# libs
require(mrf)

# params
USE.RES <- TRUE

# source
source("./experiments/mtr/helpers.R")

# load the data
d <- loadMTRdata(path = "~/Downloads/mtr-datasets/", dataset.name = "andro")

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
res_pinball <- runRandomPinballAnalysis(param.knn = selected.k, param.gauss = selected.sigma, k = 2, alpha_seq = 0.9,
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
save(d, res_pinball, selected.k, selected.sigma, file = "./experiments/mtr/data/andro.Rdata")