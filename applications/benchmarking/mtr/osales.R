# osales dataset

# OSALES
# This is a pre-processed version of the dataset used in Kaggle’s “Online Product Sales” competition (Kaggle 2012) that concerns the prediction of the online sales of consumer products.
# Each row in the dataset corresponds to a different product that is described by various prod123
# 84 Mach Learn (2016) 104:55–98
# uct features as well as features of an advertising campaign. There are 12 target variables
# corresponding to the monthly sales for the first 12 months after the product launches. For the
# purposes of this study we removed examples with missing values in any target variable (112
#                                                                                        out of 751) and attributes with one distinct value (145 out of 558).                                                                              for less than 1% of the cases) were removed.

# repro
set.seed(1)

# libs
require(mrf)

# source
source("./experiments/mtr/helpers.R")

# load the data
d <- loadMTRdata(path = "~/Downloads/mtr-datasets/", dataset.name = "osales")

# hyper-param selection 
res_hyper_param <- hyperParamSelection(Y=d$Y, X.knn = d$X.knn, X.gauss = d$X.gauss, k = 2)

# best parameters
selected.k <- c(5, 10, 20, sqrt(nrow(d$X.knn)))[which.min(lapply(res_hyper_param$knn, function(res) mean(res)))]
selected.sigma <- c(0.1, 0.5, 1, 2, 10)[which.min(lapply(res_hyper_param$gauss, function(res) mean(res)))]

# run pinball analysis (l)
res_pinball <- runRandomPinballAnalysis(param.knn = selected.k, param.gauss = selected.sigma, k = 2, alpha_seq = 0.9,
                                        X=d$X, Y=d$Y, X.knn = d$X.knn, X.gauss = d$X.gauss, num_features = 100, nb_random_directions = 100)

# # run pinball analysis (nl)
# res_pinball_nl <- runRandomPinballNLAnalysis(param.knn = selected.k, param.gauss = selected.sigma, k = 10,
#                                              X=d$X, Y=d$Y, X.knn = d$X.knn, X.gauss = d$X.gauss, num_features = 100)
# 
# # run coverage analysis 
# res_coverage <- runNormalCoverage(param.knn = selected.k, param.gauss = selected.sigma, k = 10,
#                                   X=d$X, Y=d$Y, X.knn = d$X.knn, X.gauss = d$X.gauss, num_features = 100)


# save results res_pinball_nl, res_coverage, 
save(d, res_pinball, selected.k, selected.sigma, file = "./experiments/mtr/data/osales.Rdata")