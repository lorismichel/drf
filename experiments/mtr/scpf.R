# scpf dataset

# SCPF
# This is a pre-processed version of the dataset used in Kaggle’s “See Click Predict Fix”
# competition (Kaggle 2013). It concerns the prediction of three target variables that represent
# the number of views, clicks and comments that a specific 311 issue will receive. The issues
# have been collected from 4 cities (Oakland, Richmond, New Haven, Chicago) in the US and
# span a period of 12 months (01/2012–12/2012). The version of the dataset that we use here is
# a random 1% sample of the data. In terms of features we use the number of days that an issues
# stayed online, the source from where the issue was created (e.g. android, iphone, remote api,
#                                                             etc.), the type of the issue (e.g. graffiti, pothole, trash, etc.), the geographical co-ordinates of
# the issue, the city it was published from and the distance from the city center. All multi-valued
# nominal variables were first transformed to binary and then rare binary variables (being true
#                                                                                    for less than 1% of the cases) were removed.

# repro
set.seed(1)

# libs
require(mrf)

# params
USE.RES <- TRUE

# source
source("./experiments/mtr/helpers.R")

# load the data
d <- loadMTRdata(path = "~/Downloads/mtr-datasets/", dataset.name = "scpf")

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
save(d, res_pinball, selected.k, selected.sigma, file = "./experiments/mtr/data/scpf.Rdata")
