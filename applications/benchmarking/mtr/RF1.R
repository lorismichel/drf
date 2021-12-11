# RF1 dataset

# RF 
# The river flow datasets concern the prediction of river network flows for 48 h in the future
# at specific locations. The dataset contains data from hourly flow observations for 8 sites in
# the Mississippi River network in the United States and were obtained from the US National
# Weather Service. Each row includes the most recent observation for each of the 8 sites as
# well as time-lagged observations from 6, 12, 18, 24, 36, 48 and 60 h in the past. In RF1, each
# site contributes 8 attribute variables to facilitate prediction. There are a total of 64 variables
# plus 8 target variables.The RF2 dataset extends the RF1 data by adding precipitation forecast
# information for each of the 8 sites (expected rainfall reported as discrete values: 0.0, 0.01, 0.25,
#                                      1.0 inches). For each observation and gauge site, the precipitation forecast for 6 h windows
# up to 48 h in the future is added (6, 12, 18, 24, 30, 36, 42, and 48 h). The two datasets both
# contain over 1 year of hourly observations (>9000 h) collected from September 2011 to
# September 2012. The domain is a natural candidate for multi-target regression because there
# are clear physical relationships between readings in the contiguous river network.

# repro
set.seed(1)

# libs
require(mrf)

# source
source("./experiments/mtr/helpers.R")

# load the data
d <- loadMTRdata(path = "~/Downloads/mtr-datasets/", dataset.name = "RF2")

# hyper-param selection 
res_hyper_param <- hyperParamSelection(Y=d$Y, X.knn = d$X.gauss, X.gauss = d$X.gauss, k = 2)

# best parameters
selected.k <- c(5, 10, 20, sqrt(nrow(d$X.knn)))[which.min(lapply(res_hyper_param$knn, function(res) mean(res)))]
selected.sigma <- c(0.1, 0.5, 1, 2, 10)[which.min(lapply(res_hyper_param$gauss, function(res) mean(res)))]

# run pinball analysis (l)
res_pinball <- runRandomPinballAnalysis(param.knn = selected.k, param.gauss = selected.sigma, k = 2,
                                        X=d$X, Y=d$Y, X.knn = d$X.knn, X.gauss = d$X.gauss, num_features = 100, nb_random_directions = 100)

# # run pinball analysis (nl)
# res_pinball_nl <- runRandomPinballNLAnalysis(param.knn = selected.k, param.gauss = selected.sigma, k = 5,
#                                              X=d$X, Y=d$Y, X.knn = d$X.gauss, X.gauss = d$X.gauss, num_features = 100)
# 
# # run coverage analysis 
# res_coverage <- runNormalCoverage(param.knn = selected.k, param.gauss = selected.sigma, k = 5,
#                                   X=d$X, Y=d$Y, X.knn = d$X.gauss, X.gauss = d$X.gauss, num_features = 100)


# save results
save(d, res_pinball, selected.k, selected.sigma, file = "./experiments/mtr/data/RF1.Rdata")
