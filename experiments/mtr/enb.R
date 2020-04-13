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
d <- loadMTRdata(dataset.name = "enb")

# run pinball analysis (l)
res_pinball <- runRandomPinballAnalysis(X=d$X, Y=d$Y, num_features = 100)

# run pinball analysis (nl)
res_pinball_nl <- runRandomPinballNLAnalysis(X=d$X, Y=d$Y, num_features = 100)

# run coverage analysis
res_coverage <- runNormalCoverage(X=d$X, Y=d$Y, num_features = 100)

# save results
save(res_pinball, res_pinball_nl, res_coverage, file = "./experiments/mtr/data/enb.Rdata")