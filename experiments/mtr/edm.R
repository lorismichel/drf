# edm dataset

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
d <- loadMTRdata(dataset.name = "edm")

# run analysis
res <- runRandomPinballAnalysis(X=d$X, Y=d$Y, num_features = 100)

# save results
save(res, file = "./experiments/mtr/data/edm.Rdata")