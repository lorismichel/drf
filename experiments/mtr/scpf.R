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

# source
source("./experiments/mtr/helpers.R")

# load the data
d <- loadMTRdata(dataset.name = "scpf")

# run analysis
res <- runRandomPinballAnalysis(X=d$X, Y=d$Y, num_features = 100)

# save results
save(res, file = "./experiments/mtr/data/scpf.Rdata")