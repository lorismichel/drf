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
d <- loadMTRdata(dataset.name = "osales")

# run analysis
res <- runRandomPinballAnalysis(X=d$X, Y=d$Y, num_features = 100)

# save results
save(res, file = "./experiments/mtr/data/osales.Rdata")