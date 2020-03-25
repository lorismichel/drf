# wq dataset

# WQ
# The Water Quality dataset (Dzeroski et al. 2000) has 14 target attributes that refer to the
# relative representation of plant and animal species in Slovenian rivers and 16 input attributes
# that refer to physical and chemical water quality parameters.

# repro
set.seed(1)

# libs
require(mrf)

# source
source("./experiments/mtr/helpers.R")

# load the data
d <- loadMTRdata(dataset.name = "wq")

# run analysis
res <- runRandomPinballAnalysis(X=d$X, Y=d$Y, num_features = 100)

# save results
save(res, file = "./experiments/mtr/data/wq.Rdata")
