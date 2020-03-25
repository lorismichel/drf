# slump dataset

# slump
# The Concrete Slump dataset (Yeh 2007) concerns the prediction of three properties of concrete (slump, flow and compressive strength) as a function of the content of seven concrete
# ingredients: cement,fly ash, blast furnace slag, water, superplasticizer, coarse aggregate, and
# fine aggregate.

# repro
set.seed(1)

# libs
require(mrf)

# source
source("./experiments/mtr/helpers.R")

# load the data
d <- loadMTRdata(dataset.name = "slump")

# run analysis
res <- runRandomPinballAnalysis(X=d$X, Y=d$Y, num_features = 100)

# save results
save(res, file = "./experiments/mtr/data/slump.Rdata")