# repro
set.seed(1)

# proceed to the analysis
source("./genData.R")
source("./univariateComparison.R")

# names of datasets
#dataset.names <- c("synthetic1", "synthetic2", 
#                   "synthetic3", "synthetic4",
#                   "Abalone", 
#                   "Boston", 
#                   #"BigMac", 
#                   "Ozone")

dataset.names <- c("synthetic1","synthetic2","synthetic3")
# rep per datasets
nrep <- 10

results <- list()
for (dname in dataset.names) {
  for (i in 1:nrep) {
    for (p in c(39)) {

    results[[dname]][[i]] <- tryCatch(univariateComparison(dataset = dname, 
                         verbose = FALSE, n = 2000, meanShift = 0.8, sdShift = 1,
                         p = p, test.frac = 0.3, quantiles.grid = c(0.1, 0.3, 0.5, 0.7, 0.9)),error=function(e) e)
    }
    print(i)
  }
  print(dname)
}

save(results, file = "./results_univariate_paper.Rdata")

