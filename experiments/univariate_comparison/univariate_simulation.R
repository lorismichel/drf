# simulations for the paper about univariate analysis

# libs
require(parallel)

# reproduciblity of the simulations
set.seed(1)

# proceed to the analysis
source("./genData.R")
source("./univariateComparison.R")

# dataasets used for simulation
dataset.names <- c("synthetic1","synthetic2","synthetic3")


# rep per datasets
nrep <- 10

# stored results
results.quantiles <- list()
results.mean <- list()

# quantile losses
for (dname in dataset.names) {

    results[[dname]] <- mclapply(X = 1:nrep, FUN = function(i) tryCatch(univariateComparison(dataset = dname,
                                                                                             verbose = FALSE,
											                                                                       n = 2000,
											                                                                       meanShift = 0.8,
										                                                                         sdShift = 1,
                         								                                                     p = 39,
											                                                                       test.frac = 0.3,
											                                                                       quantiles.grid = setdiff(seq(0,1,length.out = 100),c(0,1))),
									                                                       error=function(e) e),
                                                                         mc.set.seed = 1)

    print(dname)
}

save(results.quantiles, file = "../../data/computed_data/results_quantiles_univariate_paper_final.Rdata")


# mse losses
for (dname in dataset.names) {
  
  results.mean[[dname]] <- mclapply(X = 1:nrep, FUN = function(i) tryCatch(univariateComparison(dataset = dname,
                                                                                           verbose = FALSE,
                                                                                           n = 1000,
                                                                                           meanShift = 0.8,
                                                                                           sdShift = 1,
                                                                                           p = 39,
                                                                                           test.frac = 0.3,
                                                                                           quantiles.grid = setdiff(seq(0,1,length.out = 100),c(0,1)),
                                                                                           conditional.mean.analysis = TRUE),
                                                                      error=function(e) e),
                               mc.set.seed = 1)
  
  print(dname)
}

save(results.mean, file = "../../data/computed_data/results_mean_univariate_paper_final.Rdata")
