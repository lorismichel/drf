# generate plots of quantile lines for the appendix

# libs
require(parallel)

# reproduciblity of the simulations
set.seed(1)

# proceed to the analysis
source("./genData.R")
source("./univariateComparison.R")

# datasets names
dataset.names <- c("synthetic1","synthetic2","synthetic3")

# repro
set.seed(1)

res1 <- univariateComparison(dataset = "synthetic1",
                     verbose = TRUE,
                     n = 200,
                     meanShift = 0.8,
                     sdShift = 1,
                     p = 39,
                     test.frac = 0.3,
                     quantiles.grid = c(0.1, 0.3, 0.5, 0.7, 0.9, seq(0.1,0.9,length.out = 10)))
res2 <- univariateComparison(dataset = "synthetic2",
                     verbose = TRUE,
                     n = 200,
                     meanShift = 0.8,
                     sdShift = 1,
                     p = 39,
                     test.frac = 0.3,
                     quantiles.grid = c(0.1, 0.3, 0.5, 0.7, 0.9, 0.05))
res3 <- univariateComparison(dataset = "synthetic3",
                     verbose = TRUE,
                     n = 2000,
                     meanShift = 0.8,
                     sdShift = 1,
                     p = 39,
                     test.frac = 0.3,
                     quantiles.grid = c(0.1, 0.3, 0.5, 0.7, 0.9, 0.05))