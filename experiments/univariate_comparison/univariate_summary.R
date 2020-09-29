# creating the summaries
load("./../../experiments/univariate_comparison/results_univariate_paper_w2.Rdata")


# pinball for first example
print("synthetic1")
mat.res.pinball <- Reduce(x = results$synthetic1[[1]]$q.losses[1:5], f = rbind)
for (i in 2:length(results$synthetic1)) {
  mat.res.pinball <- mat.res.pinball + Reduce(x = results$synthetic1[[i]]$q.losses[1:5], f = rbind)
}
mat.res.pinball <- mat.res.pinball / 10
print(mat.res.pinball)

# pinball for second example
print("synthetic2")
mat.res.pinball <- Reduce(x = results$synthetic2[[1]]$q.losses[1:5], f = rbind)
for (i in 2:length(results$synthetic2)) {
  mat.res.pinball <- mat.res.pinball + Reduce(x = results$synthetic2[[i]]$q.losses[1:5], f = rbind)
}
mat.res.pinball <- mat.res.pinball / 10
print(mat.res.pinball)

# pinball for third example
print("synthetic3")
mat.res.pinball <- Reduce(x = results$synthetic3[[1]]$q.losses[1:5], f = rbind)
for (i in 2:length(results$synthetic3)) {
  mat.res.pinball <- mat.res.pinball + Reduce(x = results$synthetic3[[i]]$q.losses[1:5], f = rbind)
}
mat.res.pinball <- mat.res.pinball / 10
print(mat.res.pinball)

