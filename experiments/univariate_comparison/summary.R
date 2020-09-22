load("~/Desktop/resultsSimsUniDim.Rdata")
  
print(results)

print("Ozone")
print(lapply(results$Ozone[[1]]$q.losses, function(x) names(sort(x))))
print(lapply(results$Ozone[[2]]$q.losses, function(x) names(sort(x))))
print(lapply(results$Ozone[[3]]$q.losses, function(x) names(sort(x))))
print(lapply(results$Ozone[[4]]$q.losses, function(x) names(sort(x))))

print("Boston")
print(lapply(results$Boston[[1]]$q.losses, function(x) names(sort(x))))
print(lapply(results$Boston[[2]]$q.losses, function(x) names(sort(x))))
print(lapply(results$Boston[[3]]$q.losses, function(x) names(sort(x))))

print("Abalone")
print(lapply(results$Abalone[[1]]$q.losses, function(x) names(sort(x))))
print(lapply(results$Abalone[[2]]$q.losses, function(x) names(sort(x))))
print(lapply(results$Abalone[[3]]$q.losses, function(x) names(sort(x))))
