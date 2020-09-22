load("./results_paper.Rdata")

print(results)



print("synthetic1")
print(lapply(results$synthetic1[[1]][[39]]$q.losses, function(x) names(sort(x))))
#print(lapply(results$synthetic1[[2]]$q.losses, function(x) names(sort(x))))
#print(lapply(results$synthetic1[[3]]$q.losses, function(x) names(sort(x))))
#print(lapply(results$synthetic1[[4]]$q.losses, function(x) names(sort(x))))


print("synthetic2")
print(lapply(results$synthetic2[[1]][[39]]$q.losses, function(x) names(sort(x))))
#print(lapply(results$synthetic2[[2]]$q.losses, function(x) names(sort(x))))
#print(lapply(results$synthetic2[[3]]$q.losses, function(x) names(sort(x))))
#print(lapply(results$synthetic2[[4]]$q.losses, function(x) names(sort(x))))

print("synthetic3")
print(lapply(results$synthetic3[[1]][[39]]$q.losses, function(x) names(sort(x))))
#print(lapply(results$synthetic3[[2]]$q.losses, function(x) names(sort(x))))
#print(lapply(results$synthetic3[[3]]$q.losses, function(x) names(sort(x))))
#print(lapply(results$synthetic3[[4]]$q.losses, function(x) names(sort(x))))

print("synthetic4")
print(lapply(results$synthetic4[[1]]$q.losses, function(x) names(sort(x))))
print(lapply(results$synthetic4[[2]]$q.losses, function(x) names(sort(x))))
print(lapply(results$synthetic4[[3]]$q.losses, function(x) names(sort(x))))
print(lapply(results$synthetic4[[4]]$q.losses, function(x) names(sort(x))))


print("Ozone")
print(lapply(results$Ozone[[1]]$q.losses, function(x) names(sort(x))))

print("Boston")
print(lapply(results$Boston[[1]]$q.losses, function(x) names(sort(x))))

print("Abalone")
print(lapply(results$Abalone[[1]]$q.losses, function(x) names(sort(x))))
