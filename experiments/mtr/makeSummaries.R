# datasets
set.seed(0)
andro.summaries <- makeSummaries(dataset = "andro", nrep = 500)
slump.summaries <- makeSummaries(dataset = "slump", nrep = 500)
edm.summaries <- makeSummaries(dataset = "edm", nrep = 500)
enb.summaries <- makeSummaries(dataset = "enb", nrep = 500)
scpf.summaries <- makeSummaries(dataset = "scpf", nrep = 500)
jura.summaries <- makeSummaries(dataset = "jura", nrep = 500)
wq.summaries <- makeSummaries(dataset = "wq", nrep = 500)
example1.summaries <- makeSummaries(dataset = "example1", nrep = 500)


res <- lapply(list(andro.summaries, slump.summaries,
         edm.summaries, enb.summaries,
         scpf.summaries, jura.summaries,
         wq.summaries, example1.summaries), function(l) cbind(unlist(lapply(l$vec.mse.u.mrf, function(ll) mean(ll))), unlist(lapply(l$vec.mse.u.gini, function(ll) mean(ll))), unlist(lapply(l$vec.mse.u.gauss, function(ll) mean(ll))), unlist(lapply(l$vec.mse.u.knn, function(ll) mean(ll)))))

s <- cbind(jura.summaries, slump.summaries, wq.summaries, edm.summaries, enb.summaries, andro.summaries, example1.summaries)
s
