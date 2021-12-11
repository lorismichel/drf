library(ggplot2)
library(plyr)
library(cowplot)
library(reticulate)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source_python("read_pickle.py")
pickle_data <- read_pickle_file("data/output/vignette.data")

n_test = 1000
plotdf = data.frame(x=pickle_data$X_test[1:n_test,1], y=pickle_data$ypred_drf_sample_test[1:n_test,1,10], method="DRF")
plotdf = rbind(plotdf, 
               data.frame(x=pickle_data$X_test[1:n_test,1], y=pickle_data$ypred_gan_sample_test[1:n_test,1,10], method="CGAN"))
plotdf = rbind(plotdf, 
               data.frame(x=pickle_data$X_test[1:n_test,1], y=pickle_data$ypred_cvae_sample_test[1:n_test,1,10], method="CVAE"))
plotdf = rbind(plotdf, 
               data.frame(x=pickle_data$X_test[1:n_test,1], y=pickle_data$ypred_maf_sample_test[1:n_test,1,10], method="MAF"))
plotdf = rbind(plotdf, 
               data.frame(x=pickle_data$X_test[1:n_test,1], y=pickle_data$ypred_knn_sample_test[1:n_test,1,10], method="k-NN"))
#plotdf = rbind(plotdf, 
#               data.frame(x=pickle_data$X_test[1:n_test,1], y=pickle_data$ypred_rf_sample_test[1:n_test,1,10], method="RF"))

plotdf$method = factor(plotdf$method, levels = c("DRF", "CGAN", "CVAE", "MAF", "k-NN"))

gg1 = ggplot(plotdf, aes(x=x, y=y)) +
  geom_point(size=0.01, aes(color=method)) +
  #geom_smooth(aes(color=method)) +
  geom_abline(intercept=qunif(0.5), color='black', linetype='solid', size=0.5) +
  geom_abline(intercept=qunif(0.1), color='black', linetype='dashed', size=0.5) +
  geom_abline(intercept=qunif(0.9), color='black', linetype='dashed', size=0.5) +
  facet_grid(.~method) +
  labs(x=expression(X["1"]), y=expression(Y["1"]))+
  theme_light()+
  theme(axis.text.x = element_text(size=7), axis.text.y = element_text(size=8), legend.position='none')+
  ylim(c(-0.3, 2.2))
gg1

#qplot(pickle_data$X_test[,1], y=pickle_data$ypred_knn_sample_test[,1,1])
#qplot(pickle_data$X_test[,2], y=pickle_data$ypred_gp_rbf_sample_test[,2,1])


plotdf2 = data.frame(x=pickle_data$X_test[1:n_test,2], y=pickle_data$ypred_drf_sample_test[1:n_test,2,10], method="DRF")
plotdf2 = rbind(plotdf2, 
               data.frame(x=pickle_data$X_test[1:n_test,2], y=pickle_data$ypred_gan_sample_test[1:n_test,2,10], method="CGAN"))
plotdf2 = rbind(plotdf2, 
               data.frame(x=pickle_data$X_test[1:n_test,2], y=pickle_data$ypred_cvae_sample_test[1:n_test,2,10], method="CVAE"))
plotdf2 = rbind(plotdf2, 
               data.frame(x=pickle_data$X_test[1:n_test,2], y=pickle_data$ypred_maf_sample_test[1:n_test,2,10], method="MAF"))
plotdf2 = rbind(plotdf2, 
               data.frame(x=pickle_data$X_test[1:n_test,2], y=pickle_data$ypred_knn_sample_test[1:n_test,2,10], method="k-NN"))
#plotdf2 = rbind(plotdf2, 
#                data.frame(x=pickle_data$X_test[1:n_test,2], y=pickle_data$ypred_rf_sample_test[1:n_test,2,10], method="RF"))

plotdf2$method = factor(plotdf2$method, levels = c("DRF", "CGAN", "CVAE", "MAF", "k-NN"))

gg2 = ggplot(plotdf2, aes(x=x, y=y)) +
  geom_point(size=0.01, aes(color=method)) +
  #geom_smooth(aes(color=method)) +
  geom_abline(slope=qunif(0.5), color='black', linetype='solid', size=0.5) +
  geom_abline(slope=qunif(0.1), color='black', linetype='dashed', size=0.5) +
  geom_abline(slope=qunif(0.9), color='black', linetype='dashed', size=0.5) +
  facet_grid(.~method) +
  labs(x=expression(X["2"]), y=expression(Y["2"]))+
  theme_light()+
  theme(axis.text.x = element_text(size=7), axis.text.y = element_text(size=8), legend.position='none')+
  ylim(c(-0.35, 1.05))
gg2

plot_grid(gg1, gg2, ncol=1)
ggsave('plots/vignette.png', width=10, height=4)
