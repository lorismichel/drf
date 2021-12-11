library(ggplot2)
library(plyr)
library(cowplot)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
df = read.csv('data/output/pred_mean_vignette.csv')

plotdf = df[df$response=='Y1' & df$method!='DRF_CART',]
plotdf$method = revalue(plotdf$method, c("DRF_MMD"="DRF","GP_RBF"="GP", "HOMO"="Homogeneous", "KNN"="kNN"))
plotdf$method = factor(plotdf$method, levels = c("DRF", "CGAN", "GP", "Homogeneous", "kNN"))
gg1 = ggplot(plotdf, aes(x=x, y=pred)) +
  geom_point(size=0.15) +
  geom_abline(color='red', linetype='dashed', size=1) +
  facet_grid(~method) +
  ylim(c(-1.7, 2.7)) +
  labs(x=expression(X["1"]), y=expression('mean of ' ~ Y["1"]))+
  theme_light()+
  theme(axis.text.x = element_text(size=7), axis.text.y = element_text(size=8))
gg1

df = read.csv('data/output/pred_quantile_09_vignette.csv')
plotdf2 = df[df$response=='Y2' & df$method!='DRF_CART',]
plotdf2$method = revalue(plotdf2$method, c("DRF_MMD"="DRF", "HOMO"="Homogeneous", "KNN"="kNN", "GP_RBF"="GP"))
plotdf2$method = factor(plotdf2$method, levels = c("DRF", "CGAN", "GP", "Homogeneous", "kNN"))
gg2 = ggplot(plotdf2, aes(x=x, y=pred)) +
  geom_point(size=0.15) +
  geom_abline(slope=qnorm(0.9), color='red', linetype='dashed', size=1) +
  #geom_abline(slope=-qnorm(0.9), color='red', linetype='dashed') +
  facet_grid(~method) +
  ylim(c(-0.1, 1.7)) +
  labs(x=expression(X["2"]), y=expression('0.9-quantile of ' ~ Y["2"])) +
  theme_light()+
  theme(axis.text.x = element_text(size=7), axis.text.y = element_text(size=8)) 
gg2

gg = plot_grid(gg1, gg2, ncol=1)
gg

df = read.csv('data/output/pred_quantile_01_vignette.csv')
plotdf = df[df$response=='Y2' & df$method!='DRF_CART',]
gg3 = ggplot(plotdf, aes(x=x, y=pred)) +
  geom_point() +
  geom_abline(slope=1.96, color='red', linetype='dashed') +
  geom_abline(slope=-1.96, color='red', linetype='dashed') +
  facet_grid(~method) +
  ylim(c(-3.2, 0.5)) +
  ylab("quantile of Y2")+
  theme_light()
gg3
