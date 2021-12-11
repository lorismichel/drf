library(ggplot2)
library(cowplot)

setwd('~/Documents/projects/DRF/plots/univariate/')
load('univariateQuantiles.Rdata')

#scenario 1

gg1 = ggplot(df) + 
  #geom_smooth(method = 'gam', se=TRUE, aes(color=method, fill=method)) + 
  geom_point(aes(x=active, y=q.9, color=method)) + 
  facet_grid(~method) +
  theme_light() + 
  labs(x=expression(X["1"]), y='quantile of Y') +
  geom_line(data=data.frame(x=seq(-1, 0, length.out=100), y=qnorm(0.9, mean=0, sd=1)), 
            aes(x=x, y=y), linetype='dashed') + 
  geom_line(data=data.frame(x=seq(0, 1, length.out=100), y=qnorm(0.9, mean=0.8, sd=1)), 
            aes(x=x, y=y), linetype='dashed')+
  geom_point(aes(x=active, y=q.5, color=method)) + 
  geom_line(data=data.frame(x=seq(-1, 0, length.out=100), y=qnorm(0.5, mean=0, sd=1)), 
            aes(x=x, y=y), linetype='dashed') + 
  geom_line(data=data.frame(x=seq(0, 1, length.out=100), y=qnorm(0.5, mean=0.8, sd=1)), 
            aes(x=x, y=y), linetype='dashed')+
  geom_point(aes(x=active, y=q.1, color=method)) + 
  geom_line(data=data.frame(x=seq(-1, 0, length.out=100), y=qnorm(0.1, mean=0, sd=1)), 
            aes(x=x, y=y), linetype='dashed') + 
  geom_line(data=data.frame(x=seq(0, 1, length.out=100), y=qnorm(0.1, mean=0.8, sd=1)), 
            aes(x=x, y=y), linetype='dashed')
  

#scenario 2

load('univariateQuantiles2.Rdata')
gg2 = ggplot(df, aes(x=active, y=q.9)) + 
  #geom_smooth(method = 'gam', se=TRUE, aes(color=method, fill=method)) + 
  geom_point(aes(color=method)) + facet_grid(~method) +
  theme_light() + 
  labs(x=expression(X["1"]), y='quantile of Y') +
  geom_line(data=data.frame(x=seq(-1, 0, length.out=100), y=qnorm(0.9, mean=0, sd=1)), 
            aes(x=x, y=y), linetype='dashed') + 
  geom_line(data=data.frame(x=seq(0, 1, length.out=100), y=qnorm(0.9, mean=0, sd=2)), 
            aes(x=x, y=y), linetype='dashed')+
  geom_point(aes(x=active, y=q.5, color=method)) + 
  geom_line(data=data.frame(x=seq(-1, 0, length.out=100), y=qnorm(0.5, mean=0, sd=1)), 
            aes(x=x, y=y), linetype='dashed') + 
  geom_line(data=data.frame(x=seq(0, 1, length.out=100), y=qnorm(0.5, mean=0, sd=2)), 
            aes(x=x, y=y), linetype='dashed')+
  geom_point(aes(x=active, y=q.1, color=method)) + 
  geom_line(data=data.frame(x=seq(-1, 0, length.out=100), y=qnorm(0.1, mean=0, sd=1)), 
            aes(x=x, y=y), linetype='dashed') + 
  geom_line(data=data.frame(x=seq(0, 1, length.out=100), y=qnorm(0.1, mean=0, sd=2)), 
            aes(x=x, y=y), linetype='dashed')
  

#scenario 3

load('univariateQuantiles3.Rdata')
gg3 = ggplot(df, aes(x=active, y=q.9)) + 
  #geom_smooth(method = 'gam', se=TRUE, aes(color=method, fill=method)) + 
  geom_point(aes(color=method)) + facet_grid(~method) +
  theme_light() + 
  labs(x=expression(X["1"]), y='quantile of Y') +
  geom_line(data=data.frame(x=seq(-1, 0, length.out=100), y=qnorm(0.9, mean=1, sd=1)), 
            aes(x=x, y=y), linetype='dashed') + 
  geom_line(data=data.frame(x=seq(0, 1, length.out=100), y=qexp(0.9, rate=1)), 
            aes(x=x, y=y), linetype='dashed') + 
  geom_point(aes(x=active, y=q.5, color=method)) + 
  geom_line(data=data.frame(x=seq(-1, 0, length.out=100), y=qnorm(0.5, mean=1, sd=1)), 
            aes(x=x, y=y), linetype='dashed') + 
  geom_line(data=data.frame(x=seq(0, 1, length.out=100), y=qexp(0.5, rate=1)), 
            aes(x=x, y=y), linetype='dashed') + 
  geom_point(aes(x=active, y=q.1, color=method)) + 
  geom_line(data=data.frame(x=seq(-1, 0, length.out=100), y=qnorm(0.1, mean=1, sd=1)), 
            aes(x=x, y=y), linetype='dashed') + 
  geom_line(data=data.frame(x=seq(0, 1, length.out=100), y=qexp(0.1, rate=1)), 
            aes(x=x, y=y), linetype='dashed') +
  ylim(c(-0.5,3.2))

plot_grid(gg1, gg2, gg3, ncol=1)

#all

library(plyr)
load('univariateQuantiles.Rdata')
df$truth0.1 = qnorm(0.1, mean=0.8*(df$active > 0), sd=1)
df$truth0.5 = qnorm(0.5, mean=0.8*(df$active > 0), sd=1)
df$truth0.9 = qnorm(0.9, mean=0.8*(df$active > 0), sd=1)
plotdf = data.frame(df, scenario='1')
load('univariateQuantiles2.Rdata')
df$truth0.1 = qnorm(0.1, sd=1+(df$active > 0))
df$truth0.5 = qnorm(0.5, sd=1+(df$active > 0))
df$truth0.9 = qnorm(0.9, sd=1+(df$active > 0))
plotdf = rbind(plotdf, data.frame(df, scenario='2'))
load('univariateQuantiles3.Rdata')
df$truth0.1 = ifelse(df$active > 0, qexp(0.1, rate=1), qnorm(0.1, mean=1, sd=1))
df$truth0.5 = ifelse(df$active > 0, qexp(0.5, rate=1), qnorm(0.5, mean=1, sd=1))
df$truth0.9 = ifelse(df$active > 0, qexp(0.9, rate=1), qnorm(0.9, mean=1, sd=1))
plotdf = rbind(plotdf, data.frame(df, scenario='3'))

plotdf$method = revalue(plotdf$method, c("drf"="DRF", "grf"="GRF","qrf"="QRF", "trf"="TRF"))
plotdf$scenario = revalue(plotdf$scenario, c("1"="1: mean shift", "2"="2: variance shift", "3"="3: mean&variance constant"))
gg = ggplot(plotdf[plotdf$q.9<3.1,]) + 
  #geom_smooth(method = 'gam', se=TRUE, aes(color=method, fill=method)) + 
  geom_point(aes(x=active, y=q.1, color=method), size=0.4) +
  geom_line(aes(x=active, y=truth0.1), linetype='dashed') + 
  geom_point(aes(x=active, y=q.5, color=method), size=0.4) +
  geom_line(aes(x=active, y=truth0.5), linetype='dashed') + 
  geom_point(aes(x=active, y=q.9, color=method), size=0.4) +
  geom_line(aes(x=active, y=truth0.9), linetype='dashed') + 
  facet_grid(scenario~method, scales='free') +
  theme_light() + 
  labs(x=expression(X["1"]), y='quantile of Y') +
  theme(legend.position='none')+
  theme(axis.text.x = element_text(size=10), axis.text.y = element_text(size=10),
        axis.title=element_text(size=14))
gg
ggsave(gg, filename='~/Documents/projects/DRF/plots/univariate/univariate.png', width=10, height=5.5)

gg = ggplot(plotdf[plotdf$q.9<3.1,]) + 
  #geom_smooth(method = 'gam', se=TRUE, aes(color=method, fill=method)) + 
  #geom_point(aes(x=active, y=q.1, color=method), size=0.4) +
  #geom_line(aes(x=active, y=truth0.1), linetype='dashed') + 
  geom_point(aes(x=active, y=q.5, color=method), alpha=0.5, fill='black') +
  geom_line(aes(x=active, y=truth0.5), linetype='dashed') + 
  geom_point(aes(x=active, y=q.9, color=method), size=0.4) +
  geom_line(aes(x=active, y=truth0.9), linetype='dashed') + 
  facet_grid(scenario~method, scales='free') +
  theme_light() + 
  labs(x=expression(X["1"]), y='quantile of Y') +
  theme(legend.position='none')+
  theme(axis.text.x = element_text(size=10), axis.text.y = element_text(size=10),
        axis.title=element_text(size=14))
gg



