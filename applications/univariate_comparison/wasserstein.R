library(tidyverse)
library(cowplot)

setwd('~/Documents/projects/DRF/plots/univariate/wasserstein')

library(plyr)
load('quantiles_scenario1.Rdata')
df$truth = qnorm(df$q, mean=0.8*(df$x > 0), sd=1)
plotdf = data.frame(df, scenario='1')
load('quantiles_scenario2.Rdata')
df$truth = qnorm(df$q, sd=1+(df$x > 0))
plotdf = rbind(plotdf, data.frame(df, scenario='2'))
load('quantiles_scenario3.Rdata')
df$truth = ifelse(df$x > 0, qexp(0.9, rate=1), qnorm(0.9, mean=1, sd=1))
plotdf = rbind(plotdf, data.frame(df, scenario='3'))
#plotdf$method = revalue(plotdf$method, c("drf"="DRF", "grf"="GRF","qrf"="QRF", "trf"="TRF"))
plotdf$scenario = revalue(plotdf$scenario, c("1"="1: mean shift", "2"="2: variance shift", "3"="3: mean & variance constant"))

plotdf2 = 
  plotdf %>% mutate(error=(truth-value)^2) %>% 
  group_by(x, method, scenario) %>% 
  summarise(wasserstein=mean(error)^0.5)

gg = ggplot(plotdf2) + 
  #geom_smooth(method = 'gam', se=TRUE, aes(color=method, fill=method)) + 
  geom_line(aes(x=x, y=wasserstein, color=method), size=0.6) +  
  facet_wrap(.~scenario, scales='free') +
  theme_light() + 
  labs(x=expression(X["1"]), y=expression(W["2"])) +
  theme(legend.position='top')+
  theme(axis.text.x = element_text(size=9), axis.text.y = element_text(size=9),
        axis.title=element_text(size=12))
gg
ggsave(gg, filename='~/Documents/projects/DRF/plots/univariate/wasserstein/wasserstein.png', width=8.5, height=3)

