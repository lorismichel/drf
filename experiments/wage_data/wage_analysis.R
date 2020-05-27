library(drf)
library(ggplot2)
library(fastDummies)
library(Hmisc)

load("~/Documents/projects/heterogeneity/wage_data/wage_data")

which = rep(TRUE, nrow(wage))
which = which & (wage$age >= 17)
which = which & (wage$weeks_worked > 48)
which = which & (wage$hours_worked > 16)
which = which & (wage$employment_status == 'employed')
which = which & (wage$employer != 'self-employed')
which[is.na(which)] = FALSE

data = wage[which, ]
sum(is.na(data))
colSums(is.na(data))
rownames(data) = 1:nrow(data)
#data = na.omit(data)

data$log_wage = log(data$salary / (data$weeks_worked * data$hours_worked))

data$weight=1
data$plotweight[data$sex=='female'] = data$weight[data$sex=='female']/sum(data$weight[data$sex=='female'])
data$plotweight[data$sex=='male'] = data$weight[data$sex=='male']/sum(data$weight[data$sex=='male'])

#pooled data
ggplot(data, aes(log_wage)) +
  geom_density(adjust=2.5, alpha = 0.3, show.legend=TRUE,  aes(fill=sex, weight=plotweight)) +
  coord_cartesian(xlim=c(0.7, 5.8)) +
  theme_light()+
  scale_fill_discrete(name = "gender", labels = c('female', "male"))+
  theme(legend.position = c(0.83, 0.66),
        legend.text=element_text(size=18),
        legend.title=element_text(size=20),
        legend.background = element_rect(fill=alpha('white', 0.5)),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=19),
        axis.title.y = element_text(size=19))+
  labs(x='log(hourly_wage)')
#ggsave('~/Documents/projects/heterogeneity/paper/observational.png', width=16, height=11, units='cm')

quantile_male = wtd.quantile(x=data$log_wage, weights=data$plotweight*(data$sex=='male'), normwt=TRUE, probs=0.5)
quantile_female = wtd.quantile(x=data$log_wage, weights=data$plotweight*(data$sex=='female'), normwt=TRUE, probs=0.5)
exp(quantile_female)/exp(quantile_male)

################

X = data[,c(
  'age',
  'race',
  'hispanic_origin',
  'citizenship',
  'nativity',
  
  'marital',
  'family_size',
  'children',
  
  'education_level',
  'english_level',
  
  'economic_region'
)]
X$occupation = unlist(lapply(as.character(data$occupation), function(s){return(substr(s, 1, 2))}))
X$occupation = as.factor(X$occupation)
X$industry = unlist(lapply(as.character(data$industry), function(s){return(substr(s, 1, 2))}))
X$industry[X$industry %in% c('32', '33', '3M')] = '31'
X$industry[X$industry %in% c('42')] = '41'
X$industry[X$industry %in% c('45', '4M')] = '44'
X$industry[X$industry %in% c('49')] = '48'
X$industry[X$industry %in% c('92')] = '91'
X$industry = as.factor(X$industry)
X=dummy_cols(X, remove_selected_columns=TRUE)
X = as.matrix(X)

Y = data[,c('sex', 'log_wage')]
Y$sex = (Y$sex == 'male')
Y = as.matrix(Y)

set.seed(22)
train_idx = sample(1:nrow(data), 300000, replace=FALSE)
drf_fit = drf(X=X[train_idx,], Y=Y[train_idx,], min.node.size = 20, splitting.rule='FourierMMD', num.features=10)
#save(drf_fit, train_idx, data, X, Y, file='~/Documents/projects/heterogeneity/wage_data/computed_data')

##################################
#load(file='~/Documents/projects/heterogeneity/wage_data/computed_data')

point_description = function(test_point){
  out = ''
  
  out = paste(out, 'job: ', test_point$occupation_description[1], sep='')
  out = paste(out, '\nindustry: ', test_point$industry_description[1], sep='')
  
  out = paste(out, '\neducation: ', test_point$education[1], sep='')
  out = paste(out, '\nemployer: ', test_point$employer[1], sep='')
  out = paste(out, '\nregion: ', test_point$economic_region[1], sep='')
  
  out = paste(out, '\nmarital: ', test_point$marital[1], sep='')
  out = paste(out, '\nfamily_size: ', test_point$family_size[1], sep='')
  out = paste(out, '\nchildren: ', test_point$children[1], sep='')
  
  out = paste(out, '\nnativity: ', test_point$nativity[1], sep='')
  out = paste(out, '\nhispanic: ', test_point$hispanic_origin[1], sep='')
  out = paste(out, '\nrace: ', test_point$race[1], sep='')
  out = paste(out, '\nage: ', test_point$age[1], sep='')
  
  return(out)
}

plotdf = data[train_idx, ]
set.seed(22)
for(i in sample((1:nrow(X))[-train_idx], 50)){
  test_point = X[i,]
  weights = predict(drf_fit, newdata=matrix(test_point, nrow=1))$weights[1,]
  
  print(i)
  print(data[i,])
  
  propensity = sum(weights[plotdf$sex=='female'])
  plotdf$plotweight = 0
  plotdf$plotweight[plotdf$sex=='female'] = weights[plotdf$sex=='female']/propensity
  plotdf$plotweight[plotdf$sex=='male'] = weights[plotdf$sex=='male']/(1-propensity)
  
  gg = ggplot(plotdf, aes(log_wage)) +
    geom_density(adjust=5, alpha = 0.3, show.legend=TRUE,  aes(fill=sex, weight=plotweight)) +
    coord_cartesian(xlim=c(0.7, 6)) +
    labs(x='log(hourly wage)')+
    #ggtitle(sprintf('%g', i)) +
    #theme(plot.title = element_text(size = 10, face = "italic")) +
    theme_light()+
    scale_fill_discrete(name = "gender", labels = c(sprintf("female: %g%%", round(100*propensity, 1)), sprintf("male: %g%%", round(100*(1-propensity), 1))))+
    theme(legend.position = c(0.83, 0.66),
          legend.text=element_text(size=18),
          legend.title=element_text(size=20),
          legend.background = element_rect(fill=alpha('white', 0.5)),
          axis.text.x = element_text(size=14),
          axis.text.y = element_text(size=14),
          axis.title.x = element_text(size=19),
          axis.title.y = element_text(size=19))+
    annotate("text", x=0.46, y=Inf, hjust=0, vjust=1, size=5, label = point_description(data[i,]))
  plot(gg)

  cat ("Press [enter] to continue")
  line <- readline()
  #ggsave('~/Documents/projects/heterogeneity/paper/wage_data/job1.png', width=16, height=11, units='cm')
}
#292457
#999398 mechanic
#575092 repairmen
#935445 pharmacy technician -> interesting
#165076 veterinarian - high salary
#81835 nurse -> interesting

###########################################################

N=800
plotdf$plotweight = 0
cnt=0
for(i in sample((1:nrow(X))[-train_idx], N)){
  print(cnt)
  cnt = cnt+1
  test_point = X[i,]
  
  weights = predict(drf_fit, newdata=matrix(test_point, nrow=1))$weights[1,]
  
  propensity = sum(weights[plotdf$sex=='female'])
  plotdf$plotweight[plotdf$sex=='female'] = plotdf$plotweight[plotdf$sex=='female'] + weights[plotdf$sex=='female']/propensity/N
  plotdf$plotweight[plotdf$sex=='male'] = plotdf$plotweight[plotdf$sex=='male'] + weights[plotdf$sex=='male']/(1-propensity)/N
}

#save(plotdf, file='~/Documents/projects/heterogeneity/wage_data/computed_data2')
load(file='~/Documents/projects/heterogeneity/wage_data/computed_data2')

plotdf$log_wage = data[train_idx,]$log_wage
#interventional distribution
ggplot(plotdf, aes(log_wage)) +
  geom_density(adjust=2, alpha = 0.3, show.legend=TRUE,  aes(fill=sex, weight=plotweight)) +
  coord_cartesian(xlim=c(0.7, 5.8)) +
  theme_light()+
  scale_fill_discrete(name = "gender", labels = c('female', "male"))+
  theme(legend.position = c(0.83, 0.66),
        legend.text=element_text(size=14),
        legend.title=element_text(size=16),
        legend.background = element_rect(fill=alpha('white', 0.5)),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15))+
  labs(x='log(hourly wage)')
#ggsave('~/Documents/projects/heterogeneity/paper/counterfactual.png', width=16, height=11, units='cm')

####################################
library(Hmisc)
grid = seq(0.001, 0.999, length.out=1000)
quantile_male = wtd.quantile(x=exp(plotdf$log_wage)*40*5*49, weights=plotdf$plotweight*(plotdf$sex=='male'), normwt=TRUE, probs=grid)
quantile_female = wtd.quantile(x=exp(plotdf$log_wage)*40*5*49, weights=plotdf$plotweight*(plotdf$sex=='female'), normwt=TRUE, probs=grid)
idx1 = c(200, 400, 600, 800, 900, 940, 960, 970, 980, 990, 991, 992, 993, 994, 995)
qplot(quantile_male, quantile_female, geom='line', size=I(1)) + 
  geom_abline(color='red', linetype='dashed', size=0.6) +
  annotate(x=quantile_male[idx1]+7500, y=quantile_female[idx1]-12000, "text", label=round(grid[idx1], 2), size=4.6) +
  #annotate(x=quantile_male[idx2], y=quantile_female[idx2]-18000, "text", label=round(grid[idx2], 3)) +
  annotate(x=quantile_male[c(idx1, idx2)], y=quantile_female[c(idx1, idx2)], "point", size=2.5) +
  theme_light()+
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15))+
  coord_cartesian(xlim=c(0, 320000), ylim=c(0, 320000)) +
  scale_x_continuous(breaks=c(0, 100000, 200000, 300000),
                     labels=c("0", "100K", "200K", "300K")) +
  scale_y_continuous(breaks=c(0, 100000, 200000, 300000),
                     labels=c("0", "100K", "200K", "300K")) +
  labs(x='wage quantile men', y='wage quantile women')
ggsave('~/Documents/projects/heterogeneity/paper/wage_quantiles2.png', width=16, height=11, units='cm')

#####################################
N=1000
plotdf$plotweight = 0
cnt=0

which = (1:nrow(X))[-train_idx]
which = which[data$sex[which] == 'female']
for(i in sample(which, N, replace=FALSE)){
  print(cnt)
  cnt = cnt+1
  print(data[i,])
  
  test_point = X[i,]
  weights = predict(mrf_fit, newdata=matrix(test_point, nrow=1))$weights[1,]
  
  propensity = sum(weights[plotdf$sex=='female'])
  plotdf$plotweight[plotdf$sex=='female'] = plotdf$plotweight[plotdf$sex=='female'] + weights[plotdf$sex=='female']/propensity/N
  plotdf$plotweight[plotdf$sex=='male'] = plotdf$plotweight[plotdf$sex=='male'] + weights[plotdf$sex=='male']/(1-propensity)/N
}

#save(plotdf, file='~/Documents/projects/heterogeneity/wage_data/computed_data3')
load(file='~/Documents/projects/heterogeneity/wage_data/computed_data3')

#interventional distribution
ggplot(plotdf, aes(log_wage)) +
  geom_density(adjust=2, alpha = 0.3, show.legend=TRUE,  aes(fill=sex, weight=plotweight)) +
  coord_cartesian(xlim=c(-0.9, 4.4)) +
  theme_light()+
  scale_fill_discrete(name = "", labels = c("observed women's wages", "wages if they were men"))+
  theme(legend.position = c(0.78, 0.95),
        legend.text=element_text(size=13),
        legend.title=element_text(size=16),
        legend.background = element_rect(fill=alpha('white', 0)),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15))+
  labs(x='log(hourly_wage)')
ggsave('~/Documents/projects/heterogeneity/paper/counterfactual2.png', width=16, height=11, units='cm')

quantile_male = wtd.quantile(x=plotdf$log_wage, weights=plotdf$plotweight*(plotdf$sex=='male'), normwt=TRUE, probs=0.5)
quantile_female = wtd.quantile(x=plotdf$log_wage, weights=plotdf$plotweight*(plotdf$sex=='female'), normwt=TRUE, probs=0.5)
#avg_female = sum(data$log_wage[data$sex=='female']*data$plotweight[data$sex=='female'])
#avg_male = sum(data$log_wage[data$sex=='male']*data$plotweight[data$sex=='male'])
exp(quantile_female)/exp(quantile_male)
