library(mrf)
library(ggplot2)
library(fastDummies)

load("~/Documents/projects/heterogeneity/wage_data/wage_data")

which = rep(TRUE, nrow(wage))
which = which & (wage$race %in% c('white', 'black', 'asian'))
#which = which & (wage$hispanic_origin == 'no')
#which = which & (wage$nativity == 'native')
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

data$log_wage = log(data$salary/(data$weeks_worked * data$hours_worked)) 

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
  
  'employer',
  'economic_region'
)]
X$occupation = unlist(lapply(as.character(data$occupation), function(s){return(substr(s, 1, 2))}))
X$occupation = as.factor(X$occupation)
X$employer = as.factor(X$employer)
X$race = as.factor(X$race)
X$industry = unlist(lapply(as.character(data$industry), function(s){return(substr(s, 1, 2))}))
X$industry[X$industry %in% c('32', '33', '3M')] = '31'
X$industry[X$industry %in% c('42')] = '41'
X$industry[X$industry %in% c('45', '4M')] = '44'
X$industry[X$industry %in% c('49')] = '48'
X$industry[X$industry %in% c('92')] = '91'
X$industry = as.factor(X$industry)
X=dummy_cols(X, remove_selected_columns=TRUE)
X = as.matrix(X)

Y = data[,c('log_wage'), drop=FALSE]
Y$male = (data$sex == 'male')
Y = as.matrix(Y)

set.seed(22)
subsample = sample(1:nrow(X), 20000, replace=FALSE)
X = X[subsample,]
Y = Y[subsample,]

save(X, Y, file='~/Documents/projects/heterogeneity/wage_data/wage_benchmark.Rdata')

