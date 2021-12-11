library(ggplot2)
library(mrf)
library(GGally)
library(fastDummies)

load("../../data/birth_data/computed_data/births.Rdata")
births = na.omit(births)
births = births[births$race_mother %in% c('white', 'black', 'asian'), ]
births = births[births$race_father %in% c('white', 'black', 'asian'), ]

X = births[,c(
  'race_mother',
  'age_mother',
  'education_mother',
  'marital_status_mother',
  'height_mother',
  'BMI_mother',
  'weight_gain_mother',
  'race_father',
  'age_father',
  'education_father',
  'plurality',
  'birth_order',
  'delivery_method',
  'gender',
  'cigarettes_during_pregnancy'
)]
X$precare = births$precare!=Inf
X$race_father = factor(X$race_father)
X$race_mother = factor(X$race_mother)
X=dummy_cols(X, remove_selected_columns=TRUE)
X = as.matrix(X)

Y = births[, c('pregnancy_duration', 'birthweight')]
Y = as.matrix(Y)

set.seed(22)
subsample = sample(1:nrow(X), 30000, replace=FALSE)
X = X[subsample, ]
Y = Y[subsample, ]
save(X, Y, file='../../data/birth_data/computed_data/births_benchmark1.Rdata')


X = births[,c(
  'race_mother',
  'age_mother',
  'education_mother',
  'marital_status_mother',
  'height_mother',
  'BMI_mother',
  'weight_gain_mother',
  'race_father',
  'age_father',
  'education_father',
  'plurality',
  'birth_order',
  'delivery_method',
  'gender',
  'cigarettes_during_pregnancy',
  'pregnancy_duration'
)]
X$precare = births$precare!=Inf
X$race_father = factor(X$race_father)
X$race_mother = factor(X$race_mother)
X=dummy_cols(X, remove_selected_columns=TRUE)
X = as.matrix(X)

Y = births[, c('birthweight', 'apgar_5min')]
Y$abnormal_conditions = (births$abnormal_conditions == 'yes')
Y$congenital_anomalies = (births$congenital_anomalies == 'yes')
Y = as.matrix(Y)

set.seed(22)
subsample = sample(1:nrow(X), 30000, replace=FALSE)
X = X[subsample, ]
Y = Y[subsample, ]
save(X, Y, file='../../data/birth_data/computed_data/births_benchmark2.Rdata')
