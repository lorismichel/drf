# helpers for mtr

# libs 
require(foreign)


# atp1d and atp2d
# The Airline Ticket Price dataset concerns the prediction of airline ticket prices. The rows
# are a sequence of time-ordered observations over several days. Each sample in this dataset
# represents a set of observations from a specific observation date and departure date pair. The
# input variables for each sample are values that may be useful for prediction of the airline ticket
# prices for a specific departure date. The target variables in these datasets are the next day
# (ATP1D) price or minimum price observed over the next 7 days (ATP7D) for 6 target flight
# preferences: (1) any airline with any number of stops, (2) any airline non-stop only, (3) Delta
# Airlines, (4) Continental Airlines, (5) Airtrain Airlines, and (6) United Airlines. The input
# variables include the following types: the number of days between the observation date and
# the departure date (1 feature), the boolean variables for day-of-the-week of the observation
# date (7 features), the complete enumeration of the following 4 values: (1) the minimum price,
# mean price, and number of quotes from (2) all airlines and from each airline quoting more
# than 50% of the observation days (3) for non-stop, one-stop, and two-stop flights, (4) for the
# current day, previous day, and two days previous. The result is a feature set of 411 variables.
# For specific details on how these datasets are constructed please consult Groves and Gini
# (2015). The nature of these datasets is heterogeneous with a mixture of several types of
# variables including boolean variables, prices, and counts.


# edm
# The Electrical Discharge Machining dataset (Karalic and Bratko 1997) represents a two-target
# regression problem. The task is to shorten the machining time by reproducing the behaviour
# of a human operator that controls the values of two variables. Each of the target variables
# takes 3 distinct numeric values ({−1, 0, 1}) and there are 16 continuous input variables.

# sf1 and sf2
# The Solar Flare dataset (Lichman 2013) has 3 target variables that correspond to the number
# of times 3 types of solar flare (common, moderate, severe) are observed within 24 h. There
# are two versions of this dataset. SF1 contains data from year 1969 and SF2 from year 1978.

# jura
# The Jura (Goovaerts 1997) dataset consists of measurements of concentrations of seven heavy
# metals (cadmium, cobalt, chromium, copper, nickel, lead, and zinc), recorded at 359 locations
# 123
# Mach Learn (2016) 104:55–98 83
# in the topsoil of a region of the Swiss Jura. The type of land use (Forest, Pasture, Meadow,
# Tillage) and rock type (Argovian, Kimmeridgian, Sequanian, Portlandian, Quaternary) were
# also recorded for each location. In a typical scenario (Goovaerts 1997; Álvarez and Lawrence
# 2011), we are interested in the prediction of the concentration of metals that are more expensive to measure 
# (primary variables) using measurements of metals that are cheaper to sample
# (secondary variables). In this study, cadmium, copper and lead are treated as target variables
# while the remaining metals along with land use type, rock type and the coordinates of each
# location are used as predictive features


# wq
# The Water Quality dataset (Dzeroski et al. 2000) has 14 target attributes that refer to the
# relative representation of plant and animal species in Slovenian rivers and 16 input attributes
# that refer to physical and chemical water quality parameters.

# enb
# The Energy Building dataset (Tsanas and Xifara 2012) concerns the prediction of the heating
# load and cooling load requirements of buildings (i.e. energy efficiency) as a function of eight
# building parameters such as glazing area, roof area, and overall height, amongst others.


# slumb
# The Concrete Slump dataset (Yeh 2007) concerns the prediction of three properties of concrete (slump, flow and compressive strength) as a function of the content of seven concrete
# ingredients: cement,fly ash, blast furnace slag, water, superplasticizer, coarse aggregate, and
# fine aggregate.

# andro
# The Andromeda dataset (Hatzikos et al. 2008) concerns the prediction of future values for
# six water quality variables (temperature, pH, conductivity, salinity, oxygen, turbidity) in
# Thermaikos Gulf of Thessaloniki, Greece. Measurements of the target variables are taken
# from under-water sensors with a sampling interval of 9 seconds and then averaged to get a
# single measurement for each variable over each day. The specific dataset that we use here
# corresponds to using a window of 5 days (i.e. features attributes correspond to the values of
# the six water quality variables up to 5 days in the past) and a lead of 5 days (i.e. we predict the values of each variable 6 days ahead)


# edm
# The Electrical Discharge Machining dataset (Karalic and Bratko 1997) represents a two-target
# regression problem. The task is to shorten the machining time by reproducing the behaviour
# of a human operator that controls the values of two variables. Each of the target variables
# takes 3 distinct numeric values ({−1, 0, 1}) and there are 16 continuous input variables.


# wq
# The Water Quality dataset (Dzeroski et al. 2000) has 14 target attributes that refer to the
# relative representation of plant and animal species in Slovenian rivers and 16 input attributes
# that refer to physical and chemical water quality parameters.

loadMTRdata <- function(dataset.name = "atp1d", path = '~/Documents/projects/heterogeneity/mtr-datasets/') {
  dataset <- read.arff(file = paste0(path, dataset.name, ".arff"))
  if (dataset.name == "enb") {
    names.dataset <- c("Relative Compactness",
                       "Surface Area",
                       "Wall Area",
                       "Roof Area",
                       "Overall Height",
                       "Orientation",
                       "Glazing Area",
                       "Glazing Area Distribution",
                       "Heating Load",
                       "Cooling Load")
    
    names(dataset) <- names.dataset
    
    return(list(X = as.matrix(dataset[,-c(9:10)]), Y = as.matrix(dataset[,c(9:10)])))
  } else if (dataset.name == "scpf") {
      dataset <- na.omit(dataset)
      # could we do it better?
      return(list(X = as.matrix(dataset[,c("daysUntilLastIssue", "latitude", "longitude", "distanceFromCenter", "city=Oakland",
                                           "city=Chicago", "city=NH", "city=Richmond")]), Y = as.matrix(dataset[,c(24:26)])))
  } else if (dataset.name == "osales") {
    # could we do it better?
    nb.NA <- apply(dataset,2,function(x) sum(is.na(x)))
    dataset <- dataset[,nb.NA == 0]
    return(list(X = as.matrix(dataset[,c(names(dataset)[!grepl(names(dataset), pattern = "Outcome")])]), Y = as.matrix(dataset[,names(dataset)[grepl(names(dataset), pattern = "Outcome")]])))
  } else if (dataset.name == "RF1") {
    # could we do it better?
    dataset <- na.omit(dataset)
    return(list(X = as.matrix(dataset[,c(names(dataset)[!grepl(names(dataset), pattern = "48H")])]), Y = as.matrix(dataset[,names(dataset)[grepl(names(dataset), pattern = "48H")]])))
  } else if (dataset.name == "RF2") {
    # could we do it better?
    dataset <- na.omit(dataset)
    return(list(X = as.matrix(dataset[,c(names(dataset)[!grepl(names(dataset), pattern = "48H")])]), Y = as.matrix(dataset[,names(dataset)[grepl(names(dataset), pattern = "48H")]])))
  } else if (dataset.name == "slump") {
    return(list(X = as.matrix(dataset[,1:7]), Y = as.matrix(dataset[,-c(1:7)])))
  } else if (dataset.name == "andro") {
    return(list(X = as.matrix(dataset[,-c(31:36)]), Y = as.matrix(dataset[,c(31:36)])))
  } else if (dataset.name == "edm") {
    return(list(X = as.matrix(dataset[,-c(17:18)]), Y = as.matrix(dataset[,c(17:18)])))
  } else if (dataset.name == "wq") {
    return(list(X = as.matrix(dataset[,-c(17:30)]), Y = as.matrix(dataset[,c(17:30)])))
  } else if (dataset.name == "atp1d") {
    #return(list(X = as.matrix(dataset[,-c(17:30)]), Y = as.matrix(dataset[,c(17:30)])))
  }
  else if (dataset.name == "atp7d") {
    #return(list(X = as.matrix(dataset[,-c(17:30)]), Y = as.matrix(dataset[,c(17:30)])))
  } else if (dataset.name == "jura") {
    return(list(X = as.matrix(dataset[,-c(16:18)]), Y = as.matrix(dataset[,c(16:18)])))
  }
}

kFoldCV <- function(n, k = 10) {
  ids <- sample(cut(1:n, breaks = k, labels = FALSE))
  return(lapply(1:k, function(i) which(ids == i)))
}

RMSE_t <- function(Y, Yhat) {
  return(sqrt(apply((Y-Yhat) * (Y-Yhat),2,mean)))
}

qLoss <- function(y,yhat,alpha) {
  return(mean(alpha*pmax(y-yhat,0)+(1-alpha)*pmax(yhat-y,0)))
}

generateRandomDirection <- function(dim = 2, nb = 1) {
  w <- list()
  for (i in 1:nb) {
    x <- rnorm(n = dim, mean = 0, sd = 1)
    w[[i]] <- x/sqrt(sum(x^2))
  }
  return(w)
}

runRandomPinballAnalysis <- function(X, 
                                     Y, 
                                     k = 10, 
                                     alpha_seq = c(.005, .025, .05, .3, .5, .7, .95, .975, .995), 
                                     nb_random_directions = 10,
                                     seed = 0,
                                     ...) {
  
  Y = scale(Y)
  
  # repro
  set.seed(seed)
  
  # create folds
  folds <- kFoldCV(n = nrow(X), k = k)
  
  # properties of the simulations
  w <- generateRandomDirection(dim = ncol(Y), nb = nb_random_directions)
  mrf_loss <- matrix(0,nrow=nb_random_directions, ncol=length(alpha_seq))
  gini_loss <- matrix(0,nrow=nb_random_directions, ncol=length(alpha_seq))
  
  # CV loop
  for (kk in 1:k) {
    
    mRF <- mrf(X = X[-folds[[kk]],], Y = Y[-folds[[kk]],], splitting.rule = "fourier", ...)
    giniRF <- mrf(X = X[-folds[[kk]],], Y = Y[-folds[[kk]],], splitting.rule = "gini")
    
    for (i in 1:length(w)) {
      
      yhat_mrf <- predict(mRF, newdata = X[folds[[kk]],], type = "functional", 
                          quantiles = alpha_seq, f = function(y) sum(w[[i]]*y))$functional
      yhat_gini <- predict(giniRF, newdata = X[folds[[kk]],], type = "functional", 
                           quantiles = alpha_seq, f = function(y) sum(w[[i]]*y))$functional
      
      for (j in 1:length(alpha_seq)) {
        mrf_loss[i,j] <- mrf_loss[i,j] +  qLoss(y = Y[folds[[kk]],] %*% w[[i]],
                                                yhat = yhat_mrf[,j], alpha = alpha_seq[j])/k
        gini_loss[i,j] <- gini_loss[i,j] + qLoss(y = Y[folds[[kk]],] %*% w[[i]], 
                                                 yhat = yhat_gini[,j], alpha = alpha_seq[j])/k
      }
    }
  }
  
  return(list(mrf_loss = mrf_loss, gini_loss = gini_loss))
}