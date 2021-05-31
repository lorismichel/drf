# function to run univariate comparison
source("../mtr/helpers.R")

univariateComparison <- function(dataset = "synthetic1",
                                 verbose = TRUE, n = 5000,
                                 p = 10, test.frac = 0.3, 
                                 meanShift = 1, 
                                 sdShift = 1, 
                                 quantiles.grid = c(0.1, 0.9),
                                 conditional.mean.analysis = FALSE,
                                 fit.trf = FALSE) {

  # libs
  if (!require(ranger) || !require(drf) || !require(grf) || !require(trtf)) {
    stop("missing library.")
  }

  # generate the data
  d <- genData(dataset = dataset, meanShift = meanShift, sdShift = sdShift, n = n, p = p)

  if (dataset %in% c("Abalone","Boston", 
                     "BigMac", "Ozone")) {
    n <- nrow(d$X)
  }


  # indices of train and test
  ids.train <- sample(1:n, size = (1-test.frac)*n, replace = FALSE)
  ids.test <- setdiff(1:n, ids.train)

  # competitors

  # simple quantile regression forest
  qRF <- ranger(formula = y~., data = data.frame(y=d$y[ids.train],
                                       x=d$X[ids.train,]),
                num.trees = 500, quantreg = TRUE)

  # distributional random forest
  dRF <- drf(X = d$X[ids.train,], 
             Y = matrix(d$y[ids.train],ncol=1),
             num.trees = 500,
             splitting.rule = "FourierMMD",
             num.features = 10)
  
  if (conditional.mean.analysis) {
    
    qQRF.test <- predict(qRF, data = data.frame(x = d$X[ids.test,]))$predictions
    qDRF.test <- predict(dRF, newdata = d$X[ids.test,], functional = "mean")$mean
    
    return(list(mse.qrf = mean((qQRF.test-d$y[ids.test])^2), mse.drf = mean((qDRF.test-d$y[ids.test])^2)))
  }

  # generalized random forest
  gRF <- quantile_forest(X = d$X[ids.train,],
                         Y = matrix(d$y[ids.train],ncol=1), num.trees = 500)

  # knn
  qKNN <- KNN(X = d$X[ids.train,], Y = matrix(d$y[ids.train],ncol=1))

  
  if (fit.trf) {
    # transformation forest
    nmax <- Inf
  
    if (dataset %in% c("synthetic1", "synthetic2", "synthetic3", "synthetic4", "friedman1", "friedman2", "friedman3")) {
      var_y <- numeric_var("y", support = c(-5, 5))
      B_y <- Bernstein_basis(var_y, order = 2, ui = "increasing")
      m_y <- ctm(B_y)
      trf <- traforest(m_y, formula = y ~ ., data = data.frame(y = d$y[ids.train], x = d$X[ids.train,]),
                       ntree = 500,
                       control = ctree_control(mincriterion = 0,
                                               minsplit = 25, minbucket = 10),
                       mtry = sqrt(p + 1), trace = TRUE)
    } else if (dataset == "Abalone") {
      data("abalone", package = "AppliedPredictiveModeling")
      response <- "Rings"
      abalone[[response]] <- as.numeric(abalone[[response]])
      ns <- 100
      fm <- Rings ~ Type + LongestShell + Diameter + Height + WholeWeight + ShuckedWeight +
        VisceraWeight + ShellWeight
      mtry <- ceiling(length(all.vars(fm[[3]])) / 3)
      var_m <- numeric_var("Rings", support = quantile(abalone[[response]], prob = c(.1, .9)),
                           add = range(abalone[[response]]) - quantile(abalone[[response]], prob = c(.1, .9)),
                           bounds = c(0, Inf))
  
      B_m <- Bernstein_basis(var_m, order = 4, ui = "increasing")
      uc_ctm_AB <- ctm(B_m, data = abalone, todistr = "Normal")
      uc_mlt_AB <- mlt(uc_ctm_AB, data = abalone, scale = FALSE)
  
      c_ctm_AB <- ctm(B_m, data = abalone, todistr = "Normal", shift = fm[c(1, 3)])
      c_mlt_AB <- mlt(c_ctm_AB, data = abalone, scale = TRUE, maxit = 2500)
  
      trf <- traforest(uc_ctm_AB, formula = fm, data = abalone[ids.train,], ntree = 500, trace = TRUE,
                         mltargs = list(maxit = 10000, scale = TRUE, gtol = 1e-3, trace = FALSE),
                         control = ctree_control(mincriterion = 0, minsplit = ns*2, minbucket = ns, nmax = nmax),
                         mtry = mtry)
  
    } else if (dataset == "Boston") {
      data("BostonHousing2", package = "mlbench")
      response <- "cmedv"
      BostonHousing2[[response]] <- as.numeric(BostonHousing2[[response]])
      ns <- 40
      fm <- cmedv ~ crim + zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio + b + lstat
      mtry <- ceiling(length(all.vars(fm[[3]])) / 3)
      var_m <- numeric_var("cmedv", support = quantile(BostonHousing2[[response]], prob = c(.1, .9)),
                           add = range(BostonHousing2[[response]]) - quantile(BostonHousing2[[response]], prob = c(.1, .9)),
                           bounds = c(0, Inf))
      B_m <- Bernstein_basis(var_m, order = 4, ui = "increasing")
      uc_ctm_BH <- ctm(B_m, data = BostonHousing2, todistr = "Normal")
      uc_mlt_BH <- mlt(uc_ctm_BH, data = BostonHousing2, scale = FALSE, trace = FALSE)
  
      c_ctm_BH <- ctm(B_m, data = BostonHousing2, todistr = "Normal", shift = fm[c(1, 3)])
      c_mlt_BH <- mlt(c_ctm_BH, data = BostonHousing2, scale = TRUE, trace = FALSE)
  
      trf <- traforest(uc_ctm_BH, formula = fm, data = BostonHousing2[ids.train,], ntree = 500,
                         control = ctree_control(mincriterion = 0, minsplit = 2*ns, minbucket = ns,
                                                 nmax = nmax), mtry = mtry,
                         trace = TRUE, mltargs = list(maxit = 10000, scale = TRUE, gtol = 1e-3, trace = FALSE))
    } else if (dataset == "BigMac") {
      data("BigMac2003", package = "alr3")
      response <- "BigMac"
      BigMac2003[[response]] <- as.numeric(BigMac2003[[response]])
      ns <- 20
      fm <- BigMac ~ Bread + Rice + FoodIndex + Bus + Apt + TeachGI +
        TeachNI + TaxRate + TeachHours
      mtry <- ceiling(length(all.vars(fm[[3]])) / 3)
      var_m <- numeric_var("BigMac", support = quantile(BigMac2003[[response]], prob = c(.1, .9)),
                           add = range(BigMac2003[[response]]) - quantile(BigMac2003[[response]], prob = c(.1, .9)),
                           bounds = c(0, Inf))
  
      B_m <- Bernstein_basis(var_m, order = 4, ui = "increasing")
      uc_ctm_BM <- ctm(B_m, data = BigMac2003, todistr = "Normal")
      uc_mlt_BM <- mlt(uc_ctm_BM, data = BigMac2003, scale = FALSE, trace = FALSE)
  
      c_ctm_BM <- ctm(B_m, data = BigMac2003, todistr = "Normal", shift = fm[c(1, 3)])
      c_mlt_BM <- mlt(c_ctm_BM, data = BigMac2003, scale = TRUE, trace = FALSE)
  
      trf <- traforest(uc_ctm_BM, formula = fm, data = BigMac2003[ids.train,], ntree = 500,
                         control = ctree_control(mincriterion = 0, minsplit = 2*ns, minbucket = ns,
                                                 nmax = nmax), mltargs = list(maxit = 10000, scale = TRUE, gtol = 1e-3, trace = FALSE),
                         trace = TRUE, mtry = mtry)
    } else if (dataset == "Ozone") {
      data("Ozone", package = "mlbench")
      Ozone <- subset(Ozone, complete.cases(Ozone))
      Ozone <- as.data.frame(lapply(Ozone, function(x) {
        x <- x[, drop = TRUE]
        if (is.factor(x)) return(as.ordered(x))
        x
      }))
      response <- "V4"
      Ozone[[response]] <- as.numeric(Ozone[[response]])
  
      ns <- 20
      fm <- V4 ~ V1 + V2 + V3 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13
      mtry <- ceiling(length(all.vars(fm[[3]])) / 3)
      var_m <- numeric_var("V4", support = quantile(Ozone[[response]], prob = c(.1, .9)),
                           add = range(Ozone[[response]]) - quantile(Ozone[[response]], prob = c(.1, .9)),
                           bounds = c(0, Inf))
  
      B_m <- Bernstein_basis(var_m, order = 4, ui = "increasing")
      uc_ctm_Ozone <- ctm(B_m, data = Ozone, todistr = "Normal")
      uc_mlt_Ozone <- mlt(uc_ctm_Ozone, data = Ozone, scale = FALSE)
  
      c_ctm_Ozone <- ctm(B_m, data = Ozone, todistr = "Normal", shift = fm[c(1, 3)])
      c_mlt_Ozone <- mlt(c_ctm_Ozone, data = Ozone, scale = TRUE, maxit = 10000)
      trf <- traforest(uc_ctm_Ozone, formula = fm, data = Ozone[ids.train,], ntree = 500,
                            control = ctree_control(mincriterion = 0, minsplit = 2*ns,
                                                    minbucket = ns, nmax = nmax), trace = TRUE, mtry = mtry,
                            mltargs = list(maxit = 10000, scale = TRUE, gtol = 1e-3, trace = FALSE))
  
    }
    
  }
  
    if (dataset %in% c("synthetic1","synthetic2","synthetic3","synthetic4")) {
      
      # get the grid predictions
      x <- c(- 50:1 / 51, 1:50 / 51)
      X <- matrix(0, nrow = length(x), ncol = ncol(d$X)-1)
      nd <- data.frame(x = cbind(x, X))
      qQRF <- predict(qRF, data = nd, quantiles = quantiles.grid, type = "quantiles")
      qDRF <- predict(dRF, newdata = cbind(x, X), functional = "quantile", quantiles = quantiles.grid)$quantile
      ord.quant <- order(quantiles.grid, decreasing = FALSE)
      qGRF <- predict(gRF, newdata = cbind(x, X), quantiles = quantiles.grid[ord.quant])
      qGRF <- qGRF[,ord.quant]
      #qTRF <- list(p1 = predict(trf, newdata = nd, type = "quantile", prob = .1),
      #             p2 = predict(trf, newdata = nd, type = "quantile", prob = .3),
      #             p3 = predict(trf, newdata = nd, type = "quantile", prob = .4),
      #             p4 = predict(trf, newdata = nd, type = "quantile", prob = .7),
      #             p5 = predict(trf, newdata = nd, type = "quantile", prob = .9))
      
      if (fit.trf) {
        qTRF <- lapply(quantiles.grid, function(q) predict(trf, newdata = nd, type = "quantile", prob = q))
      }
      
      if (dataset == "synthetic1") {
        q1 <- qnorm(.1, mean = meanShift*(x >  0))
        q3 <- qnorm(.3, mean = meanShift*(x >  0))
        q5 <- qnorm(.5, mean = meanShift*(x >  0))
        q7 <- qnorm(.7, mean = meanShift*(x >  0))
        q9 <- qnorm(.9, mean = meanShift*(x >  0))
        #q1.train <- qnorm(.1, mean = meanShift*(d$X[ids.train,1] > 0))
        #q3.train <- qnorm(.3, mean = meanShift*(d$X[ids.train,1] > 0))
        #q5.train <- qnorm(.5, mean = meanShift*(d$X[ids.train,1] > 0))
        #q7.train <- qnorm(.7, mean = meanShift*(d$X[ids.train,1] > 0))
        #q9.train <- qnorm(.9, mean = meanShift*(d$X[ids.train,1] > 0))
        q.test <- lapply(quantiles.grid, function(q) qnorm(q, mean = meanShift*(d$X[ids.test,1] > 0)))

        
      } else if (dataset == "synthetic2") {
        q1 <- qnorm(.1, sd = 1 + sdShift*(x > 0))
        q3 <- qnorm(.3, sd = 1 + sdShift*(x > 0))
        q5 <- qnorm(.5, sd = 1 + sdShift*(x > 0))
        q7 <- qnorm(.7, sd = 1 + sdShift*(x > 0))
        q9 <- qnorm(.9, sd = 1 + sdShift*(x > 0))
        #q1.train <- qnorm(.1, sd = 1 + sdShift*(d$X[ids.train,1] > 0))
        #q3.train <- qnorm(.3, sd = 1 + sdShift*(d$X[ids.train,1] > 0))
        #q5.train <- qnorm(.5, sd = 1 + sdShift*(d$X[ids.train,1] > 0))
        #q7.train <- qnorm(.7, sd = 1 + sdShift*(d$X[ids.train,1] > 0))
        #q9.train <- qnorm(.9, sd = 1 + sdShift*(d$X[ids.train,1] > 0))
        q.test <- lapply(quantiles.grid, function(q) qnorm(q, sd = 1 + sdShift*(d$X[ids.test,1] > 0)))

      } else if (dataset == "synthetic3") {
        q1 <- ifelse(x >= 0, qexp(.1, 1), qnorm(.1))
        q3 <- ifelse(x >= 0, qexp(.3, 1), qnorm(.3))
        q5 <- ifelse(x >= 0, qexp(.5, 1), qnorm(.5))
        q7 <- ifelse(x >= 0, qexp(.7, 1), qnorm(.7))
        q9 <- ifelse(x >= 0, qexp(.9, 1), qnorm(.9))
        #q1.train <- ifelse(d$X[ids.train,1] >= 0, qexp(.1, 1), qnorm(.1))
        #q3.train <- ifelse(d$X[ids.train,1] >= 0, qexp(.3, 1), qnorm(.3))
        #q5.train <- ifelse(d$X[ids.train,1] >= 0, qexp(.5, 1), qnorm(.5))
        #q7.train <- ifelse(d$X[ids.train,1] >= 0, qexp(.7, 1), qnorm(.7))
        #q9.train <- ifelse(d$X[ids.train,1] >= 0, qexp(.9, 1), qnorm(.9))
        q.test <- lapply(quantiles.grid, function(q) ifelse(d$X[ids.test,1] >= 0, qexp(q, 1), qnorm(q, 1, 1)))

        
      } else if (dataset == "synthetic4") {
        q1 <- sin(4*pi*x) + ifelse(x >= .5, qnorm(.1), qnorm(.1, sd = 2))
        q9 <- sin(4*pi*x) + ifelse(x >= .5, qnorm(.9), qnorm(.9, sd = 2))
        q1.train <- sin(4*pi*d$X[ids.train,1]) + ifelse(d$X[ids.train,1] >= .5, qnorm(.1), qnorm(.1, sd = 2))
        q9.train <- sin(4*pi*d$X[ids.train,1]) + ifelse(d$X[ids.train,1] >= .5, qnorm(.9), qnorm(.9, sd = 2))
        q.test <- lapply(quantiles.grid, function(q) sin(4*pi*d$X[ids.test,1]) + ifelse(d$X[ids.test,1] >= 0, qnorm(q), qnorm(q, sd = 2)))

      }

    if (verbose) {
      #png(filename = paste0("./plot_scatter_", dataset,".png"), width = 2000, height = 2000)
      lwd <- 1.5
      col <- rgb(.1, .1, .1, .3)
      colR <- rgb(.75, 0, 0, .8)
      colRight <- rgb(.75, 0, 0, .1)
      colB <- rgb(0, 0, 0.75, .8)
      plot(d$X[ids.train,1], d$y[ids.train], xlab=expression(X[1]), ylab=expression(Y[1]), pch = 19, col = col, cex = 1)
      lines(x, q1, lty = 2, lwd = lwd * 1.5, col = "black", type = "S")
      #lines(x, q3, lty = 2, lwd = lwd * 1.5, col = "black", type = "S")
      lines(x, q5, lty = 2, lwd = lwd * 1.5, col = "black", type = "S")
      #lines(x, q7, lty = 2, lwd = lwd * 1.5, col = "black", type = "S")
      lines(x, q9, lty = 2, lwd = lwd * 1.5, col = "black", type = "S")
      lines(x, qQRF$predictions[,1], lty = 1, lwd = lwd * 1.5, col = "darkgreen", type = "S")
      #lines(x, qQRF$predictions[,2], lty = 1, lwd = lwd * 1.5, col = "darkgreen", type = "S")
      lines(x, qQRF$predictions[,3], lty = 1, lwd = lwd * 1.5, col = "darkgreen", type = "S")
      #lines(x, qQRF$predictions[,4], lty = 1, lwd = lwd * 1.5, col = "darkgreen", type = "S")
      lines(x, qQRF$predictions[,5], lty = 1, lwd = lwd * 1.5, col = "darkgreen", type = "S")
      lines(x, qDRF[,1,1],lty = 1, lwd = lwd * 1.5, col = "blue", type = "S")
      #lines(x, qDRF[,1,2],lty = 1, lwd = lwd * 1.5, col = "blue", type = "S")
      lines(x, qDRF[,1,3],lty = 1, lwd = lwd * 1.5, col = "blue", type = "S")
      #lines(x, qDRF[,1,4],lty = 1, lwd = lwd * 1.5, col = "blue", type = "S")
      lines(x, qDRF[,1,5],lty = 1, lwd = lwd * 1.5, col = "blue", type = "S")
      lines(x, qGRF[,1],lty = 1, lwd = lwd * 1.5, col = "brown", type = "S")
      #lines(x, qGRF[,2],lty = 1, lwd = lwd * 1.5, col = "brown", type = "S")
      lines(x, qGRF[,3],lty = 1, lwd = lwd * 1.5, col = "brown", type = "S")
      #lines(x, qGRF[,4],lty = 1, lwd = lwd * 1.5, col = "brown", type = "S")
      lines(x, qGRF[,5],lty = 1, lwd = lwd * 1.5, col = "brown", type = "S")
      #lines(xn, qTRT[,1], lty = 2, lwd = lwd * 1.5, col = colB, type = "S")
      #lines(xn, qTRT[,2], lty = 2, lwd = lwd * 1.5, col = colB, type = "S")
      if (fit.trf) {
        lines(x, unlist(qTRF[[1]]), lty = 1, lwd = lwd * 1.5, col = "pink", type = "S")
        #lines(x, unlist(qTRF[[2]]), lty = 1, lwd = lwd * 1.5, col = "pink", type = "S")
        lines(x, unlist(qTRF[[3]]), lty = 1, lwd = lwd * 1.5, col = "pink", type = "S")
        #lines(x, unlist(qTRF[[4]]), lty = 1, lwd = lwd * 1.5, col = "pink", type = "S")
        lines(x, unlist(qTRF[[5]]), lty = 1, lwd = lwd * 1.5, col = "pink", type = "S")
      }
      #dev.off()
      #legend("topleft", lty = c(1, 1, 1, 1), lwd = c(lwd * 1.5, lwd * 1.5, lwd * 1.5, lwd * 1.5),
      #       col = c("black", colB, "darkgreen", "brown"),
      #       legend = c("Quantile Regression Forest", "Transformation Forest",
      #                  "Distributional Random Forest", "Generalized Random Forest"),
      #       bty = "n")
    }
  }

  # checking the quantile loss out of sample
  qLoss <- function(y, q, alpha) alpha*pmax(0, y-q) + (1-alpha)*pmax(0, q-y)

  qQRF.test <- predict(qRF, data = data.frame(x = d$X[ids.test,]), quantiles = quantiles.grid, type = "quantiles")
  qDRF.test <- predict(dRF, newdata = d$X[ids.test,], functional = "quantile", quantiles = quantiles.grid)$quantile
  qGRF.test <- predict(gRF, newdata = d$X[ids.test,], quantiles = quantiles.grid)
  qKNN_5.test <- predictKNN(qKNN,
                            d$X[ids.test,],
                            k = 5,
                            type = "functional",
                            f = function(y) y[1],
                            quantiles = quantiles.grid)
  qKNN_20.test <- predictKNN(qKNN,
                            d$X[ids.test,],
                            k = 20,
                            type = "functional",
                            f = function(y) y[1],
                            quantiles = quantiles.grid)
  qKNN_40.test <- predictKNN(qKNN,
                            d$X[ids.test,],
                            k = 40,
                            type = "functional",
                            f = function(y) y[1],
                            quantiles = quantiles.grid)
  
  if (fit.trf) {
    if (dataset %in% c("synthetic1","synthetic2","synthetic3","synthetic4")) {
      qTRF.test <- lapply(quantiles.grid, function(q) predict(trf, newdata = data.frame(x = d$X[ids.test,]), type = "quantile", prob = q))
    } else if (dataset == "Abalone") {
      qTRF.test <- lapply(quantiles.grid, function(q) predict(trf, newdata = abalone[ids.test,], type = "quantile", prob = q))
    } else if (dataset == "Boston") {
      qTRF.test <- lapply(quantiles.grid, function(q) predict(trf, newdata = BostonHousing2[ids.test,], type = "quantile", prob = q))
    } else if (dataset == "BigMac") {
      qTRF.test <- lapply(quantiles.grid, function(q) predict(trf, newdata = BigMac2003[ids.test,], type = "quantile", prob = q))
    } else if (dataset == "Ozone") {
      qTRF.test <- lapply(quantiles.grid, function(q) predict(trf, newdata = Ozone[ids.test,], type = "quantile", prob = q))
    }
  }
  

  losses <- lapply(1:length(quantiles.grid), function(i) {
    if (dataset %in% c("synthetic1","synthetic2","synthetic3","synthetic")) {
      q.loss <- c(mean(qLoss(y = d$y[ids.test], q = q.test[[i]], alpha = quantiles.grid[i])),
                  mean(qLoss(y = d$y[ids.test], q = qQRF.test$predictions[,i], alpha = quantiles.grid[i])),
                  mean(qLoss(y = d$y[ids.test], q = qDRF.test[,1,i], alpha = quantiles.grid[i])),
                  mean(qLoss(y = d$y[ids.test], q = qGRF.test[,i], alpha = quantiles.grid[i])),
                  if (fit.trf) mean(qLoss(y = d$y[ids.test], q = unlist(qTRF.test[[i]]), alpha = quantiles.grid[i])) else NA,
                  mean(qLoss(y = d$y[ids.test], q = qKNN_5.test$functional[,i], alpha = quantiles.grid[i])),
                  mean(qLoss(y = d$y[ids.test], q = qKNN_20.test$functional[,i], alpha = quantiles.grid[i])),
                  mean(qLoss(y = d$y[ids.test], q = qKNN_40.test$functional[,i], alpha = quantiles.grid[i])))
      names(q.loss) <- c("truth", "qrf", "drf", "grf", "trf", "knn5", "knn20", "knn40")
    } else {
      q.loss <- c(mean(qLoss(y = d$y[ids.test], q = qQRF.test$predictions[,i], alpha = quantiles.grid[i])),
                  mean(qLoss(y = d$y[ids.test], q = qDRF.test[,1,i], alpha = quantiles.grid[i])),
                  mean(qLoss(y = d$y[ids.test], q = qGRF.test[,i], alpha = quantiles.grid[i])),
                  if (fit.trf) mean(qLoss(y = d$y[ids.test], q = unlist(qTRF.test[[i]]), alpha = quantiles.grid[i])) else NA)
      names(q.loss) <- c("qrf", "drf", "grf", "trf")
    }

    return(q.loss)
  })

  names(losses) <- quantiles.grid

  #qTRF.test <- sapply(qTRF.test, unlist)
  
  #df <- data.frame()
  #methods <- c("qrf","drf","grf")
  #if (fit.trf) methods <- c(methods, "trf") 
  #for (method in methods) {
  #  q <- if (method == "qrf") qQRF.test$predictions else if (method == "grf") qGRF.test else if (method == "drf") qDRF.test[,1,] else if (method == "trf") qTRF.test
  #  colnames(q) <- NULL
  #  df <- rbind(df, data.frame(active = d$X[ids.test,1], method = method, q = q))
  #}
  
  return(list(df, 
              d = d, 
              ids.train = ids.train,
              ids.test = ids.test,
              q.losses = losses, 
              qQRF = qQRF,
              qDRF = qDRF,
              qGRF = qGRF, 
              qTRF = if (fit.trf) qTRF else NA))
  
  #save(df, file = "~/Downloads/univariateQuantiles3.Rdata")
  # x_test (active) # quantile prediction, # mean, # method, # scenario 
  
}

