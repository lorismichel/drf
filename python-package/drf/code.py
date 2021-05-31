import pandas as pd
import numpy as np

import rpy2.robjects as ro
import rpy2.robjects.numpy2ri
rpy2.robjects.numpy2ri.activate()
from rpy2.robjects import pandas2ri
pandas2ri.activate()

from rpy2.robjects.packages import importr
base_r_package = importr('base')
drf_r_package = importr('drf')

def convert_to_df(X):
    if type(X) == np.ndarray:
        if len(X.shape) == 1:
            X = X.reshape(1, X.shape[0])
        return pd.DataFrame(X)

    if type(X) == list or type(X) == tuple:
        return pd.DataFrame([X])

    return pd.DataFrame(X)

def w_cov(x, y, w):
    mx = np.average(x, weights=w)
    my = np.average(y, weights=w)
    return np.average((x - mx) * (y - my), weights=w)

def w_quantile(values,
                quantiles,
                sample_weight=None,
                values_sorted=False):
    """ Very close to numpy.percentile, but supports weights.
    NOTE: quantiles should be in [0, 1]!
    :param values: numpy.array with data
    :param quantiles: array-like with many quantiles needed
    :param sample_weight: array-like of the same length as `array`
    :param values_sorted: bool, if True, then will avoid sorting of
        initial array
    :return: numpy.array with computed quantiles.
    """
    values = np.array(values)
    quantiles = np.array(quantiles)
    if sample_weight is None:
        sample_weight = np.ones(len(values))
    sample_weight = np.array(sample_weight)
    assert np.all(quantiles >= 0) and np.all(quantiles <= 1), \
        'quantiles should be in [0, 1]'

    if not values_sorted:
        sorter = np.argsort(values)
        values = values[sorter]
        sample_weight = sample_weight[sorter]

    weighted_quantiles = np.cumsum(sample_weight) - 0.5 * sample_weight
    weighted_quantiles /= np.sum(sample_weight)
    return np.interp(quantiles, weighted_quantiles, values)

class predict_output:
    pass

class drf:
    def __init__(self, **fit_params): # ok
        self.fit_params = fit_params

    def fit(self, X, Y): # ok
        X = convert_to_df(X)
        Y = convert_to_df(Y)
        
        self.X_train = X
        self.Y_train = Y

        X_r = ro.conversion.py2rpy(X)
        Y_r = ro.conversion.py2rpy(Y)
        self.r_fit_object = drf_r_package.drf(X_r, Y_r, **self.fit_params)

    def info(self): # ok
        drf_r_package.print_drf(self.r_fit_object)
    
    def variable_importance(self): # ok
        ro.r('sink("/dev/null")')
        if self.r_fit_object.variable_importance is None:
          ret = drf_r_package.variableImportance(self.r_fit_object)
        else:
          ret = self.r_fit_object.variable_importance
        ro.r('sink()')
        return ret
    
    def predict(self, newdata, **predict_params): #ok
        newdata = convert_to_df(newdata)
        newdata_r = ro.conversion.py2rpy(newdata)
        r_output = drf_r_package.predict_drf(self.r_fit_object, newdata_r)
        #print(len(r_output))
        #print(type(rpy2.robjects.conversion.ri2py(r_output[0])))
        #print(type(r_output))
        weights = base_r_package.as_matrix(r_output[0])
        
        Y = pd.DataFrame(base_r_package.as_matrix(r_output[1]))
        
        if 'functional' not in predict_params.keys():
            functional = 'weights'
        else:
            functional = predict_params['functional']
        if 'transformation' not in predict_params.keys():
            transformation = lambda y:y
        else:
            transformation = predict_params['transformation']
        
        Y = Y.apply(transformation).apply(pd.Series)

        ret = predict_output()        
        
        if functional == 'weights':
            ret.weights = weights
            ret.y = Y.to_numpy()

        elif functional == 'mean':
            ret.mean = np.zeros((newdata.shape[0], Y.shape[1]))
            
            for i in range(newdata.shape[0]):
                ret.mean[i, :] = Y.multiply(weights[i, :],
                                            axis=0).sum().to_numpy()

        elif functional == 'sd':
            ret.sd = np.zeros((newdata.shape[0], Y.shape[1]))
            
            for i in range(newdata.shape[0]):
                for j in range(Y.shape[1]):
                    ret.sd[i, j] = w_cov(Y.iloc[:, j], Y.iloc[:, j],
                                          weights[i, :])**0.5

        elif functional == 'cov':
            ret.cov = np.zeros((newdata.shape[0], Y.shape[1], Y.shape[1]))
            
            for i in range(newdata.shape[0]):
                for j in range(Y.shape[1]):
                    for k in range(Y.shape[1]):
                        ret.cov[i, j, k] = w_cov(Y.iloc[:, j], Y.iloc[:, k],
                                                  weights[i, :])

        elif functional == 'cor':
            ret.cor = np.zeros((newdata.shape[0], Y.shape[1], Y.shape[1]))
            
            for i in range(newdata.shape[0]):
                for j in range(Y.shape[1]):
                    for k in range(Y.shape[1]):
                        cov = w_cov(Y.iloc[:, j], Y.iloc[:, k], weights[i, :])
                        sd1 = w_cov(Y.iloc[:, j], Y.iloc[:, j],
                                     weights[i, :])**0.5
                        sd2 = w_cov(Y.iloc[:, k], Y.iloc[:, k],
                                     weights[i, :])**0.5
                        ret.corr[i, j, k] = cov / (sd1 * sd2)

        elif functional == 'quantile':
            if 'quantiles' in predict_params.keys():
                quantiles = predict_params['quantiles']
            else:
                quantiles = [0.1, 0.5, 0.9]
            #print(quantiles)
            ret.quantile = np.zeros(
                (newdata.shape[0], Y.shape[1], len(quantiles)))
            
            for i in range(newdata.shape[0]):
                for j in range(Y.shape[1]):
                    ret.quantile[i, j, :] = w_quantile(
                        Y.iloc[:, j], quantiles, sample_weight=weights[i, :])

        elif functional == 'sample':
          if 'n' in predict_params.keys():
            n = predict_params['n']
            ret.sample = np.zeros(
                (newdata.shape[0], Y.shape[1], n))
            for i in range(newdata.shape[0]):
                for j in range(n): 
                  ids = np.random.choice(range(Y.shape[0]), 1, p=weights[i, :])[0]
                  ret.sample[i,:, j] = Y.iloc[ids,:]
      
        return ret
