import pandas as pd

import rpy2.robjects as ro

import rpy2.robjects.numpy2ri
rpy2.robjects.numpy2ri.activate()
from rpy2.objects import pandas2ri
pandas2ri.activate()

def convert_to_R(X):
    if type(X) == np.ndarray:
        if len(X.shape) == 1: 
            X = X.reshape(1, X.shape[0])
        nr, nc = X.shape
        return ro.r.matrix(X, nrow=nr, ncol=nc)

    if type(X) == pd.core.frame.DataFrame:
        return ro.conversion.py2rpy(X)

class drf:
    def __init__(self, **fit_params):
        self.fit_params = fit_params 
    
    class predict_output: pass
    
    def fit(self, X, Y):
        self.X_train = X
        self.Y_train = Y
        
        X_r = convert_to_R(X)
        Y_r = convert_to_R(Y)
        
        self.r_fit_object = drf_r_package.drf(X_r, Y_r, **self.fit_params)

    def myFun(**kwargs):  
        for key, value in kwargs.items(): 
            print ("%s == %s" %(key, value)) 

    def predict(self, newdata, **predict_params):
        newdata_r = convert_to_R(newdata)
        r_output = drf_r_package.predict_drf(self.r_fit_object, newdata_r, **predict_params)

        ret = self.predict_output()
        for i in range(len(r_output)):
            value = r_output[i]
            if r_output.names[i] == 'weights':
                value = base.as_matrix(r_output[i])
            setattr(ret, r_output.names[i], value)
        return ret