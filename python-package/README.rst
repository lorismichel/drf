Python Wrapper for Distributional Random Forests (DRF)
=================================================================================
Installation
_________________________________________________________________________________

To install the python package from PyPI, type::
    
    pip install -i https://test.pypi.org/simple/ drf==0.1
    
Alternatively, it can be directly installed from Github by running:: 
    
    pip install -e "git+https://github.com/lorismichel/drf#egg=pkg&subdirectory=python-package"

If you have the Github repository "lorismichel/drf" already cloned, you can run the following command
from the python-package subdirectory::

    pip install .

Usage
_________________________________________________________________________________
Here is a short example illustrating the usage of the package:

.. code-block:: python
    
    import pandas as pd
    import numpy as np
    from drf import drf

    # generate data
    n = 1000
    p = 10
    d = 2
    X = np.random.normal(0, 1, size=(n, p))
    Y = np.random.normal(0, 1, size=(n, d))
    Y[:,0] = Y[:,0] + X[:,0] #mean shift of Y1 based on X1
    Y[:,1] = Y[:,1] * X[:,1] #variance shift of Y2 based on X2
    X = pd.DataFrame(X)
    Y = pd.DataFrame(Y)

    # fit model
    DRF = drf(min_node_size = 15, num_trees = 2000, splitting_rule = "FourierMMD") #those are the default values
    DRF.fit(X, Y)
    DRF.info() #prints variable importance

    #generate test data
    X_test = pd.DataFrame(np.random.normal(0, 1, size=(100, p)))

    # estimated conditional distribution represented via weights
    out = DRF.predict(newdata = X_test)
    print(out.weights)

    # many distributional functionals are implemented and do not need to be manually computed from the weights  
    out = DRF.predict(newdata = X_test, functional = "mean")
    print(out.mean)

    # covariance matrix at a fixed test point
    out = DRF.predict(newdata = [0]*p, functional = "cov")
    print(out.cov[0,:,:])

    # we can transform the response beforehand to obtain more complicated quantities 
    out = DRF.predict(newdata = X_test, functional = "quantile", transformation = lambda y: (np.sin(y[1]), y[1]*y[2], y[2]**2), quantiles=[0.1, 0.9])
    print(out.quantile[0,1,:]) # 0.1 and 0.9 quantiles for first test point in newdata and for the second component of transformed y

    # we automatically handle factor variables by using one-hot encoding
    X['cat'] = np.random.choice(['a', 'b', 'c', 'd', 'e'], n, replace=True)
    Y['new'] = np.random.normal(0, 1, size=n) + (X['cat']=='a')
    DRF.fit(X, Y)
    DRF.info()
