from rpy2.robjects.packages import importr
import rpy2.robjects.packages as rpackages

# Choosing a CRAN Mirror
utils = rpackages.importr('utils')
utils.chooseCRANmirror(ind=1)
# install packages from CRAN if not present
# packnames = ('drf')
# names_to_install = [x for x in packnames if not rpackages.isinstalled(x)]
# if len(names_to_install) > 0:
#     utils.install_packages(StrVector(names_to_install))

# load packages
base, drf_r_package = importr('base'), importr('drf')

from .code import *
