# distributional random forests
<a href='https://github.com/lorismichel/drf/blob/master/experiments/DRFlogo.png'><img src='https://github.com/lorismichel/drf/blob/master/experiments/DRFlogo.png' align="right" height="175" /></a>


[![Build Status](https://travis-ci.com/lorismichel/drf.svg?branch=master)](https://travis-ci.com/lorismichel/drf)

A package for forest-based conditional distribution estimation of a possibly multivariate response. The estimated distribution is in a simple form which allows for simple and fast computation of different functionals of the conditional distributions such as, for example, conditional quantiles, conditional correlations or conditional probability statements. One can do a heterogeneity adjustment with DRF by obtaining the weighting function which describes the relevance of each training point for a given test point and which can further be used as an input to some other method.

This repository started as a fork from the [grf] (https://github.com/grf-labs/grf) repository, which is itself forked from [ranger](https://github.com/imbs-hl/ranger) repository -- we sincerely thank the authors of both repositories for their useful and free packages.

### Installation

The latest release of the package can be installed through CRAN:

```R
install.packages("drf")
```

Any published release can also be installed from source:

```R
install.packages("https://raw.github.com/lorismichel/drf/master/releases/drf_1.0.0.tar.gz", repos = NULL, type = "source")
```

Note that to install from source, a compiler that implements C++11 is required (clang 3.3 or higher, or g++ 4.8 or higher). If installing on Windows, the RTools toolchain is also required.


### Usage Example
