

[![Travis-CI Build Status](https://travis-ci.org/mlampros/FeatureSelection.svg?branch=master)](https://travis-ci.org/mlampros/FeatureSelection)
[![codecov.io](https://codecov.io/github/mlampros/FeatureSelection/coverage.svg?branch=master)](https://codecov.io/github/mlampros/FeatureSelection?branch=master)


#### Feature Selection in R using glmnet-lasso, xgboost and ranger
<br>
  This is a wrapper-package for glmnet-lasso, xgboost and ranger. After downloading use ? to read info about each function (i.e. ?feature_selection). More details can be found in the blog-post (http://mlampros.github.io/2016/02/14/feature-selection/) and to download the latest version from Github use the 'install_github' function of the devtools package,
<br>

```R

devtools::install_github('mlampros/FeatureSelection')

```

<br>


UPDATED : 18-05-2016 [ I added tests and code-coverage ]
<br>

UPDATE : 13-06-2017 [ The package uses an xgboost version of 0.4-4 ]

```R

devtools::install_version("xgboost", version = "0.4-4", repos = "http://cran.us.r-project.org")

```
