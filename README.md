

[![Travis-CI Build Status](https://travis-ci.org/mlampros/FeatureSelection.svg?branch=master)](https://travis-ci.org/mlampros/FeatureSelection)
[![codecov.io](https://codecov.io/github/mlampros/FeatureSelection/coverage.svg?branch=master)](https://codecov.io/github/mlampros/FeatureSelection?branch=master)
<a href="https://www.buymeacoffee.com/VY0x8snyh" target="_blank"><img src="https://www.buymeacoffee.com/assets/img/custom_images/orange_img.png" alt="Buy Me A Coffee" height="21px" ></a>


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

UPDATE : 13-06-2017 [ One needs an older xgboost version (= 0.4-4) to install (and experiment with) the package ]

```R

devtools::install_version("xgboost", version = "0.4-4", repos = "http://cran.us.r-project.org")

```
