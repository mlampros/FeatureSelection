
## FeatureSelection

* **18-05-2016**: I added tests and code-coverage
* **03-02-2020**: 
  + Updated the R files so that *Feature Selection* works with the newest versions of the imported R packages
  + Adjusted the tests
  + Added Dockerfile and docker image
  + Updated the README.md and .travis.yml files
* **19-05-2021**: I replaced **doMC** with **doParallel** because **doMC** does not work on both **Unix** and **Windows** OS (applies only to **'glmnet-lasso'** method if number of threads > 1)
