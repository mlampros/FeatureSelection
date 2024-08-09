
## FeatureSelection

* **09-08-2024**: I replaced **dplyr** with **data.table** because the **dplyr** functions 'summarise_each_' and 'funs' gave deprecation warnings
* **19-05-2021**: I replaced **doMC** with **doParallel** because **doMC** does not work on both **Unix** and **Windows** OS (applies only to **'glmnet-lasso'** method if number of threads > 1)
* **03-02-2020**: 
  + Updated the R files so that *Feature Selection* works with the newest versions of the imported R packages
  + Adjusted the tests
  + Added Dockerfile and docker image
  + Updated the README.md and .travis.yml files
* **18-05-2016**: I added tests and code-coverage
