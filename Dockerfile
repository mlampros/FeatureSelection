FROM rocker/rstudio:devel

LABEL maintainer='Lampros Mouselimis'

RUN export DEBIAN_FRONTEND=noninteractive; apt-get -y update && \
 apt-get install -y make zlib1g-dev libssl-dev libcurl4-openssl-dev && \
 apt-get install -y sudo && \
 apt-get -y update && \
 R -e "install.packages(c( 'doParallel', 'data.table', 'glmnet', 'ranger', 'xgboost', 'Matrix', 'magrittr', 'utils', 'stats', 'graphics', 'grDevices', 'rlang', 'testthat', 'covr', 'remotes' ), repos =  'https://cloud.r-project.org/' )" && \
 R -e "remotes::install_github('mlampros/FeatureSelection', upgrade = 'always', dependencies = TRUE, repos = 'https://cloud.r-project.org/')" && \
 apt-get autoremove -y && \
 apt-get clean

ENV USER rstudio


