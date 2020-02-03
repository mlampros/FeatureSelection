

[![Travis-CI Build Status](https://travis-ci.org/mlampros/FeatureSelection.svg?branch=master)](https://travis-ci.org/mlampros/FeatureSelection)
[![codecov.io](https://codecov.io/github/mlampros/FeatureSelection/coverage.svg?branch=master)](https://codecov.io/github/mlampros/FeatureSelection?branch=master)
<a href="https://www.buymeacoffee.com/VY0x8snyh" target="_blank"><img src="https://www.buymeacoffee.com/assets/img/custom_images/orange_img.png" alt="Buy Me A Coffee" height="21px" ></a>
[![](https://img.shields.io/docker/automated/mlampros/featureselection.svg)](https://hub.docker.com/r/mlampros/featureselection)

<br>

#### Feature Selection in R using glmnet-lasso, xgboost and ranger

<br>

This R package wraps **glmnet-lasso**, **xgboost** and **ranger** to perform feature selection. After downloading use ? to read info about each function (i.e. ?feature_selection). More details can be found in the blog-post (http://mlampros.github.io/2016/02/14/feature-selection/). To download the latest version from Github use,

<br>

```R
remotes::install_github('mlampros/FeatureSelection')

```

<br>

Currently there is a new version of *glmnet* (3.0.0) with new functionality (*relax*,  *trace*,  *assess*, *bigGlm*), however it requires an R version of 3.6.0 (see the [new vignette](https://cran.r-project.org/web/packages/glmnet/vignettes/relax.pdf)  for more information).

<br>


**UPDATE 03-02-2020**

<br>

**Docker images** of the *FeatureSelection* package are available to download from my [dockerhub](https://hub.docker.com/r/mlampros/featureselection) account. The images come with *Rstudio* and the *R-development* version (latest) installed. The whole process was tested on Ubuntu 18.04. To **pull** & **run** the image do the following,

<br>

```R

docker pull mlampros/featureselection:rstudiodev

docker run -d --name rstudio_dev -e USER=rstudio -e PASSWORD=give_here_your_password --rm -p 8787:8787 mlampros/featureselection:rstudiodev

```

<br>

The user can also **bind** a home directory / folder to the image to use its files by specifying the **-v** command,

<br>

```R

docker run -d --name rstudio_dev -e USER=rstudio -e PASSWORD=give_here_your_password --rm -p 8787:8787 -v /home/YOUR_DIR:/home/rstudio/YOUR_DIR mlampros/featureselection:rstudiodev


```

<br>

In the latter case you might have first give permission privileges for write access to **YOUR_DIR** directory (not necessarily) using,

<br>

```R

chmod -R 777 /home/YOUR_DIR


```

<br>

The **USER** defaults to *rstudio* but you have to give your **PASSWORD** of preference (see [www.rocker-project.org](https://www.rocker-project.org/) for more information).

<br>

Open your web-browser and depending where the docker image was *build / run* give, 

<br>

**1st. Option** on your personal computer,

<br>

```R
http://0.0.0.0:8787 

```

<br>

**2nd. Option** on a cloud instance, 

<br>

```R
http://Public DNS:8787

```

<br>

to access the Rstudio console in order to give your username and password.

<br>

