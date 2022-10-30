---
title: "Oregon spotted a frog: Rana Pretiosa"
excerpt: "Machine Learning with mlr3"
date: 
author: "Federica Gazzelloni"
draft: false
images:
series:
tags:
categories: 
layout: single
---





# Overview

I made a package! Looking for data to use for one of my data visualization I stomped into this data about frogs in Oregon (US) and realized that it was very interesting for making both classification and regression models. 

So, I wrapped the data into a package, and even if it is still a work in progress I started using it for practicing **machine learning** algorithms.

More info about it will follow. For now this project is all about how to use {mlr3} package for analysing and predicting data. 

**mlr3** is a machine learning **ecosystem**, it provides a unified interface to use different machine learning models in R.


Let's load the libraries, and start using **mlr3** with **oregonfrogs** data!


```r
library(tidyverse)
library(mlr3)
# remotes::install_github("mlr-org/mlr3spatiotempcv")
library(mlr3spatiotempcv)
library(mlr3learners) # needed to initiate a new learner (such as classif.ranger)
# remotes::install_github("mlr-org/mlr3extralearners")
library(mlr3extralearners)
library(ranger)
# install.packages("apcluster")
# remotes::install_github("mlr-org/mlr3proba")
library("mlr3viz")
```


To install {oregonfrogs}, which is still in its development version you need to install it from github:

```r
# remotes::install_github("fgazzelloni/oregonfrogs")
library(oregonfrogs)
```


```r
oregonfrogs %>% 
  head(3) %>% 
  select(3:4,6:8,10:11)
```

```
# A tibble: 3 × 7
  HabType SurveyDate Frequency UTME_83 UTMN_83 Female Water        
  <chr>   <chr>          <dbl>   <dbl>   <dbl>  <dbl> <chr>        
1 Pond    9/25/2018       164.  597369 4846486      0 Deep water   
2 Pond    10/2/2018       164.  597352 4846487      0 Deep water   
3 Pond    10/9/2018       164.  597345 4846458      0 Shallow water
```



```r
oregonfrogs1 <- oregonfrogs %>%
  dplyr::select(UTME_83, UTMN_83) %>%
  sf::st_as_sf(coords = c(1,2), 
               crs = "+proj=utm +zone=10") %>%
  sf::st_transform(crs = "+proj=longlat +datum=WGS84")  %>%
  sf::st_coordinates() %>%
  cbind(oregonfrogs) %>%
  mutate(SurveyDate=as.Date(SurveyDate,"%m/%d/%Y"),
         month=lubridate::month(SurveyDate),
         Water=as.factor(Water))%>%
  select(-SurveyDate,-UTME_83,-UTMN_83,-Site)
```



```
  UTME_83 UTMN_83         X        Y
1  597369 4846486 -121.7903 43.76502
2  597352 4846487 -121.7905 43.76503
3  597345 4846458 -121.7906 43.76477
4  597340 4846464 -121.7907 43.76483
5  597344 4846460 -121.7906 43.76479
6  597410 4846451 -121.7898 43.76470
```


**mlr3 package** provides a `task()` function, which allows to set the data to use inside the selected model.


More specifically, here is used `mlr3spatiotempcv::TaskClassifST()` specific for **spatial classification** modeling tasks.  

Among the function's *options*, there are:
- *id*: the  identification of the location,  
- *backend*: the data to use in the model
- *target*: the response variable
- ...



```r
task = mlr3spatiotempcv::TaskClassifST$new(
  id = "lake",
  backend = mlr3::as_data_backend(oregonfrogs1), 
  target = "Water", 
  #positive = "FALSE",  ### ????
  coordinate_names = c("X", "Y"),
  coords_as_features = TRUE,
  crs = "+proj=longlat +datum=WGS84"   # "+proj=utm +zone=10"
)
```

Some visualizations:


```r
# plot response against each predictor
mlr3viz::autoplot(task, type = "duo")

# plot all variables against each other
mlr3viz::autoplot(task, type = "pairs")
```

And the list of available `learners`:

```r
mlr3extralearners::list_mlr3learners(
  filter = list(class = "classif",type="multiclass"), 
  select = c("id", "mlr3_package", "required_packages")) |>
  head()
```


The second step is to set the **learner**: `mlr3::lrn()`

This is the model type and prediction outcome type:


```r
# model type
# mlr_learners$get("classif.ranger")
learner = mlr3::lrn("classif.ranger", predict_type = "prob")
# learner$help()
learner
```

```
<LearnerClassifRanger:classif.ranger>
* Model: -
* Parameters: num.threads=1
* Packages: mlr3, mlr3learners, ranger
* Predict Types:  response, [prob]
* Feature Types: logical, integer, numeric, character, factor, ordered
* Properties: hotstart_backward, importance, multiclass, oob_error,
  twoclass, weights
```


```r
learner$param_set
```


```r
learner$train(task)
```


```r
learner$model
```

```
Ranger result

Call:
 ranger::ranger(dependent.variable.name = task$target_names, data = task$data(),      probability = self$predict_type == "prob", case.weights = task$weights$weight,      num.threads = 1L) 

Type:                             Probability estimation 
Number of trees:                  500 
Sample size:                      311 
Number of independent variables:  14 
Mtry:                             3 
Target node size:                 10 
Variable importance mode:         none 
Splitrule:                        gini 
OOB prediction error (Brier s.):  0.1483088 
```



```r
prediction <- learner$predict_newdata(oregonfrogs1)
measure = msr("classif.acc")
prediction$score(measure)
```

```
classif.acc 
  0.9549839 
```



```r
prediction$confusion
```

```
               truth
response        Deep water No water Shallow water Unknown water
  Deep water            75        0             6             0
  No water               0       10             0             0
  Shallow water          5        2           194             0
  Unknown water          1        0             0            18
```


```r
mlr3viz::autoplot(prediction)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-15-1.png" width="672" />



```r
# resampling
resampling = mlr3::rsmp("repeated_spcv_coords", 
                        folds = 5, 
                        repeats = 100)
resampling
```

```
<ResamplingRepeatedSpCVCoords>: Repeated coordinate-based k-means clustering resampling
* Iterations: 500
* Instantiated: FALSE
* Parameters: folds=5, repeats=100
```

A *logging record** can be set for debugging in case model crashes.


```r
lgr::get_logger("mlr3")$set_threshold("warn")
```

Parallelization:

```r
parallelly::availableCores()
set_threads(learner)
```



```r
# fit resamples
time = Sys.time()
rr_spcv_ranger = mlr3::resample(task = task,
                             learner = learner,
                             resampling = resampling)
Sys.time() - time
```



```r
rr_spcv_ranger$score
```



```r
# accuracy
score_spcv_ranger = rr_spcv_ranger$score(
  measure = mlr3::msr("classif.acc")) %>% 
  select(task_id, learner_id, resampling_id, classif.acc)
```


```r
# avg accuracy
mean(score_spcv_ranger$classif.acc) %>%
  round(2)  # 0.64
```

## Tweaking the hyperparameters of the learner


In **random forests**, the hyperparameters *mtry*, *min.node.size* and *sample.fraction* determine the degree of randomness, and should be tuned.

- **mtry** indicates how many predictor variables should be used in each tree
- **sample.fraction** parameter specifies the fraction of observations to be used in each tree
- **min.node.size** parameter indicates the number of observations a terminal node should at least have

source: https://geocompr.robinlovelace.net/eco.html



```r
#### tuning
tune_level = mlr3::rsmp("spcv_coords", folds = 5)

terminator = mlr3tuning::trm("evals", n_evals = 50)

tuner = mlr3tuning::tnr("random_search")
```

### Search space

To specify tuning limits `paradox::ps()` is used:

```r
search_space = 
  paradox::ps(
    mtry = paradox::p_int(lower = 1, 
                          upper = ncol(task$data()) - 1),
    sample.fraction = paradox::p_dbl(lower = 0.2, 
                                     upper = 0.9),
    min.node.size = paradox::p_int(lower = 1, 
                                   upper = 10)
    )
```


### Automation

Automated tuning specification via the `mlr3tuning::AutoTuner()` function:

```r
autotuner_rf = 
  mlr3tuning::AutoTuner$new(
    learner = learner,
    resampling = mlr3::rsmp("spcv_coords", folds = 5), # spatial partitioning
    measure = mlr3::msr("classif.acc"), # performance measure
    terminator = mlr3tuning::trm("evals", n_evals = 50), # specify 50 iterations
    search_space = search_space, # predefined hyperparameter search space
    tuner = mlr3tuning::tnr("random_search") # specify random search
    )
```



```r
# hyperparameter tuning
time = Sys.time()

set.seed(0412022)
autotuner_rf$train(task)

Sys.time() - time
```


```r
autotuner_rf$tuning_result
```


```r
autotuner_rf$predict(task)
# pred = terra::predict(..., model = autotuner_rf, fun = predict)

# save.image("data/oregonfrogs_mlr3.RData")
```

