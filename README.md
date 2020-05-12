# 2020_group09
### Description
Final project for 22100 R for bio data science - group 9

In this project we analyse prostate cancer data using R and tidyverse. 


### Data
All the data is available within this git.
The original data was found
[here](http://biostat.mc.vanderbilt.edu/wiki/Main/DataSets "biostat.vanderbilt") and is a dataset on prostate cancer from 1976 with 502 patients and 18 variables. The labels are explained [here](http://biostat.mc.vanderbilt.edu/wiki/pub/Main/DataSets/Cprostate.html "Labels explained") and the original article can be downloaded [here](https://cdn.discordapp.com/attachments/697348216479547433/700257977130811463/SELECTING_OPTIMAL_TREATMENT_IN_CLINICA_original.pdf "selecting optimal treatment in clinical trials using covariate information").

To add more information and patients the dataset was sublemented with the GDC - TCGA-PRAD [dataset](shorturl.at/emT04 "UCSCXena"). This contains two different data sets, phenotype and survival, each with 623 observations, and 127 and 4 variables respectively.    


### Packages and their versions

The following packages needs to be installed for this project to run.\
modelr_0.1.7 &nbsp;    keras_2.2.5.0  &nbsp; &nbsp;   devtools_2.3.0  &nbsp;   usethis_1.6.1    &nbsp; clusteval_0.2.1\
styler_1.3.2   &nbsp;  ggthemes_4.2.0    &nbsp; patchwork_1.0.0 &nbsp; broom_0.5.6  &nbsp;    forcats_0.5.0  \
stringr_1.4.0  &nbsp; dplyr_0.8.5     &nbsp; purrr_0.3.4   &nbsp;   readr_1.3.1   &nbsp;   tidyr_1.0.3    \
tibble_3.0.1  &nbsp; ggplot2_3.3.0   &nbsp; tidyverse_1.3.0\

### Installations 
##### Keras 
Keras is used for running artifitial neural networks and is in this project used to run classification of the patients status (alive, dead from PCa, dead from other causes). \
For installing Keras you need to run the following code:

```
install.packages("devtools")
library("devtools")
install_github("rstudio/keras")
# Would you like to install miniconda? Y
library(keras)
install_keras(tensorflow = "1.13.1")
```

##### Clusterval 
Clusterval is used for evaluatinig the different clusters genertated running K means clustering. 
To install Clusterval one needs to do the following:

```
install.packages("remotes")
remotes::install_github("ramhiser/clusteval")
```
