# Analysis for paper 5

library(readxl)
library(tidyverse)
library(lubridate)
library(zoo)
library(epitools)
library(survival)
library(rms)
library(survminer)
library(broom)

# set theme for ggplot2 figures
theme_set(theme_bw(base_size = 10, base_family = "sans"))

#######################################
#### Importing the data; wrangling ####
#######################################