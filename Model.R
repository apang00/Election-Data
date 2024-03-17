#### Preamble ####
# Author: Yi Fei Pang
# Date: 2024-03-17
# Student ID: 1005182598
# Source: Cohn, Nate. 2016. “We Gave Four Good Pollsters the Same Raw Data. 
#   They Had Four Different Results.” The New York Times, September. 
#   https://www.nytimes.com/interactive/2016/09/20/upshot/the-error-the-polling-world-rarely-talks-about.html.

#### Description ####
# Since the data we will be looking at will be election results between two
# Parties, I would be using a logistic regression. EXPAND

library(boot)
library(collapse)
library(dataverse)
library(gutenbergr)
library(janitor)
library(knitr)
library(marginaleffects)
library(modelsummary)
library(rstanarm)
library(tidybayes)
library(tidyverse)
library(dplyr)