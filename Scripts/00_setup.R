#########################################################################
# Name of file - 00_setup.R
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 4.4.2
#
# Description - Sets up environment required for running the SSCQ QA RAP.

#########################################################################

### 1 - Load packages ----
library(tidyverse)
library(survey)
library(readxl)
library(haven)
library(sgplot)
library(ggbreak)
library(cowplot)
library(knitr)
library(dplyr)
library(ragg)
library(rmarkdown)

sgplot::use_sgplot()


### 2 - Load functions from functions folder of SSCQ QA RAP ----

walk(list.files(here::here("Functions"), pattern = "\\.R$", full.names = TRUE), 
     source)


### 3 - Load config file from code folder of SSCQ QA RAP ----

# The config.R script is the only file which needs to be updated before 
# the RAP can be run. 

source(here::here("Scripts", "config.R"))

