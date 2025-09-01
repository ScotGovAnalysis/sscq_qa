#########################################################################
# Name of file - 28_render_report.R
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 4.4.2
#
# Description - Sets up environment required for running the SSCQ QA RAP.

#########################################################################

# clear global environment
rm(list = ls())

### 0 - Setup ----

# Run setup script which loads all required packages and functions and 
# executes the config.R script.

source(here::here("Scripts", "00_setup.R"))


### 1 - Render ----
# Render the report
rmarkdown::render(
  input       = here::here("Scripts", "Internal QA Report.Rmd"),  # adjust if your file has a different name
  output_file = here::here("Outputs", "Internal_QA_Report.docx")
)
