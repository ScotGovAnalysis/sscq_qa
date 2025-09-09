#########################################################################
# Name of file - config_example.R
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 4.4.2
#
# Description - Specifies file paths, file names.
# This is the only file which requires manual changes before the 
# RAP process is run. It is not pushed to git as it contains 
# sensitive information.
#########################################################################

### 1 - Sample year - TO UPDATE ----

# initiate list 
config <- list()

# Year the data is being weighted for (usually the previous calendar year)
# (i.e., 20XX)
config$sscqyear <- 20xx

# Previous year
config$prevyear1 <- 20xx

# Previous year
config$prevyear2 <- 20xx

# Previous year
config$prevyear3 <- 20xx

# Previous year
config$prevyear4 <- 20xx

# Previous year
config$prevyear5 <- 20xx

# Date that the RAP is run to produce the weights
# MUST be changed each time it's run (if on a different day)
config$date <- xxx


### 2 - File paths - TO UPDATE ----

# This section may need to be updated if the data storage location has changed

# Path to SAS data
config$sasdata.path <- xxx

# Path to SSCQ year data
config$sscq.path <- xxx
# Path to file with cluster info 
config$sscq.xref <- xxx

# Path to prevyear1 data
config$prevyear1.path <- xxx
# Path to file with cluster info 2022
config$prevyear1.xref <- xxx


# Path to prevyear2 data
config$prevyear2.path <- xxx
# Path to file with cluster info 2019
config$prevyear2.xref <- xxx


# Path to prevyear3 data
config$prevyear3.path <- xxx
# Path to file with cluster info 2018
config$prevyear3.xref <- xxx


# Path to prevyear4 data
config$prevyear4.path <- xxx
# Path to file with cluster info 2017
config$prevyear4.xref <- xxx


# Path to prevyear5 data
config$prevyear5.path <- xxx
# Path to file with cluster info 2016
config$prevyear5.xref <- xxx


# Path to SCJS trans data
config$sscq.scjs <- xxx

config$prevyear1.scjs <- xxx

#config$prevyear2.scjs <- xxx

#config$prevyear3.scjs <- xxx


# Path to SHeS trans data
config$sscq.shes <- xxx

config$prevyear1.shes <- xxx

#config$prevyear2.shes <- xxx

#config$prevyear3.shes <- xxx


# Path to SHS trans data
config$sscq.shs <- xxx

config$prevyear1.shs <- xxx

#config$prevyear2.shs <- xxx

#config$prevyear3.shs <- xxx




