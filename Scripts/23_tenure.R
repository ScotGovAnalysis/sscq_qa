#########################################################################
# Name of file - 23_tenure.R
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 4.4.2
#
# Description - Imports the relevant data and variables. Means and CIs
# calculated an then plotted in tables and graphs for each level

#########################################################################

# clear global environment
rm(list = ls())

### 0 - Setup ----

# Run setup script which loads all required packages and functions and 
# executes the config.R script.

source(here::here("Scripts", "00_setup.R"))


### 1 - Import files ----
# Path to current SSCQ data
sscqyear <- haven::read_sas(config$sscq.path)
# Path to file with cluster info 
sscqxref <- haven::read_sas(config$sscq.xref)


# Path to previous SSCQ data
prevyear1 <- haven::read_sas(config$prevyear1.path)
# Path to file with cluster info 
prevyear1.xref <- haven::read_sas(config$prevyear1.xref)


# Path to previous SSCQ data
prevyear2 <- haven::read_sas(config$prevyear2.path)
# Path to file with cluster info 
prevyear2.xref <- haven::read_sas(config$prevyear2.xref)


# Path to previous SSCQ data
prevyear3 <- haven::read_sas(config$prevyear3.path)
# Path to file with cluster info 
prevyear3.xref <- haven::read_sas(config$prevyear3.xref)


### 2 - Merge data frames and recode variables ----
sscqyear <- sscqyear %>% 
  left_join(sscqxref, by = "SSCQid") %>%
  select(SSCQid, tenure, LA, pooled_hh_wt, cluster) %>% 
  filter(pooled_hh_wt > 0) %>% 
  mutate(tenure = factor(tenure, levels = c(-1, 1, 2, 3, 4, 5),
                         labels = c(NA,
                                    "Buying with the help of a mortgage or loan",
                                    "Own outright (i.e. no mortgage or loan)",
                                    "Pay part rent and part mortgage (shared ownership)",
                                    "Renting (including in receipt of housing benefit)",
                                    "Living here rent free")))
prevyear1 <- prevyear1 %>% 
  left_join(prevyear1.xref, by = "SSCQid") %>%
  select(SSCQid, tenure, LA, pooled_hh_wt, cluster) %>% 
  filter(pooled_hh_wt > 0) %>% 
  mutate(tenure = factor(tenure, levels = c(-1, 1, 2, 3, 4, 5),
                         labels = c(NA,
                                    "Buying with the help of a mortgage or loan",
                                    "Own outright (i.e. no mortgage or loan)",
                                    "Pay part rent and part mortgage (shared ownership)",
                                    "Renting (including in receipt of housing benefit)",
                                    "Living here rent free")))

prevyear2 <- prevyear2 %>% 
  left_join(prevyear2.xref, by = "SSCQid") %>%
  select(SSCQid, tenure, LA, pooled_hh_wt, cluster) %>% 
  filter(pooled_hh_wt > 0) %>% 
  mutate(tenure = factor(tenure, levels = c(-1, 1, 2, 3, 4, 5),
                         labels = c(NA,
                                    "Buying with the help of a mortgage or loan",
                                    "Own outright (i.e. no mortgage or loan)",
                                    "Pay part rent and part mortgage (shared ownership)",
                                    "Renting (including in receipt of housing benefit)",
                                    "Living here rent free")))

prevyear3 <- prevyear3 %>% 
  left_join(prevyear3.xref, by = "SSCQid") %>%
  select(SSCQid, tenure, LA, pooled_hh_wt, cluster) %>% 
  filter(pooled_hh_wt > 0) %>% 
  mutate(tenure = factor(tenure, levels = c(-1, 1, 2, 3, 4, 5),
                         labels = c(NA,
                                    "Buying with the help of a mortgage or loan",
                                    "Own outright (i.e. no mortgage or loan)",
                                    "Pay part rent and part mortgage (shared ownership)",
                                    "Renting (including in receipt of housing benefit)",
                                    "Living here rent free")))


### 3 - Survey proportions ----

sscq_year <- calculate_svymean(
  data = sscqyear,
  var_name = "tenure",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_hh_wt")

prev_year1 <- calculate_svymean(
  data = prevyear1,
  var_name = "tenure",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_hh_wt")

prev_year2 <- calculate_svymean(
  data = prevyear2,
  var_name = "tenure",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_hh_wt")

prev_year3 <- calculate_svymean(
  data = prevyear3,
  var_name = "tenure",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_hh_wt")


### 4 - Combining datasets ----
Tenure <- bind_rows(sscq_year, prev_year1, prev_year2, prev_year3)

Year <- c(config$sscqyear, config$sscqyear, config$sscqyear, config$sscqyear, config$sscqyear,
          config$prevyear1, config$prevyear1, config$prevyear1, config$prevyear1, config$prevyear1,
          config$prevyear2, config$prevyear2, config$prevyear2, config$prevyear2, config$prevyear2, 
          config$prevyear3, config$prevyear3, config$prevyear3, config$prevyear3, config$prevyear3)

Tenure$Year <- Year

Tenure_table <- Tenure %>% select(Year, tenure, perc_with_ci)
Tenure_table

### 5 - Visualise estimates ----
Tenure_table <- Tenure_table %>% 
  group_by(Year) %>% 
  pivot_wider(
    names_from = tenure,
    values_from = perc_with_ci)

print(Tenure_table)

saveRDS(Tenure_table, "Outputs/Tenure_table.rds")


# Mortgaged
Tenure1 <- graph_plot(
  data = Tenure,
  var_name = "tenure",
  var_value = "Buying with the help of a mortgage or loan",
  plot_title = "SSCQ Respondent Tenure by Year (Mortgaged)")

print(Tenure1)

ggsave(filename = "Outputs/Tenure1.png", plot = Tenure1,
       width = 200, height = 120, units = "mm")

# Owned outright
Tenure2 <- graph_plot(
  data = Tenure,
  var_name = "tenure",
  var_value = "Own outright (i.e. no mortgage or loan)",
  plot_title = "SSCQ Respondent Tenure by Year (Owned Outright)")

print(Tenure2)

ggsave(filename = "Outputs/Tenure2.png", plot = Tenure2,
       width = 200, height = 120, units = "mm")

# Shared ownership
Tenure3 <- graph_plot(
  data = Tenure,
  var_name = "tenure",
  var_value = "Pay part rent and part mortgage (shared ownership)",
  plot_title = "SSCQ Respondent Tenure by Year (Shared Ownership)")

print(Tenure3)

ggsave(filename = "Outputs/Tenure3.png", plot = Tenure3,
       width = 200, height = 120, units = "mm")

# Renting
Tenure4 <- graph_plot(
  data = Tenure,
  var_name = "tenure",
  var_value = "Renting (including in receipt of housing benefit)",
  plot_title = "SSCQ Respondent Tenure by Year (Renting)")

print(Tenure4)

ggsave(filename = "Outputs/Tenure4.png", plot = Tenure4,
       width = 200, height = 120, units = "mm")

# Rent free
Tenure5 <- graph_plot(
  data = Tenure,
  var_name = "tenure",
  var_value = "Living here rent free",
  plot_title = "SSCQ Respondent Tenure by Year (Rent Free)")

print(Tenure5)

ggsave(filename = "Outputs/Tenure5.png", plot = Tenure5,
       width = 200, height = 120, units = "mm")

