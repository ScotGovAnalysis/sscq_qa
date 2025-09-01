#########################################################################
# Name of file - 05_employment.R
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
  select(SSCQid, ILOEmp, LA, pooled_ind_wt, cluster) %>% 
  filter(pooled_ind_wt > 0) %>% 
  mutate(ILOEmp = factor(ILOEmp, levels = c(1, 2, 3, 4),
                         labels = c("In employment",
                                    "ILO unemployed",
                                    "Inactive",
                                    NA)))

prevyear1 <- prevyear1 %>% 
  left_join(prevyear1.xref, by = "SSCQid") %>%
  select(SSCQid, ILOEmp, LA, pooled_ind_wt, cluster) %>% 
  filter(pooled_ind_wt > 0) %>% 
  mutate(ILOEmp = factor(ILOEmp, levels = c(1, 2, 3, 4),
                         labels = c("In employment",
                                    "ILO unemployed",
                                    "Inactive",
                                    NA)))

prevyear2 <- prevyear2 %>% 
  left_join(prevyear2.xref, by = "SSCQid") %>%
  select(SSCQid, ILOEmp, LA, pooled_ind_wt, cluster) %>% 
  filter(pooled_ind_wt > 0) %>% 
  mutate(ILOEmp = factor(ILOEmp, levels = c(1, 2, 3, 4),
                         labels = c("In employment",
                                    "ILO unemployed",
                                    "Inactive",
                                    NA)))

prevyear3 <- prevyear3 %>% 
  left_join(prevyear3.xref, by = "SSCQid") %>%
  select(SSCQid, ILOEmp, LA, pooled_ind_wt, cluster) %>% 
  filter(pooled_ind_wt > 0) %>% 
  mutate(ILOEmp = factor(ILOEmp, levels = c(1, 2, 3, 4),
                         labels = c("In employment",
                                    "ILO unemployed",
                                    "Inactive",
                                    NA)))

### 3 - Survey proportions ----

sscq_year <- calculate_svymean(
  data = sscqyear,
  var_name = "ILOEmp",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_ind_wt")

prev_year1 <- calculate_svymean(
  data = prevyear1,
  var_name = "ILOEmp",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_ind_wt")

prev_year2 <- calculate_svymean(
  data = prevyear2,
  var_name = "ILOEmp",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_ind_wt")

prev_year3 <- calculate_svymean(
  data = prevyear3,
  var_name = "ILOEmp",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_ind_wt")


### 4 - Combining datasets ----
ILOEmp <- bind_rows(sscq_year, prev_year1, prev_year2, prev_year3)

Year <- c(config$sscqyear, config$sscqyear, config$sscqyear, 
          config$prevyear1, config$prevyear1, config$prevyear1,  
          config$prevyear2, config$prevyear2, config$prevyear2,  
          config$prevyear3, config$prevyear3, config$prevyear3)

ILOEmp$Year <- Year

ILOEmp_table <- ILOEmp %>% select(Year, ILOEmp, perc_with_ci)
ILOEmp_table


### 5 - Visualise estimates ----
ILOEmp_table <- ILOEmp_table %>% 
  group_by(Year) %>% 
  pivot_wider(
    names_from = ILOEmp,
    values_from = perc_with_ci)

print(ILOEmp_table)

saveRDS(ILOEmp_table, "Outputs/ILOEmp_table.rds")


# In employment
ILOEmp1 <- graph_plot(
  data = ILOEmp,
  var_name = "ILOEmp",
  var_value = "In employment",
  plot_title = "SSCQ Respondent Employment by Year (In employment)")

print(ILOEmp1)

ggsave(filename = "Outputs/ILOEmp1.png", plot = ILOEmp1,
       width = 200, height = 120, units = "mm")


# ILO unemployed
ILOEmp2 <- graph_plot(
  data = ILOEmp,
  var_name = "ILOEmp",
  var_value = "ILO unemployed",
  plot_title = "SSCQ Respondent Employment by Year (ILO unemployed)")

print(ILOEmp2)

ggsave(filename = "Outputs/ILOEmp2.png", plot = ILOEmp2,
       width = 200, height = 120, units = "mm")


# Inactive
ILOEmp3 <- graph_plot(
  data = ILOEmp,
  var_name = "ILOEmp",
  var_value = "Inactive",
  plot_title = "SSCQ Respondent Employment by Year (Inactive)")

print(ILOEmp3)

ggsave(filename = "Outputs/ILOEmp3.png", plot = ILOEmp3,
       width = 200, height = 120, units = "mm")
