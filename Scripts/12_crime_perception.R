#########################################################################
# Name of file - 12_crime_perception.R
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
prevyear4 <- haven::read_sas(config$prevyear4.path)
# Path to file with cluster info 
prevyear4.xref <- haven::read_sas(config$prevyear4.xref)


# Path to previous SSCQ data
prevyear5 <- haven::read_sas(config$prevyear5.path)
# Path to file with cluster info 
prevyear5.xref <- haven::read_sas(config$prevyear5.xref)


### 2 - Merge data frames and recode variables ----

sscqyear <- sscqyear %>% 
  left_join(sscqxref, by = "SSCQid") %>%
  select(SSCQid, CrimeArea, LA, pooled_crim_wt, cluster) %>% 
  filter(pooled_crim_wt > 0) %>% 
  mutate(CrimeArea = factor(CrimeArea, levels = c(-4, -1, 1, 2, 3, 4, 5),
                            labels = c(NA,
                                       NA,
                                       "A lot more",
                                       "A little more",
                                       "About the same",
                                       "A little less",
                                       "A lot less")))

prevyear1 <- prevyear1 %>% 
  left_join(prevyear1.xref, by = "SSCQid") %>%
  select(SSCQid, CrimeArea, LA, pooled_crim_wt, cluster) %>% 
  filter(pooled_crim_wt > 0) %>% 
  mutate(CrimeArea = factor(CrimeArea, levels = c(-4, -1, 1, 2, 3, 4, 5),
                            labels = c(NA,
                                       NA,
                                       "A lot more",
                                       "A little more",
                                       "About the same",
                                       "A little less",
                                       "A lot less")))

prevyear4 <- prevyear4 %>% 
  left_join(prevyear4.xref, by = "SSCQid") %>%
  select(SSCQid, CrimeArea, LA, pooled_crim_wt, cluster) %>% 
  filter(pooled_crim_wt > 0) %>% 
  mutate(CrimeArea = factor(CrimeArea, levels = c(-4, -1, 1, 2, 3, 4, 5),
                            labels = c(NA,
                                       NA,
                                       "A lot more",
                                       "A little more",
                                       "About the same",
                                       "A little less",
                                       "A lot less")))

prevyear5 <- prevyear5 %>% 
  left_join(prevyear5.xref, by = "SSCQid") %>%
  select(SSCQid, CrimeArea, LA, pooled_crim_wt, cluster) %>% 
  filter(pooled_crim_wt > 0) %>% 
  mutate(CrimeArea = factor(CrimeArea, levels = c(-4, -1, 1, 2, 3, 4, 5),
                            labels = c(NA,
                                       NA,
                                       "A lot more",
                                       "A little more",
                                       "About the same",
                                       "A little less",
                                       "A lot less")))

### 3 - Survey proportions ----
sscq_year <- calculate_svymean(
  data = sscqyear,
  var_name = "CrimeArea",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_crim_wt")

prev_year1 <- calculate_svymean(
  data = prevyear1,
  var_name = "CrimeArea",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_crim_wt")

prev_year4 <- calculate_svymean(
  data = prevyear4,
  var_name = "CrimeArea",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_crim_wt")

prev_year5 <- calculate_svymean(
  data = prevyear5,
  var_name = "CrimeArea",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_crim_wt")


### 4 - Combining datasets ----
CrimeArea <- bind_rows(sscq_year, prev_year1, prev_year4, prev_year5)

Year <- c(config$sscqyear, config$sscqyear, config$sscqyear, config$sscqyear, config$sscqyear,
          config$prevyear1, config$prevyear1, config$prevyear1, config$prevyear1, config$prevyear1,
          config$prevyear4, config$prevyear4, config$prevyear4, config$prevyear4, config$prevyear4, 
          config$prevyear5, config$prevyear5, config$prevyear5, config$prevyear5, config$prevyear5)

CrimeArea$Year <- Year

CrimeArea_table <- CrimeArea %>% select(Year, CrimeArea, perc_with_ci)
CrimeArea_table


### 5 - Visualise estimates ----
CrimeArea_table <- CrimeArea_table %>% 
  group_by(Year) %>% 
  pivot_wider(
    names_from = CrimeArea,
    values_from = perc_with_ci)

print(CrimeArea_table)

saveRDS(CrimeArea_table, "Outputs/CrimeArea_table.rds")


# A lot more
CrimeArea1 <- graph_plot(
  data = CrimeArea,
  var_name = "CrimeArea",
  var_value = "A lot more",
  plot_title = "Perception of Local Crime Rate by Year (A lot more)")

print(CrimeArea1)

ggsave(filename = "Outputs/CrimeArea1.png", plot = CrimeArea1,
       width = 200, height = 120, units = "mm")

# A little more
CrimeArea2 <- graph_plot(
  data = CrimeArea,
  var_name = "CrimeArea",
  var_value = "A little more",
  plot_title = "Perception of Local Crime Rate by Year (A little more)")

print(CrimeArea2)

ggsave(filename = "Outputs/CrimeArea2.png", plot = CrimeArea2,
       width = 200, height = 120, units = "mm")

# About the same
CrimeArea3 <- graph_plot(
  data = CrimeArea,
  var_name = "CrimeArea",
  var_value = "About the same",
  plot_title = "Perception of Local Crime Rate by Year (About the same)")

print(CrimeArea3)

ggsave(filename = "Outputs/CrimeArea3.png", plot = CrimeArea3,
       width = 200, height = 120, units = "mm")

# A little less
CrimeArea4 <- graph_plot(
  data = CrimeArea,
  var_name = "CrimeArea",
  var_value = "A little less",
  plot_title = "Perception of Local Crime Rate by Year (A little less)")

print(CrimeArea4)

ggsave(filename = "Outputs/CrimeArea4.png", plot = CrimeArea4,
       width = 200, height = 120, units = "mm")

# A lot less
CrimeArea5 <- graph_plot(
  data = CrimeArea,
  var_name = "CrimeArea",
  var_value = "A lot less",
  plot_title = "Perception of Local Crime Rate by Year (A lot less)")

print(CrimeArea5)

ggsave(filename = "Outputs/CrimeArea5.png", plot = CrimeArea5,
       width = 200, height = 120, units = "mm")
