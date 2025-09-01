#########################################################################
# Name of file - 08_household_type.R
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
  select(SSCQid, htype2a, LA, pooled_hh_wt, cluster) %>% 
  filter(pooled_hh_wt > 0) %>% 
  mutate(htype2a = factor(htype2a, levels = c(-1, 1, 2, 3, 4, 5, 6, 7, 8),
                          labels = c(NA,
                                     "Single Adult",
                                     "Small Adult",
                                     "Large Adult",
                                     "Single Parent",
                                     "Small Family",
                                     "Large Family",
                                     "Single Pensioner",
                                     "Older Couple")))
prevyear1 <- prevyear1 %>% 
  left_join(prevyear1.xref, by = "SSCQid") %>%
  select(SSCQid, htype2a, LA, pooled_hh_wt, cluster) %>% 
  filter(pooled_hh_wt > 0) %>% 
  mutate(htype2a = factor(htype2a, levels = c(-1, 1, 2, 3, 4, 5, 6, 7, 8),
                          labels = c(NA,
                                     "Single Adult",
                                     "Small Adult",
                                     "Large Adult",
                                     "Single Parent",
                                     "Small Family",
                                     "Large Family",
                                     "Single Pensioner",
                                     "Older Couple")))

prevyear2 <- prevyear2 %>% 
  left_join(prevyear2.xref, by = "SSCQid") %>%
  select(SSCQid, htype2a, LA, pooled_hh_wt, cluster) %>% 
  filter(pooled_hh_wt > 0) %>% 
  mutate(htype2a = factor(htype2a, levels = c(-1, 1, 2, 3, 4, 5, 6, 7, 8),
                          labels = c(NA,
                                     "Single Adult",
                                     "Small Adult",
                                     "Large Adult",
                                     "Single Parent",
                                     "Small Family",
                                     "Large Family",
                                     "Single Pensioner",
                                     "Older Couple")))

prevyear3 <- prevyear3 %>% 
  left_join(prevyear3.xref, by = "SSCQid") %>%
  select(SSCQid, htype2a, LA, pooled_hh_wt, cluster) %>% 
  filter(pooled_hh_wt > 0) %>% 
  mutate(htype2a = factor(htype2a, levels = c(-1, 1, 2, 3, 4, 5, 6, 7, 8),
                          labels = c(NA,
                                     "Single Adult",
                                     "Small Adult",
                                     "Large Adult",
                                     "Single Parent",
                                     "Small Family",
                                     "Large Family",
                                     "Single Pensioner",
                                     "Older Couple")))


### 3 - Survey proportions ----

sscq_year <- calculate_svymean(
  data = sscqyear,
  var_name = "htype2a",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_hh_wt")

prev_year1 <- calculate_svymean(
  data = prevyear1,
  var_name = "htype2a",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_hh_wt")

prev_year2 <- calculate_svymean(
  data = prevyear2,
  var_name = "htype2a",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_hh_wt")

prev_year3 <- calculate_svymean(
  data = prevyear3,
  var_name = "htype2a",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_hh_wt")


### 4 - Combining datasets ----
HType <- bind_rows(sscq_year, prev_year1, prev_year2, prev_year3)

Year <- c(config$sscqyear, config$sscqyear, config$sscqyear, config$sscqyear, config$sscqyear, config$sscqyear, config$sscqyear, config$sscqyear,
          config$prevyear1, config$prevyear1, config$prevyear1, config$prevyear1, config$prevyear1, config$prevyear1, config$prevyear1, config$prevyear1,
          config$prevyear2, config$prevyear2, config$prevyear2, config$prevyear2, config$prevyear2, config$prevyear2, config$prevyear2, config$prevyear2,
          config$prevyear3, config$prevyear3, config$prevyear3, config$prevyear3, config$prevyear3, config$prevyear3, config$prevyear3, config$prevyear3)

HType$Year <- Year

HType_table <- HType %>% select(Year, htype2a, perc_with_ci)
HType_table

### 5 - Visualise estimates ----
HType_table <- HType_table %>% 
  group_by(Year) %>% 
  pivot_wider(
    names_from = htype2a,
    values_from = perc_with_ci)

print(HType_table)

saveRDS(HType_table, "Outputs/HType_table.rds")


# Single Adult
HType1 <- graph_plot(
  data = HType,
  var_name = "htype2a",
  var_value = "Single Adult",
  plot_title = "SSCQ Respondent Household Type by Year (Single Adult)")

print(HType1)

ggsave(filename = "Outputs/HType1.png", plot = HType1,
       width = 200, height = 120, units = "mm")

# Small Adult
HType2 <- graph_plot(
  data = HType,
  var_name = "htype2a",
  var_value = "Small Adult",
  plot_title = "SSCQ Respondent Household Type by Year (Small Adult)")

print(HType2)

ggsave(filename = "Outputs/HType2.png", plot = HType2,
       width = 200, height = 120, units = "mm")

# Large Adult
HType3 <- graph_plot(
  data = HType,
  var_name = "htype2a",
  var_value = "Large Adult",
  plot_title = "SSCQ Respondent Household Type by Year (Large Adult)")

print(HType3)

ggsave(filename = "Outputs/HType3.png", plot = HType3,
       width = 200, height = 120, units = "mm")

# Single Parent
HType4 <- graph_plot(
  data = HType,
  var_name = "htype2a",
  var_value = "Single Parent",
  plot_title = "SSCQ Respondent Household Type by Year (Single Parent)")

print(HType4)

ggsave(filename = "Outputs/HType4.png", plot = HType4,
       width = 200, height = 120, units = "mm")

# Small Family
HType5 <- graph_plot(
  data = HType,
  var_name = "htype2a",
  var_value = "Small Family",
  plot_title = "SSCQ Respondent Household Type by Year (Small Family)")

print(HType5)

ggsave(filename = "Outputs/HType5.png", plot = HType5,
       width = 200, height = 120, units = "mm")

# Large Family
HType6 <- graph_plot(
  data = HType,
  var_name = "htype2a",
  var_value = "Large Family",
  plot_title = "SSCQ Respondent Household Type by Year (Large Family)")

print(HType6)

ggsave(filename = "Outputs/HType6.png", plot = HType6,
       width = 200, height = 120, units = "mm")

# Single Pensioner
HType7 <- graph_plot(
  data = HType,
  var_name = "htype2a",
  var_value = "Single Pensioner",
  plot_title = "SSCQ Respondent Household Type by Year (Single Pensioner)")

print(HType7)

ggsave(filename = "Outputs/HType7.png", plot = HType7,
       width = 200, height = 120, units = "mm")

# Older Couple
HType8 <- graph_plot(
  data = HType,
  var_name = "htype2a",
  var_value = "Older Couple",
  plot_title = "SSCQ Respondent Household Type by Year (Older Couple)")

print(HType8)

ggsave(filename = "Outputs/HType8.png", plot = HType8,
       width = 200, height = 120, units = "mm")
