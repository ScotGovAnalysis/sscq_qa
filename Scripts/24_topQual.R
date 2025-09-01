#########################################################################
# Name of file - 24_topQual.R
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
  select(SSCQid, topQual, LA, pooled_ind_wt, cluster) %>% 
  filter(pooled_ind_wt > 0) %>% 
  mutate(topQual = factor(topQual, levels = c(-1, 1, 2, 3, 4, 5, 6, 7, 999),
                          labels = c(NA, 
                                     "Level 1 - O Grade, Standard grade or equiv (SVQ level 1 or 2)",
                                     "Level 2 - Higher, A level or equivalent (SVQ Level 3)",
                                     "Level 3 - HNC/HND or equivalent (SVQ Level 4)",
                                     "Level 4 - Degree, Professional qualification (Above SVQ Level 4)",
                                     "Other qualification",
                                     "No qualifications",
                                     "Qualifications not known",
                                     NA)))

prevyear1 <- prevyear1 %>% 
  left_join(prevyear1.xref, by = "SSCQid") %>%
  select(SSCQid, topQual, LA, pooled_ind_wt, cluster) %>% 
  filter(pooled_ind_wt > 0) %>% 
  mutate(topQual = factor(topQual, levels = c(-1, 1, 2, 3, 4, 5, 6, 7, 999),
                          labels = c(NA, 
                                     "Level 1 - O Grade, Standard grade or equiv (SVQ level 1 or 2)",
                                     "Level 2 - Higher, A level or equivalent (SVQ Level 3)",
                                     "Level 3 - HNC/HND or equivalent (SVQ Level 4)",
                                     "Level 4 - Degree, Professional qualification (Above SVQ Level 4)",
                                     "Other qualification",
                                     "No qualifications",
                                     "Qualifications not known",
                                     NA)))

prevyear2 <- prevyear2 %>% 
  left_join(prevyear2.xref, by = "SSCQid") %>%
  select(SSCQid, topQual, LA, pooled_ind_wt, cluster) %>% 
  filter(pooled_ind_wt > 0) %>% 
  mutate(topQual = factor(topQual, levels = c(-1, 1, 2, 3, 4, 5, 6, 7, 999),
                          labels = c(NA, 
                                     "Level 1 - O Grade, Standard grade or equiv (SVQ level 1 or 2)",
                                     "Level 2 - Higher, A level or equivalent (SVQ Level 3)",
                                     "Level 3 - HNC/HND or equivalent (SVQ Level 4)",
                                     "Level 4 - Degree, Professional qualification (Above SVQ Level 4)",
                                     "Other qualification",
                                     "No qualifications",
                                     "Qualifications not known",
                                     NA)))

prevyear3 <- prevyear3 %>% 
  left_join(prevyear3.xref, by = "SSCQid") %>%
  select(SSCQid, topQual, LA, pooled_ind_wt, cluster) %>% 
  filter(pooled_ind_wt > 0) %>% 
  mutate(topQual = factor(topQual, levels = c(-1, 1, 2, 3, 4, 5, 6, 7, 999),
                          labels = c(NA, 
                                     "Level 1 - O Grade, Standard grade or equiv (SVQ level 1 or 2)",
                                     "Level 2 - Higher, A level or equivalent (SVQ Level 3)",
                                     "Level 3 - HNC/HND or equivalent (SVQ Level 4)",
                                     "Level 4 - Degree, Professional qualification (Above SVQ Level 4)",
                                     "Other qualification",
                                     "No qualifications",
                                     "Qualifications not known",
                                     NA)))


### 3 - Survey proportions ----
sscq_year <- calculate_svymean(
  data = sscqyear,
  var_name = "topQual",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_ind_wt")

prev_year1 <- calculate_svymean(
  data = prevyear1,
  var_name = "topQual",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_ind_wt")

prev_year2 <- calculate_svymean(
  data = prevyear2,
  var_name = "topQual",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_ind_wt")

prev_year3 <- calculate_svymean(
  data = prevyear3,
  var_name = "topQual",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_ind_wt")



### 4 - Combining datasets ----

topQual <- bind_rows(sscq_year, prev_year1, prev_year2, prev_year3)

Year <- c(config$sscqyear, config$sscqyear, config$sscqyear, config$sscqyear, config$sscqyear, config$sscqyear, config$sscqyear,
          config$prevyear1, config$prevyear1, config$prevyear1, config$prevyear1, config$prevyear1, config$prevyear1, config$prevyear1,
          config$prevyear2, config$prevyear2, config$prevyear2, config$prevyear2, config$prevyear2, config$prevyear2, config$prevyear2,
          config$prevyear3, config$prevyear3, config$prevyear3, config$prevyear3, config$prevyear3, config$prevyear3, config$prevyear3)

topQual$Year <- Year

topQual_table <- topQual %>% select(Year, topQual, perc_with_ci)
topQual_table

### 5 - Visualise estimates ----

topQual_table <- topQual_table %>% 
  group_by(Year) %>% 
  pivot_wider(
    names_from = topQual,
    values_from = perc_with_ci)

print(topQual_table)

saveRDS(topQual_table, "Outputs/topQual_table.rds")


# Level 1
topQual1 <- graph_plot(
  data = topQual,
  var_name = "topQual",
  var_value = "Level 1 - O Grade, Standard grade or equiv (SVQ level 1 or 2)",
  plot_title = "SSCQ Respondent Top 
    Qualification by Year
    (Level 1)")

print(topQual1)

ggsave(filename = "Outputs/topQual1.png", plot = topQual1,
       width = 200, height = 120, units = "mm")

# Level 2
topQual2 <- graph_plot(
  data = topQual,
  var_name = "topQual",
  var_value = "Level 2 - Higher, A level or equivalent (SVQ Level 3)",
  plot_title = "SSCQ Respondent Top 
    Qualification by Year 
    (Level 2)")

print(topQual2)

ggsave(filename = "Outputs/topQual2.png", plot = topQual2,
       width = 200, height = 120, units = "mm")

# Level 3
topQual3 <- graph_plot(
  data = topQual,
  var_name = "topQual",
  var_value = "Level 3 - HNC/HND or equivalent (SVQ Level 4)",
  plot_title = "SSCQ Respondent Top 
    Qualification by Year 
    (Level 3)")

print(topQual3)

ggsave(filename = "Outputs/topQual3.png", plot = topQual3,
       width = 200, height = 120, units = "mm")

# Level 4
topQual4 <- graph_plot(
  data = topQual,
  var_name = "topQual",
  var_value = "Level 4 - Degree, Professional qualification (Above SVQ Level 4)",
  plot_title = "SSCQ Respondent Top 
    Qualification by Year 
    (Level 4)")

print(topQual4)

ggsave(filename = "Outputs/topQual4.png", plot = topQual4,
       width = 200, height = 120, units = "mm")

# Other qualifications
topQual5 <- graph_plot(
  data = topQual,
  var_name = "topQual",
  var_value = "Other qualification",
  plot_title = "SSCQ Respondent Top 
    Qualification by Year 
    (Other qualifications)")

print(topQual5)

ggsave(filename = "Outputs/topQual5.png", plot = topQual5,
       width = 200, height = 120, units = "mm")

# no qualifications
topQual6 <- graph_plot(
  data = topQual,
  var_name = "topQual",
  var_value = "No qualifications",
  plot_title = "SSCQ Respondent Top 
    Qualification by Year 
    (No qualifications)")

print(topQual6)

ggsave(filename = "Outputs/topQual6.png", plot = topQual6,
       width = 200, height = 120, units = "mm")

# Qualifications not known
topQual7 <- graph_plot(
  data = topQual,
  var_name = "topQual",
  var_value = "Qualifications not known",
  plot_title = "SSCQ Respondent Top 
    Qualification by Year 
    (Qualifications not known)")

print(topQual7)

ggsave(filename = "Outputs/topQual7.png", plot = topQual7,
       width = 200, height = 120, units = "mm")
