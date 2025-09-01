#########################################################################
# Name of file - 07_general_health.R
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
  select(SSCQid, genhealth, LA, pooled_ind_wt, cluster) %>% 
  filter(pooled_ind_wt > 0) %>% 
  mutate(genhealth = factor(genhealth, levels = c(-1, 1, 2, 3, 4, 5),
                            labels = c(NA,
                                       "Very Good",
                                       "Good",
                                       "Fair",
                                       "Bad",
                                       "Very Bad")))

prevyear1 <- prevyear1 %>% 
  left_join(prevyear1.xref, by = "SSCQid") %>%
  select(SSCQid, genhealth, LA, pooled_ind_wt, cluster) %>% 
  filter(pooled_ind_wt > 0) %>% 
  mutate(genhealth = factor(genhealth, levels = c(-1, 1, 2, 3, 4, 5),
                            labels = c(NA,
                                       "Very Good",
                                       "Good",
                                       "Fair",
                                       "Bad",
                                       "Very Bad")))

prevyear2 <- prevyear2 %>% 
  left_join(prevyear2.xref, by = "SSCQid") %>%
  select(SSCQid, genhealth, LA, pooled_ind_wt, cluster) %>% 
  filter(pooled_ind_wt > 0) %>% 
  mutate(genhealth = factor(genhealth, levels = c(-1, 1, 2, 3, 4, 5),
                            labels = c(NA,
                                       "Very Good",
                                       "Good",
                                       "Fair",
                                       "Bad",
                                       "Very Bad")))

prevyear3 <- prevyear3 %>% 
  left_join(prevyear3.xref, by = "SSCQid") %>%
  select(SSCQid, genhealth, LA, pooled_ind_wt, cluster) %>% 
  filter(pooled_ind_wt > 0) %>% 
  mutate(genhealth = factor(genhealth, levels = c(-1, 1, 2, 3, 4, 5),
                            labels = c(NA,
                                       "Very Good",
                                       "Good",
                                       "Fair",
                                       "Bad",
                                       "Very Bad")))

### 3 - Survey proportions ----

sscq_year <- calculate_svymean(
  data = sscqyear,
  var_name = "genhealth",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_ind_wt")

prev_year1 <- calculate_svymean(
  data = prevyear1,
  var_name = "genhealth",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_ind_wt")

prev_year2 <- calculate_svymean(
  data = prevyear2,
  var_name = "genhealth",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_ind_wt")

prev_year3 <- calculate_svymean(
  data = prevyear3,
  var_name = "genhealth",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_ind_wt")


### 4 - Combining datasets ----
GenHealth <- bind_rows(sscq_year, prev_year1, prev_year2, prev_year3)

Year <- c(config$sscqyear, config$sscqyear, config$sscqyear, config$sscqyear, config$sscqyear,
          config$prevyear1, config$prevyear1, config$prevyear1, config$prevyear1, config$prevyear1,
          config$prevyear2, config$prevyear2, config$prevyear2, config$prevyear2, config$prevyear2,
          config$prevyear3, config$prevyear3, config$prevyear3, config$prevyear3, config$prevyear3)

GenHealth$Year <- Year

GenHealth_table <- GenHealth %>% select(Year, genhealth, perc_with_ci)
GenHealth_table


### 5 - Visualise estimates ----
GenHealth_table <- GenHealth_table %>% 
  group_by(Year) %>% 
  pivot_wider(
    names_from = genhealth,
    values_from = perc_with_ci)

print(GenHealth_table)

saveRDS(GenHealth_table, "Outputs/GenHealth_table.rds")


# Very Good
GenHealth1 <- graph_plot(
  data = GenHealth,
  var_name = "genhealth",
  var_value = "Very Good",
  plot_title = "SSCQ Respondent General Health by Year (Very Good)")

print(GenHealth1)

ggsave(filename = "Outputs/GenHealth1.png", plot = GenHealth1,
       width = 200, height = 120, units = "mm")


# Good
GenHealth2 <- graph_plot(
  data = GenHealth,
  var_name = "genhealth",
  var_value = "Good",
  plot_title = "SSCQ Respondent General Health by Year (Good)")

print(GenHealth2)

ggsave(filename = "Outputs/GenHealth2.png", plot = GenHealth2,
       width = 200, height = 120, units = "mm")


# Fair
GenHealth3 <- graph_plot(
  data = GenHealth,
  var_name = "genhealth",
  var_value = "Fair",
  plot_title = "SSCQ Respondent General Health by Year (Fair)")

print(GenHealth3)

ggsave(filename = "Outputs/GenHealth3.png", plot = GenHealth3,
       width = 200, height = 120, units = "mm")


# Bad
GenHealth4 <- graph_plot(
  data = GenHealth,
  var_name = "genhealth",
  var_value = "Bad",
  plot_title = "SSCQ Respondent General Health by Year (Bad)")

print(GenHealth4)

ggsave(filename = "Outputs/GenHealth4.png", plot = GenHealth4,
       width = 200, height = 120, units = "mm")


# Very Bad
GenHealth5 <- graph_plot(
  data = GenHealth,
  var_name = "genhealth",
  var_value = "Very Bad",
  plot_title = "SSCQ Respondent General Health by Year (Very Bad)")

print(GenHealth5)

ggsave(filename = "Outputs/GenHealth5.png", plot = GenHealth5,
       width = 200, height = 120, units = "mm")
