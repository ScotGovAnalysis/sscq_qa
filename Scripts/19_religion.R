#########################################################################
# Name of file - 19_religion.R
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
  select(SSCQid, Religion, LA, pooled_ind_wt, cluster) %>% 
  filter(pooled_ind_wt > 0) %>% 
  mutate(Religion = ifelse(Religion >= 6, 6, Religion),
         Religion = factor(Religion, levels = c(-1, 1:6),
                           labels = c(NA,
                                      "No religion",
                                      "Church of Scotland",
                                      "Roman Catholic",
                                      "Other Christian",
                                      "Muslim",
                                      "Other")))

prevyear1 <- prevyear1 %>% 
  left_join(prevyear1.xref, by = "SSCQid") %>%
  select(SSCQid, Religion, LA, pooled_ind_wt, cluster) %>% 
  filter(pooled_ind_wt > 0) %>% 
  mutate(Religion = ifelse(Religion >= 6, 6, Religion),
         Religion = factor(Religion, levels = c(-1, 1:6),
                           labels = c(NA,
                                      "No religion",
                                      "Church of Scotland",
                                      "Roman Catholic",
                                      "Other Christian",
                                      "Muslim",
                                      "Other")))
prevyear2 <- prevyear2 %>% 
  left_join(prevyear2.xref, by = "SSCQid") %>%
  select(SSCQid, Religion, LA, pooled_ind_wt, cluster) %>% 
  filter(pooled_ind_wt > 0) %>% 
  mutate(Religion = ifelse(Religion >= 6, 6, Religion),
         Religion = factor(Religion, levels = c(-1, 1:6),
                           labels = c(NA,
                                      "No religion",
                                      "Church of Scotland",
                                      "Roman Catholic",
                                      "Other Christian",
                                      "Muslim",
                                      "Other")))

prevyear3 <- prevyear3 %>% 
  left_join(prevyear3.xref, by = "SSCQid") %>%
  select(SSCQid, Religion, LA, pooled_ind_wt, cluster) %>% 
  filter(pooled_ind_wt > 0) %>% 
  mutate(Religion = ifelse(Religion >= 6, 6, Religion),
         Religion = factor(Religion, levels = c(-1, 1:6),
                           labels = c(NA,
                                      "No religion",
                                      "Church of Scotland",
                                      "Roman Catholic",
                                      "Other Christian",
                                      "Muslim",
                                      "Other")))


### 3 - Survey proportions ----
sscq_year <- calculate_svymean(
  data = sscqyear,
  var_name = "Religion",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_ind_wt")

prev_year1 <- calculate_svymean(
  data = prevyear1,
  var_name = "Religion",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_ind_wt")

prev_year2 <- calculate_svymean(
  data = prevyear2,
  var_name = "Religion",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_ind_wt")

prev_year3 <- calculate_svymean(
  data = prevyear3,
  var_name = "Religion",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_ind_wt")



### 4 - Combining datasets ----

Religion <- bind_rows(sscq_year, prev_year1, prev_year2, prev_year3)

Year <- c(config$sscqyear, config$sscqyear, config$sscqyear, config$sscqyear, config$sscqyear, config$sscqyear,
          config$prevyear1, config$prevyear1, config$prevyear1, config$prevyear1, config$prevyear1, config$prevyear1,
          config$prevyear2, config$prevyear2, config$prevyear2, config$prevyear2, config$prevyear2, config$prevyear2,
          config$prevyear3, config$prevyear3, config$prevyear3, config$prevyear3, config$prevyear3, config$prevyear3)

Religion$Year <- Year

Religion_table <- Religion %>% select(Year, Religion, perc_with_ci)
Religion_table


### 5 - Visualise estimates ----

Religion_table <- Religion_table %>% 
  group_by(Year) %>% 
  pivot_wider(
    names_from = Religion,
    values_from = perc_with_ci)

saveRDS(Religion_table, "Outputs/Religion_table.rds")


# No religion
Religion1 <- graph_plot(
  data = Religion,
  var_name = "Religion",
  var_value = "No religion",
  plot_title = "SSCQ Respondent Religion by Year (No Religion)")

print(Religion1)

ggsave(filename = "Outputs/Religion1.png", plot = Religion1,
       width = 200, height = 120, units = "mm")

#Church of Scotland
Religion2 <- graph_plot(
  data = Religion,
  var_name = "Religion",
  var_value = "Church of Scotland",
  plot_title = "SSCQ Respondent Religion by Year (Church of Scotland)")

print(Religion2)

ggsave(filename = "Outputs/Religion2.png", plot = Religion2,
       width = 200, height = 120, units = "mm")

# Roman Catholic
Religion3 <- graph_plot(
  data = Religion,
  var_name = "Religion",
  var_value = "Roman Catholic",
  plot_title = "SSCQ Respondent Religion by Year (Roman Catholic)")

print(Religion3)

ggsave(filename = "Outputs/Religion3.png", plot = Religion3,
       width = 200, height = 120, units = "mm")

# Other Christian
Religion4 <- graph_plot(
  data = Religion,
  var_name = "Religion",
  var_value = "Other Christian",
  plot_title = "SSCQ Respondent Religion by Year (Other Christian)")

print(Religion4)

ggsave(filename = "Outputs/Religion4.png", plot = Religion4,
       width = 200, height = 120, units = "mm")

# Muslim
Religion5 <- graph_plot(
  data = Religion,
  var_name = "Religion",
  var_value = "Muslim",
  plot_title = "SSCQ Respondent Religion by Year (Muslim)")

print(Religion5)

ggsave(filename = "Outputs/Religion5.png", plot = Religion5,
       width = 200, height = 120, units = "mm")

# Other
Religion6 <- graph_plot(
  data = Religion,
  var_name = "Religion",
  var_value = "Other",
  plot_title = "SSCQ Respondent Religion by Year (Other)")

print(Religion6)

ggsave(filename = "Outputs/Religion6.png", plot = Religion6,
       width = 200, height = 120, units = "mm")
