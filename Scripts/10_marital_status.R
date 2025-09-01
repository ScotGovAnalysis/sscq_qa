#########################################################################
# Name of file - 10_marital_status.R
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
  select(SSCQid, MaritalStatus, LA, pooled_ind_wt, cluster) %>% 
  filter(pooled_ind_wt > 0) %>% 
  mutate(MaritalStatus = factor(MaritalStatus, levels = c(-1, 1, 2, 3, 4, 5, 6, 7, 8, 9),
                                labels = c(NA,
                                           "Never married and never registered a same-sex civil partnership",
                                           "Married",
                                           "In a registered same-sex civil partnership",
                                           "Separated, but still legally married",
                                           "Separated, but still legally in a same-sex civil partnership",
                                           "Divorced",
                                           "Formerly in a same-sex civil partnership",
                                           "Widowed",
                                           "Surviving partner from a same-sex civil partnership")))

prevyear1 <- prevyear1 %>% 
  left_join(prevyear1.xref, by = "SSCQid") %>%
  select(SSCQid, MaritalStatus, LA, pooled_ind_wt, cluster) %>% 
  filter(pooled_ind_wt > 0) %>% 
  mutate(MaritalStatus = factor(MaritalStatus, levels = c(-1, 1, 2, 3, 4, 5, 6, 7, 8, 9),
                                labels = c(NA,
                                           "Never married and never registered a same-sex civil partnership",
                                           "Married",
                                           "In a registered same-sex civil partnership",
                                           "Separated, but still legally married",
                                           "Separated, but still legally in a same-sex civil partnership",
                                           "Divorced",
                                           "Formerly in a same-sex civil partnership",
                                           "Widowed",
                                           "Surviving partner from a same-sex civil partnership")))

prevyear2 <- prevyear2 %>% 
  left_join(prevyear2.xref, by = "SSCQid") %>%
  select(SSCQid, MaritalStatus, LA, pooled_ind_wt, cluster) %>% 
  filter(pooled_ind_wt > 0) %>% 
  mutate(MaritalStatus = factor(MaritalStatus, levels = c(-1, 1, 2, 3, 4, 5, 6, 7, 8, 9),
                                labels = c(NA,
                                           "Never married and never registered a same-sex civil partnership",
                                           "Married",
                                           "In a registered same-sex civil partnership",
                                           "Separated, but still legally married",
                                           "Separated, but still legally in a same-sex civil partnership",
                                           "Divorced",
                                           "Formerly in a same-sex civil partnership",
                                           "Widowed",
                                           "Surviving partner from a same-sex civil partnership")))

prevyear3 <- prevyear3 %>% 
  left_join(prevyear3.xref, by = "SSCQid") %>%
  select(SSCQid, MaritalStatus, LA, pooled_ind_wt, cluster) %>% 
  filter(pooled_ind_wt > 0) %>% 
  mutate(MaritalStatus = factor(MaritalStatus, levels = c(-1, 1, 2, 3, 4, 5, 6, 7, 8, 9),
                                labels = c(NA,
                                           "Never married and never registered a same-sex civil partnership",
                                           "Married",
                                           "In a registered same-sex civil partnership",
                                           "Separated, but still legally married",
                                           "Separated, but still legally in a same-sex civil partnership",
                                           "Divorced",
                                           "Formerly in a same-sex civil partnership",
                                           "Widowed",
                                           "Surviving partner from a same-sex civil partnership")))

### 3 - Survey proportions ----

sscq_year <- calculate_svymean(
  data = sscqyear,
  var_name = "MaritalStatus",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_ind_wt")

prev_year1 <- calculate_svymean(
  data = prevyear1,
  var_name = "MaritalStatus",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_ind_wt")

prev_year2 <- calculate_svymean(
  data = prevyear2,
  var_name = "MaritalStatus",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_ind_wt")

prev_year3 <- calculate_svymean(
  data = prevyear3,
  var_name = "MaritalStatus",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_ind_wt")


### 4 - Combining datasets ----
MaritalStatus <- bind_rows(sscq_year, prev_year1, prev_year2, prev_year3)

Year <- c(config$sscqyear, config$sscqyear, config$sscqyear, config$sscqyear, config$sscqyear, config$sscqyear, config$sscqyear, config$sscqyear, config$sscqyear,
          config$prevyear1, config$prevyear1, config$prevyear1, config$prevyear1, config$prevyear1, config$prevyear1, config$prevyear1, config$prevyear1, config$prevyear1, 
          config$prevyear2, config$prevyear2, config$prevyear2, config$prevyear2, config$prevyear2, config$prevyear2, config$prevyear2, config$prevyear2, config$prevyear2, 
          config$prevyear3, config$prevyear3, config$prevyear3, config$prevyear3, config$prevyear3, config$prevyear3, config$prevyear3, config$prevyear3, config$prevyear3)

MaritalStatus$Year <- Year

MaritalStatus_table <- MaritalStatus %>% select(Year, MaritalStatus, perc_with_ci)
MaritalStatus_table


### 5 - Visualise estimates ----
MaritalStatus_table <- MaritalStatus_table %>% 
  group_by(Year) %>% 
  pivot_wider(
    names_from = MaritalStatus,
    values_from = perc_with_ci)

print(MaritalStatus_table)

saveRDS(MaritalStatus_table, "Outputs/MaritalStatus_table.rds")


# Never married and never registered a same-sex civil partnership
MaritalStatus1 <- graph_plot(
  data = MaritalStatus,
  var_name = "MaritalStatus",
  var_value = "Never married and never registered a same-sex civil partnership",
  plot_title = "SSCQ Respondent Marital Status by Year (Never Married)")

print(MaritalStatus1)

ggsave(filename = "Outputs/MaritalStatus1.png", plot = MaritalStatus1,
       width = 200, height = 120, units = "mm")

# Married
MaritalStatus2 <- graph_plot(
  data = MaritalStatus,
  var_name = "MaritalStatus",
  var_value = "Married",
  plot_title = "SSCQ Respondent Marital Status by Year (Married)")

print(MaritalStatus2)

ggsave(filename = "Outputs/MaritalStatus2.png", plot = MaritalStatus2,
       width = 200, height = 120, units = "mm")

# In a registered same-sex civil partnership
MaritalStatus3 <- graph_plot(
  data = MaritalStatus,
  var_name = "MaritalStatus",
  var_value = "In a registered same-sex civil partnership",
  plot_title = "SSCQ Respondent Marital Status by Year (Same-sex civil partnership)")

print(MaritalStatus3)

ggsave(filename = "Outputs/MaritalStatus3.png", plot = MaritalStatus3,
       width = 200, height = 120, units = "mm")

# Separated, but still legally married
MaritalStatus4 <- graph_plot(
  data = MaritalStatus,
  var_name = "MaritalStatus",
  var_value = "Separated, but still legally married",
  plot_title = "SSCQ Respondent Marital Status by Year 
  (Separated, but still legally married)")

print(MaritalStatus4)

ggsave(filename = "Outputs/MaritalStatus4.png", plot = MaritalStatus4,
       width = 200, height = 120, units = "mm")

# Separated, but still legally in a same-sex civil partnership
MaritalStatus5 <- graph_plot(
  data = MaritalStatus,
  var_name = "MaritalStatus",
  var_value = "Separated, but still legally in a same-sex civil partnership",
  plot_title = "SSCQ Respondent Marital Status by Year 
  (Separated, but still legally in a same-sex civil partnership)")

print(MaritalStatus5)

ggsave(filename = "Outputs/MaritalStatus5.png", plot = MaritalStatus5,
       width = 200, height = 120, units = "mm")

# Divorced
MaritalStatus6 <- graph_plot(
  data = MaritalStatus,
  var_name = "MaritalStatus",
  var_value = "Divorced",
  plot_title = "SSCQ Respondent Marital Status by Year (Divorced)")

print(MaritalStatus6)

ggsave(filename = "Outputs/MaritalStatus6.png", plot = MaritalStatus6,
       width = 200, height = 120, units = "mm")

# Formerly in a same-sex civil partnership
MaritalStatus7 <- graph_plot(
  data = MaritalStatus,
  var_name = "MaritalStatus",
  var_value = "Formerly in a same-sex civil partnership",
  plot_title = "SSCQ Respondent Marital Status by Year 
  (Formerly in a same-sex civil partnership)")

print(MaritalStatus7)

ggsave(filename = "Outputs/MaritalStatus7.png", plot = MaritalStatus7,
       width = 200, height = 120, units = "mm")

# Widowed
MaritalStatus8 <- graph_plot(
  data = MaritalStatus,
  var_name = "MaritalStatus",
  var_value = "Widowed",
  plot_title = "SSCQ Respondent Marital Status by Year (Widowed)")

print(MaritalStatus8)

ggsave(filename = "Outputs/MaritalStatus8.png", plot = MaritalStatus8,
       width = 200, height = 120, units = "mm")

# Surviving partner from a same-sex civil partnership
MaritalStatus9 <- graph_plot(
  data = MaritalStatus,
  var_name = "MaritalStatus",
  var_value = "Surviving partner from a same-sex civil partnership",
  plot_title = "SSCQ Respondent Marital Status by Year 
  (Surviving partner from a same-sex civil partnership)")

print(MaritalStatus9)

ggsave(filename = "Outputs/MaritalStatus9.png", plot = MaritalStatus9,
       width = 200, height = 120, units = "mm")
