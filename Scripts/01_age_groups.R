#########################################################################
# Name of file - 01_age_groups.R
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
  select(SSCQid, ageG, LA, pooled_ind_wt, cluster) %>% 
  filter(pooled_ind_wt > 0) %>% 
  mutate(ageG = factor(ageG, levels = c(1, 2, 3, 4, 5, 6, 7),
                       labels = c("16-24",
                                  "25-34",
                                  "35-44",
                                  "45-54",
                                  "55-64",
                                  "65-74",
                                  "75+")))

prevyear1 <- prevyear1 %>% 
  left_join(prevyear1.xref, by = "SSCQid") %>%
  select(SSCQid, ageG, LA, pooled_ind_wt, cluster) %>% 
  filter(pooled_ind_wt > 0) %>% 
  mutate(ageG = factor(ageG, levels = c(1, 2, 3, 4, 5, 6, 7),
                        labels = c("16-24",
                                    "25-34",
                                    "35-44",
                                    "45-54",
                                    "55-64",
                                    "65-74",
                                    "75+")))

prevyear2 <- prevyear2 %>% 
  left_join(prevyear2.xref, by = "SSCQid") %>%
  select(SSCQid, ageG, LA, pooled_ind_wt, cluster) %>% 
  filter(pooled_ind_wt > 0) %>% 
  mutate(ageG = factor(ageG, levels = c(1, 2, 3, 4, 5, 6, 7),
                        labels = c("16-24",
                                    "25-34",
                                    "35-44",
                                    "45-54",
                                    "55-64",
                                    "65-74",
                                    "75+")))
  
prevyear3 <- prevyear3 %>% 
  left_join(prevyear3.xref, by = "SSCQid") %>%
  select(SSCQid, ageG, LA, pooled_ind_wt, cluster) %>% 
  filter(pooled_ind_wt > 0) %>% 
  mutate(ageG = factor(ageG, levels = c(1, 2, 3, 4, 5, 6, 7),
                        labels = c("16-24",
                                    "25-34",
                                    "35-44",
                                    "45-54",
                                    "55-64",
                                    "65-74",
                                    "75+")))
  

### 3 - Survey proportions ----
sscq_year <- calculate_svymean(
  data = sscqyear,
  var_name = "ageG",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_ind_wt")
  
prev_year1 <- calculate_svymean(
  data = prevyear1,
  var_name = "ageG",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_ind_wt")

prev_year2 <- calculate_svymean(
  data = prevyear2,
  var_name = "ageG",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_ind_wt")

prev_year3 <- calculate_svymean(
  data = prevyear3,
  var_name = "ageG",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_ind_wt")



### 4 - Combining datasets ----

AgeG <- bind_rows(sscq_year, prev_year1, prev_year2, prev_year3)

Year <- c(config$sscqyear, config$sscqyear, config$sscqyear, config$sscqyear, config$sscqyear, config$sscqyear, config$sscqyear,
          config$prevyear1, config$prevyear1, config$prevyear1, config$prevyear1, config$prevyear1, config$prevyear1, config$prevyear1,
          config$prevyear2, config$prevyear2, config$prevyear2, config$prevyear2, config$prevyear2, config$prevyear2, config$prevyear2,
          config$prevyear3, config$prevyear3, config$prevyear3, config$prevyear3, config$prevyear3, config$prevyear3, config$prevyear3)

AgeG$Year <- Year

AgeG_table <- AgeG %>% select(Year, ageG, perc_with_ci)
AgeG_table


### 5 - Visualise estimates ----

AgeG_table <- AgeG_table %>% 
  group_by(Year) %>% 
  pivot_wider(
    names_from = ageG,
    values_from = perc_with_ci)

print(AgeG_table)

saveRDS(AgeG_table, "Outputs/AgeG_table.rds")


#by each age group
#16-24
AgeG1 <- graph_plot(
  data = AgeG,
  var_name = "ageG",
  var_value = "16-24",
  plot_title = "SSCQ Respondent Age Group by Year (16-24)")

print(AgeG1)

ggsave(filename = "Outputs/AgeG1.png", plot = AgeG1,
       width = 200, height = 120, units = "mm")

#25-34
AgeG2 <- graph_plot(
  data = AgeG,
  var_name = "ageG",
  var_value = "25-34",
  plot_title = "SSCQ Respondent Age Group by Year (25-34)")

print(AgeG2)

ggsave(filename = "Outputs/AgeG2.png", plot = AgeG2,
       width = 200, height = 120, units = "mm")

#35-44
AgeG3 <- graph_plot(
  data = AgeG,
  var_name = "ageG",
  var_value = "35-44",
  plot_title = "SSCQ Respondent Age Group by Year (35-44)")

print(AgeG3)

ggsave(filename = "Outputs/AgeG3.png", plot = AgeG3,
       width = 200, height = 120, units = "mm")

#45-54
AgeG4 <- graph_plot(
  data = AgeG,
  var_name = "ageG",
  var_value = "45-54",
  plot_title = "SSCQ Respondent Age Group by Year (45-54)")

print(AgeG4)

ggsave(filename = "Outputs/AgeG4.png", plot = AgeG4,
       width = 200, height = 120, units = "mm")

#55-64
AgeG5 <- graph_plot(
  data = AgeG,
  var_name = "ageG",
  var_value = "55-64",
  plot_title = "SSCQ Respondent Age Group by Year (55-64)")

print(AgeG5)

ggsave(filename = "Outputs/AgeG5.png", plot = AgeG5,
       width = 200, height = 120, units = "mm")

#65-74
AgeG6 <- graph_plot(
  data = AgeG,
  var_name = "ageG",
  var_value = "65-74",
  plot_title = "SSCQ Respondent Age Group by Year (65-74)")

print(AgeG6)

ggsave(filename = "Outputs/AgeG6.png", plot = AgeG6,
       width = 200, height = 120, units = "mm")

#75+
AgeG7 <- graph_plot(
  data = AgeG,
  var_name = "ageG",
  var_value = "75+",
  plot_title = "SSCQ Respondent Age Group by Year (75+)")

print(AgeG7)

ggsave(filename = "Outputs/AgeG7.png", plot = AgeG7,
       width = 200, height = 120, units = "mm")
