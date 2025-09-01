#########################################################################
# Name of file - 06_ethnic_group.R
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
  select(SSCQid, ethGroup, LA, pooled_ind_wt, cluster) %>% 
  filter(pooled_ind_wt > 0) %>% 
  mutate(ethSuperGroup = ifelse(ethGroup == 5, 3,
                                ifelse(ethGroup %in% c(3, 4, 6), 4, 
                                       ifelse(ethGroup %in% c(8:12), 5,
                                              ifelse(ethGroup == -1, 6,
                                                     ifelse(ethGroup == 1, 1,
                                                            ifelse(ethGroup == 2, 2, 6)))))),
         ethSuperGroup = factor(ethSuperGroup, levels = c(1:6),
                                labels = c("White: Scottish",
                                           "White: Other British",
                                           "White: Polish",
                                           "White: Other",
                                           "Asian",
                                           "All other ethnic groups")))

prevyear1 <- prevyear1 %>% 
  left_join(prevyear1.xref, by = "SSCQid") %>%
  select(SSCQid, ethGroup, LA, pooled_ind_wt, cluster) %>% 
  filter(pooled_ind_wt > 0) %>% 
  mutate(ethSuperGroup = ifelse(ethGroup == 5, 3,
                                ifelse(ethGroup %in% c(3, 4, 6), 4, 
                                       ifelse(ethGroup %in% c(8:12), 5,
                                              ifelse(ethGroup == -1, 6,
                                                     ifelse(ethGroup == 1, 1,
                                                            ifelse(ethGroup == 2, 2, 6)))))),
         ethSuperGroup = factor(ethSuperGroup, levels = c(1:6),
                                labels = c("White: Scottish",
                                           "White: Other British",
                                           "White: Polish",
                                           "White: Other",
                                           "Asian",
                                           "All other ethnic groups")))

prevyear2 <- prevyear2 %>% 
  left_join(prevyear2.xref, by = "SSCQid") %>%
  select(SSCQid, ethGroup, LA, pooled_ind_wt, cluster) %>% 
  filter(pooled_ind_wt > 0) %>% 
  mutate(ethSuperGroup = ifelse(ethGroup == 5, 3,
                                ifelse(ethGroup %in% c(3, 4, 6), 4, 
                                       ifelse(ethGroup %in% c(8:12), 5,
                                              ifelse(ethGroup == -1, 6,
                                                     ifelse(ethGroup == 1, 1,
                                                            ifelse(ethGroup == 2, 2, 6)))))),
         ethSuperGroup = factor(ethSuperGroup, levels = c(1:6),
                                labels = c("White: Scottish",
                                           "White: Other British",
                                           "White: Polish",
                                           "White: Other",
                                           "Asian",
                                           "All other ethnic groups")))

prevyear3 <- prevyear3 %>% 
  left_join(prevyear3.xref, by = "SSCQid") %>%
  select(SSCQid, ethGroup, LA, pooled_ind_wt, cluster) %>% 
  filter(pooled_ind_wt > 0) %>% 
  mutate(ethSuperGroup = ifelse(ethGroup == 5, 3,
                                ifelse(ethGroup %in% c(3, 4, 6), 4, 
                                       ifelse(ethGroup %in% c(8:12), 5,
                                              ifelse(ethGroup == -1, 6,
                                                     ifelse(ethGroup == 1, 1,
                                                            ifelse(ethGroup == 2, 2, 6)))))),
         ethSuperGroup = factor(ethSuperGroup, levels = c(1:6),
                                labels = c("White: Scottish",
                                           "White: Other British",
                                           "White: Polish",
                                           "White: Other",
                                           "Asian",
                                           "All other ethnic groups")))

### 3 - Survey proportions ----

sscq_year <- calculate_svymean(
  data = sscqyear,
  var_name = "ethSuperGroup",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_ind_wt")

prev_year1 <- calculate_svymean(
  data = prevyear1,
  var_name = "ethSuperGroup",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_ind_wt")

prev_year2 <- calculate_svymean(
  data = prevyear2,
  var_name = "ethSuperGroup",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_ind_wt")

prev_year3 <- calculate_svymean(
  data = prevyear3,
  var_name = "ethSuperGroup",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_ind_wt")


### 4 - Combining datasets ----
Ethnicity <- bind_rows(sscq_year, prev_year1, prev_year2, prev_year3)

Year <- c(config$sscqyear, config$sscqyear, config$sscqyear, config$sscqyear, config$sscqyear, config$sscqyear, 
          config$prevyear1, config$prevyear1, config$prevyear1, config$prevyear1, config$prevyear1, config$prevyear1,
          config$prevyear2, config$prevyear2, config$prevyear2, config$prevyear2, config$prevyear2, config$prevyear2, 
          config$prevyear3, config$prevyear3, config$prevyear3, config$prevyear3, config$prevyear3, config$prevyear3)

Ethnicity$Year <- Year

Ethnicity_table <- Ethnicity %>% select(Year, ethSuperGroup, perc_with_ci)
Ethnicity_table

### 5 - Visualise estimates ----
Ethnicity_table <- Ethnicity_table %>% 
  group_by(Year) %>% 
  pivot_wider(
    names_from = ethSuperGroup,
    values_from = perc_with_ci)

print(Ethnicity_table)

saveRDS(Ethnicity_table, "Outputs/Ethnicity_table.rds")


# White Scottish
Ethnicity1 <- graph_plot(
  data = Ethnicity,
  var_name = "ethSuperGroup",
  var_value = "White: Scottish",
  plot_title = "SSCQ Respondent Ethnicity by Year (White: Scottish)")

print(Ethnicity1)

ggsave(filename = "Outputs/Ethnicity1.png", plot = Ethnicity1,
       width = 200, height = 120, units = "mm")

# White: Other British
Ethnicity2 <- graph_plot(
  data = Ethnicity,
  var_name = "ethSuperGroup",
  var_value = "White: Other British",
  plot_title = "SSCQ Respondent Ethnicity by Year (White: Other British)")

print(Ethnicity2)

ggsave(filename = "Outputs/Ethnicity2.png", plot = Ethnicity2,
       width = 200, height = 120, units = "mm")

# White: Polish
Ethnicity3 <- graph_plot(
  data = Ethnicity,
  var_name = "ethSuperGroup",
  var_value = "White: Polish",
  plot_title = "SSCQ Respondent Ethnicity by Year (White: Polish)")

print(Ethnicity3)

ggsave(filename = "Outputs/Ethnicity3.png", plot = Ethnicity3,
       width = 200, height = 120, units = "mm")

# White: Other
Ethnicity4 <- graph_plot(
  data = Ethnicity,
  var_name = "ethSuperGroup",
  var_value = "White: Other",
  plot_title = "SSCQ Respondent Ethnicity by Year (White: Other)")

print(Ethnicity4)

ggsave(filename = "Outputs/Ethnicity4.png", plot = Ethnicity4,
       width = 200, height = 120, units = "mm")

# Asian
Ethnicity5 <- graph_plot(
  data = Ethnicity,
  var_name = "ethSuperGroup",
  var_value = "Asian",
  plot_title = "SSCQ Respondent Ethnicity by Year (Asian)")

print(Ethnicity5)

ggsave(filename = "Outputs/Ethnicity5.png", plot = Ethnicity5,
       width = 200, height = 120, units = "mm")

# All other ethnic groups
Ethnicity6 <- graph_plot(
  data = Ethnicity,
  var_name = "ethSuperGroup",
  var_value = "All other ethnic groups",
  plot_title = "SSCQ Respondent Ethnicity by Year (All other ethnic groups)")

print(Ethnicity6)

ggsave(filename = "Outputs/Ethnicity6.png", plot = Ethnicity6,
       width = 200, height = 120, units = "mm")
