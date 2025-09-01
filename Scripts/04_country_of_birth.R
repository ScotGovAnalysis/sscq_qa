#########################################################################
# Name of file - 04_country_of_birth.R
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
  select(SSCQid, BirthCountry, LA, pooled_ind_wt, cluster) %>% 
  filter(pooled_ind_wt > 0) %>% 
  mutate(BirthCountry = ifelse(BirthCountry %in% c(-2, -1, 0, 1001), -1,
                               ifelse(BirthCountry %in% c(40, 56, 100, 191, 203, 208, 233, 246, 276, 300, 348, 372, 380, 428, 440, 442, 470, 528, 616, 620, 642, 703, 705, 752, 901, 911, 913, 981), 3,
                                      ifelse(BirthCountry %in% c(921, 922, 924, 925, 926), 2,
                                             ifelse(BirthCountry %in% c(923), 1,
                                                    ifelse(BirthCountry %in% c(4:9999), 4, NA))))),
         BirthCountry = factor(BirthCountry, levels = c(-1, 1, 2, 3, 4),
                               labels = c(NA,
                                          "Scotland",
                                          "Rest of UK",
                                          "EU27",
                                          "Rest of World")))

prevyear1 <- prevyear1 %>% 
  left_join(prevyear1.xref, by = "SSCQid") %>%
  select(SSCQid, BirthCountry, LA, pooled_ind_wt, cluster) %>% 
  filter(pooled_ind_wt > 0) %>% 
  mutate(BirthCountry = ifelse(BirthCountry %in% c(-2, -1, 0, 1001), -1,
                               ifelse(BirthCountry %in% c(40, 56, 100, 191, 203, 208, 233, 246, 276, 300, 348, 372, 380, 428, 440, 442, 470, 528, 616, 620, 642, 703, 705, 752, 901, 911, 913, 981), 3,
                                      ifelse(BirthCountry %in% c(921, 922, 924, 925, 926), 2,
                                             ifelse(BirthCountry %in% c(923), 1,
                                                    ifelse(BirthCountry %in% c(4:9999), 4, NA))))),
         BirthCountry = factor(BirthCountry, levels = c(-1, 1, 2, 3, 4),
                               labels = c(NA,
                                          "Scotland",
                                          "Rest of UK",
                                          "EU27",
                                          "Rest of World")))

prevyear2 <- prevyear2 %>% 
  left_join(prevyear2.xref, by = "SSCQid") %>%
  select(SSCQid, BirthCountry, LA, pooled_ind_wt, cluster) %>% 
  filter(pooled_ind_wt > 0) %>% 
  mutate(BirthCountry = ifelse(BirthCountry %in% c(-2, -1, 0, 1001), -1,
                               ifelse(BirthCountry %in% c(40, 56, 100, 191, 203, 208, 233, 246, 276, 300, 348, 372, 380, 428, 440, 442, 470, 528, 616, 620, 642, 703, 705, 752, 901, 911, 913, 981), 3,
                                      ifelse(BirthCountry %in% c(921, 922, 924, 925, 926), 2,
                                             ifelse(BirthCountry %in% c(923), 1,
                                                    ifelse(BirthCountry %in% c(4:9999), 4, NA))))),
         BirthCountry = factor(BirthCountry, levels = c(-1, 1, 2, 3, 4),
                               labels = c(NA,
                                          "Scotland",
                                          "Rest of UK",
                                          "EU27",
                                          "Rest of World")))

prevyear3 <- prevyear3 %>% 
  left_join(prevyear3.xref, by = "SSCQid") %>%
  select(SSCQid, BirthCountry, LA, pooled_ind_wt, cluster) %>% 
  filter(pooled_ind_wt > 0) %>% 
  mutate(BirthCountry = ifelse(BirthCountry %in% c(-2, -1, 0, 1001), -1,
                               ifelse(BirthCountry %in% c(40, 56, 100, 191, 203, 208, 233, 246, 276, 300, 348, 372, 380, 428, 440, 442, 470, 528, 616, 620, 642, 703, 705, 752, 901, 911, 913, 981), 3,
                                      ifelse(BirthCountry %in% c(921, 922, 924, 925, 926), 2,
                                             ifelse(BirthCountry %in% c(923), 1,
                                                    ifelse(BirthCountry %in% c(4:9999), 4, NA))))),
         BirthCountry = factor(BirthCountry, levels = c(-1, 1, 2, 3, 4),
                               labels = c(NA,
                                          "Scotland",
                                          "Rest of UK",
                                          "EU27",
                                          "Rest of World")))


### 3 - Survey proportions ----

sscq_year <- calculate_svymean(
  data = sscqyear,
  var_name = "BirthCountry",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_ind_wt")

prev_year1 <- calculate_svymean(
  data = prevyear1,
  var_name = "BirthCountry",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_ind_wt")

prev_year2 <- calculate_svymean(
  data = prevyear2,
  var_name = "BirthCountry",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_ind_wt")

prev_year3 <- calculate_svymean(
  data = prevyear3,
  var_name = "BirthCountry",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_ind_wt")


### 4 - Combining datasets ----
CoB <- bind_rows(sscq_year, prev_year1, prev_year2, prev_year3)

Year <- c(config$sscqyear, config$sscqyear, config$sscqyear, config$sscqyear, 
          config$prevyear1, config$prevyear1, config$prevyear1, config$prevyear1, 
          config$prevyear2, config$prevyear2, config$prevyear2, config$prevyear2,  
          config$prevyear3, config$prevyear3, config$prevyear3, config$prevyear3)

CoB$Year <- Year

CoB_table <- CoB %>% select(Year, BirthCountry, perc_with_ci)
CoB_table

### 5 - Visualise estimates ----
CoB_table <- CoB_table %>% 
  group_by(Year) %>% 
  pivot_wider(
    names_from = BirthCountry,
    values_from = perc_with_ci)

print(CoB_table)

saveRDS(CoB_table, "Outputs/CoB_table.rds")


# Scotland
CoB1 <- graph_plot(
  data = CoB,
  var_name = "BirthCountry",
  var_value = "Scotland",
  plot_title = "SSCQ Respondent Birth Country by Year (Scotland)")

print(CoB1)

ggsave(filename = "Outputs/CoB1.png", plot = CoB1,
       width = 200, height = 120, units = "mm")


# Rest of the UK
CoB2 <- graph_plot(
  data = CoB,
  var_name = "BirthCountry",
  var_value = "Rest of UK",
  plot_title = "SSCQ Respondent Birth Country by Year (Rest of UK)")

print(CoB2)

ggsave(filename = "Outputs/CoB2.png", plot = CoB2,
       width = 200, height = 120, units = "mm")


# EU27
CoB3 <- graph_plot(
  data = CoB,
  var_name = "BirthCountry",
  var_value = "EU27",
  plot_title = "SSCQ Respondent Birth Country by Year (EU27)")

print(CoB3)

ggsave(filename = "Outputs/CoB3.png", plot = CoB3,
       width = 200, height = 120, units = "mm")


# Rest of the World
CoB4 <- graph_plot(
  data = CoB,
  var_name = "BirthCountry",
  var_value = "Rest of World",
  plot_title = "SSCQ Respondent Birth Country by Year (Rest of World)")

print(CoB4)

ggsave(filename = "Outputs/CoB4.png", plot = CoB4,
       width = 200, height = 120, units = "mm")
