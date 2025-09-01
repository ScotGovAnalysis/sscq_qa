#########################################################################
# Name of file - 21_sexual_orientation.R
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
  select(SSCQid, sexID, LA, pooled_ind_wt, cluster) %>% 
  filter(pooled_ind_wt > 0) %>% 
  mutate(sexID = ifelse(sexID %in% c(2:4), 2, 
                        ifelse(sexID %in% c(-1, 5, 9), NA, sexID)),
         sexID = factor(sexID, levels = c(-1, 1, 2),
                        labels = c(NA,
                                   "Heterosexual",
                                   "LGB+")))

prevyear1 <- prevyear1 %>% 
  left_join(prevyear1.xref, by = "SSCQid") %>%
  select(SSCQid, sexID, LA, pooled_ind_wt, cluster) %>% 
  filter(pooled_ind_wt > 0) %>% 
  mutate(sexID = ifelse(sexID %in% c(2:4), 2, 
                        ifelse(sexID %in% c(-1, 5, 9), NA, sexID)),
         sexID = factor(sexID, levels = c(-1, 1, 2),
                        labels = c(NA,
                                   "Heterosexual",
                                   "LGB+")))

prevyear2 <- prevyear2 %>% 
  left_join(prevyear2.xref, by = "SSCQid") %>%
  select(SSCQid, sexID, LA, pooled_ind_wt, cluster) %>% 
  filter(pooled_ind_wt > 0) %>% 
  mutate(sexID = ifelse(sexID %in% c(2:4), 2, 
                        ifelse(sexID %in% c(-1, 5, 9), NA, sexID)),
         sexID = factor(sexID, levels = c(-1, 1, 2),
                        labels = c(NA,
                                   "Heterosexual",
                                   "LGB+")))

prevyear3 <- prevyear3 %>% 
  left_join(prevyear3.xref, by = "SSCQid") %>%
  select(SSCQid, sexID, LA, pooled_ind_wt, cluster) %>% 
  filter(pooled_ind_wt > 0) %>% 
  mutate(sexID = ifelse(sexID %in% c(2:4), 2, 
                        ifelse(sexID %in% c(-1, 5, 9), NA, sexID)),
         sexID = factor(sexID, levels = c(-1, 1, 2),
                        labels = c(NA,
                                   "Heterosexual",
                                   "LGB+")))


### 3 - Survey proportions ----
sscq_year <- calculate_svymean(
  data = sscqyear,
  var_name = "sexID",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_ind_wt")

prev_year1 <- calculate_svymean(
  data = prevyear1,
  var_name = "sexID",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_ind_wt")

prev_year2 <- calculate_svymean(
  data = prevyear2,
  var_name = "sexID",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_ind_wt")

prev_year3 <- calculate_svymean(
  data = prevyear3,
  var_name = "sexID",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_ind_wt")



### 4 - Combining datasets ----

SexID <- bind_rows(sscq_year, prev_year1, prev_year2, prev_year3)

Year <- c(config$sscqyear, config$sscqyear,
          config$prevyear1, config$prevyear1,
          config$prevyear2, config$prevyear2,
          config$prevyear3, config$prevyear3)

SexID$Year <- Year

sexID_table <- SexID %>% select(Year, sexID, perc_with_ci)


### 5 - Visualise estimates ----

SexID_table <- sexID_table %>% 
  group_by(Year) %>% 
  pivot_wider(
    names_from = sexID,
    values_from = perc_with_ci)

saveRDS(SexID_table, "Outputs/SexID_table.rds")


# Heterosexual
SexID1 <- graph_plot(
  data = SexID,
  var_name = "sexID",
  var_value = "Heterosexual",
  plot_title = "SSCQ Respondent Sexual Orientation by Year (Heterosexual)")

print(SexID1)

ggsave(filename = "Outputs/sexID1.png", plot = SexID1,
       width = 200, height = 120, units = "mm")

# LGB+
SexID2 <- graph_plot(
  data = SexID,
  var_name = "sexID",
  var_value = "LGB+",
  plot_title = "SSCQ Respondent Sexual Orientation by Year (LGB+)")

print(SexID2)

ggsave(filename = "Outputs/sexID2.png", plot = SexID2,
       width = 200, height = 120, units = "mm")
