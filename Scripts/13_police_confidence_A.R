#########################################################################
# Name of file - 13_police_confidence_A.R
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
  select(SSCQid, PolConA, LA, pooled_crim_wt, cluster) %>% 
  filter(pooled_crim_wt > 0) %>% 
  mutate(PolConA = ifelse(PolConA == 99, 0, PolConA),
         PolConA = factor(PolConA, levels = c(-1, 1, 2, 3, 4),
                          labels = c(NA,
                                     "Very confident",
                                     "Fairly confident",
                                     "Not very confident",
                                     "Not at all confident")))


prevyear1 <- prevyear1 %>% 
  left_join(prevyear1.xref, by = "SSCQid") %>%
  select(SSCQid, PolConA, LA, pooled_crim_wt, cluster) %>% 
  filter(pooled_crim_wt > 0) %>% 
  mutate(PolConA = ifelse(PolConA == 99, 0, PolConA),
         PolConA = factor(PolConA, levels = c(-1, 1, 2, 3, 4),
                          labels = c(NA,
                                     "Very confident",
                                     "Fairly confident",
                                     "Not very confident",
                                     "Not at all confident")))


prevyear4 <- prevyear4 %>% 
  left_join(prevyear4.xref, by = "SSCQid") %>%
  select(SSCQid, PolConA, LA, pooled_crim_wt, cluster) %>% 
  filter(pooled_crim_wt > 0) %>% 
  mutate(PolConA = ifelse(PolConA == 99, 0, PolConA),
         PolConA = factor(PolConA, levels = c(-1, 1, 2, 3, 4),
                          labels = c(NA,
                                     "Very confident",
                                     "Fairly confident",
                                     "Not very confident",
                                     "Not at all confident")))


prevyear5 <- prevyear5 %>% 
  left_join(prevyear5.xref, by = "SSCQid") %>%
  select(SSCQid, PolConA, LA, pooled_crim_wt, cluster) %>% 
  filter(pooled_crim_wt > 0) %>% 
  mutate(PolConA = ifelse(PolConA == 99, 0, PolConA),
         PolConA = factor(PolConA, levels = c(-1, 1, 2, 3, 4),
                          labels = c(NA,
                                     "Very confident",
                                     "Fairly confident",
                                     "Not very confident",
                                     "Not at all confident")))


### 3 - Survey proportions ----
sscq_year <- calculate_svymean(
  data = sscqyear,
  var_name = "PolConA",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_crim_wt")

prev_year1 <- calculate_svymean(
  data = prevyear1,
  var_name = "PolConA",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_crim_wt")

prev_year4 <- calculate_svymean(
  data = prevyear4,
  var_name = "PolConA",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_crim_wt")

prev_year5 <- calculate_svymean(
  data = prevyear5,
  var_name = "PolConA",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_crim_wt")


### 4 - Combining datasets ----
PolConA <- bind_rows(sscq_year, prev_year1, prev_year4, prev_year5)

Year <- c(config$sscqyear, config$sscqyear, config$sscqyear, config$sscqyear,
          config$prevyear1, config$prevyear1, config$prevyear1, config$prevyear1,
          config$prevyear4, config$prevyear4, config$prevyear4, config$prevyear4,  
          config$prevyear5, config$prevyear5, config$prevyear5, config$prevyear5)

PolConA$Year <- Year

PolConA_table <- PolConA %>% select(Year, PolConA, perc_with_ci)
PolConA_table


### 5 - Visualise estimates ----
PolConA_table <- PolConA_table %>% 
  group_by(Year) %>% 
  pivot_wider(
    names_from = PolConA,
    values_from = perc_with_ci)

saveRDS(PolConA_table, 
        "Outputs/PolConA_table.rds")


# Very confident
PolConA1 <- graph_plot(
  data = PolConA,
  var_name = "PolConA",
  var_value = "Very confident",
  plot_title = "Police Confidence A by Year (Very confident)")

print(PolConA1)

ggsave(filename = "Outputs/PolConA1.png", plot = PolConA1,
       width = 200, height = 120, units = "mm")


# Fairly confident
PolConA2 <- graph_plot(
  data = PolConA,
  var_name = "PolConA",
  var_value = "Fairly confident",
  plot_title = "Police Confidence A by Year (Fairly confident)")

print(PolConA2)

ggsave(filename = "Outputs/PolConA2.png", plot = PolConA2,
       width = 200, height = 120, units = "mm")


# Not very confident
PolConA3 <- graph_plot(
  data = PolConA,
  var_name = "PolConA",
  var_value = "Not very confident",
  plot_title = "Police Confidence A by Year (Not very confident)")

print(PolConA3)

ggsave(filename = "Outputs/PolConA3.png", plot = PolConA3,
       width = 200, height = 120, units = "mm")


# Not at all confident
PolConA4 <- graph_plot(
  data = PolConA,
  var_name = "PolConA",
  var_value = "Not at all confident",
  plot_title = "Police Confidence A by Year (Not at all confident)")

print(PolConA4)

ggsave(filename = "Outputs/PolConA4.png", plot = PolConA4,
       width = 200, height = 120, units = "mm")
