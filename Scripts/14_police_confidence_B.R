#########################################################################
# Name of file - 14_police_confidence_B.R
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
  select(SSCQid, PolConB, LA, pooled_crim_wt, cluster) %>% 
  filter(pooled_crim_wt > 0) %>% 
  mutate(PolConB = ifelse(PolConB == 99, 0, PolConB),
         PolConB = factor(PolConB, levels = c(-1, 1, 2, 3, 4),
                          labels = c(NA,
                                     "Very confident",
                                     "Fairly confident",
                                     "Not very confident",
                                     "Not at all confident")))


prevyear1 <- prevyear1 %>% 
  left_join(prevyear1.xref, by = "SSCQid") %>%
  select(SSCQid, PolConB, LA, pooled_crim_wt, cluster) %>% 
  filter(pooled_crim_wt > 0) %>% 
  mutate(PolConB = ifelse(PolConB == 99, 0, PolConB),
         PolConB = factor(PolConB, levels = c(-1, 1, 2, 3, 4),
                          labels = c(NA,
                                     "Very confident",
                                     "Fairly confident",
                                     "Not very confident",
                                     "Not at all confident")))


prevyear4 <- prevyear4 %>% 
  left_join(prevyear4.xref, by = "SSCQid") %>%
  select(SSCQid, PolConB, LA, pooled_crim_wt, cluster) %>% 
  filter(pooled_crim_wt > 0) %>% 
  mutate(PolConB = ifelse(PolConB == 99, 0, PolConB),
         PolConB = factor(PolConB, levels = c(-1, 1, 2, 3, 4),
                          labels = c(NA,
                                     "Very confident",
                                     "Fairly confident",
                                     "Not very confident",
                                     "Not at all confident")))


prevyear5 <- prevyear5 %>% 
  left_join(prevyear5.xref, by = "SSCQid") %>%
  select(SSCQid, PolConB, LA, pooled_crim_wt, cluster) %>% 
  filter(pooled_crim_wt > 0) %>% 
  mutate(PolConB = ifelse(PolConB == 99, 0, PolConB),
         PolConB = factor(PolConB, levels = c(-1, 1, 2, 3, 4),
                          labels = c(NA,
                                     "Very confident",
                                     "Fairly confident",
                                     "Not very confident",
                                     "Not at all confident")))


### 3 - Survey proportions ----
sscq_year <- calculate_svymean(
  data = sscqyear,
  var_name = "PolConB",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_crim_wt")

prev_year1 <- calculate_svymean(
  data = prevyear1,
  var_name = "PolConB",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_crim_wt")

prev_year4 <- calculate_svymean(
  data = prevyear4,
  var_name = "PolConB",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_crim_wt")

prev_year5 <- calculate_svymean(
  data = prevyear5,
  var_name = "PolConB",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_crim_wt")


### 4 - Combining datasets ----
PolConB <- bind_rows(sscq_year, prev_year1, prev_year4, prev_year5)

Year <- c(config$sscqyear, config$sscqyear, config$sscqyear, config$sscqyear,
          config$prevyear1, config$prevyear1, config$prevyear1, config$prevyear1,
          config$prevyear4, config$prevyear4, config$prevyear4, config$prevyear4,  
          config$prevyear5, config$prevyear5, config$prevyear5, config$prevyear5)

PolConB$Year <- Year

PolConB_table <- PolConB %>% select(Year, PolConB, perc_with_ci)
PolConB_table


### 5 - Visualise estimates ----
PolConB_table <- PolConB_table %>% 
  group_by(Year) %>% 
  pivot_wider(
    names_from = PolConB,
    values_from = perc_with_ci)

saveRDS(PolConB_table, 
        "Outputs/PolConB_table.rds")


# Very confident
PolConB1 <- graph_plot(
  data = PolConB,
  var_name = "PolConB",
  var_value = "Very confident",
  plot_title = "Police Confidence B by Year (Very confident)")

print(PolConB1)

ggsave(filename = "Outputs/PolConB1.png", plot = PolConB1,
       width = 200, height = 120, units = "mm")


# Fairly confident
PolConB2 <- graph_plot(
  data = PolConB,
  var_name = "PolConB",
  var_value = "Fairly confident",
  plot_title = "Police Confidence B by Year (Fairly confident)")

print(PolConB2)

ggsave(filename = "Outputs/PolConB2.png", plot = PolConB2,
       width = 200, height = 120, units = "mm")


# Not very confident
PolConB3 <- graph_plot(
  data = PolConB,
  var_name = "PolConB",
  var_value = "Not very confident",
  plot_title = "Police Confidence B by Year (Not very confident)")

print(PolConB3)

ggsave(filename = "Outputs/PolConB3.png", plot = PolConB3,
       width = 200, height = 120, units = "mm")


# Not at all confident
PolConB4 <- graph_plot(
  data = PolConB,
  var_name = "PolConB",
  var_value = "Not at all confident",
  plot_title = "Police Confidence B by Year (Not at all confident)")

print(PolConB4)

ggsave(filename = "Outputs/PolConB4.png", plot = PolConB4,
       width = 200, height = 120, units = "mm")
