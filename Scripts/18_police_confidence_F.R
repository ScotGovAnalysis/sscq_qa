#########################################################################
# Name of file - 18_police_confidence_F.R
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
  select(SSCQid, PolConF, LA, pooled_crim_wt, cluster) %>% 
  filter(pooled_crim_wt > 0) %>% 
  mutate(PolConF = ifelse(PolConF == 99, 0, PolConF),
         PolConF = factor(PolConF, levels = c(-1, 1, 2, 3, 4),
                          labels = c(NA,
                                     "Very confident",
                                     "Fairly confident",
                                     "Not very confident",
                                     "Not at all confident")))


prevyear1 <- prevyear1 %>% 
  left_join(prevyear1.xref, by = "SSCQid") %>%
  select(SSCQid, PolConF, LA, pooled_crim_wt, cluster) %>% 
  filter(pooled_crim_wt > 0) %>% 
  mutate(PolConF = ifelse(PolConF == 99, 0, PolConF),
         PolConF = factor(PolConF, levels = c(-1, 1, 2, 3, 4),
                          labels = c(NA,
                                     "Very confident",
                                     "Fairly confident",
                                     "Not very confident",
                                     "Not at all confident")))


prevyear4 <- prevyear4 %>% 
  left_join(prevyear4.xref, by = "SSCQid") %>%
  select(SSCQid, PolConF, LA, pooled_crim_wt, cluster) %>% 
  filter(pooled_crim_wt > 0) %>% 
  mutate(PolConF = ifelse(PolConF == 99, 0, PolConF),
         PolConF = factor(PolConF, levels = c(-1, 1, 2, 3, 4),
                          labels = c(NA,
                                     "Very confident",
                                     "Fairly confident",
                                     "Not very confident",
                                     "Not at all confident")))


prevyear5 <- prevyear5 %>% 
  left_join(prevyear5.xref, by = "SSCQid") %>%
  select(SSCQid, PolConF, LA, pooled_crim_wt, cluster) %>% 
  filter(pooled_crim_wt > 0) %>% 
  mutate(PolConF = ifelse(PolConF == 99, 0, PolConF),
         PolConF = factor(PolConF, levels = c(-1, 1, 2, 3, 4),
                          labels = c(NA,
                                     "Very confident",
                                     "Fairly confident",
                                     "Not very confident",
                                     "Not at all confident")))


### 3 - Survey proportions ----
sscq_year <- calculate_svymean(
  data = sscqyear,
  var_name = "PolConF",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_crim_wt")

prev_year1 <- calculate_svymean(
  data = prevyear1,
  var_name = "PolConF",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_crim_wt")

prev_year4 <- calculate_svymean(
  data = prevyear4,
  var_name = "PolConF",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_crim_wt")

prev_year5 <- calculate_svymean(
  data = prevyear5,
  var_name = "PolConF",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_crim_wt")


### 4 - Combining datasets ----
PolConF <- bind_rows(sscq_year, prev_year1, prev_year4, prev_year5)

Year <- c(config$sscqyear, config$sscqyear, config$sscqyear, config$sscqyear,
          config$prevyear1, config$prevyear1, config$prevyear1, config$prevyear1,
          config$prevyear4, config$prevyear4, config$prevyear4, config$prevyear4,  
          config$prevyear5, config$prevyear5, config$prevyear5, config$prevyear5)

PolConF$Year <- Year

PolConF_table <- PolConF %>% select(Year, PolConF, perc_with_ci)
PolConF_table


### 5 - Visualise estimates ----
PolConF_table <- PolConF_table %>% 
  group_by(Year) %>% 
  pivot_wider(
    names_from = PolConF,
    values_from = perc_with_ci)

saveRDS(PolConF_table, 
        "Outputs/PolConF_table.rds")


# Very confident
PolConF1 <- graph_plot(
  data = PolConF,
  var_name = "PolConF",
  var_value = "Very confident",
  plot_title = "Police Confidence F by Year (Very confident)")

print(PolConF1)

ggsave(filename = "Outputs/PolConF1.png", plot = PolConF1,
       width = 200, height = 120, units = "mm")


# Fairly confident
PolConF2 <- graph_plot(
  data = PolConF,
  var_name = "PolConF",
  var_value = "Fairly confident",
  plot_title = "Police Confidence F by Year (Fairly confident)")

print(PolConF2)

ggsave(filename = "Outputs/PolConF2.png", plot = PolConF2,
       width = 200, height = 120, units = "mm")


# Not very confident
PolConF3 <- graph_plot(
  data = PolConF,
  var_name = "PolConF",
  var_value = "Not very confident",
  plot_title = "Police Confidence F by Year (Not very confident)")

print(PolConF3)

ggsave(filename = "Outputs/PolConF3.png", plot = PolConF3,
       width = 200, height = 120, units = "mm")


# Not at all confident
PolConF4 <- graph_plot(
  data = PolConF,
  var_name = "PolConF",
  var_value = "Not at all confident",
  plot_title = "Police Confidence F by Year (Not at all confident)")

print(PolConF4)

ggsave(filename = "Outputs/PolConF4.png", plot = PolConF4,
       width = 200, height = 120, units = "mm")
