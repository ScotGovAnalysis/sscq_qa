#########################################################################
# Name of file - 02_age.R
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
  select(SSCQid, age, LA, pooled_ind_wt, cluster) %>% 
filter(pooled_ind_wt > 0) 

prevyear1 <- prevyear1 %>% 
  left_join(prevyear1.xref, by = "SSCQid") %>%
  select(SSCQid, age, LA, pooled_ind_wt, cluster) %>% 
filter(pooled_ind_wt > 0)

prevyear2 <- prevyear2 %>% 
  left_join(prevyear2.xref, by = "SSCQid") %>%
  select(SSCQid, age, LA, pooled_ind_wt, cluster) %>% 
filter(pooled_ind_wt > 0) 

prevyear3 <- prevyear3 %>% 
  left_join(prevyear3.xref, by = "SSCQid") %>%
  select(SSCQid, age, LA, pooled_ind_wt, cluster) %>% 
filter(pooled_ind_wt > 0) 


### 3 - Survey proportions ----
sscq_year <- calc_svymean_cont(
  data = sscqyear,
  target_var = "age",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_ind_wt")


prev_year1 <- calc_svymean_cont(
  data = prevyear1,
  target_var = "age",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_ind_wt")


prev_year2 <- calc_svymean_cont(
  data = prevyear2,
  target_var = "age",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_ind_wt")


prev_year3 <- calc_svymean_cont(
  data = prevyear3,
  target_var = "age",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_ind_wt")


### 4 - Combining datasets ----
Age <- bind_rows(sscq_year, prev_year1, prev_year2, prev_year3)

Year <- c(config$sscqyear,
          config$prevyear1,
          config$prevyear2,
          config$prevyear3)

Age$Year <- Year

Age_table <- Age %>% select(Year, mean_with_ci)
Age_table

Age_plot <- Age %>% select(Year, value, lower_ci, upper_ci)


### 5 - Visualise estimates ----

Age_table <- Age_table %>%
  select(Year, mean_with_ci) %>%
  group_by(Year) %>% 
  rename("Average SSCQ Respondent Age by Year" = mean_with_ci)

print(Age_table)

saveRDS(Age_table, "Outputs/Age_table.rds")


(Mean_Age <- ggplot(Age_plot, aes(x = Year, y = value)) +
    geom_point(fill = sg_colour_values["dark-blue"])+
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                  colour = sg_colour_values[4]) +
    labs(
      x = NULL,
      y = NULL,
      title = "Average SSCQ Respondent 
    Age by Year"))

ggsave(filename = "Outputs/Age.png", plot = Mean_Age,
       width = 200, height = 120, units = "mm")
