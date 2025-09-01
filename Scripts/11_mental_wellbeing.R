#########################################################################
# Name of file - 11_mental_wellbeing.R
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
  select(SSCQid, swemwbs, LA, pooled_ind_wt, cluster) %>% 
  filter(pooled_ind_wt > 0)%>%
  mutate(across(where(is.numeric), ~ ifelse(. == -1, NA, .))) 

prevyear1 <- prevyear1 %>% 
  left_join(prevyear1.xref, by = "SSCQid") %>%
  select(SSCQid, swemwbs, LA, pooled_ind_wt, cluster) %>% 
  filter(pooled_ind_wt > 0)%>%
  mutate(across(where(is.numeric), ~ ifelse(. == -1, NA, .)))

prevyear4 <- prevyear4 %>% 
  left_join(prevyear4.xref, by = "SSCQid") %>%
  select(SSCQid, swemwbs, LA, pooled_ind_wt, cluster) %>% 
  filter(pooled_ind_wt > 0) %>%
  mutate(across(where(is.numeric), ~ ifelse(. == -1, NA, .)))

prevyear5 <- prevyear5 %>% 
  left_join(prevyear5.xref, by = "SSCQid") %>%
  select(SSCQid, swemwbs, LA, pooled_ind_wt, cluster) %>% 
  filter(pooled_ind_wt > 0) %>%
  mutate(across(where(is.numeric), ~ ifelse(. == -1, NA, .)))


### 3 - Survey proportions ----
sscq_year <- calc_svymean_cont(
  data = sscqyear,
  target_var = "swemwbs",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_ind_wt")


prev_year1 <- calc_svymean_cont(
  data = prevyear1,
  target_var = "swemwbs",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_ind_wt")


prev_year4 <- calc_svymean_cont(
  data = prevyear4,
  target_var = "swemwbs",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_ind_wt")


prev_year5 <- calc_svymean_cont(
  data = prevyear5,
  target_var = "swemwbs",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_ind_wt")


### 4 - Combining datasets ----
swemwbs <- bind_rows(sscq_year, prev_year1, prev_year4, prev_year5)

Year <- c(config$sscqyear,
          config$prevyear1,
          config$prevyear4,
          config$prevyear5)

swemwbs$Year <- Year

swemwbs_table <- swemwbs %>% select(Year, mean_with_ci)
swemwbs_table

swemwbs_plot <- swemwbs %>% select(Year, value, lower_ci, upper_ci)


### 5 - Visualise estimates ----
swemwbs_table <- swemwbs_table %>%
  select(Year, mean_with_ci) %>%
  group_by(Year) %>% 
  rename("Average SSCQ Respondent Mental Wellbeing Score by Year" = mean_with_ci)

print(swemwbs_table)

saveRDS(swemwbs_table, "Outputs/swemwbs_table.rds")


(swemwbs_plot <- ggplot(swemwbs_plot, aes(x = Year, y = value)) +
    geom_point(fill = sg_colour_values["dark-blue"])+
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                  colour = sg_colour_values[4]) +
    labs(
      x = NULL,
      y = NULL,
      title = "Average SSCQ Respondent Mental 
    Wellbeing Score by Year"))

ggsave(filename = "Outputs/swemwbs.png", plot = swemwbs_plot,
       width = 200, height = 120, units = "mm")
