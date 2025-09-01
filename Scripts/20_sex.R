#########################################################################
# Name of file - 20_sex.R
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
  select(SSCQid, sex, LA, pooled_ind_wt, cluster) %>% 
  filter(pooled_ind_wt > 0) %>% 
  mutate(sex = factor(sex, levels = c(1, 2),
                      labels = c("Male",
                                 "Female")))

prevyear1 <- prevyear1 %>% 
  left_join(prevyear1.xref, by = "SSCQid") %>%
  select(SSCQid, sex, LA, pooled_ind_wt, cluster) %>% 
  filter(pooled_ind_wt > 0) %>% 
  mutate(sex = factor(sex, levels = c(1, 2),
                      labels = c("Male",
                                 "Female")))
prevyear2 <- prevyear2 %>% 
  left_join(prevyear2.xref, by = "SSCQid") %>%
  select(SSCQid, sex, LA, pooled_ind_wt, cluster) %>% 
  filter(pooled_ind_wt > 0) %>% 
  mutate(sex = factor(sex, levels = c(1, 2),
                      labels = c("Male",
                                 "Female")))

prevyear3 <- prevyear3 %>% 
  left_join(prevyear3.xref, by = "SSCQid") %>%
  select(SSCQid, sex, LA, pooled_ind_wt, cluster) %>% 
  filter(pooled_ind_wt > 0) %>% 
  mutate(sex = factor(sex, levels = c(1, 2),
                      labels = c("Male",
                                 "Female")))


### 3 - Survey proportions ----
sscq_year <- calculate_svymean(
  data = sscqyear,
  var_name = "sex",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_ind_wt")

prev_year1 <- calculate_svymean(
  data = prevyear1,
  var_name = "sex",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_ind_wt")

prev_year2 <- calculate_svymean(
  data = prevyear2,
  var_name = "sex",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_ind_wt")

prev_year3 <- calculate_svymean(
  data = prevyear3,
  var_name = "sex",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_ind_wt")



### 4 - Combining datasets ----

Sex <- bind_rows(sscq_year, prev_year1, prev_year2, prev_year3)

Year <- c(config$sscqyear, config$sscqyear,
          config$prevyear1, config$prevyear1,
          config$prevyear2, config$prevyear2,
          config$prevyear3, config$prevyear3)

Sex$Year <- Year

Sex_table <- Sex %>% select(Year, sex, perc_with_ci)
Sex_table

Sex_plot <- Sex %>% select(Year, sex, perc, lower_ci, upper_ci)


### 5 - Visualise estimates ----

Sex_table <- Sex_table %>% 
  group_by(Year) %>% 
  pivot_wider(
    names_from = sex,
    values_from = perc_with_ci)

saveRDS(Sex_table, "Outputs/Sex_table.rds")


# Male
Sex1 <- graph_plot(
  data = Sex,
  var_name = "sex",
  var_value = "Male",
  plot_title = "SSCQ Respondent Sex by Year (Male)")

print(Sex1)

ggsave(filename = "Outputs/Sex1.png", plot = Sex1,
       width = 200, height = 120, units = "mm")

# Female
Sex2 <- graph_plot(
  data = Sex,
  var_name = "sex",
  var_value = "Female",
  plot_title = "SSCQ Respondent Sex by Year (Female)")

print(Sex2)

ggsave(filename = "Outputs/Sex2.png", plot = Sex2,
       width = 200, height = 120, units = "mm")
