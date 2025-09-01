#########################################################################
# Name of file - 03_car_access.R
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
  select(SSCQid, CarAccess, LA, pooled_ind_wt, cluster) %>% 
  filter(pooled_ind_wt > 0) %>% 
  mutate(CarAccess = ifelse(CarAccess == 99, 0, CarAccess),
         CarAccess = factor(CarAccess, levels = c(0, 1, 2, 3),
                            labels = c("No car",
                                       "1 car",
                                       "2 cars",
                                       "3 or more cars")))

prevyear1 <- prevyear1 %>% 
  left_join(prevyear1.xref, by = "SSCQid") %>%
  select(SSCQid, CarAccess, LA, pooled_ind_wt, cluster) %>% 
  filter(pooled_ind_wt > 0) %>% 
  mutate(CarAccess = ifelse(CarAccess == 99, 0, CarAccess),
         CarAccess = factor(CarAccess, levels = c(0, 1, 2, 3),
                            labels = c("No car",
                                       "1 car",
                                       "2 cars",
                                       "3 or more cars")))

prevyear2 <- prevyear2 %>% 
  left_join(prevyear2.xref, by = "SSCQid") %>%
  select(SSCQid, CarAccess, LA, pooled_ind_wt, cluster) %>% 
  filter(pooled_ind_wt > 0) %>% 
  mutate(CarAccess = ifelse(CarAccess == 99, 0, CarAccess),
         CarAccess = factor(CarAccess, levels = c(0, 1, 2, 3),
                            labels = c("No car",
                                       "1 car",
                                       "2 cars",
                                       "3 or more cars")))

prevyear3 <- prevyear3 %>% 
  left_join(prevyear3.xref, by = "SSCQid") %>%
  select(SSCQid, CarAccess, LA, pooled_ind_wt, cluster) %>% 
  filter(pooled_ind_wt > 0) %>% 
  mutate(CarAccess = ifelse(CarAccess == 99, 0, CarAccess),
         CarAccess = factor(CarAccess, levels = c(0, 1, 2, 3),
                            labels = c("No car",
                                       "1 car",
                                       "2 cars",
                                       "3 or more cars")))


### 3 - Survey proportions ----
sscq_year <- calculate_svymean(
  data = sscqyear,
  var_name = "CarAccess",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_ind_wt")

prev_year1 <- calculate_svymean(
  data = prevyear1,
  var_name = "CarAccess",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_ind_wt")

prev_year2 <- calculate_svymean(
  data = prevyear2,
  var_name = "CarAccess",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_ind_wt")

prev_year3 <- calculate_svymean(
  data = prevyear3,
  var_name = "CarAccess",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_ind_wt")


### 4 - Combining datasets ----
Car_Access <- bind_rows(sscq_year, prev_year1, prev_year2, prev_year3)

Year <- c(config$sscqyear, config$sscqyear, config$sscqyear, config$sscqyear, 
          config$prevyear1, config$prevyear1, config$prevyear1, config$prevyear1, 
          config$prevyear2, config$prevyear2, config$prevyear2, config$prevyear2,  
          config$prevyear3, config$prevyear3, config$prevyear3, config$prevyear3)

Car_Access$Year <- Year

Car_Access_table <- Car_Access %>% select(Year, CarAccess, perc_with_ci)
Car_Access_table


### 5 - Visualise estimates ----
Car_Access_table <- Car_Access_table %>% 
  group_by(Year) %>% 
  pivot_wider(
    names_from = CarAccess,
    values_from = perc_with_ci)

print(Car_Access_table)

saveRDS(Car_Access_table, "Outputs/Car_Access_table.rds")

# No Car
CarAccess1 <- graph_plot(
  data = Car_Access,
  var_name = "CarAccess",
  var_value = "No car",
  plot_title = "SSCQ Respondent Car Access by Year (No Car)")

print(CarAccess1)

ggsave(filename = "Outputs/CarAccess1.png", plot = CarAccess1,
       width = 200, height = 120, units = "mm")


# 1 Car
CarAccess2 <- graph_plot(
  data = Car_Access,
  var_name = "CarAccess",
  var_value = "1 car",
  plot_title = "SSCQ Respondent Car Access by Year (1 Car)")

print(CarAccess2)

ggsave(filename = "Outputs/CarAccess2.png", plot = CarAccess2,
       width = 200, height = 120, units = "mm")


#2 Cars
CarAccess3 <- graph_plot(
  data = Car_Access,
  var_name = "CarAccess",
  var_value = "2 cars",
  plot_title = "SSCQ Respondent Car Access by Year (2 Cars)")

print(CarAccess3)

ggsave(filename = "Outputs/CarAccess3.png", plot = CarAccess3,
       width = 200, height = 120, units = "mm")


#3 Cars
CarAccess4 <- graph_plot(
  data = Car_Access,
  var_name = "CarAccess",
  var_value = "3 or more cars",
  plot_title = "SSCQ Respondent Car Access by Year (3 or More Cars)")

print(CarAccess4)

ggsave(filename = "Outputs/CarAccess4.png", plot = CarAccess4,
       width = 200, height = 120, units = "mm")
