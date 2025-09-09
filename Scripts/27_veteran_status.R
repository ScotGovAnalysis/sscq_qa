#########################################################################
# Name of file - 27_veteran_status.R
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
#prevyear2 <- haven::read_sas(config$prevyear2.path)
# Path to file with cluster info 
#prevyear2.xref <- haven::read_sas(config$prevyear2.xref)


# Path to previous SSCQ data
#prevyear3 <- haven::read_sas(config$prevyear3.path)
# Path to file with cluster info 
#prevyear3.xref <- haven::read_sas(config$prevyear3.xref)


### 2 - Merge data frames and recode variables ----

sscqyear <- sscqyear %>% 
  left_join(sscqxref, by = "SSCQid") %>%
  select(SSCQid, Vets, LA, pooled_ind_wt, cluster) %>% 
  filter(pooled_ind_wt > 0) %>% 
  mutate(Vets = ifelse(Vets == -1, NA, Vets),
         Vets = factor(Vets, levels = c(-1, 0, 1),
                       labels = c(NA,
                                  "Not a UK Armed Forces veteran",
                                  "UK Armed Forces veteran")))

prevyear1 <- prevyear1 %>% 
  left_join(prevyear1.xref, by = "SSCQid") %>%
  select(SSCQid, Vets, LA, pooled_ind_wt, cluster) %>% 
  filter(pooled_ind_wt > 0) %>% 
  mutate(Vets = ifelse(Vets == -1, NA, Vets),
         Vets = factor(Vets, levels = c(-1, 0, 1),
                       labels = c(NA,
                                  "Not a UK Armed Forces veteran",
                                  "UK Armed Forces veteran")))

#prevyear2 <- prevyear2 %>% 
 # left_join(prevyear2.xref, by = "SSCQid") %>%
  #select(SSCQid, Vets, LA, pooled_ind_wt, cluster) %>% 
  #filter(pooled_ind_wt > 0) %>% 
  #mutate(Vets = ifelse(Vets == -1, NA, Vets),
   #      Vets = factor(Vets, levels = c(-1, 0, 1),
    #                   labels = c(NA,
     #                             "Not a UK Armed Forces veteran",
      #                            "UK Armed Forces veteran")))

#prevyear3 <- prevyear3 %>% 
# left_join(prevyear3.xref, by = "SSCQid") %>%
#select(SSCQid, Vets, LA, pooled_ind_wt, cluster) %>% 
#filter(pooled_ind_wt > 0) %>% 
#mutate(Vets = ifelse(Vets == -1, NA, Vets),
#      Vets = factor(Vets, levels = c(-1, 0, 1),
#                   labels = c(NA,
#                             "Not a UK Armed Forces veteran",
#                            "UK Armed Forces veteran")))

### 3 - Survey proportions ----
sscq_year <- calculate_svymean(
  data = sscqyear,
  var_name = "Vets",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_ind_wt")

prev_year1 <- calculate_svymean(
  data = prevyear1,
  var_name = "Vets",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_ind_wt")

#prev_year2 <- calculate_svymean(
 # data = prevyear2,
  #var_name = "Vets",
  #cluster_col = "cluster",
  #strata_col = "LA",
  #weights_col = "pooled_ind_wt")

#prev_year3 <- calculate_svymean(
# data = prevyear3,
#var_name = "Vets",
#cluster_col = "cluster",
#strata_col = "LA",
#weights_col = "pooled_ind_wt")
### 4 - Combining datasets ----

Vets <- bind_rows(sscq_year, prev_year1#, prev_year2, prev_year3
                  )

Year <- c(config$sscqyear, config$sscqyear,  
          config$prevyear1, config$prevyear1#,
          #config$prevyear2, config$prevyear2,
          #config$prevyear3, config$prevyear3
          )

Vets$Year <- Year

Vets_table <- Vets %>% select(Year, Vets, perc_with_ci)
Vets_table

### 5 - Visualise estimates ----

Vets_table <- Vets_table %>% 
  group_by(Year) %>% 
  pivot_wider(
    names_from = Vets,
    values_from = perc_with_ci)

print(Vets_table)

saveRDS(Vets_table, "Outputs/Vets_table.rds")


# Not a veteran
Vets1 <- graph_plot(
  data = Vets,
  var_name = "Vets",
  var_value = "Not a UK Armed Forces veteran",
  plot_title = "SSCQ Respondent Veteran Status 
    by Year (Not a UK Armed 
    Forces veteran)")

print(Vets1)

ggsave(filename = "Outputs/Vets1.png", plot = Vets1,
       width = 200, height = 120, units = "mm")

# No
Vets2 <- graph_plot(
  data = Vets,
  var_name = "Vets",
  var_value = "UK Armed Forces veteran",
  plot_title = "SSCQ Respondent Veteran Status 
    by Year (UK Armed Forces veteran)")

print(Vets2)

ggsave(filename = "Outputs/Vets2.png", plot = Vets2,
       width = 200, height = 120, units = "mm")

