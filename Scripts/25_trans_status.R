#########################################################################
# Name of file - 25_trans_status.R
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


# Path to SCJS data
prevyear1.scjs <- haven::read_sas(config$prevyear1.scjs,
                               col_select = c(SERIAL,
                                              QDTRANS,
                                              QDTRANSTAT)) %>%
  rename(oldref_scjs =  SERIAL,
         trans_status_scjs = QDTRANS)

# path to shes trans data
prevyear1.shes <- read_csv(config$prevyear1.shes) %>%
  select(CPSerialA, TRANS, TRANS2, TransY) %>%
  rename(oldref_shes =  CPSerialA,
         trans_status_shes = TRANS) 

# Path to SHS data
prevyear1.shs <- haven::read_sas(config$prevyear1.shs,
                              col_select = c(UNIQID,
                                             RANDTRANS))%>%
  rename(oldref_shs =  UNIQID,
         trans_status_shs = RANDTRANS)


# Path to SCJS 2023 data
sscq.scjs <- haven::read_sas(config$sscq.scjs,
                               col_select = c(Serial,
                                              QDTRANS)) %>%
  rename(oldref_scjs =  Serial,
         trans_status_scjs = QDTRANS)

# path to shes trans data
sscq.shes <- read_csv(config$sscq.shes) %>%
  select(CPSerialA, TRANS, TransY) %>%
  rename(oldref_shes =  CPSerialA,
         trans_status_shes = TRANS)

# Path to SHS 2023 data
sscq.shs <- haven::read_sas(config$sscq.shs,
                              col_select = c(UNIQID,
                                             RANDTRANS))%>%
  rename(oldref_shs =  UNIQID,
         trans_status_shs = RANDTRANS)


### 2 - Merge data frames and recode variables ----

sscqyear <- sscqyear %>% 
  left_join(sscqxref, by = "SSCQid") %>%
  select(SSCQid, LA, pooled_ind_wt, pooled_ind_wt_sc, cluster, oldref) %>% 
  filter(pooled_ind_wt > 0) 

prevyear1 <- prevyear1 %>% 
  left_join(prevyear1.xref, by = "SSCQid") %>%
  select(SSCQid, LA, pooled_ind_wt, pooled_ind_wt_sc, cluster, oldref) %>% 
  filter(pooled_ind_wt > 0) 


### 3 - Data wrangling -------------------------------------------------------------------
allsurveyprev1 <- bind_rows(prevyear1.scjs, prevyear1.shes, prevyear1.shs) %>%
  mutate(oldref = ifelse(is.na(oldref_scjs) == FALSE, oldref_scjs, 
                         ifelse(is.na(oldref_shes) == FALSE, oldref_shes, oldref_shs)),
         trans_status = ifelse(is.na(trans_status_scjs) == FALSE, trans_status_scjs, 
                               ifelse(is.na(trans_status_shes) == FALSE, trans_status_shes, trans_status_shs))) %>%
  select(-c(starts_with("oldref_"), starts_with("trans_status_"))) %>%
  mutate(trans_status = ifelse(trans_status < 0, -1, trans_status))



allsurveysscq <- bind_rows(sscq.scjs, sscq.shes, sscq.shs) %>%
  mutate(oldref = ifelse(is.na(oldref_scjs) == FALSE, oldref_scjs, 
                         ifelse(is.na(oldref_shes) == FALSE, oldref_shes, oldref_shs)),
         trans_status = ifelse(is.na(trans_status_scjs) == FALSE, trans_status_scjs, 
                               ifelse(is.na(trans_status_shes) == FALSE, trans_status_shes, trans_status_shs))) %>%
  select(-c(starts_with("oldref_"), starts_with("trans_status_"))) %>%
  mutate(trans_status = ifelse(trans_status < 0, -1, trans_status))


### 4 - Build SSCQ data --------------------------------------------------------------
sscqyear <- sscqyear %>% 
  left_join(allsurveysscq,
            by = c("oldref")) %>%
  mutate(trans_status = ifelse(trans_status %in% c(-1, 3), 3, trans_status),
         trans_status = factor(trans_status, levels = c(1:3),
                               labels = c("No: Not trans and does not have a trans history",
                                          "Yes: Trans or has a trans history",
                                          NA)))

prevyear1 <- prevyear1 %>% 
  left_join(allsurveyprev1,
            by = c("oldref")) %>%
  mutate(trans_status = ifelse(trans_status %in% c(-1, 3), 3, trans_status),
         trans_status = factor(trans_status, levels = c(1:3),
                               labels = c("No: Not trans and does not have a trans history",
                                          "Yes: Trans or has a trans history",
                                          NA)))


### 5 - Survey proportions ----
sscq_year <- calculate_svymean(
  data = sscqyear,
  var_name = "trans_status",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_ind_wt")

prev_year1 <- calculate_svymean(
  data = prevyear1,
  var_name = "trans_status",
  cluster_col = "cluster",
  strata_col = "LA",
  weights_col = "pooled_ind_wt")


### 6 - Combining datasets ----

Trans <- bind_rows(sscq_year, prev_year1)

Year <- c(config$sscqyear, config$sscqyear,
          config$prevyear1, config$prevyear1)

Trans$Year <- Year

Trans_table <- Trans %>% select(Year, trans_status, perc_with_ci)

### 7 - Visualise estimates ----

Trans_table <- Trans_table %>% 
  group_by(Year) %>% 
  pivot_wider(
    names_from = trans_status,
    values_from = perc_with_ci)

saveRDS(Trans_table, "Outputs/Trans_table.rds")


# Yes
Trans1 <- graph_plot(
  data = Trans,
  var_name = "trans_status",
  var_value = "Yes: Trans or has a trans history",
  plot_title = "SSCQ Respondent Trans 
    Status by Year (Yes: Trans or 
    has a trans history)")

print(Trans1)

ggsave(filename = "Outputs/Trans1.png", plot = Trans1,
       width = 200, height = 120, units = "mm")

# No
Trans2 <- graph_plot(
  data = Trans,
  var_name = "trans_status",
  var_value = "No: Not trans and does not have a trans history",
  plot_title = "SSCQ Respondent Trans Status 
    by Year (No: Not trans and does not 
    have a trans history)")

print(Trans2)

ggsave(filename = "Outputs/Trans2.png", plot = Trans2,
       width = 200, height = 120, units = "mm")

