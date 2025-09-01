#' Calculate svymean
#' 
#' @param data Data frame that needs svymean calculated
#' 
#' @returns Data frame with new named LA column


calculate_svymean <- function(data,
                              var_name,
                              cluster_col,
                              strata_col,
                              weights_col) {
  
  # Build the survey design dynamically
  survey_design <- svydesign(
    id = as.formula(paste0("~", cluster_col)),
    strata = as.formula(paste0("~", strata_col)),
    weights = as.formula(paste0("~", weights_col)),
    data = data
  )
  
  # Build the formula for svymean
  svymean_formula <- as.formula(paste0("~", var_name))
  
  # Compute results
  result <- svymean(svymean_formula, survey_design, na.rm = TRUE) %>%
    as_tibble() %>% 
    mutate(
      !!var_name := levels(data[[var_name]]),  # insert dynamic variable
      .before = mean
    ) %>%
    mutate(
      lower_ci = (mean - 1.96 * SE) * 100,
      upper_ci = (mean + 1.96 * SE) * 100,
      perc = mean * 100,
      margin_of_error = upper_ci - perc,
      perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))
    ) %>%
    select(-SE) %>%
    filter(!is.na(.data[[var_name]]))
  
  return(result)
}





#continuous variable version
calc_svymean_cont <- function(data,
                            target_var,
                            cluster_col,
                            strata_col,
                            weights_col) {
  
  # Create survey design
  survey_design <- svydesign(
    id = as.formula(paste0("~", cluster_col)),
    strata = as.formula(paste0("~", strata_col)),
    weights = as.formula(paste0("~", weights_col)),
    data = data
  )
  
  # Run svymean
  svy_formula <- as.formula(paste0("~", target_var))
  svy_result <- svymean(svy_formula, survey_design, na.rm = TRUE)
  
  # Extract means & SE
  means <- as.numeric(svy_result)
  ses <- as.numeric(SE(svy_result))
  
  # Build tibble
  result <- tibble(
    value = means,
    SE = ses
  ) %>%
    mutate(
      lower_ci = value - 1.96 * SE,
      upper_ci = value + 1.96 * SE,
      margin_of_error = upper_ci - value,
      mean_with_ci = paste0(round(value, 1), " ± ", round(margin_of_error, 1))
    )
  
  return(result)
}