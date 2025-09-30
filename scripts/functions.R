############
## started: 06.06.2025
## file name: functions.R
## context: Save all functions here to source into other scripts
############

############################## Function to calculate swapping error tau ##############################
calculate_tau <- function(df) {
  IRR <- mean(df$IRR_id == "reliable", na.rm = TRUE)
  tau <- (1 - sqrt(1 - 2 * (1 - IRR))) / 2
  return(tau)
}

############################### Base function for marginal means ################################### 
get_marginal_means_df <- function(model, newdata_vars) {
  # Loop over each attribute in newdata_vars
  emm_list <- lapply(newdata_vars, function(attr) {
    # Create a one-variable formula (e.g., ~ Attr_Economy)
    formula_attr <- as.formula(paste("~", attr))
    
    # Compute marginal means using a balanced grid (emmeans default)
    emm_obj <- emmeans(model, specs = formula_attr)
    
    # Convert the emmeans object to a data frame
    emm_df <- as.data.frame(emm_obj)
    
    # Rename columns to match what your plotting & bias-correction code expects
    emm_df <- emm_df %>%
      rename(
        std.error = SE,       # Standard Error
        estimate  = emmean,   # Estimated mean
        conf.low  = asymp.LCL,
        conf.high = asymp.UCL
      )
    
    # Add "term" (the attribute's name) and "value" (the level of that attribute)
    emm_df <- emm_df %>%
      mutate(
        term = attr,
        value = .data[[attr]]
      )
    
    # Keep only the columns needed by your code
    emm_df <- emm_df %>%
      select(term, value, estimate, std.error, conf.low, conf.high)
    
    return(emm_df)
  })
  
  # Combine the data frames for all attributes into one
  result <- bind_rows(emm_list)
  return(result)
}

############################ Adapted function for subgroup and interaction marginal means ############################ 
get_marginal_means_by_group <- function(model, newdata_vars, group_var, group_level) {
  emm_list <- lapply(newdata_vars, function(attr) {
    formula_attr <- as.formula(paste("~", attr))
    
    emm_obj <- emmeans(model, specs = formula_attr, at = setNames(list(group_level), group_var))
    
    emm_df <- as.data.frame(emm_obj)
    
    emm_df <- emm_df %>%
      rename(
        std.error = SE,
        estimate = emmean,
        conf.low = asymp.LCL,
        conf.high = asymp.UCL
      ) %>%
      mutate(
        term = attr,
        value = .data[[attr]],
        group = group_level
      ) %>%
      select(term, value, estimate, std.error, conf.low, conf.high, group)
    
    return(emm_df)
  })
  
  result <- bind_rows(emm_list)
  return(result)
}

############################### Bias correction function ################################### 
correct_bias_mm <- function(mm_df) {
  # Extract the uncorrected marginal mean and its variance
  tau <- tau
  mm <- mm_df$estimate
  var_mm <- mm_df$std.error^2
  
  # Set variance and covariance for tau (assumed 0 here)
  var_tau <- 0
  cov_mm_tau <- 0
  
  # Calculate the bias-corrected marginal mean:
  mm_fixed <- (mm - tau) / (1 - 2 * tau)
  
  # Calculate the variance of the corrected marginal mean:
  var_corrected_mm <- (mm_fixed^2 / (1 - 2 * tau)^2) *
    (
      ((var_mm + var_tau - 2 * cov_mm_tau) / mm_fixed^2) +
        (4 * (cov_mm_tau - var_tau) / mm_fixed) +
        4 * var_tau
    )
  
  # Corrected standard error is the square root of the corrected variance
  se_corrected <- sqrt(var_corrected_mm)
  
  # Construct the 95% confidence interval
  conf_low  <- mm_fixed - 1.96 * se_corrected
  conf_high <- mm_fixed + 1.96 * se_corrected
  
  # Return full dataframe with corrected values replacing the originals
  corrected_df <- mm_df %>%
    mutate(
      estimate  = mm_fixed,
      std.error = se_corrected,
      statistic = NA,
      p.value   = NA,
      s.value   = NA,
      conf.low  = conf_low,
      conf.high = conf_high,
      estimand  = "MM corrected"
    )
  
  return(corrected_df)
}

############################### Helper functions for plotting ################################### 
#Maybe combine into one

#Function to assign frame to df of model estimates
assign_frame <- function(df, frame_label) {
  df <- df %>% mutate(frame = frame_label)
  return(df)
}

#Function to assign choice/rating to df of model estimates
assign_evaluation <- function(df, eval_label) {
  df <- df %>% mutate(evaluation = eval_label)
  return(df)
}

# Function: Create a new variable to designate primary vs. ancillary measures
assign_measure_type <- function(df) {
  df <- df %>%
    mutate(
      measure_type = case_when(
        term %in% c("Attr_Economy", "Attr_Train", "Attr_SAF", "Attr_Limit") ~ "Primary Measures",
        term %in% c("Attr_Rewards", "Attr_Sharing", "Attr_Compensation", "Attr_Infrastructure") ~ "Ancillary Measures",
        TRUE ~ "Other"
      )
    )
  return(df)
}

#############################  Helper function to extract fixed effects and format the results for tables ############################ 
get_model_estimates <- function(model, model_name, digits = 2) {
  tidy_model <- broom.mixed::tidy(model, effects = "fixed") %>% 
    mutate(
      stars = case_when(
        p.value < 0.01 ~ "***",
        p.value < 0.05 ~ "**",
        p.value < 0.1  ~ "*",
        TRUE ~ ""
      ),
      estimate_round = round(estimate, digits),
      se_round = round(std.error, digits),
      result = paste0(estimate_round, " (", se_round, ") ", stars),
      Model = model_name
    )
  return(tidy_model)
}
