############
## started: 28.05.2025
## file name: conjoint_analysis.R
## context: Conjoint analysis and subgroup analysis
############

#load necessary packages
library(tidyverse)
library(lmerTest) #computes p values for linear mixed-effects regressions
library(emmeans)
library(performance) #to check model fit

#load the preprocessed dataset (private)
df_cj <- read.csv("data/Recoded_Conjoint_Data.csv", sep = ",", dec=",")

############################### Inspect and prepare data ################################### 
glimpse(df_cj)

# Convert all columns to factors (if not already)
df_cj <- df_cj %>%
  mutate(across(everything(), as.factor))

# Convert choice and rating back to numeric
df_cj$choice <- as.numeric(as.character(df_cj$choice))
df_cj$rating <- as.numeric(as.character(df_cj$rating))

# Cluster flying frequency and role
df_cj <- df_cj %>%
  mutate(
    flying_frequency = as.character(flying_frequency),
    flying_frequency = case_when(
      flying_frequency == "0" ~ "Non-flyers",
      flying_frequency == "1" ~ "Infrequent flyers",
      flying_frequency %in% c("2", "3", "4") ~ "Frequent flyers",
      flying_frequency == "Prefer not to say" ~ NA_character_,
      TRUE ~ flying_frequency
    ),
    flying_frequency = factor(flying_frequency),  # back to factor
    role_group = case_when(
      role == "Doctoral student" ~ "Doctoral students",
      role %in% c(
        "Postdoctoral Researcher (Postdoc & Scientific Assistant I + II)",
        "Senior Assistant (Senior Assistant I + II & Scientific Collaborator I + II)",
        "Senior Scientist (Senior Scientist I + II / Titulary Professor & Executive Scientific Collaborator I + II)"
      ) ~ "Other scientific staff",
      role == "Full / Associate / Assistant Professor" ~ "Professors",
      TRUE ~ as.character(role)  # fallback to keep original if no match
    )
  )

#Count
df_cj %>%
  count(df_cj$flying_frequency)

df_cj %>%
  count(df_cj$role_group)

# Set baseline for subgroup analysis
df_cj$role_group <- relevel(factor(df_cj$role_group), ref = "Doctoral students")
df_cj$flying_frequency <- relevel(df_cj$flying_frequency, ref = "Non-flyers")

# Count NA values in each column for diagnostics; note: 10 rows per participant
na_counts <- sapply(df_cj, function(x) sum(is.na(x)))
print(na_counts[na_counts > 0])

# Rename levels to make them unique by prefixing with the attribute name
df_cj <- df_cj %>%
  mutate(
    Attr_Economy = factor(Attr_Economy, 
                          levels = levels(Attr_Economy),
                          labels = paste("Economy:", levels(Attr_Economy))),
    Attr_Train = factor(Attr_Train, 
                        levels = levels(Attr_Train),
                        labels = paste("Train:", levels(Attr_Train))),
    Attr_SAF = factor(Attr_SAF, 
                      levels = levels(Attr_SAF),
                      labels = paste("SAF:", levels(Attr_SAF))),
    Attr_Infrastructure = factor(Attr_Infrastructure, 
                                 levels = levels(Attr_Infrastructure),
                                 labels = paste("Infrastructure:", levels(Attr_Infrastructure))),
    Attr_Limit = factor(Attr_Limit, 
                        levels = levels(Attr_Limit),
                        labels = paste("Limit:", levels(Attr_Limit))),
    Attr_Rewards = factor(Attr_Rewards, 
                          levels = levels(Attr_Rewards),
                          labels = paste("Rewards:", levels(Attr_Rewards))),
    Attr_Sharing = factor(Attr_Sharing, 
                          levels = levels(Attr_Sharing),
                          labels = paste("Sharing:", levels(Attr_Sharing))),
    Attr_Compensation = factor(Attr_Compensation, 
                               levels = levels(Attr_Compensation),
                               labels = paste("Compensation:", levels(Attr_Compensation)))
  )

############################### Set baseline attribute levels ################################### 
df_cj$Attr_Economy <- relevel(df_cj$Attr_Economy, ref = "Economy: Mandatory in Europe")
df_cj$Attr_Train <- relevel(df_cj$Attr_Train, ref = "Train: Voluntary")
df_cj$Attr_SAF <- relevel(df_cj$Attr_SAF, ref = "SAF: Voluntary")
df_cj$Attr_Infrastructure <- relevel(df_cj$Attr_Infrastructure, ref = "Infrastructure: No")
df_cj$Attr_Limit <- relevel(df_cj$Attr_Limit, ref = "Limit: Recommended limit")
df_cj$Attr_Rewards <- relevel(df_cj$Attr_Rewards, ref = "Rewards: No rewards")
df_cj$Attr_Sharing <- relevel(df_cj$Attr_Sharing, ref = "Sharing: Not allowed")
df_cj$Attr_Compensation <- relevel(df_cj$Attr_Compensation, ref = "Compensation: Not allowed")

############################### Filter dataset to new sample, excluding BSc/MSc students, admin/tech staff, and NAs ################################### 
df_cj <- df_cj %>%
  filter(
    !role %in% c("Bachelor's or Master's student", "Administrative or technical staff"),
    !is.na(role)
  )

############################### Calculate IRR and tau for remaining sample ################################### 
#IRR of entire sample (both frames)
IRR <- mean(df_cj$IRR_id == "reliable", na.rm = TRUE)

#Function to calculate swapping error tau
calculate_tau <- function(df) {
  IRR <- mean(df$IRR_id == "reliable", na.rm = TRUE)
  tau <- (1 - sqrt(1 - 2 * (1 - IRR))) / 2
  return(tau)
}

tau <- calculate_tau(df_cj)

############################### Split Data into Control and Outcome Samples ################################### 
df_control <- df_cj %>% filter(frame == "Control" | is.na(frame))
df_outcome <- df_cj %>% filter(frame == "Outcome" | is.na(frame))

############################### Fit Baseline Models (choice and rating, both frames) ################################### 
#control, choice
m_control_choice_base <- lmer(choice ~ Attr_Economy + Attr_Train + Attr_SAF + Attr_Limit +
                                Attr_Rewards + Attr_Sharing + Attr_Compensation + Attr_Infrastructure +
                                (1 | ResponseId), data = df_control)

summary(m_control_choice_base)

#control, rating
m_control_rate_base <- lmer(rating ~ Attr_Economy + Attr_Train + Attr_SAF + Attr_Limit +
                              Attr_Rewards + Attr_Sharing + Attr_Compensation + Attr_Infrastructure +
                              (1 | ResponseId), data = df_control)

summary(m_control_rate_base)

#outcome, choice
m_outcome_choice_base <- lmer(choice ~ Attr_Economy + Attr_Train + Attr_SAF + Attr_Limit +
                                Attr_Rewards + Attr_Sharing + Attr_Compensation + Attr_Infrastructure +
                                (1 | ResponseId), data = df_outcome)

summary(m_outcome_choice_base)

#outcome, rating
m_outcome_rate_base <- lmer(rating ~ Attr_Economy + Attr_Train + Attr_SAF + Attr_Limit +
                              Attr_Rewards + Attr_Sharing + Attr_Compensation + Attr_Infrastructure +
                              (1 | ResponseId), data = df_outcome)

summary(m_outcome_rate_base)

############################### Check model fit ############################### 
check_model(m_control_choice_base)
check_model(m_control_rate_base)
check_model(m_outcome_choice_base)
check_model(m_outcome_rate_base)



############################### Robustness check: Fit controlled Models (only rating, both frames) ################################### 
#control, rating
m_control_rate_ctrl <- lmer(rating ~ Attr_Economy + Attr_Train + Attr_SAF + Attr_Limit +
                              Attr_Rewards + Attr_Sharing + Attr_Compensation + Attr_Infrastructure +
                              role_group + #Role
                              domain + #Department
                              age_group +
                              flying_frequency + 
                              Q3_relevance_numeric + Q26_importance_numeric + Q8_approval_numeric + 
                              (1 | ResponseId), data = df_control)

summary(m_control_rate_ctrl)


#outcome, rating
m_outcome_rate_ctrl <- lmer(rating ~ Attr_Economy + Attr_Train + Attr_SAF + Attr_Limit +
                              Attr_Rewards + Attr_Sharing + Attr_Compensation + Attr_Infrastructure +
                              role_group + #Role
                              domain + #Department
                              age_group +
                              flying_frequency + 
                              Q3_relevance_numeric + Q26_importance_numeric + Q8_approval_numeric + 
                              (1 | ResponseId), data = df_outcome)

summary(m_outcome_rate_ctrl)

############################### Functions for marginal means ################################### 
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


## Choose which attributes to include in the plot

# All attributes
# newdata_vars_base <- c("Attr_Economy", "Attr_Train", "Attr_SAF",
#                       "Attr_Limit", "Attr_Rewards", "Attr_Sharing", 
#                       "Attr_Compensation", "Attr_Infrastructure")

# Only Budget-related attributes for paper
newdata_vars_base <- c("Attr_Limit", "Attr_Rewards", "Attr_Sharing", "Attr_Compensation")


############################### Complete sample: Marginal Means, choice and rating, both frames ################################### 
#Apply marginal means functions
mm_df_control_choice_base <- get_marginal_means_df(m_control_choice_base, newdata_vars_base)
mm_df_control_rate_base <- get_marginal_means_df(m_control_rate_base, newdata_vars_base)
mm_df_outcome_choice_base <- get_marginal_means_df(m_outcome_choice_base, newdata_vars_base)
mm_df_outcome_rate_base <- get_marginal_means_df(m_outcome_rate_base, newdata_vars_base)

#Correct choice dfs
corrected_mm_df_control_choice_base <- correct_bias_mm(mm_df_control_choice_base)
corrected_mm_df_outcome_choice_base <- correct_bias_mm(mm_df_outcome_choice_base)

#Assign frame to dfs
mm_df_control_rate_base <- assign_frame(mm_df_control_rate_base, "Control")
mm_df_outcome_rate_base <- assign_frame(mm_df_outcome_rate_base, "Outcome")
corrected_mm_df_control_choice_base <- assign_frame(corrected_mm_df_control_choice_base, "Control")
corrected_mm_df_outcome_choice_base <- assign_frame(corrected_mm_df_outcome_choice_base, "Outcome")

#Assign Choice/Rating to dfs
mm_df_control_rate_base <- assign_evaluation(mm_df_control_rate_base, "Rating")
mm_df_outcome_rate_base <- assign_evaluation(mm_df_outcome_rate_base, "Rating")
corrected_mm_df_control_choice_base <- assign_evaluation(corrected_mm_df_control_choice_base, "Choice")
corrected_mm_df_outcome_choice_base <- assign_evaluation(corrected_mm_df_outcome_choice_base, "Choice")

#Combine all model outputs
combined_df <- bind_rows(mm_df_control_rate_base,
                         mm_df_outcome_rate_base,
                         corrected_mm_df_control_choice_base,
                         corrected_mm_df_outcome_choice_base)

#Calculate mean rating for dashed line
mean_rating <- mean(combined_df$estimate[combined_df$evaluation == "Rating"], na.rm = TRUE)

# Create a data frame for the horizontal lines
hline_df <- data.frame(evaluation = c("Choice", "Rating"), intercept = c(0.5, mean_rating))

#Establish attribute order for plotting
desired_order <- c("Attr_Limit", "Attr_Rewards", "Attr_Sharing", "Attr_Compensation")
combined_df <- combined_df %>%
  mutate(term = factor(term, levels = desired_order))

############################### Figure 1: Conjoint ################################### 

# Create the plot: 
# - x-axis: attribute level (value)
# - y-axis: marginal mean estimate
# - shape (and grouping) by measure_type
# - facet by term (each attribute gets its own row)

figure_1_conjoint <- ggplot(combined_df, aes(x = value, y = estimate, 
                                      color = frame, shape = frame)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  size = 0.5,
                  position = position_dodge(width = 0.5)) +
  geom_hline(data = hline_df, 
             aes(yintercept = intercept), 
             linetype = "dashed",
             color = "grey30", 
             show.legend = FALSE) +
  facet_grid(term ~ evaluation, scales = "free") +
  theme_bw() +
  coord_flip() +
  ylab("Marginal Means") +
  xlab("Policy") +
  scale_shape_manual(name = "Framing Group", 
                     values = c("Control" = 17,
                                "Outcome" = 19)) +
  scale_color_manual(name = "Framing Group", 
                     values = c("Control" = "#339999", "Outcome" = "#CC66FF")) +
  scale_alpha_manual(name = "Significance", values = c("yes" = 1, "no" = 0.5)) +
  guides(color = guide_legend(override.aes = list(shape = c(17, 19))),
         shape = "none",
         alpha = "none") +
  theme(
    axis.title.x = element_text(vjust = 0, size = 14),
    axis.title.y = element_text(vjust = 2, size = 14),
    axis.text = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.5),
    legend.position = "bottom",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text.x = element_text(size = 14),  # set after theme_bw()
    strip.text.y = element_blank()  # remove row facet text
  )

print(figure_1_conjoint)

ggsave(figure_1_conjoint, filename = "plots/figure_1_conjoint.eps", width = 11, height = 8.5, unit="in", dpi = 300)
ggsave(figure_1_conjoint, filename = "plots/figure_1_conjoint.png", width = 11, height = 8.5, unit="in", dpi = 300)

############################### Subgroup analysis: Flying frequency ################################### 
# Only choice data for now

# Fit models (choice data, both frames)
#control, choice
m_control_choice_freq <- lmer(choice ~ (Attr_Limit + Attr_Rewards + Attr_Sharing + Attr_Compensation) * #attributes of interest
                                flying_frequency + #predictor
                                Attr_Infrastructure + Attr_Economy + Attr_Train + Attr_SAF + #remaining attributes
                                (1 | ResponseId), data = df_control)

summary(m_control_choice_freq)

#outcome, choice
m_outcome_choice_freq <- lmer(choice ~ (Attr_Limit + Attr_Rewards + Attr_Sharing + Attr_Compensation) * #attributes of interest
                                flying_frequency + #predictor
                                Attr_Infrastructure + Attr_Economy + Attr_Train + Attr_SAF + #remaining attributes
                                (1 | ResponseId), data = df_outcome)

summary(m_outcome_choice_freq)

# Marginal means per subgroup (flying frequency)

# Define subgroups
flying_levels <- c("Non-flyers", "Infrequent flyers", "Frequent flyers")

# Adapted function for subgroup MMs
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

#Loop over subgroups and compute MMs for both models

# Initialize empty list
mm_list_control <- list()
mm_list_outcome <- list()

# Loop over flying frequency groups
for (group_level in flying_levels) {
  mm_list_control[[group_level]] <- get_marginal_means_by_group(
    model = m_control_choice_freq,
    newdata_vars = newdata_vars_base,
    group_var = "flying_frequency",
    group_level = group_level
  )
  
  mm_list_outcome[[group_level]] <- get_marginal_means_by_group(
    model = m_outcome_choice_freq,
    newdata_vars = newdata_vars_base,
    group_var = "flying_frequency",
    group_level = group_level
  )
}

# Combine, correct bias, and annotate
# Bind all rows
mm_df_control_choice_freq <- bind_rows(mm_list_control)
mm_df_outcome_choice_freq <- bind_rows(mm_list_outcome)

# Correct bias
corrected_mm_df_control_choice_freq <- correct_bias_mm(mm_df_control_choice_freq)
corrected_mm_df_outcome_choice_freq <- correct_bias_mm(mm_df_outcome_choice_freq)

# Assign additional info
corrected_mm_df_control_choice_freq <- corrected_mm_df_control_choice_freq %>%
  assign_frame("Control") %>%
  assign_evaluation("Choice")

corrected_mm_df_outcome_choice_freq <- corrected_mm_df_outcome_choice_freq %>%
  assign_frame("Outcome") %>%
  assign_evaluation("Choice")

# Combine both framing conditions
combined_freq_df <- bind_rows(corrected_mm_df_control_choice_freq,
                              corrected_mm_df_outcome_choice_freq)

# Reorder attributes for plotting
combined_freq_df <- combined_freq_df %>%
  mutate(term = factor(term, levels = desired_order))


############################### Figure 2: Subgroup analysis flying frequency on choice data ################################### 
figure_subgroup_freq <- ggplot(combined_freq_df, 
                                     aes(x = value, y = estimate, 
                                         color = group, shape = group)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  size = 0.5,
                  position = position_dodge(width = 0.5)) +
  geom_hline(aes(yintercept = 0.5), 
             linetype = "dashed",
             color = "grey30", 
             show.legend = FALSE) +
  facet_grid(term ~ frame, scales = "free", switch = "y") +
  coord_flip() +
  theme_bw() +
  ylab("Marginal Means") +
  xlab("Policy") +
  scale_shape_manual(
    name = "Flying Frequency", 
    values = c("Non-flyers" = 17, 
               "Infrequent flyers" = 15, 
               "Frequent flyers" = 19)
  ) +
  scale_color_manual(
    name = "Flying Frequency", 
    values = c("Non-flyers" = "#339999", 
               "Infrequent flyers" = "#FFAA00", 
               "Frequent flyers" = "#CC66FF")
  ) +
  guides(
    color = guide_legend(override.aes = list(shape = c(17, 15, 19))),
    shape = "none"
  ) +
  theme(
    axis.title.x = element_text(vjust = 0, size = 14),
    axis.title.y = element_text(vjust = 2, size = 14),
    axis.text = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.5),
    legend.position = "bottom",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text.x = element_text(size = 14),
    strip.text.y = element_blank()
  )

print(figure_subgroup_freq)

ggsave(figure_subgroup_freq, filename = "plots/figure_2_subgroup_freq_choice.png",
       width = 12, height = 8.5, unit = "in", dpi = 300)

############################### Interaction analysis between upper limit and support policies ################################### 

#Function for interaction MMs
get_marginal_means_df <- function(model, newdata_vars) {
  # Loop over each attribute or interaction term
  emm_list <- lapply(newdata_vars, function(attr) {
    # Check if it's an interaction term
    if (grepl(":", attr)) {
      # Split interaction term into components
      attr_parts <- unlist(strsplit(attr, ":"))
      
      # Create interaction formula (e.g., ~ Attr_Limit * Attr_Rewards)
      formula_attr <- as.formula(paste("~", paste(attr_parts, collapse = "*")))
      
      # Get emmeans for interaction
      emm_obj <- emmeans(model, specs = formula_attr)
      
      # Convert to data frame
      emm_df <- as.data.frame(emm_obj) %>%
        rename(
          std.error = SE,
          estimate  = emmean,
          conf.low  = asymp.LCL,
          conf.high = asymp.UCL
        ) %>%
        mutate(
          term = paste(attr_parts, collapse = " × "),
          value = interaction(!!!syms(attr_parts), sep = " × ")
        ) %>%
        select(term, value, estimate, std.error, conf.low, conf.high)
      
    } else {
      # Main effect — single attribute
      formula_attr <- as.formula(paste("~", attr))
      emm_obj <- emmeans(model, specs = formula_attr)
      
      emm_df <- as.data.frame(emm_obj) %>%
        rename(
          std.error = SE,
          estimate  = emmean,
          conf.low  = asymp.LCL,
          conf.high = asymp.UCL
        ) %>%
        mutate(
          term = attr,
          value = .data[[attr]]
        ) %>%
        select(term, value, estimate, std.error, conf.low, conf.high)
    }
    
    return(emm_df)
  })
  
  # Combine all attribute/interaction results
  result <- bind_rows(emm_list)
  return(result)
}


#1) Rewards

# Fit models (choice data, both frames)
#control, choice
m_control_choice_rewards <- lmer(choice ~ Attr_Limit * Attr_Rewards +
                                (1 | ResponseId), data = df_control)

summary(m_control_choice_rewards)

#outcome, choice
m_outcome_choice_rewards <- lmer(choice ~ Attr_Limit * Attr_Rewards +
                                   (1 | ResponseId), data = df_outcome)

summary(m_outcome_choice_rewards)

# Compute MMs (doesn't really work yet)
newdata_vars_rewards <- c("Attr_Limit", "Attr_Rewards")
mm_control_choice_rewards <- get_marginal_means_df(m_control_choice_rewards, newdata_vars_rewards)


# Next steps:
# Bias correction
# Plotting
# Repeat for the other support policies: Compensation, Sharing
# Repeat for rating data -> supplemental materials