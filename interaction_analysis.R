############
## started: 06.06.2025
## file name: interaction_analysis.R
## context: Interaction analysis of the conjoint data (support policies and strict limit)
############

#load necessary packages
library(tidyverse)
library(lmerTest) #computes p values for linear mixed-effects regressions
library(emmeans)
library(performance) #to check model fit

#Source functions
source("functions.R")

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

# Count NA values in each column for diagnostics; note: 10 rows per participant
na_counts <- sapply(df_cj, function(x) sum(is.na(x)))
print(na_counts[na_counts > 0])

# Set baseline attribute levels
df_cj$Attr_Economy <- relevel(df_cj$Attr_Economy, ref = "Economy: Mandatory in Europe")
df_cj$Attr_Train <- relevel(df_cj$Attr_Train, ref = "Train: Voluntary")
df_cj$Attr_SAF <- relevel(df_cj$Attr_SAF, ref = "SAF: Voluntary")
df_cj$Attr_Infrastructure <- relevel(df_cj$Attr_Infrastructure, ref = "Infrastructure: No")
df_cj$Attr_Limit <- relevel(df_cj$Attr_Limit, ref = "Limit: Recommended limit")
df_cj$Attr_Rewards <- relevel(df_cj$Attr_Rewards, ref = "Rewards: No rewards")
df_cj$Attr_Sharing <- relevel(df_cj$Attr_Sharing, ref = "Sharing: Not allowed")
df_cj$Attr_Compensation <- relevel(df_cj$Attr_Compensation, ref = "Compensation: Not allowed")

############################### Calculate IRR and tau for remaining sample ################################### 
#IRR of entire sample (both frames)
IRR <- mean(df_cj$IRR_id == "reliable", na.rm = TRUE)

tau <- calculate_tau(df_cj)

############################### Split Data into Control and Outcome Samples ################################### 
df_control <- df_cj %>% filter(frame == "Control" | is.na(frame))
df_outcome <- df_cj %>% filter(frame == "Outcome" | is.na(frame))

############################### Interaction analysis between upper limit and support policies ################################### 

# Define attribute of interest
newdata_vars_interact <- c("Attr_Limit")

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

# Define subgroups
rewards_levels <- c("Rewards: No rewards", 
                    "Rewards: Additional funding for social events", 
                    "Rewards: Additional vacation days")

#Loop over subgroups and compute MMs for both models

# Initialize empty list
mm_list_control <- list()
mm_list_outcome <- list()

# Loop over subgroups
for (group_level in rewards_levels) {
  mm_list_control[[group_level]] <- get_marginal_means_by_group(
    model = m_control_choice_rewards,
    newdata_vars = newdata_vars_interact,
    group_var = "Attr_Rewards",
    group_level = group_level
  )
  
  mm_list_outcome[[group_level]] <- get_marginal_means_by_group(
    model = m_outcome_choice_rewards,
    newdata_vars = newdata_vars_interact,
    group_var = "Attr_Rewards",
    group_level = group_level
  )
}

# Combine, correct bias, and annotate
# Bind all rows
mm_df_control_choice_rewards <- bind_rows(mm_list_control)
mm_df_outcome_choice_rewards <- bind_rows(mm_list_outcome)

# Correct bias
corrected_mm_df_control_choice_rewards <- correct_bias_mm(mm_df_control_choice_rewards)
corrected_mm_df_outcome_choice_rewards <- correct_bias_mm(mm_df_outcome_choice_rewards)

# Assign additional info
corrected_mm_df_control_choice_rewards <- corrected_mm_df_control_choice_rewards %>%
  assign_frame("Control") %>%
  assign_evaluation("Choice")

corrected_mm_df_outcome_choice_rewards <- corrected_mm_df_outcome_choice_rewards %>%
  assign_frame("Outcome") %>%
  assign_evaluation("Choice")

# Combine both framing conditions
combined_rewards_df <- bind_rows(corrected_mm_df_control_choice_rewards,
                              corrected_mm_df_outcome_choice_rewards)

# Plot 1: Rewards

figure_interaction_rewards <- ggplot(combined_rewards_df, 
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
    name = "Rewards policy", 
    values = c("Rewards: No rewards" = 17, 
               "Rewards: Additional funding for social events" = 15, 
               "Rewards: Additional vacation days" = 19)
  ) +
  scale_color_manual(
    name = "Rewards policy", 
    values = c("Rewards: No rewards" = "#339999", 
               "Rewards: Additional funding for social events" = "#FFAA00", 
               "Rewards: Additional vacation days" = "#CC66FF")
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

print(figure_interaction_rewards)

ggsave(figure_interaction_rewards, filename = "plots/figure_3a_interaction_rewards_choice.png",
       width = 14, height = 6, unit = "in", dpi = 300)

#2) Sharing

# Fit models (choice data, both frames)
#control, choice
m_control_choice_sharing <- lmer(choice ~ Attr_Limit * Attr_Sharing +
                                   (1 | ResponseId), data = df_control)

summary(m_control_choice_sharing)

#outcome, choice
m_outcome_choice_sharing <- lmer(choice ~ Attr_Limit * Attr_Sharing +
                                   (1 | ResponseId), data = df_outcome)

summary(m_outcome_choice_sharing)

# Define subgroups
sharing_levels <- c("Sharing: Not allowed", 
                    "Sharing: Allowed within department / domain", 
                    "Sharing: Allowed within entire university")

#Loop over subgroups and compute MMs for both models

# Initialize empty list
mm_list_control <- list()
mm_list_outcome <- list()

# Loop over subgroups
for (group_level in sharing_levels) {
  mm_list_control[[group_level]] <- get_marginal_means_by_group(
    model = m_control_choice_sharing,
    newdata_vars = newdata_vars_interact,
    group_var = "Attr_Sharing",
    group_level = group_level
  )
  
  mm_list_outcome[[group_level]] <- get_marginal_means_by_group(
    model = m_outcome_choice_sharing,
    newdata_vars = newdata_vars_interact,
    group_var = "Attr_Sharing",
    group_level = group_level
  )
}

# Combine, correct bias, and annotate
# Bind all rows
mm_df_control_choice_sharing <- bind_rows(mm_list_control)
mm_df_outcome_choice_sharing <- bind_rows(mm_list_outcome)

# Correct bias
corrected_mm_df_control_choice_sharing <- correct_bias_mm(mm_df_control_choice_sharing)
corrected_mm_df_outcome_choice_sharing <- correct_bias_mm(mm_df_outcome_choice_sharing)

# Assign additional info
corrected_mm_df_control_choice_sharing <- corrected_mm_df_control_choice_sharing %>%
  assign_frame("Control") %>%
  assign_evaluation("Choice")

corrected_mm_df_outcome_choice_sharing <- corrected_mm_df_outcome_choice_sharing %>%
  assign_frame("Outcome") %>%
  assign_evaluation("Choice")

# Combine both framing conditions
combined_sharing_df <- bind_rows(corrected_mm_df_control_choice_sharing,
                                 corrected_mm_df_outcome_choice_sharing)

# Plot 2: Sharing

figure_interaction_sharing <- ggplot(combined_sharing_df, 
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
    name = "Sharing policy", 
    values = c("Sharing: Not allowed" = 17, 
               "Sharing: Allowed within department / domain" = 15, 
               "Sharing: Allowed within entire university" = 19)
  ) +
  scale_color_manual(
    name = "Sharing policy", 
    values = c("Sharing: Not allowed" = "#339999", 
               "Sharing: Allowed within department / domain" = "#FFAA00", 
               "Sharing: Allowed within entire university" = "#CC66FF")
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

print(figure_interaction_sharing)

ggsave(figure_interaction_sharing, filename = "plots/figure_3b_interaction_sharing_choice.png",
       width = 14, height = 6, unit = "in", dpi = 300)

#3) Compensation

# Fit models (choice data, both frames)
#control, choice
m_control_choice_comp <- lmer(choice ~ Attr_Limit * Attr_Compensation +
                                   (1 | ResponseId), data = df_control)

summary(m_control_choice_comp)

#outcome, choice
m_outcome_choice_comp <- lmer(choice ~ Attr_Limit * Attr_Compensation +
                                   (1 | ResponseId), data = df_outcome)

summary(m_outcome_choice_comp)

# Define subgroups
comp_levels <- c("Compensation: Not allowed", 
                    "Compensation: Allowed but limited", 
                    "Compensation: Allowed, unlimited")

#Loop over subgroups and compute MMs for both models

# Initialize empty list
mm_list_control <- list()
mm_list_outcome <- list()

# Loop over flying frequency groups
for (group_level in comp_levels) {
  mm_list_control[[group_level]] <- get_marginal_means_by_group(
    model = m_control_choice_comp,
    newdata_vars = newdata_vars_interact,
    group_var = "Attr_Compensation",
    group_level = group_level
  )
  
  mm_list_outcome[[group_level]] <- get_marginal_means_by_group(
    model = m_outcome_choice_comp,
    newdata_vars = newdata_vars_interact,
    group_var = "Attr_Compensation",
    group_level = group_level
  )
}

# Combine, correct bias, and annotate
# Bind all rows
mm_df_control_choice_comp <- bind_rows(mm_list_control)
mm_df_outcome_choice_comp <- bind_rows(mm_list_outcome)

# Correct bias
corrected_mm_df_control_choice_comp <- correct_bias_mm(mm_df_control_choice_comp)
corrected_mm_df_outcome_choice_comp <- correct_bias_mm(mm_df_outcome_choice_comp)

# Assign additional info
corrected_mm_df_control_choice_comp <- corrected_mm_df_control_choice_comp %>%
  assign_frame("Control") %>%
  assign_evaluation("Choice")

corrected_mm_df_outcome_choice_comp <- corrected_mm_df_outcome_choice_comp %>%
  assign_frame("Outcome") %>%
  assign_evaluation("Choice")

# Combine both framing conditions
combined_compensation_df <- bind_rows(corrected_mm_df_control_choice_comp,
                                 corrected_mm_df_outcome_choice_comp)

# Plot 2: Sharing

figure_interaction_compensation <- ggplot(combined_compensation_df, 
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
    name = "Compensation policy", 
    values = c("Compensation: Not allowed" = 17, 
               "Compensation: Allowed but limited" = 15, 
               "Compensation: Allowed, unlimited" = 19)
  ) +
  scale_color_manual(
    name = "Compensation policy", 
    values = c("Compensation: Not allowed" = "#339999", 
               "Compensation: Allowed but limited" = "#FFAA00", 
               "Compensation: Allowed, unlimited" = "#CC66FF")
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

print(figure_interaction_compensation)

ggsave(figure_interaction_compensation, filename = "plots/figure_3c_interaction_compensation_choice.png",
       width = 14, height = 6, unit = "in", dpi = 300)



# Plotting
# Repeat for the other support policies: Compensation, Sharing
# Repeat for rating data -> supplemental materials