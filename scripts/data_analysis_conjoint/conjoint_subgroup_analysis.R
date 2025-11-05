############
## started: 06.06.2025
## file name: conjoint_subgroup_analysis.R
## context: Subgroup analysis of the conjoint analysis based on flying frequency
############

#load necessary packages
library(tidyverse)
library(lmerTest) #computes p values for linear mixed-effects regressions
library(emmeans)
library(patchwork)

#Source functions
source("scripts/functions.R")

#load the preprocessed dataset
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

# Set baseline for subgroup analysis
df_cj$role_group <- relevel(factor(df_cj$role_group), ref = "Doctoral students")
df_cj$flying_frequency <- relevel(df_cj$flying_frequency, ref = "Non-flyers")

############################### Calculate IRR and tau for remaining sample ################################### 
#IRR of entire sample (both frames)
IRR <- mean(df_cj$IRR_id == "reliable", na.rm = TRUE)

tau <- calculate_tau(df_cj)

############################### Split Data into Control and Outcome Samples ################################### 
df_control <- df_cj %>% filter(frame == "Control" | is.na(frame))
df_outcome <- df_cj %>% filter(frame == "Outcome" | is.na(frame))

############################### Subgroup analysis: Flying frequency ################################### 
# Fit models (only choice data, both frames)
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

#control, rating
m_control_rating_freq <- lmer(rating ~ (Attr_Limit + Attr_Rewards + Attr_Sharing + Attr_Compensation) * #attributes of interest
                                flying_frequency + #predictor
                                Attr_Infrastructure + Attr_Economy + Attr_Train + Attr_SAF + #remaining attributes
                                (1 | ResponseId), data = df_control)

summary(m_control_rating_freq)

#outcome, rating
m_outcome_rating_freq <- lmer(rating ~ (Attr_Limit + Attr_Rewards + Attr_Sharing + Attr_Compensation) * #attributes of interest
                                flying_frequency + #predictor
                                Attr_Infrastructure + Attr_Economy + Attr_Train + Attr_SAF + #remaining attributes
                                (1 | ResponseId), data = df_outcome)

summary(m_outcome_rating_freq)

# Define subgroups for plotting
flying_levels <- c("Non-flyers", "Infrequent flyers", "Frequent flyers")

# Choose which attributes to include in the plots: Only strictness of the upper limit
newdata_vars_base <- c("Attr_Limit")

############################### Subgroup analysis flying frequency ################################### 
#1) Choice data
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

# Assign frame and evaluation
corrected_mm_df_control_choice_freq <- assign_frame_and_evaluation(corrected_mm_df_control_choice_freq,"No framing (Control)", "Choice")
corrected_mm_df_outcome_choice_freq <- assign_frame_and_evaluation(corrected_mm_df_outcome_choice_freq,"Policy effectiveness framing", "Choice")

#2) Rating data
#Loop over subgroups and compute MMs for both models

# Initialize empty list
mm_list_control_rating <- list()
mm_list_outcome_rating <- list()

# Loop over flying frequency groups
for (group_level in flying_levels) {
  mm_list_control_rating[[group_level]] <- get_marginal_means_by_group(
    model = m_control_rating_freq,
    newdata_vars = newdata_vars_base,
    group_var = "flying_frequency",
    group_level = group_level
  )
  
  mm_list_outcome_rating[[group_level]] <- get_marginal_means_by_group(
    model = m_outcome_rating_freq,
    newdata_vars = newdata_vars_base,
    group_var = "flying_frequency",
    group_level = group_level
  )
}

# Combine and annotate
# Bind all rows
mm_df_control_rating_freq <- bind_rows(mm_list_control_rating)
mm_df_outcome_rating_freq <- bind_rows(mm_list_outcome_rating)

# Assign additional info
mm_df_control_rating_freq <- assign_frame_and_evaluation(mm_df_control_rating_freq, "No framing (Control)", "Rating")
mm_df_outcome_rating_freq <- assign_frame_and_evaluation(mm_df_outcome_rating_freq, "Policy effectiveness framing", "Rating")

############################### Figure S2: Plot Subgroup analysis flying frequency ################################### 
#1) Choice data (upper part)
subgroup_control_choice_freq <- ggplot(corrected_mm_df_control_choice_freq, 
                                aes(x = value, y = estimate, 
                                    color = group, shape = group)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  size = 1,
                  position = position_dodge(width = 0.5)) +
  geom_hline(aes(yintercept = 0.5), 
             linetype = "dashed",
             color = "grey30", 
             show.legend = FALSE) +
  facet_grid(term ~ frame, scales = "free", switch = "y") +
  coord_flip() +
  theme_bw() +
  ylab("Marginal means (Choice)") +
  xlab("Policy") +
  scale_shape_manual(
    name = "Subgroup (Control)", 
    values = c("Non-flyers" = 17, 
               "Infrequent flyers" = 15, 
               "Frequent flyers" = 19)
  ) +
  scale_color_manual(
    name = "Subgroup (Control)", 
    values = c("Non-flyers" = "#81CDC1", 
               "Infrequent flyers" = "#419A8F", 
               "Frequent flyers" = "#00665d")
  ) +
  theme(
    axis.title.x = element_text(vjust = 0, size = 14),
    axis.title.y = element_text(vjust = 2, size = 14),
    axis.ticks = element_blank(),      # removes tick marks
    axis.text = element_text(size = 14, color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.5),
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text.x = element_text(size = 14),
    strip.text.y = element_blank()
  )

print(subgroup_control_choice_freq)

subgroup_outcome_choice_freq <- ggplot(corrected_mm_df_outcome_choice_freq, 
                                aes(x = value, y = estimate, 
                                    color = group, shape = group)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  size = 1,
                  position = position_dodge(width = 0.5)) +
  geom_hline(aes(yintercept = 0.5), 
             linetype = "dashed",
             color = "grey30", 
             show.legend = FALSE) +
  facet_grid(term ~ frame, scales = "free", switch = "y") +
  coord_flip() +
  theme_bw() +
  ylab("Marginal means (Choice)") +
  xlab("Policy") +
  scale_shape_manual(
    name = "Subgroup (Framed)", 
    values = c("Non-flyers" = 17, 
               "Infrequent flyers" = 15, 
               "Frequent flyers" = 19)
  ) +
  scale_color_manual(
    name = "Subgroup (Framed)", 
    values = c("Non-flyers" = "#FFAF47", 
               "Infrequent flyers" = "#FA8124", 
               "Frequent flyers" = "#f55200")
  ) +
  theme(
    axis.title.x = element_text(vjust = 0, size = 14),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),      # removes tick marks
    axis.text.x = element_text(size = 14, color = "black"),
    axis.text.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.5),
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text.x = element_text(size = 14),
    strip.text.y = element_blank()
  )
  
print(subgroup_outcome_choice_freq)

#2) Rating data (lower part)

subgroup_control_rate_freq <- ggplot(mm_df_control_rating_freq, 
                                aes(x = value, y = estimate, 
                                    color = group, shape = group)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  size = 1,
                  position = position_dodge(width = 0.5)) +
  geom_hline(aes(yintercept = 3.0), 
             linetype = "dashed",
             color = "grey30", 
             show.legend = FALSE) +
  facet_grid(term ~ frame, scales = "free", switch = "y") +
  coord_flip() +
  theme_bw() +
  ylab("Marginal means (Rating)") +
  xlab("Policy") +
  scale_shape_manual(
    name = "Subgroup (Control)", 
    values = c("Non-flyers" = 17, 
               "Infrequent flyers" = 15, 
               "Frequent flyers" = 19)
  ) +
  scale_color_manual(
    name = "Subgroup (Control)", 
    values = c("Non-flyers" = "#81CDC1", 
               "Infrequent flyers" = "#419A8F", 
               "Frequent flyers" = "#00665d")
  ) +
  theme(
    axis.title.x = element_text(vjust = 0, size = 14),
    axis.title.y = element_text(vjust = 2, size = 14),
    axis.ticks = element_blank(),      # removes tick marks
    axis.text = element_text(size = 14, color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.5),
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text.x = element_text(size = 14),
    strip.text.y = element_blank()
  )

print(subgroup_control_rate_freq)

subgroup_outcome_rate_freq <- ggplot(mm_df_outcome_rating_freq, 
                                aes(x = value, y = estimate, 
                                    color = group, shape = group)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  size = 1,
                  position = position_dodge(width = 0.5)) +
  geom_hline(aes(yintercept = 3.0), 
             linetype = "dashed",
             color = "grey30", 
             show.legend = FALSE) +
  facet_grid(term ~ frame, scales = "free", switch = "y") +
  coord_flip() +
  theme_bw() +
  ylab("Marginal means (Rating)") +
  xlab("Policy") +
  scale_shape_manual(
    name = "Subgroup (Framed)", 
    values = c("Non-flyers" = 17, 
               "Infrequent flyers" = 15, 
               "Frequent flyers" = 19)
  ) +
  scale_color_manual(
    name = "Subgroup (Framed)", 
    values = c("Non-flyers" = "#FFAF47", 
               "Infrequent flyers" = "#FA8124", 
               "Frequent flyers" = "#f55200")
  ) +
  theme(
    axis.title.x = element_text(vjust = 0, size = 14),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),      # removes tick marks
    axis.text.x = element_text(size = 14, color = "black"),
    axis.text.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.5),
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text.x = element_text(size = 14),
    strip.text.y = element_blank()
  )

print(subgroup_outcome_rate_freq)

#3) Combine choice and rating plots

figure_subgroup_freq <- (
  (subgroup_control_choice_freq + subgroup_outcome_choice_freq) /
    plot_spacer() /   #vertical space
    (subgroup_control_rate_freq + subgroup_outcome_rate_freq)
) +
  plot_layout(guides = "collect", heights = c(1, 0.1, 1)) &  # 0.1 controls the gap size
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14)
  )

print(figure_subgroup_freq)

#Save combined figure S2
ggsave(figure_subgroup_freq, filename = "plots/supplementary_figures/Figure_S2_subgroup_freq.png",
       width = 12, height = 8, unit = "in", dpi = 300)
ggsave(figure_subgroup_freq, filename = "plots/supplementary_figures/Figure_S2_subgroup_freq.eps",
       width = 12, height = 8, unit = "in", dpi = 300)
