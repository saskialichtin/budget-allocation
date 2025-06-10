############
## started: 06.06.2025
## file name: subgroup_analysis.R
## context: Subgroup analysis of the conjoint analysis based on flying frequency
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

#Count subgroups
df_cj %>%
  count(df_cj$flying_frequency)

df_cj %>%
  count(df_cj$role_group)

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

############################### Paper: Subgroup analysis: Flying frequency ################################### 
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
# Only Budget-related attributes for paper
newdata_vars_base <- c("Attr_Limit", "Attr_Rewards", "Attr_Sharing", "Attr_Compensation")

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

#Establish attribute order for plotting
desired_order <- c("Attr_Limit", "Attr_Rewards", "Attr_Sharing", "Attr_Compensation")

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

############################### Appendix: Subgroup Analysis on the Rating Data ################################### 
# Models
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

# Define subgroups
flying_levels <- c("Non-flyers", "Infrequent flyers", "Frequent flyers")

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
mm_df_control_rating_freq <- mm_df_control_rating_freq %>%
  assign_frame("Control") %>%
  assign_evaluation("Rating")

mm_df_outcome_rating_freq <- mm_df_outcome_rating_freq %>%
  assign_frame("Outcome") %>%
  assign_evaluation("Rating")

# Combine both framing conditions
combined_freq_rating_df <- bind_rows(mm_df_control_rating_freq,
                                     mm_df_outcome_rating_freq)

# Reorder attributes
combined_freq_rating_df <- combined_freq_rating_df %>%
  mutate(term = factor(term, levels = desired_order))

############################### Supplemental Figure: Subgroup analysis flying frequency on rating data ################################### 

#Change dashed line to mean rating
mean_rating <- mean(combined_freq_rating_df$estimate)


supp_figure_subgroup_freq_rating <- ggplot(combined_freq_rating_df, 
                                      aes(x = value, y = estimate, 
                                          color = group, shape = group)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  size = 0.5,
                  position = position_dodge(width = 0.5)) +
  geom_hline(aes(yintercept = mean_rating), 
             linetype = "dashed",
             color = "grey30", 
             show.legend = FALSE) +
  facet_grid(term ~ frame, scales = "free_y", switch = "y") +
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

print(supp_figure_subgroup_freq_rating)

ggsave(supp_figure_subgroup_freq_rating, filename = "supplemental_material/supp_figure_subgroup_freq_rating.png",
       width = 12, height = 8.5, unit = "in", dpi = 300)

