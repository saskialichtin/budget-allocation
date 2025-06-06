############
## started: 28.05.2025
## file name: conjoint_analysis.R
## context: Conjoint analysis
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

############################### Paper: Complete sample: Marginal Means, choice and rating, both frames ################################### 
## Choose which attributes to include in the plot
# Only Budget-related attributes for paper
newdata_vars_base <- c("Attr_Limit", "Attr_Rewards", "Attr_Sharing", "Attr_Compensation")

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

############################### Appendix: All attributes, complete sample: Marginal Means, choice and rating, both frames ################################### 
## Choose which attributes to include in the plot
# Only Budget-related attributes for paper
newdata_vars_all <- c("Attr_Economy", "Attr_Train", "Attr_SAF",
                      "Attr_Limit", "Attr_Rewards", "Attr_Sharing", 
                      "Attr_Compensation", "Attr_Infrastructure")

#Apply marginal means functions
mm_df_control_choice_all <- get_marginal_means_df(m_control_choice_base, newdata_vars_all)
mm_df_control_rate_all <- get_marginal_means_df(m_control_rate_base, newdata_vars_all)
mm_df_outcome_choice_all <- get_marginal_means_df(m_outcome_choice_base, newdata_vars_all)
mm_df_outcome_rate_all <- get_marginal_means_df(m_outcome_rate_base, newdata_vars_all)

#Correct choice dfs
corrected_mm_df_control_choice_all <- correct_bias_mm(mm_df_control_choice_all)
corrected_mm_df_outcome_choice_all <- correct_bias_mm(mm_df_outcome_choice_all)

#Assign frame to dfs
mm_df_control_rate_all <- assign_frame(mm_df_control_rate_all, "Control")
mm_df_outcome_rate_all <- assign_frame(mm_df_outcome_rate_all, "Outcome")
corrected_mm_df_control_choice_all <- assign_frame(corrected_mm_df_control_choice_all, "Control")
corrected_mm_df_outcome_choice_all <- assign_frame(corrected_mm_df_outcome_choice_all, "Outcome")

#Assign Choice/Rating to dfs
mm_df_control_rate_all <- assign_evaluation(mm_df_control_rate_all, "Rating")
mm_df_outcome_rate_all <- assign_evaluation(mm_df_outcome_rate_all, "Rating")
corrected_mm_df_control_choice_all <- assign_evaluation(corrected_mm_df_control_choice_all, "Choice")
corrected_mm_df_outcome_choice_all <- assign_evaluation(corrected_mm_df_outcome_choice_all, "Choice")

#Combine all model outputs
combined_df_all <- bind_rows(mm_df_control_rate_all,
                         mm_df_outcome_rate_all,
                         corrected_mm_df_control_choice_all,
                         corrected_mm_df_outcome_choice_all)

#Calculate mean rating for dashed line
mean_rating_all <- mean(combined_df_all$estimate[combined_df_all$evaluation == "Rating"], na.rm = TRUE)

# Create a data frame for the horizontal lines
hline_df_all <- data.frame(evaluation = c("Choice", "Rating"), intercept = c(0.5, mean_rating_all))

#Establish attribute order for plotting
desired_order_all <- c("Attr_Economy", "Attr_Train", "Attr_SAF",
                   "Attr_Limit", "Attr_Rewards", "Attr_Sharing", 
                   "Attr_Compensation", "Attr_Infrastructure")
combined_df_all <- combined_df_all %>%
  mutate(term = factor(term, levels = desired_order_all))

############################### Supplemental Figure: Conjoint ################################### 

# Create the plot: 
# - x-axis: attribute level (value)
# - y-axis: marginal mean estimate
# - shape (and grouping) by measure_type
# - facet by term (each attribute gets its own row)

supp_figure_conjoint <- ggplot(combined_df_all, aes(x = value, y = estimate, 
                                             color = frame, shape = frame)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  size = 0.5,
                  position = position_dodge(width = 0.5)) +
  geom_hline(data = hline_df_all, 
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

print(supp_figure_conjoint)

ggsave(supp_figure_conjoint, filename = "supplemental_material/supp_figure_conjoint.eps", width = 11, height = 8.5, unit="in", dpi = 300)
ggsave(supp_figure_conjoint, filename = "supplemental_material/supp_figure_conjoint.png", width = 11, height = 8.5, unit="in", dpi = 300)

