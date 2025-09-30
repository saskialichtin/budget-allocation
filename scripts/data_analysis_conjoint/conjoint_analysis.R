############
## started: 28.05.2025
## file name: conjoint_analysis.R
## context: Conjoint analysis
############

#load necessary packages
library(tidyverse)
library(lmerTest) #computes p values for linear mixed-effects regressions
library(emmeans)
library(patchwork)

#Source functions
source("scripts/functions.R")

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

############################### Split Data into separate framing groups ################################### 
#Control (no framing), Outcome (policy effectiveness framing, i.e. saw the policy mix's Outcome)
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

############################### Figure 1: Conjoint experiment results ################################### 
## Choose which attributes to include in the plot: Only Budget-related attributes for publishing article
newdata_vars_base <- c("Attr_Limit", "Attr_Rewards", "Attr_Sharing", "Attr_Compensation")

#Apply marginal means functions
mm_df_control_choice_base <- get_marginal_means_df(m_control_choice_base, newdata_vars_base)
mm_df_control_rate_base <- get_marginal_means_df(m_control_rate_base, newdata_vars_base)
mm_df_outcome_choice_base <- get_marginal_means_df(m_outcome_choice_base, newdata_vars_base)
mm_df_outcome_rate_base <- get_marginal_means_df(m_outcome_rate_base, newdata_vars_base)

#Correct choice dfs
corrected_mm_df_control_choice_base <- correct_bias_mm(mm_df_control_choice_base)
corrected_mm_df_outcome_choice_base <- correct_bias_mm(mm_df_outcome_choice_base)

#Assign frame and evaluation to dfs
mm_df_control_rate_base <- assign_frame_and_evaluation(mm_df_control_rate_base, "No framing (Control)", "Rating")
mm_df_outcome_rate_base <- assign_frame_and_evaluation(mm_df_outcome_rate_base, "Policy effectiveness framing", "Rating")
corrected_mm_df_control_choice_base <- assign_frame_and_evaluation(corrected_mm_df_control_choice_base, "No framing (Control)", "Choice")
corrected_mm_df_outcome_choice_base <- assign_frame_and_evaluation(corrected_mm_df_outcome_choice_base, "Policy effectiveness framing", "Choice")

#Establish attribute order for plotting
desired_order <- c("Attr_Limit", "Attr_Rewards", "Attr_Sharing", "Attr_Compensation")

#Combine model outputs
combined_df_choice <- bind_rows(corrected_mm_df_control_choice_base,
                                corrected_mm_df_outcome_choice_base)
combined_df_choice <- combined_df_choice %>%
  mutate(term = factor(term, levels = desired_order))

combined_df_rate <- bind_rows(mm_df_control_rate_base,
                              mm_df_outcome_rate_base)
combined_df_rate <- combined_df_rate %>%
  mutate(term = factor(term, levels = desired_order))

#Choice plot
conjoint_choice <- ggplot(combined_df_choice, aes(x = value, y = estimate, 
                                             color = frame, shape = frame)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  size = 0.5,
                  position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0.5, 
             linetype = "dashed",
             color = "grey30", 
             show.legend = FALSE) +
  facet_grid(term ~ evaluation, scales = "free") +
  theme_bw() +
  coord_flip() +
  ylab("Marginal means") +
  xlab("Policy") +
  scale_shape_manual(name = "Subgroup", 
                     values = c("No framing (Control)" = 17,
                                "Policy effectiveness framing" = 19)) +
  scale_color_manual(name = "Subgroup", 
                     values = c("No framing (Control)" = "#01665e", 
                                "Policy effectiveness framing" = "#f55200")) +
  guides(color = guide_legend(override.aes = list(shape = c(17, 19))),
         shape = "none",
         alpha = "none") +
  theme(
    axis.title.x = element_text(vjust = 0, size = 14),
    axis.title.y = element_text(vjust = 2, size = 14),
    axis.ticks = element_blank(),      # removes tick marks
    axis.text = element_text(size = 14, color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.5),
    legend.position = "none",
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text.x = element_text(size = 14),  # set after theme_bw()
    strip.text.y = element_blank()  # remove row facet text
  )

#Rating plot
conjoint_rating <- ggplot(combined_df_rate, aes(x = value, y = estimate, 
                                                  color = frame, shape = frame)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  size = 0.5,
                  position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 3.0, 
             linetype = "dashed",
             color = "grey30", 
             show.legend = FALSE) +
  facet_grid(term ~ evaluation, scales = "free") +
  theme_bw() +
  coord_flip() +
  ylim(2.5, 3.5) +
  ylab("Marginal means") +
  xlab(NULL) +
  scale_shape_manual(name = "Subgroup", 
                     values = c("No framing (Control)" = 17,
                                "Policy effectiveness framing" = 19)) +
  scale_color_manual(name = "Subgroup", 
                     values = c("No framing (Control)" = "#01665e", 
                                "Policy effectiveness framing" = "#f55200")) +
  guides(color = guide_legend(override.aes = list(shape = c(17, 19))),
         shape = "none",
         alpha = "none") +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),      # removes tick marks
    axis.text.x = element_text(size = 14, color = "black"),
    axis.text.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.5),
    legend.position = "none",
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text.x = element_text(size = 14),  # set after theme_bw()
    strip.text.y = element_blank()  # remove row facet text
  )


#Combined plot
figure_1_conjoint <- conjoint_choice + conjoint_rating +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14))

print(figure_1_conjoint)

ggsave(figure_1_conjoint, filename = "plots/figure_1_conjoint.eps", width = 11, height = 8.5, unit="in", dpi = 300)
ggsave(figure_1_conjoint, filename = "plots/figure_1_conjoint.png", width = 11, height = 8.5, unit="in", dpi = 300)

############################### Supplementary Figure A1: Conjoint experiment results of all attributes ################################### 
## Choose which attributes to include in the plot, all:
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

#Assign frame and evaluation to dfs
mm_df_control_rate_all <- assign_frame_and_evaluation(mm_df_control_rate_all, "No framing (Control)", "Rating")
mm_df_outcome_rate_all <- assign_frame_and_evaluation(mm_df_outcome_rate_all, "Policy effectiveness framing", "Rating")
corrected_mm_df_control_choice_all <- assign_frame_and_evaluation(corrected_mm_df_control_choice_all, "No framing (Control)", "Choice")
corrected_mm_df_outcome_choice_all <- assign_frame_and_evaluation(corrected_mm_df_outcome_choice_all, "Policy effectiveness framing", "Choice")

#Combine all model outputs
combined_df_all_choice <- bind_rows(corrected_mm_df_control_choice_all,
                         corrected_mm_df_outcome_choice_all)

combined_df_all_rate <- bind_rows(mm_df_control_rate_all,
                                    mm_df_outcome_rate_all)

#Establish attribute order for plotting
desired_order_all <- c("Attr_Economy", "Attr_Train", "Attr_SAF",
                       "Attr_Limit", "Attr_Rewards", "Attr_Sharing", 
                       "Attr_Compensation", "Attr_Infrastructure")

combined_df_all_choice <- combined_df_all_choice %>%
  mutate(term = factor(term, levels = desired_order_all))

combined_df_all_rate <- combined_df_all_rate %>%
  mutate(term = factor(term, levels = desired_order_all))

#Choice plot
supp_conjoint_choice <- ggplot(combined_df_all_choice, aes(x = value, y = estimate, 
                                                  color = frame, shape = frame)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  size = 0.5,
                  position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0.5, 
             linetype = "dashed",
             color = "grey30", 
             show.legend = FALSE) +
  facet_grid(term ~ evaluation, scales = "free") +
  theme_bw() +
  coord_flip() +
  ylab("Marginal means") +
  xlab("Policy") +
  scale_shape_manual(name = "Subgroup", 
                     values = c("No framing (Control)" = 17,
                                "Policy effectiveness framing" = 19)) +
  scale_color_manual(name = "Subgroup", 
                     values = c("No framing (Control)" = "#01665e", 
                                "Policy effectiveness framing" = "#f55200")) +
  guides(color = guide_legend(override.aes = list(shape = c(17, 19))),
         shape = "none",
         alpha = "none") +
  theme(
    axis.title.x = element_text(vjust = 0, size = 14),
    axis.title.y = element_text(vjust = 2, size = 14),
    axis.ticks = element_blank(),      # removes tick marks
    axis.text = element_text(size = 12, color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.5),
    legend.position = "none",
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text.x = element_text(size = 14),  # set after theme_bw()
    strip.text.y = element_blank()  # remove row facet text
  )

#Rating plot
supp_conjoint_rating <- ggplot(combined_df_all_rate, aes(x = value, y = estimate, 
                                                color = frame, shape = frame)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  size = 0.5,
                  position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 3.0, 
             linetype = "dashed",
             color = "grey30", 
             show.legend = FALSE) +
  facet_grid(term ~ evaluation, scales = "free") +
  theme_bw() +
  coord_flip() +
  ylim(2.5, 3.6) +
  ylab("Marginal means") +
  xlab(NULL) +
  scale_shape_manual(name = "Subgroup", 
                     values = c("No framing (Control)" = 17,
                                "Policy effectiveness framing" = 19)) +
  scale_color_manual(name = "Subgroup", 
                     values = c("No framing (Control)" = "#01665e", 
                                "Policy effectiveness framing" = "#f55200")) +
  guides(color = guide_legend(override.aes = list(shape = c(17, 19))),
         shape = "none",
         alpha = "none") +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),      # removes tick marks
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.5),
    legend.position = "none",
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text.x = element_text(size = 14),  # set after theme_bw()
    strip.text.y = element_blank()  # remove row facet text
  )


#Combined plot
supp_figure_conjoint <- supp_conjoint_choice + supp_conjoint_rating +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14))

print(supp_figure_conjoint)

ggsave(supp_figure_conjoint, filename = "plots/supplementary_figures/Figure_A1_conjoint.eps", width = 11, height = 11, unit="in", dpi = 300)
ggsave(supp_figure_conjoint, filename = "plots/supplementary_figures/Figure_A1_conjoint.png", width = 11, height = 11, unit="in", dpi = 300)
