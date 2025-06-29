############
## started: 28.05.2025
## file name: supplemental_material.R
## context: Create tables for supplemental materials, based on other scripts
############

# To Do:
# Change p.value levels to start from 0.05 not 0.1 for tables
# Change formatting of tables

library(tidyverse)
library(emmeans)
library(officer)
library(lmerTest) #computes p values for linear mixed-effects regressions

#Source functions
source("functions.R")

############################### Robustness check conjoint: Table to compare controlled ratings with base model ################################### 
#Only rating data
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

# Extract estimates for each model
df_control_rate_base  <- get_model_estimates(m_control_rate_base, "Control Rate Base")
df_outcome_rate_base  <- get_model_estimates(m_outcome_rate_base, "Outcome Rate Base")
df_control_rate_ctrl  <- get_model_estimates(m_control_rate_ctrl, "Control Rate Ctrl")
df_outcome_rate_ctrl  <- get_model_estimates(m_outcome_rate_ctrl, "Outcome Rate Ctrl")


# Combine all models into one data frame
combined_estimates <- bind_rows(df_control_rate_base,
                                df_control_rate_ctrl,
                                df_outcome_rate_base,
                                df_outcome_rate_ctrl)

# Pivot the data so that each attribute (term) is one row and we have columns for each model.
comparison_table <- combined_estimates %>% 
  select(term, Model, result) %>% 
  pivot_wider(names_from = Model, values_from = result)

#Export the table
doc <- read_docx()

# Try using the "Normal" style (or any style you know exists)
doc <- body_add_table(doc, value = comparison_table, style = "Normal")
print(doc, target = "supplemental_material/Robustness_Check_Conjoint_Table.docx")

############################### Supplemental figure: Robustness check conjoint randomisation ################################### 
# Step 1: Select relevant attributes
attribute_df <- df_cj %>%
  select(
    Attr_Economy, Attr_Train, Attr_SAF, Attr_Limit,
    Attr_Rewards, Attr_Sharing, Attr_Compensation, Attr_Infrastructure
  )

attr_order <- c(
  "Attr_Economy", "Attr_Train", "Attr_SAF", "Attr_Limit",
  "Attr_Rewards", "Attr_Sharing", "Attr_Compensation", "Attr_Infrastructure"
)

# Step 2: Generate all unique attribute pairs
attr_pairs <- combn(names(attribute_df), 2, simplify = FALSE)

# Step 3: Compute joint frequency counts
get_joint_freq <- function(var1, var2) {
  attribute_df %>%
    count(!!sym(var1), !!sym(var2)) %>%
    mutate(x_attr = var2, y_attr = var1,
           x = !!sym(var2), y = !!sym(var1))
}

joint_freq_data <- map_dfr(attr_pairs, ~get_joint_freq(.x[1], .x[2])) %>%
  mutate(
    x_attr = factor(x_attr, levels = attr_order),
    y_attr = factor(y_attr, levels = attr_order)
  )

# Step 4: Define facet labels
facet_labels <- c(
  Attr_Economy = "Economy Class",
  Attr_Train = "Train",
  Attr_SAF = "SAF",
  Attr_Limit = "Limit",
  Attr_Rewards = "Rewards",
  Attr_Sharing = "Sharing",
  Attr_Compensation = "Compensation",
  Attr_Infrastructure = "Infrastructure"
)


# Step 5: Plot matrix of heatmaps
level_correlations <- ggplot(joint_freq_data, aes(x = x, y = y, fill = n)) +
  geom_tile(color = "white") +
  facet_grid(
    rows = vars(y_attr),
    cols = vars(x_attr),
    scales = "free", space = "free",
    labeller = labeller(x_attr = facet_labels, y_attr = facet_labels)
  ) +
  scale_fill_viridis_c(option = "C") +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.spacing = unit(1, "lines"),
        strip.text = element_text(face = "bold")) +
  labs(x = "Policy level", y = "Policy level", fill = "Count")

print(level_correlations)

ggsave(level_correlations, filename = "supplemental_material/supp_figure_level_correlations.png", width = 11, height = 8.5, unit="in", dpi = 300)
