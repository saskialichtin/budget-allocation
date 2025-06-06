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

