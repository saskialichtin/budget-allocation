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

# To Do
# Use subgroup MM function with Rewards as group

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

# Compute MMs
newdata_vars_rewards <- c("Attr_Limit")



# Next steps:
# Bias correction
# Plotting
# Repeat for the other support policies: Compensation, Sharing
# Repeat for rating data -> supplemental materials