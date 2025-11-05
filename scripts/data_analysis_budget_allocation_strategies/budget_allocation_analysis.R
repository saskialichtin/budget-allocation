############
## started: 10.06.2025
## file name: budget_allocation_analysis.R
## context: Quantitative analysis of budget allocation data
############

# load necessary packages
library(tidyverse)
library(scales)

# load the preprocessed dataset
df <- read.csv("data/Recoded_Budget_Data.csv", sep = ",", dec=",")

glimpse(df)

# Convert to numeric
df$Q20_2_Utilitarian_numeric <- as.numeric(df$Q20_2_Utilitarian_numeric)
df$Q20_3_Prioritarian_numeric <- as.numeric(df$Q20_3_Prioritarian_numeric)
df$Q20_1_Egalitarian_numeric <- as.numeric(df$Q20_1_Egalitarian_numeric)
df$Q20_4_Proportional_numeric <- as.numeric(df$Q20_4_Proportional_numeric)
df$Q20_5_Sufficientarian_numeric <- as.numeric(df$Q20_5_Sufficientarian_numeric)
df$Q20_6_NeedBased_numeric <- as.numeric(df$Q20_6_NeedBased_numeric)

########################## Numeric means ##########################
# For Egalitarian distribution
summary(df$Q20_1_Egalitarian_numeric)
sd(df$Q20_1_Egalitarian_numeric, na.rm = TRUE)

# For Utilitarian distribution
summary(df$Q20_2_Utilitarian_numeric)
sd(df$Q20_2_Utilitarian_numeric, na.rm = TRUE)

# For Prioritarian distribution
summary(df$Q20_3_Prioritarian_numeric)
sd(df$Q20_3_Prioritarian_numeric, na.rm = TRUE)

# For Proportional distribution
summary(df$Q20_4_Proportional_numeric)
sd(df$Q20_4_Proportional_numeric, na.rm = TRUE)

# For Sufficientarian distribution
summary(df$Q20_5_Sufficientarian_numeric)
sd(df$Q20_5_Sufficientarian_numeric, na.rm = TRUE)

# For Needs-Based distribution
summary(df$Q20_6_NeedBased_numeric)
sd(df$Q20_6_NeedBased_numeric, na.rm = TRUE)

########################## Prep for plotting ##########################
# 1) Pivot to long format
df_long <- df %>%
  dplyr::select(
    Q20_1_Egalitarian,
    Q20_2_Utilitarian,
    Q20_3_Prioritarian,
    Q20_4_Proportional,
    Q20_5_Sufficientarian,
    Q20_6_NeedBased
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Q20_item",     # e.g. "Q20_1_Egalitarian"
    values_to = "response"     # e.g. "Strongly disagree"
  )

# 2) Ensure 'response' is a factor in the desired order
df_long <- df_long %>%
  mutate(
    response = factor(
      response,
      levels = c(
        "Don't know / Prefer not to say",
        "Strongly agree",
        "Agree somewhat",
        "Neither agree nor disagree",
        "Disagree somewhat",
        "Strongly disagree"
      )
    )
  )

# 3) Summarize counts (n) and calculate percentages
df_plot <- df_long %>%
  group_by(Q20_item, response) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Q20_item) %>%
  mutate(
    total_responses = sum(n),
    pct = 100 * n / total_responses
  )

# 4) Create a factor with long descriptive labels
df_plot <- df_plot %>%
  mutate(
    Q20_item_label = factor(
      Q20_item,
      levels = c(
        "Q20_2_Utilitarian", "Q20_3_Prioritarian", "Q20_1_Egalitarian", "Q20_4_Proportional", "Q20_6_NeedBased","Q20_5_Sufficientarian"
      ),
      labels = c("Utilitarian","Prioritarian", "Egalitarian", "Proportional", "Needs-Based", "Sufficientarian")
    )
  )

######################## Figure 4: Plot all #################################

budget_plot <- ggplot(df_plot, aes(x = pct, y = Q20_item_label, fill = response)) +
  geom_col(position = "stack", width = 0.6) +
  geom_text(aes(label = paste0(round(pct),"%")), 
            position = position_stack(vjust = 0.5), 
            color = "black", size = 4.5) +
  #scale_fill_brewer(palette = "BrBG") +
  scale_fill_manual(
    values = c(
      "Strongly disagree"              = "#f55200",
      "Disagree somewhat"              = "#ffaf47",
      "Neither agree nor disagree"     = "#f5f5f5",
      "Agree somewhat"                 = "#80cdc1",
      "Strongly agree"                 = "#01665e",
      "Don't know / Prefer not to say" = "#d7c4b0"
    )
  ) +
  labs(x = "Percentage of responses",
       y = "Allocation strategy",
       fill = "Response"
  ) +
  theme_minimal() +
theme(
  axis.title.x = element_text(vjust = -2, size = 14, color = "black"),
  axis.title.y = element_text(vjust = 2, size = 14, color = "black"),
  axis.text = element_text(size = 14, color = "black"),
  legend.position = "bottom",
  legend.text = element_text(size = 12)
) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE, reverse = TRUE))

print(budget_plot)

ggsave(budget_plot, filename = "plots/Figure_2_budget_allocation.png", width = 11, height = 5, unit="in", dpi = 300)
ggsave(budget_plot, filename = "plots/Figure_2_budget_allocation.eps", width = 11, height = 5, unit="in", dpi = 300)
