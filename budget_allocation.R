############
## started: 10.06.2025
## file name: budget_allocation.R
## context: Quantitative analysis of budget allocation data
############

# load necessary packages
library(tidyverse)
library(scales)
library(RColorBrewer)

# load the preprocessed dataset (private)
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

# For Need-Based distribution
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
        "Strongly disagree",
        "Disagree somewhat",
        "Neither agree nor disagree",
        "Agree somewhat",
        "Strongly agree"
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
        "Q20_2_Utilitarian",
        "Q20_3_Prioritarian",
        "Q20_1_Egalitarian",
        "Q20_4_Proportional",
        "Q20_5_Sufficientarian",
        "Q20_6_NeedBased"
      ),
      labels = c(
        # Q20_2_Utilitarian
        "Utilitarian: Everyone has to reduce, except \nhigher-career individuals (e.g. professors, \nexecutive staff).",
        
        # Q20_3_Prioritarian
        "Prioritarian: Everyone has to reduce, except \nearly-career individuals (e.g. PhDs, postdocs).",
        
        # Q20_1_Egalitarian
        "Egalitarian: Everyone gets the \nsame air travel budget.",
        
        # Q20_4_Proportional
        "Proportional: Everyone reduces their \ncurrent emissions by 50%.",
        
        # Q20_5_Sufficientarian
        "Sufficientarian: ETH members who already\nfly very little don’t have to reduce.",
        
        # Q20_6_NeedBased
        "Need-Based: ETH members who have a \nhigher need for flying get a higher budget."
      )
    )
  )

######################## Figure 4: Plot all #################################

budget_plot <- ggplot(df_plot, aes(x = pct, y = Q20_item_label, fill = response)) +
  geom_col(position = "stack", width = 0.6) +
  geom_text(aes(label = paste0(round(pct),"%")), 
            position = position_stack(vjust = 0.5), 
            color = "black", size = 3.5) +
  #scale_fill_brewer(palette = "BrBG") +
  scale_fill_manual(
    values = c(
      "Strongly disagree"              = "#825911",
      "Disagree somewhat"              = "#dfc27d",
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
  axis.title.x = element_text(vjust = -2, size = 12),
  axis.title.y = element_text(vjust = 2, size = 12),
  axis.text = element_text(size = 11),
  strip.text = element_text(size = 12, face = "bold"),
  legend.position = "bottom",
  legend.text = element_text(size = 10)
) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE, reverse = TRUE))

print(budget_plot)



ggsave(budget_plot, filename = "plots/figure_4_budget_allocation.eps", width = 12, height = 4, unit="in", dpi = 300)

ggsave(budget_plot, filename = "plots/figure_4_budget_allocation.png", width = 12, height = 4, unit="in", dpi = 300)


# Original colours
budget_plot <- ggplot(df_plot, aes(x = Q20_item_label, y = n, fill = response)) +
  geom_col(position = "fill", width = 0.5, color = "black") +
  geom_text(
    aes(label = paste0(round(pct), "%")),
    position = position_fill(vjust = 0.5),
    size = 4
  ) +
  coord_flip() +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(
    values = c(
      "Strongly disagree"              = "#d73027",
      "Disagree somewhat"              = "#f46d43",
      "Neither agree nor disagree"     = "#f7f7f7",
      "Agree somewhat"                 = "#74add1",
      "Strongly agree"                 = "#2c7bb6",
      "Don't know / Prefer not to say" = "grey70"
    )
  ) +
  labs(x = "Allocation strategy",
       y = "Percentage of responses",
       fill = "Answer category"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(vjust = 0, size = 14),
    axis.title.y = element_text(vjust = 2, size = 14),
    axis.text = element_text(size = 12),
    legend.position = "bottom",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 11)) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE, reverse = TRUE))

print(budget_plot)