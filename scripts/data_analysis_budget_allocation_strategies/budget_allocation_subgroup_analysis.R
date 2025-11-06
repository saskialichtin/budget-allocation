############
## started: 10.06.2025
## file name: budget_allocation_subgroup_analysis.R
## context: Subgroup analysis of budget allocation data
############

# load necessary packages
library(tidyverse)
library(scales)

# load the preprocessed dataset
df <- read.csv("data/Recoded_Budget_Data.csv", sep = ",", dec=",")

glimpse(df)

########################## Prep for plotting ##########################
# Define levels and labels
response_levels <- c(
  "Don't know / Prefer not to say",
  "Strongly agree",
  "Agree somewhat",
  "Neither agree nor disagree",
  "Disagree somewhat",
  "Strongly disagree"
)

q20_levels <- c(
  "Q20_2_Utilitarian",
  "Q20_3_Prioritarian",
  "Q20_1_Egalitarian",
  "Q20_4_Proportional",
  "Q20_6_NeedBased",
  "Q20_5_Sufficientarian"
)

q20_labels <- c(
  "Utilitarian",
  "Prioritarian",
  "Egalitarian",
  "Proportional",
  "Needs-Based",
  "Sufficientarian"
)

fill_colors <- c(
  "Strongly disagree"              = "#f55200",
  "Disagree somewhat"              = "#ffaf47",
  "Neither agree nor disagree"     = "#f5f5f5",
  "Agree somewhat"                 = "#80cdc1",
  "Strongly agree"                 = "#01665e",
  "Don't know / Prefer not to say" = "#d7c4b0"
)

df_frame <- df %>%
  select(frame, all_of(q20_levels)) %>%
  pivot_longer(cols = all_of(q20_levels), names_to = "Q20_item", values_to = "response") %>%
  mutate(
    response = factor(response, levels = response_levels),
    Q20_item_label = factor(Q20_item, levels = q20_levels, labels = q20_labels),
    frame = recode(frame,
                   "Control" = "No framing (Control)",
                   "Outcome" = "Policy effectiveness framing")
  ) %>%
  group_by(frame, Q20_item_label, response) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(frame, Q20_item_label) %>%
  mutate(
    total = sum(n),
    pct = 100 * n / total,
    pct_label = paste0(round(pct), "%")
  )

########################## Figure S3: Framing effect in budget allocation analysis, Robustness check ##########################

frame_subgroup_plot <- ggplot(df_frame, aes(x = pct, y = Q20_item_label, fill = response)) +
  geom_col(position = "stack", width = 0.7) +
  geom_text(aes(label = pct_label),
            position = position_stack(vjust = 0.5),
            color = "black", size = 3.5) +
  facet_wrap(~ frame) +
  scale_fill_manual(values = fill_colors) +
  labs(x = "Percentage of responses", y = "Allocation strategy", fill = "Response") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(vjust = -2, size = 14),
    axis.title.y = element_text(vjust = 2, size = 14),
    axis.text = element_text(size = 14, color = "black"),
    strip.text = element_text(size = 14),
    legend.position = "bottom",
    legend.text = element_text(size = 14),
    panel.spacing = unit(1, "lines")
  ) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE, reverse = TRUE))

print(frame_subgroup_plot)

ggsave("plots/supplementary_figures/Figure_S3_framing_effect_budget_allocation.png", frame_subgroup_plot,
       width = 11, height = 4.5, units = "in", dpi = 300)
ggsave("plots/supplementary_figures/Figure_S3_framing_effect_budget_allocation.eps", frame_subgroup_plot,
       width = 11, height = 4.5, units = "in", dpi = 300)
