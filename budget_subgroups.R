############
## started: 10.06.2025
## file name: budget_subgroups.R
## context: Subgroup analysis of budget allocation data
############

# load necessary packages
library(tidyverse)
library(scales)

# load the preprocessed dataset (private)
df <- read.csv("data/Recoded_Budget_Data.csv", sep = ",", dec=",")

glimpse(df)

########################## Prep for plotting ##########################

# Define levels once
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
  "Need-Based",
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

# Utility: reshape and label
reshape_q20 <- function(df, group_var) {
  df %>%
    select(all_of(group_var), all_of(q20_levels)) %>%
    pivot_longer(cols = all_of(q20_levels), names_to = "Q20_item", values_to = "response") %>%
    mutate(
      response = factor(response, levels = response_levels),
      Q20_item_label = factor(Q20_item, levels = q20_levels, labels = q20_labels)
    )
}

# Utility: aggregate and compute % per group
aggregate_pct <- function(df_long, group_var = NULL) {
  if (!is.null(group_var)) {
    df_long %>%
      group_by(across(all_of(group_var)), Q20_item_label, response) %>%
      summarise(n = n(), .groups = "drop") %>%
      group_by(across(all_of(group_var)), Q20_item_label) %>%
      mutate(
        total = sum(n),
        pct = 100 * n / total,
        pct_label = paste0(round(pct), "%")
      )
  } else {
    df_long %>%
      group_by(Q20_item_label, response) %>%
      summarise(n = n(), .groups = "drop") %>%
      group_by(Q20_item_label) %>%
      mutate(
        total = sum(n),
        pct = 100 * n / total,
        pct_label = paste0(round(pct), "%")
      )
  }
}


# Utility: plot function
plot_q20_subgroup <- function(data, facet_var) {
  p <- ggplot(data, aes(x = pct, y = Q20_item_label, fill = response)) +
    geom_col(position = "stack", width = 0.7) +
    geom_text(aes(label = paste0(round(pct), "%")),
              position = position_stack(vjust = 0.5),
              color = "black", size = 3.5) +
    facet_wrap(as.formula(paste("~", facet_var))) +
    scale_fill_manual(values = fill_colors) +
    labs(x = "Percentage of responses", y = "Allocation strategy", fill = "Response") +
    theme_minimal() +
    theme(
      axis.title.x = element_text(vjust = -2, size = 12),
      axis.title.y = element_text(vjust = 2, size = 12),
      axis.text = element_text(size = 11),
      strip.text = element_text(size = 12, face = "bold"),
      legend.position = "bottom",
      legend.text = element_text(size = 12),
      panel.spacing = unit(1, "lines")
    ) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE, reverse = TRUE))
  
  print(p)
}

######################## Plot subgroups: Flying Frequency ########################
flying_levels <- c("Overall", "Non-flyers", "Infrequent flyers", "Frequent flyers")

#### Version which includes "Overall" in a 4-tile facet

# Flying subgroup plot
df_fly <- reshape_q20(df, "flying_frequency") %>%
  filter(!is.na(flying_frequency)) %>%
  aggregate_pct("flying_frequency") %>%
  mutate(flying_frequency = factor(flying_frequency, levels = flying_levels))

# Overall using same utilities (no group var)
df_overall <- reshape_q20(df, NULL) %>%
  aggregate_pct(NULL) %>%
  mutate(flying_frequency = "Overall")

# Combine and factor
df_combined <- bind_rows(df_fly, df_overall) %>%
  mutate(flying_frequency = factor(flying_frequency, levels = flying_levels))

flying_subgroup_plot_overall <- plot_q20_subgroup(df_combined, "flying_frequency")

ggsave(flying_subgroup_plot_overall, filename = "plots/flying_subgroup_budget_overall.png", width = 15, height = 8.5, unit="in", dpi = 300)


#### Version which only includes the three subgroups
flying_subgroup_plot_base <- plot_q20_subgroup(df_fly, "flying_frequency")

ggsave(flying_subgroup_plot_base, filename = "plots/flying_subgroup_budget_base.png", width = 15, height = 4.5, unit="in", dpi = 300)

######################## Supplemental: Plot subgroups: Roles #################################

df_role <- reshape_q20(df, "role_group") %>% aggregate_pct("role_group")
role_subgroup_plot <- plot_q20_subgroup(df_role, "role_group")

ggsave(role_subgroup_plot, filename = "supplemental_material/supp_role_budget.png", width = 15, height = 8.5, unit="in", dpi = 300)

######################## Plot subgroups: Framing groups #################################

df_frame <- reshape_q20(df, "frame") %>% aggregate_pct("frame")
frame_subgroup_plot <- plot_q20_subgroup(df_frame, "frame")

ggsave(frame_subgroup_plot, filename = "supplemental_material/supp_frame_budget.png", width = 15, height = 4.5, unit="in", dpi = 300)