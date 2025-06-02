############
## started: 28.05.2025
## file name: supplemental_material.R
## context: Create tables for supplemental materials, based on other scripts
############

# To Do:
# Change p.value levels to start from 0.05 not 0.1 for tables
# Change formatting of tables

library(officer)

############################### Robustness check conjoint: Table to compare controlled ratings with base model ################################### 

# Helper function to extract fixed effects and format the results
get_model_estimates <- function(model, model_name, digits = 2) {
  tidy_model <- broom.mixed::tidy(model, effects = "fixed") %>% 
    mutate(
      stars = case_when(
        p.value < 0.01 ~ "***",
        p.value < 0.05 ~ "**",
        p.value < 0.1  ~ "*",
        TRUE ~ ""
      ),
      estimate_round = round(estimate, digits),
      se_round = round(std.error, digits),
      result = paste0(estimate_round, " (", se_round, ") ", stars),
      Model = model_name
    )
  return(tidy_model)
}

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


############################### Conjoint Subgroup Analysis on the Rating Data ################################### 
# Choice data will be in the article, rating data here
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

# Adapted function for subgroup MMs
get_marginal_means_by_group <- function(model, newdata_vars, group_var, group_level) {
  emm_list <- lapply(newdata_vars, function(attr) {
    formula_attr <- as.formula(paste("~", attr))
    
    emm_obj <- emmeans(model, specs = formula_attr, at = setNames(list(group_level), group_var))
    
    emm_df <- as.data.frame(emm_obj)
    
    emm_df <- emm_df %>%
      rename(
        std.error = SE,
        estimate = emmean,
        conf.low = asymp.LCL,
        conf.high = asymp.UCL
      ) %>%
      mutate(
        term = attr,
        value = .data[[attr]],
        group = group_level
      ) %>%
      select(term, value, estimate, std.error, conf.low, conf.high, group)
    
    return(emm_df)
  })
  
  result <- bind_rows(emm_list)
  return(result)
}

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

############################### Figure: Subgroup analysis flying frequency on rating data ################################### 

#Change dashed line to mean rating
mean_rating <- mean(combined_freq_rating_df$estimate)


figure_subgroup_freq_rating <- ggplot(combined_freq_rating_df, 
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
  guides(
    color = guide_legend(override.aes = list(shape = c(17, 15, 19))),
    shape = "none"
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

print(figure_subgroup_freq_rating)

ggsave(figure_subgroup_freq_rating, filename = "plots/Supplemental_figure_subgroup_freq_rating.png",
       width = 12, height = 8.5, unit = "in", dpi = 300)

