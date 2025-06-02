############
## started: 28.05.2025
## file name: data_recoding.R
## context: Preprocessing data for survey and conjoint analysis
############

#load necessary packages
library(tidyverse)
library(data.table)

#load the pseudonymised dataset (private)
df <- read.csv("data/241216_Finished_Responses_PSEUDON.csv", sep = ",", dec=",")

############################### Data Cleaning ##################################

#Remove the first and second rows temporarily for analysis
#Row 1: Question
#Row 2: ImportId
header_rows <- df[1:2, ]  # Store the first and second rows
df <- df[-c(1, 2), ]  # Exclude the first and second rows for processing

#To see all columns, their data type, and preview of contents
glimpse(df)

# Convert duration column to numeric
df$DurationSeconds <- as.numeric(df$Duration..in.seconds.)

#Convert to minutes
df$DurationMinutes <- df$DurationSeconds/60

#Overview of statistics
summary(df$DurationMinutes)

#Exclude speeders
#Set threshold 
threshold <- 4

# Filter data under threshold
df <- df %>%
  filter(DurationMinutes >= threshold)

################################ Conjoint attributes recoding ################################
#frame = Control
#Choice 1: Q111
#Choice 2: Q52
#Choice 3: Q56
#Choice 4: Q77
#Choice 5: Q81 (intra-participant reliability test)

#frame = Outcome
#Choice 1: Q116
#Choice 2: Q61
#Choice 3: Q65
#Choice 4: Q69
#Choice 5: Q73 (intra-participant reliability test)

#Left = Policy mix 1
#Right = Policy mix 2

# Add new columns for choices and ratings to combine the results from framing and control
# Define the columns to add
columns_to_add <- c(
  "choice_C1", "choice_C2", "choice_C3", "choice_C4", "choice_C5",
  "rating_C1_L", "rating_C2_L", "rating_C3_L", "rating_C4_L", "rating_C5_L",
  "rating_C1_R", "rating_C2_R", "rating_C3_R", "rating_C4_R", "rating_C5_R"
)

# Add new columns to the dataframe
for (col in columns_to_add) {
  if (!col %in% colnames(df)) {
    df[[col]] <- NA  # Initialize missing columns with NA
  }
}

# Inspect the updated dataframe
print(head(df))

# Replace empty strings ("") with NA in all relevant columns
columns_to_clean <- c(
  "Q111", "Q116", "Q52", "Q61", "Q56", "Q65", "Q77", "Q69", "Q81", "Q73",  # Choice columns
  "Q112_1", "Q117_1", "Q112_2", "Q117_2", 
  "Q53_1", "Q62_1", "Q53_2", "Q62_2", 
  "Q57_1", "Q66_1", "Q57_2", "Q66_2", 
  "Q78_1", "Q70_1", "Q78_2", "Q70_2", 
  "Q82_1", "Q74_1", "Q82_2", "Q74_2"   # Rating columns
)

df <- df %>%
  mutate(across(all_of(columns_to_clean), ~ ifelse(. == "", NA, .)))

# Populate choice_C... and rating_C... columns
df <- df %>%
  mutate(
    # Populate "choice_C" columns
    choice_C1 = coalesce(Q111, Q116),
    choice_C2 = coalesce(Q52, Q61),
    choice_C3 = coalesce(Q56, Q65),
    choice_C4 = coalesce(Q77, Q69),
    choice_C5 = coalesce(Q81, Q73),
    
    # Populate "rating_C" columns
    rating_C1_L = coalesce(Q112_1, Q117_1),
    rating_C1_R = coalesce(Q112_2, Q117_2),
    rating_C2_L = coalesce(Q53_1, Q62_1),
    rating_C2_R = coalesce(Q53_2, Q62_2),
    rating_C3_L = coalesce(Q57_1, Q66_1),
    rating_C3_R = coalesce(Q57_2, Q66_2),
    rating_C4_L = coalesce(Q78_1, Q70_1),
    rating_C4_R = coalesce(Q78_2, Q70_2),
    rating_C5_L = coalesce(Q82_1, Q74_1),
    rating_C5_R = coalesce(Q82_2, Q74_2)
  )

#Check result
print(head(df))

#some recoding to make the attribute variables uniform
#Make variables like Economy_C1L into format Economy_C1_L (new space between 1 and L)
colnames(df) <- gsub("(C[1-5])([LR])", "\\1_\\2", colnames(df))

############################### Recoding Ratings ##################################
# Specify the columns to recode
columns_to_recode <- c(
  "rating_C1_L", "rating_C2_L", "rating_C3_L", "rating_C4_L", "rating_C5_L",
  "rating_C1_R", "rating_C2_R", "rating_C3_R", "rating_C4_R", "rating_C5_R"
)

# Recode the columns
df <- df %>%
  mutate(across(
    all_of(columns_to_recode),
    ~ case_when(
      . == "Strongly oppose" ~ 1,
      . == "Somewhat oppose" ~ 2,
      . == "Neither oppose nor support" ~ 3,
      . == "Somewhat support" ~ 4,
      . == "Strongly support" ~ 5,
      . == "Prefer not to say" ~ NA_real_,
      . == "" ~ NA_real_,  # Handle empty strings
      TRUE ~ NA_real_  # Default for unexpected values
    ),
    .names = "{.col}"  # Optional: Save recoded values in new columns
  ))

#Check for NA's (should still be included in ratings)
# Define the rating columns to inspect
rating_columns <- c(
  "rating_C1_L", "rating_C2_L", "rating_C3_L", "rating_C4_L", "rating_C5_L",
  "rating_C1_R", "rating_C2_R", "rating_C3_R", "rating_C4_R", "rating_C5_R"
)

# Create a summary table for all rating columns
rating_summary <- lapply(rating_columns, function(col) {
  table(df[[col]], useNA = "ifany")  # Count each option, including NA
})

# Print results for each rating column
names(rating_summary) <- rating_columns
for (col in rating_columns) {
  cat("Counts for", col, ":\n")
  print(rating_summary[[col]])
  cat("\n")
}
#Here, NAs still exist

############################### Inspect Inconsistencies in Choices vs. Ratings ##################################

# Add a new column to count inconsistencies per row; making sure NAs don't count (if one of the ratings is NA, it will not count as an inconsistency)
df <- df %>%
  mutate(
    Inconsistencies = 
      coalesce(choice_C1 == "Policy mix 1" & rating_C1_R > rating_C1_L, FALSE) +
      coalesce(choice_C1 == "Policy mix 2" & rating_C1_L > rating_C1_R, FALSE) +
      coalesce(choice_C2 == "Policy mix 1" & rating_C2_R > rating_C2_L, FALSE) +
      coalesce(choice_C2 == "Policy mix 2" & rating_C2_L > rating_C2_R, FALSE) +
      coalesce(choice_C3 == "Policy mix 1" & rating_C3_R > rating_C3_L, FALSE) +
      coalesce(choice_C3 == "Policy mix 2" & rating_C3_L > rating_C3_R, FALSE) +
      coalesce(choice_C4 == "Policy mix 1" & rating_C4_R > rating_C4_L, FALSE) +
      coalesce(choice_C4 == "Policy mix 2" & rating_C4_L > rating_C4_R, FALSE) +
      coalesce(choice_C5 == "Policy mix 1" & rating_C5_R > rating_C5_L, FALSE) +
      coalesce(choice_C5 == "Policy mix 2" & rating_C5_L > rating_C5_R, FALSE)
  )

# View the first few rows
head(df$Inconsistencies)


# Plot the distribution of inconsistencies with labels above the bars
ggplot(df, aes(x = Inconsistencies)) +
  geom_bar(fill = "skyblue", color = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 4) +
  labs(
    title = "Distribution of Inconsistencies per Respondent",
    x = "Number of Inconsistencies",
    y = "Count of Respondents"
  ) +
  theme_minimal()

# Check for patterns of inconsistencies per role
summary_table <- df %>%
  group_by(Q21) %>%
  summarise(
    Mean = mean(Inconsistencies, na.rm = TRUE),
    Median = median(Inconsistencies, na.rm = TRUE),
    SD = sd(Inconsistencies, na.rm = TRUE),
    Count = n()
  )

print(summary_table)

############################### Data Cleaning 2: Excluding inconsistencies ##################################
#Exclude responses with 2 or more inconsistencies:
# Create a new dataframe
df_consistent <- df %>%
  filter(Inconsistencies < 2 | is.na(Inconsistencies))

# Verify the result
cat("Number of rows in df_consistent:", nrow(df_consistent), "\n")

#Check again if NA's exist (as they should)
# Define the rating columns to inspect
rating_columns <- c(
  "rating_C1_L", "rating_C2_L", "rating_C3_L", "rating_C4_L", "rating_C5_L",
  "rating_C1_R", "rating_C2_R", "rating_C3_R", "rating_C4_R", "rating_C5_R"
)

# Create a summary table for all rating columns
rating_summary <- lapply(rating_columns, function(col) {
  table(df_consistent[[col]], useNA = "ifany")  # Count each option, including NA
})

# Print results for each rating column
names(rating_summary) <- rating_columns
for (col in rating_columns) {
  cat("Counts for", col, ":\n")
  print(rating_summary[[col]])
  cat("\n")
}

#Duration after data cleaning for sample description
summary(df_consistent$DurationMinutes)

############################### Recode other survey questions #################################### 

#Recode perceived issue importance (Q26)
df_consistent <- df_consistent %>%
  mutate(
    Q26_importance_numeric = case_when(
      Q26 == "Very unimportant"                    ~ 1,
      Q26 == "More unimportant than important"     ~ 2,
      Q26 == "Neither important nor unimportant"   ~ 3,
      Q26 == "More important than unimportant"     ~ 4,
      Q26 == "Very important"                      ~ 5,
      Q26 == "Don’t know / Prefer not to say"      ~ NA_real_,
      TRUE                                         ~ NA_real_
    )
  )

df_consistent %>%
  count(Q26_importance_numeric)

#Recode flying frequency (Q1 & Q2)
df_consistent <- df_consistent %>%
  mutate(
    flying_frequency = case_when(
      Q1 == "No"                   ~ 0,
      Q2 == "1-2 trips"            ~ 1,
      Q2 == "3-5 trips"            ~ 2,
      Q2 == "6-10 trips"           ~ 3,
      Q2 == "More than 10 trips"   ~ 4,
      Q2 == "Prefer not to say"    ~ NA_real_,
      TRUE                         ~ NA_real_
    )
  )

df_consistent %>%
  count(flying_frequency)

#Recode perceived relevance of air travel for own work/studies (Q3)
df_consistent <- df_consistent %>%
  mutate(
    Q3_relevance_numeric = case_when(
      Q3 == "Very irrelevant"                    ~ 1,
      Q3 == "More irrelevant than relevant"      ~ 2,
      Q3 == "Neither relevant nor irrelevant"    ~ 3,
      Q3 == "More relevant than irrelevant"      ~ 4,
      Q3 == "Very relevant"                      ~ 5,
      Q3 == "Don’t know / Prefer not to say"     ~ NA_real_,
      TRUE                                       ~ NA_real_
    )
  )
df_consistent %>%
  count(Q3_relevance_numeric)

#Recode approval of the Air Travel Project (Q8)
df_consistent <- df_consistent %>%
  mutate(
    Q8_approval_numeric = case_when(
      Q8 == "Strongly disapprove"           ~ 1,
      Q8 == "Disapprove somewhat"           ~ 2,
      Q8 == "Neither approve nor disapprove"~ 3,
      Q8 == "Approve somewhat"              ~ 4,
      Q8 == "Strongly approve"              ~ 5,
      Q8 == "Don’t know / Prefer not to say"~ NA_real_,
      TRUE                                  ~ NA_real_
    )
  )

df_consistent %>%
  count(Q8_approval_numeric)

#Recode role (Q21)
df_consistent <- df_consistent %>%
  mutate(Q21 = na_if(Q21, "Other / Prefer not to say"))

#Recode age (Q22)
df_consistent <- df_consistent %>%
  mutate(Q22 = na_if(Q22, "Prefer not to say"))

#Recode dept. (Q23)
df_consistent <- df_consistent %>%
  mutate(Q23 = na_if(Q23, "Other / Prefer not to say"))

# Change column names
df_consistent <- df_consistent %>%
  rename(
    role = Q21,
    age_group = Q22,
    domain = Q23
  )

############################### Creating new subset for conjoint analysis ##################################

# Intra-Respondent Reliablity
df_consistent <- df_consistent %>%
  mutate(IRR_id = case_when(
    (choice_C1 == "Policy mix 1" & choice_C5 == "Policy mix 2") ~ "reliable",
    (choice_C1 == "Policy mix 2" & choice_C5 == "Policy mix 1") ~ "reliable",
    TRUE ~ "unreliable"
  ))

#### Create subset of data for conjoint analysis
df_conj <- df_consistent %>% select("ResponseId", 
                                        "choice_C1", "choice_C2", "choice_C3", "choice_C4", "choice_C5",
                                        "rating_C1_L", "rating_C2_L", "rating_C3_L", "rating_C4_L", "rating_C5_L",
                                        "rating_C1_R", "rating_C2_R", "rating_C3_R", "rating_C4_R", "rating_C5_R", 
                                        "frame", "IRR_id", "role", "flying_frequency", "Q3_relevance_numeric", "Q26_importance_numeric", "Q8_approval_numeric", 
                                        "age_group", "domain", #age and dept.
                                        # Left Columns (C1_L to C5_L)
                                        "Economy_C1_L", "Train_C1_L", "SAF_C1_L", "Infrastructure_C1_L", "Limit_C1_L", "Outcome_C1_L", "Rewards_C1_L", "Sharing_C1_L", "Compensation_C1_L",
                                        "Economy_C2_L", "Train_C2_L", "SAF_C2_L", "Infrastructure_C2_L", "Limit_C2_L", "Outcome_C2_L", "Rewards_C2_L", "Sharing_C2_L", "Compensation_C2_L",
                                        "Economy_C3_L", "Train_C3_L", "SAF_C3_L", "Infrastructure_C3_L", "Limit_C3_L", "Outcome_C3_L", "Rewards_C3_L", "Sharing_C3_L", "Compensation_C3_L",
                                        "Economy_C4_L", "Train_C4_L", "SAF_C4_L", "Infrastructure_C4_L", "Limit_C4_L", "Outcome_C4_L", "Rewards_C4_L", "Sharing_C4_L", "Compensation_C4_L",
                                        "Economy_C5_L", "Train_C5_L", "SAF_C5_L", "Infrastructure_C5_L", "Limit_C5_L", "Outcome_C5_L", "Rewards_C5_L", "Sharing_C5_L", "Compensation_C5_L",
                                        # Right Columns (C1_R to C5_R)
                                        "Economy_C1_R", "Train_C1_R", "SAF_C1_R", "Infrastructure_C1_R", "Limit_C1_R", "Outcome_C1_R", "Rewards_C1_R", "Sharing_C1_R", "Compensation_C1_R",
                                        "Economy_C2_R", "Train_C2_R", "SAF_C2_R", "Infrastructure_C2_R", "Limit_C2_R", "Outcome_C2_R", "Rewards_C2_R", "Sharing_C2_R", "Compensation_C2_R",
                                        "Economy_C3_R", "Train_C3_R", "SAF_C3_R", "Infrastructure_C3_R", "Limit_C3_R", "Outcome_C3_R", "Rewards_C3_R", "Sharing_C3_R", "Compensation_C3_R",
                                        "Economy_C4_R", "Train_C4_R", "SAF_C4_R", "Infrastructure_C4_R", "Limit_C4_R", "Outcome_C4_R", "Rewards_C4_R", "Sharing_C4_R", "Compensation_C4_R",
                                        "Economy_C5_R", "Train_C5_R", "SAF_C5_R", "Infrastructure_C5_R", "Limit_C5_R", "Outcome_C5_R", "Rewards_C5_R", "Sharing_C5_R", "Compensation_C5_R")


#melt in long format
df_conj_long <- melt(as.data.table(df_conj), id.vars = c("ResponseId", "frame", "role", "flying_frequency", "IRR_id", "Q3_relevance_numeric", "Q26_importance_numeric", "Q8_approval_numeric", "age_group", "domain"))

#add column proposal and recode proposal left/right
df_conj_long$proposal <- NA
df_conj_long$proposal[grep("_R", df_conj_long$variable)] <- "R"
df_conj_long$proposal[grep("_L", df_conj_long$variable)] <- "L"

#set conjoint comparison round
df_conj_long$round <- NA
for (i in 1:5) {
  df_conj_long$round[grep(paste0("C",i), df_conj_long$variable)] <- i
}

#set attribute names
df_conj_long$var <- NA #Column for all fields that vary between people/choices
for (i in c("Economy", "Train", "SAF", "Infrastructure", "Limit", "Outcome", "Rewards", "Sharing", "Compensation")) {
  df_conj_long$var[grep(paste0(i), df_conj_long$variable)] <- paste0("Attr_",i)
}

#rating
df_conj_long$var[grep("rating", df_conj_long$variable)] <- "rating"


#choice
df_conj_long$var[grep("choice", df_conj_long$variable)] <- "choice"

#Remove the variable column (not the var)
df_conj_long <- df_conj_long %>% select(-variable)

#Reshape long to wide format in new data frame
df_conj1 <- dcast(df_conj_long, ResponseId + round + proposal + frame + role + flying_frequency + IRR_id + Q3_relevance_numeric + Q26_importance_numeric + Q8_approval_numeric + age_group + domain ~ var)  
#ResponseId + round + proposal are row identifiers + all other non-conjoint variables
#after ~ comes the var (all varying fields) -> new column names

head(df_conj1)

#Create unique identifier for each respondent and round by adding the round to the end of the responseId
df_conj1$idround <- paste0(df_conj1$ResponseId, df_conj1$round)

#new variable id with unique identifier
id <- df_conj1$idround

#Duplicate choices for left and right
#New df where choice column is NA
df_conj2 <- df_conj1[!is.na(df_conj1$choice), ]
#Subset with only idround and choice columns
df_conj2 <- df_conj2 %>% select(idround, choice)

#Duplicate Rows for L and R proposals
df_conj3 <- df_conj2
df_conj2$idround <- paste0(df_conj2$idround, "R") #Add R to idround of df_conj2
df_conj3$idround <- paste0(df_conj3$idround, "L") #Add L to idround of df_conj3

#merge left and right proposal datasets
df_conj2 <- rbind(df_conj2,df_conj3)

#Updates identifier with new R and L in full dataset
df_conj1$idround <- paste0(df_conj1$idround, df_conj1$proposal)
df_conj1 = subset(df_conj1, select = -c(choice) ) #Remove choice column as it will be merged back later

#merge choice dataset back to full dataset
df_conj1 <- merge(df_conj2,df_conj1,by="idround")

#New column to save original choice
df_conj1$choice_original <- df_conj1$choice

#Binary code for choice, 1 if chosen, 0 if not chosen
df_conj1$choice <- ifelse(df_conj1$proposal == "L" & df_conj1$choice == "Policy mix 1", 1,
                          ifelse(df_conj1$proposal == "R" & df_conj1$choice == "Policy mix 2", 1, 0))

#Check how often each level was shown, and which other entries exist in this column to check for errors
table(df_conj1$Attr_Economy)
table(df_conj1$Attr_Train)
table(df_conj1$Attr_SAF)
table(df_conj1$Attr_Infrastructure)
table(df_conj1$Attr_Limit)
table(df_conj1$Attr_Outcome)
table(df_conj1$Attr_Rewards)
table(df_conj1$Attr_Sharing)
table(df_conj1$Attr_Compensation)
table(df_conj1$frame)
table(df_conj1$role)
table(df_conj1$flying_frequency)
table(df_conj1$IRR_id)

#Check if there are rows with missing values, TRUE = missing values present
summary(is.na(df_conj1))

############################### Saving Conjoint File ################################### 
write.csv(df_conj1, file = "data/Recoded_Conjoint_Data.csv", row.names = FALSE)
