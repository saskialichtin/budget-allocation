############
## started: 28.05.2025
## file name: data_recoding.R
## context: Preprocessing data for survey and conjoint analysis
############

#load necessary packages
library(tidyverse)
library(data.table)

#Source functions
source("scripts/functions.R")

#load the pseudonymised dataset (private)
df <- read.csv("data/250930_Cleaned_Responses_PSEUDON.csv", sep = ",", dec=",")

#Remove the first and second rows temporarily for analysis
#Row 1: Survey question, Row 2: ImportId
header_rows <- df[1:2, ]  # Store the first and second rows
df <- df[-c(1, 2), ]  # Exclude the first and second rows for processing

############################### Data Recoding ################################### 
#1) Conjoint experiment: Choice data
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

#2) Conjoint experiment: Rating data
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
    )
  ))

#3) Flying frequency (Q1 & Q2)
df <- df %>%
  mutate(
    flying_frequency_numeric = case_when(
      Q1 == "No"                   ~ 0,
      Q2 == "1-2 trips"            ~ 1,
      Q2 == "3-5 trips"            ~ 2,
      Q2 == "6-10 trips"           ~ 3,
      Q2 == "More than 10 trips"   ~ 4,
      Q2 == "Prefer not to say"    ~ NA_real_,
      TRUE                         ~ NA_real_
    )
  )

#4) Role (Q21)
df <- df %>%
  mutate(Q21 = na_if(Q21, "Other / Prefer not to say"))

# Change column name
df <- df %>%
  rename(
    role = Q21
  )

# Cluster flying frequency and role
df <- df %>%
  mutate(
    flying_frequency_numeric = as.character(flying_frequency_numeric),
    flying_frequency = case_when(
      flying_frequency_numeric == "0" ~ "Non-flyers",
      flying_frequency_numeric == "1" ~ "Infrequent flyers",
      flying_frequency_numeric %in% c("2", "3", "4") ~ "Frequent flyers",
      flying_frequency_numeric == "Prefer not to say" ~ NA_character_,
      TRUE ~ flying_frequency_numeric
    ),
    flying_frequency = factor(flying_frequency),  # back to factor
    role_group = case_when(
      role == "Doctoral student" ~ "Doctoral students",
      role %in% c(
        "Postdoctoral Researcher (Postdoc & Scientific Assistant I + II)",
        "Senior Assistant (Senior Assistant I + II & Scientific Collaborator I + II)",
        "Senior Scientist (Senior Scientist I + II / Titulary Professor & Executive Scientific Collaborator I + II)"
      ) ~ "Other scientific staff",
      role == "Full / Associate / Assistant Professor" ~ "Professors",
      TRUE ~ as.character(role)  # fallback to keep original if no match
    )
  )

#5) Perceived relevance of air travel for own work/studies (Q3)
df <- df %>%
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

#6) Approval of the Air Travel Project (Q8)
df <- df %>%
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

#7) Perceived issue importance (Q26)
df <- df %>%
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

############################### Filter dataset to new sample for article ################################### 
#Exclude BSc/MSc students and NAs
df <- df %>%
  filter(!role %in% c("Bachelor's or Master's student"),
         !is.na(role))

# Verify the number of responses after filtering
cat("Number of rows:", nrow(df), "\n")

############################### Data Cleaning 1: Excluding speeders ##################################
# Convert duration column to numeric
df$DurationSeconds <- as.numeric(df$Duration..in.seconds.)

#Convert to minutes
df$DurationMinutes <- df$DurationSeconds/60

#Overview of statistics
summary(df$DurationMinutes)

#Exclude speeders
#Set threshold in minutes
threshold <- 4

# Filter data under threshold
df <- df %>%
  filter(DurationMinutes >= threshold)

# Verify the number of responses after excluding speeders
cat("Number of rows:", nrow(df), "\n")

############################### Data Cleaning 2: Excluding inconsistencies ##################################
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

df %>%
  count(Inconsistencies)

#Exclude responses with 2 or more inconsistencies: Create a new dataframe
df_consistent <- df %>%
  filter(Inconsistencies < 2 | is.na(Inconsistencies))

# Verify the result
cat("Number of rows in df_consistent:", nrow(df_consistent), "\n")

############################### Sample description ################################### 
#Duration
summary(df_consistent$DurationMinutes)

#Role groups
df_consistent %>%
  count(role_group)

#Framing groups
df_consistent %>%
  count(frame)

#Air travel frequency
df_consistent %>%
  count(flying_frequency)

############################### Creating new subset for budget allocation analysis ##################################
#### Create subset of data for conjoint analysis
df_budget <- df_consistent %>% select("ResponseId",
                                    "frame", "role", "role_group", "flying_frequency", 
                                    "Q20_1", "Q20_2", "Q20_3", "Q20_4", "Q20_5", "Q20_6" #budget allocation strategies
                                    )

#Rename headers to include justice principle
df_budget <- df_budget %>%
  rename(
    Q20_1_Egalitarian    = Q20_1,
    Q20_2_Utilitarian    = Q20_2,
    Q20_3_Prioritarian   = Q20_3,
    Q20_4_Proportional   = Q20_4,
    Q20_5_Sufficientarian= Q20_5,
    Q20_6_NeedBased      = Q20_6
  )

#Create additional columns with numeric ratings
df_budget <- df_budget %>%
  mutate(
    across(
      .cols = c(
        Q20_1_Egalitarian,
        Q20_2_Utilitarian,
        Q20_3_Prioritarian,
        Q20_4_Proportional,
        Q20_5_Sufficientarian,
        Q20_6_NeedBased
      ),
      .fns = ~ case_when(
        . == "Strongly disagree"            ~ 1,
        . == "Disagree somewhat"            ~ 2,
        . == "Neither agree nor disagree"   ~ 3,
        . == "Agree somewhat"               ~ 4,
        . == "Strongly agree"               ~ 5,
        . == "Don't know / Prefer not to say" ~ NA_real_,
        TRUE                                ~ NA_real_
      ),
      .names = "{.col}_numeric"
    )
  )

# Save the dataframe as a CSV file
write.csv(df_budget, file = "data/Recoded_Budget_Data.csv", row.names = FALSE)

############################### Creating new subset for conjoint analysis ##################################
# Intra-Respondent Reliablity IRR
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
                                        "frame", "IRR_id", "role", "role_group", "flying_frequency", "Q3_relevance_numeric", "Q26_importance_numeric", "Q8_approval_numeric",
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
df_conj_long <- melt(as.data.table(df_conj), id.vars = c("ResponseId", "frame", "role", "role_group", "flying_frequency", "IRR_id", "Q3_relevance_numeric", "Q26_importance_numeric", "Q8_approval_numeric"))

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
df_cj <- dcast(df_conj_long, ResponseId + round + proposal + frame + role + role_group + flying_frequency + IRR_id + Q3_relevance_numeric + Q26_importance_numeric + Q8_approval_numeric ~ var)  
#ResponseId + round + proposal are row identifiers + all other non-conjoint variables
#after ~ comes the var (all varying fields) -> new column names

head(df_cj)

#Create unique identifier for each respondent and round by adding the round to the end of the responseId
df_cj$idround <- paste0(df_cj$ResponseId, df_cj$round)

#new variable id with unique identifier
id <- df_cj$idround

#Duplicate choices for left and right
#New df where choice column is NA
df_cj2 <- df_cj[!is.na(df_cj$choice), ]
#Subset with only idround and choice columns
df_cj2 <- df_cj2 %>% select(idround, choice)

#Duplicate Rows for L and R proposals
df_cj3 <- df_cj2
df_cj2$idround <- paste0(df_cj2$idround, "R") #Add R to idround of df_cj2
df_cj3$idround <- paste0(df_cj3$idround, "L") #Add L to idround of df_cj3

#merge left and right proposal datasets
df_cj2 <- rbind(df_cj2,df_cj3)

#Updates identifier with new R and L in full dataset
df_cj$idround <- paste0(df_cj$idround, df_cj$proposal)
df_cj = subset(df_cj, select = -c(choice) ) #Remove choice column as it will be merged back later

#merge choice dataset back to full dataset
df_cj <- merge(df_cj2,df_cj,by="idround")

#New column to save original choice
df_cj$choice_original <- df_cj$choice

#Binary code for choice, 1 if chosen, 0 if not chosen
df_cj$choice <- ifelse(df_cj$proposal == "L" & df_cj$choice == "Policy mix 1", 1,
                          ifelse(df_cj$proposal == "R" & df_cj$choice == "Policy mix 2", 1, 0))

#Check how often each level was shown, and which other entries exist in this column to check for errors
table(df_cj$Attr_Economy)
table(df_cj$Attr_Train)
table(df_cj$Attr_SAF)
table(df_cj$Attr_Infrastructure)
table(df_cj$Attr_Limit)
table(df_cj$Attr_Outcome)
table(df_cj$Attr_Rewards)
table(df_cj$Attr_Sharing)
table(df_cj$Attr_Compensation)
table(df_cj$frame)
table(df_cj$IRR_id)

#Check if there are rows with missing values, TRUE = missing values present
summary(is.na(df_cj))

############################### Prep for analysis ################################### 
# Convert all columns to factors (if not already)
df_cj <- df_cj %>%
  mutate(across(everything(), as.factor))

# Convert choice and rating back to numeric
df_cj$choice <- as.numeric(as.character(df_cj$choice))
df_cj$rating <- as.numeric(as.character(df_cj$rating))

# Count NA values in each column for diagnostics; note: 10 rows per participant
na_counts <- sapply(df_cj, function(x) sum(is.na(x)))
print(na_counts[na_counts > 0])

# Rename levels to make them unique by prefixing with the attribute name
df_cj <- df_cj %>%
  mutate(
    Attr_Economy = factor(Attr_Economy, 
                          levels = levels(Attr_Economy),
                          labels = paste("Economy:", levels(Attr_Economy))),
    Attr_Train = factor(Attr_Train, 
                        levels = levels(Attr_Train),
                        labels = paste("Train:", levels(Attr_Train))),
    Attr_SAF = factor(Attr_SAF, 
                      levels = levels(Attr_SAF),
                      labels = paste("SAF:", levels(Attr_SAF))),
    Attr_Infrastructure = factor(Attr_Infrastructure, 
                                 levels = levels(Attr_Infrastructure),
                                 labels = paste("Infrastructure:", levels(Attr_Infrastructure))),
    Attr_Limit = factor(Attr_Limit, 
                        levels = levels(Attr_Limit),
                        labels = paste("Limit:", levels(Attr_Limit))),
    Attr_Rewards = factor(Attr_Rewards, 
                          levels = levels(Attr_Rewards),
                          labels = paste("Rewards:", levels(Attr_Rewards))),
    Attr_Sharing = factor(Attr_Sharing, 
                          levels = levels(Attr_Sharing),
                          labels = paste("Sharing:", levels(Attr_Sharing))),
    Attr_Compensation = factor(Attr_Compensation, 
                               levels = levels(Attr_Compensation),
                               labels = paste("Compensation:", levels(Attr_Compensation)))
  )

############################### Saving Conjoint File ################################### 
write.csv(df_cj, file = "data/Recoded_Conjoint_Data.csv", row.names = FALSE)
