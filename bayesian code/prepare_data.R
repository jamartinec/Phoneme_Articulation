#-------------------------------------
# Prepare data for Bayesian analysis
#-------------------------------------


# Load packages
library(dplyr)
library(tidyr)
library(psych)    # For factor analysis and scree plot
library(readr)   
library(brms)     # For Bayesian analysis
library(splines)  # For natural splines
library(tidyverse)



# Helper information:
# - Code from Tristan: https://www.tjmahr.com/wisclabmisc/articles/index.html
# - Phoneme complexities: Level 1 (vowel), Level 2 (vowel), Level 3 (vowel), Level 3 (consonant), 
#                         Level 4 (vowel), Level 4 (consonant), Level 5 (vowel), Level 5 (consonant), 
#                         Level 6 (consonant), Level 7 (consonant), Level 8 (consonant)


# Load data
#df <- read.csv("../../data/probabilities-max-frame.csv")
df <- read.csv("./data/input_data/probabilities-max-frame.csv")

# Filter to keep rows where expected_phoneme matches phoneme
df <- df %>%
  filter(expected_phoneme == phoneme)

# Geometric mean function
geometric_mean <- function(x) {
  exp(mean(log(x)))  # Geometric mean, avoiding log(0)
}

# Aggregate over instances of phoneme, using geometric mean
df_summary <- df %>%
  group_by(speaker, expected_phoneme) %>%
  summarize(
    mean_prob = geometric_mean(prob), 
    age_months = first(age_months-30),  # Retains the first observed age for each speaker
    .groups = "drop"
  )


# Group phenomes together by type (level and whether consonant or vowel)
#---------------------------------------------------------------------
# Phoneme groups
phoneme_levels <- list(
  Vowels = list(
    Level1 = c("AA", "AH"),
    Level2 = c("UW", "IY", "OW"),
    Level3 = c("EH", "AO", "AY", "AW", "OY"),
    Level4 = c("IH", "EY", "AE", "UH"),
    Level5 = c("ER")
  ),
  Consonants = list(
    Level3 = c("P", "M", "N", "W", "HH"),
    Level4 = c("B", "D", "K", "G", "F", "Y"),
    Level5 = c("T", "R", "L", "NG"),
    Level6 = c("S", "Z", "SH", "ZH", "CH", "JH", "V", "TH", "DH")
  )
)

# Convert the phoneme_levels list into a dataframe for easier merging
phoneme_df <- do.call(rbind, lapply(names(phoneme_levels), function(type) {
  do.call(rbind, lapply(names(phoneme_levels[[type]]), function(level) {
    data.frame(
      expected_phoneme = phoneme_levels[[type]][[level]],
      Type = type,
      Level = level,
      stringsAsFactors = FALSE
    )
  }))
}))

# Join your data frame with the phoneme_df to classify phonemes
df_final <- df_summary %>%
  left_join(phoneme_df, by = "expected_phoneme")

# Count unique phonemes by Type and Level
unique_counts <- df_final %>%
  distinct(expected_phoneme, Type, Level) %>%  # Keep unique phoneme-type-level combinations
  group_by(Type, Level) %>%
  summarise(Unique_Phoneme_Count = n(), .groups = 'drop')

# Display the summary of unique phonemes
print(unique_counts)
# Type       Level  Unique_Phoneme_Count
# <chr>      <chr>                 <int>
#   1 Consonants Level3                  5
# 2 Consonants Level4                    6
# 3 Consonants Level5                    4
# 4 Consonants Level6                    8
# 5 Vowels     Level1                    2
# 6 Vowels     Level2                    3
# 7 Vowels     Level3                    5
# 8 Vowels     Level4                    4
# 9 Vowels     Level5                    1


# Save df_classified as an R data file
#save(df_final, file = "../../data/df_final.RData")
save(df_final, file = "./data/processed_data/df_final.RData")
save(phoneme_levels, file = "./data/processed_data/phoneme_levels.RData")

# Clear everything from the environment except df_classified
rm(list = ls())