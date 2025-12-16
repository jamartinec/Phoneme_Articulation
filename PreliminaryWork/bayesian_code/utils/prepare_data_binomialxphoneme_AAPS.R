#-------------------------------------------------------------
# Prepare AAPS (empirical/clinical) data for Bayesian analysis
#-------------------------------------------------------------
# Load packages
library(dplyr)
library(tidyr)
library(psych)    # For factor analysis and scree plot
library(readr)   
library(brms)     # For Bayesian analysis
library(splines)  # For natural splines
library(tidyverse)
library(ggplot2)


# Helper information:
# - Code from Tristan: https://www.tjmahr.com/wisclabmisc/articles/index.html
# - Phoneme complexities: Level 1 (vowel), Level 2 (vowel), Level 3 (vowel), Level 3 (consonant), 
#                         Level 4 (vowel), Level 4 (consonant), Level 5 (vowel), Level 5 (consonant), 
#                         Level 6 (consonant), Level 7 (consonant), Level 8 (consonant)

#rm(list=ls())

# Load data
#df <- read.csv("../../data/probabilities-max-frame.csv")
#df <- read.csv("./data/input_data/probabilities-max-frame.csv.gz")
df <- read.csv("./data/input_data/AAPS Score Data (Long Version).csv")

# Filter to keep rows where expected_phoneme matches phoneme
# df <- df %>%
#   filter(expected_phoneme == phoneme)

df <- df %>%
  mutate(expected_phoneme = Phoneme)

# Geometric mean function
geometric_mean <- function(x) {
  exp(mean(log(x)))  # Geometric mean, avoiding log(0)
}

# Aggregate over instances of phoneme, using geometric mean
# df_summary <- df %>%
#   group_by(SubjectNum, expected_phoneme) %>%
#   summarize(
#     mean_prob = mean(Score), 
#     age_months = first(Age),  # Retains the first observed age for each speaker
#     .groups = "drop"
#   )

df_summary <- df %>%
  group_by(SubjectNum, expected_phoneme) %>%
  summarize(
    sum_score = sum(Score),
    num_score = sum(Score>=0),
    age_months = first(Age),
    .groups = "drop"
  )


# ggplot(df_summary, aes(x = mean_prob)) +
#   geom_histogram(binwidth = 0.05, fill = "steelblue", color = "white") +
#   labs(
#     title = "Histogram of Mean Probabilities",
#     x = "Mean Probability",
#     y = "Count"
#   ) +
#   theme_minimal(base_size = 14)



# Helper function to compute the statistical mode
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Group by phoneme and compute mode of num_score
phoneme_numscore_mode <- df_summary %>%
  group_by(expected_phoneme) %>%
  summarize(
    mode_num_score = get_mode(num_score),
    .groups = "drop"
  )





# Group phenomes together by type (level and whether consonant or vowel)
#-------------------------------------------------------------------------------------------
# Phoneme groups
# phoneme_levels <- list(
#   Vowels = list(
#     Level1 = c("AA", "AH"),
#     Level2 = c("UW", "IY", "OW"),
#     Level3 = c("EH", "AO", "AY", "AW", "OY"),
#     Level4 = c("IH", "EY", "AE", "UH"),
#     Level5 = c("ER")
#   ),
#   Consonants = list(
#     Level3 = c("P", "M", "N", "W", "HH"),
#     Level4 = c("B", "D", "K", "G", "F", "Y"),
#     Level5 = c("T", "R", "L", "NG"),
#     Level6 = c("S", "Z", "SH", "ZH", "CH", "JH", "V", "TH", "DH")
#   )
# )

# This time we want to create a single phoneme level that contains all phonemes, because we want 
# to fit a model per phoneme
# ------------------------------------------------------------------------------------------------
# Phoneme_groups
phoneme_levels <- list(
  Vowels = list(
    Levelphoneme1 = c("AA"),
    Levelphoneme2 = c("AH"),
    Levelphoneme3 = c("UW"),
    Levelphoneme4 = c("IY"),
    Levelphoneme5 = c("OW"),
    Levelphoneme6 = c("EH"),
    Levelphoneme7 = c("AO"),
    Levelphoneme8 = c("AY"),
    Levelphoneme9 = c("AW"),
    Levelphoneme10 = c("OY"),
    Levelphoneme11 = c("IH"),
    Levelphoneme12 = c("EY"),
    Levelphoneme13 = c("AE"),
    Levelphoneme14 = c("UH"),
    Levelphoneme15 = c("ER")
    
  ),
  Consonants = list(
    Levelphoneme16 = c("P"),
    Levelphoneme17 = c("M"),
    Levelphoneme18 = c("N"),
    Levelphoneme19 = c("W"),
    Levelphoneme20 = c("HH"),
    Levelphoneme21 = c("B"),
    Levelphoneme22 = c("D"),
    Levelphoneme23 = c("K"),
    Levelphoneme24 = c("G"),
    Levelphoneme25 = c("F"),
    Levelphoneme26 = c("Y"),
    Levelphoneme27 = c("T"),
    Levelphoneme28 = c("R"),
    Levelphoneme29 = c("L"),
    Levelphoneme30 = c("NG"),
    Levelphoneme31 = c("S"),
    Levelphoneme32 = c("Z"),
    Levelphoneme33 = c("SH"),
    Levelphoneme34 = c("ZH"),
    Levelphoneme35 = c("CH"),
    Levelphoneme36 = c("JH"),
    Levelphoneme37 = c("V"),
    Levelphoneme38 = c("TH"),
    Levelphoneme39 = c("DH")
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

glimpse(df_final)

# Count unique phonemes by Type and Level
unique_counts <- df_final %>%
  distinct(expected_phoneme, Type, Level) %>%  # Keep unique phoneme-type-level combinations
  group_by(Type, Level) %>%
  summarise(Unique_Phoneme_Count = n(), .groups = 'drop')

# Display the summary of unique phonemes
print(unique_counts)


# Save df_classified as an R data file
#save(df_final, file = "../../data/df_finalVersion2.RData")
save(df_final, file = "./data/processed_data/df_final_binomialxphoneme_AAPS.RData")
save(phoneme_levels, file = "./data/processed_data/phoneme_levels_binomialxphoneme_AAPS.RData")
save(phoneme_numscore_mode,file = "./data/processed_data/phoneme_numscore_mode_binomialxphoneme_AAPS.RData")


# Clear everything from the environment except df_classified
rm(list = ls())