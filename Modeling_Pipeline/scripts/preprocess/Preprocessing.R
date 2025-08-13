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
#-------------------------------------------------------------------------------

raw_data_path <- "./Modeling_Pipeline/data/raw/probabilities-max-frame_W.csv.gz"
phoneme_grouping_data_path <- "./Modeling_Pipeline/phoneme_grouping/phoneme_grouping1.csv"
# Define geometric mean
geometric_mean <- function(x) {
  exp(mean(log(x[x > 0]), na.rm = TRUE))  # Avoid log(0) or negative probs
}

export("create_preprocessed_df")
create_preprocessed_df <- function(raw_data_type,model_type, raw_data_path,phoneme_grouping_data_path){
  df <- read.csv(raw_data_path)
  phoneme_df <- read.csv(phoneme_grouping_data_path)
  df_summary <- create_summary(df,raw_data_type, model_type)
  df_final <- df_summary %>%
  left_join(phoneme_df, by = "expected_phoneme")
  phoneme_numscore_mode <- compute_num_score_mode()
  result_list <- list(df_final = df_final, phoneme_numscore_mode = phoneme_numscore_mode)
  return(result_list)
}


compute_num_score_mode<- function(df_summary,model_type = c("beta", "binomial")){
  model_type <- match.arg(model_type)
  if(model_type != "binomial") return(NULL)
    # Group by phoneme and compute mode of num_score
    phoneme_numscore_mode <- df_summary %>%
      group_by(expected_phoneme) %>%
      summarize(
        mode_num_score = get_mode(num_score),
        .groups = "drop"
      )
  
  
  return(phoneme_num_score)
  }

# preprocessing function
create_summary <- function(df, raw_data_type = c("pllr", "aaps"), model_type = c("beta", "binomial")) {
  raw_data_type <- match.arg(raw_data_type)
  model_type <- match.arg(model_type)
  # try to manage exception
  
  if (raw_data_type == "pllr"){
  
    if (model_type == "beta") {
      df_summary <- df %>%
        group_by(speaker, expected_phoneme) %>%
        summarize(
          mean_prob = geometric_mean(prob),
          age_months = first(age_months - 30),
          .groups = "drop"
        )
    } else if (model_type == "binomial") {
      df_summary <- df %>%
        group_by(speaker, expected_phoneme) %>%
        summarize(
          sum_score = sum(phoneme_match),
          num_score = sum(phoneme_match >= 0),
          age_months = first(age_months - 30),
          .groups = "drop"
        )
    }
  } else if (raw_data_type == "aaps" ){
      if (model_type == "beta") {
        df_summary <- df %>%
          group_by(SubjectNum, expected_phoneme) %>%
          summarize(
            mean_prob = mean(Score),
            age_months = first(Age),
            .groups = "drop"
          )
      } else if (model_type == "binomial") {
        df_summary <- df %>%
          group_by(SubjectNum, expected_phoneme) %>%
          summarize(
            sum_score = sum(Score),
            num_score = sum(Score>=0),
            age_months = first(Age),
            .groups = "drop"
          )
      }
  }
  
  return(df_summary)
}



