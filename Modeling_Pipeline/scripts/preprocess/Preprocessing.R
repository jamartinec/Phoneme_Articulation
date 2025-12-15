#-------------------------------------
# Prepare data for Bayesian modeling
#-------------------------------------
# Load required packages
import("dplyr")
import("tidyr")
import("psych")    # For factor analysis and scree plot
import("readr")   
import("brms")     # For Bayesian analysis
import("splines")  # For natural splines
import("tidyverse")
import("utils")
import("glue")
#-------------------------------------------------------------------------------
Paths <- modules::use("./bayesian_code/utils/file_paths.R")

# Define a geometric mean function
geometric_mean <- function(x) {
  exp(mean(log(x[x > 0]), na.rm = TRUE))  # Avoid log(0) or negative probs
}

# Helper function to compute the statistical mode
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

export("create_preprocessed_df")
create_preprocessed_df <- function(raw_data_type,model_type,phoneme_grouping_type, raw_data_path,phoneme_grouping_data_path){
  dftest <-read.csv(raw_data_path)
  
  #message("colnames")
  #print(colnames(dftest))
  df <- read.csv(raw_data_path)%>%
    { if (raw_data_type == "aaps")
      dplyr::rename(., expected_phoneme = Phoneme)
      else
        .
    }
  
  #print(colnames(df))
  if (raw_data_type== "pllr"){
    df <- df%>%
      dplyr::filter(expected_phoneme == phoneme)
  }
  
  
  phoneme_df <- read.csv(phoneme_grouping_data_path)
  
  df_summary <- create_summary(df,raw_data_type, model_type)
  df_final <- df_summary %>%
  left_join(phoneme_df, by = "expected_phoneme")
  
  # print(df)
 
  # Save `df_final`.
  # Consider defining a function to check whether the file already exists
  # and load it instead of recomputing.
  
  
  folder_path <- Paths$Pipeline_preprocesseddata_dir
  prefix_name <- glue("preprocessed_{raw_data_type}_{model_type}_{phoneme_grouping_type}")
  file_name   <- glue("{prefix_name}.rds")
  df_final_file_path <- file.path(folder_path, file_name)
  #ensure_dir(subfolder_path)  # <<< create the directory tree here
  #ensure_parent_dir(df_final_file_path)  # <<< make sure parent exists before save
  saveRDS(df_final, df_final_file_path)
  message(glue(
    "preprocessed data (df_final: {raw_data_type}, {model_type}, {phoneme_grouping_type}) created and saved."
  ))
  
  # Save the computed `phoneme_numscore_mode`
  phoneme_numscore_mode <- compute_num_score_mode(df_summary,model_type)
  
  prefix_name <- glue("phoneme_num_score_mode_{raw_data_type}_{model_type}_{phoneme_grouping_type}")
  file_name   <- glue("{prefix_name}.rds")
  phoneme_numscore_mode_file_path <- file.path(folder_path, file_name)
  saveRDS(phoneme_numscore_mode, phoneme_numscore_mode_file_path)
  message(glue(
    "preprocessed data (phoneme_numscore_mode: {raw_data_type}, {model_type}, {phoneme_grouping_type}) created and saved."
  ))
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
  
  
  return(phoneme_numscore_mode)
  }

# Preprocessing function
create_summary <- function(df, raw_data_type = c("pllr", "aaps"), model_type = c("beta", "binomial")) {
  raw_data_type <- match.arg(raw_data_type)
  model_type <- match.arg(model_type)
  
  # Attempt to handle exceptional cases
  
  if (raw_data_type == "pllr"){
  
    if (model_type == "beta") {
      df_summary <- df %>%
        group_by(speaker, expected_phoneme) %>%
        summarize(
          mean_prob = geometric_mean(prob),
          age_months = first(age_months),
          age_months_shifted = first(age_months - 30),
          .groups = "drop"
        )
    } else if (model_type == "binomial") {
      df <- df %>% mutate(
        phoneme_match = if_else(expected_phoneme==most_likely_phoneme, 1L, 0L)
        )
      df_summary <- df %>%
        group_by(speaker, expected_phoneme) %>%
        summarize(
          sum_score = sum(phoneme_match),
          num_score = sum(phoneme_match >= 0),
          age_months = first(age_months),
          age_months_shifted = first(age_months - 30),
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



