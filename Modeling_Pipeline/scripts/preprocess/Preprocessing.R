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
Paths <- modules::use("./Modeling_Pipeline/pipeline/config/file_paths.R")

# Define a geometric mean function
geometric_mean <- function(x) {
  exp(mean(log(x[x > 0]), na.rm = TRUE))  # Avoid log(0) or negative probs
}

# Helper function to compute the statistical mode
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#' Create and save preprocessed modeling data
#'
#' Reads raw phoneme-level data, applies dataset-specific preprocessing rules,
#' aggregates observations into a modeling-ready summary, and joins phoneme
#' grouping metadata. The resulting datasets are saved to disk following the
#' pipeline directory conventions and returned invisibly for immediate use.
#'
#' In addition to the preprocessed dataset (\code{df_final}), this function
#' computes and saves metadata describing the phoneme number-score mode used
#' for modeling.
#'
#' @param raw_data_type Character scalar. Identifier for the raw data source
#'   (e.g., \code{"aaps"}, \code{"pllr"}).
#' @param model_type Character scalar. Model family or likelihood
#'   (e.g., \code{"beta"}, \code{"binomial"}).
#' @param phoneme_grouping_type Character scalar. Identifier for the phoneme
#'   grouping scheme used.
#' @param raw_data_path Character scalar. Path to the raw input CSV file.
#' @param phoneme_grouping_data_path Character scalar. Path to the CSV file
#'   defining phoneme categories and complexity levels.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{df_final}{Preprocessed and merged dataset used for modeling.}
#'   \item{phoneme_numscore_mode}{Metadata describing the phoneme number-score
#'   mode computed from the summarized data.}
#' }
#'
#' @details
#' Processing steps include:
#' \itemize{
#'   \item Dataset-specific column harmonization (e.g., renaming phoneme columns
#'   for AAPS data).
#'   \item Dataset-specific filtering (e.g., phoneme self-matches for PLLR data).
#'   \item Aggregation via \code{create_summary()}.
#'   \item Joining phoneme grouping metadata.
#'   \item Saving outputs to \code{Paths$Pipeline_preprocesseddata_dir}.
#' }
#'
#' This function has side effects: it writes multiple \code{.rds} files to disk.
#' Existing files are overwritten.
#'
#' @seealso \code{\link{read_preprocessed_files}},
#'   \code{\link{create_summary}},
#'   \code{\link{compute_num_score_mode}}
#'
#' @export
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

#' Create modeling summary from raw phoneme-level data
#'
#' Aggregates raw phoneme-level observations into a speaker-by-phoneme summary
#' suitable for statistical modeling. The exact aggregation logic depends on
#' the data source (\code{raw_data_type}) and the model likelihood
#' (\code{model_type}).
#'
#' For beta models, the summary produces a continuous phoneme-level score
#' (e.g., mean or geometric mean probability). For binomial models, the summary
#' produces success counts and trial totals.
#'
#' @param df A data frame containing raw phoneme-level observations.
#' @param raw_data_type Character scalar. Type of raw data source. Must be one of
#'   \code{"pllr"} or \code{"aaps"}.
#' @param model_type Character scalar. Model likelihood. Must be one of
#'   \code{"beta"} or \code{"binomial"}.
#'
#' @return A data frame with one row per speaker (or subject) and target phoneme,
#' containing aggregated scores and age information. The returned columns depend
#' on the selected \code{raw_data_type} and \code{model_type}.
#'
#' @details
#' The aggregation rules are:
#'
#' \strong{PLLR data}
#' \itemize{
#'   \item \code{beta}: geometric mean of phoneme probabilities
#'     (\code{mean_prob}).
#'   \item \code{binomial}: number of correct phoneme matches
#'     (\code{sum_score}) out of total trials (\code{num_score}).
#' }
#'
#' \strong{AAPS data}
#' \itemize{
#'   \item \code{beta}: arithmetic mean of phoneme scores
#'     (\code{mean_prob}).
#'   \item \code{binomial}: sum of scores and number of trials.
#' }
#'
#' Age is carried forward using the first observed value per speaker–phoneme
#' pair. For PLLR data, an age-shifted version (\code{age_months_shifted}) is
#' also computed for modeling convenience.
#'
#' @seealso \code{\link{create_preprocessed_df}},
#'   \code{\link{compute_num_score_mode}}
#'
#' @export
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


export("auxiliary_preprocess_cache")
auxiliary_preprocess_cache<-function(args) {
  
  key_str <- paste(
    args$raw_data_type,
    args$model_type,
    args$phoneme_grouping_type,
    sep = "|"
  )
  
  message("Preprocessing: ", key_str)
  
    create_preprocessed_df(
    raw_data_type = args$raw_data_type,
    model_type = args$model_type,
    phoneme_grouping_type = args$phoneme_grouping_type,
    raw_data_path = args$raw_data_path,
    phoneme_grouping_data_path = args$phoneme_grouping_data_path
  )
}

export("read_preprocessed_files")
read_preprocessed_files <- function(raw_data_type,
                                    model_type,
                                    phoneme_grouping_type){
  
  
  folder_path <- Paths$Pipeline_preprocesseddata_dir
  prefix_name <- glue("preprocessed_{raw_data_type}_{model_type}_{phoneme_grouping_type}")
  file_name   <- glue("{prefix_name}.rds")
  df_final_file_path <- file.path(folder_path, file_name)
  df_final  <- readRDS(df_final_file_path)
  
  prefix_name <- glue("phoneme_num_score_mode_{raw_data_type}_{model_type}_{phoneme_grouping_type}")
  file_name   <- glue("{prefix_name}.rds")
  phoneme_numscore_mode_file_path <- file.path(folder_path, file_name)
  phoneme_numscore_mode <- readRDS(phoneme_numscore_mode_file_path)
  
  preprocessed_result_list <- list(df_final=df_final,phoneme_numscore_mode=phoneme_numscore_mode)
  
  
  
  return(preprocessed_result_list)
  
}

export("read_preprocessed_from_key")
#read the preprocessed files if they already exist:
read_preprocessed_from_key <- function(key1) {
  
    read_preprocessed_files(
    raw_data_type = key1["raw_data_type"],
    model_type = key1["model_type"],
    phoneme_grouping_type = key1["phoneme_grouping_type"]
  )
}



export("get_preprocessed_for_instance")
get_preprocessed_for_instance <- function(instance, cache) {
  
  key <- make_key_string2(instance$key1)
  cache[[key]]
}

export("make_key_string2")
make_key_string2 <- function(key1) {
  paste(key1, collapse = "|")
}

