library(tidyverse)
Paths <- modules::use("./bayesian_code/utils/file_paths.R")

#' extract_validation_results
#'
#' Load the validation results for a list of models saved as Rds file and 
#' extract some attributes useful for comparison and report.
#'
#' @param filename Name of the Rds file that contains the validation results
#' @return dataframe which contains the wac and loo scores for each model in the list loaded
#' @export
extract_validation_results <- function(filename){
  folder_path <- Paths$model_valresults_dir
  filename <- paste0(filename,".rds")
  file_path <- file.path(folder_path,filename)
  model_validation_results <- readRDS(file_path)
  
  rows <- list()
  for (key in names(model_validation_results)){
    subdict <- model_validation_results[[key]]
    waic_table <- subdict["waic"]$waic$estimates
    elpd_waic <- waic_table["elpd_waic","Estimate"]
    p_waic <- waic_table["p_waic","Estimate"]
    waic <- waic_table["waic","Estimate"]
    loo_table2 <- subdict["loo"]$loo$estimates
    elpd_loo <- loo_table2["elpd_loo","Estimate"]
    p_loo <- loo_table2["p_loo","Estimate"]
    looic <- loo_table2["looic","Estimate"]
    
    extracted <- list(
      model_opt = subdict["model_opt"],
      phoneme_group = subdict["phoneme_group_str"],
      elpd_waic = elpd_waic,
      p_waic = p_waic,
      waic = waic,
      elpd_loo = elpd_loo,
      p_loo = p_loo,
      looic = looic
    )
    rows[[length(rows) + 1]] <- extracted
  }
  result_df <- do.call(rbind, lapply(rows, as.data.frame))
  rownames(result_df) <- NULL 
  return(result_df)
}

#' stack_validation_results
#'
#' Load the validation results for a list of models saved as Rds file and 
#' extract some attributes useful for comparison and report.
#'
#' @param result_df dataframe that contains the stacked validation results
#' @param output_filename a string corresponding to the name of the output (pivot table) csv file
#' @return pivot_combined dataframe which contains the summary report with loo 
#' and waic scores for the models/data contained in the 'initial Rds file
#' @export
stack_validation_results <- function (result_df,output_filename){
  # Pivot for elpd_waic
  waic_pivot <- result_df %>%
    select(phoneme_group_str, model_opt, elpd_waic) %>%
    pivot_wider(names_from = model_opt, values_from = elpd_waic, names_prefix = "waic_")
  
  # Pivot for elpd_loo
  loo_pivot <- result_df %>%
    select(phoneme_group_str, model_opt, elpd_loo) %>%
    pivot_wider(names_from = model_opt, values_from = elpd_loo, names_prefix = "loo_")
  
  
  pivot_combined <- left_join(waic_pivot, loo_pivot, by = "phoneme_group_str")
  print(pivot_combined)
}

#' combine_validation_reports
#'
#' Load all the csv files which starts with 'pivot' and combine them in a single
#' summary report.
#'
#' @param list Name of the Rds file that contains the validation results
#' @return dataframe which contains the wac and loo scores for each model in the list loaded
#' @export
combine_validation_reports <- function(file_list = NULL,
                                       combine_all = FALSE,
                                       folder_path = Paths.model_valreports_dir
                                       ){
  # Check for readr package
  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("Please install the 'readr' package.")
  }
  
  # Use file_list if provided
  if (!is.null(file_list)) {
    file_paths <- file_list
  } else if (combine_all) {
    file_paths <- list.files(path = folder_path, 
                             pattern = "^pivot_combined.*\\.csv$", 
                             full.names = TRUE)
    if (length(file_paths) == 0) {
      stop("No CSV files found in the folder.")
    }
  } else {
    stop("Either provide 'file_list' or set 'combine_all = TRUE'.")
  }
  
  # Read all CSVs
  df_list <- lapply(file_paths, readr::read_csv)
  # Left join by "phoneme_group_str"
  joined_df <- Reduce(function(x, y) dplyr::left_join(x, y, by = "phoneme_group_str"), df_list)
  # Reorder columns: keep first column, sort the rest
  joined_df <- joined_df[, c(1, order(names(joined_df)[-1]) + 1)]
  return(joined_df)
  write.csv(joined_df, file = "./bayesian_code/model_validation/validation_results.csv", row.names = FALSE)
  
}
## if there are several score files from previous experiments unify them

file_path1 <- "./bayesian_code/model_validation/pivot_combined.csv"
file_path2 <- "./bayesian_code/model_validation/pivot_combined2.csv"

file_paths <-list(file_path1, file_path2)
# call the functions

build_validation <- function(input_filename, output_filename){
  result_df <- extract_validation_results(input_filename)
  print(result_df)
  stack_name <- paste0(input_filename,"_stack.rds")
  file_path <- file.path(Paths$model_valresults_dir,stack_name)
  saveRDS(result_df, file = file_path)
  
  pivot_combined<-stack_validation_results(result_df,output_filename)
  output_filename <- paste0(output_filename,".csv")
  pivot_file_path <- file.path(Paths$model_valreports_dir,output_filename)
  write.csv(pivot_combined, file = pivot_file_path, row.names = FALSE)
}

build_validation( "model_validation_resultsTRIQUIS","pivot_combinedTRIQUIS")