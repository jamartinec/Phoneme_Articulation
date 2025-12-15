#-----------------------
# Run Bayesian analysis
#-----------------------
import("dplyr")
import("tidyr")
import("readr") 
import("brms") # Run Bayesian modeling
import("splines") # For natural splines
import("tidyverse")
import("utils")
import("glue")

# Load file path
Paths <- modules::use("./bayesian_code/utils/file_paths.R")
brms_help <- modules::use("./bayesian_code/utils/tristan_brm_helper.R")


# Move to utils 
ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

ensure_parent_dir <- function(file_path) {
  ensure_dir(dirname(file_path))
}


# Fit a Bayesian model for a single phoneme group and save the fitted object.
# The model is always refitted and augmented with validation criteria (LOO, WAIC).
export("fit_bayesian_model_funct")
fit_bayesian_model_funct <- function(model_specific,
                                     prior_specific,
                                     df_filtered,
                                     fitted_model_dir,
                                     fitted_model_file_path,
                                     phoneme_group_str
){
  
  
  
  model_name = paste0(fitted_model_file_path,".rds")
  ensure_dir(fitted_model_dir)
  
  
  # Be careful with the `file_refit` argument
  brm_args <- brms_help$brms_args_create()
  args <- brm_args(formula = model_specific,
                   data = df_filtered,
                   prior = prior_specific,
                   file = fitted_model_file_path,
                   # The model is always refitted, even if an object with the same name already exists.
                   file_refit = "always", 
                   seed = 20250625,
                   chains = 4,
                   iter  = 4000,
                   cores = 4#,
                   #...
  )
  args$backend <- "cmdstanr"
  cat("  → Fitting model: ", model_name, "\n")
  model <- do.call(brm, args)
  # Add validation criteria (e.g., LOO and WAIC) and save the fitted model
  cat("  → Adding validation criteria (loo, waic)...\n")
  model <- brms_help$add_validation_criterion(
    model, 
    val_list=c("loo","waic"), 
    use_reloo = FALSE)
  saveRDS(model, file = paste0(fitted_model_file_path,".rds"))
  rm(model)
  gc()
  
}

# Fit a single model instance using its associated filtered data
fit_one <- function(item) {
  # Read only the filtered data required for this instance
  df_filtered <- readRDS(item$filtered_file_path)
  fit_bayesian_model_funct(
    model_specific        = item$model_opt,
    prior_specific        = item$prior_specific,
    df_filtered           = df_filtered,
    fitted_model_dir      = item$fitted_model_dir,
    fitted_model_file_path= item$fitted_model_file_path,
    phoneme_group_str     = item$phoneme_group_str
  )
  rm(df_filtered); invisible(gc())
}


# Iterate over all model instances and run Bayesian fitting with error handling
export("iterate_run_bayesian_modeling")
#iterate_run_bayesian_modeling <- function(raw_data_type,model_type,phoneme_grouping_type,list_of_instances){
iterate_run_bayesian_modeling <- function(list_of_instances){  
  
  
  # Collect failures without stopping the pipeline and write a persistent log
  # with timestamps for later inspection or reruns.
  failures <- list()
  purrr::iwalk(list_of_instances, function(item, idx) {
    cat("\n--- Running:", item$model_name, item$category, item$phoneme_group_str, "\n")
    tryCatch(
      fit_one(item),
      error = function(e) {
        warning(sprintf(
          "Error fitting model: %s | Category: %s | Levels: %s\nMessage: %s",
          item$model_name, item$category, paste(item$levels, collapse = "_"), e$message
        ))
        failures[[length(failures) + 1]] <<- list(
          model_name = item$model_name,
          category   = item$category,
          levels     = paste(item$levels, collapse = "_"),
          message    = e$message
        )
      }
    )
  })
  
  if (length(failures)) {
    cat("\n--- The following model/data combinations failed:\n")
    failure_df <- dplyr::bind_rows(lapply(failures, as.data.frame))
    failure_df$timestamp <- Sys.time()
    log_path <- file.path(Paths$modeling_dir, "failed_models_log.txt")
    readr::write_tsv(failure_df, log_path, append = file.exists(log_path), col_names = !file.exists(log_path))
    print(failure_df)
  } else {
    cat("\n All models ran successfully.\n")
  }
  invisible(failures)
}