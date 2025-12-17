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
import("wisclabmisc")# <- tristan wisclab package

# Load file path, 
Paths <- modules::use("./Modeling_Pipeline/pipeline/config/file_paths.R")
brms_help <- modules::use("./Modeling_Pipeline/scripts/train/tristan_brm_helper.R")
# Import better wisclab package.


# Move to utils 
ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

ensure_parent_dir <- function(file_path) {
  ensure_dir(dirname(file_path))
}


#' Fit and save a Bayesian model for a phoneme group
#'
#' Fits a Bayesian model for a single phoneme group using \code{brms}, augments
#' the fitted object with model validation criteria (LOO and WAIC), and saves
#' the resulting model object to disk.
#'
#' The model is always refitted, even if a fitted object with the same filename
#' already exists. This function is intended to be used as a pipeline
#' orchestration step rather than a pure modeling function.
#'
#' @param model_specific Model formula specification, typically created with
#'   \code{brms::bf()}.
#' @param prior_specific Prior specification, typically created with
#'   \code{brms::prior()}.
#' @param df_filtered A data frame containing the filtered and preprocessed data
#'   used for model fitting.
#' @param fitted_model_dir Character scalar. Directory where the fitted model
#'   object will be saved.
#' @param fitted_model_file_path Character scalar. File path (without extension)
#'   used to save the fitted model object.
#' @param phoneme_group_str Character scalar. Identifier for the phoneme group,
#'   used for logging and file naming.
#'
#' @details
#' This function:
#' \itemize{
#'   \item Creates the output directory if it does not already exist.
#'   \item Fits the model using the \code{cmdstanr} backend.
#'   \item Forces refitting by setting \code{file_refit = "always"}.
#'   \item Adds validation criteria using LOO and WAIC.
#'   \item Saves the fitted model as an \code{.rds} file.
#' }
#'
#' This function has side effects: it performs MCMC sampling and writes
#' model objects to disk. Large objects are removed from memory after saving.
#'
#' @seealso \code{\link{brms::brm}},
#'   \code{\link{loo::loo}},
#'   \code{\link{brms::add_criterion}}
#'
#' @export
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
  brm_args <- wisclabmisc::brms_args_create()
  args <- brm_args(formula = model_specific,
                   data = df_filtered,
                   prior = prior_specific,
                   file = fitted_model_file_path,
                   # The model is always refitted, even if an object with the same name already exists.
                   file_refit = "always", 
                   seed = 20250625,
                   chains = 4,
                   #iter  = 4000,
                   iter  = 100,
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


#' Run Bayesian modeling for multiple model instances with error handling
#'
#' Iterates over a list of \code{ModelInstance} objects and fits each model
#' using the Bayesian modeling pipeline. Errors encountered during fitting
#' are caught and logged without stopping the overall execution, allowing
#' the pipeline to continue processing remaining instances.
#'
#' Failed model configurations are collected and written to a persistent
#' log file with timestamps for later inspection or reruns.
#'
#' @param list_of_instances A list of \code{ModelInstance} objects defining
#'   the models and data configurations to be fitted.
#'
#' @return Invisibly returns a list describing failed model instances. Each
#'   element contains the model name, category, phoneme levels, and error
#'   message. Returns an empty list if all models run successfully.
#'
#' @details
#' For each model instance, this function:
#' \itemize{
#'   \item Calls \code{fit_one()} to perform Bayesian model fitting.
#'   \item Catches and reports errors without interrupting the loop.
#'   \item Accumulates metadata for failed fits.
#'   \item Writes a tab-separated failure log to
#'     \code{Paths$Pipeline_fittingmodelserrors_dir}.
#' }
#'
#' This function is intended as a high-level pipeline orchestration step and
#' has side effects, including model fitting, console output, and writing
#' log files to disk.
#'
#' @seealso \code{\link{fit_bayesian_model_funct}},
#'   \code{\link{fit_one}}
#'
#' @export
export("iterate_run_bayesian_modeling")
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
    log_path <- file.path(Paths$Pipeline_fittingmodelserrors_dir, "failed_models_log.txt")
    readr::write_tsv(failure_df, log_path, append = file.exists(log_path), col_names = !file.exists(log_path))
    print(failure_df)
  } else {
    cat("\n All models ran successfully.\n")
  }
  invisible(failures)
}