#-------------------------------------
# Read the instance specification
#-------------------------------------
import("dplyr")
import("purrr")
import("tibble")
import("utils")
import("readr")
import("glue")

## Sanity Check: Ensure consistency between `phoneme_grouping1` and `subset_data_grouping1`.


# Load model and prior definitions
Paths <- modules::use("./Modeling_Pipeline/pipeline/config/file_paths.R")
model_definitions_lib <- modules::use("./Modeling_Pipeline/models/models_definition/models_definition.R")
conventions <- modules::use(
  "./Modeling_Pipeline/pipeline/config/conventions.R"
)



# Helper function to extract target phonemes for a given category and level set
.get_target_phonemes <- function(phoneme_df, category, levels) {
  # Expects columns: Type, Level, expected_phoneme
  if (all(c("Type","Level","expected_phoneme") %in% names(phoneme_df))) {
    phoneme_df |>
      dplyr::filter(.data$Type == !!category,
                    .data$Level %in% !!levels) |>
      dplyr::pull(.data$expected_phoneme) |>
      unique()
  } else {
    NULL
  }
}


#' Create a ModelInstance object
#'
#' Constructs and validates a \code{ModelInstance} object, which encapsulates
#' all metadata, file paths, and modeling specifications required to fit,
#' store, and visualize a phoneme-level statistical model within the modeling
#' pipeline.
#'
#' This constructor performs basic type checking via
#' \code{\link{validate_model_instance}} and assigns the S3 class
#' \code{"ModelInstance"}.
#'
#' @param raw_data_type Character scalar. Identifier for the raw data source
#'   (e.g., \code{"aaps"}, \code{"multiword"}).
#' @param model_type Character scalar. High-level model family or likelihood
#'   (e.g., \code{"beta"}, \code{"binomial"}).
#' @param model_name Character scalar. Human-readable model identifier.
#' @param model_opt Model specification object, typically a
#'   \code{brms::bf()} formula.
#' @param prior_name Character scalar. Label identifying the prior configuration.
#' @param prior_specific Prior object, typically created with
#'   \code{brms::prior()}.
#' @param category Character scalar. Phoneme category (e.g., vowels, consonants).
#' @param levels Character vector. Phoneme complexity levels included in the model.
#' @param phoneme_grouping_type Character scalar. Name of the phoneme grouping
#'   scheme used.
#' @param subset_data Character scalar. Identifier for the data subset used
#'   (e.g., age range, token type).
#' @param fitted_model_dir Character scalar or NULL. Directory where fitted
#'   models are stored.
#' @param phoneme_group_str Character scalar or NULL. String encoding the phoneme
#'   group used in filenames.
#' @param filtered_file_path Character scalar or NULL. Path to the filtered input
#'   dataset used for fitting.
#' @param target_phonemes Character vector or NULL. Explicit list of phonemes
#'   included in the model.
#' @param fitted_model_file_path Character scalar or NULL. Path (without extension)
#'   to the fitted model file.
#' @param plots_folder_path Character scalar or NULL. Directory where plots
#'   associated with this model should be saved.
#' @param key1 Optional auxiliary key for downstream indexing or bookkeeping.
#'
#' @return An object of class \code{"ModelInstance"}.
#'
#' @section ModelInstance structure:
#' A \code{ModelInstance} is a named list containing model metadata (e.g.,
#' grouping, priors, categories), modeling objects (formula and priors), and
#' filesystem paths required by the modeling and visualization pipeline.
#'
#' @export
export("new_model_instance")
new_model_instance <- function(raw_data_type,
                               model_type,
                               model_name, model_opt, prior_name, prior_specific,
                               category, levels,
                               phoneme_grouping_type, subset_data,
                               fitted_model_dir = NULL,
                               phoneme_group_str = NULL,
                               filtered_file_path = NULL,
                               target_phonemes = NULL,
                               fitted_model_file_path = NULL,
                               plots_folder_path = NULL,
                               key1 = NULL
                               ) {
  x <- list(
    raw_data_type         = raw_data_type,
    model_type            = model_type,
    model_name            = model_name,
    model_opt             = model_opt,       # f. eg. brms::bf(...)
    prior_name            = prior_name,
    prior_specific        = prior_specific,  # f. eg. brms::prior(...)
    phoneme_grouping_type = phoneme_grouping_type,
    subset_data           = subset_data,
    category              = category,
    levels                = levels,
    fitted_model_dir      = fitted_model_dir,
    phoneme_group_str     = phoneme_group_str,
    filtered_file_path    = filtered_file_path,
    target_phonemes       = target_phonemes,
    fitted_model_file_path = fitted_model_file_path,
    plots_folder_path     = plots_folder_path,
    key1                  = key1
  )
  validate_model_instance(x)
  class(x) <- "ModelInstance"
  x
}
#' Validate a ModelInstance object
#'
#' Performs structural and type validation for a \code{ModelInstance}-like
#' object. This function is intended for internal use and is called by
#' \code{\link{new_model_instance}}.
#'
#' The validator checks required fields, optional path fields, and basic
#' compatibility of modeling objects (e.g., \code{brms} formulas and priors).
#'
#' @param x A list intended to represent a \code{ModelInstance}.
#'
#' @return The validated object \code{x}, invisibly.
#'
#' @keywords internal
validate_model_instance <- function(x) {
  stopifnot(is.list(x),
            is.character(x$raw_data_type),   length(x$raw_data_type)   == 1,
            is.character(x$model_type),   length(x$model_type)   == 1,
            is.character(x$model_name), length(x$model_name) == 1,
            is.character(x$prior_name), length(x$prior_name) == 1,
            is.character(x$phoneme_grouping_type), length(x$phoneme_grouping_type) == 1,
            is.character(x$subset_data), length(x$subset_data) == 1,
            is.character(x$category),   length(x$category)   == 1,
            is.character(x$levels)     # vector of phoneme levels

            
            
  )
  if (!is.null(x$fitted_model_dir))   stopifnot(is.character(x$fitted_model_dir), length(x$fitted_model_dir) == 1)
  if (!is.null(x$phoneme_group_str))  stopifnot(is.character(x$phoneme_group_str), length(x$phoneme_group_str) == 1)
  if (!is.null(x$filtered_file_path)) stopifnot(is.character(x$filtered_file_path), length(x$filtered_file_path) == 1)
  if (!is.null(x$target_phonemes))    stopifnot(is.character(x$target_phonemes))
  if (!is.null(x$fitted_model_file_path)) stopifnot(is.character(x$fitted_model_file_path), length(x$fitted_model_file_path) == 1)
  if (!is.null(x$plots_folder_path)) stopifnot(is.character(x$plots_folder_path), length(x$plots_folder_path) == 1)
  # Class compatibility checks
  if (!inherits(x$model_opt, c("brmsformula","bf"))) warning("model_opt no parece brms::bf")
  if (!inherits(x$prior_specific, "brmsprior"))       warning("prior_specific no parece brms::prior")
  
  x
}
  

#' Print method for ModelInstance objects
#'
#' Provides a readable summary of a \code{ModelInstance},
#'
#' @param x A \code{ModelInstance} object.
#' @param ... Additional arguments (ignored).
#'
#' @return Invisibly returns \code{x}.
#'
#' @export
#' @method print ModelInstance
print.ModelInstance <- function(x, ...) {
  cat(sprintf("<ModelInstance %s | %s | %s | %s[%s]>\n",
              x$model_name, x$prior_name, x$category, paste(x$levels, collapse=",")))
  invisible(x)
}



as_model_instance <- function(x) {
  # `x` is a single list element (e.g., one entry from a list of instances)
  
  x <- validate_model_instance(x)
  class(x) <- c("ModelInstance", class(x))
  x
}

#instances <- lapply(list_of_instances, as_model_instance)



export("read_instances_specifications")
read_instances_specifications <- function(instance_to_fit_path, subset_data_grouping_path,phoneme_grouping_data_path){

  
  subsetdata_grouping <- readr::read_csv(subset_data_grouping_path,show_col_types = FALSE)
  instance_to_fit_df <- readr::read_csv(instance_to_fit_path,show_col_types = FALSE)
  phoneme_df <- readr::read_csv(phoneme_grouping_data_path,show_col_types = FALSE)
  # Future work: validate that input paths belong to the expected directory structure
  
  
  # Assumption: within each `subdata`, all phonemes belong to the same category
  # but may span multiple levels.
  
  
  df_subdata_grouping_list <- subsetdata_grouping |>
    dplyr::arrange(subdata, level) |>
    dplyr::group_by(subdata) |>
    dplyr::summarise(
      category = dplyr::first(category),
      levels   = list(unique(level)),
      .groups  = "drop"
    )
  
  # Map each `subdata` label to a list containing its category and levels
  list_subdata <- df_subdata_grouping_list |>
    dplyr::transmute(
      subdata,
      value = purrr::map2(category, levels, ~ list(category = .x, levels = .y))
    ) |>
    tibble::deframe()
  
  # Read the lists of models and priors definitions.
  defs <- model_definitions_lib$return_lists()
  model_list <- defs$model_list
  prior_list <- defs$prior_list
  
  # This function processes a single row from the instance specification table
  # (one row per model instance) and converts it into a structured list.
  extract_one_instance <- function(row,phoneme_df) {
    s <- list_subdata[[row$subset_data]]
    if (is.null(s)) stop("Unknown subdata: ", row$subset_data, call. = FALSE)
    
    mdl <- model_list[[row$model]]
    if (is.null(mdl)) stop("Unknown model: ", row$model, call. = FALSE)
    
    pr  <- prior_list[[row$prior]]
    if (is.null(pr)) stop("Unknown prior: ", row$prior, call. = FALSE)
    
    # Check compatibility between model and prior
    if (!row$model %in% pr$valid_models) {
      stop("Prior '", row$prior, "' is not valid for model '", row$model, "'.", call. = FALSE)}
    
    
    category <- s$category
    levels   <- s$levels
    
    
    
    
    phoneme_group_str      <- paste(c(category, levels), collapse = "_")
    raw_data_type          <- row$raw_data_type
    model_type             <- row$model_type
    model_name             <- row$model
    prior_name             <- row$prior
    phoneme_grouping_type  <- row$phoneme_grouping_type
    subset_data            <- row$subset_data
    filtered_folder_path   <- file.path(Paths$Pipeline_filtered_data_dir, raw_data_type, model_type, phoneme_grouping_type )
    filtered_file_path     <- file.path(filtered_folder_path, paste0(phoneme_group_str, ".rds"))
    target_phonemes        <- .get_target_phonemes(phoneme_df, category, levels)
    fitted_model_dir       <- file.path(Paths$Pipeline_fitted_models_dir,raw_data_type,phoneme_grouping_type,model_type,model_name)
    fitted_model_file_path <- file.path(fitted_model_dir, paste0("model_", phoneme_group_str)) 
    plots_folder_path <- file.path(Paths$Pipeline_visualsplots_dir,raw_data_type,phoneme_grouping_type,model_type,model_name,phoneme_group_str)
    # Added December 2025: composite key for caching and lookup
    
    key1 <- c(
      raw_data_type = row$raw_data_type,
      model_type = row$model_type,
      phoneme_grouping_type = row$phoneme_grouping_type
    )
    
    
    new_model_instance(
      raw_data_type          = raw_data_type,
      model_type             = model_type,
      model_name             = model_name,
      model_opt              = mdl,
      prior_name             = prior_name,
      prior_specific         = pr$object,
      category               = category,
      levels                 = levels,
      phoneme_grouping_type  = phoneme_grouping_type,
      subset_data            = subset_data,
      fitted_model_dir       = fitted_model_dir,
      phoneme_group_str      = phoneme_group_str,
      filtered_file_path     = filtered_file_path,
      target_phonemes        = target_phonemes,
      fitted_model_file_path = fitted_model_file_path,
      plots_folder_path      = plots_folder_path,
      key1                   = key1
    )
  }
  
  rows   <- purrr::transpose(instance_to_fit_df)
  list_of_instances <- purrr::map(rows, ~ extract_one_instance(.x, phoneme_df))
  return(list_of_instances)
}


#' Read and construct model instances using pipeline conventions
#'
#' Reads a CSV file that explicitly defines model instances to be fitted and
#' converts each row into a validated \code{ModelInstance} object. Unlike
#' \code{read_instances_specifications()}, this function resolves phoneme
#' groupings and subset definitions indirectly using the pipeline
#' \code{conventions} object, rather than requiring explicit file paths as
#' arguments.
#'
#' Phoneme groupings and subset data files are dynamically loaded based on the
#' identifiers specified in the instance definition table.
#'
#' @param instance_to_fit_path Character scalar. Path to a CSV file where each
#'   row defines a single model instance (model, prior, data source, phoneme
#'   grouping, and subset).
#'
#' @return A list of \code{ModelInstance} objects, one per row in the instance
#'   specification file.
#'
#' @details
#' This function assumes that:
#' \itemize{
#'   \item The global \code{conventions} object defines mappings from
#'   \code{phoneme_grouping_type} and \code{set_data_file} identifiers to
#'   their corresponding CSV file paths.
#'   \item Each \code{subset_data} label corresponds to a unique phoneme
#'   category and one or more complexity levels.
#'   \item Model and prior definitions are available via
#'   \code{model_definitions_lib$return_lists()}.
#'   \item Directory paths follow the pipeline conventions defined in
#'   \code{Paths}.
#' }
#'
#' Validation errors are raised if unknown groupings, subset labels, models,
#' or priors are encountered, or if a prior is incompatible with a model.
#'
#' @seealso \code{\link{read_instances_specifications}},
#'   \code{\link{new_model_instance}}
#'
#' @export
export("read_instances_specifications_modified")
read_instances_specifications_modified <- function(instance_to_fit_path){
  
  grouping_paths <- conventions$grouping_paths
  setdatafiles_paths <- conventions$setdatafiles_paths
  instance_to_fit_df <- readr::read_csv(instance_to_fit_path, show_col_types = FALSE)
  
 
  unique_groupings <- unique(instance_to_fit_df$phoneme_grouping_type)
  unique_setdatafiles <- unique(instance_to_fit_df$set_data_file)
  
  grouping_dfs <- map(unique_groupings, ~ readr::read_csv(grouping_paths[[.x]], show_col_types = FALSE)) |>
    set_names(unique_groupings)
  
  setdatafiles_dfs <- map(unique_setdatafiles, ~ readr::read_csv(setdatafiles_paths[[.x]], show_col_types = FALSE)) |>
    set_names(unique_setdatafiles)
  print("this is setdatafiles_dfs\n" )
  print(setdatafiles_dfs)
  

  
  # Future work: validate that input paths belong to the expected directory structure
  # Assumption: within each `subdata`, all phonemes belong to the same category
  # but may span multiple levels.
  
  transform_funct <- function(df) {
    df |>
      dplyr::arrange(subdata, level) |>
      dplyr::group_by(subdata) |>
      dplyr::summarise(
        category = dplyr::first(category),
        levels   = list(unique(level)),
        .groups  = "drop"
      ) |>
      dplyr::transmute(
        subdata,
        value = purrr::map2(category, levels, ~ list(category = .x, levels = .y))
      ) |>
      tibble::deframe()
  }
  
  list_list_subdata <- purrr::map(setdatafiles_dfs, transform_funct) # this is a named list of lists :)
  
  
  
  # Read the lists of models and priors definitions. 
  defs <- model_definitions_lib$return_lists()
  model_list <- defs$model_list
  prior_list <- defs$prior_list
  # This function processes a single row from the instance specification table
  # (one row per model instance) and converts it into a structured list.
  
  extract_one_instance <- function(row) {
    
    phoneme_df <- grouping_dfs[[row$phoneme_grouping_type]]
    
    list_subdata <- list_list_subdata[[row$set_data_file]]
    
    s <- list_subdata[[row$subset_data]]
    if (is.null(s)) stop("Unknown subdata: ", row$subset_data, call. = FALSE)
    
    mdl <- model_list[[row$model]]
    if (is.null(mdl)) stop("Unknown model: ", row$model, call. = FALSE)
    
    pr  <- prior_list[[row$prior]]
    if (is.null(pr)) stop("Unknown prior: ", row$prior, call. = FALSE)
    
    # Check compatibility between model and prior
    if (!row$model %in% pr$valid_models) {
      stop("Prior '", row$prior, "' is not valid for model '", row$model, "'.", call. = FALSE)}
    
    
    category <- s$category
    levels   <- s$levels
    
    
    
    
    phoneme_group_str      <- paste(c(category, levels), collapse = "_")
    raw_data_type          <- row$raw_data_type
    model_type             <- row$model_type
    model_name             <- row$model
    prior_name             <- row$prior
    phoneme_grouping_type  <- row$phoneme_grouping_type
    subset_data            <- row$subset_data
    filtered_folder_path   <- file.path(Paths$Pipeline_filtered_data_dir, raw_data_type, model_type, phoneme_grouping_type )
    filtered_file_path     <- file.path(filtered_folder_path, paste0(phoneme_group_str, ".rds"))
    target_phonemes        <- .get_target_phonemes(phoneme_df, category, levels)
    fitted_model_dir       <- file.path(Paths$Pipeline_fitted_models_dir,raw_data_type,phoneme_grouping_type,model_type,model_name)
    fitted_model_file_path <- file.path(fitted_model_dir, paste0("model_", phoneme_group_str)) 
    plots_folder_path <- file.path(Paths$Pipeline_visualsplots_dir,raw_data_type,phoneme_grouping_type,model_type,model_name,phoneme_group_str)
    # Added December 2025: composite key for caching and lookup
    key1              <- list("raw_data_type"= row$raw_data_type,"model_type" = row$model_type, "phoneme_grouping_type"= row$phoneme_grouping_type)
    
    new_model_instance(
      raw_data_type          = raw_data_type,
      model_type             = model_type,
      model_name             = model_name,
      model_opt              = mdl,
      prior_name             = prior_name,
      prior_specific         = pr$object,
      category               = category,
      levels                 = levels,
      phoneme_grouping_type  = phoneme_grouping_type,
      subset_data            = subset_data,
      fitted_model_dir       = fitted_model_dir,
      phoneme_group_str      = phoneme_group_str,
      filtered_file_path     = filtered_file_path,
      target_phonemes        = target_phonemes,
      fitted_model_file_path = fitted_model_file_path,
      plots_folder_path      = plots_folder_path,
      key1                   = key1
    )
  }
  
  rows   <- purrr::transpose(instance_to_fit_df)
  list_of_instances <- purrr::map(rows, ~ extract_one_instance(.x))
  return(list_of_instances)
}

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

export("read_preprocessed_files_instances")
read_preprocessed_files_instances <- function(list_of_instances){
  
  
  df_triplets <- tibble::tibble(
    raw_data_type = purrr::map_chr(list_of_instances, "raw_data_type"),
    model_type = purrr::map_chr(list_of_instances, "model_type"),
    phoneme_grouping_type = purrr::map_chr(list_of_instances, "phoneme_grouping_type")
  )
  
  df_triplets_unique <- distinct(df_triplets)
  print(df_triplets_unique)
  triplets_unique <- pmap(df_triplets_unique, list)
  
  print(triplets_unique)
  preprocessed_files_list <- purrr::pmap(df_triplets_unique, read_preprocessed_files)
 
  return(list(triplets_unique = triplets_unique, preprocessed_files_list = preprocessed_files_list))
}

export("find_unique_instances_keys")
find_unique_instances_keys <- function(list_of_instances) {
  
  
  unique_keys1 <- list_of_instances |>
    purrr::map("key1") |>
    unique()
  
  
  unique_phoneme_grouping_type <- list_of_instances |>
    purrr::map_chr("phoneme_grouping_type") |>
    unique()
  
  list(
    unique_keys1 = unique_keys1,
    unique_phoneme_grouping_type = unique_phoneme_grouping_type
  )
}




export("make_key_string")
make_key_string <- function(key1) {
  paste(
    # key1["raw_data_type"],
    # key1["model_type"],
    # key1["phoneme_grouping_type"],
    key1$raw_data_type,
    key1$model_type,
    key1$phoneme_grouping_type,
    sep = "|"
  )
}

export("expand_preprocessing_key") 
#  I think I can avoid this step by just including this information as instance attribute.
expand_preprocessing_key <- function(key1,
                                     rawdata_paths,
                                     grouping_paths) {
  
  #raw_data_path <- rawdata_paths[[ key1["raw_data_type"] ]]
  raw_data_path <- rawdata_paths[[ key1$raw_data_type ]]
  if (is.null(raw_data_path))
    # stop("Unknown raw_data_type: ", key1["raw_data_type"], call. = FALSE)
    stop("Unknown raw_data_type: ", key1$raw_data_type, call. = FALSE)
  
  #phoneme_grouping_data_path <- grouping_paths[[ key1["phoneme_grouping_type"] ]]
  phoneme_grouping_data_path <- grouping_paths[[ key1$phoneme_grouping_type ]]
  if (is.null(phoneme_grouping_data_path))
    #stop("Unknown phoneme_grouping_type: ", key1["phoneme_grouping_type"], call. = FALSE)
    stop("Unknown phoneme_grouping_type: ",  key1$phoneme_grouping_type, call. = FALSE)
  
  c(
    as.list(key1),
    list(
      raw_data_path = raw_data_path,
      phoneme_grouping_data_path = phoneme_grouping_data_path
    )
  )
}

