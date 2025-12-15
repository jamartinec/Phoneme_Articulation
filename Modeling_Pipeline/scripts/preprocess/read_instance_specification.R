#-------------------------------------
# Read the instance specification
#-------------------------------------
import("dplyr")
import("purrr")
import("tibble")
import("utils")
import("readr")
import("glue")

## SANITY CHECK: PILAS, debe haber coherencia entre los archivos phoneme_grouping1 y subset data grouping1.

# Call the .R file which contain the models and prior definitions.
Paths <- modules::use("./bayesian_code/utils/file_paths.R")
model_definitions_lib <- modules::use("./Modeling_Pipeline/models/models_definition/models_definition.R")
conventions <- modules::use(
  "./Modeling_Pipeline/pipeline/config/conventions.R"
)



#  helper
.get_target_phonemes <- function(phoneme_df, category, levels) {
  # expects columns: category, level, phoneme
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


# constructor + validador
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
    model_opt             = model_opt,       # p. ej. brms::bf(...)
    prior_name            = prior_name,
    prior_specific        = prior_specific,  # p. ej. brms::prior(...)
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

validate_model_instance <- function(x) {
  stopifnot(is.list(x),
            is.character(x$raw_data_type),   length(x$raw_data_type)   == 1,
            is.character(x$model_type),   length(x$model_type)   == 1,
            is.character(x$model_name), length(x$model_name) == 1,
            is.character(x$prior_name), length(x$prior_name) == 1,
            is.character(x$phoneme_grouping_type), length(x$phoneme_grouping_type) == 1,
            is.character(x$subset_data), length(x$subset_data) == 1,
            is.character(x$category),   length(x$category)   == 1,
            is.character(x$levels)     # vector de niveles,
            
            
  )
  if (!is.null(x$fitted_model_dir))   stopifnot(is.character(x$fitted_model_dir), length(x$fitted_model_dir) == 1)
  if (!is.null(x$phoneme_group_str))  stopifnot(is.character(x$phoneme_group_str), length(x$phoneme_group_str) == 1)
  if (!is.null(x$filtered_file_path)) stopifnot(is.character(x$filtered_file_path), length(x$filtered_file_path) == 1)
  if (!is.null(x$target_phonemes))    stopifnot(is.character(x$target_phonemes))
  if (!is.null(x$fitted_model_file_path)) stopifnot(is.character(x$fitted_model_file_path), length(x$fitted_model_file_path) == 1)
  if (!is.null(x$plots_folder_path)) stopifnot(is.character(x$plots_folder_path), length(x$plots_folder_path) == 1)
  # checks de clase:
  if (!inherits(x$model_opt, c("brmsformula","bf"))) warning("model_opt no parece brms::bf")
  if (!inherits(x$prior_specific, "brmsprior"))       warning("prior_specific no parece brms::prior")
  
  x
}
  
 

# método print # UPDATE THIS
print.ModelInstance <- function(x, ...) {
  cat(sprintf("<ModelInstance %s | %s | %s | %s[%s]>\n",
              x$model_name, x$prior_name, x$category, paste(x$levels, collapse=",")))
  invisible(x)
}



as_model_instance <- function(x) {
  # x es una de tus listas [[i]]
  x <- validate_model_instance(x)
  class(x) <- c("ModelInstance", class(x))
  x
}

#instances <- lapply(list_of_instances, as_model_instance)



export("read_instances_specifications")
read_instances_specifications <- function(instance_to_fit_path, subset_data_grouping_path,phoneme_grouping_data_path){

  # Leer el archivo csv el cual contiene el agrupamiento de fonemas en "subdata" por ejemplo,
  # para el primer ejercicio habiamos agrupado  data1. data 1 incluye vowels level 1 y level 2.
  # es decir, este archivo contiene la definicion de agrupamientos entre fonemas de diferentes niveles 
  # que queramos hacer, basta con especificar el mismo label en la columna subdata.
  
  #subsetdata_grouping <- read.csv("./Modeling_Pipeline/instance_specification/subset_data_grouping1.csv")
  #instance_to_fit_df <- read.csv("./Modeling_Pipeline/instance_specification/instance_to_fit.csv")
  # Ya no tengo que crear las instancias como un producto cartesiano de tres listas
  # modelos, prior, data, si no que las tripletas se leen directamente del csv.
  

  
  subsetdata_grouping <- readr::read_csv(subset_data_grouping_path,show_col_types = FALSE)
  instance_to_fit_df <- readr::read_csv(instance_to_fit_path,show_col_types = FALSE)
  phoneme_df <- readr::read_csv(phoneme_grouping_data_path,show_col_types = FALSE)
  #-----------------------------------------------------------------------------
  # Future work: validity check for the path passed (be sure that it belongs to 
  # the correct folder, otherwise stop and return a warning).
  #-----------------------------------------------------------------------------
  # en el siguiente agrupamiento estamos suponiendo que en un "subdata" (digamos data1)
  # solo tenemos fonemas de la misma categoria y  diferentes niveles.
  
  df_subdata_grouping_list <- subsetdata_grouping |>
    dplyr::arrange(subdata, level) |>
    dplyr::group_by(subdata) |>
    dplyr::summarise(
      category = dplyr::first(category),
      levels   = list(unique(level)),
      .groups  = "drop"
    )
  
  # subdata -> list(category=..., levels=c(...))
  list_subdata <- df_subdata_grouping_list |>
    dplyr::transmute(
      subdata,
      value = purrr::map2(category, levels, ~ list(category = .x, levels = .y))
    ) |>
    tibble::deframe()
  
  # Leemos la lista de definiciones de modelos y de priors 
  defs <- model_definitions_lib$return_lists()
  model_list <- defs$model_list
  prior_list <- defs$prior_list
  # esta funcion recibe una fila del data frame que contiene la informacion de la
  # instancia a fittear  (una instancia por fila) y la organiza en un named list. 
  
  extract_one_instance <- function(row,phoneme_df) {
    s <- list_subdata[[row$subset_data]]
    if (is.null(s)) stop("Unknown subdata: ", row$subset_data, call. = FALSE)
    
    mdl <- model_list[[row$model]]
    if (is.null(mdl)) stop("Unknown model: ", row$model, call. = FALSE)
    
    pr  <- prior_list[[row$prior]]
    if (is.null(pr)) stop("Unknown prior: ", row$prior, call. = FALSE)
    
    # compatibility check:
    if (!row$model %in% pr$valid_models) {
      stop("Prior '", row$prior, "' is not valid for model '", row$model, "'.", call. = FALSE)}
    
    
    category <- s$category
    levels   <- s$levels
    
    
    #prefix                <- file.path(Paths$processed_data_dir, row$model)
    
    phoneme_group_str      <- paste(c(category, levels), collapse = "_")
    raw_data_type          <- row$raw_data_type
    model_type             <- row$model_type
    model_name             <- row$model
    prior_name             <- row$prior
    phoneme_grouping_type  <- row$phoneme_grouping_type
    subset_data            <- row$subset_data
    filtered_folder_path   <- file.path(Paths$filtered_data_dir, raw_data_type, model_type, phoneme_grouping_type )
    filtered_file_path     <- file.path(filtered_folder_path, paste0(phoneme_group_str, ".rds"))
    target_phonemes        <- .get_target_phonemes(phoneme_df, category, levels)
    fitted_model_dir       <- file.path(Paths$Pipeline_fitted_models_dir,raw_data_type,phoneme_grouping_type,model_type,model_name)
    fitted_model_file_path <- file.path(fitted_model_dir, paste0("model_", phoneme_group_str)) 
    plots_folder_path <- file.path(Paths$Pipeline_visualsplots_dir,raw_data_type,phoneme_grouping_type,model_type,model_name,phoneme_group_str)
    #new: dec12
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

export("read_instances_specifications_modified")
read_instances_specifications_modified <- function(instance_to_fit_path 
                                                   #,subset_data_grouping_path,phoneme_grouping_data_path
                                                   ){
  
  # Leer el archivo csv el cual contiene el agrupamiento de fonemas en "subdata" por ejemplo,
  # para el primer ejercicio habiamos agrupado  data1. data 1 incluye vowels level 1 y level 2.
  # es decir, este archivo contiene la definicion de agrupamientos entre fonemas de diferentes niveles 
  # que queramos hacer, basta con especificar el mismo label en la columna subdata.
  
  #subsetdata_grouping <- read.csv("./Modeling_Pipeline/instance_specification/subset_data_grouping1.csv")
  #instance_to_fit_df <- read.csv("./Modeling_Pipeline/instance_specification/instance_to_fit.csv")
  # Ya no tengo que crear las instancias como un producto cartesiano de tres listas
  # modelos, prior, data, si no que las tripletas se leen directamente del csv.
  
  # # 1. Define the paths to the grouping files
  # grouping_paths <- list(
  #   grouping2 = file.path(Paths$Pipeline_phoneme_grouping_dir, "phoneme_grouping2.csv"),
  #   grouping1 = file.path(Paths$Pipeline_phoneme_grouping_dir, "phoneme_grouping1.csv")
  # )
  # 
  # setdatafiles_paths <- list(
  #   subset_data_grouping2 = file.path(Paths$Pipeline_instance_specification_dir, "subset_data_grouping2.csv"),
  #   subset_data_grouping1 = file.path(Paths$Pipeline_instance_specification_dir, "subset_data_grouping1.csv")
  # )
  
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
  
  
  #subsetdata_grouping <- readr::read_csv(subset_data_grouping_path,show_col_types = FALSE)
  #subsetdata_grouping < setdatafiles_dfs[["subset_data_grouping2"]]
  
  #phoneme_df <- readr::read_csv(phoneme_grouping_data_path,show_col_types = FALSE)
  #phoneme_df <- grouping_dfs[["grouping2"]]
  
  #-----------------------------------------------------------------------------
  # Future work: validity check for the path passed (be sure that it belongs to 
  # the correct folder, otherwise stop and return a warning).
  #-----------------------------------------------------------------------------
  # en el siguiente agrupamiento estamos suponiendo que en un "subdata" (digamos data1)
  # solo tenemos fonemas de la misma categoria y  diferentes niveles.
  
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
  
  
  
  # Leemos la lista de definiciones de modelos y de priors 
  defs <- model_definitions_lib$return_lists()
  model_list <- defs$model_list
  prior_list <- defs$prior_list
  # esta funcion recibe una fila del data frame que contiene la informacion de la
  # instancia a fittear  (una instancia por fila) y la organiza en un named list. 
  
  extract_one_instance <- function(row) {
    
    phoneme_df <- grouping_dfs[[row$phoneme_grouping_type]]
    
    list_subdata <- list_list_subdata[[row$set_data_file]]
    
    s <- list_subdata[[row$subset_data]]
    if (is.null(s)) stop("Unknown subdata: ", row$subset_data, call. = FALSE)
    
    mdl <- model_list[[row$model]]
    if (is.null(mdl)) stop("Unknown model: ", row$model, call. = FALSE)
    
    pr  <- prior_list[[row$prior]]
    if (is.null(pr)) stop("Unknown prior: ", row$prior, call. = FALSE)
    
    # compatibility check:
    if (!row$model %in% pr$valid_models) {
      stop("Prior '", row$prior, "' is not valid for model '", row$model, "'.", call. = FALSE)}
    
    
    category <- s$category
    levels   <- s$levels
    
    
    #prefix                <- file.path(Paths$processed_data_dir, row$model)
    
    phoneme_group_str      <- paste(c(category, levels), collapse = "_")
    raw_data_type          <- row$raw_data_type
    model_type             <- row$model_type
    model_name             <- row$model
    prior_name             <- row$prior
    phoneme_grouping_type  <- row$phoneme_grouping_type
    subset_data            <- row$subset_data
    filtered_folder_path   <- file.path(Paths$filtered_data_dir, raw_data_type, model_type, phoneme_grouping_type )
    filtered_file_path     <- file.path(filtered_folder_path, paste0(phoneme_group_str, ".rds"))
    target_phonemes        <- .get_target_phonemes(phoneme_df, category, levels)
    fitted_model_dir       <- file.path(Paths$Pipeline_fitted_models_dir,raw_data_type,phoneme_grouping_type,model_type,model_name)
    fitted_model_file_path <- file.path(fitted_model_dir, paste0("model_", phoneme_group_str)) 
    plots_folder_path <- file.path(Paths$Pipeline_visualsplots_dir,raw_data_type,phoneme_grouping_type,model_type,model_name,phoneme_group_str)
    #new: dec12
    key1              <- list(row$raw_data_type,row$model_type,row$phoneme_grouping_type)
    
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
  
  key1_chr <- list_of_instances |>
    purrr::map(\(x) paste(x$key1, collapse = "|")) |>
    unique()
  
  unique_keys1 <- key1_chr |>
    strsplit("\\|") |>
    purrr::map(\(k) {
      setNames(
        k,
        names(list_of_instances[[1]]$key1)
      )
    })
  
  unique_phoneme_grouping_type <- list_of_instances |>
    purrr::map_chr("phoneme_grouping_type") |>
    unique()
  
  list(
    unique_keys1 = unique_keys1,
    unique_phoneme_grouping_type = unique_phoneme_grouping_type
  )
}


export("get_preprocessed_for_instance")
get_preprocessed_for_instance <- function(instance, cache) {
  
  key <- make_key_string(instance$key1)
  cache[[key]]
}

export("make_key_string")
make_key_string <- function(key1) {
  paste(key1, collapse = "|")
}
  

