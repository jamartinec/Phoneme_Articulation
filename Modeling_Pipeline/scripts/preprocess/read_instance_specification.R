#-------------------------------------
# Read the instance specification
#-------------------------------------
library(dplyr)
library(purrr)
library(tibble)

## SANITY CHECK: PILAS, debe haber coherencia entre los archivos phoneme_grouping1 y subset data grouping1.

# Call the .R file which contain the models and prior definitions.
model_definitions_lib <- modules::use("./Modeling_Pipeline/models/models_definition/models_definition.R")

# Leer el archivo csv el cual contiene el agrupamiento de fonemas en "subdata" por ejemplo,
# para el primer ejercicio habiamos agrupado en data1 hasta data1. data 1 incluye vowels level 1 y level 2.
# es decir, este archivo contiene la definicion de agrupamientos entre fonemas de diferentes niveles 
# que queramos hacer, basta con especificar el mismo label en la columna subdata.

subdata_grouping <- read.csv("./Modeling_Pipeline/instance_specification/subset_data_grouping1.csv")
instance_to_fit_df <- read.csv("./Modeling_Pipeline/instance_specification/instance_to_fit.csv")
# Ya no tengo que crear las instancias como un producto cartesiano de tres listas
# modelos, prior, data, si no que las tripletas se leen directamente del csv.

# en el siguiente agrupamiento estamos suponiendo que en un "subdata" (digamos data1)
# solo tenemos fonemas de la misma categoria y  diferentes niveles.
df_subdata_grouping_list <- subdata_grouping %>%
  group_by(subdata) %>%
  summarise(
    category = first(category),
    levels = list(level),
    .groups = "drop"
  )

# convertimos en una lista
list_subdata <- df_subdata_grouping_list %>%
  transmute(
    subdata,
    value = map2(category, levels, ~ list(category = .x, levels = .y))
  ) %>%
  deframe()

# Leemos la lista de definiciones de modelos y de priors 
models_priors_list <- model_definitions_lib$return_lists()
model_list <- models_priors_list$model_list
prior_list <- models_priors_list$prior_list


# esta funcion recibe una fila del data frame que contiene la informacion de la
# instancia a fittear  (una instancia por fila) y la organiza en un named list. 

extract_one_instance <- function(row) {
  s <- list_subdata[[row$subset_data]]
  if (is.null(s)) stop("Unknown subdata: ", row$subset_data, call. = FALSE)
  
  mdl <- model_list[[row$model]]
  if (is.null(mdl)) stop("Unknown model: ", row$model, call. = FALSE)
  
  pr  <- prior_list[[row$prior]]
  if (is.null(pr)) stop("Unknown prior: ", row$prior, call. = FALSE)
  
  # Optional compatibility check:
  if (!row$model %in% pr$valid_models) {
    stop("Prior '", row$prior, "' is not valid for model '", row$model, "'.", call. = FALSE)
  }
  
  list(
    model_name     = row$model,
    model_opt      = mdl,
    prior_name     = row$prior,
    prior_specific = pr$object,
    category       = s$category,
    levels         = s$levels
  )
}

rows   <- purrr::transpose(instance_to_fit_df)
list_of_instances <- purrr::map(rows, extract_one_instance)



