# Run pipeline
library(tidyverse)
library(dplyr)
library(posterior)
library(yaml)
library(purrr)
library(glue)
Paths <- modules::use("./bayesian_code/utils/file_paths.R")
read_instances_specifications_lib <- modules::use("./Modeling_Pipeline/scripts/preprocess/read_instance_specification.R")
preprocessing_lib                 <- modules::use("./Modeling_Pipeline/scripts/preprocess/Preprocessing.R")
filtering_lib                     <- modules::use("./Modeling_Pipeline/scripts/preprocess/filtering_data_instances.R")
fit_models_lib                    <- modules::use("./Modeling_Pipeline/scripts/train/fit_models.R")
visualize_models_lib0             <- modules::use("./Modeling_Pipeline/scripts/visualize/visualize_age_standards.R")
visualize_models_lib1             <- modules::use("Modeling_Pipeline/scripts/visualize/run_visuals.R")
conventions <- modules::use("./Modeling_Pipeline/pipeline/config/conventions.R")




# Recordemos los inputs que necesita read_instance specification
# Debemos leer el archivo que contiene los agrupamientos de los fonemas en 
# data levels que queremos modelar. Recordar: hay clasificaciones de fonemas 
# en categorias y niveles, pero queremos tener la flexibilidad de modelar grupos
# de datos que por ejemplo, pertenezcan a la misma categoria pero incluyan diferentes
# niveles (esto es subset_data_grouping).

# Debemos leer el archivo que contiene las instancias que queremos ajustar, esto
# es 4 tuplas que consisten del nombre del modelo, nombre del prior, nombre del 
# nivel de datos a usar.

# -------------------------------------------------------------------------------------------------------
## SANITY CHECK: PILAS, debe haber coherencia entre los archivos phoneme_grouping1 y subsetdatagrouping1. 
# future work: create a function that check that columns in subset data in instance
# file belongs to the subsetdata column in the grouping file
#--------------------------------------------------------------------------------------------------------

#subdata_grouping <- read.csv("./Modeling_Pipeline/instance_specification/subset_data_grouping1.csv")
#instance_to_fit_df <- read.csv("./Modeling_Pipeline/instance_specification/instance_to_fit.csv")

raw_data_path <- "./Modeling_Pipeline/data/raw/probabilities-max-frame_W.csv.gz"
phoneme_grouping_data_path <- "./Modeling_Pipeline/phoneme_grouping/phoneme_grouping1.csv"
subset_data_grouping_path <- "./Modeling_Pipeline/instance_specification/subset_data_grouping1.csv"
instance_to_fit_path <- "./Modeling_Pipeline/instance_specification/instance_to_fit.csv"

raw_data_type <- "pllr" # coherente con raw_data_path
phoneme_grouping_type <- "grouping1" # coherente con phoneme_grouping_data_path y con subset_data_grouping_path
model_type  <- "binomial" # debemos hablar mejor de response variable, porque esto detemrina el tipo de preprocesamiento 
# y el tipo de modelos que podemos usar. model_type debe ser el mismo que aparece en la columna model_type para todas las filas
# de instance_to_fit.csv!!

# dec12: dictionaries with correspondence between keys in instances specification to files.
rawdata_paths<-conventions$rawdata_paths
grouping_paths<-conventions$grouping_paths
setdatafiles_paths<-conventions$setdatafiles_paths

list_of_instances <- read_instances_specifications_lib$read_instances_specifications(instance_to_fit_path, subset_data_grouping_path,phoneme_grouping_data_path)

# Preprocessing, commented on dec12:
# preprocessed_result_list <- preprocessing_lib$create_preprocessed_df(raw_data_type,model_type,phoneme_grouping_type, raw_data_path,phoneme_grouping_data_path)
# df_final <- preprocessed_result_list$df_final
# phoneme_numscore_mode <- preprocessed_result_list$phoneme_numscore_mode

# dec12: Preprocessing without assuming all instances have the same: raw_data_type,model_type,phoneme_grouping_type

# fin the list of unique keys in the instances we're considering

unique_keys <- find_unique_instances_keys(list_of_instances)
unique_keys1 <- unique_keys$unique_keys1# this is just raw_data_type, model_type, phoneme_grouping_type
unique_phoneme_grouping_type<- unique_keys$unique_phoneme_grouping_type
# for each 3tuple in unique_keys1 create a 5 tuple of arguments 
#(raw_data_type, model_type, phoneme_grouping_type)-->(raw_data_type, model_type, phoneme_grouping_type, raw_data_path,phoneme_grouping_data_path)
#(raw_data_type, model_type, phoneme_grouping_type, rawdata_paths(raw_data_type),grouping_paths(phoneme_grouping_type))

#  I think I can avoid this step by just including this information as instance attribute.
expand_preprocessing_key <- function(key1,
                                     rawdata_paths,
                                     grouping_paths) {
  
  raw_data_path <- rawdata_paths[[ key1["raw_data_type"] ]]
  if (is.null(raw_data_path))
    stop("Unknown raw_data_type: ", key1["raw_data_type"], call. = FALSE)
  
  phoneme_grouping_data_path <- grouping_paths[[ key1["phoneme_grouping_type"] ]]
  if (is.null(phoneme_grouping_data_path))
    stop("Unknown phoneme_grouping_type: ", key1["phoneme_grouping_type"], call. = FALSE)
  
  c(
    as.list(key1),
    list(
      raw_data_path = raw_data_path,
      phoneme_grouping_data_path = phoneme_grouping_data_path
    )
  )
}

preprocessing_keys <- purrr::map(
  unique_keys$unique_keys1,
  expand_preprocessing_key,
  rawdata_paths = rawdata_paths,
  grouping_paths = grouping_paths
)

# now pass each unique 5-tuple and get in a new list  the preprocessed files for each case
preprocessed_cache <- purrr::imap(
  preprocessing_keys,
  function(args, i) {
    
    key_str <- paste(
      args$raw_data_type,
      args$model_type,
      args$phoneme_grouping_type,
      sep = "|"
    )
    
    message("Preprocessing: ", key_str)
    
    preprocessing_lib$create_preprocessed_df(
      raw_data_type = args$raw_data_type,
      model_type = args$model_type,
      phoneme_grouping_type = args$phoneme_grouping_type,
      raw_data_path = args$raw_data_path,
      phoneme_grouping_data_path = args$phoneme_grouping_data_path
    )
  }
)

# acces later with:
#read_instances_specifications_lib$get_preprocessed_for_instance(instance, cache)


#read the preprocessed files if they already exist:

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

read_preprocessed_from_key <- function(key1) {
  
  read_preprocessed_files(
    raw_data_type = key1["raw_data_type"],
    model_type = key1["model_type"],
    phoneme_grouping_type = key1["phoneme_grouping_type"]
  )
}



preprocessed_cache <- purrr::map(
  unique_keys1,
  read_preprocessed_from_key
)

names(preprocessed_cache) <- purrr::map_chr(
  unique_keys1,
  read_instances_specifications_lib$make_key_string
)




# dec12, comentar las siguientes lineas, porque estamos interesados en la forma
# mas robusta definida arriba
# preprocessed_result_list <-read_preprocessed_files(raw_data_type,model_type,phoneme_grouping_type)
# df_final <- preprocessed_result_list$df_final
# phoneme_numscore_mode <- preprocessed_result_list$phoneme_numscore_mode
# filtering_data_instances
#phoneme_df <- read.csv(phoneme_grouping_data_path)

missing <- setdiff(unique_phoneme_grouping_type, names(grouping_paths))
if (length(missing) > 0) {
  stop(
    "No grouping_paths defined for: ",
    paste(missing, collapse = ", "),
    call. = FALSE
  )
}

list_df_phonemes <- purrr::imap(
  grouping_paths[unique_phoneme_grouping_type],
  function(path, grouping_type) {
    read.csv(path, stringsAsFactors = FALSE)
  }
)



# la funcion filtering_data recibe una instancia de la lista list_of_intances
# la pregunta ahora es, debo correr un ciclo for o usar map?
# este paso se usaba dentro de un ciclo en el que ajustamos una instancia a la vez
# siguiendo un ciclo for. La ventaja es que no mantenemos los datos filtrados todo el 
# tiempo. Qu'e ser'e mejor completar fases de todos o completar instancia por instancia?

# Creo que a este punto lo mejor es completar por fases breath mejor que depth.
# porque el siguiente paso que es fittear potencialmente consume mucho tiempo, 
# quisieramos establecer de pront alguna regla de prioridad, o potencialmente hacerlo distribuido.

#no queremos mantener en memoria los objetos (potencialmente consume mucha memoria), pero si las direcciones a los objetos

#dec12: comentamos la linea de abajo, para usar mejor opcion mas general
# list_of_df_filters_file_paths <- purrr::map(list_of_instances,
#                                  ~ filtering_lib$filtering_data(.x,df_final,phoneme_df))


list_of_df_filters_file_paths <- purrr::map(
  list_of_instances,
  function(instance) {
    
    # lookup preprocessed data
    prep <- read_instances_specifications_lib$get_preprocessed_for_instance(instance, preprocessed_cache)
    df_final <- prep$df_final
    
    # lookup phoneme grouping df
    phoneme_df <- list_df_phonemes[[ instance$phoneme_grouping_type ]]
    
    if (is.null(phoneme_df)) {
      stop(
        "No phoneme_df found for grouping type: ",
        instance$phoneme_grouping_type,
        call. = FALSE
      )
    }
    
    # call existing filtering logic
    filtering_lib$filtering_data(
      instance,
      df_final,
      phoneme_df
    )
  }
)


#dec12: the only argument needed is list_of_instances.
#fit_models_lib$iterate_run_bayesian_modeling(raw_data_type,model_type,phoneme_grouping_type,list_of_instances)
fit_models_lib$iterate_run_bayesian_modeling(list_of_instances)

# folder_path <- Paths$processed_data_dir
# filename <-  paste0("phoneme_numscore_mode_binomialxphoneme_Prob_singleWords", ".RData") 
# phoneme_numscore_mode_file_path <- file.path(folder_path,filename)
# phoneme_numscore_mode <-load_from_rdata(phoneme_numscore_mode_file_path,"phoneme_numscore_mode")
# 
# model_path_file <- Paths$processed_data_dir
# model_path_file <- file.path(model_path_file,"model_binomial_Probability_singleWords")
# model_file_name <- "model_Vowels_Levelphoneme1.rds"
# model_path_file <- file.path(model_path_file,model_file_name)
# model<-readRDS(model_path_file)


# borrar este llamado, vamos a usar la funcion de run_visuals (itera sobre las instancias)
# visualize_models_lib0$visualize_age_standards_funct(
#   phoneme_num_score_mode_file_path,# asumiendo que vamos a correr binomial
#   agerange,
#   instance,
#   fitted_model)

#dec12: comentamos las sgts lineas para usar version mas general
# visualize_models_lib1$iterate_plots(model_type,
#                                     df_final,
#                                     phoneme_numscore_mode,
#                                     list_of_instances)


visualize_models_lib1$iterate_plots_modified(
  #model_type,#df_final,#phoneme_numscore_mode,
  list_of_instances,
  preprocessed_cache
)

###############################################################
# Vamos a correr el pipeline pero ahora suponiendo que vamos a modelar
# la variable mean_prob, la cual puede ser modelado mediante una regresion Beta
raw_data_path <- "./Modeling_Pipeline/data/raw/probabilities-max-frame_W.csv.gz"
phoneme_grouping_data_path <- "./Modeling_Pipeline/phoneme_grouping/phoneme_grouping1.csv"
subset_data_grouping_path <- "./Modeling_Pipeline/instance_specification/subset_data_grouping1.csv"
instance_to_fit_path <- "./Modeling_Pipeline/instance_specification/instance_to_fit2.csv"


raw_data_type <- "pllr" # coherente con raw_data_path
phoneme_grouping_type <- "grouping1" # coherente con phoneme_grouping_data_path y con subset_data_grouping_path
model_type  <- "beta" # debemos hablar mejor de response variable, porque esto detemrina el tipo de preprocesamiento 
# y el tipo de modelos que podemos usar. model_type debe ser el mismo que aparece en la columna model_type para todas las filas
# de instance_to_fit.csv!!


list_of_instances <- read_instances_specifications_lib$read_instances_specifications(instance_to_fit_path, subset_data_grouping_path,phoneme_grouping_data_path)

preprocessed_result_list <- preprocessing_lib$create_preprocessed_df(raw_data_type,model_type,phoneme_grouping_type, raw_data_path,phoneme_grouping_data_path)
df_final <- preprocessed_result_list$df_final
phoneme_numscore_mode <- preprocessed_result_list$phoneme_numscore_mode
phoneme_df <- read.csv(phoneme_grouping_data_path)

list_of_df_filters_file_paths <- purrr::map(list_of_instances,
                                            ~ filtering_lib$filtering_data(.x,df_final,phoneme_df))

fit_models_lib$iterate_run_bayesian_modeling(raw_data_type,model_type,phoneme_grouping_type,list_of_instances)


visualize_models_lib1$iterate_plots(model_type,
                                    df_final,
                                    phoneme_numscore_mode,
                                    list_of_instances)



#############################################################
# Let's analyze the pipeline but now using the aaps data + binomial model


raw_data_path <- "./Modeling_Pipeline/data/raw/AAPS Score Data (Long Version).csv"
phoneme_grouping_data_path <- "./Modeling_Pipeline/phoneme_grouping/phoneme_grouping1.csv"
subset_data_grouping_path <- "./Modeling_Pipeline/instance_specification/subset_data_grouping1.csv"
instance_to_fit_path <- "./Modeling_Pipeline/instance_specification/instance_to_fit3.csv"


raw_data_type <- "aaps" # coherente con raw_data_path
phoneme_grouping_type <- "grouping1" # coherente con phoneme_grouping_data_path y con subset_data_grouping_path
model_type  <- "binomial" # debemos hablar mejor de response variable, porque esto detemrina el tipo de preprocesamiento 
# y el tipo de modelos que podemos usar. model_type debe ser el mismo que aparece en la columna model_type para todas las filas
# de instance_to_fit.csv!!


list_of_instances <- read_instances_specifications_lib$read_instances_specifications(instance_to_fit_path, subset_data_grouping_path,phoneme_grouping_data_path)

preprocessed_result_list <- preprocessing_lib$create_preprocessed_df(raw_data_type,model_type,phoneme_grouping_type, raw_data_path,phoneme_grouping_data_path)
df_final <- preprocessed_result_list$df_final
phoneme_numscore_mode <- preprocessed_result_list$phoneme_numscore_mode
phoneme_df <- read.csv(phoneme_grouping_data_path)

list_of_df_filters_file_paths <- purrr::map(list_of_instances,
                                            ~ filtering_lib$filtering_data(.x,df_final,phoneme_df))

fit_models_lib$iterate_run_bayesian_modeling(raw_data_type,model_type,phoneme_grouping_type,list_of_instances)


visualize_models_lib1$iterate_plots(model_type,
                                    df_final,
                                    phoneme_numscore_mode,
                                    list_of_instances)



#############################################################
# PUNTOS DE CORTE!
###########################################################
# Run pipeline provisional para mirar puntos de corte de algunos
# phonemas de interes, especificados en instance_to_fit_cp1.csv e
# instance_to_fit_cp2.csv.

raw_data_path <- "./Modeling_Pipeline/data/raw/AAPS Score Data (Long Version).csv"
phoneme_grouping_data_path <- "./Modeling_Pipeline/phoneme_grouping/phoneme_grouping2.csv"
subset_data_grouping_path <- "./Modeling_Pipeline/instance_specification/subset_data_grouping2.csv"
instance_to_fit_path <- "./Modeling_Pipeline/instance_specification/instance_to_fit_cp1V2.csv"


raw_data_type <- "aaps" # coherente con raw_data_path
phoneme_grouping_type <- "grouping2" # coherente con phoneme_grouping_data_path y con subset_data_grouping_path
model_type  <- "binomial" # debemos hablar mejor de response variable, porque esto detemrina el tipo de preprocesamiento 
# y el tipo de modelos que podemos usar. model_type debe ser el mismo que aparece en la columna model_type para todas las filas
# de instance_to_fit.csv!!


list_of_instances <- read_instances_specifications_lib$read_instances_specifications(instance_to_fit_path, subset_data_grouping_path,phoneme_grouping_data_path)

# Preprocessing
preprocessed_result_list <- preprocessing_lib$create_preprocessed_df(raw_data_type,model_type,phoneme_grouping_type, raw_data_path,phoneme_grouping_data_path)
df_final <- preprocessed_result_list$df_final
phoneme_numscore_mode <- preprocessed_result_list$phoneme_numscore_mode

# filtering_data_instances
phoneme_df <- read.csv(phoneme_grouping_data_path)

list_of_df_filters_file_paths <- purrr::map(list_of_instances,
                                            ~ filtering_lib$filtering_data(.x,df_final,phoneme_df))
fit_models_lib$iterate_run_bayesian_modeling(raw_data_type,model_type,phoneme_grouping_type,list_of_instances)

visualize_models_lib1$iterate_plots(model_type,
                                    df_final,
                                    phoneme_numscore_mode,
                                    list_of_instances)

# ahora instance_to_fit_cp2.csv:########################
raw_data_path <- "./Modeling_Pipeline/data/raw/probabilities-max-frame_W.csv.gz"
phoneme_grouping_data_path <- "./Modeling_Pipeline/phoneme_grouping/phoneme_grouping2.csv"
subset_data_grouping_path <- "./Modeling_Pipeline/instance_specification/subset_data_grouping2.csv"
instance_to_fit_path <- "./Modeling_Pipeline/instance_specification/instance_to_fit_cp2.csv"


raw_data_type <- "pllr" # coherente con raw_data_path
phoneme_grouping_type <- "grouping2" # coherente con phoneme_grouping_data_path y con subset_data_grouping_path
model_type  <- "beta" # debemos hablar mejor de response variable, porque esto detemrina el tipo de preprocesamiento 
# y el tipo de modelos que podemos usar. model_type debe ser el mismo que aparece en la columna model_type para todas las filas
# de instance_to_fit.csv!!


list_of_instances <- read_instances_specifications_lib$read_instances_specifications(instance_to_fit_path, subset_data_grouping_path,phoneme_grouping_data_path)

# Preprocessing
preprocessed_result_list <- preprocessing_lib$create_preprocessed_df(raw_data_type,model_type,phoneme_grouping_type, raw_data_path,phoneme_grouping_data_path)
df_final <- preprocessed_result_list$df_final
phoneme_numscore_mode <- preprocessed_result_list$phoneme_numscore_mode

# filtering_data_instances
phoneme_df <- read.csv(phoneme_grouping_data_path)

list_of_df_filters_file_paths <- purrr::map(list_of_instances,
                                            ~ filtering_lib$filtering_data(.x,df_final,phoneme_df))
fit_models_lib$iterate_run_bayesian_modeling(raw_data_type,model_type,phoneme_grouping_type,list_of_instances)

visualize_models_lib1$iterate_plots(model_type,
                                    df_final,
                                    phoneme_numscore_mode,
                                    list_of_instances)

#####################################################################
#Ahora vamos a calcular puntos de corte pero usando modelos binomial
#tanto para aaps data como para pllr data. Binomial para aaps data ya
#se corri'o (usamos binomial v2 la cual contiene 3 splines), ahora corremos
# binomial con 4 splines para pllr data:


# ahora instance_to_fit_cp2V2.csv:###################################

raw_data_path <- "./Modeling_Pipeline/data/raw/probabilities-max-frame_W.csv.gz"
phoneme_grouping_data_path <- "./Modeling_Pipeline/phoneme_grouping/phoneme_grouping2.csv"
subset_data_grouping_path <- "./Modeling_Pipeline/instance_specification/subset_data_grouping2.csv"
instance_to_fit_path <- "./Modeling_Pipeline/instance_specification/instance_to_fit_cp2V2.csv"


raw_data_type <- "pllr" # coherente con raw_data_path
phoneme_grouping_type <- "grouping2" # coherente con phoneme_grouping_data_path y con subset_data_grouping_path
model_type  <- "binomial" # debemos hablar mejor de response variable, porque esto detemrina el tipo de preprocesamiento 
# y el tipo de modelos que podemos usar. model_type debe ser el mismo que aparece en la columna model_type para todas las filas
# de instance_to_fit.csv!!


list_of_instances <- read_instances_specifications_lib$read_instances_specifications(instance_to_fit_path, subset_data_grouping_path,phoneme_grouping_data_path)

# Preprocessing
preprocessed_result_list <- preprocessing_lib$create_preprocessed_df(raw_data_type,model_type,phoneme_grouping_type, raw_data_path,phoneme_grouping_data_path)
df_final <- preprocessed_result_list$df_final
phoneme_numscore_mode <- preprocessed_result_list$phoneme_numscore_mode

# filtering_data_instances
phoneme_df <- read.csv(phoneme_grouping_data_path)

list_of_df_filters_file_paths <- purrr::map(list_of_instances,
                                            ~ filtering_lib$filtering_data(.x,df_final,phoneme_df))
fit_models_lib$iterate_run_bayesian_modeling(raw_data_type,model_type,phoneme_grouping_type,list_of_instances)

visualize_models_lib1$iterate_plots(model_type,
                                    df_final,
                                    phoneme_numscore_mode,
                                    list_of_instances)
