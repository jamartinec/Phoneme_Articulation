# Run pipeline
library(tidyverse)
library(dplyr)
library(posterior)
library(yaml)
library(purrr)

read_instances_specifications_lib <- modules::use("./Modeling_Pipeline/scripts/preprocess/read_instance_specification.R")
preprocessing_lib                 <- modules::use("./Modeling_Pipeline/scripts/preprocess/Preprocessing.R")
filtering_lib                     <- modules::use("./Modeling_Pipeline/scripts/preprocess/filtering_data_instances.R")
fit_models_lib                    <- modules::use("./Modeling_Pipeline/scripts/train/fit_models.R")
visualize_models_lib              <- modules::use("./Modeling_Pipeline/scripts/visualize/visualize_age_standards_binomial.R")

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


list_of_instances <- read_instances_specifications_lib$read_instances_specifications(instance_to_fit_path, subset_data_grouping_path,phoneme_grouping_data_path)

# Preprocessing
preprocessed_result_list <- preprocessing_lib$create_preprocessed_df(raw_data_type,model_type,phoneme_grouping_type, raw_data_path,phoneme_grouping_data_path)
df_final <- preprocessed_result_list$df_final
phoneme_numscore_mode <- preprocessed_result_list$phoneme_numscore_mode

read_preprocessed_files <- function(raw_data_type,
                                    model_type,
                                    phoneme_grouping_type){
  
  
  folder_path <- Paths$Pipeline_preprocesseddata_dir
  prefix_name <- glue("preprocessed_{raw_data_type}_{model_type}_{phoneme_grouping_type}")
  file_name   <- glue("{prefix_name}.rds")
  df_final_file_path <- file.path(folder_path, file_name)
  df_final_data  <- readRDS(df_final_file_path)
  
  # READ ALSO phoneme_num_score_mode.
  
  
  return(df_final_data)
  
}
# df_final_data <- read_preprocessed_files(raw_data_type,
#                                               model_type,
#                                               phoneme_grouping_type)


# filtering_data_instances
phoneme_df <- read.csv(phoneme_grouping_data_path)

# la funcion filtering_data recibe una instancia de la lista list_of_intances
# la pregunta ahora es, debo correr un ciclo for o usar map?
# este paso se usaba dentro de un ciclo en el que ajustamos una instancia a la vez
# siguiendo un ciclo for. La ventaja es que no mantenemos los datos filtrados todo el 
# tiempo. Qu'e ser'e mejor completar fases de todos o completar instancia por instancia?

# Creo que a este punto lo mejor es completar por fases breath mejor que depth.
# porque el siguiente paso que es fittear potencialmente consume mucho tiempo, 
# quisieramos establecer de pront alguna regla de prioridad, o potencialmente hacerlo distribuido.

#no queremos mantener en memoria los objetos (potencialmente consume mucha memoria), pero si las direcciones a los objetos

list_of_df_filters_file_paths <- purrr::map(list_of_instances,
                                 ~ filtering_lib$filtering_data(.x,df_final,phoneme_df))

fit_models_lib$iterate_run_bayesian_modeling(raw_data_type,model_type,phoneme_grouping_type,list_of_instances)


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



visualize_models_lib$visualize_age_standards_funct(
  phoneme_num_score_mode_file_path,# asumiendo que vamos a correr binomial
  agerange,
  instance,
  fitted_model)











