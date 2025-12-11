library(tidyverse)
library(dplyr)
library(posterior)
library(yaml)
library(purrr)
library(glue)
library(tidybayes)
library(scales)
library(ggrepel)

Paths <- modules::use("./bayesian_code/utils/file_paths.R")
read_instances_specifications_lib <- modules::use("./Modeling_Pipeline/scripts/preprocess/read_instance_specification.R")
#################################################################

###############################################################################
# Read and format Crow

read_crow_mcleod <- function(){
  file_path <- file.path(Paths$Pipeline_preprocesseddata_dir,"crow-mcleod.csv")
  crow_mcleod <- read.csv(file_path)
  
  
  crow_mcleod_long <- crow_mcleod %>%
    transmute(
      expected_phoneme = phoneme,
      group,
      `0.50` = fifty_percent,
      `0.75` = seventyfive_percent,
      `0.90` = ninety_percent
    ) %>%
    pivot_longer(
      cols = c(`0.50`, `0.75`, `0.90`),
      names_to = "prob_x_eq_1_hat",
      values_to = "age_months"
    ) %>%
    mutate(prob_x_eq_1_hat = as.numeric(prob_x_eq_1_hat),
           age_months = as.numeric(round(age_months,0))
    )
  
  crow_mcleod_long_sd <- crow_mcleod %>%
    transmute(
      expected_phoneme = phoneme,
      group,
      `0.50` = fifty_sd,
      `0.75` = seventyfive_sd,
      `0.90` = ninety_sd
    ) %>%
    pivot_longer(c(`0.50`, `0.75`, `0.90`),
                 names_to = "prob_x_eq_1_hat",
                 values_to = "age_months_sd") %>%
    mutate(prob_x_eq_1_hat = as.numeric(prob_x_eq_1_hat),
           age_months_sd = as.numeric(round(age_months_sd,0))
    )
  
  crow_joined <- crow_mcleod_long%>%
    dplyr::left_join(crow_mcleod_long_sd,by=c("expected_phoneme","group","prob_x_eq_1_hat"))%>%
    dplyr::mutate(age_months_m1sd = age_months-age_months_sd,
                  age_months_p1sd = age_months+age_months_sd,
                  age_months_m2sd = age_months -2*age_months_sd,
                  age_months_p2sd = age_months +2*age_months_sd,
                  age_months_m3sd = age_months -3*age_months_sd,
                  age_months_p3sd = age_months +3*age_months_sd
    )
  
  
  # return(list(crow_mcleod_long=crow_mcleod_long,crow_mcleod_long_sd=crow_mcleod_long_sd))
  return(crow_joined)
  
}

####################################################################

extract_q_aaps_binomial <- function(model_type=c("binomial"),
                                    phoneme_numscore_mode, #only used for binomial                                        
                                    agerange,
                                    instance,
                                    fitted_model,
                                    success_frac = 1.0,            # f in [0,1]
                                    target_rule = c("ceil","round","floor")  # how to turn f*n into k
                                    
){
  
  model_type  <- match.arg(model_type)       # here it's always "binomial"
  target_rule <- match.arg(target_rule)
  
  
  
  newdata <- tidyr::crossing(
    age_months       = seq(agerange[1], agerange[2], by = 1), # RANGE!#seq(0, 90, by = 1), # RANGE!
    expected_phoneme = instance$target_phonemes,#phonemes,
    speaker          = "fake" # Tristan's recommendation.
    
  )
  
  # Use the mode across participants for that particular phoneme.
  # binomial-only: attach mode num_score
  
  if(model_type == "binomial"){
    
    newdata <- newdata %>%
      dplyr::left_join(phoneme_numscore_mode, by = "expected_phoneme") %>%
      dplyr::mutate(num_score = as.integer(mode_num_score)) %>%
      dplyr::select(-mode_num_score)  
  }
  
  # clamp success_frac to [0,1] just in case
  f <- min(max(success_frac, 0), 1)
  raw_k <- f * newdata$num_score
  k_vec <- switch(target_rule,
                  ceil  = ceiling(raw_k),
                  floor = floor(raw_k),
                  round = round(raw_k)
  )
  
  # make sure 0 <= k <= n
  k_vec <- pmax(0L, pmin(as.integer(k_vec), newdata$num_score))
  newdata$target_successes <- k_vec
  
  message("this is k_vec:")
  print(k_vec)
  
  
  preds <- tidybayes::add_predicted_draws(
    object          = fitted_model,
    newdata          = newdata,
    re_formula       = NULL,
    allow_new_levels = TRUE
  ) %>% tibble::as_tibble()
  
  
  
  message("this is preds from binomial: ")
  print(preds)
  
  message("this is the maximum value predicted")
  print(max(preds$.prediction))
  
  # P(X = 1) under the posterior predictiv
  
  q_exact1_ppc<- preds%>%
    dplyr::group_by(expected_phoneme, age_months) %>%
    dplyr::summarise(prob_x_eq_1_hat = mean(.prediction == 1L), .groups = "drop")
  
  message("this is q_exact1_ppc")
  print(q_exact1_ppc)
  
  # P(X ≥ 1) under the posterior predictive
  q_atleast1_ppc <- preds %>%
    dplyr::group_by(expected_phoneme, age_months) %>%
    dplyr::summarise(prob_x_eq_1_hat = mean(.prediction >= 1L), .groups = "drop")
  
  message("this is q_atleast1_ppc")
  print(q_atleast1_ppc)
  
  
  # stricter threshold using k = target_successes
  q_exact_k_ppc <- preds %>%
    dplyr::group_by(expected_phoneme, age_months) %>%
    dplyr::summarise(
      n   = dplyr::first(num_score),
      k   = dplyr::first(target_successes),
      prob_x_eq_1_hat = mean(.prediction == dplyr::first(target_successes)),
      .groups = "drop"
    )
  
  message("this is q_exact_k_ppc")
  print(q_exact_k_ppc)
  
  q_atleast_k_ppc <- preds %>%
    dplyr::group_by(expected_phoneme, age_months) %>%
    dplyr::summarise(
      n   = dplyr::first(num_score),
      k   = dplyr::first(target_successes),
      prob_x_eq_1_hat = mean(.prediction >= dplyr::first(target_successes)),
      .groups = "drop"
    )
  
  message("this is q_atleast_k_ppc")
  print(q_atleast_k_ppc)
  
  
  posterior_mean_numb_success<-preds %>%
    dplyr::group_by(expected_phoneme, age_months) %>%
    dplyr::summarise(
      post_mean_numb_success = mean(.prediction),
      .groups = "drop"
    )
  
  plot_post_mean <- ggplot(posterior_mean_numb_success,
                           aes(x = age_months/12, y = post_mean_numb_success, color = expected_phoneme)) +
    geom_line(linewidth = 1) +
    labs(x = "Age (years)", y = "Posterior mean # successes") +
    theme_minimal()
  
  message("this is posterior_mean_numb_success")
  print(posterior_mean_numb_success)
  
  
  result<-list(
    q_exact1_ppc    = q_exact1_ppc,
    q_atleast1_ppc  = q_atleast1_ppc,
    q_exact_k_ppc   = q_exact_k_ppc,
    q_atleast_k_ppc = q_atleast_k_ppc,
    plot_post_mean = plot_post_mean
  )
  plot_post_mean
  return(result)
  
}
################################################################
## Especificaciones de A (instancias AAPS)
# raw_data_path <- "./Modeling_Pipeline/data/raw/AAPS Score Data (Long Version).csv"
# phoneme_grouping_data_path <- "./Modeling_Pipeline/phoneme_grouping/phoneme_grouping2.csv"
# subset_data_grouping_path <- "./Modeling_Pipeline/instance_specification/subset_data_grouping2.csv"
# #instance_to_fit_path <- "./Modeling_Pipeline/instance_specification/instance_to_fit3.csv" #--->pilas usar con grouping1
# ## MODIFICAR ACORDE:
# instance_to_fit_path <- "./Modeling_Pipeline/instance_specification/instance_to_fit_cp1V2.csv"
instance_to_fitA_path_mod <- "./Modeling_Pipeline/instance_specification/cutting_points_instances/instance_to_fit1_A.csv"
# raw_data_type,model_type,model,prior,phoneme_grouping_type,set_data_file,subset_data
# aaps,binomial,model_binomialv2,prior_binomial,grouping2,subset_data_grouping2,dataPhoneme27
# aaps,binomial,model_binomialv2,prior_binomial,grouping2,subset_data_grouping2,dataPhoneme28

# raw_data_type <- "aaps" # coherente con raw_data_path
# phoneme_grouping_type <- "grouping2" # coherente con phoneme_grouping_data_path y con subset_data_grouping_path
# model_type  <- "binomial" # debemos hablar mejor de response variable, porque esto detemrina el tipo de preprocesamiento 
# y el tipo de modelos que podemos usar. model_type debe ser el mismo que aparece en la columna model_type para todas las filas
# de instance_to_fit.csv!!

#list_of_instancesA <- read_instances_specifications_lib$read_instances_specifications(instance_to_fit_path, subset_data_grouping_path,phoneme_grouping_data_path)
list_of_instancesA_modread <- read_instances_specifications_lib$read_instances_specifications_modified(instance_to_fitA_path_mod)


#preprocessed_files_per_instance is a named list:
#list(triplets_unique = triplets_unique, preprocessed_files_list = preprocessed_files_list)
# triplets unique is a list with the unique tripletes (raw_data_type, model_type, phoneme_grouping_type)
#in the instance_to_fit file.

# processed_file_list is a list, each entry related with the triplete in the same list position in triplets_unique.
# each element in processed_file_list is also a named list with entries: df_final and phoneme_numscore_mode.
# df_final is the set of points used for fitting the specified model in the instance and phoneme_nuscore is a dataframe 
#containing the mode of each phoneme and is used when the model is bin

preprocessed_filesA_per_instance <- read_instances_specifications_lib$read_preprocessed_files_instances(list_of_instancesA_modread)
triplets_uniqueA <- preprocessed_filesA_per_instance$triplets_unique
preprocessed_files_listA <- preprocessed_filesA_per_instance$preprocessed_files_list

#-------
# this has to be acces now using the triplets of each instance.
# Read preprocessed of raw_data used for instances A
# preprocessed_result_listA <-read_preprocessed_files(raw_data_type,model_type,phoneme_grouping_type)
# df_finalA <- preprocessed_result_listA$df_final
# phoneme_numscore_modeA <- preprocessed_result_listA$phoneme_numscore_mode
#-------



## Especificaciones de B (instancias pllr)
# raw_data_path <- "./Modeling_Pipeline/data/raw/probabilities-max-frame_W.csv.gz"
# phoneme_grouping_data_path <- "./Modeling_Pipeline/phoneme_grouping/phoneme_grouping2.csv"
# subset_data_grouping_path <- "./Modeling_Pipeline/instance_specification/subset_data_grouping2.csv"
# #instance_to_fit_path <- "./Modeling_Pipeline/instance_specification/instance_to_fit2.csv" #---> pilas usar con grouping1
# #dependiendo de si beta o binomial para pllr
# instance_to_fit_path <- "./Modeling_Pipeline/instance_specification/instance_to_fit_cp2.csv"
# #instance_to_fit_path <- "./Modeling_Pipeline/instance_specification/instance_to_fit_cp2V2.csv"
instance_to_fitB_path_mod <-  "./Modeling_Pipeline/instance_specification/cutting_points_instances/instance_to_fit1_B.csv"

# raw_data_type <- "pllr" # coherente con raw_data_path
# phoneme_grouping_type <- "grouping2" # coherente con phoneme_grouping_data_path y con subset_data_grouping_path
# model_type  <- "beta" 
#model_type  <- "binomial"
# debemos hablar mejor de response variable, porque esto detemrina el tipo de preprocesamiento 
# y el tipo de modelos que podemos usar. model_type debe ser el mismo que aparece en la columna model_type para todas las filas
# de instance_to_fit.csv!!
#list_of_instancesB <- read_instances_specifications_lib$read_instances_specifications(instance_to_fit_path, subset_data_grouping_path,phoneme_grouping_data_path)

list_of_instancesB_modread <- read_instances_specifications_lib$read_instances_specifications_modified(instance_to_fitB_path_mod)
preprocessed_filesB_per_instance <- read_instances_specifications_lib$read_preprocessed_files_instances(list_of_instancesB_modread)

triplets_uniqueB <- preprocessed_filesB_per_instance$triplets_unique
preprocessed_files_listB <- preprocessed_filesB_per_instance$preprocessed_files_list

# Read preprocessed of raw_data used for instances B
# preprocessed_result_listB <-read_preprocessed_files(raw_data_type,model_type,phoneme_grouping_type)
# df_finalB <- preprocessed_result_listB$df_final
# phoneme_numscore_modeB <- preprocessed_result_listB$phoneme_numscore_mode



# Probemos con
# no 8, no 4
k<- 10
instancia_pruebaA <- list_of_instancesA_modread[[k]]
instancia_pruebaB <- list_of_instancesB_modread[[k]]

# extract the 3-tuple
# find the position of 3-tuple in the list of unique 3-tuples
# extract the information in processed_files

#tuple <- list(instancia_pruebaA$raw_data_type, instancia_pruebaA$model_type, instancia_pruebaA$phoneme_grouping_type)
j <- purrr::detect_index(
  triplets_uniqueA,
  ~ .x$raw_data_type        == instancia_pruebaA$raw_data_type &&
    .x$model_type           == instancia_pruebaA$model_type &&
    .x$phoneme_grouping_type == instancia_pruebaA$phoneme_grouping_type
)

k <- purrr::detect_index(
  triplets_uniqueB,
  ~ .x$raw_data_type        == instancia_pruebaB$raw_data_type &&
    .x$model_type           == instancia_pruebaB$model_type &&
    .x$phoneme_grouping_type == instancia_pruebaB$phoneme_grouping_type
)



df_finalA <- preprocessed_files_listA[[j]]$df_final
df_finalB <- preprocessed_files_listB[[k]]$df_final

phoneme_numscore_modeA<- preprocessed_files_listA[[j]]$phoneme_numscore_mode
phoneme_numscore_modeB<- preprocessed_files_listB[[j]]$phoneme_numscore_mode

agerangeA <- range(df_finalA$age_months)
agerangeB <- range(df_finalB$age_months)
agerange <- c(agerangeA[1],agerangeB[2])
message("agerangeA (aaps): ")
print(agerangeA)
#model_typeA<- "binomial"
fitted_modelA <- readRDS(paste0(instancia_pruebaA$fitted_model_file_path,".rds"))
fitted_modelB <- readRDS(paste0(instancia_pruebaB$fitted_model_file_path,".rds"))




success_frac <- 1.0            # f in [0,1]
target_rule <- "floor"  # how to turn f*n into k


extract_q_result <- extract_q_aaps_binomial(instancia_pruebaA$model_type,
                                            phoneme_numscore_modeA, #only used for binomial                                        
                                            #agerangeA,
                                            #agerangeB,
                                            agerange,
                                            instancia_pruebaA,
                                            fitted_modelA,
                                            success_frac,
                                            target_rule
)
q_atleast1_ppc<- extract_q_result$q_atleast1_ppc
q_exact1_ppc<- extract_q_result$q_exact1_ppc
q_atleast_k_ppc<- extract_q_result$q_atleast_k_ppc
q_exact_k_ppc<-extract_q_result$q_exact_k_ppc
plot_post_mean<- extract_q_result$plot_post_mean




crow_joined <- read_crow_mcleod()

list_extracted_x <- extract_x_q_pllr_beta(instancia_pruebaB$model_type,
                                          phoneme_numscore_modeB, #only used for binomial                                        
                                          #agerangeB,
                                          #agerangeA,
                                          agerange,
                                          instancia_pruebaB,
                                          fitted_modelB,
                                          q_exact_k_ppc,
                                          #q_atleast1_ppc
                                          #q_exact1_ppc
                                          crow_joined # sept22
)

xq_predictive<- list_extracted_x$xq_predictive
xq_mu <- list_extracted_x$xq_mu

age_plot <- list_extracted_x$age_plot
age_plot_crow <-list_extracted_x$age_plot_crow
age_plot_crow2<-list_extracted_x$age_plot_crow2
age_plot_crow3<-list_extracted_x$age_plot_crow3
age_plot_crow4<-list_extracted_x$age_plot_crow4
age_plot_crow5<-list_extracted_x$age_plot_crow5
xq_predictive_crow<- list_extracted_x$xq_predictive_crow

message("xq_predictive:")
print(xq_predictive)
#message("xq_mu:")
#print(xq_mu)
age_plot
plot_post_mean
age_plot_crow
age_plot_crow2
age_plot_crow3
age_plot_crow4
age_plot_crow5