# En este script vamos a computar puntos de corte.

# Imaginemos que tenemos dos csv files con las especificaciones de instancias
# instanciaSA:  instancias aaps -binomial/prob_succes 
# instanciasB:  instancias pllr -beta/man_prob

# Las especificaciones de nivel de agregacion y datos a modelar
# es decir phoneme_grouping_type and subset_data en la fila 
# ide instanciasA deben ser iguales a las que aparecen en la fila i
# de instanciasB.

# Supondremos que ya existen los fitted models para estas instancias.

# La primera funcion lee las especificaciones de instanciasA y calcula para
# cada instancia la tabla q(age). la cual nos da para cada edad age,
# cual es la probabilidad de obtener al menos(?) (exctamente?) un exito. 

# Guardamos estos q en una lista?

library(tidyverse)
library(dplyr)
library(posterior)
library(yaml)
library(purrr)
library(glue)
library(tidybayes)

Paths <- modules::use("./bayesian_code/utils/file_paths.R")
read_instances_specifications_lib <- modules::use("./Modeling_Pipeline/scripts/preprocess/read_instance_specification.R")
#preprocessing_lib                 <- modules::use("./Modeling_Pipeline/scripts/preprocess/Preprocessing.R")
#filtering_lib                     <- modules::use("./Modeling_Pipeline/scripts/preprocess/filtering_data_instances.R")
#fit_models_lib                    <- modules::use("./Modeling_Pipeline/scripts/train/fit_models.R")
#visualize_models_lib0             <- modules::use("./Modeling_Pipeline/scripts/visualize/visualize_age_standards.R")
#visualize_models_lib1             <- modules::use("Modeling_Pipeline/scripts/visualize/run_visuals.R")
# Probemos con
# no 8, no 4
k<- 10

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

## Especificaciones de A (instancias AAPS)
raw_data_path <- "./Modeling_Pipeline/data/raw/AAPS Score Data (Long Version).csv"
phoneme_grouping_data_path <- "./Modeling_Pipeline/phoneme_grouping/phoneme_grouping2.csv"
subset_data_grouping_path <- "./Modeling_Pipeline/instance_specification/subset_data_grouping2.csv"
#instance_to_fit_path <- "./Modeling_Pipeline/instance_specification/instance_to_fit3.csv" #--->pilas usar con grouping1
instance_to_fit_path <- "./Modeling_Pipeline/instance_specification/instance_to_fit_cp1.csv"

raw_data_type <- "aaps" # coherente con raw_data_path
phoneme_grouping_type <- "grouping2" # coherente con phoneme_grouping_data_path y con subset_data_grouping_path
model_type  <- "binomial" # debemos hablar mejor de response variable, porque esto detemrina el tipo de preprocesamiento 
# y el tipo de modelos que podemos usar. model_type debe ser el mismo que aparece en la columna model_type para todas las filas
# de instance_to_fit.csv!!

list_of_instancesA <- read_instances_specifications_lib$read_instances_specifications(instance_to_fit_path, subset_data_grouping_path,phoneme_grouping_data_path)

# Read preprocessed of raw_data used for instances A
preprocessed_result_listA <-read_preprocessed_files(raw_data_type,model_type,phoneme_grouping_type)
df_finalA <- preprocessed_result_listA$df_final
phoneme_numscore_modeA <- preprocessed_result_listA$phoneme_numscore_mode




## Especificaciones de B (instancias pllr)
raw_data_path <- "./Modeling_Pipeline/data/raw/probabilities-max-frame_W.csv.gz"
phoneme_grouping_data_path <- "./Modeling_Pipeline/phoneme_grouping/phoneme_grouping2.csv"
subset_data_grouping_path <- "./Modeling_Pipeline/instance_specification/subset_data_grouping2.csv"
#instance_to_fit_path <- "./Modeling_Pipeline/instance_specification/instance_to_fit2.csv" #---> pilas usar con grouping1
instance_to_fit_path <- "./Modeling_Pipeline/instance_specification/instance_to_fit_cp2.csv"

raw_data_type <- "pllr" # coherente con raw_data_path
phoneme_grouping_type <- "grouping2" # coherente con phoneme_grouping_data_path y con subset_data_grouping_path
model_type  <- "beta" # debemos hablar mejor de response variable, porque esto detemrina el tipo de preprocesamiento 
# y el tipo de modelos que podemos usar. model_type debe ser el mismo que aparece en la columna model_type para todas las filas
# de instance_to_fit.csv!!


list_of_instancesB <- read_instances_specifications_lib$read_instances_specifications(instance_to_fit_path, subset_data_grouping_path,phoneme_grouping_data_path)

# Read preprocessed of raw_data used for instances B

preprocessed_result_listB <-read_preprocessed_files(raw_data_type,model_type,phoneme_grouping_type)
df_finalB <- preprocessed_result_listB$df_final
phoneme_numscore_modeB <- preprocessed_result_listB$phoneme_numscore_mode



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
  
  
  # --- New stricter threshold using k = target_successes ---
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
  
  return(result)
  
}


instancia_pruebaA <- list_of_instancesA[[k]]
agerangeA <- range(df_finalA$age_months)
agerangeB <- range(df_finalB$age_months)

agerange <- c(agerangeA[1],agerangeB[2])
message("agerangeA (aaps): ")
print(agerangeA)
model_typeA<- "binomial"
fitted_modelA <- readRDS(paste0(instancia_pruebaA$fitted_model_file_path,".rds"))
success_frac <- 1.0            # f in [0,1]
target_rule <- "floor"  # how to turn f*n into k


extract_q_result <- extract_q_aaps_binomial(model_typeA,
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

instancia_pruebaB <- list_of_instancesB[[k]]
agerangeB <- range(df_finalB$age_months)
message("agerangeB (pllr): ")
print(agerangeB)
model_typeB<- "beta"
fitted_modelB <- readRDS(paste0(instancia_pruebaB$fitted_model_file_path,".rds"))

extract_x_q_pllr_beta<-function(model_type=c("binomial","beta"),
                      phoneme_numscore_mode, #only used for binomial                                        
                      agerange,
                      instance,
                      fitted_model,
                      q_atleast1_ppc
                      ){
  
  # esperamos usar beta, pero mantenemos lo sgt para reciclar codigo
  model_type <- match.arg(model_type)
  resp_col   <- if (model_type == "binomial") "proportion" else if (model_type == "beta") "mean_prob"
  y_label    <- if (model_type == "binomial") "Probability of success" else if (model_type == "beta")  "Phoneme goodness score"
  
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
  
  predicted <- tidybayes::add_predicted_draws(
    object  = fitted_model,  
    newdata = newdata,
    re_formula = NULL,
    allow_new_levels = TRUE
  ) %>%
    dplyr::mutate(
      draw   = .draw,
      row_id = .row
    ) 
  
  message("predicted for the beta model:")
  print(predicted)
  
  # unify to a single response vector `.resp`, then give it a model-specific name
  
  if(model_type == "binomial"){
    predicted <- predicted|>
      mutate(
        sum_score = .prediction,
        #proportion = sum_score/num_score
        .resp     =  .prediction/ num_score
      ) #|>
    #select(-c(.prediction,.draw))
  }else if(model_type == "beta"){
    predicted <- predicted|>
      mutate(
        mean_prob = .prediction,
        .resp = .prediction
      ) #|>
    #select(-c(.prediction,.draw))
  }
  
  message("predicted for the beta model:")
  print(predicted)
  
  predicted <- predicted %>%
    dplyr::mutate(
      # if `resp_col` exists, keep it; if not, create it from `.resp`
      "{resp_col}" := if (rlang::has_name(cur_data_all(), resp_col))
        .data[[resp_col]]
      else
        .resp
    ) %>%
    dplyr::select(-.resp) 
  
  message("predicted for the beta model:")
  print(predicted)
  
  # Load filtered data for plot overlay
  df_filtered <- readRDS(instance$filtered_file_path)
  df_filtered <- readRDS(instance$filtered_file_path) %>%
    dplyr::filter(dplyr::between(age_months, agerange[1], agerange[2]))
  
  
  df_points <- df_filtered %>%
    mutate(
      y_obs = if (model_type == "binomial")
        (sum_score / num_score) + stats::runif(n(), -0.02, 0.02)
      else if (model_type == "beta")
        mean_prob
    )
  
  # summarize intervals on the unified response
  plot_data <- predicted %>%
    group_by(expected_phoneme, age_months) %>%
    tidybayes::median_qi(.data[[resp_col]], .width = c(0.5, 0.95)) %>%
    pivot_wider(
      names_from  = .width,
      values_from = c(.lower, .upper)
    ) %>%
    rename(
      q025 = `.lower_0.95`,
      q975 = `.upper_0.95`,
      q25  = `.lower_0.5`,
      q75  = `.upper_0.5`,
      q50  = !!resp_col
    )
  
  message("plot_data")
  print(plot_data)
  
  q_beta_join <- q_atleast1_ppc %>%
    dplyr::select(expected_phoneme, age_months, q_age = prob_x_eq_1_hat)
  
  preds_with_q <- predicted %>%
    dplyr::inner_join(q_beta_join, by = c("expected_phoneme","age_months")) %>%
    dplyr::mutate(p_quant = pmax(0, pmin(1, 1 - q_age)))  # convert tail prob to CDF prob
  
  message("preds_with_q:")
  print(preds_with_q)
  
  xq_predictive <- preds_with_q %>%
    dplyr::group_by(expected_phoneme, age_months, q_age) %>%
    dplyr::summarise(
      x_q = stats::quantile(.prediction, probs = unique(p_quant), names = FALSE, na.rm = TRUE),
      .groups = "drop"
    )

  message("this is xq_predictive")
  print(xq_predictive)
  mu_draws <- tidybayes::add_epred_draws(
    newdata = newdata,
    object  = fitted_model,
    re_formula = NULL
  )  
  
  mu_with_q <- mu_draws %>%
    dplyr::inner_join(q_beta_join, by = c("expected_phoneme","age_months")) %>%
    dplyr::mutate(p_quant = pmax(0, pmin(1, 1 - q_age)))
  
  xq_mu <- mu_with_q %>%
    dplyr::group_by(expected_phoneme, age_months, q_age) %>%
    dplyr::summarise(
      x_q_mu = stats::quantile(.epred, probs = unique(p_quant), names = FALSE, na.rm = TRUE),
      .groups = "drop"
    )
  
  message("this is xq_mu")
  print(xq_mu)
  
  # single plotting block
  # #age_plot <- ggplot(plot_data, aes(x = (age_months + 30) / 12, y = q50, color = expected_phoneme)) +
  # age_plot <- ggplot(plot_data, aes(x = (age_months) / 12, y = q50, color = expected_phoneme)) +
  #   geom_line(linewidth = 1.1, color = "steelblue") +
  #   geom_line(aes(y = q50, linetype = "Posterior median"), linewidth = 1.1) +
  #   geom_ribbon(aes(ymin = q025, ymax = q975, fill = expected_phoneme), alpha = 0.2, color = NA) +
  #   geom_ribbon(aes(ymin = q25,  ymax = q75,  fill = expected_phoneme), alpha = 0.4, color = NA) +
  #   facet_wrap(~ expected_phoneme) +
  #   geom_point(
  #     data = df_points,
  #     #aes(x = (age_months + 30) / 12, y = y_obs, color = expected_phoneme),
  #     aes(x = (age_months ) / 12, y = y_obs, color = expected_phoneme),
  #     inherit.aes = FALSE, alpha = 0.5, size = 1.5
  #   ) +
  #   labs(x = "Age (years)", y = y_label) +
  #   theme_minimal(base_size = 14) +
  #   theme(
  #     axis.title   = element_text(size = 16, face = "bold"),
  #     axis.text    = element_text(size = 14),
  #     strip.text   = element_text(size = 14, face = "bold"),
  #     legend.title = element_blank(),
  #     legend.position = "bottom",
  #     panel.grid.minor = element_blank(),
  #     panel.grid.major = element_line(color = "gray90"),
  #     plot.title = element_blank(),
  #     plot.subtitle = element_blank()
  #   )
  # # Add the x_q overlay if present
  # if (!is.null(xq_predictive)) {
  #   age_plot <- age_plot +
  #     geom_line(
  #       data = xq_predictive,
  #       #aes(x = (age_months + 30) / 12, y = x_q, color = expected_phoneme, linetype = "x_q threshold"),
  #       aes(x = age_months/12, y = x_q, color = expected_phoneme, linetype = "x_q threshold"),
  #       linewidth = 0.9,
  #       inherit.aes = FALSE
  #     )# +
  #     #scale_linetype_manual(values = c("Posterior median" = "solid", "x_q threshold" = "longdash"))
  # } 
  # 
  # if (!is.null(xq_mu)) {
  #   age_plot <- age_plot +
  #     geom_line(
  #       data = xq_mu,
  #       #aes(x = (age_months + 30) / 12, y = x_q, color = expected_phoneme, linetype = "x_q threshold"),
  #       aes(x = age_months/12, y = x_q_mu, color = expected_phoneme, linetype = "x_mu threshold"),
  #       linewidth = 0.9,
  #       inherit.aes = FALSE
  #     ) +
  #     scale_linetype_manual(values = c("Posterior median" = "solid", "x_q threshold" = "longdash", "x_mu threshold"="dotted"))
  # } 
  # 
  
  age_plot <- ggplot(plot_data, aes(x = age_months/12, y = q50, color = expected_phoneme)) +
    # one median line only
    geom_line(aes(linetype = "Posterior median"), linewidth = 1.1) +
    # ribbons (inherit x from ggplot mapping)
    geom_ribbon(aes(ymin = q025, ymax = q975, fill = expected_phoneme), alpha = 0.2, color = NA) +
    geom_ribbon(aes(ymin = q25,  ymax = q75,  fill = expected_phoneme), alpha = 0.4, color = NA) +
    facet_wrap(~ expected_phoneme) +
    geom_point(
      data = df_points,
      aes(x = age_months/12, y = y_obs, color = expected_phoneme),
      inherit.aes = FALSE, alpha = 0.5, size = 1.5
    ) +
    labs(x = "Age (years)", y = y_label) +
    theme_minimal(base_size = 14) +
    theme(
      axis.title = element_text(size = 16, face = "bold"),
      axis.text  = element_text(size = 14),
      strip.text = element_text(size = 14, face = "bold"),
      legend.title = element_blank(),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90")
    )
  
  # Overlays with fixed colors
  if (!is.null(xq_predictive)) {
    age_plot <- age_plot +
      geom_line(
        data = xq_predictive,
        aes(x = age_months/12, y = x_q, linetype = "x_q threshold"),
        color = "black", linewidth = 0.9, inherit.aes = FALSE
      )
  }
  
  if (!is.null(xq_mu)) {
    age_plot <- age_plot +
      geom_line(
        data = xq_mu,
        aes(x = age_months/12, y = x_q_mu, linetype = "x_mu threshold"),
        color = "firebrick", linewidth = 0.9, inherit.aes = FALSE
      )
  }
  
  age_plot <- age_plot +
    scale_linetype_manual(values = c(
      "Posterior median" = "solid",
      "x_q threshold"    = "longdash",
      "x_mu threshold"   = "dotdash"
    ))
  print(age_plot)
  
  
  
  age_plot
  
  return(list(xq_predictive=xq_predictive,xq_mu=xq_mu, age_plot= age_plot))
}

list_extracted_x <- extract_x_q_pllr_beta(model_typeB,
                                phoneme_numscore_modeB, #only used for binomial                                        
                                #agerangeB,
                                #agerangeA,
                                agerange,
                                instancia_pruebaB,
                                fitted_modelB,
                                q_exact_k_ppc
                                #q_atleast1_ppc
                                #q_exact1_ppc
)

xq_predictive<- list_extracted_x$xq_predictive
xq_mu <- list_extracted_x$xq_mu
age_plot <- list_extracted_x$age_plot
message("xq_predictive:")
print(xq_predictive)
message("xq_mu:")
print(xq_mu)
age_plot





