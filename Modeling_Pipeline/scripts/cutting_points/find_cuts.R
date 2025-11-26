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
library(scales)
library(ggrepel)

Paths <- modules::use("./bayesian_code/utils/file_paths.R")
read_instances_specifications_lib <- modules::use("./Modeling_Pipeline/scripts/preprocess/read_instance_specification.R")
#preprocessing_lib                 <- modules::use("./Modeling_Pipeline/scripts/preprocess/Preprocessing.R")
#filtering_lib                     <- modules::use("./Modeling_Pipeline/scripts/preprocess/filtering_data_instances.R")
#fit_models_lib                    <- modules::use("./Modeling_Pipeline/scripts/train/fit_models.R")
#visualize_models_lib0             <- modules::use("./Modeling_Pipeline/scripts/visualize/visualize_age_standards.R")
#visualize_models_lib1             <- modules::use("Modeling_Pipeline/scripts/visualize/run_visuals.R")


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

###############################################################

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
  plot_post_mean
  return(result)
  
}
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

##############################################################################
extract_x_q_pllr_beta <- function(model_type=c("binomial","beta"),
                                phoneme_numscore_mode, #only used for binomial                                        
                                agerange,
                                instance,
                                fitted_model,
                                q_atleast1_ppc,
                                crow_joined # modification 09/22
){
  
  # esperamos usar beta, pero mantenemos lo sgt para reciclar codigo
  model_type <- match.arg(model_type)
  resp_col   <- if (model_type == "binomial") "proportion" else if (model_type == "beta") "mean_prob"
  y_label    <- if (model_type == "binomial") "Probability of success" else if (model_type == "beta")  "Phoneme goodness score"
  
  age_lower_bound <- max(min(crow_joined$age_months_m3sd, agerange[1]), 4)
  message("this is age_lower bound")
  print(age_lower_bound)
  
  newdata <- tidyr::crossing(
    #age_months       = seq(agerange[1], agerange[2], by = 1), # RANGE!#seq(0, 90, by = 1), # RANGE!
    age_months       = seq(age_lower_bound, agerange[2], by = 1),
    expected_phoneme = instance$target_phonemes,#phonemes,
    speaker          = "fake" # Tristan's recommendation.
    
  )
  
  message("newdata")
  print(newdata,width=Inf)
  
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
  
  message("predicted for the  model (pllr data):")
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
  
  message("predicted for the  model (pllr data):")
  print(predicted,width = Inf)
  
  predicted <- predicted %>%
    dplyr::mutate(
      # if `resp_col` exists, keep it; if not, create it from `.resp`
      "{resp_col}" := if (rlang::has_name(cur_data_all(), resp_col))
        .data[[resp_col]]
      else
        .resp
    ) %>%
    dplyr::select(-.resp) 
  
  message("predicted for the  model (pllr data):")
  print(predicted, width = Inf)
  
  # Load filtered data for plot overlay
  #df_filtered <- readRDS(instance$filtered_file_path)
  df_filtered <- readRDS(instance$filtered_file_path) %>%
    dplyr::filter(dplyr::between(age_months, agerange[1], agerange[2]))
  
  
  df_points <- df_filtered %>%
    mutate(
      y_obs = if (model_type == "binomial")
        (sum_score / num_score) + stats::runif(n(), -0.02, 0.02)
      else if (model_type == "beta")
        mean_prob
    )
  message("df_points")
  print(df_points, width = Inf)
  
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
  print(plot_data,width = Inf)
  
  q_beta_join <- q_atleast1_ppc %>%
    dplyr::select(expected_phoneme, age_months, q_age = prob_x_eq_1_hat)
  
  message("this is q_beta_join")
  print(q_beta_join)
  
  ##############################################################################
  q_crow <- crow_joined %>%
    dplyr::select(expected_phoneme,age_months, q_age_crow = prob_x_eq_1_hat)
  message("this is q_crow")
  print(q_crow)
  
  q_crowm1sd<- crow_joined %>%
    dplyr::select(expected_phoneme, age_months = age_months_m1sd, q_age_crow = prob_x_eq_1_hat)
  
  message("this is q_crowm1sd")
  print(q_crowm1sd)
  
  q_crowp1sd<- crow_joined %>%
    dplyr::select(expected_phoneme, age_months = age_months_p1sd, q_age_crow = prob_x_eq_1_hat)
  message("this is q_crowp1sd")
  print(q_crowp1sd)
  
  q_crowm3sd<- crow_joined %>%
    dplyr::select(expected_phoneme, age_months = age_months_m3sd, q_age_crow = prob_x_eq_1_hat)
  
  message("this is q_crowm3sd")
  print(q_crowm3sd)
  
  q_crowp3sd<- crow_joined %>%
    dplyr::select(expected_phoneme, age_months = age_months_p3sd, q_age_crow = prob_x_eq_1_hat)
  message("this is q_crowp3sd")
  print(q_crowp3sd)
  

  #############################################################################
  
    preds_with_q <- predicted %>%
    dplyr::inner_join(q_beta_join, by = c("expected_phoneme","age_months")) %>%
    dplyr::mutate(p_quant = pmax(0, pmin(1, 1 - q_age)))  # convert tail prob to CDF prob
  
  #######################################################
  preds_with_q_crow <- predicted %>%
    dplyr::inner_join(q_crow, by = c("expected_phoneme","age_months"))%>%
  dplyr::mutate(p_quant_crow = pmax(0, pmin(1, 1 - q_age_crow)))  # convert tail prob to CDF prob
  
  preds_with_q_crowm1 <- predicted %>%
    dplyr::inner_join(q_crowm1sd, by = c("expected_phoneme","age_months"))%>%
  dplyr::mutate(p_quant_crowm1 = pmax(0, pmin(1, 1 - q_age_crow)))  # convert tail prob to CDF prob
  
  preds_with_q_crowp1 <- predicted %>%
    dplyr::inner_join(q_crowp1sd, by = c("expected_phoneme","age_months"))%>%
  dplyr::mutate(p_quant_crowp1 = pmax(0, pmin(1, 1 - q_age_crow)))  # convert tail prob to CDF prob
  
  
  preds_with_q_crowm3 <- predicted %>%
    dplyr::inner_join(q_crowm3sd, by = c("expected_phoneme","age_months"))%>%
    dplyr::mutate(p_quant_crowm3 = pmax(0, pmin(1, 1 - q_age_crow)))  # convert tail prob to CDF prob
  
  preds_with_q_crowp3 <- predicted %>%
    dplyr::inner_join(q_crowp3sd, by = c("expected_phoneme","age_months"))%>%
    dplyr::mutate(p_quant_crowp3 = pmax(0, pmin(1, 1 - q_age_crow)))  # convert tail prob to CDF prob
  
  
  ########################################################
  message("preds_with_q:")
  print(preds_with_q, width = Inf)
  
  ####################################################
  
  message("preds_with_q_crow")
  print(preds_with_q_crow, width = Inf)
  
  message("preds_with_q_crowm1")
  print(preds_with_q_crowm1, width = Inf)
  
  message("preds_with_q_crowp1")
  print(preds_with_q_crowp1, width = Inf)
  
  message("preds_with_q_crowm3")
  print(preds_with_q_crowm3, width = Inf)
  
  message("preds_with_q_crowp3")
  print(preds_with_q_crowp3, width = Inf)
  #####################################################
  
  xq_predictive <- preds_with_q %>%
    dplyr::group_by(expected_phoneme, age_months, q_age) %>%
    dplyr::summarise(
      ### PILAS CAMBIAR SEGUN SE ESTE USANDO BETA O BINOMIAL PARA PLRR
      x_q = stats::quantile(.prediction, probs = unique(p_quant), names = FALSE, na.rm = TRUE),
      #x_q = stats::quantile(proportion, probs = unique(p_quant), names = FALSE, na.rm = TRUE),
      #x_q = stats::quantile(.resp, probs = unique(p_quant), names = FALSE, na.rm = TRUE),
      .groups = "drop"
    )
  ##############################################################################
  xq_predictive_crow <- preds_with_q_crow %>%
    dplyr::group_by(expected_phoneme, age_months, q_age_crow) %>%
    dplyr::summarise(
      ### PILAS CAMBIAR SEGUN SE ESTE USANDO BETA O BINOMIAL PARA PLLR
      x_q = stats::quantile(.prediction, probs = unique(p_quant_crow), names = FALSE, na.rm = TRUE),
      #x_q = stats::quantile(proportion, probs = unique(p_quant_crow), names = FALSE, na.rm = TRUE),
      #x_q = stats::quantile(.resp, probs = unique(p_quant), names = FALSE, na.rm = TRUE),
      .groups = "drop"
    )
  
  xq_predictive_crowm1 <- preds_with_q_crowm1 %>%
    dplyr::group_by(expected_phoneme, age_months, q_age_crow) %>%
    dplyr::summarise(
      x_q = stats::quantile(.prediction, probs = unique(p_quant_crowm1), names = FALSE, na.rm = TRUE),
      #x_q = stats::quantile(proportion, probs = unique(p_quant_crowm1), names = FALSE, na.rm = TRUE),
      #x_q = stats::quantile(.resp, probs = unique(p_quant), names = FALSE, na.rm = TRUE),
      .groups = "drop"
    )
  
  xq_predictive_crowp1 <- preds_with_q_crowp1 %>%
    dplyr::group_by(expected_phoneme, age_months, q_age_crow) %>%
    dplyr::summarise(
      x_q = stats::quantile(.prediction, probs = unique(p_quant_crowp1), names = FALSE, na.rm = TRUE),
      #x_q = stats::quantile(proportion, probs = unique(p_quant_crowp1), names = FALSE, na.rm = TRUE),
      #x_q = stats::quantile(.resp, probs = unique(p_quant), names = FALSE, na.rm = TRUE),
      .groups = "drop"
    )
  
  xq_predictive_crowm3 <- preds_with_q_crowm3 %>%
    dplyr::group_by(expected_phoneme, age_months, q_age_crow) %>%
    dplyr::summarise(
      x_q = stats::quantile(.prediction, probs = unique(p_quant_crowm3), names = FALSE, na.rm = TRUE),
      #x_q = stats::quantile(proportion, probs = unique(p_quant_crowm3), names = FALSE, na.rm = TRUE),
      #x_q = stats::quantile(.resp, probs = unique(p_quant), names = FALSE, na.rm = TRUE),
      .groups = "drop"
    )
  
  xq_predictive_crowp3 <- preds_with_q_crowp3 %>%
    dplyr::group_by(expected_phoneme, age_months, q_age_crow) %>%
    dplyr::summarise(
      x_q = stats::quantile(.prediction, probs = unique(p_quant_crowp3), names = FALSE, na.rm = TRUE),
      #x_q = stats::quantile(proportion, probs = unique(p_quant_crowp3), names = FALSE, na.rm = TRUE),
      #x_q = stats::quantile(.resp, probs = unique(p_quant), names = FALSE, na.rm = TRUE),
      .groups = "drop"
    )
  
  #############################################################
  message("this is xq_predictive")
  print(xq_predictive)
  ############################################################
  message("this is xq_predictive_crow")
  print(xq_predictive_crow)
  ############################################################
  message("this is xq_predictive_crowm1")
  print(xq_predictive_crowm1)
  ############################################################
  message("this is xq_predictive_crowp1")
  print(xq_predictive_crowp1)
  ############################################################
  message("this is xq_predictive_crowm3")
  print(xq_predictive_crowm3)
  ############################################################
  message("this is xq_predictive_crowp3")
  print(xq_predictive_crowp3)
  ############################################################
  
  #We are not using this threshold definition.
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
  # 
  # message("this is xq_mu")
  # print(xq_mu)
  
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
  
  # if (!is.null(xq_mu)) {
  #   age_plot <- age_plot +
  #     geom_line(
  #       data = xq_mu,
  #       aes(x = age_months/12, y = x_q_mu, linetype = "x_mu threshold"),
  #       color = "firebrick", linewidth = 0.9, inherit.aes = FALSE
  #     )
  # }
  
  age_plot <- age_plot +
    scale_linetype_manual(values = c(
      "Posterior median" = "solid",
      "x_q threshold"    = "longdash"#,
      #"x_mu threshold"   = "dotdash"
    ))
  print(age_plot)
  
  
  
  age_plot
  
  
  
  age_plot_crow <- ggplot(plot_data, aes(x = age_months/12, y = q50, color = expected_phoneme)) +
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
  
  # Overlays with fixed colors: dashed line + points + simple labels
  if (!is.null(xq_predictive_crow)) {
    x_max_months <- max(plot_data$age_months, na.rm = TRUE)
    
    xq_extended <- xq_predictive_crow %>%
      arrange(age_months) %>%
      {
        last_row <- slice_tail(., n = 1) %>%
          mutate(age_months = x_max_months)
        # if the max is already the last age, don't duplicate
        #if (x_max_months > last_row$age_months[1]) bind_rows(., last_row) else .
        bind_rows(., last_row)
      }
    
    
    xq_lab <- xq_extended %>%
      mutate(
        age_years = age_months / 12,
        coord_lab = sprintf("(%.1f, %.2f)", age_years, x_q)
      )
      
    
    age_plot_crow <- age_plot_crow +
      # dashed line with legend entry
      geom_line(
        data = xq_lab,
        aes(x = age_years, y = x_q, linetype = "x_q threshold Crowe & McLeod"),
        color = "black", linewidth = 0.9, inherit.aes = FALSE
      ) +
      # points at the x_q positions
      geom_point(
        data = xq_lab,
        aes(x = age_years, y = x_q),
        color = "black", size = 2.2, inherit.aes = FALSE
      ) +
      # simple (x, y) labels near points
      geom_text(
        data = xq_lab,
        aes(x = age_years, y = x_q, label = coord_lab),
        vjust = -0.7, size = 3, color = "black", inherit.aes = FALSE
      )
    
    }
  
  # if (!is.null(xq_predictive_crowm1)) {
  #   
  #   xq_labm1 <- xq_predictive_crowm1 %>%
  #     mutate(
  #       age_years = age_months / 12,
  #       coord_lab = sprintf("(%.1f, %.2f)", age_years, x_q)
  #     )
  #   
  #   age_plot_crow <- age_plot_crow +
  #     # dashed line with legend entry
  #     geom_line(
  #       data = xq_labm1,
  #       aes(x = age_years, y = x_q, linetype = "x_q C&M -1sd"),
  #       color = "blue", linewidth = 0.9, inherit.aes = FALSE
  #     ) +
  #     # points at the x_q positions
  #     geom_point(
  #       data = xq_labm1,
  #       aes(x = age_years, y = x_q),
  #       color = "blue", size = 2.2, inherit.aes = FALSE
  #     ) +
  #     # simple (x, y) labels near points
  #     geom_text(
  #       data = xq_labm1,
  #       aes(x = age_years, y = x_q, label = coord_lab),
  #       vjust = -0.7, size = 3, color = "blue", inherit.aes = FALSE
  #     )
  # }
  
  # if (!is.null(xq_predictive_crowp1)) {
  #   
  #   xq_labp1 <- xq_predictive_crowp1 %>%
  #     mutate(
  #       age_years = age_months / 12,
  #       coord_lab = sprintf("(%.1f, %.2f)", age_years, x_q)
  #     )
  #   
  #   age_plot_crow <- age_plot_crow +
  #     # dashed line with legend entry
  #     geom_line(
  #       data = xq_labp1,
  #       aes(x = age_years, y = x_q, linetype = "x_q C&M +1sd"),
  #       color = "green", linewidth = 0.9, inherit.aes = FALSE
  #     ) +
  #     # points at the x_q positions
  #     geom_point(
  #       data = xq_labp1,
  #       aes(x = age_years, y = x_q),
  #       color = "green", size = 2.2, inherit.aes = FALSE
  #     ) +
  #     # simple (x, y) labels near points
  #     geom_text(
  #       data = xq_labp1,
  #       aes(x = age_years, y = x_q, label = coord_lab),
  #       vjust = -0.7, size = 3, color = "green", inherit.aes = FALSE
  #     )
  # }
  
  # Make sure the legend key matches the label above
  age_plot_crow <- age_plot_crow +
    scale_linetype_manual(values = c(
      "Posterior median"                = "solid",
      "x_q threshold Crowe & McLeod"    = "longdash",
      "x_q C&M -1sd" = "longdash",
      "x_q C&M +1sd" = "longdash"
      
      
      
    ))
  age_plot_crow
  
  age_plot_crow2 <- ggplot(plot_data, aes(x = age_months, y = q50, color = expected_phoneme)) +
    geom_line(aes(linetype = "Posterior median"), linewidth = 1.1) +
    geom_ribbon(aes(ymin = q025, ymax = q975, fill = expected_phoneme), alpha = 0.2, color = NA) +
    geom_ribbon(aes(ymin = q25,  ymax = q75,  fill = expected_phoneme), alpha = 0.4, color = NA) +
    facet_wrap(~ expected_phoneme) +
    geom_point(
      data = df_points,
      aes(x = age_months, y = y_obs, color = expected_phoneme),
      inherit.aes = FALSE, alpha = 0.5, size = 1.5
    ) +
    labs(x = "Age (months)", y = y_label) 
    
  # Primary axis in months; secondary axis (top) in years
    # scale_x_continuous(
    #   breaks = pretty_breaks(n = 6),
    #   minor_breaks = pretty_breaks(n = 12),
    #   sec.axis = sec_axis(
    #     ~ . / 12,
    #     name   = "years",
    #     breaks = pretty_breaks(n = 6),
    #     labels = number_format(accuracy = 1)
    #   )
    # ) +
    
  
  # compute stable limits & ticks from your data
  xmin <- floor(min(plot_data$age_months, na.rm = TRUE) / 12) * 12
  xmax <- ceiling(max(plot_data$age_months, na.rm = TRUE) / 12) * 12
  if (!is.finite(xmin) || !is.finite(xmax) || xmin == xmax) { xmin <- 0; xmax <- 12 }  # fallback
  
  month_major <- seq(xmin, xmax, by = 12)   # ticks every 12 months
  month_minor <- seq(xmin, xmax, by = 6)    # minor ticks every 6 months (optional)
  year_major  <- seq(xmin/12, xmax/12, by = 1)
  
  age_plot_crow2 <- age_plot_crow2 +
    scale_x_continuous(
      name   = "Age (months)",
      limits = c(xmin, xmax),
      breaks = month_major,
      minor_breaks = month_minor,
      sec.axis = sec_axis(
        ~ . / 12,
        name   = "years",
        breaks = year_major,
        labels = scales::number_format(accuracy = 1)
      )
    ) + theme_minimal(base_size = 14) +
    theme(
      axis.title       = element_text(size = 16, face = "bold"),
      axis.text        = element_text(size = 14),
      strip.text       = element_text(size = 14, face = "bold"),
      legend.title     = element_blank(),
      legend.position  = "bottom",
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90"),
      # style the secondary (top) axis in a subtle grey
      axis.title.x.top = element_text(color = "grey30", face = "bold"),
      axis.text.x.top  = element_text(color = "grey40"),
      axis.ticks.x.top = element_line(color = "grey60")
    )
  
  ## 2) Overlay: extend x_q series and map x in MONTHS
  if (!is.null(xq_predictive_crow)) {
    x_max_months <- max(plot_data$age_months, na.rm = TRUE)
    
    xq_extended <- xq_predictive_crow %>%
      arrange(age_months) %>%
      {
        last_row <- slice_tail(., n = 1) %>%
          mutate(age_months = x_max_months)
        bind_rows(., last_row)
      } %>%
      mutate(
        age_years = age_months / 12,
        
        coord_lab = sprintf("(%dm, %.2f)", round(age_months), x_q)
      )
    xrange <- diff(range(plot_data$age_months, na.rm = TRUE))
    age_plot_crow2 <- age_plot_crow2 +
      geom_line(
        data = xq_extended,
        aes(x = age_months, y = x_q, linetype = "x_q threshold Crowe & McLeod"),
        color = "black", linewidth = 0.9, inherit.aes = FALSE
      ) +
      geom_point(
        data = xq_extended,
        aes(x = age_months, y = x_q),
        color = "black", size = 2.2, inherit.aes = FALSE
      ) +
      geom_text_repel(
        data = xq_extended,
        aes(x = age_months, y = x_q, label = coord_lab),
        inherit.aes = FALSE,
        size = 3,
        color = "black",
        direction = "both",        # spread labels vertically
        nudge_y = 0.08,         # gentle lift
        nudge_x = 0.02 * xrange,
        box.padding = 0.3,
        point.padding = 0.20,
        min.segment.length = 0, # always draw a leader line
        max.overlaps = Inf
      ) +
      theme(plot.margin = margin(5.5, 30, 5.5, 5.5))  # extra right margin if needed
  }
  
  ## 3) Keep your linetype legend mapping
  age_plot_crow2 <- age_plot_crow2 +
    scale_linetype_manual(values = c(
      "Posterior median"             = "solid",
      "x_q threshold Crowe & McLeod" = "longdash",
      "x_q C&M -1sd"                 = "longdash",
      "x_q C&M +1sd"                 = "longdash"
    ))
  
  age_plot_crow2
  
  ####### new plot adding deviations for the cut points definitions#############
  
  age_plot_crow3 <- ggplot(plot_data, aes(x = age_months, y = q50, color = expected_phoneme)) +
    geom_line(aes(linetype = "Posterior median"), linewidth = 1.1) +
    geom_ribbon(aes(ymin = q025, ymax = q975, fill = expected_phoneme), alpha = 0.2, color = NA) +
    geom_ribbon(aes(ymin = q25,  ymax = q75,  fill = expected_phoneme), alpha = 0.4, color = NA) +
    facet_wrap(~ expected_phoneme) +
    geom_point(
      data = df_points,
      aes(x = age_months, y = y_obs, color = expected_phoneme),
      inherit.aes = FALSE, alpha = 0.5, size = 1.5
    ) +
    labs(x = "Age (months)", y = y_label) 
    
    # Primary axis in months; secondary axis (top) in years
    # scale_x_continuous(
    #   breaks = pretty_breaks(n = 6),
    #   minor_breaks = pretty_breaks(n = 12),
    #   sec.axis = sec_axis(
    #     ~ . / 12,
    #     name   = "years",
    #     breaks = pretty_breaks(n = 6),
    #     labels = number_format(accuracy = 1)
    #   )
    # ) +
  
  xmin <- floor(min(plot_data$age_months, na.rm = TRUE) / 12) * 12
  xmax <- ceiling(max(plot_data$age_months, na.rm = TRUE) / 12) * 12
  if (!is.finite(xmin) || !is.finite(xmax) || xmin == xmax) { xmin <- 0; xmax <- 12 }  # fallback
  
  month_major <- seq(xmin, xmax, by = 12)   # ticks every 12 months
  month_minor <- seq(xmin, xmax, by = 6)    # minor ticks every 6 months (optional)
  year_major  <- seq(xmin/12, xmax/12, by = 1)
  
  age_plot_crow3 <- age_plot_crow3 +
    scale_x_continuous(
      name   = "Age (months)",
      limits = c(xmin, xmax),
      breaks = month_major,
      minor_breaks = month_minor,
      sec.axis = sec_axis(
        ~ . / 12,
        name   = "years",
        breaks = year_major,
        labels = scales::number_format(accuracy = 1)
      )
    ) + theme_minimal(base_size = 14) +
    theme(
      axis.title       = element_text(size = 16, face = "bold"),
      axis.text        = element_text(size = 14),
      strip.text       = element_text(size = 14, face = "bold"),
      legend.title     = element_blank(),
      legend.position  = "bottom",
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90"),
      # style the secondary (top) axis in a subtle grey
      axis.title.x.top = element_text(color = "grey30", face = "bold"),
      axis.text.x.top  = element_text(color = "grey40"),
      axis.ticks.x.top = element_line(color = "grey60")
    )
  
  ## Overlay: extend x_q series and map x in MONTHS
  if (!is.null(xq_predictive_crow)) {
    x_max_months <- max(plot_data$age_months, na.rm = TRUE)
    
    xq_extended <- xq_predictive_crow %>%
      arrange(age_months) %>%
      {
        last_row <- slice_tail(., n = 1) %>%
          mutate(age_months = x_max_months)
        bind_rows(., last_row)
      } %>%
      mutate(
        age_years = age_months / 12,
        
        coord_lab = sprintf("(%dm, %.2f)", round(age_months), x_q)
      )
    xrange <- diff(range(plot_data$age_months, na.rm = TRUE))
    
    age_plot_crow3 <- age_plot_crow3 +
      geom_line(
        data = xq_extended,
        aes(x = age_months, y = x_q, linetype = "x_q C&M"),
        color = "black", linewidth = 0.9, inherit.aes = FALSE
      ) +
      geom_point(
        data = xq_extended,
        aes(x = age_months, y = x_q),
        color = "black", size = 2.2, inherit.aes = FALSE
      ) +
      geom_text_repel(
        data = xq_extended,
        aes(x = age_months, y = x_q, label = coord_lab),
        inherit.aes = FALSE,
        size = 3,
        color = "black",
        direction = "both",        # spread labels vertically
        nudge_y = 0.08,         # gentle lift
        nudge_x = 0.02 * xrange,
        box.padding = 0.3,
        point.padding = 0.20,
        min.segment.length = 0, # always draw a leader line
        max.overlaps = Inf
      ) +
      theme(plot.margin = margin(5.5, 30, 5.5, 5.5))  # extra right margin if needed
  }
  
  
  if (!is.null(xq_predictive_crowm3)) {
    x_max_months <- max(plot_data$age_months, na.rm = TRUE)
    
    xq_extended <- xq_predictive_crowm3 %>%
      arrange(age_months) %>%
      {
        last_row <- slice_tail(., n = 1) %>%
          mutate(age_months = x_max_months)
        bind_rows(., last_row)
      } %>%
      mutate(
        age_years = age_months / 12,
        
        coord_lab = sprintf("(%dm, %.2f)", round(age_months), x_q)
      )
    
    age_plot_crow3 <- age_plot_crow3 +
      geom_line(
        data = xq_extended,
        aes(x = age_months, y = x_q, linetype = "x_q C&M -3sd"),
        color = "blue", linewidth = 0.9, inherit.aes = FALSE
      ) +
      geom_point(
        data = xq_extended,
        aes(x = age_months, y = x_q),
        color = "blue", size = 2.2, inherit.aes = FALSE
      ) +
      geom_text_repel(
        data = xq_extended,
        aes(x = age_months, y = x_q, label = coord_lab),
        inherit.aes = FALSE,
        size = 3,
        color = "black",
        direction = "both",        # spread labels vertically
        nudge_y = 0.08,         # gentle lift
        nudge_x = 0.02 * xrange,
        box.padding = 0.3,
        point.padding = 0.20,
        min.segment.length = 0, # always draw a leader line
        max.overlaps = Inf
      ) +
      theme(plot.margin = margin(5.5, 30, 5.5, 5.5))  # extra right margin if needed
  }
  
  if (!is.null(xq_predictive_crowp3)) {
    x_max_months <- max(plot_data$age_months, na.rm = TRUE)
    
    xq_extended <- xq_predictive_crowp3 %>%
      arrange(age_months) %>%
      {
        last_row <- slice_tail(., n = 1) %>%
          mutate(age_months = x_max_months)
        bind_rows(., last_row)
      } %>%
      mutate(
        age_years = age_months / 12,
        
        coord_lab = sprintf("(%dm, %.2f)", round(age_months), x_q)
      )
    
    age_plot_crow3 <- age_plot_crow3 +
      geom_line(
        data = xq_extended,
        aes(x = age_months, y = x_q, linetype = "x_q C&M +3sd"),
        color = "blue", linewidth = 0.9, inherit.aes = FALSE
      ) +
      geom_point(
        data = xq_extended,
        aes(x = age_months, y = x_q),
        color = "blue", size = 2.2, inherit.aes = FALSE
      ) +
      geom_text_repel(
        data = xq_extended,
        aes(x = age_months, y = x_q, label = coord_lab),
        inherit.aes = FALSE,
        size = 3,
        color = "black",
        direction = "both",        # spread labels vertically
        nudge_y = 0.08,         # gentle lift
        nudge_x = 0.02 * xrange,
        box.padding = 0.3,
        point.padding = 0.20,
        min.segment.length = 0, # always draw a leader line
        max.overlaps = Inf
      ) +
      theme(plot.margin = margin(5.5, 30, 5.5, 5.5))  # extra right margin if needed
  }
  
  
  ## 3) Keep your linetype legend mapping
  age_plot_crow3 <- age_plot_crow3 +
    scale_linetype_manual(values = c(
      "Posterior median"             = "solid",
      "x_q C&M" = "longdash",
      "x_q C&M -3sd"                 = "longdash",
      "x_q C&M +3sd"                 = "longdash"
    ))
  
  
  age_plot_crow3
  
  return(list(xq_predictive=xq_predictive,xq_mu=xq_mu, age_plot= age_plot, xq_predictive_crow=xq_predictive_crow, age_plot_crow=age_plot_crow, age_plot_crow2=age_plot_crow2, age_plot_crow3=age_plot_crow3))
}

################################################################
## Especificaciones de A (instancias AAPS)
raw_data_path <- "./Modeling_Pipeline/data/raw/AAPS Score Data (Long Version).csv"
phoneme_grouping_data_path <- "./Modeling_Pipeline/phoneme_grouping/phoneme_grouping2.csv"
subset_data_grouping_path <- "./Modeling_Pipeline/instance_specification/subset_data_grouping2.csv"
#instance_to_fit_path <- "./Modeling_Pipeline/instance_specification/instance_to_fit3.csv" #--->pilas usar con grouping1
## MODIFICAR ACORDE:
instance_to_fit_path <- "./Modeling_Pipeline/instance_specification/instance_to_fit_cp1V2.csv"

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
#dependiendo de si beta o binomial para pllr
instance_to_fit_path <- "./Modeling_Pipeline/instance_specification/instance_to_fit_cp2.csv"
#instance_to_fit_path <- "./Modeling_Pipeline/instance_specification/instance_to_fit_cp2V2.csv"

raw_data_type <- "pllr" # coherente con raw_data_path
phoneme_grouping_type <- "grouping2" # coherente con phoneme_grouping_data_path y con subset_data_grouping_path
model_type  <- "beta" 
#model_type  <- "binomial"
# debemos hablar mejor de response variable, porque esto detemrina el tipo de preprocesamiento 
# y el tipo de modelos que podemos usar. model_type debe ser el mismo que aparece en la columna model_type para todas las filas
# de instance_to_fit.csv!!


list_of_instancesB <- read_instances_specifications_lib$read_instances_specifications(instance_to_fit_path, subset_data_grouping_path,phoneme_grouping_data_path)

# Read preprocessed of raw_data used for instances B
preprocessed_result_listB <-read_preprocessed_files(raw_data_type,model_type,phoneme_grouping_type)
df_finalB <- preprocessed_result_listB$df_final
phoneme_numscore_modeB <- preprocessed_result_listB$phoneme_numscore_mode

agerangeA <- range(df_finalA$age_months)
agerangeB <- range(df_finalB$age_months)
agerange <- c(agerangeA[1],agerangeB[2])

# Probemos con
# no 8, no 4
k<- 10
instancia_pruebaA <- list_of_instancesA[[k]]

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
#model_typeB<- "binomial"
fitted_modelB <- readRDS(paste0(instancia_pruebaB$fitted_model_file_path,".rds"))

crow_joined <- read_crow_mcleod()

list_extracted_x <- extract_x_q_pllr_beta(model_typeB,
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
age_plot_crow2 <-list_extracted_x$age_plot_crow2
age_plot_crow3<-list_extracted_x$age_plot_crow3
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

########################################################################
########################################################################
########################################################################
iterate_find_cuts<-function(lista)
  
  ## Especificaciones de A (instancias AAPS)
  raw_data_path <- "./Modeling_Pipeline/data/raw/AAPS Score Data (Long Version).csv"
  phoneme_grouping_data_path <- "./Modeling_Pipeline/phoneme_grouping/phoneme_grouping2.csv"
  subset_data_grouping_path <- "./Modeling_Pipeline/instance_specification/subset_data_grouping2.csv"
  #instance_to_fit_path <- "./Modeling_Pipeline/instance_specification/instance_to_fit3.csv" #--->pilas usar con grouping1
  ##########################
  ## MODIFICAR ACORDE:
  instance_to_fit_path <- "./Modeling_Pipeline/instance_specification/instance_to_fit_cp1V2.csv"
  
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
  #instance_to_fit_path <- "./Modeling_Pipeline/instance_specification/instance_to_fit_cp2V2.csv"
  
  raw_data_type <- "pllr" # coherente con raw_data_path
  phoneme_grouping_type <- "grouping2" # coherente con phoneme_grouping_data_path y con subset_data_grouping_path
  model_type  <- "beta" 
  #model_type  <- "binomial"
  # debemos hablar mejor de response variable, porque esto detemrina el tipo de preprocesamiento 
  # y el tipo de modelos que podemos usar. model_type debe ser el mismo que aparece en la columna model_type para todas las filas
  # de instance_to_fit.csv!!
  
  
  list_of_instancesB <- read_instances_specifications_lib$read_instances_specifications(instance_to_fit_path, subset_data_grouping_path,phoneme_grouping_data_path)
  
  # Read preprocessed of raw_data used for instances B
  
  preprocessed_result_listB <-read_preprocessed_files(raw_data_type,model_type,phoneme_grouping_type)
  df_finalB <- preprocessed_result_listB$df_final
  phoneme_numscore_modeB <- preprocessed_result_listB$phoneme_numscore_mode

  
  agerangeA <- range(df_finalA$age_months)
  agerangeB <- range(df_finalB$age_months)
  agerange <- c(agerangeA[1],agerangeB[2])

  for(k in lista){
  instancia_pruebaA <- list_of_instancesA[[k]]
  
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
  #model_typeB<- "binomial"
  fitted_modelB <- readRDS(paste0(instancia_pruebaB$fitted_model_file_path,".rds"))
  
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
  
  plot_name <- paste0("age_plot_",k,".png")
  plot_place <- paste0("./output/bayesian_model/", model_name,"/", plot_name)
  ggsave(plot_place, plot = age_plot, width = 8, height = 6, dpi = 300)
  
}

