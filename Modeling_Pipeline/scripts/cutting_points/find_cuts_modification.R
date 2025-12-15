library(tidyverse)
library(dplyr)
library(posterior)
library(yaml)
library(purrr)
library(glue)
library(tidybayes)
library(scales)
library(ggrepel)


Paths <- modules::use("./Modeling_Pipeline/pipeline/config/file_paths.R")
read_instances_specifications_lib <- modules::use("./Modeling_Pipeline/scripts/preprocess/read_instance_specification.R")
cutting_points_plots_lib<- modules::use("./Modeling_Pipeline/scripts/cutting_points/plot_cutting_points.R")
##############################################################################

###############################################################################
# Read and format Crow

read_crow_mcleod <- function(){
  file_path <- file.path(Paths$Pipeline_preprocesseddata_dir,"crow-mcleod.csv")
  crow_mcleod <- read.csv(file_path)
  
  
  crow_mean  <- crow_mcleod %>%
    transmute(
      expected_phoneme = phoneme,
      group,
      `0.50` = fifty_percent,
      `0.75` = seventyfive_percent,
      `0.90` = ninety_percent
    ) %>%
    tidyr::pivot_longer(
      cols = c(`0.50`, `0.75`, `0.90`),
      names_to = "prob_x_eq_1_hat",
      values_to = "age_months"
    ) %>%
    mutate(prob_x_eq_1_hat = as.numeric(prob_x_eq_1_hat),
           age_months = as.numeric(round(age_months))
    )
  
  #sd adjustments
  crow_sd <- crow_mcleod %>%
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
           age_months_sd = as.numeric(round(age_months_sd))
    )
  
  # join mean + sd columns:
  crow_joined_base <- crow_mean%>%
    dplyr::left_join(crow_sd,
                     by=c("expected_phoneme","group","prob_x_eq_1_hat")
                     )
  # %>%
  #   dplyr::mutate(age_months_m1sd = age_months-age_months_sd,
  #                 age_months_p1sd = age_months+age_months_sd,
  #                 age_months_m2sd = age_months -2*age_months_sd,
  #                 age_months_p2sd = age_months +2*age_months_sd,
  #                 age_months_m3sd = age_months -3*age_months_sd,
  #                 age_months_p3sd = age_months +3*age_months_sd
  #   )
  
  crow_joined_long <- crow_joined_base %>%
    tidyr::expand_grid(type = c("mean","m1","p1","m2","p2","m3","p3")) %>%
    mutate(
      age_months = dplyr::case_when(
        type == "mean" ~ age_months,
        type == "m1"   ~ age_months - age_months_sd,
        type == "p1"   ~ age_months + age_months_sd,
        type == "m2"   ~ age_months - 2*age_months_sd,
        type == "p2"   ~ age_months + 2*age_months_sd,
        type == "m3"   ~ age_months - 3*age_months_sd,
        type == "p3"   ~ age_months + 3*age_months_sd
      )
    ) %>%
    select(expected_phoneme, group, prob_x_eq_1_hat, type, age_months)
  
  return(crow_joined_long)
  
  
  
  # return(list(crow_mcleod_long=crow_mcleod_long,crow_mcleod_long_sd=crow_mcleod_long_sd))
  #return(crow_joined)
  
}

####################################################################

extract_q_aaps_binomial <- function(
    # model_type=c("binomial"),
    # phoneme_numscore_mode, #only used for binomial                                        
    # agerange,
    # instance,
    # fitted_model,
    # success_frac = 1.0,            # f in [0,1]
    # target_rule = c("ceil","round","floor")  # how to turn f*n into k
  
  instance,
  fitted_model,
  phoneme_numscore_mode,
  agerange,
  succes_frac){
  
  
  #instance$model_type  <- match.arg(model_type)       # here it's always "binomial"
  # target_rule <- match.arg(target_rule) ---------------> what is the target rule we are using here?
  model_type <- instance$model_type
  
  
    newdata <- tidyr::crossing(
    age_months       = seq(agerange[1], agerange[2], by = 1), # RANGE!#seq(0, 90, by = 1), # RANGE!
    expected_phoneme = instance$target_phonemes,#phonemes,
    speaker          = "fake" # create a new unseen level in the grid
    
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
  
    # Receive a model (object) fitted to some observed data, y_obs, and predictors
    # x_obs. Given new values of predictors, x_new, supplied in the data frame newdata,
    # add_predicted_draws() adds draws from the posterior predictive distribution p(y_new| x_new, y_obs)
    # to the data. It corresponds to rstanarm::posterior_predict() or brms::posterior_predict().
    
    # re_formula: formula containing group-level effects to be considered in the prediction. If NULL
    # include all group-level effects; if NA, include no group-level effects. If you want to marginalize 
    # over grouping factors specify a new level of a factor in newdata and use allow_new_levels=TRUE
    
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
      n   = dplyr::first(num_score), # total number of trials is constant per group, take the first one
      k   = dplyr::first(target_successes), # number of observed successes per group, take the first one
      # proportion of draws where the predicted number of success is equal to k successes 
      prob_x_eq_1_hat = mean(.prediction == dplyr::first(target_successes)),
      .groups = "drop"
    )
  
  # Example:
    # | expected_phoneme | age_months | n  | k | prob_x_eq_1_hat |
    # | ---------------- | ---------- | -- | - | --------------- |
    # | R                | 48         | 10 | 7 | 0.15            |
    # The model predits k succeses 15% of the posterior draws for the phoneme R at 48 months
    
  
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
extract_x_q_pllr_beta <- function(
    # model_type=c("binomial","beta"),
    # phoneme_numscore_mode, #only used for binomial                                        
    # agerange,
    # instance,
    # fitted_model,
    # q_atleast1_ppc,
    # crow_joined
  instance,
  fitted_model,
  phoneme_numscore_mode,
  agerange,
  q_age,
  crowe_mcleod
){
  
  model_type <- instance$model_type
  #instance$model_type <- match.arg(model_type)
  resp_col   <- if (model_type == "binomial") "proportion" else if (model_type == "beta") "mean_prob"
  y_label    <- if (model_type == "binomial") "Probability of success" else if (model_type == "beta")  "Phoneme goodness score"
  
  age_lower_bound <- max(min(crowe_mcleod$age_months, agerange[1]), 4)
  # message("this is age_lower bound")
  # print(age_lower_bound)
  
  newdata <- tidyr::crossing(
    age_months       = seq(age_lower_bound, agerange[2], by = 1),
    expected_phoneme = instance$target_phonemes,#phonemes,
    speaker          = "fake" 
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
  # Receive a model (object) fitted to some observed data, y_obs, and predictors
  # x_obs. Given new values of predictors, x_new, supplied in the data frame newdata,
  # add_predicted_draws() adds draws from the posterior predictive distribution p(y_new| x_new, y_obs)
  # to the data. It corresponds to rstanarm::posterior_predict() or brms::posterior_predict().
  
  # re_formula: formula containing group-level effects to be considered in the prediction. If NULL
  # include all group-level effects; if NA, include no group-level effects. If you want to marginalize 
  # over grouping factors specify a new level of a factor in newdata and use allow_new_levels=TRUE
  
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
  # q_crow <- crow_joined %>%
  #   dplyr::select(expected_phoneme,age_months, q_age_crow = prob_x_eq_1_hat)
  # message("this is q_crow")
  # print(q_crow)
  # 
  # q_crowm1sd<- crow_joined %>%
  #   dplyr::select(expected_phoneme, age_months = age_months_m1sd, q_age_crow = prob_x_eq_1_hat)
  # 
  # message("this is q_crowm1sd")
  # print(q_crowm1sd)
  # 
  # q_crowp1sd<- crow_joined %>%
  #   dplyr::select(expected_phoneme, age_months = age_months_p1sd, q_age_crow = prob_x_eq_1_hat)
  # message("this is q_crowp1sd")
  # print(q_crowp1sd)
  # 
  # q_crowm3sd<- crow_joined %>%
  #   dplyr::select(expected_phoneme, age_months = age_months_m3sd, q_age_crow = prob_x_eq_1_hat)
  # 
  # message("this is q_crowm3sd")
  # print(q_crowm3sd)
  # 
  # q_crowp3sd<- crow_joined %>%
  #   dplyr::select(expected_phoneme, age_months = age_months_p3sd, q_age_crow = prob_x_eq_1_hat)
  # message("this is q_crowp3sd")
  # print(q_crowp3sd)
  # 
  # q_crowm2sd<- crow_joined %>%
  #   dplyr::select(expected_phoneme, age_months = age_months_m2sd, q_age_crow = prob_x_eq_1_hat)
  # 
  # message("this is q_crowm2sd")
  # print(q_crowm2sd)
  # 
  # q_crowp2sd<- crow_joined %>%
  #   dplyr::select(expected_phoneme, age_months = age_months_p2sd, q_age_crow = prob_x_eq_1_hat)
  # message("this is q_crowp2sd")
  # print(q_crowp2sd)
  
  crow_grouped <- crowe_mcleod %>% 
    dplyr::rename(q_age = prob_x_eq_1_hat)%>% 
    dplyr::group_by(type)
  
  # group_keys() returns a tibble containing one row per group, showing the unique combinations of grouping variables
  # used in group_by (like index in python)
  # pull extract the 'type' in each index.
  crow_tables <- crow_grouped %>% 
    dplyr::group_split() %>%
    purrr::set_names(crow_grouped %>% dplyr::group_keys() %>% dplyr::pull(type))
  
  message("crow_tables:")
  print(crow_tables)
  
  crow_tables[["prediction"]] <- q_beta_join
  
  

  #############################################################################
  
  preds_with_q <- predicted %>%
    dplyr::inner_join(q_beta_join, by = c("expected_phoneme","age_months")) %>%
    dplyr::mutate(p_quant = pmax(0, pmin(1, 1 - q_age)))  # convert tail prob to CDF prob
  
  #######################################################
  # preds_with_q_crow <- predicted %>%
  #   dplyr::inner_join(q_crow, by = c("expected_phoneme","age_months"))%>%
  #   dplyr::mutate(p_quant_crow = pmax(0, pmin(1, 1 - q_age_crow)))  # convert tail prob to CDF prob
  # 
  # preds_with_q_crowm1 <- predicted %>%
  #   dplyr::inner_join(q_crowm1sd, by = c("expected_phoneme","age_months"))%>%
  #   dplyr::mutate(p_quant_crowm1 = pmax(0, pmin(1, 1 - q_age_crow)))  # convert tail prob to CDF prob
  # 
  # preds_with_q_crowp1 <- predicted %>%
  #   dplyr::inner_join(q_crowp1sd, by = c("expected_phoneme","age_months"))%>%
  #   dplyr::mutate(p_quant_crowp1 = pmax(0, pmin(1, 1 - q_age_crow)))  # convert tail prob to CDF prob
  # 
  # 
  # preds_with_q_crowm3 <- predicted %>%
  #   dplyr::inner_join(q_crowm3sd, by = c("expected_phoneme","age_months"))%>%
  #   dplyr::mutate(p_quant_crowm3 = pmax(0, pmin(1, 1 - q_age_crow)))  # convert tail prob to CDF prob
  # 
  # preds_with_q_crowp3 <- predicted %>%
  #   dplyr::inner_join(q_crowp3sd, by = c("expected_phoneme","age_months"))%>%
  #   dplyr::mutate(p_quant_crowp3 = pmax(0, pmin(1, 1 - q_age_crow)))  # convert tail prob to CDF prob
  # 
  # preds_with_q_crowm2 <- predicted %>%
  #   dplyr::inner_join(q_crowm2sd, by = c("expected_phoneme","age_months"))%>%
  #   dplyr::mutate(p_quant_crowm2 = pmax(0, pmin(1, 1 - q_age_crow)))  # convert tail prob to CDF prob
  # 
  # preds_with_q_crowp2 <- predicted %>%
  #   dplyr::inner_join(q_crowp2sd, by = c("expected_phoneme","age_months"))%>%
  #   dplyr::mutate(p_quant_crowp2 = pmax(0, pmin(1, 1 - q_age_crow)))  # convert tail prob to CDF prob
  # 
  # 
  ########################################################
  message("preds_with_q:")
  print(preds_with_q, width = Inf)

  ####################################################
  # 
  # message("preds_with_q_crow")
  # print(preds_with_q_crow, width = Inf)
  # 
  # message("preds_with_q_crowm1")
  # print(preds_with_q_crowm1, width = Inf)
  # 
  # message("preds_with_q_crowp1")
  # print(preds_with_q_crowp1, width = Inf)
  # 
  # message("preds_with_q_crowm3")
  # print(preds_with_q_crowm3, width = Inf)
  # 
  # message("preds_with_q_crowp3")
  # print(preds_with_q_crowp3, width = Inf)
  # 
  # message("preds_with_q_crowm2")
  # print(preds_with_q_crowm2, width = Inf)
  # 
  # message("preds_with_q_crowp2")
  # print(preds_with_q_crowp2, width = Inf)
  
  
  # crow_tables <- list(
  #   crow      = q_crow,
  #   crow_m1   = q_crowm1sd,
  #   crow_p1   = q_crowp1sd,
  #   crow_m2   = q_crowm2sd,
  #   crow_p2   = q_crowp2sd,
  #   crow_m3   = q_crowm3sd,
  #   crow_p3   = q_crowp3sd
  # )
  # 
  compute_xq <- function(predicted, crow_tbl, label) {
    
    preds_with_q <- predicted %>%
      dplyr::inner_join(crow_tbl, by = c("expected_phoneme", "age_months")) %>%
      dplyr::mutate(
        p_quant = pmax(0, pmin(1, 1 - q_age))
      )
    
    xq <- preds_with_q %>%
      dplyr::group_by(expected_phoneme, age_months, q_age) %>%
      dplyr::summarise(
        x_q = stats::quantile(.prediction, probs = unique(p_quant),
                              names = FALSE, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(version = label)
    
    return(xq)
  }
  
  results_list <- purrr::imap(
    crow_tables,
    ~ compute_xq(predicted, .x, .y)
  )
  
  xq_all <- dplyr::bind_rows(results_list)
  
  
  #####################################################
  
  # xq_predictive <- preds_with_q %>%
  #   dplyr::group_by(expected_phoneme, age_months, q_age) %>%
  #   dplyr::summarise(
  #     ### PILAS CAMBIAR SEGUN SE ESTE USANDO BETA O BINOMIAL PARA PLRR
  #     x_q = stats::quantile(.prediction, probs = unique(p_quant), names = FALSE, na.rm = TRUE),
  #     #x_q = stats::quantile(proportion, probs = unique(p_quant), names = FALSE, na.rm = TRUE),
  #     #x_q = stats::quantile(.resp, probs = unique(p_quant), names = FALSE, na.rm = TRUE),
  #     .groups = "drop"
  #   )
  # ##############################################################################
  # xq_predictive_crow <- preds_with_q_crow %>%
  #   dplyr::group_by(expected_phoneme, age_months, q_age_crow) %>%
  #   dplyr::summarise(
  #     ### PILAS CAMBIAR SEGUN SE ESTE USANDO BETA O BINOMIAL PARA PLLR
  #     x_q = stats::quantile(.prediction, probs = unique(p_quant_crow), names = FALSE, na.rm = TRUE),
  #     #x_q = stats::quantile(proportion, probs = unique(p_quant_crow), names = FALSE, na.rm = TRUE),
  #     #x_q = stats::quantile(.resp, probs = unique(p_quant), names = FALSE, na.rm = TRUE),
  #     .groups = "drop"
  #   )
  # 
  # xq_predictive_crowm1 <- preds_with_q_crowm1 %>%
  #   dplyr::group_by(expected_phoneme, age_months, q_age_crow) %>%
  #   dplyr::summarise(
  #     x_q = stats::quantile(.prediction, probs = unique(p_quant_crowm1), names = FALSE, na.rm = TRUE),
  #     #x_q = stats::quantile(proportion, probs = unique(p_quant_crowm1), names = FALSE, na.rm = TRUE),
  #     #x_q = stats::quantile(.resp, probs = unique(p_quant), names = FALSE, na.rm = TRUE),
  #     .groups = "drop"
  #   )
  # 
  # xq_predictive_crowp1 <- preds_with_q_crowp1 %>%
  #   dplyr::group_by(expected_phoneme, age_months, q_age_crow) %>%
  #   dplyr::summarise(
  #     x_q = stats::quantile(.prediction, probs = unique(p_quant_crowp1), names = FALSE, na.rm = TRUE),
  #     #x_q = stats::quantile(proportion, probs = unique(p_quant_crowp1), names = FALSE, na.rm = TRUE),
  #     #x_q = stats::quantile(.resp, probs = unique(p_quant), names = FALSE, na.rm = TRUE),
  #     .groups = "drop"
  #   )
  # 
  # xq_predictive_crowm3 <- preds_with_q_crowm3 %>%
  #   dplyr::group_by(expected_phoneme, age_months, q_age_crow) %>%
  #   dplyr::summarise(
  #     x_q = stats::quantile(.prediction, probs = unique(p_quant_crowm3), names = FALSE, na.rm = TRUE),
  #     #x_q = stats::quantile(proportion, probs = unique(p_quant_crowm3), names = FALSE, na.rm = TRUE),
  #     #x_q = stats::quantile(.resp, probs = unique(p_quant), names = FALSE, na.rm = TRUE),
  #     .groups = "drop"
  #   )
  # 
  # xq_predictive_crowp3 <- preds_with_q_crowp3 %>%
  #   dplyr::group_by(expected_phoneme, age_months, q_age_crow) %>%
  #   dplyr::summarise(
  #     x_q = stats::quantile(.prediction, probs = unique(p_quant_crowp3), names = FALSE, na.rm = TRUE),
  #     #x_q = stats::quantile(proportion, probs = unique(p_quant_crowp3), names = FALSE, na.rm = TRUE),
  #     #x_q = stats::quantile(.resp, probs = unique(p_quant), names = FALSE, na.rm = TRUE),
  #     .groups = "drop"
  #   )
  # 
  # xq_predictive_crowm2 <- preds_with_q_crowm2 %>%
  #   dplyr::group_by(expected_phoneme, age_months, q_age_crow) %>%
  #   dplyr::summarise(
  #     x_q = stats::quantile(.prediction, probs = unique(p_quant_crowm2), names = FALSE, na.rm = TRUE),
  #     #x_q = stats::quantile(proportion, probs = unique(p_quant_crowm2), names = FALSE, na.rm = TRUE),
  #     #x_q = stats::quantile(.resp, probs = unique(p_quant), names = FALSE, na.rm = TRUE),
  #     .groups = "drop"
  #   )
  # 
  # xq_predictive_crowp2 <- preds_with_q_crowp2 %>%
  #   dplyr::group_by(expected_phoneme, age_months, q_age_crow) %>%
  #   dplyr::summarise(
  #     x_q = stats::quantile(.prediction, probs = unique(p_quant_crowp2), names = FALSE, na.rm = TRUE),
  #     #x_q = stats::quantile(proportion, probs = unique(p_quant_crowp2), names = FALSE, na.rm = TRUE),
  #     #x_q = stats::quantile(.resp, probs = unique(p_quant), names = FALSE, na.rm = TRUE),
  #     .groups = "drop"
  #   )
  # 
  # #############################################################
  # message("this is xq_predictive")
  # print(xq_predictive)
  # ############################################################
  # message("this is xq_predictive_crow")
  # print(xq_predictive_crow)
  # ############################################################
  # message("this is xq_predictive_crowm1")
  # print(xq_predictive_crowm1)
  # ############################################################
  # message("this is xq_predictive_crowp1")
  # print(xq_predictive_crowp1)
  # ############################################################
  # message("this is xq_predictive_crowm3")
  # print(xq_predictive_crowm3)
  # ############################################################
  # message("this is xq_predictive_crowp3")
  # print(xq_predictive_crowp3)
  # ############################################################
  # message("this is xq_predictive_crowm2")
  # print(xq_predictive_crowm2)
  # ############################################################
  # message("this is xq_predictive_crowp2")
  # print(xq_predictive_crowp2)
  
  # #We are not using this threshold definition.
  # mu_draws <- tidybayes::add_epred_draws(
  #   newdata = newdata,
  #   object  = fitted_model,
  #   re_formula = NULL
  # )
  # 
  # 
  # mu_with_q <- mu_draws %>%
  #   dplyr::inner_join(q_beta_join, by = c("expected_phoneme","age_months")) %>%
  #   dplyr::mutate(p_quant = pmax(0, pmin(1, 1 - q_age)))
  # 
  # xq_mu <- mu_with_q %>%
  #   dplyr::group_by(expected_phoneme, age_months, q_age) %>%
  #   dplyr::summarise(
  #     x_q_mu = stats::quantile(.epred, probs = unique(p_quant), names = FALSE, na.rm = TRUE),
  #     .groups = "drop"
  #   )
  # 
  # message("this is xq_mu")
  # print(xq_mu)
  
  age_plot <- cutting_points_plots_lib$plot_cutting_points_v1(plot_data,
                                                       df_points,
                                                       y_label,
                                                       xq_all,
                                                       label1 = "x_q threshold", 
                                                       color1 = "black")
  
  age_plot_crow4 <- cutting_points_plots_lib$plot_cutting_points(plot_data, df_points, y_label, xq_all,
                                                   c = "mean", 
                                                   m2 = "m2",
                                                   p2 = "p2",
                                                   label1 = "C&M mean",
                                                   label2 = "C&M -2sd",
                                                   label3 = "C&M +2sd",
                                                   color1 = "black",
                                                   color2 = "blue",
                                                   color3 = "green")
  
  #######################################################################
  
  # return(list(xq_predictive=xq_predictive,
  #             xq_mu=xq_mu, 
  #             age_plot= age_plot, 
  #             xq_predictive_crow=xq_predictive_crow, 
  #             age_plot_crow=age_plot_crow, 
  #             age_plot_crow2=age_plot_crow2, 
  #             age_plot_crow3=age_plot_crow3,
  #             age_plot_crow4=age_plot_crow4,
  #             age_plot_crow5=age_plot_crow5))

  
  return(list(xq_all=xq_all,crow_tables=crow_tables, age_plot=age_plot, age_plot_crow4=age_plot_crow4))

}



###############################################################################
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


extract_q_result <- extract_q_aaps_binomial(
  
  # instancia_pruebaA$model_type,
  # phoneme_numscore_modeA, #only used for binomial                                        
  # #agerangeA,
  # #agerangeB,
  # agerange,
  # instancia_pruebaA,
  # fitted_modelA,
  # success_frac,
  # target_rule
  
  instancia_pruebaA,
  fitted_modelA,
  phoneme_numscore_modeA,
  agerange,
  success_frac
  )


q_atleast1_ppc<- extract_q_result$q_atleast1_ppc
q_exact1_ppc<- extract_q_result$q_exact1_ppc
q_atleast_k_ppc<- extract_q_result$q_atleast_k_ppc
q_exact_k_ppc<-extract_q_result$q_exact_k_ppc
plot_post_mean<- extract_q_result$plot_post_mean




crow_joined <- read_crow_mcleod()

list_extracted_x <- extract_x_q_pllr_beta(
  
  # instancia_pruebaB$model_type,
  # phoneme_numscore_modeB, #only used for binomial                                        
  # #agerangeB,
  # #agerangeA,
  # agerange,
  # instancia_pruebaB,
  # fitted_modelB,
  # q_exact_k_ppc,
  # #q_atleast1_ppc
  # #q_exact1_ppc
  # crow_joined # sept22
  
  instancia_pruebaB,
  fitted_modelB,
  phoneme_numscore_modeB,
  agerange,
  q_exact_k_ppc,
  crow_joined
)


crow_tables <- list_extracted_x$crow_tables
xq_all <- list_extracted_x$xq_all

age_plot <-list_extracted_x$age_plot
age_plot_crow4 <-list_extracted_x$age_plot_crow4




# xq_predictive<- list_extracted_x$xq_predictive
# xq_mu <- list_extracted_x$xq_mu
# 
# age_plot <- list_extracted_x$age_plot
# age_plot_crow <-list_extracted_x$age_plot_crow
# age_plot_crow2<-list_extracted_x$age_plot_crow2
# age_plot_crow3<-list_extracted_x$age_plot_crow3
# age_plot_crow4<-list_extracted_x$age_plot_crow4
# age_plot_crow5<-list_extracted_x$age_plot_crow5
# xq_predictive_crow<- list_extracted_x$xq_predictive_crow
# 
# message("xq_predictive:")
# print(xq_predictive)
# #message("xq_mu:")
# #print(xq_mu)
# age_plot
# plot_post_mean
# age_plot_crow
# age_plot_crow2
# age_plot_crow3
# age_plot_crow4
# age_plot_crow5