library(tidyverse)
library(dplyr)
library(posterior)
library(yaml)
library(purrr)
library(glue)
library(tidybayes)
library(scales)
library(ggrepel)
library(assertthat)


Paths                             <- modules::use("./Modeling_Pipeline/pipeline/config/file_paths.R")
read_instances_specifications_lib <- modules::use("./Modeling_Pipeline/scripts/preprocess/read_instance_specification.R")
cutting_points_plots_lib          <- modules::use("./Modeling_Pipeline/scripts/cutting_points/plot_cutting_points.R")
preprocessing_lib                 <- modules::use("./Modeling_Pipeline/scripts/preprocess/Preprocessing.R")

#' Compute posterior predictive probabilities for fixed-success binomial targets (AAPS)
#'
#' Given a fitted binomial Bayesian model for AAPS data, this function computes
#' posterior predictive quantities for a fixed target number of successes across
#' age and phoneme. The target number of successes is derived from a desired
#' success fraction and a rule for approximating to an integer.
#'
#' For each phoneme and age, the function evaluates the posterior predictive
#' probability that the model predicts exactly \eqn{k} successes out of
#' \eqn{n} trials, where \eqn{n} is fixed to the modal number of trials observed
#' in the data.
#'
#' @param instance A \code{ModelInstance} object describing the fitted model,
#'   including target phonemes and model type.
#' @param fitted_model A fitted \code{brms} model object with a binomial likelihood.
#' @param phoneme_numscore_mode Data frame containing the modal number of trials
#'   (\code{mode_num_score}) per phoneme.
#' @param agerange Numeric vector of length two giving the minimum and maximum
#'   age (in months) over which predictions are computed.
#' @param success_frac Numeric scalar in \eqn{[0,1]}. Desired fraction of
#'   successes used to define the target number of successes.
#' @param target_rule Character scalar. Rule used to map the fractional target
#'   \eqn{f \times n} to an integer number of successes. Must be one of
#'   \code{"ceil"}, \code{"floor"}, or \code{"round"}.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{q_exact_k_ppc}{A data frame containing posterior predictive probabilities
#'   \eqn{P(\hat{Y} = k \mid \text{age}, \text{phoneme})} for each phoneme–age pair.}
#'   \item{plot_post_mean}{A \code{ggplot} object showing the posterior mean number
#'   of predicted successes as a function of age (in years).}
#' }
#'
#' @details
#' The function:
#' \itemize{
#'   \item Constructs a prediction grid over age and target phonemes.
#'   \item Fixes the total number of trials \eqn{n} to the modal observed value
#'     per phoneme.
#'   \item Converts the desired success fraction into an integer target
#'     \eqn{k} using the specified approx. rule (take floor or ceil etc).
#'   \item Draws from the posterior predictive distribution using
#'     \code{tidybayes::add_predicted_draws()}.
#'   \item Estimates the probability that the predicted number of successes
#'     equals the target \eqn{k}.
#' }
#'
#' Predictions include group-level effects by allowing new levels for the
#' grouping factor (e.g., a synthetic speaker).
#'
#' @seealso \code{\link{tidybayes::add_predicted_draws}},
#'   \code{\link{fit_bayesian_model_funct}}
#'
#' @export

extract_q_aaps_binomial <- function(
    instance,
    fitted_model,
    phoneme_numscore_mode,
    agerange,
    success_frac,
    target_rule){
  
  model_type <- instance$model_type
  
  
    newdata <- tidyr::crossing(
    age_months       = seq(agerange[1], agerange[2], by = 1),
    expected_phoneme = instance$target_phonemes,
    speaker          = "fake" 
    
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
  
  
  
  
  result<-list(
    q_exact_k_ppc   = q_exact_k_ppc,
    plot_post_mean = plot_post_mean
  )
  return(result)
  
}
###############################################################################

#' Extract age-dependent quantile cutpoints from a PLLR beta (or binomial) model
#'
#' Computes age- and phoneme-specific quantile cutpoints (\eqn{x_q}) from the
#' posterior predictive distribution of a fitted PLLR model. The quantile levels
#' are determined by externally supplied age-dependent tail probabilities,
#' including reference curves derived from Crowe & McLeod norms.
#'
#' For each phoneme and age, the function evaluates the posterior predictive
#' distribution and extracts the response value \eqn{x_q} such that
#' \eqn{P(\hat{Y} \le x_q) = 1 - q(\text{age})}, where \eqn{q(\text{age})} is a
#' phoneme- and age-specific tail probability.
#'
#' @param instance A \code{ModelInstance} object describing the fitted PLLR model,
#'   including target phonemes and file paths.
#' @param fitted_model A fitted \code{brms} model object (beta or binomial).
#' @param phoneme_numscore_mode Data frame containing the modal number of trials
#'   per phoneme (used only for binomial models).
#' @param agerange Numeric vector of length two giving the minimum and maximum
#'   age (in months) over which predictions are computed.
#' @param q_age Data frame containing age- and phoneme-specific tail
#'   probabilities (\code{prob_x_eq_1_hat}) used to define quantile levels.
#' @param crowe_mcleod Data frame containing Crowe & McLeod reference curves
#'   (e.g., mean, \eqn{\pm}2 SD) expressed as age-dependent tail probabilities.
#'
#' @return A list with three elements:
#' \describe{
#'   \item{xq_all}{A data frame containing extracted quantile cutpoints
#'   \eqn{x_q} for each phoneme, age, and reference curve type.}
#'   \item{crow_tables}{A named list of reference tables (e.g., mean, -2SD, +2SD)
#'   used to compute quantiles.}
#'   \item{age_plot_crow4}{A \code{ggplot} object visualizing posterior predictive
#'   summaries, observed data, and extracted cutpoints.}
#' }
#'
#' @details
#' The function proceeds as follows:
#' \itemize{
#'   \item Constructs a prediction grid over age and target phonemes.
#'   \item Generates posterior predictive draws using
#'     \code{tidybayes::add_predicted_draws()} with group-level effects included.
#'   \item Normalizes model outputs to a unified response scale
#'     (probability or PLLR score).
#'   \item Computes posterior median and credible intervals (50\%, 95\%).
#'   \item Converts age-dependent tail probabilities into CDF quantiles.
#'   \item Extracts posterior predictive quantiles corresponding to
#'     Crowe & McLeod reference curves.
#' }
#'
#' For binomial models, predictions are converted to proportions using the
#' modal number of trials per phoneme. Observed data are overlaid with small
#' jitter for visualization purposes only.
#'
#' @seealso \code{\link{extract_q_aaps_binomial}},
#'   \code{\link{tidybayes::add_predicted_draws}},
#'   \code{\link{cutting_points_plots_lib$plot_cutting_points}}
#'
#' @export
extract_x_q_pllr_beta <- function(
                                  instance,
                                  fitted_model,
                                  phoneme_numscore_mode,
                                  agerange,
                                  q_age,
                                  crowe_mcleod
                                ){
  
  model_type <- instance$model_type
  resp_col   <- if (model_type == "binomial") "proportion" else if (model_type == "beta") "mean_prob"
  y_label    <- if (model_type == "binomial") "Probability of success" else if (model_type == "beta")  "Phoneme goodness score"
  
  age_lower_bound <- max(min(crowe_mcleod$age_months, agerange[1]), 4)
  
  newdata <- tidyr::crossing(
    age_months       = seq(age_lower_bound, agerange[2], by = 1),
    expected_phoneme = instance$target_phonemes,#phonemes,
    speaker          = "fake" 
  )
  
  
  
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
  
  
  
  q_beta_join <- q_age %>%
    dplyr::select(expected_phoneme, age_months, q_age = prob_x_eq_1_hat)
  
  
  
  crow_grouped <- crowe_mcleod %>% 
    dplyr::rename(q_age = prob_x_eq_1_hat)%>% 
    dplyr::group_by(type)
  
  # group_keys() returns a tibble containing one row per group, showing the unique combinations of grouping variables
  # used in group_by (like index in python)
  # pull extract the 'type' in each index.
  crow_tables <- crow_grouped %>% 
    dplyr::group_split() %>%
    purrr::set_names(crow_grouped %>% dplyr::group_keys() %>% dplyr::pull(type))
  
  # message("crow_tables:")
  # print(crow_tables)
  
  crow_tables[["prediction"]] <- q_beta_join
  
  
  preds_with_q <- predicted %>%
    dplyr::inner_join(q_beta_join, by = c("expected_phoneme","age_months")) %>%
    dplyr::mutate(p_quant = pmax(0, pmin(1, 1 - q_age)))  # convert tail prob to CDF prob
  
  
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
  
  
  return(list(xq_all=xq_all,crow_tables=crow_tables, age_plot_crow4=age_plot_crow4))

}
################################################################################

get_instances_and_preprocessed <-function(instance_to_fit_path_mod){
  
  list_of_instances_modread <- read_instances_specifications_lib$read_instances_specifications_modified(instance_to_fit_path_mod)
  
  unique_keys <- read_instances_specifications_lib$find_unique_instances_keys(list_of_instances_modread)
  unique_keys1 <- unique_keys$unique_keys1 # this is just raw_data_type, model_type, phoneme_grouping_type
  unique_phoneme_grouping_type<- unique_keys$unique_phoneme_grouping_type
  
  
  #read the preprocessed files if they already exist this is the same function in run_pipeline:
  preprocessed_cache_read <- unique_keys1 |>
    purrr::map(\(key1) {
      preprocessing_lib$read_preprocessed_from_key(key1)
    }) |>
    purrr::set_names(
      purrr::map_chr(unique_keys1, read_instances_specifications_lib$make_key_string)
    )
  
  return(list(list_of_instances_modread=list_of_instances_modread, preprocessed_cache_read=preprocessed_cache_read))
}

################################################################################
get_plotcuts_instance<-function(instancia_pruebaA,
                                instancia_pruebaB,
                                preprocessed_A_cache_read,
                                preprocessed_B_cache_read){
  
  
  assert_that(instancia_pruebaA$subset_data==instancia_pruebaB$subset_data)
  
  
  prepA <- preprocessing_lib$get_preprocessed_for_instance(instancia_pruebaA, preprocessed_A_cache_read)
  df_finalA <- prepA$df_final
  phoneme_numscore_modeA <-prepA$phoneme_numscore_mode
  
  prepB <- preprocessing_lib$get_preprocessed_for_instance(instancia_pruebaB, preprocessed_B_cache_read)
  df_finalB <- prepB$df_final
  phoneme_numscore_modeB <-prepB$phoneme_numscore_mode
  
  
  
  agerangeA <- range(df_finalA$age_months)
  agerangeB <- range(df_finalB$age_months)
  agerange <- c(agerangeA[1],agerangeB[2])
  
  
  fitted_modelA <- readRDS(paste0(instancia_pruebaA$fitted_model_file_path,".rds"))
  fitted_modelB <- readRDS(paste0(instancia_pruebaB$fitted_model_file_path,".rds"))
  
  success_frac <- 1.0            # f in [0,1]
  target_rule <- "floor"  # how to turn f*n into k
  
  
  extract_q_result <- extract_q_aaps_binomial(
    instancia_pruebaA,
    fitted_modelA,
    phoneme_numscore_modeA,
    agerange,
    success_frac,
    target_rule
  )
  
  
  q_exact_k_ppc<-extract_q_result$q_exact_k_ppc
  plot_post_mean<- extract_q_result$plot_post_mean
  crow_joined <- read_instances_specifications_lib$read_crow_mcleod()
  list_extracted_x <- extract_x_q_pllr_beta(
    instancia_pruebaB,
    fitted_modelB,
    phoneme_numscore_modeB,
    agerange,
    q_exact_k_ppc,
    crow_joined
  )
  
  
  crow_tables <- list_extracted_x$crow_tables
  xq_all <- list_extracted_x$xq_all
  age_plot_crow4 <-list_extracted_x$age_plot_crow4
  
  return(age_plot_crow4)
  
}
################################################################################

# Assumptions:
# - Instance file A contains models fitted using AAPS data with a binomial likelihood.
# - Instance file B contains models fitted using PLLR with a beta likelihood.
#
# Implicit assumption: the i-th row in instance file A corresponds to an instance
# using the same subset_data as the i-th row in instance file B.
#
# Ideally, instances should not be matched by position in the corresponding list. Instead, they should be
# matched explicitly using their subset_data attribute and verifying consistency.
#
# Future work:
# - Add explicit consistency checks between files A and B.
# - Process and match instances based on subset_data rather than row order.

## Instance A specifications (AAPS instances)
instance_to_fitA_path_mod <- "./Modeling_Pipeline/instance_specification/cutting_points_instances/instance_to_fit1_A.csv"
ListA <- get_instances_and_preprocessed(instance_to_fitA_path_mod)
list_of_instancesA_modread <-  ListA$list_of_instances_modread
preprocessed_A_cache_read  <-  ListA$preprocessed_cache_read


## Instance B specifications (pllr instances)
instance_to_fitB_path_mod <-  "./Modeling_Pipeline/instance_specification/cutting_points_instances/instance_to_fit1_B.csv"
ListB <- get_instances_and_preprocessed(instance_to_fitB_path_mod)
list_of_instancesB_modread <-  ListB$list_of_instances_modread
preprocessed_B_cache_read  <-  ListB$preprocessed_cache_read

# 8 instances in total
k<- 1
instancia_pruebaA <- list_of_instancesA_modread[[k]]
instancia_pruebaB <- list_of_instancesB_modread[[k]]

age_plot_crow <- get_plotcuts_instance(instancia_pruebaA,
                                instancia_pruebaB,
                                preprocessed_A_cache_read,
                                preprocessed_B_cache_read)

age_plot_crow