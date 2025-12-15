import("dplyr")
import("brms")
import("tidyr")
import("ggplot2")
import("splines")
import("tidybayes")
Paths <- modules::use("./Modeling_Pipeline/pipeline/config/file_paths.R")

# move to utils
ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

ensure_parent_dir <- function(file_path) {
  ensure_dir(dirname(file_path))
}

export("visualize_age_standards_funct")
visualize_age_standards_funct <- function(model_type=c("binomial","beta"),
                                          phoneme_numscore_mode, #only used for binomial                                        
                                          agerange,
                                          instance,
                                          fitted_model
                                          ) {
  

  
  # we should use the same age_rank from df_final (preprocessing output)
  # we have included a speaker column for managine the random effect contribution
    
  model_type <- match.arg(model_type)
  resp_col   <- if (model_type == "binomial") "proportion" else if (model_type == "beta") "mean_prob"
  y_label    <- if (model_type == "binomial") "Probability of success" else if (model_type == "beta")  "Phoneme goodness score"
  
  
  newdata <- tidyr::crossing(
    age_months       = seq(agerange[1], agerange[2], by = 1), 
    expected_phoneme = instance$target_phonemes,
    speaker          = "fake" 
    
  )
  
  
  
  # Use the mode across participants for that particular phoneme.
  # binomial-only: attach mode num_score.
  # We can avoid this step and predict directly a proportion.
  if(model_type == "binomial"){
  
  newdata <- newdata %>%
    left_join(phoneme_numscore_mode, by = "expected_phoneme") %>%
    mutate(num_score = mode_num_score) %>%
    select(-mode_num_score)  
  }
  
  # The posterior predictive draws will be done using tidybayes
  # re_formula = NA sets the random effects values to 0 and make predictions
  # re_formula = NULL, allow_new_levels = TRE will resample random 
  # effect values from existing speakers as a way of simulating/averaging over 
  # speakers.
  predicted<- tidybayes::add_predicted_draws(
    fitted_model,
    newdata = newdata,
    re_formula = NULL,
    allow_new_levels = TRUE
  ) %>%
    mutate(
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
  
  print(predicted)
  
  
  
  # Load filtered data for plot overlay
  df_filtered <- readRDS(instance$filtered_file_path)
  
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
  
  
  # single plotting block
  age_plot <- ggplot(plot_data, aes(x = (age_months) / 12, y = q50, color = expected_phoneme)) +
    geom_line(linewidth = 1.1, color = "steelblue") +
    geom_ribbon(aes(ymin = q025, ymax = q975, fill = expected_phoneme), alpha = 0.2, color = NA) +
    geom_ribbon(aes(ymin = q25,  ymax = q75,  fill = expected_phoneme), alpha = 0.4, color = NA) +
    facet_wrap(~ expected_phoneme) +
    geom_point(
      data = df_points,
      aes(x = (age_months) / 12, y = y_obs, color = expected_phoneme),
      inherit.aes = FALSE, alpha = 0.5, size = 1.5
    ) +
    labs(x = "Age (years)", y = y_label) +
    theme_minimal(base_size = 14) +
    theme(
      axis.title   = element_text(size = 16, face = "bold"),
      axis.text    = element_text(size = 14),
      strip.text   = element_text(size = 14, face = "bold"),
      legend.title = element_blank(),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90"),
      plot.title = element_blank(),
      plot.subtitle = element_blank()
    )
  
  
  plot_name <- "age_plot.png"
  plot_file_path <- file.path(instance$plots_folder_path,plot_name)
  ensure_dir(instance$plots_folder_path)
  
  
  ggsave(plot_file_path, plot = age_plot, width = 8, height = 6, dpi = 300)
  
  print(age_plot)
  
}