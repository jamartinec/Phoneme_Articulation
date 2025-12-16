# Load file path
Paths <- modules::use("./bayesian_code/utils/file_paths.R")

save_all_credible_intervals_and_plot_forest <- function(
    categories, levels_list, data_dir, output_dir
) {
  # packages
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(ggplot2)
  library(posterior)
  library(writexl)
  library(purrr)
  
  #------------------------------------------------------------
  # Helper: extract parameter CIs
  #------------------------------------------------------------
  extract_parameter <- function(
    posterior_samples, prefix, reference, rename_fun, transform_fun, param_name
  ) {
    samples <- posterior_samples %>%
      select(starts_with(prefix)) %>%
      rename_with(~ reference, .cols = 1)
    
    cols <- colnames(samples)[-1]
    phoneme_names <- rename_fun(cols)
    
    samples <- samples %>%
      mutate(across(all_of(cols), ~ . + .data[[reference]])) %>%
      mutate(across(everything(), transform_fun))
    
    colnames(samples) <- c(reference, phoneme_names)
    
    samples %>%
      pivot_longer(everything(), names_to = "Phoneme", values_to = "Value") %>%
      group_by(Phoneme) %>%
      summarise(
        lower_95 = quantile(Value, 0.025),
        median   = median(Value),
        upper_95 = quantile(Value, 0.975),
        .groups  = "drop"
      ) %>%
      mutate(Parameter = param_name)
  }
  
  #------------------------------------------------------------
  # Helper: process one group
  #------------------------------------------------------------
  get_credible_intervals <- function(category, levels, data_dir) {
    load(file.path(data_dir, "phoneme_levels.RData"))
    phonemes <- unlist(phoneme_levels[[category]][levels])
    phonemes <- setdiff(phonemes, "ZH")
    reference <- min(phonemes)
    group_str <- paste(c(category, levels), collapse = "_")
    
    load(file.path(data_dir, "df_final.RData"))
    #load(file.path(data_dir,"model5", paste0("model_", group_str, ".RData")))
    model <- readRDS(file.path(data_dir,"model5", paste0("model_", group_str, ".rds")))
    posterior_samples <- as_draws_df(model)
    
    rename_logalpha <- function(x) str_remove(x, "b_logalpha_expected_phoneme")
    rename_eta      <- function(x) str_remove(x, "b_eta_expected_phoneme")
    
    alpha_ci <- extract_parameter(
      posterior_samples, "b_logalpha", reference,
      rename_logalpha, exp, "Discrimination"
    )
    
    beta_ci <- extract_parameter(
      posterior_samples, "b_eta", reference,
      rename_eta, function(x) -x, "Difficulty"
    )
    
    bind_rows(alpha_ci, beta_ci) %>%
      mutate(Group = group_str)
  }
  
  #------------------------------------------------------------
  # Build combined CI table
  #------------------------------------------------------------
  all_cis <- map2_dfr(categories, levels_list,
                      ~ get_credible_intervals(.x, .y, data_dir))
  
  # enforce the Group ordering you passed in
  desired_groups <- map2_chr(categories, levels_list,
                             ~ paste(c(.x, .y), collapse = "_"))
  all_cis <- all_cis %>%
    mutate(Group = factor(Group, levels = desired_groups)) %>%
    arrange(Group, median) %>%
    mutate(Phoneme = factor(Phoneme, levels = unique(Phoneme)))
  
  # write full table (including age effects) to Excel
  write_xlsx(all_cis,
             file.path(output_dir, "credible_intervals_all_groups.xlsx"))
  
  #------------------------------------------------------------
  # Prepare data for forest plot (drop any age effects)
  #------------------------------------------------------------
  plot_cis <- all_cis %>%
    filter(!str_detect(Phoneme, "age"))   # removes any Phoneme with "age"
  
  # custom legend labels: "Consonants: Level6" etc.
  legend_labels <- function(groups) {
    sapply(groups, function(g) {
      parts <- str_split(g, "_")[[1]]
      cat   <- parts[1]
      levs  <- parts[-1]
      paste0(cat, ": ", paste(levs, collapse = ", "))
    })
  }
  
  #------------------------------------------------------------
  # Forest plot, ordered by Group
  #------------------------------------------------------------
  forest_plot <- ggplot(plot_cis, aes(x = median, y = Phoneme, color = Group)) +
    geom_point(size = 2) +
    geom_errorbarh(aes(xmin = lower_95, xmax = upper_95), height = 0.2) +
    facet_wrap(~Parameter, scales = "free_x") +
    
    # apply custom legend labels
    scale_color_discrete(labels = legend_labels) +
    
    # bold, larger labels; no title
    labs(
      x     = "Estimate (95% CI)",
      y     = "Phoneme"
    ) +
    theme_minimal(base_size = 16) +
    theme(
      axis.title    = element_text(face = "bold", size = 18),
      axis.text     = element_text(size = 14),
      legend.title  = element_blank(),
      legend.text   = element_text(size = 14),
      strip.text    = element_text(face = "bold", size = 16)
    )
  
  ggsave(
    file.path(output_dir, "forest_plot_all_groups.png"),
    forest_plot, width = 10, height = 10, dpi = 300
  )
  print(forest_plot)
}

save_all_credible_intervals_and_plot_forest(
  categories = c("Consonants", 
    "Consonants", "Consonants", "Consonants", "Vowels", "Vowels", "Vowels"),
  levels_list = list(
    c("Level6"),
    c("Level5"),
    c("Level4"),
    c("Level3"),
    c("Level3"),
    c("Level1", "Level2"),
    c("Level4", "Level5")
  ),
  data_dir = Paths$processed_data_dir ,#"../data/processed_data",
  output_dir = file.path(Paths$output_bayesian_dir,"model5")#"./output/bayesian_model"
)