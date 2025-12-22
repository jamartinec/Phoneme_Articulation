library(dplyr)
library(tidyr)
library(tidybayes)
library(ggplot2)
library(brms)
library(splines)
library(metR)
# AAPS ~ Age * Goodness 

Paths <- modules::use("./Modeling_Pipeline/pipeline/config/file_paths.R")
model_definitions_lib <- modules::use("./Modeling_Pipeline/models/models_definition/models_definition.R")
conventions <- modules::use("./Modeling_Pipeline/pipeline/config/conventions.R")
brms_help <- modules::use("./Modeling_Pipeline/scripts/train/tristan_brm_helper.R")




geometric_mean <- function(x) {
  exp(mean(log(x[x > 0]), na.rm = TRUE))  # Avoid log(0) or negative probs
}
# read aaps data:

df_aaps_raw <- read.csv(conventions$rawdata_paths$aaps)
df_aaps <- df_aaps_raw%>%
  dplyr::rename(speaker=SubjectNum, age_months=Age,expected_phoneme=Phoneme)%>%
  dplyr::mutate(speaker_short = substr(speaker, 1, 6)) %>%
  group_by(speaker, expected_phoneme) %>%
  summarize(
    sum_score = sum(Score),
    num_score = sum(Score>=0),
    age_months = first(age_months),
    .groups = "drop"
  )

# SubjectNum Age Sex StandardScore        Word Phoneme Score
# 1   F_0300_1973  36   F           103       horse      HH     1
# 2   F_0300_1973  36   F           103        baby       B     1
# 3   F_0300_1973  36   F           103     bathtub      AE     1
# 4   F_0300_1973  36   F           103     bathtub       B     1
# 5   F_0300_1973  36   F           103         pig       P     1
# 6   F_0300_1973  36   F           103         cup       P     1
# 7   F_0300_1973  36   F           103        nine       N     1
# 8   F_0300_1973  36   F           103       train       N     1
# 9   F_0300_1973  36   F           103      monkey       M     1
# 10  F_0300_1973  36   F           103      monkey      AH     1
# 11  F_0300_1973  36   F           103        comb       K     1
# 12  F_0300_1973  36   F           103        comb      OW     1
# 13  F_0300_1973  36   F           103        comb       M     1

# read pllr data:
df_pllr_raw <-read.csv(conventions$rawdata_paths$pllr)
df_pllr <- df_pllr_raw%>%
  dplyr::mutate(speaker_short = substr(speaker, 1, 6))%>%
  dplyr::filter(expected_phoneme == phoneme)%>%
  dplyr::group_by(speaker_short, expected_phoneme) %>%
  dplyr::summarize(
    speaker = first(speaker),
    mean_prob = geometric_mean(prob),
    age_months = first(age_months),
    age_months_shifted = first(age_months - 30),
    .groups = "drop"
  )

###############################################################################
# Keep in mind: (there is only one speaker/age combination)
# df_aaps %>%
#   +   group_by(speaker_short) %>%
#   +   summarise(n_ages = n_distinct(age_months), .groups = "drop") %>%
#   +   count(n_ages)
# # A tibble: 1 × 2
# n_ages     n
# <int> <int>
#   1      1   158
# > 
#   > df_pllr %>%
#   +   group_by(speaker_short) %>%
#   +   summarise(n_ages = n_distinct(age_months), .groups = "drop") %>%
#   +   count(n_ages)
# # A tibble: 1 × 2
# n_ages     n
# <int> <int>
#   1      1   156
################################################################################
# Deberiamos hacer una inner union x speaker and age

df_joined <- df_pllr %>%
  inner_join(
    df_aaps,
    by = c("speaker", "age_months", "expected_phoneme")
  )

# > dim(df_joined)
# [1] 4271    8
# > dim(df_pllr)
# [1] 5139    6
# > dim(df_aaps)
# [1] 18396     5
# > print(df_joined,width=Inf)
# # A tibble: 4,271 × 8
# speaker_short expected_phoneme speaker     mean_prob age_months age_months_shifted sum_score num_score
# <chr>         <chr>            <chr>           <dbl>      <int>              <dbl>     <int>     <int>
#   1 F_0207        AA               F_0207_2963   0.0518          31                  1         1         1
# 2 F_0207        AE               F_0207_2963   0.0702          31                  1         1         1
# 3 F_0207        AH               F_0207_2963   0.0422          31                  1         2         2
# 4 F_0207        AO               F_0207_2963   0.00391         31                  1         1         1
# 5 F_0207        AY               F_0207_2963   0.00641         31                  1         1         1
# 6 F_0207        B                F_0207_2963   0.0245          31                  1         2         2
# 7 F_0207        CH               F_0207_2963   0.00914         31                  1         0         2
# 8 F_0207        D                F_0207_2963   0.114           31                  1         2         2
# 9 F_0207        EH               F_0207_5374   0.00714         31                  1         1         1
# 10 F_0207        EY               F_0207_2963   0.0749          31                  1         1         1
# # ℹ 4,261 more rows
# # ℹ Use `print(n = ...)` to see more rows

df_joined_R <- df_joined%>%filter(expected_phoneme=="R")%>%
  mutate(
    mean_prob_clip  = pmin(pmax(mean_prob, 1e-6), 1 - 1e-6),
    logit_mean_prob = qlogis(mean_prob_clip)
  )

age_center <- mean(df_joined_R$age_months)
age_scale  <- sd(df_joined_R$age_months)

pgs_center <- mean(df_joined_R$logit_mean_prob)
pgs_scale  <- sd(df_joined_R$logit_mean_prob)

df_model_R <- df_joined_R %>%
  mutate(
    age_months_z = (age_months - age_center) / age_scale,
    logit_mean_prob_z = (logit_mean_prob - pgs_center) / pgs_scale,
    speaker = factor(speaker)
  )

priors_main <- c(
  prior(normal(0, 1.5), class = "Intercept"),     # prob baseline ~ no extrema
  prior(normal(0, 1.0), class = "b"),             # coeficientes (incluye ns() y logit_mean_prob)
  prior(student_t(3, 0, 1.0), class = "sd")       # sd de RE: speaker y phoneme
)

m_main <- bf(
  sum_score | trials(num_score) ~
    ns(age_months_z, df = 3) +
    logit_mean_prob_z +
    (1 | speaker),
  family = binomial(link = "logit")
)

# data = df_model,
# prior = priors_main,
# chains = 4, cores = 4, iter = 2000



m_age_only <- update(m_main, . ~ . - logit_mean_prob_z)


fitted_model_file_path <- file.path(Paths$Pipeline_fitted_models_dir,"aaps_f_pgs_age","m_main")

brm_args <- wisclabmisc::brms_args_create()
args <- brm_args(formula = m_main,
                 data = df_model_R,
                 prior = priors_main,
                 file = fitted_model_file_path,
                 # The model is always refitted, even if an object with the same name already exists.
                 file_refit = "always", 
                 seed = 20250625,
                 chains = 4,
                 iter  = 4000,
                 #iter  = 100,
                 cores = 4#,
                 #...
)
args$backend <- "cmdstanr"
model <- do.call(brm, args)
#Add validation criteria (e.g., LOO and WAIC) and save the fitted model
cat("  → Adding validation criteria (loo, waic)...\n")
model <- brms_help$add_validation_criterion(
  model,
  val_list=c("loo","waic"),
  use_reloo = FALSE)
saveRDS(model, file = paste0(fitted_model_file_path,".rds"))

# model <-readRDS(paste0(fitted_model_file_path,".rds"))

fitted_model_file_path2 <- file.path(Paths$Pipeline_fitted_models_dir,"aaps_f_pgs_age","m_age_only")

brm_args <- wisclabmisc::brms_args_create()
args2 <- brm_args(formula = m_age_only,
                 data = df_model_R,
                 prior = priors_main,
                 file = fitted_model_file_path2,
                 # The model is always refitted, even if an object with the same name already exists.
                 file_refit = "always", 
                 seed = 20250625,
                 chains = 4,
                 iter  = 4000,
                 #iter  = 100,
                 cores = 4#,
                 #...
)
args$backend <- "cmdstanr"
model2 <- do.call(brm, args2)
# Add validation criteria (e.g., LOO and WAIC) and save the fitted model
cat("  → Adding validation criteria (loo, waic)...\n")
model2 <- brms_help$add_validation_criterion(
  model2,
  val_list=c("loo","waic"),
  use_reloo = FALSE)
saveRDS(model2, file = paste0(fitted_model_file_path,".rds"))
model2 <- readRDS(paste0(fitted_model_file_path2,".rds"))

loo(model, model2)

# # 
# Output of model 'model':
#   
#   Computed from 8000 by 138 log-likelihood matrix.
# 
# Estimate   SE
# elpd_loo    -57.9  6.1
# p_loo         9.7  1.3
# looic       115.9 12.1
# ------
#   MCSE of elpd_loo is 0.1.
# MCSE and ESS estimates assume MCMC draws (r_eff in [0.5, 1.2]).
# 
# All Pareto k estimates are good (k < 0.7).
# See help('pareto-k-diagnostic') for details.
# 
# Output of model 'model2':
#   
#   Computed from 8000 by 138 log-likelihood matrix.
# 
# Estimate   SE
# elpd_loo    -68.0  5.5
# p_loo         9.4  1.0
# looic       136.1 10.9
# ------
#   MCSE of elpd_loo is 0.1.
# MCSE and ESS estimates assume MCMC draws (r_eff in [0.3, 1.0]).
# 
# All Pareto k estimates are good (k < 0.7).
# See help('pareto-k-diagnostic') for details.
# 
# Model comparisons:
#   elpd_diff se_diff
# model    0.0       0.0  
# model2 -10.1       4.1 
# Including the Phoneme Goodness Score (PGS) substantially improves out-of-sample 
#prediction of clinical phoneme production success beyond age alone.

# Llamar las funciones para producir las graficas

age_grid_months <- seq(
  from = min(df_model_R$age_months),
  to   = max(df_model_R$age_months),
  by   = 1   # monthly resolution; use 2 if you want lighter plots
)

pgs_z_seq <- seq(
  from = min(df_model_R$logit_mean_prob_z),
  to   = max(df_model_R$logit_mean_prob_z),
  length.out = 200
)

newdata_surface <- tidyr::crossing(
  age_months = seq(min(df_model_R$age_months), max(df_model_R$age_months), by = 1),
  logit_mean_prob_z = seq(min(df_model_R$logit_mean_prob_z), max(df_model_R$logit_mean_prob_z), length.out = 80),
  num_score = 1,
  speaker = "new_child"
) %>%
  mutate(
    age_months_z = (age_months - age_center) / age_scale
  )


pred_surface <- newdata_surface %>%
  tidybayes::add_epred_draws(
    model,
    re_formula = NULL,
    allow_new_levels = TRUE,
    sample_new_levels = "gaussian"
  ) %>%
  mutate(
    # un-standardize logit(PGS)
    logit_mean_prob = logit_mean_prob_z * pgs_scale + pgs_center,
    # convert back to probability-scale PGS
    PGS = plogis(logit_mean_prob)
    )
  


pred_R_filepath <- file.path(Paths$Pipeline_fitted_models_dir,"aaps_f_pgs_age","pred_R")
saveRDS(pred_surface, file = paste0(pred_R_filepath,".rds") )

surface_data <- pred_surface %>%
  group_by(age_months, PGS) %>%
  summarise(
    p = median(.epred),
    .groups = "drop"
  )


df_points_R <- df_model_R %>%
  mutate(
    PGS = mean_prob,
    prop = sum_score / num_score
  )


ggplot() +
  geom_raster(data = surface_data, aes(age_months, PGS, fill = p), interpolate = TRUE) +
  geom_point(
    data = df_points_R,
    aes(age_months, PGS, size = factor(prop)),
    color = "black",
    alpha = 0.7,
    inherit.aes = FALSE
  ) +
  scale_size_manual(
    values = c("0" = 1.2, "0.25" = 2.4, "0.5" = 3.6, "0.75" = 4.8, "1" = 6),
    name = "Observed success"
  ) +
  scale_fill_viridis_c(name = "P(success)")+
  labs(
    x = "Age (months)",
    y = "Phoneme Goodness Score (PGS)",
    title = "Phoneme R: probability of correct production"
  ) +
  theme_minimal(base_size = 14)
#########################################

surface_data <- pred_surface %>%
  group_by(age_months, PGS) %>%
  tidybayes::median_qi(.epred, .width = 0.8) %>%
  ungroup() %>%
  rename(
    p50 = .epred,
    p10 = .lower,
    p90 = .upper
  )

df_points_facets <- df_points_R %>%
  transmute(
    age_months,
    PGS,
    prop = sum_score / num_score
  ) %>%
  tidyr::crossing(
    surface = factor(
      c("Lower (p10)", "Median (p50)", "Upper (p90)"),
      levels = c("Lower (p10)", "Median (p50)", "Upper (p90)")
    )
  )



surface_long <- surface_data %>%
  select(age_months, PGS, p10, p50, p90) %>%
  pivot_longer(
    cols = c(p10, p50, p90),
    names_to = "surface",
    values_to = "p"
  ) %>%
  mutate(
    surface = factor(
      surface,
      levels = c("p10", "p50", "p90"),
      labels = c("Lower (p10)", "Median (p50)", "Upper (p90)")
    )
  )

levels_p <- seq(0.0, 1.0, by = 0.1)




ggplot(surface_long, aes(x = age_months, y = PGS, z = p)) +
  geom_contour(breaks = levels_p, linewidth = 0.8) +
  metR::geom_text_contour(
    breaks = levels_p,
    aes(label = sprintf("%.2f", after_stat(level))),  # label text
    stroke = 0.2,                                     # outline for readability
    size = 3,
    check_overlap = TRUE
  ) +
  facet_wrap(~ surface, nrow = 1) +
  labs(
    x = "Age (months)",
    y = "PGS",
    title = "Contours of P(success) surfaces (R)",
    subtitle = "Each curve is labeled by its probability level"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )



ggplot(surface_long, aes(x = age_months, y = PGS, z = p)) +
  geom_contour(breaks = levels_p, linewidth = 0.8) +
  metR::geom_text_contour(
    breaks = levels_p,
    aes(label = sprintf("%.2f", after_stat(level))),
    stroke = 0.2,
    size = 3,
    check_overlap = TRUE
  ) +
  geom_point(
    data = df_points_facets,
    aes(x = age_months, y = PGS, size = prop),
    inherit.aes = FALSE,
    color = "black",
    alpha = 0.6
  ) +
  scale_size_continuous(range = c(1, 5), name = "Observed success") +
  facet_wrap(~ surface, nrow = 1) +
  labs(
    x = "Age (months)",
    y = "PGS",
    title = "Contours of P(success) surfaces with observed points (R)",
    subtitle = "Labeled contour levels; points show observed phoneme outcomes"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )


df_points_facets <- df_model_R %>%
  transmute(
    age_months,
    PGS = mean_prob,
    y_obs = factor(sum_score > 0, levels = c(FALSE, TRUE),
                   labels = c("0", "1"))
  ) %>%
  tidyr::crossing(surface = levels(surface_long$surface))
ggplot(surface_long, aes(x = age_months, y = PGS, z = p)) +
  geom_contour(breaks = levels_p, linewidth = 0.8) +
  metR::geom_text_contour(
    breaks = levels_p,
    aes(label = sprintf("%.2f", after_stat(level))),
    stroke = 0.2,
    size = 3,
    check_overlap = TRUE
  ) +
  geom_point(
    data = df_points_facets,
    aes(x = age_months, y = PGS, size = y_obs),
    inherit.aes = FALSE,
    color = "black",
    alpha = 0.65
  ) +
  scale_size_manual(
    values = c("0" = 1.5, "1" = 4.0),
    name = "Observed success"
  ) +
  facet_wrap(~ surface, nrow = 1) +
  labs(
    x = "Age (months)",
    y = "PGS",
    title = "Contours of P(success) surfaces with observed points (R)",
    subtitle = "Labeled contour levels; points show observed phoneme outcomes"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

################################################################################
################################################################################
################################################################################
## Leets repeat the code for another phoneme S

df_joined_S <- df_joined%>%filter(expected_phoneme=="S")%>%
  mutate(
    mean_prob_clip  = pmin(pmax(mean_prob, 1e-6), 1 - 1e-6),
    logit_mean_prob = qlogis(mean_prob_clip)
  )

age_center <- mean(df_joined_S$age_months)
age_scale  <- sd(df_joined_S$age_months)

pgs_center <- mean(df_joined_S$logit_mean_prob)
pgs_scale  <- sd(df_joined_S$logit_mean_prob)

df_model_S <- df_joined_S %>%
  mutate(
    age_months_z = (age_months - age_center) / age_scale,
    logit_mean_prob_z = (logit_mean_prob - pgs_center) / pgs_scale,
    speaker = factor(speaker)
  )


fitted_model_file_path_S <- file.path(Paths$Pipeline_fitted_models_dir,"aaps_f_pgs_age","m_main_S")

brm_args <- wisclabmisc::brms_args_create()
args <- brm_args(formula = m_main,
                 data = df_model_S,
                 prior = priors_main,
                 file = fitted_model_file_path_S,
                 # The model is always refitted, even if an object with the same name already exists.
                 file_refit = "always", 
                 seed = 20250625,
                 chains = 4,
                 iter  = 4000,
                 #iter  = 100,
                 cores = 4#,
                 #...
)
args$backend <- "cmdstanr"
model_S <- do.call(brm, args)
#Add validation criteria (e.g., LOO and WAIC) and save the fitted model
cat("  → Adding validation criteria (loo, waic)...\n")
model_S <- brms_help$add_validation_criterion(
  model_S,
  val_list=c("loo","waic"),
  use_reloo = FALSE)
saveRDS(model_S, file = paste0(fitted_model_file_path_S,".rds"))
model_S<- readRDS(paste0(fitted_model_file_path_S,".rds"))

age_grid_months <- seq(
  from = min(df_model_S$age_months),
  to   = max(df_model_S$age_months),
  by   = 1   # monthly resolution; use 2 if you want lighter plots
)

pgs_z_seq <- seq(
  from = min(df_model_S$logit_mean_prob_z),
  to   = max(df_model_S$logit_mean_prob_z),
  length.out = 200
)

newdata_surface <- tidyr::crossing(
  age_months = seq(min(df_model_S$age_months), max(df_model_S$age_months), by = 1),
  logit_mean_prob_z = seq(min(df_model_S$logit_mean_prob_z), max(df_model_S$logit_mean_prob_z), length.out = 80),
  num_score = 1,
  speaker = "new_child"
) %>%
  mutate(
    age_months_z = (age_months - age_center) / age_scale
  )


pred_surface_S <- newdata_surface %>%
  tidybayes::add_epred_draws(
    model_S,
    re_formula = NULL,
    allow_new_levels = TRUE,
    sample_new_levels = "gaussian"
  ) %>%
  mutate(
    # un-standardize logit(PGS)
    logit_mean_prob = logit_mean_prob_z * pgs_scale + pgs_center,
    # convert back to probability-scale PGS
    PGS = plogis(logit_mean_prob)
  )



pred_S_filepath <- file.path(Paths$Pipeline_fitted_models_dir,"aaps_f_pgs_age","pred_S")
saveRDS(pred_surface_S, file = paste0(pred_S_filepath,".rds") )

surface_data <- pred_surface_S %>%
  group_by(age_months, PGS) %>%
  summarise(
    p = median(.epred),
    .groups = "drop"
  )


df_points_S <- df_model_S %>%
  mutate(
    PGS = mean_prob,
    prop = sum_score / num_score
  )


ggplot() +
  geom_raster(data = surface_data, aes(age_months, PGS, fill = p), interpolate = TRUE) +
  geom_point(
    data = df_points_S,
    aes(age_months, PGS, size = factor(prop)),
    color = "black",
    alpha = 0.7,
    inherit.aes = FALSE
  ) +
  scale_size_manual(
    values = c("0" = 1.2, "0.25" = 2.4, "0.5" = 3.6, "0.75" = 4.8, "1" = 6),
    name = "Observed success"
  ) +
  scale_fill_viridis_c(name = "P(success)")+
  labs(
    x = "Age (months)",
    y = "Phoneme Goodness Score (PGS)",
    title = "Phoneme S: probability of correct production"
  ) +
  theme_minimal(base_size = 14)
#########################################

surface_data <- pred_surface_S %>%
  group_by(age_months, PGS) %>%
  tidybayes::median_qi(.epred, .width = 0.8) %>%
  ungroup() %>%
  rename(
    p50 = .epred,
    p10 = .lower,
    p90 = .upper
  )

df_points_facets_S <- df_points_S %>%
  transmute(
    age_months,
    PGS,
    prop = sum_score / num_score
  ) %>%
  tidyr::crossing(
    surface = factor(
      c("Lower (p10)", "Median (p50)", "Upper (p90)"),
      levels = c("Lower (p10)", "Median (p50)", "Upper (p90)")
    )
  )



surface_long_S <- surface_data %>%
  select(age_months, PGS, p10, p50, p90) %>%
  pivot_longer(
    cols = c(p10, p50, p90),
    names_to = "surface",
    values_to = "p"
  ) %>%
  mutate(
    surface = factor(
      surface,
      levels = c("p10", "p50", "p90"),
      labels = c("Lower (p10)", "Median (p50)", "Upper (p90)")
    )
  )

levels_p <- seq(0.0, 1.0, by = 0.1)




ggplot(surface_long_S, aes(x = age_months, y = PGS, z = p)) +
  geom_contour(breaks = levels_p, linewidth = 0.8) +
  metR::geom_text_contour(
    breaks = levels_p,
    aes(label = sprintf("%.2f", after_stat(level))),  # label text
    stroke = 0.2,                                     # outline for readability
    size = 3,
    check_overlap = TRUE
  ) +
  facet_wrap(~ surface, nrow = 1) +
  labs(
    x = "Age (months)",
    y = "PGS",
    title = "Contours of P(success) surfaces (S)",
    subtitle = "Each curve is labeled by its probability level"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )






df_points_facets_S <- df_model_S %>%
  transmute(
    age_months,
    PGS = mean_prob,
    prop = sum_score/num_score
  ) %>%
  tidyr::crossing(surface = levels(surface_long_S$surface))

df_points_facets_S <- df_points_facets_S %>%
  mutate(
    prop3 = factor(round(prop, 2),
                   levels = c(0, 0.5, 1),
                   labels = c("0", "0.5", "1"))
  )



ggplot(surface_long_S, aes(x = age_months, y = PGS, z = p)) +
  geom_contour(breaks = levels_p, linewidth = 0.8) +
  metR::geom_text_contour(
    breaks = levels_p,
    aes(label = sprintf("%.2f", after_stat(level))),
    stroke = 0.2,
    size = 3,
    check_overlap = TRUE
  ) +
  geom_point(
    data = df_points_facets_S,
    aes(x = age_months, y = PGS, size = prop3),
    inherit.aes = FALSE,
    color = "black",
    alpha = 0.6
  ) +
  scale_size_manual(
    values = c("0" = 1.0, "0.5" = 2.5, "1" = 4.0),
    name = "Observed success"
  ) +
  facet_wrap(~ surface, nrow = 1) +
  labs(
    x = "Age (months)",
    y = "PGS",
    title = "Contours of P(success) surfaces with observed points (S)",
    subtitle = "Labeled contour levels, points show observed phoneme outcomes"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

### new plot####### 



target_p <- 0.9
ndraws_target <- 1000   # adjust (400-800 usually enough)

age_grid_months <- seq(min(df_model_S$age_months), max(df_model_S$age_months), by = 1)

# More PGS resolution helps contour extraction
pgs_z_seq <- seq(min(df_model_S$logit_mean_prob_z),
                 max(df_model_S$logit_mean_prob_z),
                 length.out = 200)

newdata_surface <- tidyr::crossing(
  age_months = age_grid_months,
  logit_mean_prob_z = pgs_z_seq,
  num_score = 1,
  speaker = "new_child"
) %>%
  mutate(
    age_months_z = (age_months - age_center) / age_scale,
    logit_mean_prob = logit_mean_prob_z * pgs_scale + pgs_center,
    PGS = plogis(logit_mean_prob)
  )

pred_surface_S <- newdata_surface %>%
  tidybayes::add_epred_draws(
    model_S,
    re_formula = NULL,
    allow_new_levels = TRUE,
    sample_new_levels = "gaussian",
    ndraws = ndraws_target
  )


extract_contour_pgs <- function(df, target = 0.7) {
  df <- df[order(df$PGS), ]
  p <- df$.epred
  y <- df$PGS
  d <- p - target
  
  # if any exact hits (rare), take the first
  hit <- which(d == 0)
  if (length(hit) > 0) return(y[hit[1]])
  
  # find sign change intervals (crossing)
  idx <- which(d[-length(d)] * d[-1] <= 0)
  
  if (length(idx) > 0) {
    i <- idx[1]
    # linear interpolation between i and i+1
    y0 <- y[i]; y1 <- y[i+1]
    d0 <- d[i]; d1 <- d[i+1]
    if (d1 == d0) return(y0)
    t <- (0 - d0) / (d1 - d0)
    return(y0 + t * (y1 - y0))
  }
  
  # no crossing inside grid -> take closest point
  y[which.min(abs(d))]
}

contour_draws <- pred_surface_S %>%
  dplyr::group_by(.draw, age_months) %>%
  dplyr::summarise(
    PGS_star = extract_contour_pgs(dplyr::cur_data(), target = target_p),
    .groups = "drop"
  )

contour_summ <- contour_draws %>%
  dplyr::group_by(age_months) %>%
  dplyr::summarise(
    PGS_med = stats::median(PGS_star, na.rm = TRUE),
    PGS_p10 = stats::quantile(PGS_star, 0.15, na.rm = TRUE),
    PGS_p90 = stats::quantile(PGS_star, 0.85, na.rm = TRUE),
    .groups = "drop"
  )

df_points_S <- df_model_S %>%
  mutate(
    PGS = mean_prob,
    prop = sum_score / num_score,
    prop3 = factor(round(prop, 2),
                   levels = c(0, 0.5, 1),
                   labels = c("0", "0.5", "1"))
  )


ggplot() +
  geom_ribbon(
    data = contour_summ,
    aes(x = age_months, ymin = PGS_p10, ymax = PGS_p90),
    alpha = 0.25
  ) +
  geom_line(
    data = contour_summ,
    aes(x = age_months, y = PGS_med),
    linewidth = 1.1
  ) +
  geom_point(
    data = df_points_S,
    aes(x = age_months, y = PGS, size = prop3),
    color = "black",
    alpha = 0.6
  ) +
  scale_size_manual(
    values = c("0" = 1.0, "0.5" = 2.5, "1" = 4.0),
    name = "Observed success"
  ) +
  labs(
    x = "Age (months)",
    y = "PGS",
    title = paste0("PGS threshold curve for P(success) = ", target_p, " (phoneme S)"),
    subtitle = "Median contour (line) and centered 70% credible band"
  ) +
  theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank())

