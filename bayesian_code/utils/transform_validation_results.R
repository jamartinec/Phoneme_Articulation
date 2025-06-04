import("dplyr") 
import("stringr")
import("tidyr")
import("ggplot2")
import("utils")
import("purrr")
import("stats") 

library(tidyverse)

folder_path <- "./bayesian_code/model_validation/model_validation_results2.rds"
model_validation_results <- readRDS(folder_path)
rows <- list()
for (key in names(model_validation_results)){
  subdict <- model_validation_results[[key]]
  waic_table <- subdict["waic"]$waic$estimates
  elpd_waic <- waic_table["elpd_waic","Estimate"]
  p_waic <- waic_table["p_waic","Estimate"]
  waic <- waic_table["waic","Estimate"]
  # loo_table <- subdict["loo"]$psis_loo$estimates
  # elpd_loo_psis <- loo_table["elpd_loo","Estimate"]
  # p_loo_psis <- loo_table["p_loo","Estimate"]
  # looic_psis <- loo_table["looic","Estimate"]
  loo_table2 <- subdict["loo"]$loo$estimates
  elpd_loo <- loo_table2["elpd_loo","Estimate"]
  p_loo <- loo_table2["p_loo","Estimate"]
  looic <- loo_table2["looic","Estimate"]
  
  extracted <- list(
    model_opt = subdict["model_opt"],
    phoneme_group = subdict["phoneme_group_str"],
    elpd_waic = elpd_waic,
    p_waic = p_waic,
    waic = waic,
    # elpd_loo_psis = elpd_loo_psis,
    # p_loo_psis = p_loo_psis,
    # looic_psis = looic_psis,
    elpd_loo = elpd_loo,
    p_loo = p_loo,
    looic = looic
  )
  rows[[length(rows) + 1]] <- extracted
}
result_df <- do.call(rbind, lapply(rows, as.data.frame))
rownames(result_df) <- NULL
saveRDS(result_df, file = "./bayesian_code/model_validation/result_df_unstack.rds")
print(result_df)

# Pivot for elpd_waic
waic_pivot <- result_df %>%
  select(phoneme_group_str, model_opt, elpd_waic) %>%
  pivot_wider(names_from = model_opt, values_from = elpd_waic, names_prefix = "waic_")

# Pivot for elpd_loo
loo_pivot <- result_df %>%
  select(phoneme_group_str, model_opt, elpd_loo) %>%
  pivot_wider(names_from = model_opt, values_from = elpd_loo, names_prefix = "loo_")


pivot_combined <- left_join(waic_pivot, loo_pivot, by = "phoneme_group_str")

print(pivot_combined)

write.csv(pivot_combined, file = "./bayesian_code/model_validation/pivot_combined.csv", row.names = FALSE)
