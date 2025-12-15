library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(yaml)

# If using YAML:
spec <- yaml::read_yaml("./Modeling_Pipeline/pipeline/instances.yml")

flatten_instance_spec <- function(spec) {
  # Expect spec$instances[[...]]$variables[[...]]$models[[...]]
  tibble(inst = spec$instances) %>%
    unnest_wider(inst) %>%                                   # raw_data_type, phoneme_grouping_type, variables
    unnest_longer(variables) %>%
    unnest_wider(variables) %>%                              # model_type, models
    unnest_longer(models) %>%
    unnest_wider(models) %>%                                 # name, priors, subsets
    mutate(
      priors  = map(priors, \(x) if (is.null(x)) list(NA_character_) else as.list(x)),
      subsets = map(subsets, \(x) if (is.null(x)) list(NA_character_) else as.list(x))
    ) %>%
    mutate(grid = map2(priors, subsets, \(p, s) tidyr::expand_grid(prior = unlist(p), subset_data = unlist(s)))) %>%
    select(raw_data_type, phoneme_grouping_type, model_type, model = name, grid) %>%
    unnest(grid) %>%
    # Reorder/rename to match your current CSV columns
    select(raw_data_type, model_type, model, prior, phoneme_grouping_type, subset_data) %>%
    arrange(raw_data_type, phoneme_grouping_type, model_type, model, prior, subset_data)
}

# Example:
instances_tbl <- flatten_instance_spec(spec)
readr::write_csv(instances_tbl, "instances_generated.csv")
