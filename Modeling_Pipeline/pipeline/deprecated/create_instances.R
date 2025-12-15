# Run pipeline
library(tidyverse)
library(dplyr)
library(posterior)
library(yaml)
library(purrr)
library(glue)
Paths <- modules::use("./bayesian_code/utils/file_paths.R")

#An instance would be specified with ah hierarchical structure
# maybe using a dictionary/named list??

spec <- list(
  instances = list(
    list(
      raw_data_type = "pllr",
      phoneme_grouping_type = "grouping2",
      variables = list(
        list(
          model_type = "beta",
          models = list(
            list(name="model0_Version2", priors=c("prior0_Version2","prior1"),
                 subsets=c("dataPhoneme27","dataPhoneme28")),
            list(name="model5", priors=c("prior0_Version2","prior1"),
                 subsets=c("dataPhoneme27","dataPhoneme28"))
          )
        ),
        list(
          model_type = "binomial",
          models = list(
            list(name="model_binomial_dummytest", priors="prior_binomial",
                 subsets=c("dataPhoneme27","dataPhoneme28")),
            list(name="model_binomial_Probability", priors="prior_binomial",
                 subsets=c("dataPhoneme27","dataPhoneme28"))
          )
        )
      )
    ),
    list(
      raw_data_type = "aaps",
      phoneme_grouping_type = "grouping2",
      variables = list(
        list(
          model_type = "binomial",
          models = list(
            list(name="model_binomial_Probability", priors="prior_binomial",
                 subsets=c("dataPhoneme27","dataPhoneme28"))
          )
        )
      )
    )
  )
)
