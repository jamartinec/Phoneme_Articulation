# Base data folders
base_data_dir <- "./data"
processed_data_dir <- file.path(base_data_dir,"processed_data")
input_data_dir <- file.path(base_data_dir,"input_data")
filtered_data_dir <-  file.path(processed_data_dir,"filtered_data")
filtered_data_phoneme_binomial_probability_singleWords_dir <- file.path(filtered_data_dir,"filtered_data_phoneme_binomial_probability_singleWords" )


bayesian_code_dir <- "./bayesian_code"
modeling_dir <- file.path(bayesian_code_dir, "modeling")
model_validation_dir <- file.path(bayesian_code_dir,"model_validation")
model_valresults_dir <- file.path(model_validation_dir,"validation_results")
model_valreports_dir <- file.path(model_valresults_dir,"validation_reports")
#output
base_output_dir<- "./output"
output_bayesian_dir <- file.path(base_output_dir,"bayesian_model")


Modeling_Pipeline_dir <- "./Modeling_Pipeline"
Pipeline_data_dir <- file.path(Modeling_Pipeline_dir,"data")
Pipeline_rawdata_dir <- file.path(Pipeline_data_dir,"raw")
Pipeline_preprocesseddata_dir <- file.path(Pipeline_data_dir, "preprocessed")

Pipeline_models_dir <- file.path(Modeling_Pipeline_dir, "models")
Pipeline_fitted_models_dir <- file.path(Pipeline_models_dir, "fitted_models")
Pipeline_models_definitions_dir <- file.path(Pipeline_models_dir,"models_definition")
Pipeline_instance_specification_dir <-file.path(Modeling_Pipeline_dir,"instance_specification")


# Fitting model errors
Pipeline_pipeline_dir <- file.path(Modeling_Pipeline_dir, "pipeline")
Pipeline_experiments_dir <- file.path(Pipeline_pipeline_dir, "experiments")
Pipeline_fittingmodelserrors_dir <- file.path(Pipeline_experiments_dir, "fitting_models_errors")
Pipeline_config_dir <- file.path(Pipeline_pipeline_dir, "config")

# Grouping files, set_data files
Pipeline_phoneme_grouping_dir <-file.path(Pipeline_config_dir,"phoneme_grouping")
Pipeline_set_data_files_dir <-file.path(Pipeline_config_dir,"set_data_files")


Pipeline_outputs_dir <- file.path(Modeling_Pipeline_dir,"outputs")
Pipeline_visuals_dir <- file.path(Modeling_Pipeline_dir,"visuals")
Pipeline_visualsplots_dir <- file.path(Pipeline_visuals_dir,"plots")


