
Modeling_Pipeline_dir <- "./Modeling_Pipeline"
Pipeline_data_dir <- file.path(Modeling_Pipeline_dir,"data")
Pipeline_rawdata_dir <- file.path(Pipeline_data_dir,"raw")
Pipeline_preprocesseddata_dir <- file.path(Pipeline_data_dir, "preprocessed")
Pipeline_filtered_data_dir <-  file.path(Pipeline_preprocesseddata_dir,"filtered_data")

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


