Paths <- modules::use("./Modeling_Pipeline/pipeline/config/file_paths.R")

export(
  "rawdata_paths",
  "grouping_paths",
  "setdatafiles_paths"
)

# 1. Define the paths to the raw_data_type
# We assume a one-to-one mapping between raw_data_type and data files.
# If you wish to use a different variant (e.g., PLLR computed with the minimum frame),
# introduce a new naming convention (e.g., "pllr_min") and add it to the dictionary below.

rawdata_paths <- list(
  pllr =  file.path(Paths$Pipeline_rawdata_dir,"probabilities-max-frame_W.csv.gz"),
  aaps =  file.path(Paths$Pipeline_rawdata_dir, "AAPS Score Data (Long Version).csv")
)

# 2. Define the paths to the grouping files
grouping_paths <- list(
  grouping2 = file.path(Paths$Pipeline_phoneme_grouping_dir, "phoneme_grouping2.csv"),
  grouping1 = file.path(Paths$Pipeline_phoneme_grouping_dir, "phoneme_grouping1.csv")
)

# 3. Define the paths to the subset data files
setdatafiles_paths <- list(
  subset_data_grouping2 = file.path(Paths$Pipeline_set_data_files_dir, "subset_data_grouping2.csv"),
  subset_data_grouping1 = file.path(Paths$Pipeline_set_data_files_dir, "subset_data_grouping1.csv")
)
