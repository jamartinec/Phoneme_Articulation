# Phoneme Articulation

This repository provides a flexible and reproducible pipeline for **modeling phoneme-level intelligibility growth curves** using data from two sources: **acoustic models** and **clinical assessments**.

The primary goal of this project is to support **systematic Bayesian modeling of speech development**, with  emphasis on uncertainty quantification, and visualization of posterior behavior.


- For generating Growth Curves (full pipeline) see `./Modeling_Pipeline/pipeline/run_pipeline.R`
- For generating Growth curves with cutpoints as defined in the presentation see:
`./Modeling_Pipeline/scripts/cutting_points/find_cuts.R`
---

## Project Overview

Intelligibility development is modeled as a function of age using probabilistic growth curves.  
The data used in this pipeline may come from:

- **Acoustic model outputs** (e.g., phoneme likelihood or probability estimates such as PLLR)
- **Clinical assessment data** (e.g., AAPS scores)

These heterogeneous sources are unified into a common modeling framework.

---

### 1. Modeling Flexibility

The pipeline supports:

- Modeling **individual phonemes** or **groups of phonemes**
- Multiple phoneme grouping strategies (e.g., complexity-based groupings or phoneme-level modeling)
- Different model families (e.g., Binomial, Beta, Beta–Binomial) and outcome types (e.g., probabilities, proportions)
- Alternative **prior specifications** for the same model structure

Modeling instances are fully specified via configuration files.


The key concept is an “instance”, which represents a full model specification: the phonemes to include, the raw data source, the model type (i.e., response variable), and the specific model and prior to use.
You can see an example in
Modeling_Pipeline/instance_specification/cutting_points_instances/instance_to_fit1_B.csv,
where each row defines a single instance. For example, the first row is:

| raw_data_type | model_type | model           | prior           | phoneme_grouping_type | set_data_file         | subset_data     |
|---------------|------------|-----------------|-----------------|------------------------|-----------------------|-----------------|
| pllr          | beta       | model0_Version2 | prior0_Version2 | grouping2              | subset_data_grouping2 | dataPhoneme27   |



Here is what each component means:
- raw_data_type = pllr: the input data are csv files with PLLR values, located in Modeling_Pipeline/data/raw.
- model0_Version2: the model specification defined in Modeling_Pipeline/models/models_definition.
- prior0_Version2: the corresponding prior, also defined in the same directory.
- subset_data = dataPhoneme27: specifies which phonemes are modeled. This can be traced through the configuration files grouping2 and subset_data_grouping2.
- subset_data_grouping2 points to
Modeling_Pipeline/pipeline/config/set_data_files/subset_data_grouping2.csv, where you’ll see:

|subdata      | category  |  level|
|-------------|------------|----------------|
|dataPhoneme27| Consonants| Levelphoneme27|

- grouping2 points to Modeling_Pipeline/pipeline/config/phoneme_grouping/phoneme_grouping2.csv, which contains:

| Category    | Level           | Phoneme |
|-------------|------------------|---------|
| Consonants  | Levelphoneme27   | T       |


(so in this case, we are modeling only the phoneme T).

- For context, grouping1 and subset_data_grouping1 correspond to an earlier setup where we modeled groups of phonemes based on Kent’s phoneme complexity levels, used in a preliminary analysis a few months ago.


You can find examples that walk through the different pipeline steps in
Modeling_Pipeline/pipeline/run_pipeline.R.
The code used to compute the cutting points is located in
Modeling_Pipeline/scripts/cutting_points/find_cuts.R.


---


### 2. Outputs

The **primary outputs of the pipeline are visualizations**.

Most analyses focus on:

- Posterior median growth curves
- Credible intervals (e.g., 50%, 80%, 95%)
- Posterior predictive behavior across age
- Cut-point and threshold analyses derived from posterior samples


---
