# Phoneme Articulation

This repository provides a flexible and reproducible pipeline for **modeling phoneme-level intelligibility growth curves** using data from two sources: **acoustic models** and **clinical assessments**.

The primary goal of this project is to support **systematic Bayesian modeling of speech development**, with  emphasis on uncertainty quantification, and visualization of posterior behavior.


For generating Growth Curves (full pipeline) see `./Modeling_Pipeline/pipeline/run_pipeline.R`
For generating Growth curves with cutpoints as defined in the presentation see:
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

---

### 2. Bayesian Modeling

All models are fitted using **Bayesian methods**, with inference performed via:

- **`brms`** (Stan-based Bayesian regression modeling)
- **`cmdstanr` / Stan** under the hood
- **`tidybayes`** for post-processing and summarization of posterior draws
---

### 3. Outputs

The **primary outputs of the pipeline are visualizations**.

Most analyses focus on:

- Posterior median growth curves
- Credible intervals (e.g., 50%, 80%, 95%)
- Posterior predictive behavior across age
- Cut-point and threshold analyses derived from posterior samples


---

### 4. Pipeline

The repository is organized as a modular pipeline that separates:

- **Data ingestion and preprocessing**
- **Model specification and fitting**
- **Posterior analysis and visualization**

Intermediate results (e.g., preprocessed data, fitted models) are cached to avoid unnecessary recomputation.
The pipeline is defined in `Modeling_Pipeline`.
