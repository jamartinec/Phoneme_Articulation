## Raw Data Files (`data/raw`)

This folder contains **raw input data files** required to run the modeling pipeline.  
These files are **not tracked in the GitHub repository** (they are listed in `.gitignore`) as a matter of good data-management practice.

At the start of the workflow, **four raw data files must be manually placed in this directory**.  
They come from two different data sources, described below.

---

### 1. PLLR Data (Acoustic Model Output)

Files in this category are derived from the output of the **acoustic model** and contain phoneme-level likelihood or probability information.  
We refer to this data source as **PLLR** throughout the pipeline.

Two base files are used, depending on how the representative value of each phoneme frame was computed:

- `probabilities-max-frame.csv.gz`  
  Uses the **maximum value across frames** as the representative score.

- `probabilities_middle_frame.csv.gz`  
  Uses the **middle frame value** as the representative score.

In addition, filtered versions of the *max-frame* data are used depending on token type:

- `probabilities-max-frame_W.csv.gz`  
  Contains **multiword tokens** only.

- `probabilities-max-frame_S.csv.gz`  
  Contains **single-word tokens** only.

These files are treated as raw acoustic inputs and are processed downstream by the preprocessing pipeline.

---

### 2. AAPS Data (Clinical Assessment)

The second data source corresponds to clinical assessment results from the **AAPS test**.

The file used in this project is:

- `AAPS_Score_Data (Long Version).csv`

This file contains the long-format AAPS scoring data used for binomial modeling and cut-point estimation.

---

### Notes

- All files in `data/raw` are considered **external inputs** and should not be modified by the pipeline.
- File paths are referenced via `file_paths.R` to avoid hard-coding paths in the codebase.
- If any of these files are missing, the pipeline will fail during the preprocessing stage.
