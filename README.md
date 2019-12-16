# Eliminating Bias in Treatment Effect EStimation Arising from Adaptively Collected Data

This repo contains the simulation code and the latest version of my paper.

- to simulate allocations with different treatment rules, run `scripts/mc-treatment-rules-normal-distribution.R` (make sure that the `SIMULATION_FOLDER` defined in `global.R` exists on your local repo: the script will save the resulting files there)
- to summarize the simulation results (calculate welfare, treatment effect, etc.), run `scripts/summarize-measures-across-setups.R` (make sure that the `INTERIM_RESULT_FOLDER` defined in `global.R` exists on your local repo: the script will save the resulting files there)
- the other scripts accomplish different parts of the analysis, and generate the charts and tables of the paper
