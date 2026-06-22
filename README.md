# Beyond paternal care: career stage and reproductive opportunities shape male services in vervet monkeys

## Summary

Male vervet monkeys provide several cooperative services that benefit the group, including predator alarm calling, participation in between-group conflicts, leading risky river crossings, and sentinelling. Using long-term data from four wild groups, we show that service provision varies across male career stages and is associated with reproductive opportunities. In particular, participation in between-group conflicts strongly predicts mating success, supporting the idea that some cooperative behaviours function as signals evaluated by females.

For the full abstract, see the manuscript.

## Repository Structure

├── Public_data/                 # Data used in the analyses
├── Alarm_model_p.R              # Alarm calling analyses
├── BGE_model_p.R                # Between-group conflict analyses
├── Crossing_model_p.R           # River crossing analyses
├── Sentinelling_model_p.R       # Sentinelling analyses
├── Mating_model_p.R             # Mating success analyses
├── MSIndex_automated_p.R        # Male service index calculation
├── Tables.R                     # Tables and supplementary outputs
└── Plots_DescriptiveTables_p.R  # Figures and exploratory plots

## Data and Reproducibility

All data used in the analyses can be found in the `Public_data` folder.

The script `MSIndex_automated_p.R` requires access to the complete behavioural dataset from the Inkawu Vervet Project (IVP), which cannot be publicly shared. The resulting male service indices used in the manuscript are included in the public dataset.

## Code Overview

This project focuses on four types of cooperative services:

* **Alarm calling** (`Alarm`)
* **Between-group conflicts (BGC)**, referred to in the code as **BGE** (*between-group encounters*)
* **Crossing first** (`Crossing`)
* **Sentinelling**, sometimes referred to as **vigilance** in the code

To make the scripts easier to navigate, it is recommended to collapse the code sections marked with `{}`.

### Main Analysis Scripts

Each service-specific script contains:

1. Data cleaning and preparation.
2. Model 1: sex differences in service provision.
3. Model 2: determinants of service provision among males.

### Tables and Summary Statistics

`Tables.R` generates the summary tables and model output tables presented in the supplementary material.

### Mating Analyses

`Mating_model_p.R` contains the analyses examining mating success during the mating season using a model-selection approach.

### Figures

All figures included in the manuscript can be generated from:

`Plots_DescriptiveTables_p.R`

This script also contains exploratory and alternative visualisations that were not included in the final manuscript.

## Software

Analyses were conducted in R (version 4.4.2).

Main packages include:

- glmmTMB
- lme4
- MuMIn
- DHARMa
- effects
- ggplot2
- dplyr

## Citation

If you use this code or data, please cite:

Granell Ruiz M., Tankink J.A., van de Waal E., van Schaik C.P., & Bshary R.

*Beyond paternal care: career stage and reproductive opportunities shape male services in vervet monkeys.*