# Beyond paternal care: career stage and reproductive opportunities shape male services in vervet monkeys 

## Abstract 
Why male primates invest in costly behaviours producing public goods remains debated, with two leading explanations, paternal care and reputation-based partner choice (RBPC). Using long-term data from four groups of wild vervet monkeys, we tested: (1) whether males show a bias in four protective “male services” (predator alarm calling, participation in between-group conflicts, leading river crossings and sentinelling); (2) which males contribute most; and (3) whether service provision predicts mating success during the mating season. We confirmed a male bias in all services. Consistent with the paternal care hypothesis, contributions were positively associated with past mating success, independently of rank, although potential fathers did not contribute more than non-fathers. Among non-fathers, service provision varied with rank, suggesting that newly immigrated males adjust their behaviour according to competitive state. Crucially, variation in alarm calling and between-group conflicts predicted future mating success, with between-group conflict emerging as the strongest and most consistent predictor of mating success across years and within mating seasons, whereas rank, tenure and social integration added little explanatory power. In contrast, sentinelling and leading river crossings did not reliably translate into mating benefits. Our findings indicate that male services are shaped by multiple selective pressures operating across different male career stages and that some forms of public goods provision function as signals of quality and cooperativeness to females. By directly linking cooperative investment to mating outcomes in a wild primate, this study provides rare empirical support for reputation-based partner choice beyond humans and highlights female choice as a potentially important force in the evolution of cooperation. 

## Code Overview

All data used in the analyses can be found in the `Public_data` folder.

This project focuses on four types of cooperative services:

* **Alarm calling** (`Alarm`)
* **Between-group conflicts (BGC)**, referred to in the code as **BGE** (*between-group encounters*)
* **Crossing first** (`Crossing`)
* **Sentinelling**, sometimes referred to as **vigilance** in the code

To make the scripts easier to navigate, it is recommended to collapse the code sections marked with `{}`.

### Main Analysis Scripts

The analyses are organised by service type:

* `Alarm_model_p.R`
* `BGE_model_p.R`
* `Crossing_model_p.R`
* `Sentinelling_model_p.R`

Each of these scripts contains:

1. Data cleaning and preparation.
2. **Model 1**, which tests for sex differences in service provision.
3. **Model 2**, which investigates the determinants of service provision among males.

### Tables and Summary Statistics

The script `Tables.R` generates the summary tables and model output tables presented in the supplementary material.

### Male Service Index

`MSIndex_automated_p.R` contains the code used to calculate the proportion of services provided by each individual while present in a group.

Running this script requires access to the complete behavioural dataset from the Inkawu Vervet Project (IVP), which cannot be publicly shared. The code is nevertheless provided for transparency and to document how the service provision indices were calculated. These indices are subsequently used in the mating analyses.

### Mating Analyses

`Mating_model_p.R` contains the analyses examining mating success during the mating season. The script follows a model selection approach to identify predictors of mating success.

### Figures

All figures included in the manuscript can be generated from:

* `Plots_DescriptiveTables_p.R`

This script also contains additional exploratory and alternative visualisations that were not included in the final manuscript. These are clearly labelled as trial or exploratory plots within the code.
