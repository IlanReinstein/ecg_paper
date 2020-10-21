# ECG and Elbow Radiograph 

This repository contains the code and data used to generate the models and plots presented in our paper **Multi-level longitudinal learning curve regression models integrated with item difficulty metrics for deliberate practice of visual diagnosis: groundwork for adaptive learning.**

The structure of this project is such that all R scripts should be run before rendering the final report `manuscript.Rmd` file. The order in which it is run is consistent with each of the sections of the paper:

- Section 3: Linear and Logistic models of learning and plots. See files [`R/linear_models.R`](./R/linear_models.R) and [`R/logistic_models.R](./R/logistic_models.R).

- Section 4: IRT Models. See file [`R/irt_models.R`](./R/irt_models.R)

- Sections 5, 6 and Appendices: Final model proposed and simulations for model evaluation; see files [`R/simulations.R`](./R/simulations.R).

## Data

The data is already cleaned and prepared for usage. These files can be found in [`data/interim`](./data/interim/). These are saved in `RDS` format for stability and performance.

The `data/output` folder contains the data generated from the scripts to reduce the workload of the report rendering, in other words, these are the plotting tables.

## Simulations

Because the simulations require more memory and time, the code used to generate the respective plots can be found in the [`R/simulations.R`](./R/simulations.R) script. 

## Reproducing

In order to reproduce the plots and the models please run the scripts in the following order:

1. `linear_models.R`
2. `logistic_models.R`
3. `irt_models.R`
4. `simulations.R`