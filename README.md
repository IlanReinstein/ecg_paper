# Multi-level longitudinal learning curve regression models integrated with item difficulty metrics for deliberate practice of visual diagnosis: groundwork for adaptive learning.

Visual diagnosis of radiographs, histology and electrocardiograms lends itself to deliberate practice, facilitated by large online banks of cases. Which cases to supply to which learners in which order is still to be worked out, with there being considerable potential for adapting the learning. Advances in statistical modeling, based on an accumulating learning curve, offer methods for more effectively pairing learners with cases of known calibrations. Using demonstration radiograph and electrocardiogram datasets, the advantages of moving from traditional regression to multilevel methods for modeling growth in ability or performance are demonstrated, with a final step of integrating case-level item-response information based on diagnostic grouping. This produces more precise individual-level estimates that can eventually support learner- adaptive case selection. The progressive increase in model sophistication is not simply statistical but rather brings the models into alignment with core learning principles including the importance of taking into account individual differences in baseline skill and learning rate as well as the differential interaction with cases of varying diagnosis and difficulty. The developed approach can thus give researchers and educators a better basis on which to anticipate learners’ pathways and individually adapt their future learning.

Read the full paper here (ToDo)

--------

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

Once all the files have been created in the respective folders, the report `manuscript.Rmd` should render without issues.


-----

If you have any questions or comments about the contents of this repository please contact me at `ilan.reinstein [at] nyulangone.org`.
