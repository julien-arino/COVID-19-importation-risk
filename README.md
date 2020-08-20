# covid-19-importation-risk

These are files used in and as a complement to the paper _Assessing the risk of COVID-19 importation and the effect of quarantine_, by Julien Arino, Nicolas Bajeux, Stephanie Portet and James Watmough. While the paper has not found a home yet, it is available on medRxiv [here](https://www.medrxiv.org/content/10.1101/2020.08.12.20173658v1). We will progressively be adding files here.

We have set up a small AWS server on which the (R) Shiny apps run, so you can try these components without having to install R. It is located [here](http://35.182.10.46:3838/). However, be mindful of the fact we purposefully limited capacities because of the limited capacity of the server. To run code as in the paper, you will have to download the code and run it yourself. Note that you can run the Shiny apps from R without cloning the Github repo by using a command of the form
```
shiny::runGitHub("julien-arino/covid-19-importation-risk", username = "some-github-user-name", subdir = "subdir_name")
```
where `subdir_name` is one of the subdirectories of Shiny apps: `Q_calculator_shiny`, `single_stimulation_simulations_shiny` or `CAN_PT_HR_new_cases_shiny`.

### Quarantine calculator

The (R) Shiny app `Q_calculator_shiny` is a quarantine efficacy calculator. It is also running online [here](http://35.182.10.46:3838/Q_calculator/). You can set disease progression paramaters and the duration of quarantine. It is based on the SLIAR model in the paper and provides two views: the efficacy of quarantine, which we define as the percentage of unobservable cases (L_1, L_2, A_1 and A_2) that are still unobservable at the end of the quarantine period; and the trajectories of individuals that start in one of these unobservable states.

### Single stimulation simulations

The (R) Shiny app `single_stimulation_simulations_shiny` runs a number of single stimulation simulations. It is also running online [here](http://35.182.10.46:3838/single_stimulation_trajectories_shiny/). It allows you to change most model parameters. 

### Number of jurisdictions with new cases

The (R) Shiny app `CAN_PT_HR_new_cases_shiny` (running online [here](http://35.182.10.46:3838/CAN_PT_HR_new_cases_shiny/)) allows to produce variations on what is at present the first figure in the paper. When considering imporations or reimportations, it is useful to know how long a jurisdiction has been with/without cases. For now, we only show here jurisdictions in Canada. Code to update the epidemic information is provided (`refresh_epi_data_CAN.R`, `process_epi_data_CAN_confirmed.R` and `process_epi_data_CAN_deaths.R`).
