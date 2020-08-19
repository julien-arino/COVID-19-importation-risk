# covid-19-importation-risk

These are files used in and as a complement to the paper _Assessing the risk of COVID-19 importation and the effect of quarantine_, by Julien Arino, Nicolas Bajeux, Stephanie Portet and James Watmough. While the paper has not found a home yet, it is available on medRxiv [here](https://www.medrxiv.org/content/10.1101/2020.08.12.20173658v1). We will progressively be adding files here.

## Quarantine calculator

The (R) Shiny app `Q_calculator_shiny` is a quarantine efficacy calculator. It is also running online [here](http://35.182.10.46:3838/Q_calculator/). You can set disease progression paramaters and the duration of quarantine. It is based on the SLIAR model in the paper and provides two views: the efficacy of quarantine, which we define as the percentage of unobservable cases (L_1, L_2, A_1 and A_2) that are still unobservable at the end of the quarantine period; and the trajectories of individuals that start in one of these unobservable states.

## Single stimulation simulations

The (R) Shiny app `single_stimulation_simulations_shiny` runs a number of single stimulation simulations. It is also running online [here](http://35.182.10.46:3838/single_stimulation_trajectories_shiny/). It allows you to change most model parameters. 
