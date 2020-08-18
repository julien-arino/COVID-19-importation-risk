# covid-19-importation-risk

These are files used in and complementing the paper _Assessing the risk of COVID-19 importation and the effect of quarantine_, by Julien Arino, Nicolas Bajeux, Stephanie Portet and James Watmough. While the paper has not found a home yet, it is available on medRxiv [here](https://www.medrxiv.org/content/10.1101/2020.08.12.20173658v1).

## Quarantine calculator

The (R) Shiny app `Q_calculator_shiny.R` is a quarantine efficacy calculator. You can set disease progression paramaters and the duration of quarantune. It is based on the SLIAR model in the paper and provides two views: the efficacy of quarantine, which we define as the percentage of unobservable cases (L_1, L_2, A_1 and A_2) that are still unobservable at the end of the quarantine period; and the trajectories of individuals that start in one of these unobservable states.
