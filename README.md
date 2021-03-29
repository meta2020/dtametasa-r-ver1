# DTA-META-SA CODE


This folder contains reproducible R codes of simulation studies and re-analysis of the example data.

Before running the codes, please check whether you have installed the required R packages:

- "dtametasa"; if it is not installed, please install it from GitHub `devtools::install_github("meta2020/dtametasa")`. 
(See details in https://meta2020.github.io/dtametasa/)

- "mvmeta", "foreach", "parallel", "doSNOW", "doRNG", "latex2exp", "kable; 
if it is not installed, please install from R CRAN `install.packages("package_name")`.
 

## example/

- data-IVD.csv: Example 1 data

- data-Lym.csv: Example 2 data

- examples.R: reproducible codes for figure and appendix table 


## simulation/

- 1000-times-sim.R: 1000 times simulation 

- xx.Rmd files: reproducible tables and figures 

### scenario/ 

- Readme.txt: notations

- scenario-all.R: create all scenarios in the simulation

- cal-p.R: to calculate alpha in the scenarios

- Table-scenario-s.Rmd: show the used scenarios

- 18rows/: scenarios RData


### simfun/

- simu-sa-4models.R: one-time simulation with outputs of 4 models' results

	* 2 proposed models, 2 reitsma models

- simu-sa-5models.R: one-time simulation with outputs of 5 models' results

	* 3 proposed models, 2 reitsma models

### res/

- Readme.txt: notations

- 3 folders to save the simulation results





