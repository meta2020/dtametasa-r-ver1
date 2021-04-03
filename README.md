# DTA-META-SA CODE


This folder contains reproducible R codes of simulation studies and re-analysis of the example data.

The following packages are used in the simulation or example data

- "mvmeta", "foreach", "parallel", "doSNOW", "doRNG", "latex2exp", "kable; 

If they are not installed, please install from R CRAN `install.packages("package_name")`.
 

## example/

- RData/: produced results data

- simfun/: self-made R functions 

- data-IVD.csv: Example 1 data

- data-Lym.csv: Example 2 data

- **examples.R: reproducible codes to generate results (for double-check)**

- **PDF-ivd-lym.Rmd: reproduce figures and table (for double-check)**


## simulation/

- res/

	- Readme.txt: notations

	- 3 folders to save the simulation results

- scenario/ 

	- 18rows/: scenarios RData

	- Readme.txt: notations

	- scenario-all.R: create all scenarios in the simulation

	- cal-p.R: to calculate alpha in the scenarios

	- Table-scenario-s.Rmd: show the used scenarios

- simfun/

	- some self-made functions used in the simulation

- **1000-times-sim.R: reproducible 1000 times simulation (for double-check)**
 
- **xx.Rmd files: reproduce tables and figures (for double-check)**












