## Evomics Workshop on Genomics - microbiome data analysis session

## Overview 

Exercises and other materials used for teaching the microbiome data analysis practical session at the Evomics Workshop on Genomics

- Exercises directory contains quarto documents (exercises_1/2.qmd) and their output html files (exercises_1/2.html)
- The html pages can also be viewed online at:
  - https://david-barnett.github.io/evomics-material/exercises/exercises_1.html
  - https://david-barnett.github.io/evomics-material/exercises/exercises_2.html


## Setup

- Install a recent version of R (e.g. 4.2) and RStudio (e.g. a mid/late 2023 version)
- Clone this repo (and configure this github repo to be the upstream remote so future updates can be pulled)
- Follow the installation instructions for microViz and other dependencies here: <https://github.com/david-barnett/evomics-material/blob/main/setup/install-microViz-and-deps.R>

### Test the setup

- Open RStudio
- Open the RStudio project .Rproj file for this repo
- Render both Quarto documents in the "exercises" [directory](https://github.com/david-barnett/evomics-material/tree/main/exercises): **exercises_1.qmd** and **exercises_2.qmd**
- Test the interactive microViz functions work
    - run `tax_fix_interactive(mice)` in the R console after loading the mice dataset
    - test `ord_explore()` e.g. by running the code below

```r
library(microViz)

shao19 %>%
  ps_filter(family_role == "mother") %>%
  tax_filter(min_prevalence = 2.5 / 100, verbose = FALSE) %>%
  ord_explore()
``` 
