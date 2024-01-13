## Evomics Workshop on Genomics - microbiome data analysis session

This is a readme for the GitHub repository found here: <https://github.com/david-barnett/evomics-material>

This repository contains the source code for exercises and other materials used for teaching the microbiome data analysis practical session at the Evomics Workshop on Genomics.

The exercise guidance documents for this workshop can be viewed online:

  - https://david-barnett.github.io/evomics-material/exercises/exercises_1.html
  - https://david-barnett.github.io/evomics-material/exercises/exercises_2.html


## Pre-workshop setup

Workshop participants using the RStudio Server should NOT follow these instructions. These setup steps have already been done for you.

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
