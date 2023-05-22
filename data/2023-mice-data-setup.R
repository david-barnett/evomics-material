# 2023 workshop mice data set up

# All the mice study data
allMice <- readRDS(url("https://github.com/david-barnett/evomics-material/raw/898766166ce151f837b8d668bdc0bf0f859a6dd3/data/mice.rds"))
allMice <- tax_mutate(allMice, Species = NULL) 
readr::write_rds(allMice, file = "data/allMice.rds", compress = "gz")

# Subset of mice for first exercises
mice <- readRDS("data/allMice.rds") %>% 
  ps_filter(virus == "WNV2000", treatment_days == "D7") %>% 
  ps_select(!c(plate, run, sex))
readr::write_rds(mice, file = "data/mice.rds", compress = "gz")

