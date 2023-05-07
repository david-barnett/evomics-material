# quick cleaning of shao19 dataset, and conversion to rds
library(phyloseq)
library(microViz)

load(here::here('data/other/shao19.rda'))
shao19 <- tax_fix(shao19, verbose = FALSE) %>% tax_names2rank("species")
saveRDS(shao19, file = here::here('data/shao19.rds'))
