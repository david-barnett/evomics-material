# making the ibd.rds data by quick cleaning of corncob::ibd_phylo

library(phyloseq)
library(microViz)

ibd <- corncob::ibd_phylo %>% 
  tax_mutate(Species = NULL) %>% # ibd_phylo Species column was blank -> deleted
  ps_mutate(disease = ibd == 'ibd', ibd = NULL) # adds disease state indicator variable

saveRDS(ibd, file = here::here('data/ibd.rds'))


