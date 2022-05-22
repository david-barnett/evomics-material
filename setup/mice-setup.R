# making the mice.rds data by combining 16S_data.RDS and 16S_mapping.txt from 2020 workshop: 
# https://github.com/shandley/Microbiome-Analysis-Using-R/tree/ee50c37ddb7a678c62fcfcec8ccd6a274ed21952/data

library(phyloseq)
library(microViz)

mice <- readRDS(here::here('data/2020/16S_data.RDS'))
miceMeta <-  phyloseq::import_qiime_sample_data(here::here('data/2020/16S_mapping.txt'))
mice <- phyloseq::merge_phyloseq(mice, phyloseq::sample_data(miceMeta))

taxa_names(mice) <- ntaxa(mice) %>% 
  seq_len() %>% 
  stringr::str_pad(width = 4, side = 'left', pad = '0') %>% 
  paste0('ASV', .)

sample_names(mice) <- mice@sam_data$sample_id %>% stringr::str_remove('\\d+[.]Thackray[.]')

saveRDS(mice, file = here::here('data/mice-tree.rds'))

mice@phy_tree <- NULL
saveRDS(mice, file = here::here('data/mice.rds'))
