library(tidyverse)
library(phyloseq)
library(microViz)

# set plotting output dir path
plotdir <- here::here("slides/2023/ordination")


# get data and compute pcoa ordination ------------------------------------

ibd_subset <- corncob::ibd_phylo %>% 
  tax_filter(min_prevalence = 3) %>% 
  tax_fix() %>% 
  ps_filter(
    DiseaseState %in% c("UC", "nonIBD"), age < 11,
    !activity %in% "inactive"
  ) %>% 
  ps_calc_dominant(
    rank = "Genus", n_max = 2, 
    var = "Top Genus", threshold = 0.1, other = "Other Genus"
  ) %>% 
  ps_mutate(Group = if_else(DiseaseState == "UC", "Colitis", "Healthy")) %>% 
  tax_agg(rank = "Genus")

ibd_subset_ord <- ibd_subset %>% 
  dist_calc(dist = "bray") %>%
  ord_calc(method = "PCoA")


# scree plot --------------------------------------------------------------

scree <- ibd_subset_ord %>%
  ord_get() %>%
  plot_scree() +
  geom_col(data = ~ filter(., axis %in% c("MDS1", "MDS2")), fill = "orange2") +
  scale_x_discrete(limits = paste0("MDS", 1:10)) +
  guides(x = guide_axis(title = NULL, angle = 0)) +
  labs(y = "Variation explained") +
  theme(panel.background = element_blank())

ggsave(
  plot = scree, filename = file.path(plotdir, "pcoa-scree.png"),
  width = 5, height = 2
)


# plain pcoa --------------------------------------------------------------

pcoa_plain <- ibd_subset_ord %>%
  ord_plot(alpha = 0.6, size = 4, color = "grey15", auto_caption = NA) +
  coord_cartesian(xlim = c(-1.25, 3), ylim = c(-2.5, 2)) +
  theme_classic(15) 

ggsave(
  plot = pcoa_plain, filename = file.path(plotdir, "pcoa-plain.png"),
  width = 5, height = 4
)

# dominant genus coloured -------------------------------------------------

pcoa_genera <- ibd_subset_ord %>%
  ord_plot(alpha = 0.6, size = 4, color = "Top Genus", auto_caption = NA) +
  coord_cartesian(xlim = c(-1.25, 3), ylim = c(-2.5, 2)) +
  scale_color_brewer(palette = "Set1") +
  theme_classic(15) +
  theme(
    legend.position = c(0.75, 0.25),
    legend.background = element_rect(colour = "black")
  )
  
ggsave(
  plot = pcoa_genera, filename = file.path(plotdir, "pcoa-genera.png"),
  width = 5, height = 4
)


# disease coloured --------------------------------------------------------

pcoa_uc <- ibd_subset_ord %>% 
  ord_plot(alpha = 0.6, size = 4, color = "Group", auto_caption = NA) +
  scale_color_manual(values = c("#FF4500", "#4169E1")) + 
  coord_cartesian(xlim = c(-1.25, 3), ylim = c(-2.5, 2)) +
  theme_classic(15) +
  theme(
    legend.position = c(0.75, 0.25),
    legend.background = element_rect(colour = "black")
  ) +
  stat_ellipse(aes(color = Group), level = 0.95)


ggsave(
  plot = pcoa_uc, filename = file.path(plotdir, "pcoa-UC.png"),
  width = 5, height = 4
)


# permanova ---------------------------------------------------------------

ibd_subset_ord %>% 
  dist_permanova(variables = "Group", seed = 1, n_perms = 999)


# PCA ---------------------------------------------------------------------

pca_clr <- ibd_subset %>% 
  tax_transform("clr", rank = "Family") %>% 
  ord_calc("PCA") %>% 
  ord_plot(alpha = 0.4, size = 4, color = "Group", auto_caption = NA) +
  scale_color_manual(values = rev(c("#FF4500", "#4169E1"))) + 
  coord_fixed(0.75) +
  theme_classic(15) +
  theme(legend.position = "none") 

ggsave(
  filename = file.path(plotdir, "pca-clr.png"),
  plot = pca_clr, width = 4.5, height = 3.5
)

pcoa_aitchison <- ibd_subset %>% 
  tax_transform("identity", rank = "Family") %>% 
  dist_calc("aitchison") %>% 
  ord_calc("PCoA") %>% 
  ord_plot(
    alpha = 0.4, size = 4, color = "Group", auto_caption = NA
  ) +
  scale_x_reverse() +
  scale_y_reverse() +
  scale_color_manual(values = rev(c("#FF4500", "#4169E1"))) + 
  coord_fixed(0.75, clip = "off") +
  theme_classic(15) +
  theme(legend.position = "none") 

ggsave(
  filename = file.path(plotdir, "pcoa-aitchison.png"),
  plot = pcoa_aitchison, width = 4.5, height = 3.5
)

pca_clr_taxa <- ibd_subset %>% 
  tax_transform("clr", rank = "Family") %>% 
  ord_calc("PCA") %>% 
  ord_plot(
    alpha = 0.4, size = 4, color = "Group", auto_caption = NA,
    plot_taxa = 6:1, tax_vec_length = 0.85,
    tax_lab_style = tax_lab_style(
      type = "text", max_angle = 90, aspect_ratio = 0.75, 
      size = 2.35, fontface = "bold"
    )
  ) +
  scale_color_manual(values = rev(c("#FF4500", "#4169E1"))) + 
  coord_fixed(0.75, clip = "off") +
  theme_classic(15) +
  theme(legend.position = "none") 

ggsave(
  filename = file.path(plotdir, "pca-clr-taxa.png"),
  plot = pca_clr_taxa, width = 4.5, height = 3.5
)
