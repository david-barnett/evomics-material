library(vegan)
library(tidyverse)
library(packcircles)
library(patchwork)

library(grid)
library(gridExtra)
library(gtable)
set.seed(1)

# set plotting output dir path
plotdir <- here::here("slides/2023/concepts-intro")

# Simulate biomes ------

# probability of selecting each bug (different for each "biome": A,B,C etc)
bugProbs <- list(
  A = c(10, 10, 10, 00, 00, 00, 00), # even
  B = c(10, 10, 10, 10, 10, 10, 10), # rich and even
  C = c(90, 20, 10, 01, 00, 00, 00), # uneven
  X = c(10, 20, 20, 10, 10, 00, 00),
  Y = c(00, 10, 10, 10, 10, 20, 30),
  Z = c(40, 30, 10, 10, 00, 10, 00)
)
bug_colours <- RColorBrewer::brewer.pal(n = 7, name = "Paired")
names(bug_colours) <- c("one", "two", "three", "four", "five", "six", "seven")
N <- 150 # number of circles per biome

# Function to generate a biome by sampling N bugs vector with the given probabilities
generate_biome <- function(probs, N, bugs = names(bug_colours)) {
  chr <- sample(bugs, size = N, replace = TRUE, prob = probs / sum(probs))
  return(factor(chr, levels = bugs))
}

# Generate the biomes data frame
biomes <- as.data.frame(lapply(bugProbs, generate_biome, N = N))

# Add a circle ID column
biomes$circle <- seq_len(nrow(biomes))

biomesSub <- sample_n(biomes, size = 75, replace = FALSE)

# Compute inner circles' data ------------------------------------------------
# Inner circles: centres and radii
centres <- list()
centresSub <- list()
for (biome in names(bugProbs)) {
  centres[[biome]] <- biomes[, c(biome, "circle")]
  centresSub[[biome]] <- biomesSub[, c(biome, "circle")]

  # Specify subtly varying sizes, to make the circle layout look more organic
  centres[[biome]]$size <- rnorm(n = nrow(biomes), mean = 1, sd = 0.1)
  # Subsetted version, fewer circles
  centresSub[[biome]]$size <- rnorm(n = nrow(biomesSub), mean = 1, sd = 0.1)
  
  # Compute layouts
  layout <- circleProgressiveLayout(x = centres[[biome]], sizecol = "size")
  layoutSub <- circleProgressiveLayout(x = centresSub[[biome]], sizecol = "size")
  
  # Add layouts to bug name data
  centres[[biome]] <- as_tibble(cbind(centres[[biome]], layout))
  centresSub[[biome]] <- as_tibble(cbind(centresSub[[biome]], layoutSub))
  
  # more explanatory variable names
  centres[[biome]] <- rename(centres[[biome]], y_centre = y, x_centre = x)
  centresSub[[biome]] <- rename(centresSub[[biome]], y_centre = y, x_centre = x)
}

# Inner circles: outline positions
circles <- list()
circlesSub <- list()
for (biome in names(centres)) {
  circles[[biome]] <- circleLayoutVertices(
    layout = centres[[biome]], npoints = 30, idcol = "circle",
    xysizecols = c("x_centre", "y_centre", "radius")
  )
  circlesSub[[biome]] <- circleLayoutVertices(
    layout = centresSub[[biome]], npoints = 30, idcol = "circle",
    xysizecols = c("x_centre", "y_centre", "radius")
  )
}

# Compute outer circle data -------------------------------------------------
# Radius - estimated under assumption circle packing is perfect, then add a bit
outlineRadius <- sqrt(N / pi) * 1.2

# Calculate the angle between each pair of adjacent points on outer circle
angle_step <- 2 * pi / 50

# Create a vector of angles around outer circle
angles <- seq(0, 2 * pi - angle_step, by = angle_step)

# compute outer circle outline coordinate positions
outer_circle <- data.frame(
  x_outer = outlineRadius * cos(angles),
  y_outer = outlineRadius * sin(angles)
)

# Create biome plots in lists ------------------------------------------------

# outer circle polygon geom
geom_outer_poly <- geom_polygon(
  data = outer_circle, 
  mapping = aes(x = x_outer, y = y_outer),
  colour = "grey15", fill = "grey90", linewidth = 2
)

# biome plot colour scheme
scale_fill_biome <- scale_fill_manual(values = bug_colours, guide = NULL) 

# biome plot themes
theme_biome <- theme_void(base_size = 15) +
  theme(
    plot.tag.position = c(0.95, 0.95),
    plot.tag = element_text(face = "bold"),
    plot.margin = margin(12, 12, 12, 12)
  )

# biome plot coords
coord_biome <- coord_fixed(
  xlim = c(outlineRadius, -outlineRadius),
  ylim = c(outlineRadius, -outlineRadius),
  expand = TRUE, ratio = 1
)

# function to plot outline
biome_plot_outline <- function(data) {
  ggplot(data) +
    scale_fill_biome +
    coord_biome +
    theme_biome +
    labs(tag = biome) +
    geom_outer_poly
}

plots_full <- list()
for (biome in names(circles)) {
  plots_full[[biome]] <-
    centres[[biome]] %>%
    left_join(circles[[biome]], by = c(circle = "id")) %>%
    biome_plot_outline() +
    # inner circles polygons geom
    geom_polygon(
      mapping = aes(x = x, y = y, fill = .data[[biome]], group = circle),
      colour = "grey35", linewidth = 0.2
    )
}

plots_sub <- list()
for (biome in names(circlesSub)) {
  plots_sub[[biome]] <-
    centresSub[[biome]] %>%
    left_join(circlesSub[[biome]], by = c(circle = "id")) %>%
    biome_plot_outline() +
    # inner circles polygons geom
    geom_polygon(
      mapping = aes(x = x, y = y, fill = .data[[biome]], group = circle),
      colour = "grey35", linewidth = 0.2
    )
}

plots_drop4 <- list()
for (biome in names(circles)) {
  plots_drop4[[biome]] <-
    centres[[biome]] %>%
    dplyr::filter(!.data[[biome]] %in% c("four")) %>% 
    left_join(circles[[biome]], by = c(circle = "id")) %>%
    biome_plot_outline() +
    # inner circles polygons geom
    geom_polygon(
      mapping = aes(x = x, y = y, fill = .data[[biome]], group = circle),
      colour = "grey35", linewidth = 0.2
    )
}

plots_drop43 <- list()
for (biome in names(circles)) {
  plots_drop43[[biome]] <-
    centres[[biome]] %>%
    dplyr::filter(!.data[[biome]] %in% c("four", "three")) %>% 
    left_join(circles[[biome]], by = c(circle = "id")) %>%
    biome_plot_outline() +
    # inner circles polygons geom
    geom_polygon(
      mapping = aes(x = x, y = y, fill = .data[[biome]], group = circle),
      colour = "grey35", linewidth = 0.2
    )
}

plots_drop432 <- list()
for (biome in names(circles)) {
  plots_drop432[[biome]] <-
    centres[[biome]] %>%
    dplyr::filter(!.data[[biome]] %in% c("four", "three", "two")) %>% 
    left_join(circles[[biome]], by = c(circle = "id")) %>%
    biome_plot_outline() +
    # inner circles polygons geom
    geom_polygon(
      mapping = aes(x = x, y = y, fill = .data[[biome]], group = circle),
      colour = "grey35", linewidth = 0.2
    )
}


# Save biome plots in patchwork grids ----------------------------------------

ABC_design <- "
  12
  #3"

ABCXYZ_design <- "
  14
  25
  36"

ggsave(
  plot = wrap_plots(plots_full[1:3], design = ABC_design),
  filename = file.path(plotdir, "biome-circles/ABC.png"),
  device = "png", width = 4, height = 4, units = "in"
)
# dropping some bugs - for when "moving" them to the dotplot
ggsave(
  plot = wrap_plots(plots_drop4[1:3], design = ABC_design),
  filename = file.path(plotdir, "biome-circles/ABC-drop4.png"),
  device = "png", width = 4, height = 4, units = "in"
)
ggsave(
  plot = wrap_plots(plots_drop43[1:3], design = ABC_design),
  filename = file.path(plotdir, "biome-circles/ABC-drop43.png"),
  device = "png", width = 4, height = 4, units = "in"
)
ggsave(
  plot = wrap_plots(plots_drop432[1:3], design = ABC_design),
  filename = file.path(plotdir, "biome-circles/ABC-drop432.png"),
  device = "png", width = 4, height = 4, units = "in"
)

ggsave(
  plot = wrap_plots(plots_full[1:6], design = ABCXYZ_design), 
  filename = file.path(plotdir, "biome-circles/ABCXYZ.png"),
  device = "png", width = 4, height = 6, units = "in"
)
# dropping some bugs - for when "moving" them to the dotplot
ggsave(
  plot = wrap_plots(plots_drop4[1:6], design = ABCXYZ_design),
  filename = file.path(plotdir, "biome-circles/ABCXYZ-drop4.png"),
  device = "png", width = 4, height = 6, units = "in"
)
ggsave(
  plot = wrap_plots(plots_drop43[1:6], design = ABCXYZ_design),
  filename = file.path(plotdir, "biome-circles/ABCXYZ-drop43.png"),
  device = "png", width = 4, height = 6, units = "in"
)
ggsave(
  plot = wrap_plots(plots_drop432[1:6], design = ABCXYZ_design),
  filename = file.path(plotdir, "biome-circles/ABCXYZ-drop432.png"),
  device = "png", width = 4, height = 6, units = "in"
)

ggsave(
  plot = wrap_plots(c(plots_full[4:6], plots_sub[4:6]), design = ABCXYZ_design),
  filename = file.path(plotdir, "biome-circles/XYZ-XYZsub.png"),
  device = "png", width = 4, height = 6, units = "in"
)

# Save a little 3-circle legend -------------------------------------------
circle_data <- data.frame(
  circle_id = 1:3,
  y_center = c(0.7, 0.5, 0.3)
)

# Assign colors to each circle
circle_colors <- RColorBrewer::brewer.pal(3, "Paired")
names(circle_colors) <- circle_data$circle_id

# Create the ggplot image
circle_plot <- ggplot(data = circle_data) +
  geom_point(
    aes(x = 1, y = y_center, fill = factor(circle_id)),
    shape = 21, size = 10, stroke = 0.5, colour = "grey35"
  ) +
  scale_fill_manual(values = circle_colors, guide = NULL) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0), breaks = NULL) +
  theme_void() +
  theme(plot.margin = margin(0, 0, 0, 0))

# Save the ggplot image as a file
ggsave(
  filename = file.path(plotdir, "biome-circles/three_circles_legend.png"),
  plot = circle_plot, width = 0.5, height = 2, units = "in"
)

# Compute diversity, richness and evenness --------------------------------

# Function to compute the (effective) Shannon diversity index
shannon_diversity <- function(biome, exp = TRUE) {
  counts <- table(droplevels(biome))
  proportions <- counts / sum(counts)
  diversity <- -sum(proportions * log(proportions))
  if (isTRUE(exp)) diversity <- exp(diversity)
  return(diversity)
}
# Count number of unique bugs
observed_richness <- function(biome) {
  length(unique(biome))
}
# Compute Pielou's evenness
pielou_evenness <- function(observed_richness, shannon_div, is_exp_shannon = TRUE) {
  max_diversity <- log(observed_richness)
  if (isTRUE(is_exp_shannon)) shannon_div <- log(shannon_div)
  shannon_div / max_diversity
}

# Apply the Shannon diversity, observed richness,
# and Pielou's evenness functions to the columns A, B, and C
diversity <- sapply(biomes[, c("A", "B", "C")], shannon_diversity)
richness <- sapply(biomes[, c("A", "B", "C")], observed_richness)
evenness <- mapply(pielou_evenness, richness, diversity)

# Combine the results into a data frame
diversity_table <- tibble(
  Metric = c("Richness", "Evenness", "Diversity"),
  A = c(richness["A"], evenness["A"], diversity["A"]),
  B = c(richness["B"], evenness["B"], diversity["B"]),
  C = c(richness["C"], evenness["C"], diversity["C"])
)

# diversity_table[-1] <- round(diversity_table[-1], digits = 1)
diversity_table[-1] <- lapply(
  X = diversity_table[-1],
  FUN = function(x) formatC(x, format = "f", digits = 1, flag = "#")
)


# Write diversity table as image ------------------------------------------

# Horrifying grid, gridExtra and gtable magic
# https://cran.r-project.org/web/packages/gridExtra/vignettes/tableGrob.html

# Create a table with the rounded results data frame
div_tab_theme <- ttheme_minimal(base_size = 20, padding = unit(c(13, 10), "mm"))

# Create separator grobs
Vline <- segmentsGrob(x1 = 0, gp = gpar(lty = 2))
Hline <- segmentsGrob(y1 = 0, gp = gpar(lty = 2))

div_tab_grob <- diversity_table %>% 
  tableGrob(theme = div_tab_theme, rows = NULL) %>% 
  gtable_add_grob(grobs = list(Vline, Vline), t = 2, b = 4, l = 3:4) %>% 
  gtable_add_grob(grobs = list(Hline, Hline), t = 2:3, l = 2, r = 4) 

# remove "Metric" text
div_tab_grob$grobs[[1]]$label <- "" 

# Save the table as an image
png(
  filename = file.path(plotdir, "diversity_table.png"),
  width = 4, height = 2.5, units = "in", res = 300
)
grid::grid.draw(div_tab_grob)
dev.off()

# Compute dissimilarities --------------------------------

# Create a count matrix for each relevant biome
biomes_count_matrix <- t(sapply(c("A", "B", "C"), function(col) {
  as.matrix(table(as.factor(biomes[[col]])))
}))
colnames(biomes_count_matrix) <- levels(as.factor(biomes$A))

# Compute distances
bray_curtis <- vegdist(biomes_count_matrix, method = "bray")
binary_jacc <- vegdist(biomes_count_matrix, method = "jaccard", binary = TRUE)
rclr_euclid <- vegdist(biomes_count_matrix, method = "robust.aitchison")

# Write dissimilarity tables as images ----------------------------------------

# Round the distances and format for printing
round_dists <- function(x) formatC(x, format = "f", digits = 2)
bray_curtis_mat <- apply(as.matrix(bray_curtis), MARGIN = 2, FUN = round_dists)
binary_jacc_mat <- apply(as.matrix(binary_jacc), MARGIN = 2, FUN = round_dists)
rclr_euclid_mat <- apply(as.matrix(rclr_euclid), MARGIN = 2, FUN = round_dists)

# Specify distance table grob theme
dist_tab_theme <- ttheme_minimal(
  base_size = 20, padding = unit(c(10, 10), "mm"),
  rowhead = list(fg_params = list(fontface = "bold", x = 0.5))
)

# Create table_grobs with the Bray-Curtis and BinaryJaccard dist matrices
bray_table_grob <- tableGrob(d = bray_curtis_mat, theme = dist_tab_theme)
jacc_table_grob <- tableGrob(d = binary_jacc_mat, theme = dist_tab_theme)
rclr_table_grob <- tableGrob(d = rclr_euclid_mat, theme = dist_tab_theme)

# Create separator grobs
Vline <- segmentsGrob(x1 = 0, gp = gpar(lty = 2))
Hline <- segmentsGrob(y1 = 0, gp = gpar(lty = 2))

# Add separator grobs
bray_table_grob <- bray_table_grob %>%
  gtable_add_grob(grobs = list(Vline, Vline), t = 2, b = 4, l = 3:4) %>%
  gtable_add_grob(grobs = list(Hline, Hline), t = 2:3, l = 2, r = 4)

jacc_table_grob <- jacc_table_grob %>%
  gtable_add_grob(grobs = list(Vline, Vline), t = 2, b = 4, l = 3:4) %>%
  gtable_add_grob(grobs = list(Hline, Hline), t = 2:3, l = 2, r = 4)

rclr_table_grob <- rclr_table_grob %>%
  gtable_add_grob(grobs = list(Vline, Vline), t = 2, b = 4, l = 3:4) %>%
  gtable_add_grob(grobs = list(Hline, Hline), t = 2:3, l = 2, r = 4)

# Save the tables as images
write_dist_tab_png <- function(grob, file) {
  f <- file.path(plotdir, file)
  png(filename = f, width = 4, height = 4, units = "in", res = 300)
  grid::grid.draw(grob)
  dev.off()
}
write_dist_tab_png(bray_table_grob, file = "bray_curtis_distmat.png")
write_dist_tab_png(jacc_table_grob, file = "binary_jacc_distmat.png")
write_dist_tab_png(rclr_table_grob, file = "rclr_euclid_distmat.png")


# DA dotplots -------------------------------------------------------------

dotplots <- biomes[, LETTERS[1:3], drop = FALSE] %>% 
  pivot_longer(cols = everything(), names_to = "biome", values_to = "bug") %>% 
  dplyr::filter(bug %in% c("four", "three", "two")) %>% 
  mutate(bug = factor(bug, levels = c("four", "three", "two"))) %>% 
  ggplot(aes(x = biome, fill = bug)) +
  facet_wrap(vars(bug), nrow = 1) +
  geom_dotplot(binwidth = 4/10, stackratio = 0.65, colour = "grey25") +
  scale_fill_biome +
  scale_y_continuous(limits = c(0, NA), expand = c(0.005, 0)) +
  theme_classic() +
  theme(
    axis.line.y.left = element_blank(),
    axis.ticks.y.left = element_blank(),
    axis.text.y.left = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_text(size = 28, face = "bold", color = "black"),
    panel.background = element_rect(fill = "transparent", colour = "transparent"),
    plot.background = element_rect(fill = "transparent", colour = "transparent")
  )

ggsave(
  plot = dotplots, bg = "transparent",
  filename = file.path(plotdir, "dotplots.png"),
  width = 5, height = 7, units = "in", device = "png", dpi = 300
)

dotplotsXYZ <- biomes[, c("X", "Y", "Z"), drop = FALSE] %>% 
  pivot_longer(cols = everything(), names_to = "biome", values_to = "bug") %>% 
  dplyr::filter(bug %in% c("four", "three", "two")) %>% 
  mutate(bug = factor(bug, levels = c("four", "three", "two"))) %>% 
  ggplot(aes(x = biome, fill = bug)) +
  facet_wrap(vars(bug), nrow = 1) +
  geom_dotplot(binwidth = 4/10, stackratio = 0.65, colour = "grey25") +
  scale_fill_biome +
  scale_y_continuous(limits = c(0, NA), expand = c(0.005, 0)) +
  theme_classic() +
  theme(
    axis.line.y.left = element_blank(),
    axis.ticks.y.left = element_blank(),
    axis.text.y.left = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_text(size = 28, face = "bold", color = "black"),
    panel.background = element_rect(fill = "transparent", colour = "transparent"),
    plot.background = element_rect(fill = "transparent", colour = "transparent")
  )

ggsave(
  plot = dotplotsXYZ, bg = "transparent",
  filename = file.path(plotdir, "dotplotsXYZ.png"),
  width = 5, height = 7, units = "in", device = "png", dpi = 300
)
  
# Compositions barcharts -----------------------------------------------------

# Counts data for original and subset biomes 
XYZcounts <- as.data.frame(rbind(
  t(sapply(X = biomes[, c("X", "Y", "Z")], FUN = table)),
  t(sapply(X = biomesSub[, c("X", "Y", "Z")], FUN = table))
))
XYZcounts$Biomass <- rep(c("High", "Low"), each = 3)
XYZcounts$sample <- rep(c("X", "Y", "Z"), times = 2)

# Make barplots
theme_bars <- theme_minimal() +
  theme(
    text = element_text(size = 12, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.spacing = unit(0.2, "in"), 
    strip.clip = "off",
    strip.placement = "outside",
    strip.text = element_text(size = 14, face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

# Make counts barplot
plotXYZcounts <- XYZcounts %>% 
  pivot_longer(!c(Biomass, sample), names_to = "Taxon", values_to = "Counts") %>% 
  mutate(Taxon = factor(Taxon, levels = rev(names(bug_colours)))) %>% 
  ggplot() +
  facet_grid(cols = vars(sample), scales = "free", switch = "x") +
  geom_col(
    mapping = aes(Biomass, Counts, fill = Taxon), 
    width = 0.7, colour = "grey35", linewidth = 0.3
  ) +
  scale_fill_biome +
  scale_y_continuous(limits = c(0, 160), expand = c(0.005, 0)) +
  theme_bars

# Convert to compositions
XYZcompositions <- XYZcounts %>% 
  mutate(total = rowSums(pick(where(is.numeric)))) %>% 
  mutate(across(where(is.numeric), function(x) x / total))

# Make compositions barplot
plotXYZcomp <- XYZcompositions %>% 
  select(!total) %>% 
  pivot_longer(!c(Biomass, sample), names_to = "Taxon", values_to = "Proportion") %>% 
  mutate(Taxon = factor(Taxon, levels = rev(names(bug_colours)))) %>% 
  ggplot() +
  facet_grid(cols = vars(sample), scales = "free", switch = "x") +
  geom_col(
    mapping = aes(Biomass, Proportion, fill = Taxon), 
    width = 0.7, colour = "grey35", linewidth = 0.3
  ) +
  scale_fill_biome +
  scale_y_continuous(limits = c(0, 1.025), expand = c(0.005, 0)) +
  theme_bars

ggsave(
  plot = plotXYZcounts, bg = "transparent",
  filename = file.path(plotdir, "XYZcounts.png"),
  width = 3.7, height = 3.3, units = "in", device = "png", dpi = 300
)

ggsave(
  plot = plotXYZcomp, bg = "transparent",
  filename = file.path(plotdir, "XYZcompositions.png"),
  width = 3.7, height = 3.3, units = "in", device = "png", dpi = 300
)

