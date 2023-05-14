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
N <- 150 # number of circles per biome

# Function to generate a biome by sampling N bugs vector with the given probabilities
generate_biome <- function(probs, N) {
  bugs <- c("one", "two", "three", "four", "five", "six", "seven")
  chr <- sample(bugs, size = N, replace = TRUE, prob = probs / sum(probs))
  return(factor(chr, levels = bugs))
}

# Generate the biomes data frame
biomes <- as.data.frame(lapply(bugProbs, generate_biome, N = N))

# Add a circle ID column
biomes$circle <- seq_len(nrow(biomes))

# Compute circle centre centres and radii ------
centres <- list()

for (biome in names(bugProbs)) {
  centres[[biome]] <- biomes[, c(biome, "circle")]

  # Specify subtly varying sizes, to make the circle layout look more organic
  centres[[biome]]$size <- rnorm(n = nrow(biomes), mean = 1, sd = 0.1)

  # Compute layout
  centres[[biome]] <- as_tibble(cbind(
    centres[[biome]],
    circleProgressiveLayout(x = centres[[biome]], sizecol = "size")
  ))
  # more explanatory names
  centres[[biome]] <- rename(centres[[biome]], y_centre = y, x_centre = x)
}

# Compute inner circles outline positions -----
circles <- list()

for (biome in names(centres)) {
  circles[[biome]] <- circleLayoutVertices(
    layout = centres[[biome]], npoints = 30, idcol = "circle",
    xysizecols = c("x_centre", "y_centre", "radius")
  )
}

# Compute outer circle data ------
# radius
outlineRadius <- 1.2 * sqrt(N / pi)

# Calculate the angle between each pair of adjacent points on outer circle
angle_step <- 2 * pi / 50

# Create a vector of angles around outer circle
angles <- seq(0, 2 * pi - angle_step, by = angle_step)

# compute outer circle outline coordinate positions
outer_circle <- data.frame(
  x_outer = outlineRadius * cos(angles),
  y_outer = outlineRadius * sin(angles)
)

# Create plots in list -------

# outer circle polygon geom
geom_outer_poly <- geom_polygon(
  data = outer_circle,
  mapping = aes(x = x_outer, y = y_outer),
  colour = "grey15",
  # fill = "transparent",
  fill = "grey90",
  linewidth = 2
)


plots_full <- list()
for (biome in names(circles)) {
  m <- 12 # plot margin in points

  plots_full[[biome]] <-
    centres[[biome]] %>%
    left_join(circles[[biome]], by = c(circle = "id")) %>%
    ggplot() +
    # outer circle
    geom_outer_poly +
    # inner circles
    geom_polygon(
      mapping = aes(x = x, y = y, fill = .data[[biome]], group = circle),
      colour = "grey35", linewidth = 0.2
    ) +
    scale_fill_brewer(palette = "Paired", guide = NULL) +
    coord_fixed(
      xlim = c(outlineRadius, -outlineRadius),
      ylim = c(outlineRadius, -outlineRadius),
      expand = TRUE, ratio = 1
    ) +
    theme_void(base_size = 15) +
    theme(
      plot.tag.position = c(0.95, 0.95),
      plot.tag = element_text(face = "bold"),
      plot.margin = margin(m, m, m, m)
    ) +
    labs(tag = biome)
}


# Save plots in patchwork grids -------------------------------------------

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

ggsave(
  plot = wrap_plots(plots_full, design = ABCXYZ_design), 
  filename = file.path(plotdir, "biome-circles/ABCXYZ.png"),
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


