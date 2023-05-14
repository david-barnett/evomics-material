library(tidyverse)
library(packcircles)
library(patchwork)

library(grid)
library(gridExtra)
library(gtable)
set.seed(1)

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

plots <- list()
for (biome in names(circles)) {
  m <- 12 # plot margin in points
  
  plots[[biome]] <-
    centres[[biome]] %>%
    left_join(circles[[biome]], by = c(circle = "id")) %>%
    ggplot() +
    # outer circle
    geom_polygon(
      data = outer_circle,
      mapping = aes(x = x_outer, y = y_outer),
      colour = "grey15",
      # fill = "transparent",
      fill = "grey90",
      linewidth = 2
    ) +
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
    labs(tag = biome) +
    theme_void(base_size = 15) +
    theme(
      plot.tag.position = c(0.95, 0.95), 
      plot.tag = element_text(face = "bold"),
      plot.margin = margin(m, m, m, m)
    )
}


# Save plots in patchwork grids -------------------------------------------

ABC <- wrap_plots(
  plots[1:3],
  design = "
  12
  #3
  "
)

ggsave(
  filename = here::here("slides/2023/microbiome-circles-ABC.png"),
  plot = ABC, device = "png", width = 4, height = 4, units = "in"
)

ABCXYZ <- wrap_plots(
  plots,
  design = "
  14
  25
  36"
)

ggsave(
  filename = here::here("slides/2023/microbiome-circles-ABCXYZ.png"),
  plot = ABCXYZ, device = "png", width = 4, height = 6, units = "in"
)

# Save a little 3-circle legend -------------------------------------------
circle_data <- data.frame(
  circle_id = 1:3,
  y_center = c(0.3, 0.5, 0.7)
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
  filename = here::here("slides/2023/three_circles_legend.png"), 
  plot = circle_plot, width = 0.5, height = 2, units = "in"
)


# Compute diversity, richness and evenness --------------------------------

# Function to compute the Shannon diversity index
shannon_diversity <- function(biome, exp = TRUE) {
  b <- droplevels(biome) # zeroes for some factor levels messes it up
  counts <- table(b)
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
diversity_table <- data.frame(
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
table_image <- tableGrob(
  d = diversity_table, rows = NULL, 
  theme = ttheme_minimal(base_size = 20, padding = unit(c(13, 10), "mm"))
)
# Create separator grobs
Vseparators <- replicate(
  n = ncol(table_image), 
  expr = segmentsGrob(x1 = unit(0, "npc"), gp=gpar(lty = 2)),
  simplify=FALSE
)
Hseparators <- replicate(
  n = nrow(table_image), 
  expr = segmentsGrob(y1 = unit(0, "npc"), gp = gpar(lty = 2)),
  simplify = FALSE
)

# Add separator grobs
table_image <- gtable_add_grob(
  x = table_image, grobs = Vseparators[1:2],
  t = 2, b = nrow(table_image), l = seq_len(2) + 2
)
table_image <- gtable_add_grob(
  x = table_image, grobs = Hseparators[1:2],
  t = seq_len(2) + 1, l = 2, r = ncol(table_image)
)

# Save the table as an image
png(
  filename = here::here("slides/2023/results_table.png"),
  width = 4, height = 2.5, units = "in", res = 300
)
grid::grid.draw(table_image)
dev.off()
