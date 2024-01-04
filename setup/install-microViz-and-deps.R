# Copied from https://david-barnett.github.io/microViz/index.html
# on 07 May 2023

# Install sometimes pesky Bioc dependencies up front
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
BiocManager::install(c("phyloseq", "microbiome", "ComplexHeatmap"), update = FALSE)

# Install latest microViz
install.packages(
  "microViz",
  repos = c(davidbarnett = "https://david-barnett.r-universe.dev", getOption("repos"))
)

# Install suggested dependencies - necessary for workshop
install.packages("ggraph") # for taxatree_plots()
install.packages("DT") # for tax_fix_interactive()
install.packages("ggtext") # for rotated text on ord_plot PCA figures
install.packages("GUniFrac") # for (generalised) unifrac distances

# more useful stuff just to be safe
install.packages("tidyverse")
install.packages("patchwork")
install.packages("remotes") 

# install corncob from github for other example datasets and beta binomial models
remotes::install_github('statdivlab/corncob@v0.4.1')
