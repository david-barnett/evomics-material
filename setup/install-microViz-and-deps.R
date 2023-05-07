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

# Install suggested dependencies
install.packages("ggraph") # for taxatree_plots()
install.packages("DT") # for tax_fix_interactive()
install.packages("corncob") # for example datasets and beta binomial models
