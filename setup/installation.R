# change to the focal option, I'm running bionic (you can also put this in project .Rprofile)
options(repos = c(REPO_NAME = "https://packagemanager.rstudio.com/cran/__linux__/bionic/2022-05-18"))
# options(repos = c(REPO_NAME = "https://packagemanager.rstudio.com/cran/__linux__/focal/2022-05-18"))
options(BioC_mirror = "https://packagemanager.rstudio.com/bioconductor") # also in .Rprofile
# settings from here 
# https://packagemanager.rstudio.com/client/#/repos/2/overview 

# You can try this renv function first but I suspect it will fail somewhere.
renv::hydrate()
# renv will install new packages (listed in renv.lock) into renv/library/4.2... in this project
# so probably this also helps with my preference to be able to do a last minute update of microViz
# it should link to the existing package libraries on the system, 
# including other repos and not make duplicates of what is already there.
# But if it does get annoying you can turn it off with renv::deactivate() and then delete the renv folder 
# https://rstudio.github.io/renv/index.html

# an eclectic set of install instructions to try and break up process a bit
# before going for the final install of microViz
install.packages("remotes")
install.packages("BiocManager")
install.packages("devtools") 
install.packages("tidyverse")
install.packages("shiny")
install.packages("ggtext")
install.packages("patchwork")
# https://github.com/igraph/rigraph/issues/490 # needs sys deps, see shell script
install.packages("igraph") 
install.packages("RcppEigen") 
install.packages('bioc::Rhdf5lib') 
install.packages('bioc::ComplexHeatmap') 
install.packages('bioc::phyloseq')
install.packages('bioc::GUniFrac')
install.packages('bioc::microbiome')
install.packages('bioc::dada2')

remotes::install_github("david-barnett/microViz")
