
# Clear workspace
rm(list = ls())
options(dplyr.summarise.inform=F)

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
tabledir <- "tables"
inputdir <- "data/hcrs/raw"
outputdir <- "data/hcrs/processed"

# Build data
################################################################################

# Files to merge
files2merge <- list.files(inputdir)
files2merge <- files2merge[files2merge!="PFMC_sardine.csv"]

# Loop through files and merge
data_orig <- purrr::map_df(files2merge, function(x){

  # Read data
  fdata <- read.csv(file.path(inputdir, x), as.is=T)

})

# Export data
saveRDS(data_orig, file=file.path(outputdir, "hcr_data.Rds"))
