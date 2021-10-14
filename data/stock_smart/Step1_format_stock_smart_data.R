
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
inputdir <- "data/stock_smart/raw"
outputdir <- "data/stock_smart/processed"

# Read data
data_orig <- readxl::read_excel(file.path(inputdir, "Assessment_Summary_Data.xlsx"))


# Format data
################################################################################

# Format data
data <- data_orig %>%
  # Rename
  janitor::clean_names("snake")

# Inspect
str(data)


# Export data
################################################################################

# Export
saveRDS(data, file=file.path(outputdir, "2021_stock_smart_data.Rds"))
