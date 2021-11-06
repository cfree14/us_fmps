
# Clear workspace
rm(list = ls())
options(dplyr.summarise.inform=F)

# Setup
################################################################################

# Packages
library(lubridate)
library(tidyverse)

# Directories
plotdir <- "figures"
tabledir <- "tables"
inputdir <- "data/stock_smart/raw"
outputdir <- "data/stock_smart/processed"

# Read data
data_orig <- readxl::read_excel(file.path(inputdir, "Assessment_Summary_Data.xlsx"))

# Read FMP key
fmp_key_orig <- readxl::read_excel(file.path(tabledir, "TableS2_fmps.xlsx")) %>%
  janitor::clean_names("snake") %>%
  rename(fmp_short=fmp_short_name)


# Format data
################################################################################

# Format data
data <- data_orig %>%
  # Rename
  janitor::clean_names("snake") %>%
  rename(council=jurisdiction,
         sci_name=scientific_name,
         comm_name=common_name,
         itis_id=itis_taxon_serial_number) %>%
  # Arrange
  select(council, fmp, stock_name,
         regional_ecosystem, stock_area, comm_name, sci_name, itis_id,
         everything()) %>%
  # Add assessment "date"
  mutate(assess_date=paste(assessment_year, assessment_month, "01", sep="-") %>% ymd()) %>%
  # Reduce to most recent assessment
  group_by(council, fmp, stock_name) %>%
  arrange(council, fmp, stock_name, desc(assess_date)) %>%
  slice(1) %>%
  ungroup() %>%
  # Add FMP short
  left_join(fmp_key_orig %>% select(fmp, fmp_short), by="fmp") %>%
  # Arrange
  select(council, fmp, fmp_short, everything())


# Inspect
str(data)
table(data$council)

# Export data
################################################################################

# Export
saveRDS(data, file=file.path(outputdir, "2021_stock_smart_data.Rds"))


