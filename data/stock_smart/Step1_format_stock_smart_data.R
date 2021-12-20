
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
freeR::which_duplicated(fmp_key_orig$fmp_short)


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
  # Add assessment number (1= most recent)
  group_by(council, fmp, stock_name) %>%
  arrange(council, fmp, stock_name, desc(assess_date)) %>%
  mutate(assessment_number=1:n()) %>%
  ungroup() %>%
  # Format council
  mutate(council=gsub(" / ", "/", council)) %>%
  # Add council type
  mutate(council_type=ifelse(grepl("/", council) | council %in% c("IPHC", "Atlantic HMS"), "Multiple", "Single")) %>%
  # Add FMP short
  left_join(fmp_key_orig %>% select(fmp, fmp_short), by="fmp") %>%
  mutate(fmp_short=ifelse(is.na(fmp_short), fmp, fmp_short),
         fmp_short=recode(fmp_short,
                          "Groundfish of the Bering Sea and Aleutian Islands Management Area / Groundfish of the Gulf of Alaska"="BSAI Groundfish/GOM Groundfish",
                          "Snapper-Grouper Fishery of the South Atlantic Region / Reef Fish Resources of the Gulf of Mexico"="Snapper-Grouper/GOM Reef Fish",
                          "Species Managed Under International Agreement - IPHC"="IPHC Halibut",
                          "U.S. West Coast Fisheries for Highly Migratory Species / Pacific Pelagic Fisheries of the Western Pacific Region Ecosystem"="Pacific HMS/Pelagic Fisheries")) %>%
  # Arrange
  select(council, council_type, fmp, fmp_short, stock_name:fssi_stock,
         assessment_number, assessment_year, assessment_month, assess_date, everything()) %>%
  arrange(council, fmp, stock_name, assessment_number)

# Inspect
str(data)
freeR::complete(data)

# Inspect values
table(data$council)
table(data$fmp_short)


# Export data
################################################################################

# Export
saveRDS(data, file=file.path(outputdir, "2021_stock_smart_data.Rds"))


