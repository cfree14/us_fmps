
# Clear workspace
rm(list = ls())
options(dplyr.summarise.inform=F)

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
tabledir <- "tables"
plotdir <- "data/status_of_stocks/figures"
inputdir <- "data/status_of_stocks/raw"
outputdir <- "data/status_of_stocks/processed"

# Read FMP key
fmp_key_orig <- readxl::read_excel(file.path(tabledir, "TableS2_fmps.xlsx")) %>%
  janitor::clean_names("snake") %>%
  rename(fmp_short=fmp_short_name)


# Table A
################################################################################

# Read data
table_a_orig <- readxl::read_excel(file.path(inputdir, "tabula-2020 SOS Stock Status Tables.xlsx"), sheet="Table A")

# Format data
table_a <- table_a_orig %>%
  # Rename
  setNames(c("council", "fmp", "stock", "overfishing", "overfished", "overfished_close", "mgmt_action", "rebuilding_progress", "bbmsy", "points")) %>%
  # Remove header rows
  filter(council!="Jurisdiction") %>%
  # Fix council
  mutate(council=gsub(" /\r", "/", council)) %>%
  # Format FMP
  mutate(fmp=gsub("\r", " ", fmp)) %>%
  # Format overfished
  mutate(overfished=gsub("\r", " ", overfished)) %>%
  # Format management action
  mutate(mgmt_action=gsub("\r", " ", mgmt_action)) %>%
  # Format rebuilding progress
  mutate(rebuilding_progress=gsub("\r", " ", rebuilding_progress),
         rebuilding_progress=gsub("- ", "-", rebuilding_progress),
         rebuilding_progress=gsub(" year", "-year", rebuilding_progress)) %>%
  # Format B/BMSY
  mutate(bbmsy=recode(bbmsy,
                      "not\restimated"="",
                      ">1"="",
                      "<<1"="",
                      "0.06-0.09"="",
                      "1.55\r1.11"="",
                      "1.63\r1.23"="",
                      "1.31\r1.27"="",
                      "0.43-0.64"=""),
         bbmsy=as.numeric(bbmsy)) %>%
  # Format point
  mutate(points=as.numeric(points)) %>%
  # Add type
  mutate(type="FSSI")

# Inspect
str(table_a)
table(table_a$council)
table(table_a$fmp)
table(table_a$overfishing)
table(table_a$overfished)
table(table_a$overfished_close)
table(table_a$mgmt_action)
table(table_a$rebuilding_progress)
range(table_a$bbmsy, na.rm=T)



# Table B
################################################################################

# Read data
table_b_orig <- readxl::read_excel(file.path(inputdir, "tabula-2020 SOS Stock Status Tables.xlsx"), sheet="Table B")

# Format data
table_b <- table_b_orig %>%
  # Rename
  setNames(c("council", "fmp", "stock", "overfishing", "overfished",
             "overfished_close", "mgmt_action", "rebuilding_progress")) %>%
  # Remove header rows
  filter(council!="Jurisdiction") %>%
  # Fix council
  mutate(council=gsub(" /\r", "/", council),
         council=gsub("\r", " ", council)) %>%
  # Format FMP
  mutate(fmp=gsub("\r", " ", fmp)) %>%
  # Format overfishing
  mutate(overfishing=gsub("\r", " ", overfishing)) %>%
  # Format overfished
  mutate(overfished=gsub("\r", " ", overfished)) %>%
  # Format management action
  mutate(mgmt_action=gsub("\r", " ", mgmt_action)) %>%
  # Format rebuilding progress
  mutate(rebuilding_progress=gsub("\r", " ", rebuilding_progress),
         rebuilding_progress=gsub("- ", "-", rebuilding_progress),
         rebuilding_progress=gsub(" year", "-year", rebuilding_progress)) %>%
  # Add type
  mutate(type="Non-FSSI")

# Inspect
str(table_b)
table(table_b$council)
table(table_b$fmp)
table(table_b$overfishing)
table(table_b$overfished)
table(table_b$overfished_close)
table(table_b$mgmt_action)
table(table_b$rebuilding_progress)


# Merge data
################################################################################

# Merge data
data <- bind_rows(table_a, table_b) %>%
  # Arrange
  select(type, everything()) %>%
  # Remove asterisk from stock name (denotes endnote)
  mutate(stock=gsub("\\*", "", stock) %>% stringr::str_trim(.)) %>%
  # Fix bizarre spaces
  mutate(stock=gsub("\r", " ", stock)) %>%
  # Flag stock complexes
  mutate(stock_type=ifelse(grepl("Complex", stock), "stock complex", "stock")) %>%
  # Recode stock complexes
  mutate(stock_orig=stock,
         stock=gsub("Bering Sea / Aleutian Islands ", "Bering Sea / Aleutian Islands - ", stock),
         stock=gsub("Gulf of Alaska ", "Gulf of Alaska - ", stock),
         stock=gsub("Caribbean ", "Caribbean - ", stock),
         stock=gsub("Puerto Rico ", "Puerto Rico - ", stock),
         stock=gsub("St. Croix ", "St. Croix - ", stock),
         stock=gsub("St. Thomas / St. John ", "St. Thomas / St. John - ", stock),
         stock=gsub("Gulf of Mexico ", "Gulf of Mexico - ", stock),
         stock=gsub("South Atlantic ", "South Atlantic - ", stock),
         stock=gsub("Northwestern Hawaiian Islands ", "Northwestern Hawaiian Islands - ", stock),
         stock=gsub("Pacific Remote Island Areas ", "Pacific Remote Island Areas - ", stock)) %>%
  # Split stock area and species
  tidyr::separate(stock, into=c("comm_name", "area"), sep=" - ", remove=F) %>%
  # Add FMP short code
  left_join(fmp_key_orig %>% select(fmp, fmp_short), by="fmp") %>%
  # Arrange
  select(type, council, fmp, fmp_short, stock_orig, stock, stock_type, comm_name, area, everything())

# Stocks without areas
data$stock_orig[is.na(data$area)]

# Inspect
freeR::complete(data)
table(data$type)
table(data$council)
table(data$fmp)

# Build FMP key
fmp_key <- data %>%
  group_by(council, fmp, fmp_short) %>%
  summarize(nstocks=n())

# Export FMP key
write.csv(fmp_key, file=file.path(tabledir, "TableS2_fmps.csv"), row.names=F)

# Export dats
write.csv(data, file=file.path(outputdir, "NOAA_SOS_data.csv"), row.names=F)


# Plot data
################################################################################

# Sample size
stats <- data %>%
  # Count
  group_by(council, fmp_short) %>%
  summarize(nstocks=n()) %>%
  ungroup() %>%
  # Arrange
  arrange(council, nstocks)

# Plot
ggplot(stats, aes(x=nstocks, y=fmp_short)) +
  facet_grid(council~., scales="free_y", space="free_y") +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Number of stocks", y="") +
  # Theme
  theme_bw()







