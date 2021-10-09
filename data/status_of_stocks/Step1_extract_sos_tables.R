
# Clear workspace
rm(list = ls())
options(dplyr.summarise.inform=F)

# Setup
################################################################################

# Packages
library(tabulizer)
library(tidyverse)

# Directories
plotdir <- "figures"
tabledir <- "tables"
inputdir <- "data/status_of_stocks/raw"
outputdir <- "data/status_of_stocks/processed"


# Table A
################################################################################

# Read data
table_a_orig <- readxl::read_excel(file.path(inputdir, "tabula-2020 SOS Stock Status Tables.xlsx"), sheet=1)

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
  mutate(points=as.numeric(points))


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

# Plot data
################################################################################

# Sample size
stats <- table_a %>%
  # Count
  group_by(council, fmp) %>%
  summarize(nstocks=n()) %>%
  ungroup() %>%
  # Arrange
  arrange(council, nstocks)

# Plot
ggplot(stats, aes(x=nstocks, y=factor(fmp, levels=stats$fmp))) +
  facet_grid(council~., scales="free_y", space="free_y") +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Number of stocks", y="") +
  # Theme
  theme_bw()







