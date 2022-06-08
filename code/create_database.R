## clear workspace
rm(list = ls())
options(dplyr.summarise.inform=F)

## setup
################################################################################

## packages
library(tidyverse)
library(googlesheets4)

## directories

## google sheet path
db_gsheet <- "https://docs.google.com/spreadsheets/d/1F_7i9cX2ComJtWUdXo8CoSR9Uj40VrGvCLFNCUzfKgs/edit#gid=1308093643"

## files
db_df     <- read_sheet(db_gsheet)
buffer_df <- read_sheet(db_gsheet, sheet = 2, col_types = "ccccccddddcc")
pgf_df    <- read_sheet(db_gsheet, sheet = 3)

## main database
db_df2 <- db_df %>%
  ## change tier_level not a list
  mutate(tier_level = as.character(tier_level)) %>%
  ## create a unique ID for matching %>%
  mutate(id = paste(council_short, FMP_FEP, stock, common_name, sci_name, sep = '-')) %>%
  ## remove p_star and buffer info
  select(id, council:env_linked, notes)

## buffer database
buffer_df2 <- buffer_df %>%
  ## create id
  mutate(id = paste(council_short, FMP_FEP, stock, common_name, sci_name, sep = '-')) %>%
  select(id, p_star:act_buffer, buffer_notes = notes, buffer_ref = ref)

## pacific groundfish
pgf_df2 <- pgf_df %>%
  mutate(council_short = "PFMC",
         FMP_FEP = "Pacific Coast Groundfish FMP",
         stock = paste(AREA, STOCK_OR_COMPLEX, sep = "-"),
         common_name = STOCK_OR_COMPLEX,
         sci_name = NA) %>%
  # rename(p_star = PROBABILITY,
  #        abc_buffer = ABC_BUFFER_FRACTION) %>%
  mutate(id = paste(council_short, FMP_FEP, stock, common_name, sci_name, sep = '-')) %>%
  select(id, PROBABILITY, ABC_BUFFER_FRACTION)

## compare PFMC groundfish
pg_comp <- pgf_df2 %>%
  left_join(buffer_df2)

