## clear workspace
rm(list = ls())
options(dplyr.summarise.inform=F)

## setup
################################################################################

## packages
library(tidyverse)
library(googlesheets4)

## directories
save_path <- "data/data_base"

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

## for now, go with pgf_df2
pgf_df3 <- pgf_df2 %>%
  rename(p_star = PROBABILITY,
         abc_buffer = ABC_BUFFER_FRACTION) %>%
  mutate(acl_buffer = NA,
         act_buffer = NA,
         buffer_notes = NA,
         buffer_ref = "GMT008 - Draft Annual Groundfish Harvest Specifications")

buffer_df_adj <- buffer_df2 %>%
  mutate(pgf = str_extract(id, "PFMC-Pacific Coast Groundfish FMP"),
         pgf = ifelse(is.na(pgf), "other", pgf)) %>%
  filter(pgf != "PFMC-Pacific Coast Groundfish FMP") %>%
  select(-pgf) %>%
  rbind(pgf_df3)

## join buffers with other information
full_db <- db_df2 %>%
  left_join(buffer_df_adj)


# Format data
################################################################################

# Build data
data <- full_db %>%
  # filter out Fish Resource of the Arctic FMP
  filter(FMP_FEP != 'Fish Resource of the Arctic FMP') %>%
  # make separate entries for Other rockfish-slope sub-group, NPFMC, North Pacific Fishery Management Council
  mutate(stock = ifelse(id == "NPFMC-GOA groundfish FMP-Other rockfish-slope sub-group-sharpchin rockfish-S. zacentrus",
                        'Other rockfish-slope sub-group (sharpchin rockfish)', stock)) %>%
  # Recode HCR type
  mutate(type_adj = ifelse(is.na(type), "Unknown", type),
         type_adj = stringr::str_to_sentence(type_adj),
         type_adj = ifelse(grepl("Ramped", type_adj), "Threshold F", type_adj),
         type_adj = recode(type_adj,
                     # "None"="Unknown",
                     "Constant f"="Constant F",
                     "Downward sloping"="Exempt",
                     "International exception"="Exempt",
                     "Stepped f"="Threshold F")) %>%
  # change to Catch prohibited
  mutate(type = ifelse(council_short == "SAFMC" & stock == "Nassau Grouper", "Catch prohibited", type)) %>%
  select(id, council, council_short, FMP_FEP, stock, common_name, sci_name, tier_level, type, type_adj, biomass_limit,
         ramped_shape, env_linked, notes, p_star, abc_buffer, acl_buffer, act_buffer, buffer_notes, buffer_ref)

# ## do individual sp. have different buffers?
# buffer_check <- data %>%
#   group_by(council_short, FMP_FEP, stock, type_adj) %>%
#   summarise(n_buffers = length(unique(abc_buffer))) %>%
#   ungroup()
#
# ## make separate entries for Other rockfish-slope sub-group, NPFMC, North Pacific Fishery Management Council

## db to share, v1
data_share1 <- data %>%
  select(council_short, FMP_FEP, stock, common_name, sci_name, hcr_type = type_adj, biomass_limit, p_star, abc_buffer, acl_buffer, act_buffer)

## save
write_csv(data_share1, file.path(save_path, "hcr_database_v1.csv"))

## database by stock
stock_data <- data %>%
  # select unique council-stock combo
  select(council, council_short, FMP_FEP, stock, type, type_adj, biomass_limit, p_star, abc_buffer, acl_buffer, act_buffer) %>%
  unique()

data_share2 <- stock_data %>%
  select(council_short, FMP_FEP, stock, hcr_type = type_adj, biomass_limit, p_star, abc_buffer, acl_buffer, act_buffer)

## save
write_csv(data_share2, file.path(save_path, "hcr_database_v2.csv"))



