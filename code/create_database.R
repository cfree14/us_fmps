## clear workspace
rm(list = ls())
options(dplyr.summarise.inform=F)

## setup
################################################################################

## packages
library(tidyverse)
library(googlesheets4)

## directories
save_path <- "database"

## google sheet path
db_gsheet <- "https://docs.google.com/spreadsheets/d/1F_7i9cX2ComJtWUdXo8CoSR9Uj40VrGvCLFNCUzfKgs/edit#gid=1308093643"

## files
db_df     <- read_sheet(db_gsheet)
buffer_df <- read_sheet(db_gsheet, sheet = 2, col_types = "ccccccdddddcccd")
pgf_df    <- read_sheet(db_gsheet, sheet = 3)
goa_gf_df <- read_sheet(db_gsheet, sheet = 4, col_types = "ccdddddc")
bsai_gf_df <- read_sheet(db_gsheet, sheet = 5, col_types = "ccdddddc")

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
  select(id, p_star:act_buffer, year, buffer_notes = notes, buffer_ref = ref)

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
  select(id, PROBABILITY, ABC_BUFFER_FRACTION, year)

## compare PFMC groundfish
pg_comp <- pgf_df2 %>%
  left_join(buffer_df2)

## go with pgf_df2
pgf_df3 <- pgf_df2 %>%
  rename(p_star = PROBABILITY,
         abc_buffer = ABC_BUFFER_FRACTION) %>%
  mutate(acl_buffer = NA,
         act_buffer = NA,
         year = 2023,
         buffer_notes = NA,
         buffer_ref = "GMT008 - Draft Annual Groundfish Harvest Specifications")

buffer_df_adj <- buffer_df2 %>%
  mutate(pgf = str_extract(id, "PFMC-Pacific Coast Groundfish FMP"),
         pgf = ifelse(is.na(pgf), "other", pgf)) %>%
  filter(pgf != "PFMC-Pacific Coast Groundfish FMP") %>%
  select(-pgf) %>%
  rbind(pgf_df3)

## revise goa groundfish
## ---------------------------------

goa_gf_b <- buffer_df %>%
  mutate(id = paste(council_short, FMP_FEP, stock, common_name, sci_name, sep = '-')) %>%
  mutate(goa_gf = str_extract(id, 'NPFMC-GOA groundfish FMP'),
         goa_gf = ifelse(is.na(goa_gf), 'other', goa_gf)) %>%
  filter(goa_gf != 'other') %>%
  mutate(year = 2023) %>%
  select(id, stock, common_name, p_star, year, update_schedule, ref)

goa_hsp <- goa_gf_df %>%
  rename(stock = Species) %>%
  select(-Catch, - ref) %>%
  filter(!is.na(OFL),
         Area %in% c('GOA-wide', 'Total', 'AK Total')) %>%
  mutate(abc_buffer = 1 - (ABC / OFL),
         acl_buffer = 0,
         act_buffer = 1 - (TAC / ABC)) %>%
## recpde to join with original buffer table
  mutate(stock = str_to_sentence(stock)) %>%
  mutate(stock=recode(stock,
                     "Shallow-water flatfish" = "Shallow water flatfish",
                     "Deep-water flatfish" = "Deepwater flatfish",
                     "Rougheye and blackspotted rockfish" = "Rougheye & blackspotted rockfish",
                     "Octopuses" = "Octopus")) %>%
  filter(Year == 2023) %>%
  rename(year = Year) %>%
  select(buff_match = stock, year, abc_buffer, acl_buffer, act_buffer)

## update skates so that buffer match is on common name, update other rockfish for one match
## udpate nothern and southern rock sole to be shallow water flatfish
goa_gf_b_adj <- goa_gf_b %>%
  mutate(buff_match = ifelse(common_name == "Big skate", common_name,
                             ifelse(common_name == "longnose skates", "Longnose skate",
                                    ifelse(common_name == "other skates", "Other skates",
                                           ifelse(stock %in% c("Other rockfish-slope sub-group", "Other rockfish-demersal sub-group"), "Other rockfish",
                                                  ifelse(stock %in% c("Northern rock sole", "Southern rock sole"), "Shallow water flatfish", stock)))))) %>%
  left_join(goa_hsp) %>%
  mutate(buffer_notes = NA) %>%
  rename(buffer_ref = ref)

## join to buffer table
buffer_df_adj2 <- buffer_df_adj %>%
  mutate(goagf = str_extract(id, "NPFMC-GOA groundfish FMP"),
         goagf = ifelse(is.na(goagf), "other", goagf)) %>%
  filter(goagf != "NPFMC-GOA groundfish FMP") %>%
  select(-goagf) %>%
  rbind(goa_gf_b_adj %>% select(id, p_star, abc_buffer, acl_buffer, act_buffer, year, buffer_notes, buffer_ref))


## repeat for bsai
## ---------------------------------------

## area extact
bsai_gf_b <- buffer_df %>%
  mutate(id = paste(council_short, FMP_FEP, stock, common_name, sci_name, sep = '-')) %>%
  mutate(bsai_gf = str_extract(id, 'NPFMC-Groundfish of the Bering Sea and Aleutian Islands FMP'),
         bsaigf = ifelse(is.na(bsai_gf), 'other', bsai_gf)) %>%
  filter(bsai_gf != 'other') %>%
  mutate(year = 2023) %>%
  select(id, stock, common_name, p_star, year, update_schedule, ref)

## harvest specifications
bsai_hsp <- bsai_gf_df %>%
  rename(stock = Species) %>%
  select(-`Catch as of11062021`, -ref) %>%
  filter(!is.na(OFL),
         stock != "Total") %>%
  ## recode to join with original buffer table
  mutate(stock = str_to_sentence(stock),
         stock = ifelse(stock %in% c("Pollock", "Pacific cod"), paste(stock, Area, sep = "-"), stock)) %>%
  mutate(stock=recode(stock,
                      "Pollock-EBS" = "Eastern Bering Sea pollock",
                      "Pollock-AI" = "Aleutian Islands pollock",
                      "Pollock-Bogoslof" = "Bogoslof Island Pollock",
                      "Pacific cod-AI" = "Aleutian Islands Pacific cod",
                      "Pacific cod-BS" = "Eastern Bering Sea Pacific Cod",
                      "Greenland turbot" = "Greenland Turbot",
                      "Northern rock sole" = "Northern Rock sole",
                      "Blackspotted/rougheye rockfish" = "Rougheye & blackspotted rockfish",
                      "Octopuses" = "Octopus")) %>%
  mutate(abc_buffer = 1 - (ABC / OFL),
         acl_buffer = 0,
         act_buffer = 1 - (TAC / ABC)) %>%
  filter(year == 2023) %>%
  select(stock, year, abc_buffer, acl_buffer, act_buffer)

## join harvest sp. values to df
bsai_gf_b_adj <- bsai_gf_b %>%
  left_join(bsai_hsp) %>%
  mutate(buffer_notes = NA) %>%
  rename(buffer_ref = ref)

## join to buffer table
buffer_df_adj3 <- buffer_df_adj2 %>%
  mutate(bsaigf = str_extract(id, "NPFMC-Groundfish of the Bering Sea and Aleutian Islands FMP"),
         bsaigf = ifelse(is.na(bsaigf), "other", bsaigf)) %>%
  filter(bsaigf != "NPFMC-Groundfish of the Bering Sea and Aleutian Islands FMP") %>%
  select(-bsaigf) %>%
  rbind(bsai_gf_b_adj %>% select(id, p_star, abc_buffer, acl_buffer, act_buffer, year, buffer_notes, buffer_ref))







## join buffers with other information
## ---------------------------------------

full_db <- db_df2 %>%
  left_join(buffer_df_adj3)




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
         ramped_shape, env_linked, notes, p_star, abc_buffer, acl_buffer, act_buffer, buffer_year = year, buffer_notes, buffer_ref)

# ## do individual sp. have different buffers?
buffer_check <- data %>%
  group_by(council_short, FMP_FEP, stock, type_adj) %>%
  summarise(n_buffers = length(unique(abc_buffer))) %>%
  ungroup()

## stock combos
stocks_n <- data %>%
  # select unique council-stock combo
  select(council_short, FMP_FEP, stock, type) %>%
  unique() %>%
  mutate(id2 = paste(council_short, FMP_FEP, stock, sep = "-")) %>%
  group_by(id2) %>%
  mutate(n = n()) %>%
  ungroup()

# make changes to correct duplicates
data_adj <- data %>%
  mutate(id2 = paste(council_short, FMP_FEP, stock, sep = "-")) %>%
  left_join(stocks_n) %>%
  mutate(adj_stock = ifelse(n == 2 & council_short %in% c("CFMC") & type == "catch prohibited", paste0(stock, " - prohibited_sp"),
                            ifelse(n == 2 & council_short %in% c("NOAA") & type == "catch prohibited", paste0(stock, " - prohibited_sp"), stock)),
         adj_stock = ifelse(id2 == "NPFMC-GOA groundfish FMP-Deepwater flatfish" & common_name == "Dover sole", paste(stock, common_name, sep = "-"),
                            ifelse(id2 == "NPFMC-GOA groundfish FMP-Deepwater flatfish" & common_name != "Dover sole", paste0(stock, "- except Dover sole"),
                                   ifelse(id2 == "NPFMC-GOA groundfish FMP-Demersal shelf rockfish" & common_name == "yelloweye rockfish", paste(stock, common_name, sep = "-"),
                                          ifelse(id2 == "NPFMC-GOA groundfish FMP-Demersal shelf rockfish" & common_name != "yelloweye rockfish", paste(stock, "- except yelloweye rockfish"),
                                                 ifelse(id2 == "NPFMC-Groundfish of the Bering Sea and Aleutian Islands FMP-Skates", common_name,
                                                        ifelse(id2 == "GMFMC-Gulf of Mexico Reef Fish FMP-Shallow water grouper" & common_name == "Red grouper", paste(stock, common_name, sep = "-"),
                                                               ifelse(id2 == "GMFMC-Gulf of Mexico Reef Fish FMP-Shallow water grouper" & common_name != "Red grouper", paste(stock, "- except Red grouper"), adj_stock))))))))

## check again
stocks_n_adj <- data_adj %>%
  # filter out Fish Resource of the Arctic FMP
  filter(FMP_FEP != 'Fish Resource of the Arctic FMP') %>%
  # select unique council-stock combo
  select(council_short, FMP_FEP, adj_stock, type_adj) %>%
  unique() %>%
  mutate(id = paste(council_short, FMP_FEP, adj_stock, sep = "-")) %>%
  group_by(id) %>%
  mutate(n = n()) %>%
  ungroup()


## create "stock level" data base
stock_data <- data_adj %>%
  mutate(adj_stock = ifelse(FMP_FEP == "GOA groundfish FMP" & stock == "Skates", common_name, adj_stock)) %>%
  select(council, council_short, FMP_FEP, stock = adj_stock, type = type_adj, biomass_limit, ramped_shape, env_linked, p_star:buffer_year) %>%
  unique()

stocks_n_all <- stock_data %>%
  mutate(id = paste(council_short, FMP_FEP, stock, sep = "-")) %>%
  group_by(id) %>%
  mutate(n = n()) %>%
  ungroup()

## save stock-level database
write_csv(stock_data, file.path(save_path, "hcr_stock_db.csv"))




## for review documents
## -----------------------

# ## db to share, v1
# data_share1 <- data %>%
#   select(council_short, FMP_FEP, stock, common_name, sci_name, hcr_type = type_adj, biomass_limit, p_star, abc_buffer, acl_buffer, act_buffer)
#
# ## save
# write_csv(data_share1, file.path(save_path, "hcr_database_v1.csv"))
#
# ## database by stock
# stock_data <- data %>%
#   # select unique council-stock combo
#   select(council, council_short, FMP_FEP, stock, type, type_adj, biomass_limit, p_star, abc_buffer, acl_buffer, act_buffer) %>%
#   unique()
#
# data_share2 <- stock_data %>%
#   select(council_short, FMP_FEP, stock, hcr_type = type_adj, biomass_limit, p_star, abc_buffer, acl_buffer, act_buffer)
#
# ## save
# write_csv(data_share2, file.path(save_path, "hcr_database_v2.csv"))



