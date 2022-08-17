# Clear workspace
rm(list = ls())
options(dplyr.summarise.inform=F)

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
outputdir <- "data/stock_hcr"

# Read data
data_orig <- read_csv(file.path(outputdir, "fmp_hcr.csv"))
buffer_info <- read_csv(file.path(outputdir, "fmp_buffer_info.csv"))
pgf_buffer_info <- read_csv(file.path(outputdir, "pacific-groundfish-pstar-buffer.csv"))

# figure theme
## ----------------------------------------------------------------------------

# Theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=9),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=9),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

## first make unique id for each stock
## -------------------------------------------

## hcr
data_hcr <- data_orig %>%
  select(council:env_linked) %>%
  mutate(id = paste(council_short, FMP_FEP, stock, common_name, sci_name, type, sep = "-"))

## buffer info
buffer_info_match <- buffer_info %>%
  mutate(id = paste(council_short, FMP_FEP, stock, common_name, sci_name, type, sep = "-"))

## pgf for matching
pgf_buffer_info_match <- pgf_buffer_info %>%
  mutate(stock = paste(AREA, STOCK_OR_COMPLEX, sep = "-")) %>%
  select(stock, p_star = PROBABILITY, abc_buffer = ABC_BUFFER_FRACTION)

## filter buffer info for pacific groundfish
buffer_info_pgf <- buffer_info_match %>%
  filter(FMP_FEP == "Pacific Coast Groundfish FMP") %>%
  select(-p_star, -abc_buffer) %>%
  left_join(pgf_buffer_info_match) %>%
  select(id, council_short, FMP_FEP, stock, common_name, sci_name, type, p_star,
         abc_buffer, acl_buffer, act_buffer, ref, notes) %>%
  mutate(acl_buffer = NA,
         act_buffer = NA,
         notes = "draft 2023 and 2024 groundfish harvest specifications under default and alternative harvest control rules")

## all buffer info
all_buffer_info <- buffer_info_match %>%
  filter(FMP_FEP != "Pacific Coast Groundfish FMP") %>%
  rbind(buffer_info_pgf) %>%
  select(id, p_star:ref)

## all info
hcr_buffer_data <- data_hcr %>%
  left_join(all_buffer_info) %>%
  # filter out Fish Resource of the Arctic FMP
  filter(FMP_FEP != 'Fish Resource of the Arctic FMP') %>%
  # Recode HCR type
  mutate(type=ifelse(is.na(type), "Unknown", type),
         type=stringr::str_to_sentence(type),
         type=ifelse(grepl("Ramped", type), "Threshold F", type),
         type=recode(type,
                     # "None"="Unknown",
                     "Constant f"="Constant F",
                     "Downward sloping"="Exempt",
                     "International exception"="Exempt",
                     "Stepped f"="Threshold F")) %>%
  # change to Catch prohibited
  mutate(type = ifelse(council_short == "SAFMC" & stock == "Nassau Grouper", "Catch prohibited", type)) %>%
  # Order HCR types
  mutate(type=factor(type, levels=c("Threshold F", "Constant F", "Constant escapement",
                                    "Constant catch", "Catch-based", "Catch prohibited", "None", "Exempt", "Unknown") %>% rev())) %>%
  # stock id
  mutate(stock_id = (paste(council_short, FMP_FEP, stock, type, sep = "-")))

## data with 506 (two speices in NP groundfish stocks have unique buffer info)
stock_db <- hcr_buffer_data %>%
  select(stock_id, type, biomass_limit, ramped_shape, env_linked, p_star:act_buffer) %>%
  unique() %>%
  group_by(stock_id) %>%
  mutate(n = n()) %>%
  ungroup()

write.csv(stock_db, file="data/stock_hcr/data_for_buffer_figure.csv", row.names=F)








