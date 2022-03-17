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
sp_hcr <- read_csv(file.path(outputdir, "fmp_hcr.csv")) %>%
  mutate(council_fmp_stock = paste(council_short, FMP_FEP, stock, sep = "-")) %>%
  ## filter out Fish Resource of the Arctic
  filter(FMP_FEP != "Fish Resource of the Arctic FMP")

stock_hcr <- sp_hcr %>%
  select(council, council_short, FMP_FEP, council_fmp_stock, type) %>%
  unique()

## by council
stock_hcr_summary <- stock_hcr %>%
  mutate(type = ifelse(type == "ramped F with biomass cutoff and environmental-link", "ramped F with cutoff and environmental-link", type)) %>%
  mutate(adj_type = ifelse(type %in% c("ramped F with cutoff and environmental-link",
                                       "ramped F with cutoff",
                                       "ramped/stepped F",
                                       "stepped F"), "threshold F",
                           ifelse(type %in% c("downward sloping", "international exception"), "exception", type))) %>%
  group_by(council_short, adj_type) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  select(council = council_short, adj_type, n)

## total
total_hcr <- stock_hcr_summary %>%
  group_by(adj_type) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(council = "All") %>%
  select(council, adj_type, n)

## bind
stock_hcr_summary <- stock_hcr_summary %>%
  rbind(total_hcr) %>%
  group_by(council) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  mutate(ratio = n / total)


## factor
stock_hcr_summary$adj_type <- factor(stock_hcr_summary$adj_type, levels =
                                       c("NA",
                                         "exception",
                                         "none",
                                         "catch prohibited",
                                         "constant catch",
                                         "catch-based",
                                         "constant F",
                                         "constant escapement",
                                         "threshold F"))


hcr_colors <- c("threshold F",
                "constant escapement",
                "constant F",
                "catch-based",
                "constant catch",
                "catch prohibited",
                "none",
                "exception",
                "NA")


ggplot(stock_hcr_summary, aes(x = council, y = ratio, fill = adj_type)) +
  geom_bar(position = "stack", stat = "identity", color = "grey30") +
  coord_flip() +
  labs(y = NULL,
       x = NULL,
       fill = NULL) +
  theme_bw() +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(ncol = 3))




