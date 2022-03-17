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

## total
total_hcr <- stock_hcr %>%
  group_by(type) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(council = "All") %>%
  select(council, type, n)

## by council
stock_hcr_summary <- stock_hcr %>%
  group_by(council_short, type) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  select(council = council_short, type, n) %>%
  rbind(total_hcr) %>%
  group_by(council) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  mutate(ratio = n / total) %>%
  mutate(type = ifelse(type == "ramped F with biomass cutoff and environmental-link", "ramped F with cutoff and environmental-link", type))

## factor
stock_hcr_summary$type <- factor(stock_hcr_summary$type, levels = c("ramped F with cutoff and environmental-link",
                                                                    "ramped F with cutoff",
                                                                    "ramped/stepped F",
                                                                    "stepped F",
                                                                    "downward sloping",
                                                                    "constant escapement",
                                                                    "constant F",
                                                                    "catch-based",
                                                                    "constant catch",
                                                                    "catch prohibited",
                                                                    "none",
                                                                    "international exception",
                                                                    "NA"))


hcr_colors <- c("ramped F with cutoff and environmental-link",
                "ramped F with cutoff",
                "ramped/stepped F",
                "stepped F",
                "downward sloping",
                "constant escapement",
                "constant F",
                "catch-based",
                "constant catch",
                "catch prohibited",
                "none",
                "international exception",
                "NA")


ggplot(stock_hcr_summary, aes(x = council, y = ratio, fill = type)) +
  geom_bar(position = "stack", stat = "identity") +
  coord_flip() +
  labs(y = NULL,
       x = NULL,
       fill = NULL) +
  theme_bw() +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(ncol = 3))




