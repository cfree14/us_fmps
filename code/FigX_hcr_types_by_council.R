
# Clear workspace
rm(list = ls())
options(dplyr.summarise.inform=F)

# Setup
################################################################################

# Packages
library(tidyverse)
library(cowplot)

# Directories
plotdir <- "figures"
outputdir <- "data/stock_hcr"

# Read data
data_orig <- read_csv(file.path(outputdir, "fmp_hcr.csv"))

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



# Format data
################################################################################

fmps_n <- data_orig %>%
  # filter out Fish Resource of the Arctic FMP
  filter(FMP_FEP != 'Fish Resource of the Arctic FMP') %>%
  # FMP vs FEP
  mutate(plan_type = str_extract(FMP_FEP, "FMP"),
         plan_type = ifelse(is.na(plan_type), "FEP", plan_type))

plan_breakdown <- fmps_n %>%
  select(FMP_FEP, plan_type) %>%
  unique() %>%
  group_by(plan_type) %>%
  summarise(n = n()) %>%
  ungroup()

stocks_n <- fmps_n %>%
  # select unique council-stock combo
  select(council_short, FMP_FEP, stock, type) %>%
  unique() %>%
  mutate(id = paste(council_short, FMP_FEP, stock, sep = "-")) %>%
  group_by(id) %>%
  mutate(n = n()) %>%
  ungroup()

# make changes to correct duplicates
data_adj <- data_orig %>%
  left_join(stocks_n) %>%
  mutate(adj_type = ifelse(n == 2 & council_short %in% c("CFMC") & type == "catch prohibited", "constant catch",
                           ifelse(n == 2 & council_short %in% c("NOAA") & type == "catch prohibited", "constant F", type)),
         adj_stock = ifelse(id == "NPFMC-GOA groundfish FMP-Deepwater flatfish" & common_name == "Dover sole", paste(stock, common_name, sep = "-"),
                            ifelse(id == "NPFMC-GOA groundfish FMP-Deepwater flatfish" & common_name != "Dover sole", paste0(stock, "- except Dover sole"),
                                   ifelse(id == "NPFMC-GOA groundfish FMP-Demersal shelf rockfish" & common_name == "yelloweye rockfish", paste(stock, common_name, sep = "-"),
                                          ifelse(id == "NPFMC-GOA groundfish FMP-Demersal shelf rockfish" & common_name != "yelloweye rockfish", paste(stock, "- except yelloweye rockfish"),
                                                 ifelse(id == "NPFMC-Groundfish of the Bering Sea and Aleutian Islands FMP-Skates", common_name,
                                                        ifelse(id == "GMFMC-Gulf of Mexico Reef Fish FMP-Shallow water grouper" & common_name == "Red grouper", paste(stock, common_name, sep = "-"),
                                                               ifelse(id == "GMFMC-Gulf of Mexico Reef Fish FMP-Shallow water grouper" & common_name != "Red grouper", paste(stock, "- except Red grouper"), stock))))))))


stocks_n_adj <- data_adj %>%
  # select unique council-stock combo
  select(council_short, FMP_FEP, adj_stock, adj_type) %>%
  unique() %>%
  mutate(id = paste(council_short, FMP_FEP, adj_stock, sep = "-")) %>%
  group_by(id) %>%
  mutate(n = n()) %>%
  ungroup()


# Build data
data <- data_orig %>%
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
  mutate(type = ifelse(council_short == "SAFMC" & stock == "Goliath grouper", "Catch prohibited",
                       ifelse(council_short == "SAFMC" & stock == "Nassau Grouper", "Catch prohibited", type))) %>%
  # Order HCR types
  mutate(type=factor(type, levels=c("Threshold F", "Constant F", "Constant escapement",
                                    "Constant catch", "Catch-based", "Catch prohibited", "None", "Exempt", "Unknown") %>% rev())) %>%
  # fix typo
  mutate(council_short = ifelse(council_short == "GMFMC/MAFMC", "GMFMC/SAFMC", council_short)) %>%
  # select unique council-stock combo
  select(council_short, FMP_FEP, stock, type) %>%
  unique() %>%
  # Count HCR type by council
  count(council_short, type) %>%
  # Calculate proportion
  group_by(council_short) %>%
  mutate(prop=n/sum(n)) %>%
  ungroup() %>%
  # Order councils
  rename(council=council_short)

## create version of all councils combined
data_all <- data %>%
  group_by(type) %>%
  summarise(n = sum(n)) %>%
  ungroup() %>%
  mutate(prop = n / sum(n)) %>%
  mutate(council = "All councils") %>%
  select(council, type, n, prop) %>%
  rbind(data)  %>%
  mutate(council=factor(council,
                        levels=c("All councils", "NEFMC", "MAFMC", "SAFMC", "GMFMC", "CFMC", "PFMC", "NPFMC", "WPFMC",
                                 "NOAA", "NEFMC/MAFMC", "GMFMC/SAFMC", "PFMC/WPFMC") %>% rev())) %>%
  group_by(council) %>%
  mutate(total_n = sum(n)) %>%
  ungroup() %>%
  mutate(council_n = paste0(council, " (n = ", total_n, ")")) %>%
  mutate(council_n=factor(council_n,
                          levels=c("All councils (n = 503)", "NEFMC (n = 30)", "MAFMC (n = 13)", "SAFMC (n = 37)", "GMFMC (n = 23)",
                                   "CFMC (n = 51)", "PFMC (n = 228)", "NPFMC (n = 64)", "WPFMC (n = 9)",
                                 "NOAA (n = 23)", "NEFMC/MAFMC (n = 3)", "GMFMC/SAFMC (n = 6)", "PFMC/WPFMC (n = 16)") %>% rev()))

# Plot data
################################################################################

# Plot data
g <- ggplot(data_all, aes(x=prop, y=council, fill=type)) +
  geom_bar(stat="identity", col="grey30", lwd=0.1) +
  # Labels
  labs(x="Proportion of stocks", y="") +
  # Legend
  scale_fill_manual(name="HCR type",
                    values=c("grey90", "grey60", "grey30", "black", "darkorange2", "orange", "#AF7AC5", "#138D75", "#1B4F72"),
                    guide = guide_legend(reverse = TRUE)) +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_hcr_types_by_council.png"),
       width=6.5, height=3, units="in", dpi=600)

## version with n
g_n <- ggplot(data_all, aes(x=prop, y=council_n, fill=type)) +
  geom_bar(stat="identity", col="grey30", lwd=0.1) +
  # Labels
  labs(x="Proportion of stocks", y="") +
  # Legend
  scale_fill_manual(name="HCR type",
                    values=c("grey90", "grey60", "grey30", "black", "darkorange2", "orange", "#AF7AC5", "#138D75", "#1B4F72"),
                    guide = guide_legend(reverse = TRUE)) +
  # Theme
  theme_bw() + my_theme
g_n

# Export
ggsave(g_n, filename=file.path(plotdir, "FigX_hcr_types_by_council_n.png"),
       width=6.5, height=3, units="in", dpi=600)




## --------------------------------------------------------------------------
## v2 - remove unknown, exempt, catch prohibited & break down threshold type
## --------------------------------------------------------------------------


# Build data
thresh_data <- data_orig %>%
  # filter out Fish Resource of the Arctic FMP
  filter(FMP_FEP != 'Fish Resource of the Arctic FMP') %>%
  # Recode HCR type
  mutate(type=ifelse(is.na(type), "Unknown", type),
         type=stringr::str_to_sentence(type),
         type=recode(type,
                     "Ramped f with cutoff" = "Threshold F",
                     "Ramped f with biomass cutoff and environmental-link" = "Threshold F w/ envt. link",
                     "Constant f" = "Constant F",
                     "Downward sloping" = "Exempt",
                     "International exception" = "Exempt",
                     "Stepped f" = "Stepped F",
                     "Ramped/stepped f" = "Threshold and stepped F")) %>%
  # change to Catch prohibited
  mutate(type = ifelse(council_short == "SAFMC" & stock == "Goliath grouper", "Catch prohibited",
                       ifelse(council_short == "SAFMC" & stock == "Nassau Grouper", "Catch prohibited", type))) %>%
  # fix typo
  mutate(council_short = ifelse(council_short == "GMFMC/MAFMC", "GMFMC/SAFMC", council_short)) %>%
  # filter out except, unknown, catch prohibited, none
  filter(!type %in% c('Catch-based', 'Constant catch', 'Catch prohibited', 'Exempt', 'Unknown', 'None')) %>%
  # Order HCR types
  mutate(type=factor(type, levels=c("Threshold F", "Threshold F w/ envt. link", "Threshold and stepped F",
                                    "Stepped F", "Constant F", "Constant escapement") %>% rev())) %>%
  # select unique council-stock combo
  select(council_short, FMP_FEP, stock, type) %>%
  unique() %>%
  # Count HCR type by council
  count(council_short, type) %>%
  # Calculate proportion
  group_by(council_short) %>%
  mutate(prop=n/sum(n)) %>%
  ungroup() %>%
  # Order councils
  rename(council=council_short)


## df of CFMC and PFMC/WPFMC
zero_df <- tibble(council = c("CFMC", "PFMC/WPFMC"),
                  type = c("Constant F", "Constant F"),
                  n = c(0, 0),
                  prop = c(0, 0),
                  total_n = c(0, 0),
                  council_n = c("CFMC (n = 0)", "PFMC/WPFMC (n = 0)"))

## create version of all councils combined
thresh_data_all <- thresh_data %>%
  group_by(type) %>%
  summarise(n = sum(n)) %>%
  ungroup() %>%
  mutate(prop = n / sum(n)) %>%
  mutate(council = "All councils") %>%
  select(council, type, n, prop) %>%
  rbind(thresh_data)  %>%
  mutate(council=factor(council,
                        levels=c("All councils", "NEFMC", "MAFMC", "SAFMC", "GMFMC", "CFMC", "PFMC", "NPFMC", "WPFMC",
                                 "NOAA", "NEFMC/MAFMC", "GMFMC/SAFMC", "PFMC/WPFMC") %>% rev())) %>%
  group_by(council) %>%
  mutate(total_n = sum(n)) %>%
  ungroup() %>%
  mutate(council_n = paste0(council, " (n = ", total_n, ")")) %>%
  rbind(zero_df) %>%
  mutate(council_n=factor(council_n,
                          levels=c("All councils (n = 288)", "NEFMC (n = 27)", "MAFMC (n = 9)", "SAFMC (n = 14)", "GMFMC (n = 11)",
                                   "CFMC (n = 0)", "PFMC (n = 136)", "NPFMC (n = 54)", "WPFMC (n = 9)",
                                   "NOAA (n = 20)", "NEFMC/MAFMC (n = 3)", "GMFMC/SAFMC (n = 5)", "PFMC/WPFMC (n = 0)") %>% rev()))



## -----------------------------------------------------------------------
## plot v2

# Plot data
g2 <- ggplot(thresh_data_all, aes(x = prop, y = council_n, fill = type)) +
  geom_bar(stat = "identity", col = "grey30", lwd = 0.1) +
  # Labels
  labs(x="Proportion of stocks", y="") +
  # Legend
  scale_fill_manual(name="HCR type",
                    values=c("#AF7AC5", "#138D75", "#85C1E9", "#3498DB", "#2874A6", "#1B4F72"),
                    guide = guide_legend(reverse = TRUE)) +
  # Theme
  theme_bw() + my_theme
g2

# Export
ggsave(g2, filename=file.path(plotdir, "FigX_select_hcr_types_by_council.png"),
       width=6.5, height=2.5, units="in", dpi=600)

## plot togther
####################################

g_combine <- plot_grid(
  g_n,
  g2,
  align = 'h',
  labels = c("A", "B"),
  # # labels = 'AUTO',
  label_size = 10,
  hjust = -1,
  nrow = 2,
  rel_widths = c(1, 1)
)

# Export
ggsave(g_combine, filename=file.path(plotdir, "FigX_hcr_types_by_council_panel.png"),
       width=6.5, height=5.5, units="in", dpi=600)



