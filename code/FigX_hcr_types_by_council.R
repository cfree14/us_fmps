
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
  # Order HCR types
  mutate(type=factor(type, levels=c("Threshold F", "Constant F", "Constant escapement", "Catch-based",
                                    "Constant catch", "Catch prohibited", "None", "Exempt", "Unknown") %>% rev())) %>%
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
                                 "NOAA", "NEFMC/MAFMC", "GMFMC/MAFMC", "PFMC/WPFMC") %>% rev())) %>%
  group_by(council) %>%
  mutate(total_n = sum(n)) %>%
  ungroup() %>%
  mutate(council_n = paste0(council, " (n = ", total_n, ")")) %>%
  mutate(council_n=factor(council_n,
                          levels=c("All councils (n = 503)", "NEFMC (n = 31)", "MAFMC (n = 12)", "SAFMC (n = 37)", "GMFMC (n = 23)",
                                   "CFMC (n = 51)", "PFMC (n = 228)", "NPFMC (n = 64)", "WPFMC (n = 9)",
                                 "NOAA (n = 23)", "NEFMC/MAFMC (n = 3)", "GMFMC/MAFMC (n = 6)", "PFMC/WPFMC (n = 16)") %>% rev()))

# Plot data
################################################################################

# Plot data
g <- ggplot(data_all, aes(x=prop, y=council, fill=type)) +
  geom_bar(stat="identity", col="grey30", lwd=0.1) +
  # Labels
  labs(x="Proportion of stocks", y="") +
  # Legend
  scale_fill_manual(name="HCR type",
                    values=c("grey90", "grey60", "grey30", "black", "orange", "darkorange2", "#AF7AC5", "#138D75", "#1B4F72"),
                    guide = guide_legend(reverse = TRUE)) +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_hcr_types_by_council.png"),
       width=6.5, height=2.5, units="in", dpi=600)

## version with n
g_n <- ggplot(data_all, aes(x=prop, y=council_n, fill=type)) +
  geom_bar(stat="identity", col="grey30", lwd=0.1) +
  # Labels
  labs(x="Proportion of stocks", y="") +
  # Legend
  scale_fill_manual(name="HCR type",
                    values=c("grey90", "grey60", "grey30", "black", "orange", "darkorange2", "#AF7AC5", "#138D75", "#1B4F72"),
                    guide = guide_legend(reverse = TRUE)) +
  # Theme
  theme_bw() + my_theme
g_n

# Export
ggsave(g_n, filename=file.path(plotdir, "FigX_hcr_types_by_council_n.png"),
       width=6.5, height=2.5, units="in", dpi=600)




## --------------------------------------------------------------------------
## v2 - remove unknown, exempt, catch prohibited & break down threshold type
## --------------------------------------------------------------------------

# Build data
thresh_data <- data_orig %>%
  # filter out Fish Resource of the Arctic FMP
  filter(FMP_FEP != 'Fish Resource of the Arctic FMP') %>%
  # select unique council-stock combo
  select(council_short, FMP_FEP, stock, type) %>%
  unique() %>%
  # Recode HCR type
  mutate(type=ifelse(is.na(type), "Unknown", type),
         type=stringr::str_to_sentence(type),
         type=recode(type,
                     "Ramped f with cutoff"="Threshold F",
                     "Ramped f with biomass cutoff and environmental-link"="Threshold F/envt. link",
                     "Constant f"="Constant F",
                     "Downward sloping"="Exempt",
                     "International exception"="Exempt",
                     "Stepped f"="Stepped F",
                     "Ramped/stepped f"="Threshold/stepped F")) %>%
  # filter out except, unknown, catch prohibited, none
  filter(!type %in% c('Catch prohibited', 'Exempt', 'Unknown', 'None')) %>%
  # Order HCR types
  mutate(type=factor(type, levels=c("Threshold F", "Threshold F/envt. link", "Threshold/stepped F",
                                    "Stepped F", "Constant F", "Constant escapement",
                                    "Catch-based", "Constant catch") %>% rev())) %>%
  # Count HCR type by council
  count(council_short, type) %>%
  # Calculate proportion
  group_by(council_short) %>%
  mutate(prop=n/sum(n)) %>%
  ungroup() %>%
  # Order councils
  rename(council=council_short)

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
                                 "NOAA", "NEFMC/MAFMC", "GMFMC/MAFMC", "PFMC/WPFMC") %>% rev()))

## -----------------------------------------------------------------------
## plot v2

# Plot data
g2 <- ggplot(thresh_data_all, aes(x=prop, y=council, fill=type)) +
  geom_bar(stat="identity", col="grey30", lwd=0.1) +
  # Labels
  labs(x="Proportion of stocks", y="") +
  # Legend
  scale_fill_manual(name="HCR type",
                    values=c("orange", "darkorange2", "#AF7AC5", "#138D75", "#85C1E9", "#3498DB", "#2874A6", "#1B4F72"),
                    guide = guide_legend(reverse = TRUE)) +
  # Theme
  theme_bw() + my_theme
g2

# Export
ggsave(g2, filename=file.path(plotdir, "FigX_select_hcr_types_by_council.png"),
       width=6.5, height=2.5, units="in", dpi=600)

