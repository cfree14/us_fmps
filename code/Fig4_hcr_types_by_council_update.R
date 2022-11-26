
# Clear workspace
rm(list = ls())
options(dplyr.summarise.inform=F)

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
outputdir <- "database"

# Read data
data_orig <- read_csv(file.path(outputdir, "hcr_stock_db.csv"))

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



# Format data for figure
################################################################################

hcr_type_council <- data_orig %>%
  # Order HCR types
  mutate(type=factor(type, levels=c("Threshold F", "Constant F", "Constant escapement",
                                    "Constant catch", "Catch-based", "Catch prohibited", "None", "Exempt", "Unknown") %>% rev())) %>%
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
  rename(council=council_short) %>%
  # Recoe councils
  mutate(council=recode(council,
                        "NEFMC"="New England",
                        "MAFMC"="Mid-Atlantic",
                        "SAFMC"="South Atlantic",
                        "GMFMC"="Gulf of Mexico",
                        "CFMC"="Caribbean",
                        "PFMC"="Pacific",
                        "NPFMC"="North Pacific",
                        "WPFMC"="Western Pacific",
                        "NOAA"="Highly Migratory Species",
                        "NEFMC/MAFMC"="New England & Mid-Atlantic",
                        "GMFMC/SAFMC"="Gulf of Mexico & South Atlantic",
                        "PFMC/WPFMC"="Pacific & Western Pacific"))


## create version of all councils combined
data_all <- hcr_type_council %>%
  group_by(type) %>%
  summarise(n = sum(n)) %>%
  ungroup() %>%
  mutate(prop = n / sum(n)) %>%
  mutate(council = "All councils") %>%
  select(council, type, n, prop) %>%
  rbind(hcr_type_council)  %>%
  mutate(council=factor(council,
                        levels=c("All councils",
                                 "New England", "Mid-Atlantic", "South Atlantic", "Gulf of Mexico", "Caribbean",
                                 "Pacific", "North Pacific", "Western Pacific",
                                 "Highly Migratory Species", "New England & Mid-Atlantic",
                                 "Gulf of Mexico & South Atlantic", "Pacific & Western Pacific") %>% rev())) %>%
  group_by(council) %>%
  mutate(total_n = sum(n)) %>%
  ungroup() %>%
  mutate(council_n = paste0(council, " (n = ", total_n, ")")) %>%
  mutate(council_n=factor(council_n,
                          levels=c("All councils (n = 507)",
                                   "New England (n = 30)",
                                   "Mid-Atlantic (n = 13)",
                                   "South Atlantic (n = 37)",
                                   "Gulf of Mexico (n = 23)",
                                   "Caribbean (n = 52)",
                                   "Pacific (n = 228)",
                                   "North Pacific (n = 67)",
                                   "Western Pacific (n = 9)",
                                   "Highly Migratory Species (n = 23)",
                                   "New England & Mid-Atlantic (n = 3)",
                                   "Gulf of Mexico & South Atlantic (n = 6)",
                                   "Pacific & Western Pacific (n = 16)") %>% rev()))

# Plot data
################################################################################

# Plot data
g <- ggplot(data_all, aes(x=prop, y=council, fill=type)) +
  geom_bar(stat="identity", col="grey30", lwd=0.1) +
  # Labels
  labs(x="Percent of stocks", y="") +
  scale_x_continuous(labels=scales::percent) +
  # Legend
  scale_fill_manual(name="HCR type",
                    values=c("grey90", "grey60", "grey30", "black", "darkorange2", "orange", "#AF7AC5", "#138D75", "#1B4F72"),
                    guide = guide_legend(reverse = TRUE)) +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "Fig4_hcr_types_by_council.png"),
       width=6.5, height=3, units="in", dpi=600)
ggsave(g, filename=file.path(plotdir, "Fig4_hcr_types_by_council.pdf"),
       width=6.5, height=3, units="in")

## version with n
g_n <- ggplot(data_all, aes(x=prop, y=council_n, fill=type)) +
  geom_bar(stat="identity", col="grey30", lwd=0.1) +
  # Labels
  labs(x="Percent of stocks", y="") +
  scale_x_continuous(labels=scales::percent) +
  # Legend
  scale_fill_manual(name="HCR type",
                    values=c("grey90", "grey60", "grey30", "black", "darkorange2", "orange", "#AF7AC5", "#138D75", "#1B4F72"),
                    guide = guide_legend(reverse = TRUE)) +
  # Theme
  theme_bw() + my_theme
g_n

# Export
ggsave(g_n, filename=file.path(plotdir, "Fig4_hcr_types_by_council_n.png"),
       width=6.5, height=3, units="in", dpi=600)
ggsave(g_n, filename=file.path(plotdir, "Fig4_hcr_types_by_council_n.pdf"),
       width=6.5, height=3, units="in")



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
ggsave(g2, filename=file.path(plotdir, "Fig4_select_hcr_types_by_council.png"),
       width=6.5, height=2.5, units="in", dpi=600)
ggsave(g2, filename=file.path(plotdir, "Fig4_select_hcr_types_by_council.pdf"),
       width=6.5, height=2.5, units="in", dpi=600)
