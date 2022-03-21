
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


# Format data
################################################################################

# Build data
data <- data_orig %>%
  # Recode HCR type
  mutate(type=ifelse(is.na(type), "Unknown", type),
         type=stringr::str_to_sentence(type),
         type=ifelse(grepl("Ramped", type), "Threshold F", type),
         type=recode(type,
                     "None"="Unknown",
                     "Constant f"="Constant F",
                     "Downward sloping"="Exempt",
                     "International exception"="Exempt",
                     "Stepped f"="Threshold F")) %>%
  # Order HCR types
  mutate(type=factor(type, levels=c("Threshold F", "Constant F", "Constant escapement", "Constant catch",
                                    "Catch-based", "Catch prohibited", "Exempt", "Unknown") %>% rev())) %>%
  # Count HCR type by council
  count(council_short, type) %>%
  # Calculate proportion
  group_by(council_short) %>%
  mutate(prop=n/sum(n)) %>%
  ungroup() %>%
  # Order councils
  rename(council=council_short) %>%
  mutate(council=factor(council,
                        levels=c("NEFMC", "MAFMC", "SAFMC", "GMFMC", "CFMC", "PFMC", "NPFMC", "WPFMC",
                                 "NOAA", "NEFMC/MAFMC", "GMFMC/MAFMC", "PFMC/WPFMC") %>% rev()))


# Plot data
################################################################################

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

# Plot data
g <- ggplot(data, aes(x=prop, y=council, fill=type)) +
  geom_bar(stat="identity", col="grey30", lwd=0.1) +
  # Labels
  labs(x="Proportion of stocks", y="") +
  # Legend
  scale_fill_manual(name="HCR type",
                    values=c("grey90", "grey60", "black", "orange", "darkorange2", "violet", "lightgreen", "lightblue"),
                    guide = guide_legend(reverse = TRUE)) +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_hcr_types_by_council.png"),
       width=6.5, height=2.5, units="in", dpi=600)


