
# Clear workspace
rm(list = ls())
options(dplyr.summarise.inform=F)

# Setup
################################################################################

# Packages
library(lubridate)
library(tidyverse)

# Directories
plotdir <- "figures"
tabledir <- "tables"
inputdir <- "data/stock_smart/raw"
outputdir <- "data/stock_smart/processed"

# Read data
data_orig <- readRDS(file.path(outputdir, "2021_stock_smart_data.Rds"))


# Build data
################################################################################

# Build stats
stats <- data_orig %>%
  filter(!is.na(fmp_short)) %>%
  group_by(council, fmp_short, assessment_level) %>%
  summarize(n=n()) %>%
  ungroup() %>%
  group_by(council, fmp_short) %>%
  mutate(prop=n/sum(n))



# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=10),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position="bottom")

# Plot data
g <- ggplot(stats, aes(x=prop, y=fmp_short, fill=assessment_level)) +
  facet_grid(council~., scales="free", space="free") +
  geom_bar(stat="identity", color="grey30", lwd=0.2) +
  # Labels
  labs(x="Proportion", y="") +
  # Scale
  scale_fill_gradientn(name="Assessment level", colors=RColorBrewer::brewer.pal(9, "Blues"), na.value = "grey80") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme
g


# Export figure
ggsave(g, filename=file.path(plotdir, "stock_smart_assessment_level.png"),
       width=6.5, height=6.5, units="in", dpi=600)











