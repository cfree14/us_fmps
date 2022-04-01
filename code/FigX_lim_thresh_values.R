
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
data_orig <- readxl::read_excel(file.path(outputdir, "threshold_limit_values.xlsx"))


# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=7),
                   axis.title.y=element_blank(),
                   plot.tag=element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data_orig, aes(x=bthresh_bbmsy, y=blim_bbmsy)) +
  geom_point() +
  # Labels
  labs(x=expression("B"["thresh"]*" (B/B"["MSY"]*")"),
       y=expression("B"["lim"]*" (B/B"["MSY"]*")")) +
  # Limits
  lims(x=c(0,1), y=c(0,1)) +
  # Theme
  theme_bw()
g


# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_abc_buffers_by_council.png"),
       width=6.5, height=2.5, units="in", dpi=600)


