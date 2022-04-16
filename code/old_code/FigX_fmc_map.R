
# Clear workspace
rm(list = ls())
options(dplyr.summarise.inform=F)

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
tabledir <- "tables"


# Plot data
################################################################################

# World
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
world  <- rnaturalearth::ne_countries(returnclass = "sf")

# EEZs
eezs <- marineregions::eezs_lr
eezs_usa <- eezs %>%
  filter(sovereign1=="United States")

# Setup theme
my_theme <-  theme(axis.ticks=element_blank(),
                   axis.text=element_blank(),
                   axis.title=element_blank(),
                   plot.title=element_blank(),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot
g <- ggplot() +
  # Plot EEZs
  geom_sf(data=eezs_usa, fill="darkred", color=NA) +
  # Plot world
  geom_sf(data=usa, fill="grey60", color="white", lwd=0.2) +
  geom_sf(data=world, fill="grey90", color="white", lwd=0.2) +
  # Crop
  coord_sf(xlim=c(-60,-180), ylim=c(-20, 75)) +
  # Theme
  theme_bw() + my_theme
#g

# Export
ggsave(g, filename=file.path(plotdir, "Fig1_fmc_map.png"),
       width=6.5, height=6.5, units="in", dpi=600)

