
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
datadir <- "data/status_of_stocks/processed"

# Read data
data_orig <- read.csv(file.path(datadir, "NOAA_SOS_data.csv"), as.is=T)


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

# Build stats
stats <- data_orig %>%
  filter(!is.na(overfished) & !is.na(fmp_short)) %>%
  # Number in each category
  group_by(council, fmp_short, overfished) %>%
  summarize(n=n()) %>%
  ungroup() %>%
  # Proportion in each category
  group_by(council, fmp_short) %>%
  mutate(prop=n/sum(n)) %>%
  ungroup() %>%
  # Fix status
  rename(status=overfished) %>%
  mutate(status=recode_factor(status,
                              "Unknown"="Unknown",
                              "Yes"="Overfished",
                              "No - Rebuilding"="Rebuilding",
                              "No"="Not overfished")) %>%
  # Make short council
  mutate(council_short=gsub("FMC", "", council))

# Plot data
g <- ggplot(stats, aes(x=prop, y=fmp_short, fill=status)) +
  facet_grid(council_short~., scales="free", space="free") +
  geom_bar(stat="identity", color="grey30", lwd=0.2) +
  # Labels
  labs(x="Proportion", y="") +
  # Scale
  scale_fill_manual(name="Status", values=rev(c("green", "yellow", "red", "grey80"))) +
  # Theme
  theme_bw() + my_theme
g

# Export figure
ggsave(g, filename=file.path(plotdir, "sos_fmp_overfished_status.png"),
       width=6.5, height=6.5, units="in", dpi=600)


# Plot data
################################################################################

# Build stats
stats <- data_orig %>%
  filter(!is.na(overfishing) & !is.na(fmp_short) & !grepl("Exception", overfishing)) %>%
  # Number in each category
  group_by(council, fmp_short, overfishing) %>%
  summarize(n=n()) %>%
  ungroup() %>%
  # Proportion in each category
  group_by(council, fmp_short) %>%
  mutate(prop=n/sum(n)) %>%
  ungroup() %>%
  # Fix status
  rename(status=overfishing) %>%
  mutate(status=recode_factor(status,
                              "Unknown"="Unknown",
                              "Yes"="Overfishing",
                              "No"="Not overfishing")) %>%
  # Make short council
  mutate(council_short=gsub("FMC", "", council))

# Plot data
g <- ggplot(stats, aes(x=prop, y=fmp_short, fill=status)) +
  facet_grid(council_short~., scales="free", space="free") +
  geom_bar(stat="identity", color="grey30", lwd=0.2) +
  # Labels
  labs(x="Proportion", y="") +
  # Scale
  scale_fill_manual(name="Status", values=rev(c("green", "red", "grey80"))) +
  # Theme
  theme_bw() + my_theme
g

# Export figure
ggsave(g, filename=file.path(plotdir, "sos_fmp_overfishing_status.png"),
       width=6.5, height=6.5, units="in", dpi=600)












