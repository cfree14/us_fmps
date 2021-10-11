
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

# Read data
data_orig <- readxl::read_excel(file.path(tabledir, "TableS2_fmps.xlsx"))

# Format data
data <- data_orig %>%
  janitor::clean_names("snake") %>%
  arrange(year_implemented)


# Build data
################################################################################

# Years
range(data$year_implemented)
years <- 1977:2016

# Build stats
stats <- purrr::map_df(years, function(x){

  # Build year stats
  ydata <- data %>%
    # Before year of interest
    filter(year_implemented<=x) %>%
    # Count
    group_by(council) %>%
    summarize(n_fmps=n()) %>%
    # Add year
    mutate(year=x) %>%
    # Arrange
    select(year, council, n_fmps)

})


# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=8),
                   axis.title.x=element_blank(),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key.size = unit(0.3, "cm"))

# Plot data
g <- ggplot(stats, aes(x=year, y=n_fmps, fill=council)) +
  geom_bar(stat="identity", color="grey30", lwd=0.1) +
  # Labels
  labs(x="", y="Number of FMPs") +
  scale_x_continuous(breaks=seq(1975, 2020, 5)) +
  # Legend
  scale_fill_discrete(name="Council") +
  # Theme
  theme_bw() + my_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_fmp_timeline.png"),
       width=6.5, height=2.5, units="in", dpi=600)








