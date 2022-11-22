
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


# Build data
################################################################################

# Threshold fig
thresh1 <- tibble(x=c(0, 1, 2),
                  y=c(0, 0.8, 0.8))
thresh2 <- tibble(x=c(0, 1.3, 2),
                  y=c(0, 0.8, 0.8))
thresh3 <- tibble(x=c(0, 0.7, 2),
                  y=c(0, 0.8, 0.8))

# Limit fig
limit1 <- tibble(x=c(0.3, 1, 2),
                  y=c(0, 0.8, 0.8))
limit2 <- tibble(x=c(0.1, 1, 2),
                  y=c(0, 0.8, 0.8))
limit3 <- tibble(x=c(0.5, 1, 2),
                  y=c(0, 0.8, 0.8))

# Points
pts1 <- tibble(type=c("Base", "More precautionary", "Less precautionary"),
              x=c(0, 0, 0),
              y=c(0.8, 0.7, 0.9))
pts2 <- tibble(type=c("Base", "More precautionary", "Less precautionary"),
               x=c(1.0, 1.3, 0.7),
               y=c(0.8, 0.8, 0.8))
pts3 <- tibble(type=c("Base", "More precautionary", "Less precautionary"),
               x=c(0.3, 0.5, 0.1),
               y=c(0, 0, 0))

# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=5),
                   axis.title=element_text(size=6),
                   plot.title=element_text(size=7),
                   plot.tag=element_text(size=7),
                   plot.tag.position = c(0, 0.985),
                   legend.text=element_text(size=5),
                   legend.title=element_blank(),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.border = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))


# Uncertainty buffer
g1 <- ggplot() +
  # Plot lines
  geom_hline(yintercept=1) + # FOFL
  geom_hline(yintercept=0.8, linetype="dashed") + # FABC
  geom_hline(yintercept=c(0.7, 0.9), linetype="dotted", color=c("blue", "red")) + # alternatives
  # Plot points
  geom_point(data=pts1, mapping=aes(x=x, y=y, color=type), size=1) +
  # Labels
  labs(x="Biomass",
       y="Fishing mortality rate",
       title="Uncertainty buffer",
       tag="A") +
  # X-axis
  scale_x_continuous(lim=c(0, 2),
                     breaks=c(0, 1, 2),
                     labels=c("0", expression("B"["MSY"]), expression("B"["0"]))) +
  # Y-axis
  scale_y_continuous(lim=c(0, 1.2),
                     breaks=c(0, 0.8,  1),
                     labels=c("0", expression("F"["ABC"]), expression("F"["OFL"]))) +
  # Legend
  scale_color_manual(name="", values=c("black", "red", "blue")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.4, 0.2),
        legend.key.size = unit(0.2, "cm"))
g1

# Threshhold value
g2 <- ggplot() +
  # Plot lines
  geom_hline(yintercept=1) + # FOFL
  geom_line(data=thresh1, mapping=aes(x=x, y=y), linetype="dashed") +
  geom_line(data=thresh2, mapping=aes(x=x, y=y), linetype="dotted", color="blue") +
  geom_line(data=thresh3, mapping=aes(x=x, y=y), linetype="dotted", color="red") +
  # Plot points
  geom_point(data=pts2, mapping=aes(x=x, y=y, color=type), size=1) +
  # Labels
  labs(x="Biomass",
       y="Fishing mortality rate",
       title="Threshold value",
       tag="B") +
  # X-axis
  scale_x_continuous(lim=c(0, 2),
                     breaks=c(0, 1, 2),
                     labels=c("0", expression("B"["MSY"]), expression("B"["0"]))) +
  # Y-axis
  scale_y_continuous(lim=c(0, 1.2),
                     breaks=c(0, 0.8, 1),
                     labels=c("0",  expression("F"["ABC"]), expression("F"["OFL"]))) +
  # Legend
  scale_color_manual(name="", values=c("black", "red", "blue")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g2

# Limit value
g3 <- ggplot() +
  # Plot lines
  geom_hline(yintercept=1) +
  geom_line(data=limit1, mapping=aes(x=x, y=y), linetype="dashed") +
  geom_line(data=limit2, mapping=aes(x=x, y=y), linetype="dotted", color="red") +
  geom_line(data=limit3, mapping=aes(x=x, y=y), linetype="dotted", color="blue") +
  # Plot points
  geom_point(data=pts3, mapping=aes(x=x, y=y, color=type), size=1) +
  # Labels
  labs(x="Biomass",
       y="Fishing mortality rate",
       title="Limit value",
       tag="C") +
  # X-axis
  scale_x_continuous(lim=c(0, 2),
                     breaks=c(0, 0.3, 1, 2),
                     labels=c("0", expression("B"["lim"]), expression("B"["MSY"]), expression("B"["0"]))) +
  # Y-axis
  scale_y_continuous(lim=c(0, 1.2),
                     breaks=c(0, 0.8, 1),
                     labels=c("0",  expression("F"["ABC"]), expression("F"["OFL"]))) +
  # Legend
  scale_color_manual(name="", values=c("black", "red", "blue")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g3

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, ncol=3)
g

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig6_hcr_tradeoffs.png"),
       width=6, height=2, units="in", dpi=600)


