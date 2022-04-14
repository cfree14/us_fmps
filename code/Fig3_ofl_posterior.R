
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
inputdir <- "data/stock_smart/raw"
outputdir <- "data/stock_smart/processed"


# Set parameters
################################################################################

# Parameters
# CV = sqrt(exp(sdlog^2)-1)
# sdlog = log(CV^2+1)
# mean = exp(meanlog+sdlog^2/2)
# meanlog = log(mean) - sdlog^2/2
ofl_avg <- 1
ofl_cv <- 0.4
ofl_sdlog <- log(ofl_cv^2+1)
ofl_meanlog <- log(ofl_avg) - ofl_sdlog^2/2

# Extract useful values
ofl_99 <- qlnorm(p=0.9999, meanlog=ofl_meanlog, sdlog=ofl_sdlog)
ofl_50 <- qlnorm(p=0.5, meanlog=ofl_meanlog, sdlog=ofl_sdlog)
ofl_50_dens <- dlnorm(ofl_50, meanlog=ofl_meanlog, sdlog=ofl_sdlog)
ofl_40 <- qlnorm(p=0.4, meanlog=ofl_meanlog, sdlog=ofl_sdlog)
ofl_40_dens <- dlnorm(ofl_40, meanlog=ofl_meanlog, sdlog=ofl_sdlog)
ofl_30 <- qlnorm(p=0.3, meanlog=ofl_meanlog, sdlog=ofl_sdlog)
ofl_30_dens <- dlnorm(ofl_30, meanlog=ofl_meanlog, sdlog=ofl_sdlog)
ofl_20 <- qlnorm(p=0.2, meanlog=ofl_meanlog, sdlog=ofl_sdlog)
ofl_20_dens <- dlnorm(ofl_20, meanlog=ofl_meanlog, sdlog=ofl_sdlog)
ofl_01 <- qlnorm(p=0.0001, meanlog=ofl_meanlog, sdlog=ofl_sdlog)

# Generate posterior
ofls <- seq(0,ofl_99*1.5,0.01)
densities <- dlnorm(ofls, meanlog=ofl_meanlog, sdlog=ofl_sdlog)
plot(x=ofls, y=densities, type="l")
abline(v=ofl_50)
abline(v=ofl_40, lty=2)

# Build data for plotting
ofl_posterior <- tibble(ofl=ofls,
                        density=densities)



# Figure 2
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=9),
                   axis.title=element_text(size=11),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot values
g1 <-ggplot() +
  geom_hline(yintercept=ofl_50, lwd=0.2) +
  geom_hline(yintercept=ofl_40, lwd=0.2, linetype="dashed") +
  geom_hline(yintercept=ofl_30, lwd=0.2, linetype="dotted") +
  geom_hline(yintercept=ofl_20, lwd=0.2, linetype="dotted", color="grey60") +
  # Axis labels
  labs(y="Catch limit", x="") +
  # Label reference points
  scale_y_continuous(breaks=c(ofl_50, ofl_40, ofl_30, ofl_20), labels=c("OFL", "ABC", "ACL", "ACT"), limits=c(ofl_30*0.8, ofl_50*1.05)) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank())
g1

# Export
ggsave(g1, filename=file.path(plotdir, "figure_catch_limits.png"),
       width=1.5, height=4.5, units="in", dpi=600)

# Figure 2
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=10),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot values
g1 <-ggplot() +
  geom_hline(yintercept=ofl_50, lwd=0.2) +
  geom_hline(yintercept=ofl_40, lwd=0.2, linetype="dashed") +
  geom_hline(yintercept=ofl_30, lwd=0.2, linetype="dotted") +
  geom_hline(yintercept=ofl_20, lwd=0.2, linetype="dotted", color="grey60") +
  # Axis labels
  labs(y="Catch limit", x="", tag="A") +
  # Label reference points
  scale_y_continuous(breaks=c(ofl_50, ofl_40, ofl_30, ofl_20), labels=c("OFL", "ABC", "ACL", "ACT"), limits=c(ofl_01, ofl_99*0.8)) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank())
g1

# Plot distribution
g2 <- ggplot(ofl_posterior, aes(x=ofl, y=density)) +
  geom_line(lwd=0.4) +
  coord_flip() +
  # Axis labels
  labs(x="OFL estimate", y="Density", tag="B") +
  # Mark reference points
  geom_segment(x=ofl_50, xend=ofl_50, y=0, yend=ofl_50_dens, lwd=0.2) +
  geom_segment(x=ofl_40, xend=ofl_40, y=0, yend=ofl_40_dens, lwd=0.2, linetype="dashed") +
  geom_segment(x=ofl_30, xend=ofl_30, y=0, yend=ofl_30_dens, lwd=0.2, linetype="dotted") +
  geom_segment(x=ofl_20, xend=ofl_20, y=0, yend=ofl_20_dens, lwd=0.2, linetype="dotted", color="grey60") +
  # Label reference points
  scale_x_continuous(breaks=c(ofl_50, ofl_40, ofl_30, ofl_20), labels=c("OFL", "ABC", "ACL", "ACT"), limits=c(ofl_01, ofl_99*0.8)) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank())
g2

# Merge plots
g <- gridExtra::grid.arrange(g1, g2, widths=c(0.3, 0.7))
g

# Export
ggsave(g, filename=file.path(plotdir, "Fig3_ofl_posterior.png"),
       width=4.5, height=4.5, units="in", dpi=600)



# Figure 3
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   # axis.title=element_text(size=7),
                   plot.title=element_text(size=7),
                   axis.ticks.y=element_blank(),
                   axis.text.y=element_blank(),
                   axis.title=element_blank(),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Tier 1
g1 <- ggplot(ofl_posterior, aes(x=ofl, y=density)) +
  geom_area(lwd=0.4, fill="darkgreen", color="black", alpha=0.5) +
  # Ticks
  scale_x_continuous(breaks=ofl_50, labels="OFL") +
  # Axis labels
  labs(x="OFL estimate", y="Density", title="Tier 1:\nGood OFL/uncertainty estimates") +
  # Theme
  theme_bw() + my_theme
g1

# Tier 2
g2 <- ggplot(ofl_posterior, aes(x=ofl, y=density)) +
  geom_area(lwd=0.4, fill="darkorange", color="black", alpha=0.5) +
  # Ticks
  scale_x_continuous(breaks=ofl_50, labels="OFL") +
  # Axis labels
  labs(x="OFL estimate", y="Density", title="Tier 2:\nPoor OFL/uncertainty estimates") +
  # Theme
  theme_bw() + my_theme
g2

# Tier 3
g3 <- ggplot() +
  geom_vline(xintercept=ofl_50, color="darkred") +
  # Ticks
  scale_x_continuous(breaks=ofl_50, labels="OFL") +
  # Axis labels
  labs(x="OFL estimate", y="Density", title="Tier 3:\nOFL estimates only") +
  # Theme
  theme_bw() + my_theme
g3

# Tier 4
g4 <- ggplot() +
  geom_vline(xintercept=ofl_50, color="white") +
  # Ticks
  scale_x_continuous(breaks=ofl_50, labels="OFL") +
  # Axis labels
  labs(x="OFL estimate", y="Density", title="Tier 4:\nNo OFL estimates") +
  # Theme
  theme_bw() + my_theme
g4


# Merge plots
g <- gridExtra::grid.arrange(g1, g2, g3, g4, nrow=1)
g

# Export
ggsave(g, filename=file.path(plotdir, "figure_ofl_tiers.png"),
       width=7, height=2, units="in", dpi=600)


