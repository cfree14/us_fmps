
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
inputdir <- "data/hcrs/raw"
outputdir <- "data/hcrs/processed"

# Read data
data_orig <- readRDS(file.path(outputdir, "hcr_data.Rds"))

# Plot data
################################################################################

# Theme
my_theme <- theme(axis.text=element_text(size=5),
                  axis.text.y = element_text(angle = 90, hjust = 0.5),
                  axis.title=element_text(size=6),
                  legend.text=element_text(size=4),
                  legend.title=element_text(size=5),
                  plot.title=element_text(size=6, face="bold"),
                  plot.subtitle=element_text(size=6),
                  # Gridlines
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  axis.line = element_line(colour = "black"),
                  # Legend
                  legend.position = c(0.75, 0.2),
                  legend.key.size = unit(0.2, "cm"),
                  legend.background = element_rect(fill=alpha('blue', 0)))

# Parameters
k <- 1
r <- 0.2

# MSY reference points
b_msy <- k/2
u_msy <- r/2
msy <- b_msy*u_msy

# U limits
u_ofl <- u_msy
u_abc <- u_ofl * 0.75

# F limits
f_msy <- -log(1 - u_msy)
f_ofl <- -log(1 - u_ofl)
f_abc <- -log(1 - u_abc)

# Constant
##################################

# Data
data_constant <- data_orig %>%
  filter(fmc=="PFMC" & fmp=="HMS")

# Plot F
g1 <- ggplot(data_constant, aes(x=biomass, y=f, color=value)) +
  geom_line() +
  # Limits
  scale_y_continuous(lim=c(0, f_ofl*1.1),
                     breaks=c(0, f_abc, f_ofl), labels=c("0", expression("F"["ABC"]), expression("F"["OFL"]))) +
  scale_x_continuous(breaks=c(0, b_msy, k),
                     labels=c("0", expression("B"["MSY"]), expression("B"["0"]))) +
  # Labels
  labs(x="Biomass", y="Fishing mortality rate", title="Constant", subtitle="PFMC HMS") +
  # Legend
  scale_color_discrete(name="Limit value") +
  # Theme
  theme_bw() + my_theme
g1

# Plot catch
g2 <- ggplot(data_constant, aes(x=biomass, y=catch, color=value)) +
  geom_line() +
  # Limits
  scale_y_continuous(breaks=c(msy), labels=c("MSY")) +
  scale_x_continuous(breaks=c(0, b_msy, k),
                     labels=c("0", expression("B"["MSY"]), expression("B"["0"]))) +
  # Labels
  labs(x="Biomass", y="Catch limit") +
  # Reference line
  geom_hline(yintercept=msy, linetype="dotted") +
  # Legend
  scale_color_discrete(name="Limit value") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g2



# Threshold
##################################


# Ramped
##################################

# Data
data_ramped <- data_orig %>%
  filter(fmc=="NPFMC" & fmp=="Groundfish")

# Parameters
a_bbmsy <- 0.05
a_biomass <- a_bbmsy/2
a_bbmsy_abc <- 0.5
a_biomass_abc <- a_bbmsy_abc/2
b_lim <- b_msy
b_min <- 0.1
pstar_buffer <- 0.9

# U limits
u_ofl <- u_msy
u_abc <- u_ofl * pstar_buffer

# Plot F
g3 <- ggplot(data_ramped, aes(x=biomass, y=f, color=value)) +
  geom_line() +
  # Limits
  scale_y_continuous(lim=c(0, f_ofl*1.1),
                     breaks=c(0, f_abc, f_ofl), labels=c("0", expression("F"["ABC"]), expression("F"["OFL"]))) +
  scale_x_continuous(breaks=c(0, a_biomass, a_biomass_abc, b_lim, k),
                     labels=c("0", expression("α"["1"]), expression("α"["2"]),  expression("B"["lim"]), expression("B"["0"]))) +
  # Labels
  labs(x="Biomass", y="Fishing mortality rate", title="Ramped (w/ cutoff)", subtitle="NPFMC Groundfish") +
  # Legend
  scale_color_discrete(name="Limit value") +
  # Theme
  theme_bw() + my_theme
g3

# Plot
g4 <- ggplot(data_ramped, aes(x=biomass, y=catch, color=value)) +
  geom_line() +
  # Reference line
  geom_hline(yintercept=msy, linetype="dotted") +
  # Limits
  scale_y_continuous(breaks=c(msy), labels=c("MSY")) +
  scale_x_continuous(breaks=c(0, a_biomass, a_biomass_abc, b_lim, k),
                     labels=c("0", expression("α"["1"]), expression("α"["2"]),  expression("B"["lim"]), expression("B"["0"]))) +
  # Labels
  labs(x="Biomass", y="Catch limit") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g4


# Combo
##################################


# Environmentally linked
##################################


# Merge and export
##################################

# Merge
layout_matrix <- matrix(data=c(1,3,5,7,9,
                               2,4,6,8,19), byrow=T, nrow=2)
g <- gridExtra::grid.arrange(g1, g2, g3, g4, g1, g2, g3, g4, g1, g2, layout_matrix=layout_matrix, heights=c(0.55, 0.45))
g

# Export figure
ggsave(g, filename=file.path(plotdir, "figure_hcr_typologies.png"),
       width=6.5, height=3.25, units="in", dpi=600)






