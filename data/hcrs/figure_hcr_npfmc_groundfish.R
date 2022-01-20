
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
k <- 1
r <- 0.2

# MSY reference points
b_msy <- k/2
u_msy <- r/2
msy <- b_msy*u_msy

# B limits
b_lim <- b_msy
b_min <- 0.1

# Pstar buffer
pstar_buffer <- 0.9

# U limits
u_ofl <- u_msy
u_abc <- u_ofl * pstar_buffer

# F limits
f_msy <- -log(1 - u_msy)
f_ofl <- -log(1 - u_ofl)
f_abc <- -log(1 - u_abc)


# F = -log(1 - U)
# U = 1 - exp(-F)


# Build data
################################################################################

# Biomass values
b_inc <- 0.01
b_vals <- seq(0, k, b_inc)
nvals <- length(b_vals)

# Parameters
a_bbmsy <- 0.05
a_biomass <- a_bbmsy/2
a_bbmsy_abc <- 0.5
a_biomass_abc <- a_bbmsy_abc/2

# Build data
data <- tibble(biomass=b_vals,
                bbmsy=b_vals/b_msy) %>%
  # Add F OFL
  mutate(f_ofl=ifelse(bbmsy>1, f_msy,
                      ifelse(bbmsy<=a_bbmsy, 0, f_msy*(bbmsy-a_bbmsy)/(1-a_bbmsy)))) %>%
  # Add F ABC
  mutate(f_abc=f_ofl*0.9,
         f_abc=ifelse(bbmsy<=a_bbmsy_abc, 0, f_abc)) %>%
  # Gather
  gather(key="value", value="f", 3:ncol(.)) %>%
  # Format value type
  mutate(value=recode_factor(value, "f_ofl"="OFL", "f_abc"="ABC/ACL")) %>%
  # Calculate exploitation rate
  mutate(u= 1 - exp(-f)) %>%
  # Calculate catch
  mutate(catch=u*biomass)


# Plot data
################################################################################

# My theme
my_theme <- theme(axis.text=element_text(size=6),
                  axis.title=element_text(size=8),
                  legend.text=element_text(size=6),
                  legend.title=element_text(size=8),
                  plot.tag=element_text(size=9),
                  # Gridlines
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  axis.line = element_line(colour = "black"),
                  # Legend
                  legend.position = c(0.2, 0.8),
                  legend.key.size = unit(0.3, "cm"),
                  legend.background = element_rect(fill=alpha('blue', 0)))

# Plot
g1 <- ggplot(data, aes(x=biomass, y=f, color=value)) +
  geom_line() +
  # Limits
  scale_y_continuous(lim=c(0, f_ofl*1.1),
                     breaks=c(0, f_abc, f_ofl), labels=c("0", expression("F"["ABC"]), expression("F"["OFL"]))) +
  scale_x_continuous(breaks=c(0, a_biomass, a_biomass_abc, b_lim, k),
                     labels=c("0", expression("α"["1"]), expression("α"["2"]),  expression("B"["lim"]), expression("B"["0"]))) +
  # Labels
  labs(x="Biomass", y="Fishing mortality rate", tag="A") +
  # Legend
  scale_color_discrete(name="Limit value") +
  # Theme
  theme_bw() + my_theme
g1

# Plot
g2 <- ggplot(data, aes(x=biomass, y=catch, color=value)) +
  geom_line() +
  # Reference line
  geom_hline(yintercept=msy, linetype="dotted") +
  # Limits
  scale_y_continuous(breaks=c(msy), labels=c("MSY")) +
  scale_x_continuous(breaks=c(0, a_biomass, a_biomass_abc, b_lim, k),
                     labels=c("0", expression("α"["1"]), expression("α"["2"]),  expression("B"["lim"]), expression("B"["0"]))) +
  # Labels
  labs(x="Biomass", y="Annual catch limit", tag="B") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g2

# Merge plots
g <- gridExtra::grid.arrange(g1, g2, nrow=1)
g

# Export plot
ggsave(g, filename=file.path(plotdir, "figure_hcr_npfmc_groundfish.png"),
       width=6.5, height=2.75, units="in", dpi=600)





