
# Clear workspace
rm(list = ls())
options(dplyr.summarise.inform=F)

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "data/hcrs/figures"
outdir <- "data/hcrs/raw"


# Set parameters
################################################################################

# HCR params
cutoff <- 18200
fraction <- 0.30
distribution <- 0.70
maxcatch <- 40000
buffer <- 0.9

# Parameters
k <- cutoff * 4
r <- 0.9

# MSY reference points
b_msy <- k/2
u_msy <- r/2
msy <- b_msy*u_msy

# U limits
u_ofl <- u_msy
u_abc <- u_ofl * buffer
u_ofl_usa <- u_ofl * distribution
u_abc_usa <- u_abc * distribution

# F limits
f_msy <- -log(1 - u_msy)
f_ofl <- -log(1 - u_ofl)
f_abc <- -log(1 - u_abc)
f_ofl_usa <- -log(1 - u_ofl_usa)
f_abc_usa <- -log(1 - u_abc_usa)

# F = -log(1 - U)
# U = 1 - exp(-F)


# Build data
################################################################################

# Biomass values
b_inc <- 10
b_vals <- seq(0, k, b_inc)
nvals <- length(b_vals)

# OFL values
ofls <- b_vals * u_ofl * distribution
abcs <- b_vals * u_ofl * distribution * buffer
hgs <- pmax(0, (b_vals - cutoff) * fraction * distribution)
acts <- pmin(hgs, abcs)


# Build data
data <- tibble(biomass=b_vals,
               bbmsy=biomass/b_msy,
               ofl=ofls,
               abc=abcs,
               hg=hgs) %>%
  # Gather
  gather(key="value", value="catch", 3:ncol(.)) %>%
  # Format value type
  mutate(value=toupper(value),
         value=factor(value, levels=c("OFL", "ABC", "HG"))) %>%
  # Add exploitation rate (U)
  mutate(u=catch/biomass) %>%
  # Add fishing mortality rate (F)
  mutate(f=-log(1-u)) %>%
  # Add FMC/FMP
  mutate(fmc="PFMC",
         fmp="Chub mackerel") %>%
  # Arrange
  select(fmc, fmp, everything())

# Export data
write.csv(data, file=file.path(outdir, "PFMC_chub_mackerel.csv"), row.names=F)


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

# Plot catch
g1 <- ggplot(data, aes(x=biomass, y=catch, color=value)) +
  geom_line() +
  # Limits
  scale_x_continuous(breaks=c(0, cutoff, k),
                     labels=c("0", expression("B"["lim"]), expression("B"["0"]))) +
  # Labels
  labs(x="Biomass", y="Annual catch limit", tag="A") +
  # Legend
  scale_color_discrete(name="Limit value") +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
g1

# Plot F
g2 <- ggplot(data, aes(x=biomass, y=f, color=value)) +
  geom_line() +
  # Limits
  scale_y_continuous(lim=c(0, f_ofl_usa*1.1),
                     breaks=c(0, f_abc_usa, f_ofl_usa), labels=c("0", expression("F"["ABC-USA"]), expression("F"["OFL-USA"]))) +
  scale_x_continuous(breaks=c(0, cutoff, k),
                     labels=c("0", expression("B"["lim"]), expression("B"["0"]))) +
  # Labels
  labs(x="Biomass", y="Fishing mortality rate", tag="B") +
  # Legend
  scale_color_discrete(name="Limit value") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g2

# Merge plots
g <- gridExtra::grid.arrange(g1, g2, nrow=1)
g

# Export plot
ggsave(g, filename=file.path(plotdir, "figure_hcr_pfmc_chub_mackerel.png"),
       width=6.5, height=2.75, units="in", dpi=600)


