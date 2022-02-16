
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

# Parameters
k <- 1
r <- 0.2

# MSY reference points
b_msy <- k/2
u_msy <- r/2
msy <- b_msy*u_msy
f_msy <- -log(1 - u_msy)
u_ofl <- u_msy
f_ofl <- -log(1 - u_ofl)


# Functions
################################################################################

# Calculcate catch
# Test: biomass <- 0.3; pstar <- 0.25; cv <- 0.5
calc_abc <- function(biomass, pstar, cv=1, u_ofl){

  # Build OFL distribution
  # CV = sqrt(exp(sdlog^2)-1)
  # sdlog = log(CV^2+1)
  # median = exp(meanlog)
  # meanlog = log(median)
  ofl_median <- biomass * u_ofl
  ofl_sdlog <- log(cv^2+1)
  ofl_meanlog <- log(ofl_median)

  # Confirm that you calculated the parameters correctly: specified median = derived median
  all.equal(ofl_median, qlnorm(0.5, meanlog=ofl_meanlog, sdlog=ofl_sdlog))

  # Derive the ABC given the P*
  abc <- qlnorm(pstar, meanlog=ofl_meanlog, sdlog=ofl_sdlog)

  # Return
  return(abc)

}


# Build data
################################################################################

# CVs by tier 1, 2, 3
pstars <- c(0.3, 0.4, 0.5)


# Biomass values
b_inc <- 0.005
b_vals <- seq(0, k, b_inc)
nvals <- length(b_vals)

# Build data
data <- expand.grid(biomass=b_vals,
                    pstar=pstars) %>%
  # Add B/BMSY
  mutate(bbmsy=biomass/b_msy) %>%
  # Calculate catch
  rowwise() %>%
  mutate(ofl=biomass*u_ofl,
         abc=calc_abc(biomass=biomass, u_ofl=u_ofl, pstar=pstar)) %>%
  ungroup() %>%
  # Gather
  gather(key="value", value="catch", 4:ncol(.)) %>%
  mutate(value=toupper(value)) %>%
  # Calculate mortality
  mutate(u=catch/biomass,
         f=-log(1 - u))


# Plot data
################################################################################

# My theme
my_theme <- theme(axis.text=element_text(size=6),
                  axis.title=element_text(size=8),
                  legend.text=element_text(size=6),
                  legend.title=element_text(size=7),
                  plot.title = element_text(size=8),
                  plot.tag=element_text(size=9),
                  # Gridlines
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  axis.line = element_line(colour = "black"),
                  # Legend
                  legend.position = c(0.25, 0.75),
                  legend.key.size = unit(0.25, "cm"),
                  legend.margin = margin(c(-0.1,0,0,0), unit="cm"),
                  legend.background = element_rect(fill=alpha('blue', 0)))

# Plot F
g1 <- ggplot(data %>% filter(value!="OFL"), aes(x=biomass, y=f, color=as.character(pstar), linetype=value)) +
  geom_line() +
  # Labels
  labs(x="Biomass", y="Fishing mortality rate", tag="A") +
  # Axis
  scale_x_continuous(breaks=c(0, b_msy, k),
                     labels=c("0", expression("B"["MSY"]), expression("B"["0"]))) +
  scale_y_continuous(lim=c(0, f_ofl*1.1),
                     breaks=c(0, f_ofl),
                     labels=c("0", expression("F"["OFL"]))) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g1


# Plot catch
g2 <- ggplot(data %>% filter(value!="OFL"), aes(x=biomass, y=catch, color=as.character(pstar))) +
  geom_hline(yintercept=msy, linetype="dotted", color="grey60") +
  geom_line() +
  # Labels
  labs(x="Biomass", y="Catch", tag="B") +
  # Axis
  scale_x_continuous(breaks=c(0, b_msy, k),
                     labels=c("0", expression("B"["MSY"]), expression("B"["0"]))) +
  scale_y_continuous(breaks=c(0, msy), labels=c("0", "MSY")) +
  # Legend
  scale_color_discrete("P*") +
  # Theme
  theme_bw() + my_theme
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1)
g

# Export
ggsave(g, filename=file.path(plotdir, "figure_hcr_safmc_all.png"),
       width=6.5, height=2.75, units="in", dpi=600)

