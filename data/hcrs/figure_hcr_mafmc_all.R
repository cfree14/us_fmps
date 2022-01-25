
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

# Build OFL distributions
################################################################################

# CVs by tier 1, 2, 3
cvs <- c(0.5, 0.75, 1)



# Build P* graphic
################################################################################

# Calculate P*
calc_pstar <- function(bbmsy){

  # Y = mx + b
  # b = Y - mx

  # B/BMSY <= 0.1
  if(bbmsy<=0.1){
    pstar <- 0
  }
  # B/BMSY: 0.1-1.0
  if(bbmsy>0.1 & bbmsy<=1){
    slope <- (0.45-0.00) / (1.00-0.10)
    intercept <- 0 - slope*0.1
    pstar <- intercept + slope * bbmsy
  }
  # B/BMSY: 1.0-1.5
  if(bbmsy>1.0 & bbmsy<=1.5){
    slope <- (0.49-0.45) / (1.50-1.00)
    intercept <- 0.49 - slope*1.5
    pstar <- intercept + slope * bbmsy
  }
  # B/BMSY: >=1.5
  if(bbmsy>=1.5){
    pstar <- 0.49
  }

  # Return
  return(pstar)

}

# Calculcate catch
# Test: biomass <- 0.3; pstar <- 0.25; cv <- 0.5
calc_abc <- function(biomass, pstar, cv, u_ofl){

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

# Biomass values
b_inc <- 0.005
b_vals <- seq(0, k*1.25, b_inc)
nvals <- length(b_vals)

# Build data
data <- tibble(biomass=b_vals) %>%
  # Add B/BMSY
  mutate(bbmsy=b_vals/b_msy) %>%
  # Add P*
  rowwise() %>%
  mutate(pstar=calc_pstar(bbmsy)) %>%
  ungroup() %>%
  # Calculate catch
  rowwise() %>%
  mutate(ofl=biomass*u_ofl,
         abc1=calc_abc(biomass=biomass, u_ofl=u_ofl, pstar=pstar, cv=0.50),
         abc2=calc_abc(biomass=biomass, u_ofl=u_ofl, pstar=pstar, cv=0.75),
         abc3=calc_abc(biomass=biomass, u_ofl=u_ofl, pstar=pstar, cv=1.00)) %>%
  ungroup() %>%
  # Gather
  gather(key="value", value="catch", 4:ncol(.)) %>%
  mutate(value=toupper(value),
         value=recode_factor(value,
                             "OFL"="OFL",
                             "ABC1"="ABC-Tier 1",
                             "ABC2"="ABC-Tier 2",
                             "ABC3"="ABC-Tier 3")) %>%
  # Calculate mortality
  mutate(u=catch/biomass,
         f=-log(1 - u))


# Plot data
################################################################################

# My theme
my_theme <- theme(axis.text=element_text(size=6),
                  axis.title=element_text(size=8),
                  legend.text=element_text(size=6),
                  legend.title=element_blank(),
                  plot.title = element_text(size=8),
                  plot.tag=element_text(size=9),
                  # Gridlines
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  axis.line = element_line(colour = "black"),
                  # Legend
                  legend.position = c(0.25, 0.85),
                  legend.key.size = unit(0.3, "cm"),
                  legend.background = element_rect(fill=alpha('blue', 0)))

# Plot P*
g1 <- ggplot(data, aes(x=bbmsy, y=pstar)) +
  geom_line() +
  # Reference line
  geom_hline(yintercept=0.5, linetype="dotted") +
  # Labels
  labs(x=expression("B/B"["MSY"]), y="P*") +
  # Axis
  # scale_x_continuous(breaks=c(seq(0, 2.5, 0.5), 0.10)) +
  # scale_y_continuous(breaks=c(seq(0, 0.5, 0.1), 0.49, 0.45)) +
  # Theme
  theme_bw() + my_theme
g1

# Plot catch
g2 <- ggplot(data, aes(x=bbmsy, y=catch, color=value)) +
  geom_hline(yintercept=msy, linetype="dotted", color="grey60") +
  geom_line() +
  # Labels
  labs(x=expression("B/B"["MSY"]), y="Catch") +
  # Axis
  # scale_x_continuous(breaks=c(seq(0, 2.5, 0.5), 0.10)) +
  scale_y_continuous(breaks=c(0, msy), labels=c("0", "MSY")) +
  # Legend
  scale_color_manual(name="Catch limit", values=c("black", "darkgreen", "darkorange", "darkred")) +
  # Theme
  theme_bw() + my_theme
g2

# Plot F
g3 <- ggplot(data, aes(x=bbmsy, y=f, color=value)) +
  geom_line() +
  # Labels
  labs(x=expression("B/B"["MSY"]), y="Fishing mortality rate") +
  # Axis
  # scale_x_continuous(breaks=c(seq(0, 2.5, 0.5), 0.10)) +
  scale_y_continuous(breaks=c(0, f_ofl),
                     labels=c("0", expression("F"["OFL"]))) +
  # Legend
  scale_color_manual(name="Catch limit", values=c("black", "darkgreen", "darkorange", "darkred")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g3

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, nrow=1)
g

# Export
ggsave(g, filename=file.path(plotdir, "figure_hcr_mafmc_all.png"),
       width=6.5, height=2.25, units="in", dpi=600)

