
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

# OFL CV and pstar
ofl_cv <- 0.75
pstar <- 0.4

# Calculcate catch
# Test: biomass <- 0.3; pstar <- 0.25; cv <- 0.5
calc_abc <- function(biomass, pstar, cv_ofl, u_ofl){

  # Build OFL distribution
  # CV = sqrt(exp(sdlog^2)-1)
  # sdlog = log(CV^2+1)
  # median = exp(meanlog)
  # meanlog = log(median)
  ofl_median <- biomass * u_ofl
  ofl_sdlog <- log(cv_ofl^2+1)
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
b_vals <- seq(0, k, b_inc)
nvals <- length(b_vals)

# Build data
data <- tibble(biomass=b_vals) %>%
  # Add B/BMSY
  mutate(bbmsy=b_vals/b_msy) %>%
  # Add P*
  mutate(pstar=pstar) %>%
  # Calculate catch
  rowwise() %>%
  mutate(ofl=biomass*u_ofl,
         abc=calc_abc(biomass=biomass, u_ofl=u_ofl, pstar=pstar, cv=ofl_cv)) %>%
  ungroup() %>%
  # Add ACL
  mutate(acl=abc*0.95) %>%
  # Gather
  gather(key="value", value="catch", 4:ncol(.)) %>%
  mutate(value=toupper(value)) %>%
  # Calculate mortality
  mutate(u=catch/biomass,
         f=-log(1 - u))

# Derive F ABC
f_abc <- data %>% filter(value=="ABC") %>% pull(f) %>% round(digits = 8) %>% unique() %>% na.omit() %>% as.numeric()
f_acl <- data %>% filter(value=="ACL") %>% pull(f) %>% round(digits = 8) %>% unique() %>% na.omit() %>% as.numeric()


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


# Plot F
g1 <- ggplot(data, aes(x=biomass, y=f, color=value)) +
  geom_line() +
  # Limits
  scale_y_continuous(lim=c(0, f_ofl*1.1),
                     breaks=c(0, f_acl, f_abc, f_ofl), labels=c("0", expression("F"["ACL"]), expression("F"["ABC"]), expression("F"["OFL"]))) +
  scale_x_continuous(breaks=c(0, b_msy, k),
                     labels=c("0", expression("B"["MSY"]), expression("B"["0"]))) +
  # Labels
  labs(x="Biomass", y="Fishing mortality rate", tag="A") +
  # Legend
  scale_color_discrete(name="Limit value") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g1

# Plot catch
g2 <- ggplot(data, aes(x=biomass, y=catch, color=value)) +
  geom_line() +
  # Limits
  scale_y_continuous(breaks=c(msy), labels=c("MSY")) +
  scale_x_continuous(breaks=c(0, b_msy, k),
                     labels=c("0", expression("B"["MSY"]), expression("B"["0"]))) +
  # Labels
  labs(x="Biomass", y="Annual catch limit", tag="B") +
  # Reference line
  geom_hline(yintercept=msy, linetype="dotted") +
  # Legend
  scale_color_discrete(name="Limit value") +
  # Theme
  theme_bw() + my_theme
g2


# Merge plots
g <- gridExtra::grid.arrange(g1, g2, nrow=1)
g

# Export plot
ggsave(g, filename=file.path(plotdir, "figure_hcr_nefmc_small_mesh.png"),
       width=6.5, height=2.75, units="in", dpi=600)

