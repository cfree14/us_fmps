
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

# F parameters
f_thresh <- 0.31
f_msy <- f_ofl <- f_thresh
f_abc <- 0.23
u_abc <- 1-exp(-f_abc)
u_msy <- u_ofl <- 1-exp(-f_msy)

# Derive r and Bmsy
r <- u_msy * 2
b_msy <- k/2
msy <- r * k / 4


# Build data
################################################################################

# Biomass values
b_inc <- 0.005
b_vals <- seq(0, k, b_inc)
nvals <- length(b_vals)

# Build data
data <- tibble(biomass=b_vals) %>%
  # Add B/BMSY
  mutate(bbmsy=b_vals/b_msy) %>%
  # Calculate catch
  mutate(ofl=biomass*u_ofl,
         abc=biomass*u_abc,
         act=abc*0.97) %>%
  # Gather
  gather(key="value", value="catch", 3:ncol(.)) %>%
  mutate(value=toupper(value),
         value=recode(value, "ABC"="ABC/ACL"),
         value=factor(value, levels=c("OFL", "ABC/ACL", "ACT"))) %>%
  # Calculate mortality
  mutate(u=catch/biomass,
         f=-log(1 - u))

# Derive F ACT
f_act <- data %>% filter(value=="ACT") %>% pull(f) %>% round(digits = 8) %>% unique() %>% na.omit() %>% as.numeric()



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
                     breaks=c(0, f_act, f_abc, f_ofl), labels=c("0", expression("F"["ACT"]), expression("F"["ABC/ACL"]), expression("F"["OFL"]))) +
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
ggsave(g, filename=file.path(plotdir, "figure_hcr_nefmc_monkfish.png"),
       width=6.5, height=2.75, units="in", dpi=600)

