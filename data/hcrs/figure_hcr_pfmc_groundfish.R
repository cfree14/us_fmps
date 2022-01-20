
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

# OFL values
ofls <- b_vals * u_msy

# ABC values
abcs <- b_vals * u_msy * pstar_buffer

# Build ACLs
abc_at_blim <- b_lim * u_msy * pstar_buffer
acls <- c(rep(0, sum(b_vals<b_min)),
          seq(0, abc_at_blim, length.out = sum(b_vals>=b_min & b_vals<=b_lim)),
          abcs[b_vals>b_lim])

# Build data
data <- tibble(biomass=b_vals,
               ofl=ofls,
               abc=abcs,
               acl=acls) %>%
  # Gather
  gather(key="value", value="catch", 2:ncol(.)) %>%
  # Format value type
  mutate(value=toupper(value),
         value=factor(value, levels=c("OFL", "ABC", "ACL"))) %>%
  # Add exploitation rate (U)
  mutate(u=catch/biomass) %>%
  # Add fishing mortality rate (F)
  mutate(f=-log(1-u))


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
g1 <- ggplot(data, aes(x=biomass, y=catch, color=value)) +
  geom_line() +
  # Limits
  scale_y_continuous(breaks=c(msy), labels=c("MSY")) +
  scale_x_continuous(breaks=c(0, b_min, b_lim, k),
                     labels=c("0", expression("B"["min"]), expression("B"["lim"]), expression("B"["0"]))) +
  # Labels
  labs(x="Biomass", y="Annual catch limit", tag="A") +
  # Reference line
  geom_hline(yintercept=msy, linetype="dotted") +
  # Legend
  scale_color_discrete(name="Limit value") +
  # Theme
  theme_bw() + my_theme
g1

# Plot
g2 <- ggplot(data, aes(x=biomass, y=u, color=value)) +
  geom_line() +
  # Limits
  scale_y_continuous(lim=c(0, u_ofl*1.1),
                     breaks=c(0, u_abc, u_ofl), labels=c("0", expression("U"["ABC"]), expression("U"["OFL"]))) +
  scale_x_continuous(breaks=c(0, b_min, b_lim, k),
                     labels=c("0", expression("B"["min"]), expression("B"["lim"]), expression("B"["0"]))) +
  # Labels
  labs(x="Biomass", y="Exploitation rate\n(catch / biomass)", tag="B") +
  # Legend
  scale_color_discrete(name="Limit value") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g2

# Plot
g3 <- ggplot(data, aes(x=biomass, y=f, color=value)) +
  geom_line() +
  # Limits
  scale_y_continuous(lim=c(0, f_ofl*1.1),
                     breaks=c(0, f_abc, f_ofl), labels=c("0", expression("F"["ABC"]), expression("F"["OFL"]))) +
  scale_x_continuous(breaks=c(0, b_min, b_lim, k),
                     labels=c("0", expression("B"["min"]), expression("B"["lim"]), expression("B"["0"]))) +
  # Labels
  labs(x="Biomass", y="Fishing mortality rate", tag="B") +
  # Legend
  scale_color_discrete(name="Limit value") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g3

# Merge plots
g <- gridExtra::grid.arrange(g1, g3, nrow=1)
g

# Export plot
ggsave(g, filename=file.path(plotdir, "figure_hcr_pfmc_groundfish.png"),
       width=6.5, height=2.75, units="in", dpi=600)


