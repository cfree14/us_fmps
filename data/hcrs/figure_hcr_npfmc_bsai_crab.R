
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

# ABC
abc_buffer <- 0.9

# U limits
u_ofl <- u_msy

# F limits
f_msy <- -log(1 - u_msy)
f_ofl <- -log(1 - u_ofl)


# F = -log(1 - U)
# U = 1 - exp(-F)


# Equations
################################################################################

# Calculate OFL
a <- 0.1
b <- 0.3
bbmsy <- 1.1
calc_f_ofl <- function(bbmsy, a, b, fmsy){

  # If above threshold
  if(bbmsy>=1){
    f_ofl <- fmsy
  }

  # If between threshold and limit
  if(bbmsy>b & bbmsy<1){
    f_ofl <- fmsy * (bbmsy - a) / (1 - a)
  }

  # If below threshhold
  if(bbmsy<=b){
    f_ofl <- 0
  }

  # Return
  return(f_ofl)


}

# Build data
################################################################################

# Biomass values
b_inc <- 0.01
b_vals <- seq(0, k, b_inc)
nvals <- length(b_vals)


# Build data
data <- tibble(biomass=b_vals,
               bbmsy=b_vals/b_msy) %>%
  # Add F OFL
  rowwise() %>%
  mutate(f_ofl=calc_f_ofl(bbmsy=bbmsy, a=a, b=b, fmsy=f_msy)) %>%
  ungroup() %>%
  # Add F ABC
  mutate(f_abc=f_ofl*0.9) %>%
  # Gather
  gather(key="value", value="f", 3:ncol(.)) %>%
  # Format value type
  mutate(value=recode_factor(value, "f_ofl"="OFL", "f_abc"="ABC/ACL")) %>%
  # Calculate exploitation rate
  mutate(u= 1 - exp(-f)) %>%
  # Calculate catch
  mutate(catch=u*biomass) %>%
  # Add FMC/FMP
  mutate(fmc="NPFMC",
         fmp="BSAI King and Tanner Crab") %>%
  # Arrange
  select(fmc, fmp, everything())

# Export data
write.csv(data, file=file.path(outdir, "NPFMC_bsai_crab.csv"), row.names=F)



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
g1 <- ggplot(data, aes(x=bbmsy, y=f, color=value)) +
  geom_line() +
  # Limits
  scale_y_continuous(lim=c(0, f_ofl*1.1),
                     breaks=c(0, f_ofl), labels=c("0", expression("F"["OFL"]))) +
  scale_x_continuous(breaks=c(0, a, b, 1, k/b_msy),
                     labels=c("0", "α", "β",  expression("B"["MSY"]), expression("B"["0"]))) +
  # Labels
  labs(x=expression("B/B"["MSY"]), y="Fishing mortality rate", tag="A") +
  # Legend
  scale_color_discrete(name="Limit value") +
  # Theme
  theme_bw() + my_theme
g1

# Plot
g2 <- ggplot(data, aes(x=bbmsy, y=catch, color=value)) +
  geom_line() +
  # Reference line
  geom_hline(yintercept=msy, linetype="dotted") +
  # Limits
  scale_y_continuous(breaks=c(msy), labels=c("MSY")) +
  scale_x_continuous(breaks=c(0, a, b, 1, k/b_msy),
                     labels=c("0", "α", "β",  expression("B"["MSY"]), expression("B"["0"]))) +
  # Labels
  labs(x=expression("B/B"["MSY"]), y="Annual catch limit", tag="B") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g2

# Merge plots
g <- gridExtra::grid.arrange(g1, g2, nrow=1)
g

# Export plot
ggsave(g, filename=file.path(plotdir, "figure_hcr_npfmc_crab.png"),
       width=6.5, height=2.75, units="in", dpi=600)





