
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
escapement <- b_msy

# OFL
u_ofl <- u_msy
f_ofl <- -log(1 - u_ofl)
f_msy <- -log(1 - u_msy)

# F = -log(1 - U)
# U = 1 - exp(-F)


# Build data
################################################################################

# Biomass values
b_inc <- 0.01
b_vals <- seq(0, k, b_inc)
nvals <- length(b_vals)


# Build data
data <- tibble(biomass=b_vals,
               bbmsy=b_vals/b_msy) %>%
  # Calculate catch
  mutate(catch=pmax(0, biomass-escapement)) %>%
  # Calculate exploitation rate
  mutate(u= catch / biomass) %>%
  # Calculate fishing mortality rate
  mutate(f=-log(1-u)) %>%
  # Add FMC/FMP
  mutate(fmc="NPFMC",
         fmp="Salmon") %>%
  # Arrange
  select(fmc, fmp, everything())

# Export data
write.csv(data, file=file.path(outdir, "NPFMC_salmon_tiers23.csv"), row.names=F)



# Plot data
################################################################################

# My theme
my_theme <- theme(axis.text=element_text(size=6),
                  axis.title=element_text(size=8),
                  legend.text=element_text(size=6),
                  legend.title=element_text(size=8),
                  plot.title=element_text(size=8),
                  plot.tag=element_text(size=9),
                  # Gridlines
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  axis.line = element_line(colour = "black"),
                  # Legend
                  legend.position = c(0.2, 0.2),
                  legend.key.size = unit(0.3, "cm"),
                  legend.background = element_rect(fill=alpha('blue', 0)))

# Plot
g1 <- ggplot(data, aes(x=biomass, y=f)) +
  geom_line() +
  # Limits
  scale_y_continuous(breaks=c(0, f_msy), labels=c("0",  expression("F"["MSY"]))) +
  scale_x_continuous(breaks=c(0, b_msy, k),
                     labels=c("0", expression("B"["MSY"]), expression("B"["0"]))) +
  # Labels
  labs(x="Biomass", y="Fishing mortality rate", tag="A", title="Tiers 2&3: constant escapement") +
  # Theme
  theme_bw() + my_theme
g1

# Plot
g2 <- ggplot(data, aes(x=biomass, y=catch)) +
  geom_line() +
  # Limits
  scale_y_continuous(breaks=c(0, msy), labels=c("0", "MSY")) +
  scale_x_continuous(breaks=c(0, b_msy, k),
                     labels=c("0", expression("B"["MSY"]), expression("B"["0"]))) +
  # Labels
  labs(x="Biomass", y="Annual catch limit", tag="B", title=" ") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g2

# Merge plots
g <- gridExtra::grid.arrange(g1, g2, nrow=1)
g

# Export plot
ggsave(g, filename=file.path(plotdir, "figure_hcr_npfmc_salmon.png"),
       width=6.5, height=2.75, units="in", dpi=600)




#  Tier 1
################################################################################


# Relative abundance indexes
xs <- seq(0, 2.25, 0.01)

# Function to calculate catch
calc_catch <- function(x){

  # If X < 0.05
  if(x < 0.05){
    a <- 0
    b <- 0
    y <- a*x + b
  }
  # If X = 0.05-1.00
  if(x >= 0.05 & x < 1){
    a <- 130000
    b <- 20000
    y <- a*x + b
  }
  # If X = 1-1.25
  if(x >= 1 & x < 1.25){
    a <- 285000
    b <- -135000
    y <- a*x + b
  }
  # If X = 1.25-1.55
  if(x >= 1.25 & x < 1.55){
    a <- 178495
    b <- 20000
    y <- a*x + b
  }
  # If X = 1.25-1.55
  if(x >= 1.55){
    a <- 193370
    b <- 20000
    y <- a*x + b
  }
  # Return
  return(y)

}

# Calculate catch
data1 <- tibble(a_index=xs) %>%
  # Calculate catch
  rowwise() %>%
  mutate(catch=calc_catch(x=a_index)) %>%
  ungroup() %>%
  # Calculate relative F
  mutate(f_rel=catch / a_index)

# Plot data
################################################################################

# My theme
my_theme <- theme(axis.text=element_text(size=6),
                  axis.title=element_text(size=8),
                  legend.text=element_text(size=6),
                  legend.title=element_text(size=8),
                  plot.title=element_text(size=8),
                  plot.tag=element_text(size=9),
                  # Gridlines
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  axis.line = element_line(colour = "black"),
                  # Legend
                  legend.position = c(0.2, 0.2),
                  legend.key.size = unit(0.3, "cm"),
                  legend.background = element_rect(fill=alpha('blue', 0)))



# Plot catch
g1 <- ggplot(data1, aes(x=a_index, y=catch/1e3)) +
  geom_line() +
  # Labels
  labs(x="Abundance index", y="Annual catch limit (1000s mt)", tag="A", title="Tiers 1: Chinook salmon") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g1

# Plot
g2 <- ggplot(data1, aes(x=a_index, y=f_rel/1e3)) +
  geom_line() +
  # Limits
  # Labels
  labs(x="Abundance index", y="Relative F", tag="B", title=" ") +
  # Theme
  theme_bw() + my_theme
g2

# Merge plots
g <- gridExtra::grid.arrange(g1, g2, nrow=1)
g

# Export plot
ggsave(g, filename=file.path(plotdir, "figure_hcr_npfmc_salmon_tier1.png"),
       width=6.5, height=2.75, units="in", dpi=600)






