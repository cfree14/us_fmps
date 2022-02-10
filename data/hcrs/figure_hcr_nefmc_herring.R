
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

# B theshold/limit values
bbmsy_thresh <- 0.5
bbmsy_limit <- 0.1

# MSY limits
u_ofl <- u_msy
f_msy <- -log(1 - u_msy)
f_ofl <- -log(1 - u_ofl)
f_80 <- f_msy * 0.8



# Build data
################################################################################

# Function to calculate ABC
calc_f_abc <- function(bbmsy, fmsy, bbmsy_thresh, bbmsy_limit){

  # Default ABC
  f_abc_default <- fmsy * 0.8

  # If above B/BMSY=1
  if(bbmsy>=bbmsy_thresh){
    f_abc <- f_abc_default
  }else{
    slope <- (f_abc_default - 0) / (bbmsy_thresh - bbmsy_limit)
    intercept <- 0 - slope * bbmsy_limit
    f_abc <- slope * bbmsy + intercept
  }

  # Return
  return(f_abc)

}

# Biomass values
b_inc <- 0.01
b_vals <- seq(0, k, b_inc)
nvals <- length(b_vals)

# Build data
data <- tibble(biomass=b_vals,
                bbmsy=b_vals/b_msy) %>%
  # Add F MSY
  mutate(f_msy=f_msy) %>%
  # Add F ABC
  rowwise() %>%
  mutate(f_abc=calc_f_abc(bbmsy=bbmsy, fmsy=f_msy, bbmsy_thresh=bbmsy_thresh, bbmsy_limit=bbmsy_limit)) %>%
  ungroup() %>%
  # Gather
  gather(key="value", value="f", 3:ncol(.)) %>%
  # Format value type
  mutate(value=recode_factor(value, "f_msy"="MSY", "f_abc"="ABC")) %>%
  # Calculate exploitation rate
  mutate(u= 1 - exp(-f)) %>%
  # Calculate catch
  mutate(catch=u*biomass) %>%
  # Add FMC/FMP
  mutate(fmc="NEFMC",
         fmp="Atlantic herring") %>%
  # Arrange
  select(fmc, fmp, everything())

# Export data
write.csv(data, file=file.path(outdir, "NEFMC_herring.csv"), row.names=F)



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
                  legend.position = c(0.8, 0.2),
                  legend.key.size = unit(0.3, "cm"),
                  legend.background = element_rect(fill=alpha('blue', 0)))

# Plot F
g1 <- ggplot(data, aes(x=bbmsy, y=f, color=value)) +
  geom_line() +
  # Limits
  scale_y_continuous(lim=c(0, f_ofl*1.1),
                     breaks=c(0, f_80, f_ofl), labels=c("0", expression("F"["80%"]), expression("F"["MSY"]))) +
  scale_x_continuous(breaks=c(0, bbmsy_limit, bbmsy_thresh, 1, 2),
                     labels=c("0", "0.1", "0.5",  "1.0", expression("B"["0"]))) +
  # Labels
  labs(x=expression("B/B"["MSY"]), y="Fishing mortality rate", tag="A") +
  # Legend
  scale_color_discrete(name="Limit value") +
  # Theme
  theme_bw() + my_theme
g1

# Plot catch
g2 <- ggplot(data, aes(x=biomass, y=catch, color=value)) +
  geom_line() +
  # Reference line
  geom_hline(yintercept=msy, linetype="dotted") +
  # Limits
  scale_y_continuous(breaks=c(msy), labels=c("MSY")) +
  scale_x_continuous(breaks=c(0, bbmsy_limit, bbmsy_thresh, 1, 2),
                     labels=c("0", "0.1", "0.5",  "1.0", expression("B"["0"]))) +
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
ggsave(g, filename=file.path(plotdir, "figure_hcr_nefmc_herring.png"),
       width=6.5, height=2.75, units="in", dpi=600)





