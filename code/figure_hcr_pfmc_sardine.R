
# Clear workspace
rm(list = ls())
options(dplyr.summarise.inform=F)

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"


# Helper functions
################################################################################

# Function to calculate EMSY
calc_emsy <- function(sst_c){
  emsy <- -18.46452 + 3.25209*sst_c - 0.19723*sst_c^2 + 0.0041863*sst_c^3
  return(emsy)
}

# Function to calculate OFL
calc_ofl <- function(b_mt, emsy, dist=0.87, emsy_min=0, emsy_max=0.25){
  emsy_use <- pmin(pmax(emsy_min, emsy), emsy_max)
  ofl <- b_mt * emsy_use * dist
  return(ofl)
}

# Function to calculate ABC
calc_abc <- function(b_mt, emsy, dist=0.87, pstar_buffer=0.92657, emsy_min=0, emsy_max=0.25){
  emsy_use <- pmin(pmax(emsy_min, emsy), emsy_max)
  abc <- b_mt * pstar_buffer * emsy_use * dist
  return(abc)
}

# Function to calculate HG
calc_hg <- function(b_mt, emsy, cutoff_mt=150000, maxcat_mt=200000, dist=0.87, emsy_min=0.05, emsy_max=0.20){
  emsy_use <- pmin(pmax(emsy_min, emsy), emsy_max)
  hg <- (b_mt - cutoff_mt) * dist * emsy_use
  hg <- pmax(0, hg)
  hg <- pmin(maxcat_mt, hg)
  return(hg)
}

# Test functions
# They match the 2020 stock assessment
emsy <- calc_emsy(sst_c=15.9965)
calc_ofl(b_mt=28276, emsy=emsy)
calc_abc(b_mt=28276, emsy=emsy)
calc_hg(b_mt=28276, emsy=emsy)

# Build data
################################################################################

# Build EMSY~SST data
ssts <- seq(14, 18, 0.01)
emsys <- calc_emsy(sst_c=ssts)
emsy_df <- tibble(sst_c=ssts,
                  emsy=emsys,
                  emsy_ofl=pmin(0.25, emsy) %>% pmax(0, .),
                  emsy_hg=pmin(0.2, emsy) %>% pmax(0.05, .)) %>%
  # Gather
  gather(key="emsy_type", value="emsy", 2:ncol(.))

# Values
sst_vals <- c(15, 15.5, 16)
b_vals <-seq(0, 5000, 500) * 1000

# Build data
x <- sst_vals[1]
limit_df_orig <- purrr::map_df(sst_vals, function(x){

  # Generate data
  emsy <- calc_emsy(x)
  ofls <- calc_ofl(b_mt=b_vals, emsy=emsy)
  abcs <- calc_abc(b_mt=b_vals, emsy=emsy)
  hgs <- calc_hg(b_mt=b_vals, emsy=emsy)

  # Build data
  df <- tibble(sst_c=x,
               biomass=b_vals,
               emsy=emsy,
               ofl=ofls,
               abc=abcs,
               hg=hgs) %>%
    gather(key="limit_type", value="catch", 4:ncol(.)) %>%
    mutate(u=catch/(biomass*0.87))

})

# Format data
limit_df <- limit_df_orig %>%
  # Format limit type
  mutate(limit_type=recode_factor(limit_type,
                                  "ofl"="OFL",
                                  "abc"="ABC",
                                  "hg"="HG")) %>%
  # Format SST label
  mutate(sst_label=format(sst_c, nsmall=1) %>% paste0(., "°C"))

unique(limit_df$sst_c)


# Plot data
################################################################################

# Plot EMSY data
g1 <- ggplot(emsy_df, aes(x=sst_c, y=emsy, color=emsy_type)) +
  geom_line() +
  # Labels
  labs(x="SST index (°C)", y=expression("U"["MSY"])) +
  # Limits
  lims(y=c(0,NA)) +
  # Legend
  scale_color_discrete(name="Type") +
  # Theme
  theme_bw()
g1

# Plot catch limit data
g2 <- ggplot(limit_df, aes(x=biomass/1e3, y=catch/1e3,
                           color=sst_label, linetype=limit_type)) +
  facet_wrap(~sst_label) +
  geom_line() +
  # Labels
  labs(x="Biomass (1000s mt)", y="Catch (1000s mt)") +
  # Legend
  scale_color_discrete(name="SST (°C)") +
  scale_linetype_manual(name="Catch limit", values=c("solid", "dashed", "dotted")) +
  # Theme
  theme_bw()
g2

# Plot F limit data
g3 <- ggplot(limit_df, aes(x=biomass/1e3, y=u,
                           color=sst_label, linetype=limit_type)) +
  facet_wrap(~sst_label) +
  geom_line() +
  # Limits
  lims(y=c(0,NA)) +
  # Labels
  labs(x="Biomass (1000s mt)", y="Exploitation rate") +
  # Legend
  scale_color_discrete(name="SST (°C)") +
  scale_linetype_manual(name="Catch limit", values=c("solid", "dashed", "dotted")) +
  # Theme
  theme_bw()
g3

# Merge

# Export


