
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


# Helper functions
################################################################################

# Set parameters
################################################################################

# Parameters
k <- 1
r <- 1

# MSY reference points
b_msy <- k/2
u_msy <- r/2
msy <- b_msy*u_msy

# U limits
# Tier 1: U_ABC = U_MSY * 0.95
# Tier 2: U_ABC = U_MSY * 0.90
buffer <- 0.95
u_ofl <- u_msy
u_abc <- u_msy * buffer

# F limits
f_msy <- -log(1 - u_msy)
f_ofl <- -log(1 - u_ofl)
f_abc <- -log(1 - u_abc)

# F = -log(1 - U)
# U = 1 - exp(-F)

# Klamath
msst <- b_msy * 0.5
s_msy <- b_msy
a <- msst / 2
b <- (msst + s_msy) / 2
c <- s_msy / (1-0.25)
d <- s_msy / (1 - u_abc)
u_thresh1 <- 0.1
u_thresh2 <- 0.25

# Puget coho
u_crit <- 0.2
u_low <- 0.4
f_crit <- -log(1 - u_crit)
f_low <- -log(1 - u_low)
a1 <- msst / (1 - u_low)
b1 <- s_msy / (1 - u_ofl)


# Helper functions
################################################################################

# B limits
calc_u_acl <- function(N, msst, s_msy, u_abc){

  # Parameters
  a <- msst / 2
  b <- (msst + s_msy) / 2
  c <- s_msy / (1-0.25)
  d <- s_msy / (1 - u_abc)
  u_thresh1 <- 0.1
  u_thresh2 <- 0.25

  # Derive U
  if(N <= a){
    u <- u_thresh1 * N / a
  }
  if(N > a & N <= msst){
    u <- u_thresh1
  }
  if(N >msst & N <= b){
    u <- u_thresh1 + (0.15 *((N-msst) / (b - msst)))
  }
  if(N > b & N <= c){
    u <- u_thresh2
  }
  if(N > c & N <= d){
    u <- (N - s_msy) / N
  }
  if(N > d){
    u <- u_abc
  }
  return(u)
}

calc_u_abc <- function(N, msst, s_msy, u_crit, u_low, u_ofl){

  # Derive A
  a <- msst / (1 - u_low)

  # Derive B
  b <- s_msy / (1 - u_ofl)

  # Derive U
  if(N >= b){
    u <- u_ofl # mfmt
  }
  if(N >= a & N < b){
    u <- u_low
  }
  if(N < a){
    u <- u_crit
  }
  return(u)

}


# Build data
################################################################################

# Biomass values
b_inc <- 0.01
b_vals <- seq(0, k, b_inc)
nvals <- length(b_vals)

# Build data
data_default <- tibble(biomass=b_vals,
               bbmsy=b_vals/b_msy) %>%
  # Add U values
  mutate(u_ofl=u_msy,
         u_abc=u_abc) %>%
  # Gather
  gather(key="value", value="u", 3:ncol(.)) %>%
  # Format value type
  mutate(value=recode_factor(value, "u_ofl"="OFL", "u_abc"="ABC")) %>%
  # Calculate F
  mutate(f= -log(1 - u)) %>%
  # Calculate catch
  mutate(catch=u*biomass) %>%
  # Calculate spawner escapement
  mutate(escapement=biomass*(1-u)) %>%
  # Add FMC/FMP
  mutate(fmc="PFMC",
         fmp="Salmon (default)") %>%
  # Arrange
  select(fmc, fmp, everything())

# Build data
data_klamath <- tibble(biomass=b_vals,
                       bbmsy=b_vals/b_msy) %>%
  # Add U values
  mutate(u_ofl=u_msy,
         u_abc=u_abc) %>%
  rowwise() %>%
  mutate(u_acl=calc_u_acl(N=biomass, msst=msst, s_msy=s_msy, u_abc=u_abc)) %>%
  ungroup() %>%
  # Gather
  gather(key="value", value="u", 3:ncol(.)) %>%
  # Format value type
  mutate(value=recode_factor(value, "u_ofl"="OFL", "u_abc"="ABC", "u_acl"="ACL")) %>%
  # Calculate F
  mutate(f= -log(1 - u)) %>%
  # Calculate catch
  mutate(catch=u*biomass) %>%
  # Calculate spawner escapement
  mutate(escapement=biomass*(1-u)) %>%
  # Add FMC/FMP
  mutate(fmc="PFMC",
         fmp="Klamath River/Sacramento River Fall Chinook") %>%
  # Arrange
  select(fmc, fmp, everything())

# Build data
data_puget_coho <- tibble(biomass=b_vals,
                       bbmsy=b_vals/b_msy) %>%
  # Add U values
  mutate(u_ofl=u_msy) %>%
  rowwise() %>%
  mutate(u_abc=calc_u_abc(N=biomass, msst=msst, s_msy=s_msy, u_crit=u_crit, u_low=u_low, u_ofl=u_ofl)) %>%
  ungroup() %>%
  # Gather
  gather(key="value", value="u", 3:ncol(.)) %>%
  # Format value type
  mutate(value=recode_factor(value, "u_ofl"="OFL", "u_abc"="ABC")) %>%
  # Calculate F
  mutate(f= -log(1 - u)) %>%
  # Calculate catch
  mutate(catch=u*biomass) %>%
  # Add FMC/FMP
  mutate(fmc="PFMC",
         fmp="Puget Sound Coho") %>%
  # Arrange
  select(fmc, fmp, everything())

# Merge data
data <- bind_rows(data_default, data_klamath, data_puget_coho)

# Export data
write.csv(data, file.path(outdir, "pfmc_salmon.csv"), row.names=F)


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
                  legend.position = c(0.8, 0.85),
                  legend.key.size = unit(0.3, "cm"),
                  legend.background = element_rect(fill=alpha('blue', 0)))

# Plot F
g1 <- ggplot(data_default, aes(x=biomass, y=f, color=value)) +
  geom_line() +
  # Labels
  labs(x="Spawner abundance", y="Fishing mortality rate", title="Default salmon rule") +
  # X-axis
  scale_x_continuous(breaks=c(0, s_msy, k),
                     labels=c("0", expression("S"["MSY"]), expression("B"["0"]))) +
  # Y-axis
  scale_y_continuous(breaks=c(0, f_ofl, f_abc),
                     labels=c("0", "MFMT", expression("F"["ABC"])),
                     lim=c(0, f_ofl*1.5)) +
  # Theme
  theme_bw() + my_theme
g1

# Plot catch
g2 <- ggplot(data_default, aes(x=biomass, y=catch, color=value)) +
  geom_line() +
  # Labels
  labs(x="Spawner abundance", y="Catch limit") +
  # X-axis
  scale_x_continuous(breaks=c(0, s_msy, k),
                     labels=c("0", expression("S"["MSY"]), expression("B"["0"]))) +
  # Y-axis
  scale_y_continuous(breaks=c(0, msy),
                     labels=c("0", "MSY")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g2

# Plot F
g3 <- ggplot(data_klamath, aes(x=biomass, y=f, color=value)) +
  geom_line() +
  # Labels
  labs(x="Spawner abundance", y="Fishing mortality rate", title="Klamath/Sacramento River Fall Chinook") +
  # Y-axis
  scale_x_continuous(breaks=c(a, b, c, d, msst, s_msy, k),
                     labels=c("A", "B", "C", "D", "MSST", expression("S"["MSY"]), expression("B"["0"]))) +
  # Y-axis
  scale_y_continuous(breaks=c(0, 0.1, 0.25, f_ofl, f_abc),
                     labels=c("0", "0.1", "0.25", "MFMT", expression("F"["ABC"])),
                     lim=c(0, f_ofl*1.5)) +
  # Theme
  theme_bw() + my_theme
g3

# Plot catch
g4 <- ggplot(data_klamath, aes(x=biomass, y=catch, color=value)) +
  geom_line() +
  # Labels
  labs(x="Spawner abundance", y="Catch limit") +
  # Y-axis
  scale_x_continuous(breaks=c(a, b, c, d, msst, s_msy, k),
                     labels=c("A", "B", "C", "D", "MSST", expression("S"["MSY"]), expression("B"["0"]))) +
  # Y-axis
  scale_y_continuous(breaks=c(0, msy),
                     labels=c("0", "MSY")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g4

# Plot F
g5 <- ggplot(data_puget_coho, aes(x=biomass, y=f, color=value)) +
  geom_line() +
  # Labels
  labs(x="Spawner abundance", y="Fishing mortality rate", title="Puget Sound Coho") +
  # X-axis
  scale_x_continuous(breaks=c(0, msst, s_msy, a1, b1, k*1.5),
                     labels=c("0", "MSST", expression("S"["MSY"]), "A", "B", "B0")) +
  # Y-axis
  scale_y_continuous(breaks=c(0, f_crit, f_low, f_ofl ),
                     labels=c("0", expression("F"["critical"]), expression("F"["low"]), expression("F"["OFL"])),
                     lim=c(0, f_ofl*1.5)) +
  # Theme
  theme_bw() + my_theme
g5

# Plot catch
g6 <- ggplot(data_puget_coho, aes(x=biomass, y=catch, color=value)) +
  geom_line() +
  # Labels
  labs(x="Spawner abundance", y="Catch limit") +
  # X-axis
  scale_x_continuous(breaks=c(0, msst, s_msy, a1, b1, k*1.5),
                     labels=c("0", "MSST", expression("S"["MSY"]), "A", "B", "B0")) +
  # Y-axis
  scale_y_continuous(breaks=c(0, msy),
                     labels=c("0", "MSY")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g6

# Merge plots
layout_matrix <- matrix(c(1,3,5,
                          2,4,6), byrow=T, ncol=3)
g <- gridExtra::grid.arrange(g1, g2, g5, g6, g3, g4, layout_matrix=layout_matrix)

# Export plot
ggsave(g, filename=file.path(plotdir, "figure_hcr_pfmc_salmon.png"),
       width=6.5, height=4, units="in", dpi=600)





