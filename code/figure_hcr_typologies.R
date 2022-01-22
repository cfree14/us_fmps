
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
inputdir <- "data/hcrs/raw"
outputdir <- "data/hcrs/processed"

# Read data
data_orig <- readRDS(file.path(outputdir, "hcr_data.Rds")) %>%
  mutate(value=recode(value, "ABC/ACL"="ABC"),
         value=factor(value, levels=c("OFL", "ABC", "ACL")))

# Read sardine
data_sardine <- read.csv(file.path(inputdir, "pfmc_sardine.csv"), as.is=T)

# Plot data
################################################################################

# Theme
my_theme <- theme(axis.text=element_text(size=5),
                  axis.text.x=element_text(size=4),
                  # axis.text.y = element_text(angle = 90, hjust = 0.5),
                  axis.title=element_text(size=6),
                  legend.text=element_text(size=4),
                  legend.title=element_text(size=5),
                  plot.title=element_text(size=6, face="bold"),
                  plot.subtitle=element_text(size=6),
                  # Gridlines
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  axis.line = element_line(colour = "black"),
                  # Legend
                  legend.position = c(0.75, 0.2),
                  legend.key.size = unit(0.2, "cm"),
                  legend.background = element_rect(fill=alpha('blue', 0)))

# Parameters
k <- 1
r <- 0.2

# MSY reference points
b_msy <- k/2
u_msy <- r/2
msy <- b_msy*u_msy

# U limits
u_ofl <- u_msy
u_abc <- u_ofl * 0.75

# F limits
f_msy <- -log(1 - u_msy)
f_ofl <- -log(1 - u_ofl)
f_abc <- -log(1 - u_abc)

# Constant
##################################

# Data
data_constant <- data_orig %>%
  filter(fmc=="PFMC" & fmp=="HMS")

# Plot F
g1 <- ggplot(data_constant, aes(x=biomass, y=f, color=value)) +
  geom_line() +
  # Limits
  scale_y_continuous(lim=c(0, f_ofl*1.1),
                     breaks=c(0, f_abc, f_ofl), labels=c("0", expression("F"["ABC"]), expression("F"["OFL"]))) +
  scale_x_continuous(breaks=c(0, b_msy, k),
                     labels=c("0", expression("B"["MSY"]), expression("B"["0"]))) +
  # Labels
  labs(x="Biomass", y="Fishing mortality rate", title="Constant", subtitle="PFMC HMS") +
  # Legend
  scale_color_discrete(name="Limit value", drop=F) +
  # Theme
  theme_bw() + my_theme
g1

# Plot catch
g2 <- ggplot(data_constant, aes(x=biomass, y=catch, color=value)) +
  # Reference line
  geom_hline(yintercept=msy, linetype="dotted", color="grey60") +
  geom_line() +
  # Limits
  scale_y_continuous(breaks=c(0, msy), labels=c("0", "MSY")) +
  scale_x_continuous(breaks=c(0, b_msy, k),
                     labels=c("0", expression("B"["MSY"]), expression("B"["0"]))) +
  # Labels
  labs(x="Biomass", y="Catch limit") +
  # Legend
  scale_color_discrete(name="Limit value", drop=F) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g2



# Threshold
##################################

# Data
data_thresh <- data_orig %>%
  filter(fmc=="PFMC" & fmp=="Puget Sound Coho")

# Parameters
k1 <- 1
r1 <- 1
b_msy1 <- k1/2
u_msy1 <- r1/2
msy1 <- b_msy1*u_msy1
u_ofl1 <- u_msy1
f_msy1 <- -log(1 - u_msy1)
f_ofl1 <- -log(1 - u_ofl1)

# Parameters
s_msy <- b_msy1
msst <- b_msy1 * 0.5
u_crit <- 0.2
u_low <- 0.4
f_crit <- -log(1 - u_crit)
f_low <- -log(1 - u_low)
a1 <- msst / (1 - u_low)
b1 <- s_msy / (1 - u_ofl1)

# Plot F
g3 <- ggplot(data_thresh, aes(x=biomass, y=f, color=value)) +
  geom_line() +
  # Labels
  labs(x="Abundance (spawners)", y="Fishing mortality rate", title="Threshold", subtitle="PFMC Puget Sound Coho") +
  # Legend
  scale_color_discrete(name="Limit value", drop=F) +
  # X-axis
  scale_x_continuous(breaks=c(0, msst, s_msy, a1, b1, k1*1.5),
                     labels=c("0", "MSST", expression("S"["MSY"]), "A", "B", "B0")) +
  # Y-axis
  scale_y_continuous(breaks=c(0, f_crit, f_low, f_ofl1 ),
                     labels=c("0", expression("F"["crit"]), expression("F"["low"]), expression("F"["OFL"])),
                     lim=c(0, f_ofl1*1.1)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none",
        axis.title.y=element_blank())
g3

# Plot catch
g4 <- ggplot(data_thresh, aes(x=biomass, y=catch, color=value)) +
  # Reference line
  geom_hline(yintercept=msy1, linetype="dotted", color="grey60") +
  geom_line() +
  # Labels
  labs(x="Abundance (spawners)", y="Catch limit") +
  # X-axis
  scale_x_continuous(breaks=c(0, msst, s_msy, a1, b1, k1*1.5),
                     labels=c("0", "MSST", expression("S"["MSY"]), "A", "B", "B0")) +
  # Y-axis
  scale_y_continuous(breaks=c(0, msy1),
                     labels=c("0", "MSY")) +
  # Legend
  scale_color_discrete(name="Limit value", drop=F) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none",
        axis.title.y=element_blank())
g4

# Ramped
##################################

# Data
data_ramped <- data_orig %>%
  filter(fmc=="NPFMC" & fmp=="Groundfish")

# Parameters
a_bbmsy <- 0.05
a_biomass <- a_bbmsy/2
a_bbmsy_abc <- 0.5
a_biomass_abc <- a_bbmsy_abc/2
b_lim <- b_msy
b_min <- 0.1
pstar_buffer <- 0.9

# U limits
u_ofl <- u_msy
u_abc <- u_ofl * pstar_buffer
f_abc1 <- -log(1 - u_abc)

# Plot F
g5 <- ggplot(data_ramped, aes(x=biomass, y=f, color=value)) +
  geom_line() +
  # Limits
  scale_y_continuous(lim=c(0, f_ofl*1.1),
                     breaks=c(0, f_abc1, f_ofl), labels=c("0", expression("F"["ABC"]), expression("F"["OFL"]))) +
  scale_x_continuous(breaks=c(0, a_biomass, a_biomass_abc, b_lim, k),
                     labels=c("0", expression("α"["1"]), expression("α"["2"]),  expression("B"["lim"]), expression("B"["0"]))) +
  # Labels
  labs(x="Biomass", y="Fishing mortality rate", title="Ramped (w/ cutoff)", subtitle="NPFMC Groundfish") +
  # Legend
  scale_color_discrete(name="Limit value", drop=F) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none",
        axis.title.y=element_blank())
g5

# Plot
g6 <- ggplot(data_ramped, aes(x=biomass, y=catch, color=value)) +
  # Reference line
  geom_hline(yintercept=msy, linetype="dotted", color="grey60") +
  geom_line() +
  # Limits
  scale_y_continuous(breaks=c(0, msy), labels=c("0", "MSY")) +
  scale_x_continuous(breaks=c(0, a_biomass, a_biomass_abc, b_lim, k),
                     labels=c("0", expression("α"["1"]), expression("α"["2"]),  expression("B"["lim"]), expression("B"["0"]))) +
  # Labels
  labs(x="Biomass", y="Catch limit") +
  # Legend
  scale_color_discrete(name="Limit value", drop=F) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none",
        axis.title.y=element_blank())
g6


# Combo
##################################

# Data
data_combo <- data_orig %>%
  filter(fmc=="PFMC" & fmp=="Klamath River/Sacramento River Fall Chinook")

# Parameters
msst <- b_msy1 * 0.5
s_msy <- b_msy1
u_abc1 <- u_msy1 * 0.95
a <- msst / 2
b <- (msst + s_msy) / 2
c <- s_msy / (1-0.25)
d <- s_msy / (1 - u_abc1)
u_thresh1 <- 0.1
u_thresh2 <- 0.25
f_abc1 <- -log(1 - u_abc1)

# Plot F
g7 <- ggplot(data_combo, aes(x=biomass, y=f, color=value)) +
  geom_line() +
  # Labels
  labs(x="Abundance (spawners)", y="Fishing mortality rate", title="Combo", subtitle="PFMC Klamath chinook") +
  # X-axis
  scale_x_continuous(breaks=c(0, a, b, c, d, msst, s_msy, k1),
                     labels=c("0", "A", "B", "C", "D", "MSST", expression("S"["MSY"]), expression("B"["0"]))) +
  # Y-axis
  scale_y_continuous(breaks=c(0, 0.1, 0.25, f_ofl1, f_abc1),
                     labels=c("0", "0.1", "0.25", "MFMT", expression("F"["ABC"])),
                     lim=c(0, f_ofl1*1.1)) +
  # Legend
  scale_color_discrete(name="Limit value", drop=F) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none",
        axis.title.y=element_blank())
g7

# Plot catch
g8 <- ggplot(data_combo, aes(x=biomass, y=catch, color=value)) +
  # Reference line
  geom_hline(yintercept=msy1, linetype="dotted", color="grey60") +
  geom_line() +
  # Labels
  labs(x="Abundance (spawners)", y="Catch limit") +
  # X-axis
  scale_x_continuous(breaks=c(0, a, b, c, d, msst, s_msy, k1),
                     labels=c("0", "A", "B", "C", "D", "MSST", expression("S"["MSY"]), expression("B"["0"]))) +
  # Y-axis
  scale_y_continuous(breaks=c(0, msy1),
                     labels=c("0", "MSY")) +
  # Legend
  scale_color_discrete(name="Limit value", drop=F) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none",
        axis.title.y=element_blank())
g8

# Environmentally linked
##################################

# Plot F
g9 <- ggplot(data_sardine %>% filter(limit_type=="HG"),
             aes(x=biomass/1e6, y=u, color=sst_c, group=sst_c)) +
  geom_line() +
  # Labels
  labs(x="Biomass (millions mt)", y=expression("F"["ACL"]), title="Environmentally-linked", subtitle="PFMC Pacific sardine") +
  scale_y_continuous(lim=c(0,NA)) +
  # Legend
  scale_color_gradientn(name="SST (°C)", colors=RColorBrewer::brewer.pal(9, "YlOrRd")[3:9]) +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.8, 0.7))
g9

# Plot F
g10 <- ggplot(data_sardine %>% filter(limit_type=="HG"),
             aes(x=biomass/1e6, y=catch/1e3, color=sst_c, group=sst_c)) +
  geom_line() +
  # Labels
  labs(x="Biomass (millions mt)", y="ACL (1000s mt)") +
  # Legend
  scale_color_gradientn(name="SST (°C)", colors=RColorBrewer::brewer.pal(9, "YlOrRd")[3:9]) +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g10


# Merge and export
##################################

# Merge
layout_matrix <- matrix(data=c(1,3,5,7,9,
                               2,4,6,8,19), byrow=T, nrow=2)
g <- gridExtra ::grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, layout_matrix=layout_matrix, heights=c(0.55, 0.45))
g

# Export figure
ggsave(g, filename=file.path(plotdir, "figure_hcr_typologies.png"),
       width=7.5, height=3.5, units="in", dpi=600)






