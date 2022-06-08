
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


# Build data
################################################################################

# Parameters
r <- 0.8
k <- 1

# Reference points
umsy <- r/2
bmsy <- k/2
msy <- umsy*bmsy
fmsy <- -log(1 - umsy)

# Biomass values
bvals <- seq(0, k, 0.01)

# Build constant catch
catch_based <- tibble(biomass=bvals,
                      bbmsy=biomass/bmsy) %>%
  # Add constant F
  mutate(hcr="Catch-based",
         catch=NA,
         u=NA,
         f=NA)

# Build constant catch
constant_c <- tibble(biomass=bvals,
                     bbmsy=biomass/bmsy) %>%
  # Add constant F
  mutate(hcr="Constant catch",
         catch=msy*0.1,
         u=catch/biomass,
         f=-log(1 - u))

# Build constant escapement
constant_e <- tibble(biomass=bvals,
                     bbmsy=biomass/bmsy) %>%
  # Add constant F
  mutate(hcr="Constant\nescapement",
         catch=biomass-bmsy,
         u=catch/biomass,
         f=-log(1 - u))

# Build constant F
constant_f <- tibble(biomass=bvals,
                     bbmsy=biomass/bmsy) %>%
  # Add constant F
  mutate(hcr="Constant F",
         u=umsy,
         f=fmsy,
         catch=biomass*u)

# Build stepped
stepped_f <- tibble(biomass=bvals,
                    bbmsy=biomass/bmsy) %>%
  # Add constant F
  mutate(hcr="Stepped F",
         u=ifelse(biomass>=0.5*bmsy, umsy, umsy*0.50),
         f=-log(1 - u),
         catch=biomass*u)

# Build ramped
# (b_lim, 0), (b_thresh, umsy)
# slope = (umsy-0) / b_thresh-b_lim
# 0 = slope * b_lim + b ; b = -slope*b_lim
b_thresh <- bmsy
b_lim <- 0.25*bmsy
slope <- (umsy-0) / (b_thresh-b_lim)
intercept <- -slope * b_lim
ramped_f <- tibble(biomass=bvals,
                    bbmsy=biomass/bmsy) %>%
  # Add constant F
  mutate(hcr="Ramped F",
         u=ifelse(biomass>=b_thresh, umsy, biomass * slope + intercept),
         f=-log(1 - u),
         catch=biomass*u)

# Build stepped/ramped
umsy1 <- umsy*0.5
lim1 <- 0
thresh1 <- bmsy*0.25
slope1 <- (umsy1-0) / (thresh1-lim1)
intercept1 <-  -slope1 * lim1
thresh2 <- bmsy*0.75
slope2 <- (umsy-umsy1) / (bmsy-thresh2)
# umsy2 = slope2 * thresh2 + b; b = umsy2 - slope *thresh2
intercept2 <- umsy1 - slope2*thresh2
complex_f <- tibble(biomass=bvals,
                    bbmsy=biomass/bmsy) %>%
  # Add constant F
  mutate(hcr="Stepped/\nramped F",
         u=ifelse(biomass>=bmsy, umsy,
                  ifelse(biomass>=bmsy*0.75, biomass*slope2+intercept2,
                         ifelse(biomass>=thresh1, umsy1, biomass*slope1+intercept1))),
         f=-log(1 - u),
         catch=biomass*u)

# Build data
data <- bind_rows(catch_based, constant_c, constant_e, constant_f, stepped_f, ramped_f, complex_f) %>%
  # Arrange
  select(hcr, biomass, bbmsy, u, f, catch, everything()) %>%
  # Order
  mutate(hcr=factor(hcr,
                    levels=c("Catch-based", "Constant catch", "Constant\nescapement", "Constant F",
                             "Stepped F", "Ramped F", "Stepped/\nramped F"))) %>%
  # Add type
  mutate(hcr_type=recode_factor(hcr,
                                "Catch-based"="Constant catch",
                                "Constant catch"="Constant catch",
                                "Constant\nescapement"="Constant escapement",
                                "Constant F"="Constant F",
                                "Stepped F"="Threshold F",
                                "Ramped F"="Threshold F",
                                "Stepped/\nramped F"="Threshold F"))


# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
                   strip.text=element_text(size=7.5),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                   plot.title=element_text(size=9),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.margin=margin(0,0,0,0),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot F
g1 <- ggplot(data, aes(x=biomass, y=f, color=hcr_type)) +
  facet_wrap(~hcr, nrow=1) +
  # Reference line
  geom_hline(yintercept = fmsy, linetype="dotted", color="grey80", lwd=0.7) +
  # Data
  geom_line(lwd=1.1, show.legend = F) +
  # Labels
  labs(x="Biomass", y="Fishing mortality rate",
       title="Data-limited rules                      Data-rich rules") +
  scale_color_manual(name="", values=c("darkorange2", "#AF7AC5", "#138D75", "#1B4F72")) +
  # Axes
  scale_x_continuous(breaks=c(0, bmsy, k), labels=c("0", expression("B"["MSY"]), expression("B"["0"]))) +
  scale_y_continuous(breaks=c(0, fmsy), labels=c("0", expression("F"["MSY"])), lim=c(0, NA)) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.title.x = element_blank())
g1

# Plot catch
g2 <- ggplot(data, aes(x=biomass, y=catch, color=hcr_type)) +
  facet_wrap(~hcr, nrow=1) +
  # Reference line
  geom_hline(yintercept = msy, linetype="dotted", color="grey80", lwd=0.7) +
  # Data
  geom_line(lwd=1.1) +
  # Labels
  labs(x="Biomass", y="Annual catch limit") +
  scale_color_manual(name="", values=c("darkorange2", "#AF7AC5", "#138D75", "#1B4F72")) +
  # Axes
  scale_x_continuous(breaks=c(0, bmsy, k), labels=c("0", expression("B"["MSY"]), expression("B"["0"]))) +
  scale_y_continuous(breaks=c(0, msy), labels=c("0", "MSY"), lim=c(0, NA)) +
  # Theme
  theme_bw() + my_theme +
  theme(strip.text.x = element_blank(),
        legend.position = "bottom")
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=2, heights=c(0.47, 0.53))
g

# Export
ggsave(g, filename=file.path(plotdir, "Fig1_hcr_typologies.png"),
       width=6.5, height=3.5, units="in", dpi=600)



