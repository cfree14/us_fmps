
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
fofl <- fmsy
fabc <- fofl * 0.75
b_rebuild <- bmsy*0.5

# Biomass values
bvals <- seq(0, k, 0.01)

# Build constant F
constant_f <- tibble(biomass=bvals,
                     bbmsy=biomass/bmsy) %>%
  # Add constant F
  mutate(hcr="Constant F",
         fofl=fofl,
         fabc=fabc)

# Build ramped
# (b_lim, 0), (b_thresh, umsy)
# slope = (umsy-0) / b_thresh-b_lim
# 0 = slope * b_lim + b ; b = -slope*b_lim
b_thresh <- bmsy
b_lim <- 0.25*bmsy
slope <- (fabc-0) / (b_thresh-b_lim)
intercept <- -slope * b_lim
ramped_f <- tibble(biomass=bvals,
                    bbmsy=biomass/bmsy) %>%
  # Add constant F
  mutate(hcr="Ramped F",
         fofl=fofl,
         fabc=ifelse(biomass>=b_thresh, fabc, biomass * slope + intercept))

# Build data
data <- bind_rows(constant_f, ramped_f) %>%
  # Arrange
  select(hcr, biomass, bbmsy, fofl, fabc) %>%
  # Gather
  gather(key="limit", value="f", 4:ncol(.)) %>%
  # Recode
  mutate(limit=recode_factor(limit,
                             "fofl"="Overfishing limit (OFL)",
                             "fabc"="Acceptable biological catch (ABC)"))


# Build reference points
ref_pts <- tibble(hcr="Ramped F",
                   limit="Acceptable biological catch (ABC)",
                   type=c("Limit value", "Threshold value"),
                   biomass=c(b_lim, bmsy),
                   f=c(0, fabc))


# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=7),
                   strip.text=element_text(size=7),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position = "bottom",
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data, aes(x=biomass, y=f, color=limit)) +
  facet_wrap(~hcr, nrow=1) +
  geom_line() +
  # Point
  geom_point(data=ref_pts) +
  geom_text(data=ref_pts, mapping=aes(label=type),
            color="black", hjust=-0.15, size=1.5, vjust=c(0.5, 1.2)) +
  annotate(geom="text",
           x=bmsy, y=mean(c(fofl, fabc)), label="Buffer for\nscientific uncertainty",
           fontface="italic", size=1.5) +
  # Labels
  labs(x="Biomass", y="Fishing mortality rate") +
  # Axes
  scale_x_continuous(breaks=c(0, b_lim, b_rebuild, bmsy, k),
                     labels=c("0",
                              expression("B"["lim"]),
                              expression("B"["rebuild"]),
                              expression("B"["target"]),
                              expression("B"["0"]))) +
  scale_y_continuous(breaks=c(0, fabc, fofl),
                     labels=c("0", expression("F"["ABC"]), expression("F"["OFL"]) ),
                     lim=c(0, NA)) +
  # Legend
  scale_color_discrete(name="Limit value") +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "Fig3_hcr_components.png"),
       width=4.5, height=3, units="in", dpi=600)



