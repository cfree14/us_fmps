
# Clear workspace
rm(list = ls())
options(dplyr.summarise.inform=F)

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
outputdir <- "data/stock_hcr"

# Read data
data_orig <- read.csv(file.path(outputdir, "data_for_buffer_figure.csv"), as.is=T)

# Read old one
data_orig1 <- readxl::read_excel(file.path(outputdir, "fmp_hcr_abc_buffers_pstars.xlsx")) %>%
  rename(p_star=pstar_percentile) %>%
  select(council, p_star, abc_buffer, acl_buffer, act_buffer) %>%
  filter(council=="NEFMC") %>%
  mutate(p_star=as.numeric(p_star),
         abc_buffer=recode(abc_buffer, ">90"="90") %>% as.numeric) %>%
  mutate(p_star=p_star/100,
         abc_buffer=abc_buffer/100,
         acl_buffer=acl_buffer/100,
         act_buffer=act_buffer/100)


# Format data
################################################################################

# Format data
data1 <- data_orig %>%
  # Remove stocks without values
  filter(!is.na(p_star) | !is.na(abc_buffer) | !is.na(acl_buffer) | !is.na(act_buffer)) %>%
  # Add council
  mutate(council=sapply(stock_id, function(x) strsplit(x, split="-")[[1]][1])) %>%
  # Simplify
  select(council, p_star, abc_buffer, acl_buffer, act_buffer) %>%
  # Add NEFMC
  bind_rows(data_orig1)


table(data1$council)


# Make all-council dataset
data2 <- data1 %>%
  mutate(council="All councils")

# Merge
data3 <- bind_rows(data1, data2) %>%
  # Order councils
  mutate(council=factor(council, levels=c("NEFMC", "MAFMC", "SAFMC", "GMFMC/SAFMC", "GFMC", "CFMC",
                                          "PFMC", "NPFMC", "WPFMC", "NOAA", "All councils") %>% rev()))

# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=7),
                   axis.title.y=element_blank(),
                   plot.tag=element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot pstar
g1 <- ggplot(data3, aes(y=council, x=p_star*100)) +
  geom_boxplot(outlier.size=0.5, lwd=0.2) +
  geom_segment(x=30, xend=50, y="GFMC", yend="GFMC", color="grey30") +
  # Labels
  labs(x="Probability of\noverfishing (P*)", y="", tag="A") +
  # Limits
  lims(x=c(0,50)) +
  scale_y_discrete(drop=FALSE) +
  # Theme
  theme_bw() + my_theme
g1

# Plot ABC buffer
g2 <- ggplot(data3, aes(y=council, x=abc_buffer*100)) +
  geom_boxplot(outlier.size=0.5, lwd=0.2) +
  # Labels
  labs(x="ABC buffer\n(ABC / OFL)", y="", tag="B") +
  # Limits
  lims(x=c(0,100)) +
  scale_y_discrete(drop=FALSE) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y=element_blank())
g2

# Plot ACL buffer
g3 <- ggplot(data3, aes(y=council, x=acl_buffer*100)) +
  geom_boxplot(outlier.size=0.5, lwd=0.2) +
  # Labels
  labs(x="ACL buffer\n(ACL / ABC)", y="", tag="C") +
  # Limits
  lims(x=c(0,100)) +
  scale_y_discrete(drop=FALSE) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y=element_blank())
g3

# Plot ACT buffer
g4 <- ggplot(data3, aes(y=council, x=act_buffer*100)) +
  geom_boxplot(outlier.size=0.5, lwd=0.2) +
  # Labels
  labs(x="ACT buffer\n(ACT / ACL)", y="", tag="D") +
  # Limits
  lims(x=c(0,100)) +
  scale_y_discrete(drop=FALSE) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y=element_blank())
g4

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, g4, nrow=1,
                             widths=c(0.3, rep((1-0.3)/3, 3)))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig5_abc_buffers_by_council_new.png"),
       width=6.5, height=2.5, units="in", dpi=600)


