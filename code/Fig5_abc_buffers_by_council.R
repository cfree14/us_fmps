
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
data_orig <- readxl::read_excel(file.path(outputdir, "fmp_hcr_abc_buffers_pstars.xlsx"))

# Read PFMC data
pfmc_orig <- read.csv("data/tiers/PFMC_tiers.csv", as.is=T)


# Format data
################################################################################

# Format data
data <- data_orig %>%
  rename(pstar_perc=pstar_percentile) %>%
  mutate(abc_buffer=recode(abc_buffer, ">90"="90") %>% as.numeric,
         pstar_perc=recode(pstar_perc, "30-50"="") %>% as.numeric())

# Format data
pfmc <- pfmc_orig %>%
  # Rename
  janitor::clean_names("snake") %>%
  rename(abc_buffer=abc_buffer_fraction,
         pstar_perc=probability) %>%
  # Reduce to stocks
  filter(category!="") %>%
  # Clean stock
  mutate(stock_or_complex=stringr::str_trim(stock_or_complex)) %>%
  # Format pstar
  mutate(pstar_perc=pstar_perc*100) %>%
  # Format buffers
  mutate(acl_buffer=acl / abc * 100,
         abc_buffer=(1-abc_buffer) *100) %>%
  # Add council
  mutate(council="PFMC",
         fmp="Groundfish",
         level="stock",
         level_name=paste(area, stock_or_complex)) %>%
  # Simplify
  select(council, fmp, level, level_name, pstar_perc, abc_buffer, acl_buffer)

# Make council-level dataset
data1 <- bind_rows(pfmc, data)

# Make all-council dataset
data2 <- data1 %>%
  mutate(council="All councils")

# Merge
data3 <- bind_rows(data1, data2) %>%
  # Order councils
  mutate(council=factor(council,
                        levels=c("NEFMC", "MAFMC", "SAFMC", "GFMC", "CFMC", "PFMC", "NPFMC", "WPFMC", "All councils") %>% rev()))


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
g1 <- ggplot(data3, aes(y=council, x=pstar_perc)) +
  geom_boxplot(outlier.size=0.5, lwd=0.2) +
  # Labels
  labs(x="Probability of\noverfishing (P*)", y="", tag="A") +
  # Limits
  lims(x=c(0,50)) +
  # Theme
  theme_bw() + my_theme
g1

# Plot ABC buffer
g2 <- ggplot(data3, aes(y=council, x=abc_buffer)) +
  geom_boxplot(outlier.size=0.5, lwd=0.2) +
  # Labels
  labs(x="ABC buffer\n(ABC / OFL)", y="", tag="B") +
  # Limits
  lims(x=c(0,100)) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y=element_blank())
g2

# Plot ACL buffer
g3 <- ggplot(data3, aes(y=council, x=acl_buffer)) +
  geom_boxplot(outlier.size=0.5, lwd=0.2) +
  # Labels
  labs(x="ACL buffer\n(ACL / ABC)", y="", tag="C") +
  # Limits
  lims(x=c(0,100)) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y=element_blank())
g3

# Plot ACT buffer
g4 <- ggplot(data3, aes(y=council, x=act_buffer)) +
  geom_boxplot(outlier.size=0.5, lwd=0.2) +
  # Labels
  labs(x="ACT buffer\n(ACT / ACL)", y="", tag="D") +
  # Limits
  lims(x=c(0,100)) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y=element_blank())
g4

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, g4, nrow=1,
                             widths=c(0.3, rep((1-0.3)/3, 3)))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig5_abc_buffers_by_council.png"),
       width=6.5, height=2.5, units="in", dpi=600)


