
# Clear workspace
rm(list = ls())
options(dplyr.summarise.inform=F)

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
outputdir <- "database"

# Read data
data_orig <- readxl::read_excel(file.path(outputdir, "us_hcr_database.xlsx"), na="NA")



# Format data
################################################################################

# Format data
data1 <- data_orig %>%
  # Remove stocks without values
  filter(!is.na(p_star) | !is.na(abc_buffer) | !is.na(acl_buffer) | !is.na(act_buffer)) %>%
  # Simplify
  select(council, stock, p_star, abc_buffer, acl_buffer, act_buffer) %>%
  # Format council
  mutate(council=gsub(" Fishery Management Council", "", council),
         council=recode(council,
                        "NOAA"="Highly Migratory Species",
                        "Gulf of Mexico FMC/ South Atlantic FMC"="Gulf of Mexico & South Atlantic",
                        "New England FMC/ Mid-Atlantic FMC"= "New England & Mid-Atlantic"))
  # Fix buffer fractions (someone reversed these early in the process)
  # mutate(abc_buffer=1-abc_buffer,
  #        acl_buffer=1-acl_buffer,
  #        act_buffer=1-act_buffer)

# Inspect
table(data1$council)

# Make an all-council dataset
data2 <- data1 %>%
  mutate(council="All councils")

# Merge
data3 <- bind_rows(data1, data2) %>%
  # Order councils
  mutate(council=factor(council, levels=c("All councils",
                                          "New England", "Mid-Atlantic", "South Atlantic",  "Gulf of Mexico", "Caribbean",
                                          "Pacific", "North Pacific", "Western Pacific", "Highly Migratory Species",
                                          "New England & Mid-Atlantic", "Gulf of Mexico & South Atlantic") %>% rev()))

# Stats for manuscript
data3 %>%
  group_by(council) %>%
  summarize(pstar=median(p_star, na.rm=T))

# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=5),
                   axis.text.y=element_text(size=5.5),
                   axis.title=element_text(size=6),
                   axis.title.y=element_blank(),
                   plot.tag=element_text(size=7),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot pstar
g1 <- ggplot(data3, aes(y=council, x=p_star)) +
  geom_boxplot(outlier.size=0.5, lwd=0.2, fill="grey80") +
  # geom_segment(x=0.3, xend=0.5, y="Gulf of Mexico", yend="Gulf of Mexico", color="grey30") +
  # Labels
  labs(x="Probability of\noverfishing (P*)", y="", tag="A") +
  scale_x_continuous(labels=scales::percent, lim=c(0, 0.5)) +
  # Limits
  scale_y_discrete(drop=FALSE) +
  # Theme
  theme_bw() + my_theme
g1

# Plot ABC buffer
g2 <- ggplot(data3, aes(y=council, x=abc_buffer)) +
  geom_boxplot(outlier.size=0.5, lwd=0.2, fill="grey80") +
  # Labels
  labs(x="ABC buffer\n(ABC / OFL)", y="", tag="B") +
  scale_x_continuous(labels=scales::percent, lim=c(0, 1)) +
  # Limits
  scale_y_discrete(drop=FALSE) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y=element_blank())
g2

# Plot ACL buffer
g3 <- ggplot(data3, aes(y=council, x=acl_buffer)) +
  geom_boxplot(outlier.size=0.5, lwd=0.2, fill="grey80") +
  # Labels
  labs(x="ACL buffer\n(ACL / ABC)", y="", tag="C") +
  scale_x_continuous(labels=scales::percent, lim=c(0, 1)) +
  # Limits
  scale_y_discrete(drop=FALSE) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y=element_blank())
g3

# Plot ACT buffer
g4 <- ggplot(data3, aes(y=council, x=act_buffer)) +
  geom_boxplot(outlier.size=0.5, lwd=0.2, fill="grey80") +
  # Labels
  labs(x="ACT buffer\n(ACT / ACL)", y="", tag="D") +
  scale_x_continuous(labels=scales::percent, lim=c(0, 1)) +
  # Limits
  scale_y_discrete(drop=FALSE) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y=element_blank())
g4

# Merge
prop1 <- 0.37
g <- gridExtra::grid.arrange(g1, g2, g3, g4, nrow=1,
                             widths=c(prop1, rep((1-prop1)/3, 3)))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig5_abc_buffers_by_council.png"),
       width=6.5, height=2.5, units="in", dpi=600)


