
# Clear workspace
rm(list = ls())
options(dplyr.summarise.inform=F)

# Setup
################################################################################

# Packages
library(lubridate)
library(tidyverse)

# Directories
tabledir <- "tables"
plotdir <- "data/stock_smart/figures"
inputdir <- "data/stock_smart/raw"
outputdir <- "data/stock_smart/processed"

# Read data
data_orig <- readRDS(file.path(outputdir, "2021_stock_smart_data.Rds"))


# Build data
################################################################################

# Missing
add_on <- tibble(council_label=c("WPFMC", "CFMC", "CFMC", "GMFMC", "GMFMC", "NPFMC"),
                 fmp_label=c("Remote Island Areas Ecosystem", "Corals", "Queen Conch", "GOM Corals", "Red Drum", "Arctic Fish"),
                 council_type=c("Single"))

# Format data
data <- data_orig %>%
  # Reduce to most recent assessment
  filter(assessment_number==1) %>%
  # Don't include IPHC halibut
  filter(council!="IPHC") %>%
  # Format FMP short
  mutate(fmp_short=recode(fmp_short,
                          "West Coast HMS / West Pacific Pelagic Fisheries"="Pacific HMS",
                          "Summer Flounder, Scup, and Black Sea Bass"="Summer Flounder, Scup, BSB")) %>%
  # Mark co-managed FMPs
  mutate(fmp_label=ifelse(council_type=="Single", fmp_short, paste0(fmp_short, "*"))) %>%
  # Simplify councils
  mutate(council_label=recode(council,
                              "Atlantic HMS"="NEFMC",
                              "NEFMC/MAFMC"="NEFMC",
                              "SAFMC/GMFMC"="SAFMC",
                              "PFMC/WPFMC"="PFMC")) %>%
  # Order councils
  mutate(council_label=factor(council_label, levels=c("NEFMC", "MAFMC", "SAFMC", "GMFMC", "CFMC", "PFMC", "NPFMC", "WPFMC"))) %>%
  # Format assessment levels
  mutate(assessment_level=as.character(assessment_level),
         assessment_level=ifelse(is.na(assessment_level), "NA", assessment_level),
         assessment_level=recode_factor(assessment_level,
                                        "NA"="Not provided",
                                        "0"="0-Catch-only",
                                        "1"="1-Survey data",
                                        "2"="2-Simple model",
                                        "3"="3-Production model",
                                        "4"="4-Age-structured model",
                                        "5"="5-Includes ecosystem considerations"))

# Build FMP order key
fmp_order_key <- data %>%
  # Calculate sample size
  group_by(council_label, council_type, fmp_label) %>%
  summarize(n=n()) %>%
  ungroup() %>%
  # Add add ons
  bind_rows(add_on) %>%
  mutate(n=ifelse(is.na(n), 0, n)) %>%
  # ORder
  arrange(council_label, council_type, n)

# Build stats
stats <- data %>%
  # Count by FMP
  group_by(council_type, council_label, fmp_label, assessment_level) %>%
  summarize(n=n()) %>%
  ungroup() %>%
  # Percent by FMP
  group_by(council_label, fmp_label) %>%
  mutate(prop=n/sum(n)) %>%
  # Add add ons
  bind_rows(add_on ) %>%
  # Order FMPs
  mutate(council_label=factor(council_label, levels=c("NEFMC", "MAFMC", "SAFMC", "GMFMC", "CFMC", "PFMC", "NPFMC", "WPFMC")),
         fmp_label=factor(fmp_label, levels=fmp_order_key$fmp_label))

# Build lines for seperating single and co-managed FMPS
sep_lines <- stats %>%
  filter(council_type=="Multiple") %>%
  group_by(council_label) %>%
  summarize(n_co=n_distinct(fmp_label)) %>%
  ungroup() %>%
  filter(n_co>0)

# Add on formatted
add_on1 <- add_on %>%
  mutate(council_label=factor(council_label, levels=levels(stats$council_label)),
         fmp_label=factor(fmp_label, levels=levels(stats$fmp_label)))


# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=9),
                   axis.title.y=element_blank(),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=9),
                   strip.text=element_text(size=8),
                   plot.title=element_blank(),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position="bottom")

# Plot data
n_levels <- nlevels(stats$assessment_level)
g1 <- ggplot(stats, aes(x=n, y=fmp_label, fill=assessment_level)) +
  facet_grid(council_label~., scales="free", space="free") +
  geom_bar(stat="identity", color="grey30", lwd=0.2) +
  # Ref lines
  geom_hline(data=sep_lines, mapping = aes(yintercept=n_co+0.5), linetype="dashed", lwd=0.2) +
  # Text
  geom_text(data=add_on1, mapping=aes(y=fmp_label), inherit.aes = F, x=0, hjust=0, label="Not assessed", size=2, fontface="italic") +
  # Labels
  labs(x="Number of stocks", y="") +
  # Scale
  scale_fill_manual(name="Assessment level", values=c("grey95", RColorBrewer::brewer.pal(n_levels-1, "Blues"))) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g1

# Plot data
g2 <- ggplot(stats, aes(x=prop, y=fmp_label, fill=assessment_level)) +
  facet_grid(council_label~., scales="free", space="free") +
  geom_bar(stat="identity", color="grey30", lwd=0.2) +
  # Ref lines
  geom_hline(data=sep_lines, mapping = aes(yintercept=n_co+0.5), linetype="dashed", lwd=0.2) +
  # Text
  geom_text(data=add_on1, mapping=aes(y=fmp_label), inherit.aes = F, x=0, hjust=0, label="Not assessed", size=2, fontface="italic") +
  # Labels
  labs(x="Percent of stocks", y="") +
  # Scale
  scale_x_continuous(labels = scales::percent) +
  scale_fill_manual(name="Assessment level", values=c("grey95", RColorBrewer::brewer.pal(n_levels-1, "Blues"))) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y=element_blank(),
        legend.position = "right")
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, widths=c(0.6, 0.4))
g

# Export figure
ggsave(g, filename=file.path(plotdir, "stock_smart_assessment_level.png"),
       width=9.5, height=7, units="in", dpi=600)











