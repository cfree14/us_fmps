
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

# To-do list
# Move a lot of formatting below to format file

# Build data
################################################################################

# Missing
add_on <- tibble(council_label=c("WPFMC",
                                 "SAFMC", "SAFMC", "SAFMC", "SAFMC",
                                 "CFMC", "CFMC",
                                 "GMFMC", "GMFMC",
                                 "NPFMC"),
                 fmp_label=c("Remote Island Areas Ecosystem",
                             "SA Corals", "Dolphin & Wahoo", "Golden Crab", "Sargassum",
                             "Corals", "Queen Conch",
                             "GOM Corals", "Red Drum",
                             "Arctic Fish"),
                 fmp_mgmt=c("Single"))

# Format data
data <- data_orig %>%
  # Reduce to most recent assessment
  filter(assessment_number==1) %>%
  # Don't include IPHC halibut
  filter(council!="IPHC") %>%
  # Format FMP short
  mutate(fmp_short=recode(fmp_short,
                          "Summer Flounder, Scup, and Black Sea Bass"="Summer Flounder, Scup, BSB")) %>%
  # Format assessment levels
  mutate(assessment_level=as.character(assessment_level),
         assessment_level=ifelse(is.na(assessment_level), fssi_stock, assessment_level),
         assessment_level=recode_factor(assessment_level,
                                        "N"="Non-FSSI stock",
                                        "Y"="Not provided",
                                        "0"="0-Catch-only",
                                        "1"="1-Survey data",
                                        "2"="2-Simple model",
                                        "3"="3-Production model",
                                        "4"="4-Age-structured model",
                                        "5"="5-Includes ecosystem considerations")) %>%
  # Arrange
  select(-council_type) %>%
  select(council, fmp, fmp_short, everything())

# Inspect
colnames(data)

# FMP key
fmp_key <- data %>%
  group_by(council, fmp_short) %>%
  summarize(n=n())

# Break out stocks managed by multiple FMPs
fmp_shorts <- sort(unique(data$fmp_short))
data_exp <- purrr::map_df(fmp_shorts, function(x){

  # Get FMP short
  fmp_short_do <- x
  fdata <- data %>%
    filter(fmp_short==fmp_short_do)

  # Get council
  council_orig <- fdata$council %>% unique()

  # Break apart, if necessary
  break_yn <- grepl("/", fmp_short_do)
  if(break_yn){

    # Split FMPs/councils
    fmp_shorts2 <- strsplit(fmp_short_do, split="/")[[1]]
    councils <- strsplit(council_orig, split="/")[[1]]
    if(length(councils)==1){councils <- rep(councils, length(fmp_shorts2))}
    fmp_council_key <- tibble(council=councils,
                              fmp_short=fmp_shorts2)

    # Overwrite FMP and council
    outdata <- purrr::map_df(1:nrow(fmp_council_key), function(y){
      fdata2 <- fdata %>%
        mutate(fmp_short=fmp_council_key$fmp_short[y],
               council=fmp_council_key$council[y],
               council_label=fmp_council_key$council[y])
    })

  }else{
    outdata <- fdata
  }

})

# FMPs that are co-managed
fmps_comanaged <- c("Atlantic HMS", "Monkfish", "Spiny Dogfish",
                    "Coastal Migratory Pelagics", "GOM & S. Atlantic Spiny Lobster",
                    "Pacific HMS")
fmps_with_other_fmps <- c("BSAI Groundfish", "GOA Groundfish", "Pacific HMS", "Pelagic Fisheries", "Snapper-Grouper", "GOM Reef Fish")
sort(unique(data$fmp_short))

# Format expanded data
data2 <- data_exp %>%
  # Simplify councils
  mutate(council_label=recode(council,
                              "NEFMC/MAFMC/SAFMC/GFMC/CFMC"="NEFMC",
                              "NEFMC/MAFMC"="NEFMC",
                              "SAFMC/GMFMC"="SAFMC",
                              "PFMC/WPFMC"="PFMC")) %>%
  # Order councils
  mutate(council_label=factor(council_label, levels=c("NEFMC", "MAFMC", "SAFMC", "GMFMC", "CFMC", "PFMC", "NPFMC", "WPFMC"))) %>%
  # Mark FMPs managed between multiple councils
  mutate(fmp_mgmt=ifelse(fmp_short %in% fmps_comanaged, "Multi-council", "Single council")) %>%
  # Mark FMPs with stocks managed in other FMPs
  mutate(fmp_n=ifelse(fmp_short %in% fmps_with_other_fmps, "multiple", "single")) %>%
  # Mark FMPs managed between multiple councils or with with stocks managed between multiple FMPs
  mutate(fmp_label=ifelse(fmp_mgmt!="Multi-council", fmp_short, paste0(fmp_short, "*")),
         fmp_label=ifelse(fmp_n!="multiple", fmp_label, paste0(fmp_label, "†"))) %>%
  # Arrange
  select(council, council_label, fmp, fmp_short, fmp_label, fmp_mgmt, everything())

# Build FMP order key
fmp_order_key <- data2 %>%
  # Calculate sample size
  group_by(council_label, fmp_mgmt, fmp_label) %>%
  summarize(n=n()) %>%
  ungroup() %>%
  # Add add ons
  bind_rows(add_on) %>%
  mutate(n=ifelse(is.na(n), 0, n)) %>%
  # Order
  arrange(council_label, fmp_mgmt, n)

# Build stats
stats <- data2 %>%
  # Count by FMP
  group_by(fmp_mgmt, council_label, fmp_label, assessment_level) %>%
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
  filter(fmp_mgmt=="Multi-council") %>%
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
  scale_fill_manual(name="Assessment level", values=c("grey50", "grey95", RColorBrewer::brewer.pal(n_levels-1, "Blues"))) +
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
  scale_fill_manual(name="Assessment level", values=c("grey50", "grey95", RColorBrewer::brewer.pal(n_levels-1, "Blues"))) +
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











