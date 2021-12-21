
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
  # Format model category
  mutate(model_category=as.character(model_category),
         model_category=ifelse(is.na(model_category), fssi_stock, model_category),
         model_category=recode_factor(model_category,
                                      "N"="Non-FSSI stock",
                                      "Y"="Not provided",
                                      "1"="1-Data-limited",
                                      "2"="2-Index-based",
                                      "3"="3-Aggregate biomass dynamics",
                                      "4"="4-Virtual population analysis",
                                      "5"="5-Statistical catch-at-length",
                                      "6"="6-Statistical catch-at-age")) %>%
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
  # Format assessment frequency
  mutate(assessment_frequency=as.character(assessment_frequency),
         assessment_frequency=ifelse(is.na(assessment_frequency), fssi_stock, assessment_frequency),
         assessment_frequency=recode_factor(assessment_frequency,
                                        "N"="Non-FSSI stock",
                                        "Y"="Not provided",
                                        "0"="0-Never",
                                        "1"="1-Infrequent (>3 yrs)",
                                        "2"="2-Frequent (≤3 yrs)",
                                        "3"="3-Annual or more (≤1 yr)")) %>%
  # Format biological input data
  mutate(biological_input_data=as.character(biological_input_data),
         biological_input_data=ifelse(is.na(biological_input_data), fssi_stock, biological_input_data),
         biological_input_data=recode_factor(biological_input_data,
                                             "N"="Non-FSSI stock",
                                             "Y"="Not provided",
                                             "0"="0-None",
                                             "1"="1-All from proxies",
                                             "2"="2-Some empirically derived",
                                             "3"="3-Most empirically derived",
                                             "4"="4-Time-varying growth",
                                             "5"="5-Time/space-varying parameters")) %>%
  # Format life history data
  mutate(life_history_data=as.character(life_history_data),
         life_history_data=ifelse(is.na(life_history_data), fssi_stock, life_history_data),
         life_history_data=recode_factor(life_history_data,
                                         "N"="Non-FSSI stock",
                                         "Y"="Not provided",
                                         "0"="0-None",
                                         "1"="1-Size composition",
                                         "2"="2-Basic demographic rates",
                                         "3"="3-Seasonal/spatial patterns",
                                         "4"="4-Food habits information")) %>%
  # Format composition input data
  mutate(composition_input_data=as.character(composition_input_data),
         composition_input_data=ifelse(is.na(composition_input_data), fssi_stock, composition_input_data),
         composition_input_data=recode_factor(composition_input_data,
                                              "N"="Non-FSSI stock",
                                              "Y"="Not provided",
                                              "0"="0-None",
                                              "1"="1-Some but not enough for models",
                                              "2"="2-Enough for data-limited models",
                                              "3"="3-Enough for size-structured models",
                                              "4"="4-Enough for age-structured models",
                                              "5"="5-Very complete age/size data")) %>%
  # Format abundance data
  mutate(abundance_data=as.character(abundance_data),
         abundance_data=ifelse(is.na(abundance_data), fssi_stock, abundance_data),
         abundance_data=recode_factor(abundance_data,
                                         "N"="Non-FSSI stock",
                                         "Y"="Not provided",
                                         "0"="0-None",
                                         "1"="1-CPUE",
                                         "2"="2-CPUE w/ age composition",
                                         "3"="3-Rigorous research survey",
                                         "4"="4-Habitat-specifc research survey")) %>%
  # Format abundance input data
  mutate(abundance_input_data=as.character(abundance_input_data),
         abundance_input_data=ifelse(is.na(abundance_input_data), fssi_stock, abundance_input_data),
         abundance_input_data=recode_factor(abundance_input_data,
                                            "N"="Non-FSSI stock",
                                            "Y"="Not provided",
                                            "0"="0-None",
                                            "1"="1-Fishery-dependent CPUE (low quality)",
                                            "2"="2-Fishery-dependent CPUE (high quality)",
                                            "3"="3-Fishery-independent CPUE (limited)",
                                            "4"="4-Fishery-independent CPUE (expansive)",
                                            "5"="5-Fishery-independent CPUE (calibrated)")) %>%
  # Format catch data
  mutate(catch_data=as.character(catch_data),
         catch_data=ifelse(is.na(catch_data), fssi_stock, catch_data),
         catch_data=recode_factor(catch_data,
                                  "N"="Non-FSSI stock",
                                  "Y"="Not provided",
                                  "0"="0-None",
                                  "1"="1-Landed catch",
                                  "2"="2-Size composition",
                                  "3"="3-Spatial catch info",
                                  "4"="4-Age composition",
                                  "5"="5-Complete removals")) %>%
  # Format catch input data
  mutate(catch_input_data=as.character(catch_input_data),
         catch_input_data=ifelse(is.na(catch_input_data), fssi_stock, catch_input_data),
         catch_input_data=recode_factor(catch_input_data,
                                  "N"="Non-FSSI stock",
                                  "Y"="Not provided",
                                  "0"="0-None",
                                  "1"="1-Large data gaps",
                                  "2"="2-Some data gaps",
                                  "3"="3-Few data gaps",
                                  "4"="4-No data gaps (but uncertainty)",
                                  "5"="5-Very complete knowledge")) %>%
  # Format ecosystem linkage
  mutate(ecosystem_linkage=as.character(ecosystem_linkage),
         ecosystem_linkage=ifelse(is.na(ecosystem_linkage), fssi_stock, ecosystem_linkage),
         ecosystem_linkage=recode_factor(ecosystem_linkage,
                                         "N"="Non-FSSI stock",
                                         "Y"="Not provided",
                                         "0"="0-None",
                                         "1"="1-Inform structure or input processing",
                                         "2"="2-Variability but not explicit about source",
                                         "3"="3-≥1 dynamically-linked feature",
                                         "4"="4-≥1 dynamic feature supported by process study",
                                         "5"="5-Full approach dynamically-linked")) %>%
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



# Plot function
################################################################################

# Plot data
col_name <- "assessment_level"; legend_title <- "Assessment level"
plot_data <- function(col_name, legend_title){

  # Build stats
  ##############################################

  # Rename a specific colums
  data3 <- data2
  col_num <- which(colnames(data3)==col_name)
  colnames(data3)[col_num] <- "plot_group"

  # Build stats
  stats <- data3 %>%
    # Count by FMP
    group_by(fmp_mgmt, council_label, fmp_label, plot_group) %>%
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
  ##############################################

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
  n_levels <- nlevels(stats$plot_group)
  g1 <- ggplot(stats, aes(x=n, y=fmp_label, fill=plot_group)) +
    facet_grid(council_label~., scales="free", space="free") +
    geom_bar(stat="identity", color="grey30", lwd=0.2) +
    # Ref lines
    geom_hline(data=sep_lines, mapping = aes(yintercept=n_co+0.5), linetype="dashed", lwd=0.2) +
    # Text
    geom_text(data=add_on1, mapping=aes(y=fmp_label), inherit.aes = F, x=0, hjust=0, label="Not assessed", size=2, fontface="italic") +
    # Labels
    labs(x="Number of stocks", y="") +
    # Scale
    scale_fill_manual(name=legend_title, values=c("grey50", "grey95", RColorBrewer::brewer.pal(n_levels-1, "Blues"))) +
    # Theme
    theme_bw() + my_theme +
    theme(legend.position = "none")
  g1

  # Plot data
  g2 <- ggplot(stats, aes(x=prop, y=fmp_label, fill=plot_group)) +
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
    scale_fill_manual(name=legend_title, values=c("grey50", "grey95", RColorBrewer::brewer.pal(n_levels-1, "Blues"))) +
    # Theme
    theme_bw() + my_theme +
    theme(axis.text.y=element_blank(),
          legend.position = "right")
  g2

  # Merge
  g <- gridExtra::grid.arrange(g1, g2, widths=c(0.6, 0.4))
  g
  print(g)

  # Export figure
  outfig <- paste0("stock_smart_", tolower(legend_title) %>% gsub(" ", "_", .), ".png")
  ggsave(g, filename=file.path(plotdir,   outfig),
         width=9.5, height=7, units="in", dpi=600)

}


# Plot a bunch
plot_data(col_name="model_category", legend_title = "Model category")
plot_data(col_name="assessment_level", legend_title = "Assessment level")
plot_data(col_name="assessment_frequency", legend_title = "Assessment frequency")
plot_data(col_name="life_history_data", legend_title = "Life history data")
plot_data(col_name="biological_input_data", legend_title = "Biological input data")
plot_data(col_name="abundance_data", legend_title = "Abundance data")
plot_data(col_name="abundance_input_data", legend_title = "Abundance input data")
plot_data(col_name="catch_data", legend_title = "Catch data")
plot_data(col_name="catch_input_data", legend_title = "Catch input data")
plot_data(col_name="composition_input_data", legend_title = "Composition input data")
plot_data(col_name="ecosystem_linkage", legend_title = "Ecosystem linkage")




