
# Clear workspace
rm(list = ls())

# Turn off scientific notation
options(scipen=999)

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "data/ram"

# Read RAM Legacy Database v4.491
load("/Users/cfree/Dropbox/Chris/UCSB/data/ramldb/RAM v4.491 Files (1-14-20)/RAM v4.491/DB Files With Assessment Data/R Data/DBdata[asmt][v4.491].Rdata")


# Build stock key
################################################################################

# Identify most recent assessments
assessments <- assessment %>% 
  # Reduce to most recent
  filter(mostrecent %in% c(999, -1)) %>% 
  # Simplify
  select(stockid, assessid, assessorid, assessmethod, assessyear) %>% 
  # Rename
  rename(assess_range=assessyear) %>% 
  # Extract last year
  mutate(assess_year=substr(assess_range, 6, 10) %>% as.numeric())

# All unique? Yes!
anyDuplicated(assessments$stockid)
anyDuplicated(assessments$assessid)
  
# Build stock key
stock_key <- stock %>% 
  # Reduce to most recent assessment
  filter(stockid %in% assessments$stockid) %>% 
  # Elimintate useless columns
  select(-c(tsn, inmyersdb, myersstockid)) %>% 
  # Add area name
  left_join(select(area, areaid, country, areaname), by="areaid") %>% 
  # Add assessment info
  left_join(assessments, by="stockid") %>% 
  rename(assessor_id=assessorid, assess_method=assessmethod) %>% 
  # Add family name
  left_join(taxonomy %>% select(family, scientificname)) %>% 
  # Rename columns
  rename(species=scientificname, comm_name=commonname, area=areaname) %>% 
  # Format columns
  mutate(comm_name=freeR::sentcase(comm_name),
         species=gsub("spp.", "spp", species),
         species=recode(species, 
                        "Chrysophrys auratus"="Pagrus auratus",
                        "Clupea pallasii"="Clupea pallasii pallasii",
                        "Epinephelus flavolimbatus"="Hyporthodus flavolimbatus",
                        "Epinephelus niveatus"="Hyporthodus niveatus",
                        "Etrumeus teres"="Etrumeus sadina",
                        "Loligo bleekeri"="Heterololigo bleekeri",
                        "Loligo pealeii"="Doryteuthis pealeii",
                        "Merluccius gayi"="Merluccius gayi gayi",
                        "Mullus barbatus"="Mullus barbatus barbatus",
                        "Neoplatycephalus richardsoni"="Platycephalus richardsoni",
                        "Psetta maxima"="Scophthalmus maximus",
                        "Tetrapturus albidus"="Kajikia albida",
                        "Sardinops melanostictus"="Sardinops sagax",
                        "Clupea bentincki"="Strangomera bentincki",
                        "Raja binoculata"="Beringraja binoculata",
                        "Raja rhina"="Beringraja rhina",
                        "Theragra chalcogramma"="Gadus chalcogrammus")) %>% 
  # Rearrange columns
  select(stockid, stocklong, 
         assessid, assessor_id, assess_method,
         country, region, area, 
         family, species, comm_name) %>% 
  # Filter
  filter(country=="USA")

# Check names
freeR::check_names(stock_key$species)

# Export stock key
write.csv(stock_key, file=file.path(datadir, "RAM_stock_key_usa.csv"), row.names=F)

