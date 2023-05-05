# Prepare 8ab data

# 1. SETUP ----------------------------------------------------------------------

library(tidyverse)  # process data frame
library(readr)      # process rds, csv 
library(lubridate)  # process time
library(readxl)     # read xlsx


# 2.1 OTOLITH  ---------------------------------------------------------------------

dir_otl <- "D:/OneDrive - UGent/FWO/@FWO/05_MSc thesis/2022_Kelly/git_kelly/data"
otl_8ab <- read_rds(file.path(dir_otl, "otl_8ab_130223.rds"))

# REMOVE INCOMPLETE GROWTH INCREMENT

# incomplete growth increments are increment that are the increment in a year which is 
# measured but not completed as the winter ring of that growth year

# incomplete growth increments are the last increment in fish sampled in the first quarter (Jan - March)
# in April, it is very likely that the increment is incomplete so will not be included in analysis
# in May and June, it is ambiguous so the otoliths will be checked carefully at image reading stage

# workflow to remove incomplete growth increment
# create increment id -> extract incomplete growth -> remove incomplete growth

# 1. create increment id
otl_8ab <- otl_8ab %>% rowid_to_column("IncrementID")

# 2. extract incomplete growth increments
otl_incomplete <- otl_8ab %>% filter(month(SamplingDate) <= 4, AgeAtCapture == Age) 

# 3. remove incomplete growth
otl <- otl_8ab %>% filter(!(IncrementID %in% otl_incomplete$IncrementID)) 
otl <- otl %>% select(-IncrementID)


# 2.2. EXTERNAL DATA ------------------------------------------------------

# 2.2.1. TEMPERATURE (SEA BOTTOM TEMPERATURE) -----------------------------
# ISIMIP/MPIESM Sea Bottom Temperature - processed in mpiesm_data processing.R

dir_sbt <- "D:/OneDrive - UGent/FWO/@FWO/05_MSc thesis/2022_Kelly/git_kelly/data"
sbt_ices <- read_rds(file.path(dir_sbt, "isimip_sbt_ices.rds"))

# average 
sbt_ices <- sbt_ices %>% group_by(IcesArea, Year) %>% summarize(SeaBottomTemperature.degC = mean(isimip_sbt, na.rm = T))

# add SeaBottomTemperature to otl data
otl <- left_join(otl, sbt_ices, by = c("IcesAreaGroup" = "IcesArea", "GrowingYear" = "Year"))


# 2.2.2. FISHING MORTALITY, STOCK BIOMASS -----------------------------

dir_ices <- "D:/OneDrive - UGent/FWO/@FWO/05_MSc thesis/2022_Kelly/git_kelly/data"
ices <- read_delim(file.path(dir_ices, "ICES_StockAssessment_2021.csv"), delim = ";")

# fishing mortality 
f.age_7a <- read_excel(file.path(dir_ices, "WGCSE2021_7a_F at age.xlsx"))
f.age_4 <- read_excel(file.path(dir_ices, "WGNSSK 2021_4_F at age.xlsx"))
f.age_8ab <- read_excel(file.path(dir_ices, "WGBIE 2021_8ab_F at age.xlsx"))

# biomass
biomass_7a <- read_excel(file.path(dir_ices, "WGCSE2021_7a_biomass juvenile summary.xlsx"))
biomass_4 <- read_excel(file.path(dir_ices, "WGNSSK 2021_4_biomass juvenile summary.xlsx"))
biomass_8ab <- read_excel(file.path(dir_ices, "WGBIE 2021_8ab_biomass juvenile summary.xlsx"))

# combine ices fishing data
ices_7a <- left_join(f.age_7a, biomass_7a, by = "Year")
ices_4 <- left_join(f.age_4, biomass_4, by = "Year") 
ices_8ab <- left_join(f.age_8ab, biomass_8ab, by = "Year") 

ices_7a <- ices_7a %>% mutate(IcesArea = "7a")
ices_4 <- ices_4 %>% mutate(IcesArea = "4bc")
ices_8ab <- ices_8ab %>% mutate(IcesArea = "8ab")

ices <- bind_rows(ices_7a, ices_4, ices_8ab)

ices <- ices %>% mutate(FishingMortality = Fbar,
                        SpawningStockBiomass.1000t = SSB/1000) 


# Add FishingMortality to otl data
ices_f.bar <- ices %>% select(IcesArea, Year, FishingMortality) 
otl <- left_join(otl, ices_f.bar, by = c("IcesAreaGroup" = "IcesArea", "GrowingYear" = "Year"))

# Add biomass to otl data
ices_biomass <- ices %>% select(IcesArea, Year, SpawningStockBiomass.1000t)
otl <- left_join(otl, ices_biomass, by = c("IcesAreaGroup" = "IcesArea", "GrowingYear" = "Year"))

# save file
write_rds(otl, file.path(dir_otl,"otl_8ab_full for analysis_130223.rds"))
