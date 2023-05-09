# ****************************************
# OTOLITH DATA PREPARATION FOR ANALYSIS **
# ****************************************

# INFO --------------------------------------------------------------------

# PROJECT: FWO PHD - WP1

# Prepare otolith data for analysis

# Input: 
## otolith data from ILVO (including IFREMER pictures) and WUR
### ILVO: Smartlab
### WUR: dutch_sole_otolith_increments_20090326.xls | in D:/OneDrive - UGent/data/WP1/otolith/@raw

## Temperature and Fishing data
### Temperature: isimip_sbt_ices.rds | in D:/OneDrive - UGent/data/Env/Sea Temperature_ISIMIP3_0.5deg_1850-2014
### Temperature data is processed from script: isimip_thetao_process.R

### Fishing data: ICES_StockAssessment_2021.csv | in D:/OneDrive - UGent/data/ICES
### Fishing data is collated from ICES stock assessment reports (North Sea, Irish Sea, Bay of Biscay)

# Output: otolith data with information of otolith growth, temperature, and fishing data
## the data has potential outliers but are remained to be removed in data exploration step

# 1. SETUP ----------------------------------------------------------------

# load packages
library(tidyverse) # process data frame
library(readr)     # process csv, rds
library(readxl)    # read xlxs
library(lubridate) # process time

# dir otl
dir_otl <- "D:/OneDrive - UGent/data/WP1/otolith"

# load function
dir_function <- "D:/OneDrive - UGent/FWO/@FWO/01_fwo_phd/fwo_phd_git/Data/otolith/function"

## function to get data from SmartLab
source(file.path(dir_function,"getdata_SmartLab.R"))

## function to transform SmartLab data into AnnulusDiameter and AnnulusDiameterIncrement
source(file.path(dir_function,"transformdata_SmartLab.R"))


# 2. LOAD AND PROCESS DATA ------------------------------------------------

## 2.1. ILVO DATA  ----------------------------------------------------------------

### 2.1.1. Extract data from Smartlab ---------------------------------------

# Set variables to get SmartLab data
lab <- 'OTL'  
project <- 'SEAWISE'
year <- 2022

# metadata info
DsOtl <- loadDataOtl(lab, project, year)

# reading info
DsAgeReadingDetail <- loadAgeReadingDetail(lab, project, year)

# link DsOtl and DsAgeReadingDetail via Outcome_ID -> otl_ilvo
otl_smartlab <- left_join(DsOtl, DsAgeReadingDetail, by = c("Outcome_ID" = "OutcomeID") ) %>% select(-LineIndex, -DotShape)
otl_smartlab <- otl_smartlab %>% filter(is.na(Sample_Number) == F, is.na(Outcome_ID) == F, Outcome_Result != 0) # remove NA values

# save otl_smartlab
write_rds(otl_smartlab, file.path(dir_otl, "otl_smartlab.rds"))

### 2.1.2. Process main data (otl_ilvo) -----------------------------------------------------

#### 2.1.2.1. Transform Smartlab data into actual measurements ------------------------

# 1. keep only data read by and Kelly (8ab)
list_8ab <- grep("8AB", unique(otl_smartlab$SampleSet_Code), value = TRUE)

otl_ilvo <- otl_smartlab %>% filter(Outcome_LabTechnician == "CLO\\kdiaz", 
                                     SampleSet_Code %in% list_8ab) 

# 2. transform SmartLab data into AnnulusDiameter and AnnulusDiameterIncrement 
otl_ilvo <- SmartDots_to_OtolithIncrement(otl_ilvo)

#### 2.1.2.2. Add metadata info and change field names ------------------------

# 4. add information from metadata
# note Sample_IcesDivison was set to 8A for all 8AB, for correct area need to link to SmartFish

# Add SAM.Date from metadata because 
# 1) Sample_CatchDate is in unfriendly date format dd/mm/yyyy (that needs transformation)
# 2) Sample_CatchDate has NA values due to NA HAU.HaulTime so use TRI.DepartureDate as sampling date SAM.Date
# 3) SAM.Date is used at Year and Month level so uncertainty in day is acceptable

metadata_otl_ilvo <- read_rds(file.path("D:/OneDrive - UGent/data/WP1/otolith/@processed","sol_select_full.rds"))
metadata_otl_ilvo <- metadata_otl_ilvo %>% filter(UniqueID %in% otl_ilvo$Sample_NumberExternal) 

# change date - NA SAM.Date use TRI.Departure date
metadata_otl_ilvo <- metadata_otl_ilvo %>% mutate(SAM.Date = if_else(is.na(SAM.Date) == T,
                                                                     as_date(sub("\\|.*", "", TRI.Date)),
                                                                     SAM.Date)) 

metadata_otl_ilvo <- metadata_otl_ilvo %>% select(SAM.SpeciesFaoCode:SPA.Age,
                                                  TRI.Date:UniqueID, SPE.Length:SPE.Sex)

# join info from metadata into otl_ilvo
otl_ilvo <- left_join(otl_ilvo, metadata_otl_ilvo, by = c("Sample_NumberExternal" = "UniqueID"))

# 6. arrange field names
## calculate reading age at capture
AgeAtCapture <- otl_ilvo %>% group_by(Sample_NumberExternal) %>% summarize(AgeAtCapture = max(DotIndex))
otl_ilvo <- left_join(otl_ilvo, AgeAtCapture)

## arrange field name
otl_ilvo <- otl_ilvo %>% mutate(FishID = Sample_NumberExternal,
                                SmartlabNumber = Sample_Number,
                                SpeciesFaoCode = SAM.SpeciesFaoCode,
                                OtolithProcessingMethod = Analysis_ProcessingMethod,
                                AQCode = Outcome_Quality,
                                Scale.pixelpermm = File_Scale,
                                IcesArea = HAU.IcesArea,
                                IcesAreaGroup = HAU.IcesAreaGroup,
                                TripDate = TRI.Date,
                                SamplingDate = SAM.Date,
                                SamplingYear = TRI.Year,
                                Cohort = SamplingYear - AgeAtCapture,
                                AgeAtCapture = AgeAtCapture,
                                Length.mm = SPE.Length,
                                Weight.g = SPE.Weight,
                                Sex = SPE.Sex,
                                Age = DotIndex,
                                AgeAtCapture.Database = SPA.Age,
                                GrowingYear = Cohort + Age - 1, #Growing year (Age 1 - Yearclass 1970 -> growing period in year 1970)
                                AnnulusDiameter.um = round(AnnulusDiameter.um, 2),
                                AnnulusDiameterIncrement.um = round(AnnulusDiameterIncrement.um, 2),
                                OtolithWidth.um = round(OtolithWidth.um, 2),
                                DataSource = "ILVO")

otl_ilvo <- otl_ilvo %>% select(FishID:GrowingYear, AgeAtCapture, AnnulusDiameter.um:OtolithWidth.um, DataSource)

## change OtolithProcessingMethod 
### 1. more explicit: B&B -> broken/burnt; S&S -> sectioned/stained
### 2. change ifremer otolith to: sectioned
otl_ilvo <- otl_ilvo %>% mutate(OtolithProcessingMethod = if_else(OtolithProcessingMethod == "B&B", "broken/burnt", "sectioned/stained"))
otl_ilvo <- otl_ilvo %>% mutate(OtolithProcessingMethod = if_else(FishID %in% list_ifremer, "sectioned", OtolithProcessingMethod))


#### 2.1.2.3. Pre-explore data ------------------------

# 3.0 check negative increment
neg_increment <- otl_ilvo %>% filter(AnnulusDiameterIncrement.um <= 0) %>% 
  select(IcesAreaGroup, SamplingDate, Scale.pixelpermm, SmartlabNumber, Age, AnnulusDiameter.um:OtolithWidth.um)

# 3.1. check for NA increment (due to error in aging annotation, or comment (same LONG/SHORT))
na_increment <- otl_ilvo %>% filter(is.na(AnnulusDiameter.um) == T) %>% 
  select(IcesAreaGroup, SamplingYear, SmartlabNumber)

# 3.2. check for NA File_Scale 
na_scale <- otl_ilvo %>% filter(is.na(Scale.pixelpermm) == T) %>% 
  select(IcesAreaGroup, SamplingYear, SmartlabNumber)

# 3.3. check for unusual scale
## list ifremer data
list_ifremer <- unique(filter(otl_ilvo, str_detect(FishID, "RE|AL|CO") == TRUE)$FishID)

# usual scale zeiss: 134, 172, 217, 219, 269, 270, 272, 345
# usual scale leica: 429, 437, 545, 593
sort(unique(filter(otl_ilvo, !FishID %in% list_ifremer)$Scale.pixelpermm)) 

# usual scale ifremer: 182, 212, 223, 224, 220, 226, 228, 228.5, 232, 260, 267, 279 
sort(unique(filter(otl_ilvo, FishID %in% list_ifremer)$Scale.pixelpermm)) 

# 3.4. check for NA Outcome_Quality
na_quality <- otl_ilvo %>% filter(is.na(AQCode) == T)

# 3.5. Check for outliner (due to wrong assignment of scale/reading error)
ggplot() +
  geom_point(data = otl_ilvo, aes(x = Length.mm, y = AnnulusDiameter.um), alpha = 0.5) + 
  facet_grid(~ Age)

## 2.2. REMOVE INCOMPLETE GROWTH INCREMENT ----------------------------------------------------

# incomplete growth increments are increment that are the increment in a year which is 
# measured but not completed as the winter ring of that growth year

# incomplete growth increments are the last increment in fish sampled in the first quarter (Jan - March)
# in April, it is very likely that the increment is incomplete so will not be included in analysis
# in May and June, it is ambiguous so the otoliths will be checked carefully at image reading stage
# it is also assumed that growth is completed in May and June

# workflow to remove incomplete growth increment
# create increment id -> extract incomplete growth -> remove incomplete growth

# 1. create increment id
otl_ilvo <- otl_ilvo %>% rowid_to_column("IncrementID")

# 2. extract incomplete growth increments
otl_incomplete <- otl_ilvo %>% filter(month(SamplingDate) <= 4, AgeAtCapture == Age) 

# 3. remove incomplete growth
otl_ilvo <- otl_ilvo %>% filter(!(IncrementID %in% otl_incomplete$IncrementID)) 
otl_ilvo <- otl_ilvo %>% select(-IncrementID)

## 2.3. ADD TEMPERATURE AND FISHING DATA ----------------------------------------------------

### 2.3.1. TEMPERATURE (SEA BOTTOM TEMPERATURE) -----------------------------
# ISIMIP Sea Bottom Temperature - processed in isimip_thetao_process.R

dir_sbt <- "D:/OneDrive - UGent/data/Env/Sea Temperature_ISIMIP3_0.5deg_1850-2014"
sbt_ices <- read_rds(file.path(dir_sbt, "isimip_sbt_ices.rds"))

# pre-process sbt_ices to merge 
sbt_ices <- sbt_ices %>% group_by(IcesArea, Year) %>% summarize(SeaBottomTemperature.degC = mean(isimip_sbt, na.rm = T))

# add SeaBottomTemperature to otl data
otl_ilvo <- left_join(otl_ilvo, sbt_ices, by = c("IcesAreaGroup" = "IcesArea", "GrowingYear" = "Year"))

### 2.3.2. FISHING MORTALITY, STOCK BIOMASS -----------------------------

# load ices data
dir_ices <- "D:/OneDrive - UGent/data/ICES"

ices_4 <- read_excel(file.path(dir_ices, "WGNSSK 2021_4_summary.xlsx")) %>% mutate(IcesAreaGroup = "4bc")
ices_7a <- readxl::read_excel(file.path(dir_ices, "WGCSE2021_7a_summary.xlsx")) %>% mutate(IcesAreaGroup = "7a")
ices_8ab <- readxl::read_excel(file.path(dir_ices, "WGBIE 2021_8ab_summary.xlsx")) %>% mutate(IcesAreaGroup = "8ab")

ices <- bind_rows(ices_4, ices_7a, ices_8ab)

# rename filds
ices <- ices %>% 
  mutate(SpawningStockBiomass.1000t = SSB_t/1000,
         FishingMortality = as.numeric(Fbar),
         GrowingYear = Year) %>%
  select(IcesAreaGroup, GrowingYear, SpawningStockBiomass.1000t, FishingMortality)

# Add FishingMortality and SpawningStockBiomass to otl_ilvo 
otl_ilvo <- left_join(otl_ilvo, ices, by = c("IcesAreaGroup", "GrowingYear"))

# 3. SAVE DATA ---------------------------------------------------------------
write_rds(otl_ilvo, file.path(dir_otl, "otl_ilvo.rds"))

