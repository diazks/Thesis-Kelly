# ****************************************
# OTOLITH DATA PREPARATION FOR ANALYSIS **
# ****************************************

# INFO --------------------------------------------------------------------

# PROJECT: FWO PHD - WP1
# Tuan Anh Bui (27/06/2022)

# Prepare otolith data for analysis

# Input: otolith data from ILVO, IFREMER, WUR
# - ILVO: Smartlab
# - IFREMER: Smartlab (metadata needs process)
# - WUR

# Output: otolith data with year, cohort, icesarea, otolith diameter, otolith increment, otolith width



# 1. SETUP ----------------------------------------------------------------

library(tidyverse) # process data frame
library(readr)     # process csv, rds
library(readxl)    # read xlxs
library(lubridate) # process time

# 2. LOAD AND PROCESS DATA ------------------------------------------------

# 2.1. ILVO  ----------------------------------------------------------------

## LOAD FUNCTIONS

# function dir 
dir_otl_git <- "D:/OneDrive - UGent/FWO/@FWO/05_MSc thesis/2022_Kelly/git_kelly/script/function"

# function to get data from SmartLab
source(file.path(dir_otl_git,"getdata_SmartLab.R"))

# function to transform SmartLab data into AnnulusDiameter and AnnulusDiameterIncrement
source(file.path(dir_otl_git,"otolith_transformdata_SmartLab.R"))


## EXTRACT DATA FROM SMARTLAB  - #NOTE: SAMPLESET 1990 MISSING

# Set variables to get SmartLab data
lab <- 'OTL'  
project <- 'SEAWISE'
year <- 2022

# metadata info
DsOtl <- loadDataOtl(lab, project, year)

# reading info
DsAgeReadingDetail <- loadAgeReadingDetail(lab, project, year)

# link DsOtl and DsAgeReadingDetail via Outcome_ID -> otl_ilvo
DsAgeReadingDetail <- rename(DsAgeReadingDetail, Outcome_ID = OutcomeID) # rename OutcomeID to Outcome_ID in DsAgeReadingDetail

otl_smartlab <- left_join(DsOtl, DsAgeReadingDetail, by = "Outcome_ID") %>% select(-LineIndex, -DotShape)
otl_smartlab <- otl_smartlab %>% filter(is.na(Sample_Number) == F, is.na(Outcome_ID) == F, Outcome_Result != 0) # remove NA values

## PROCESS DATA

# 1. remove experiment data
list_exp_sampleset <- grep("Rosa", unique(otl_smartlab$SampleSet_Code), value = TRUE) #list of experiment SampleSet_Code

otl_smartlab <- otl_smartlab %>% filter(!(SampleSet_Code %in% list_exp_sampleset))

# 2. remove SampleID with duplicated records used for "OTOLITH READING CONSISTENCY TEST"
metadata_ilvo <- read_csv(file.path("D:/OneDrive - UGent/data/WP1/otolith/@processed","sol_select_full.csv"))
list_consistency <- metadata_ilvo %>% filter(Consistency.test == 1)

# among the duplicated records, only keep records made by CLO\\tbui (Tuan Anh)
otl_smartlab <- otl_smartlab %>% filter(!(Sample_NumberExternal %in% list_consistency$UniqueID & Outcome_LabTechnician != "CLO\\tbui"))

#only 8AB
list_8ab_sampleset <- grep("8AB", unique(otl_smartlab$SampleSet_Code), value = TRUE)
otl_smartlab <- otl_smartlab %>% filter(SampleSet_Code %in% list_8ab_sampleset)

# 3. transform SmartLab data into AnnulusDiameter and AnnulusDiameterIncrement 
otl_ilvo_raw <- SmartDots_to_OtolithIncrement(otl_smartlab)

# 3.0 check SampleSet_Code
sort(unique(otl_ilvo_raw$SampleSet_Code))

# 3.0 check negative increment
neg_increment <- otl_ilvo_raw %>% filter(AnnulusDiameterIncrement.um <= 0) %>% 
  select(Sample_IcesDivision, Sample_CatchDate, File_Scale, Sample_Number, DotIndex, AnnulusDiameter.um:OtolithWidth.um)

# 3.1. check for NA increment (due to error in aging annotation)
na_increment <- otl_ilvo_raw %>% filter(is.na(AnnulusDiameter.um) == T)

# 3.2. check for NA File_Scale 
na_scale <- otl_ilvo_raw %>% filter(is.na(File_Scale) == T)

# 3.3. check for unusual scale
# usual scale: 134 172, 217, 219, 269, 429, 437, 429 545, 593
sort(unique(otl_ilvo_raw$File_Scale)) 
#scale_check <- otl_ilvo_raw %>% 
#  filter(File_Scale %in% c(443, 542)) %>% 
#  select(Sample_IcesDivision, Sample_CatchDate, File_Scale, Sample_Number)

# 3.4. check for NA Outcome_Quality
na_quality <- otl_ilvo_raw %>% filter(is.na(Outcome_Quality) == T)

# 4. save otl_ilvo
dir_otl <- "D:/OneDrive - UGent/data/WP1/otolith"
write_rds(otl_ilvo_raw, file.path(dir_otl, "otl_ilvo smartlab.rds"))

# 5. select important fields
# note Sample_IcesDivison was set to 8A for all 8AB, for correct area need to link to SmartFish

otl_ilvo_raw <- read_rds(file.path(dir_otl, "otl_ilvo smartlab.rds"))

otl_ilvo <- otl_ilvo_raw
#otl_ilvo <- otl_ilvo_raw %>% select(Sample_Species, Sample_IcesDivision, Sample_CatchDate, Analysis_ProcessingMethod,
#                                    Sample_Number, Sample_NumberExternal, File_Scale,
#                                    Outcome_Quality,  Outcome_Result, Sample_Length, Sample_Weight, Sample_Sex, DotIndex, 
#                                    (AnnulusDiameter.um:OtolithWidth.um))

# 6. add information from metadata
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

# 7. arrange field names

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
                                Yearclass = Yearclass,
                                AgeAtCapture = SPA.Age,
                                Length.mm = SPE.Length,
                                Weight.g = SPE.Weight,
                                Sex = SPE.Sex,
                                Age = DotIndex,
                                GrowingYear = Yearclass + Age - 1, #Growing year (Age 1 - Yearclass 1970 -> growing period in year 1970)
                                AnnulusDiameter.um = round(AnnulusDiameter.um, 2),
                                AnnulusDiameterIncrement.um = round(AnnulusDiameterIncrement.um, 2),
                                OtolithWidth.um = round(OtolithWidth.um, 2),
                                DataSource = "ILVO")
                            
                            
otl_ilvo <- otl_ilvo %>% select(FishID:SamplingYear, Yearclass, AgeAtCapture:GrowingYear, AnnulusDiameter.um:OtolithWidth.um, DataSource)
# DataSource: ILVO, WUR, IFREMER - for data from smartlab 8AB -> IFREMER

# SAVE FILE

write_rds(otl_ilvo, file.path(dir_otl, "otl_ilvo.rds"))

otl_8ab <- otl_ilvo %>% filter(IcesAreaGroup == "8ab")
otl_8ab <- otl_8ab %>% filter(is.na(AnnulusDiameter.um) == F)

dir_save <- "D:/OneDrive - UGent/FWO/@FWO/05_MSc thesis/2022_Kelly/git_kelly/data"
write_rds(otl_8ab, file.path(dir_save, "otl_8ab_130223.rds"))



# 2.2. WUR ---------------------------------------------------------------------

## LOAD DATA
# otolith data
dir_otl_wur <- "D:/OneDrive - UGent/data/WP1/otolith/@raw"
otl_wur_raw <- read_excel(path = file.path(dir_otl_wur, "dutch_sole_otolith_increments_20090326.xls"), sheet = "sol")


## PROCESS DATA - with assumption about the data (not confirmed yet by 28/06/22)

#Data note
# edge value: ror - ring op rand → the last annulus diameter = otolith width (the same as ILVO 1st quarter) and the value of last annulus diameter is not noted. So backcal_age = 6 has 5 annulus diameter and the last diameter is otolith width
# edge value: blank -  all diameters of readable annulus noted, no change needed. 
# age > number of annulus diameters: the diameters of the latest years of life were difficult to measure, so only the diameters of the last readable annulus and the final otolith width. - follow the approach mentioned in Millner and Whitting 1996 "In old specimens it was not possible to separate the outer annual growth rings and measurements were made to the last reliable ring and to the outside edge"

#logic to process data 
#edge = ror 
#   - age = increment + 1 → add last diameter as otolith width
#   - age > increment + 1 (large age) → no change
#other: → nochange

# 1. up pivot to have each row is 1 increment record
otl_wur <- pivot_longer(otl_wur_raw, cols = starts_with("_"), names_to = "age_increment", values_to = "diameter")

otl_wur <- otl_wur %>% filter(is.na(diameter) == F) # remove NA diameter values 
otl_wur <- otl_wur %>% mutate(age_increment = as.numeric(str_remove(age_increment,"_"))) #remove _ in age_increment

# 3. Extract increment from diameter values 
# late months (sep - dec): need to reduce 1 age
# early months (jan - april): 
# - age = age_increment: keep age -> need to add increment
# - age > age_increment + 1: no change

otl_wur_list <- unique(otl_wur$image_ID) # create list of image_ID
otl_wur_increment = otl_wur[FALSE,]        # create an empty df

for (i in 1:length(otl_wur_list)) {
  
  print(paste("processing", otl_wur_list[i], sep = " "))
  
  otl <- otl_wur %>% filter(image_ID == otl_wur_list[i])
  
  if(is.na(unique(otl$edge)) == F) {
    print("edge = ror - ring op rand")
    
    if(unique(otl$backcal_age) == max(otl$age_increment) + 1) {
      print("age = age_increment + 1 -> add last diameter width = otolith width")
      a <- nrow(otl)
      otl[a+1,] <- otl[a,]
      otl[a+1,]$age_increment <- unique(otl$backcal_age)
      otl[a+1,]$diameter <- unique(otl$width)
    }
    
    else {
      print("age > age_increment + 1 -> no change")
      otl <- otl
    }
  } 
  
  else {
    print("edge = blank -> no change")
    }
  
  otl$increment <- c(otl$diameter[1], diff(otl$diameter))
  otl_wur_increment <- rbind(otl_wur_increment, otl)
  
   
}

# 4. arrange field names
otl_wur <-  otl_wur_increment %>%  mutate(FishID = image_ID,
                                          SmartlabNumber = NA,
                                          SpeciesFaoCode = "SOL",
                                          OtolithProcessingMethod = NA,
                                          AQCode = NA,
                                          Scale.pixelpermm = NA,
                                          IcesArea = if_else(lat_deg_min <= 53.5, "4c", "4b"), 
                                          IcesAreaGroup = "4bc",
                                          TripDate = NA,
                                          SamplingDate = as_date(paste(Year, month, day, sep = "-")),
                                          SamplingYear = Year,
                                          Yearclass = yc,
                                          AgeAtCapture = backcal_age,
                                          Length.mm = length*10,
                                          Weight.g = weight, #weight otl_wur has problem (160 fish with weight < 250, even very old/long fish)
                                          Sex = sex,
                                          Age = age_increment,
                                          GrowingYear = Yearclass + Age - 1, #Growing year (Age 1 - Yearclass 1970 -> growing period in year 1970)
                                          AnnulusDiameter.um = round(diameter, 2),
                                          AnnulusDiameterIncrement.um = round(increment, 2),
                                          OtolithWidth.um = round(width, 2),
                                          DataSource = "WUR")
                                
otl_wur <- otl_wur %>% 
  select(FishID:DataSource) %>% 
  filter(Sex == "F", Yearclass >= 1957) #only female + yearclass from 1957 for this study

# SAVE FILE

write_rds(otl_wur, file.path(dir_otl, "otl_wur.rds"))


# 2.3. IFREMER ------------------------------------------------------------


# 3. EXPLORE DATA ---------------------------------------------------------

# Check the data to see if there is any errors 

# LOAD DATA
dir_otl <- "D:/OneDrive - UGent/data/WP1/otolith"
otl_ilvo <- read_rds(file.path(dir_otl, "otl_ilvo.rds"))
otl_wur <- read_rds(file.path(dir_otl, "otl_wur.rds"))

otl_full <- bind_rows(otl_ilvo, otl_wur)

# OtolithWidt.um
# AnnulusDiameter.um
# AnnulusDiameterIncrement.um
# Length.mm
# Weight.g

# 3.1. OtolithWidth.um -------------------------------------------------------------

# OtolithWidth.um vs AgeAtCapture

ggplot(data = otl_full, aes(x = AgeAtCapture, y = OtolithWidth.um)) + 
  geom_point() + 
  #geom_smooth(se = F) +
  facet_grid(DataSource ~ IcesAreaGroup) 

# 1. ILVO: few width are above 5500 -> check Width > 5500 - ILVO 7A (as 4BC is assured)
# Diameter > 5500
otl_width_gt5000 <- otl_ilvo %>% filter(OtolithWidth.um > 5500, DataSource == "ILVO", IcesAreaGroup == "7a")
otl_width_gt5000 <- unique(otl_width_gt5000 %>% select(FishID, IcesAreaGroup, SamplingYear, Scale.pixelpermm, OtolithWidth.um, SmartlabNumber, AgeAtCapture, Length.mm, Weight.g)) 

# 3 otoliths should be checked:
# 1986 - 693
# 1990 - 829
# 1981 - 402

# 2. WUR: 2 otoliths have very small width (< 1500) 
otl_width_lt1500 <-  otl_full %>% filter(OtolithWidth.um < 1500)

# width < diamater -> maybe typo, cannot check so need to be removed

otl_full <- otl_full %>% filter(!(FishID %in% unique(otl_width_lt1500$FishID)))

# 3.2. AnnulusDiameter.um  -------------------------------------------------------------

# AnnulusDiameter.um vs Age
ggplot(data = otl_full, aes(x = Age, y = AnnulusDiameter.um)) + 
  geom_point() + 
  #geom_smooth(se = F) +
  facet_grid(DataSource ~ IcesAreaGroup) 

# the growth curve of 4bc in ILVO and WUR data is quite similar

# 1. 1 point with very small dimater
otl_diameter_lt200 <- otl_full %>% filter(AnnulusDiameter.um < 200)
otl_diameter_lt200 <- otl_full %>% filter(FishID == otl_diameter_lt200$FishID)

# remove
otl_full <- otl_full %>% filter(!(FishID %in% unique(otl_diameter_lt200$FishID)))

# AnnulusDiameter.um vs Age and Length.mm 
ggplot(otl_full, aes(x = Length.mm, y = AnnulusDiameter.um)) +
  geom_point(alpha = 0.1) + 
  facet_grid(~ Age)

# 1. 1 otolith with AnnulusDiameter.um < 1100 at age 2, unusual  
otl_diameter_lt1100 <- otl_full %>% filter(Age == 2, AnnulusDiameter.um < 1100)
otl_diameter_lt1100 <- otl_full %>% filter(FishID == otl_diameter_lt1100$FishID)

# remove
otl_full <- otl_full %>% filter(!(FishID %in% unique(otl_diameter_lt1100$FishID)))

# 3.3. AnnulusDiameterIncrement.um  -------------------------------------------------------------

# increment distribution -> normal - reduce eventually by age
ggplot() + geom_histogram(data = otl_full, aes(x = AnnulusDiameterIncrement.um), binwidth = 30) +
  facet_wrap(~ Age) # by age

# AnnulusDiameterIncrement.um vs Age
ggplot(data = otl_full, aes(x = Age, y = AnnulusDiameterIncrement.um)) + 
  geom_point(alpha = 0.2) + 
  #geom_smooth(se = F) +
  facet_grid(DataSource ~ IcesAreaGroup) 

# similar in 4bc in ILVO and WUR



# 3.4. Length.g -----------------------------------------------------------

#Length.mm vs OtolithWidth.um
ggplot(data = otl_full, aes(x = OtolithWidth.um, y = Length.mm, color = DataSource)) + 
  geom_point(alpha = 0.2) + 
  #geom_smooth(se = F) +
  facet_grid(~ IcesAreaGroup) 

# The relationship between length and otolith width is similar in 4bc in ILVO and WUR data


# 3.5. Weight.g ------------------------------------------------------------

# Weight.g vs Length.mm

ggplot(data = otl_full, aes(x = Length.mm, y = Weight.g, color = DataSource)) + 
  geom_point(alpha = 0.2) + 
  #geom_smooth(se = F) +
  facet_grid(~ IcesAreaGroup) 

# WUR: 4bc - small weight (< 250) at length > 300 mm
otl_weight_lt250 <- otl_full %>% filter(DataSource == "WUR", Weight.g < 250, Length.mm > 300)
otl_weight_lt250$Weight.g_new <- otl_weight_lt250$Weight.g*10

ggplot() + 
  geom_point(data = otl_full %>% filter(!(FishID %in% otl_weight_lt250$FishID), DataSource == "WUR"), aes(x = Length.mm, y = Weight.g), color = "grey") +
  geom_point(data = otl_weight_lt250, aes(x = Length.mm, y = Weight.g), color = "black") +
  geom_point(data = otl_weight_lt250, aes(x = Length.mm, y = Weight.g_new), color = "red") +
  #geom_smooth(se = F) +
  facet_grid(~ IcesAreaGroup) 

#Very likely that there was a typo in weight and the weight should be *10 for those with very small weight
otl_full <- otl_full %>% mutate(Weight.g = if_else(FishID %in% otl_weight_lt250$FishID, Weight.g*10, Weight.g))

# Weight.g vs OtolithWidth.um

#normal
ggplot(data = otl_full, aes(x = OtolithWidth.um, y = Weight.g, color = DataSource)) + 
  geom_point(alpha = 0.2) + 
  #geom_smooth(se = F) +
  facet_grid(~ IcesAreaGroup) 

#log-log
ggplot(data = otl_full, aes(x = log10(OtolithWidth.um), y = log(Weight.g), color = DataSource)) + 
  geom_point(alpha = 0.2) + 
  #geom_smooth(se = F) +
  facet_grid(~ IcesAreaGroup) 

cor.test(log10(otl_full$Weight.g), log10(otl_full$OtolithWidth.um))
#Correlation between log(weight) and log(otolith width) (all data): 0.69


# 4. SAVE CLEANED DATA ----------------------------------------------------
#The data is explored and cleaned and be ready for analysis
write_rds(otl_full, file.path(dir_otl, "otl_full.rds"))
