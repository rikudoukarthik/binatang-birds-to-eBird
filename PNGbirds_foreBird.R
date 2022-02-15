############## Modifying PNG birds data for eBird upload #############
### Bird surveys conducted by Katerina Sam and New Guinea Binatang Research Centre ###
### from 2010 to 2015. Modifying raw data to make it suitable for upload to eBird. ###
############### Code by Karthik Thrikkadeeri (June 2021) ##############
######################################################################

library(tidyverse)
library(lubridate)



# importing ####

# species codes with corresponding scientific names from sheet "PNGspec"
# splitting to Genus and Species
spec_info <- readxl::read_xlsx("PNGbirds_RAWdata_20210625.xlsx", sheet = "PNGspec") %>% 
  separate(ScientificName, into = c("Genus","Species"), sep = " ")

# GPS coordinates of survey points from sheet "GPS"
point_info <- readxl::read_xlsx("PNGbirds_RAWdata_20210625.xlsx", 
                                sheet = "GPS", range = "A1:D305")

# Point Count data
birds_PC <- readxl::read_xlsx("PNGbirds_RAWdata_20210625.xlsx", sheet = "PointCounts") %>% 
  rename(Conditions_ID = Conditions.ID,
         Start_time = `Start time`,
         Stop_time = `Stop time`) %>% 
  mutate(Start_time = hms::as_hms(Start_time),
         Stop_time = hms::as_hms(Stop_time))

# MacKinnon data
birds_MK <- readxl::read_xlsx("PNGbirds_RAWdata_20210625.xlsx", sheet = "MacKinnon") %>% 
  rename(Survey_no = `Survey No.`,
         Start_time = `Start time`,
         Stop_time = `Stop time`) %>% 
  mutate(Start_time = hms::as_hms(Start_time),
         Stop_time = hms::as_hms(Stop_time))

# mistnetting data
birds_MN <- readxl::read_xlsx("PNGbirds_RAWdata_20210625.xlsx", sheet = "Mistnetting") %>% 
  rename(Patrol_Day = `Patrol Day`,
         Start_time = `Start time`,
         Stop_time = `Stop time`) %>% 
  mutate(Start_time = hms::as_hms(Start_time),
         Stop_time = hms::as_hms(Stop_time),
         Time = hms::as_hms(Time))


# modifying point counts ####

# summarising observations of same species in same survey by introducing Number column
pointcounts <- birds_PC %>% 
  select(Plot_Point, Date, Start_time, Stop_time, Observer, Spec_Code) %>% 
  group_by(Date, Start_time, Stop_time, Plot_Point, Observer, Spec_Code) %>% 
  summarise(Number = n()) %>% 
  ungroup() %>% 
  left_join(spec_info, by = "Spec_Code") %>% 
  left_join(point_info, by = "Plot_Point") %>% 
  mutate(Protocol = "Stationary", 
         NumberofObservers = 1, 
         Duration = 15, 
         Allobservationsreported = "Y",
         Submissioncomments = paste0("Observation by ",
                                     case_when(Observer == "Bonny" ~ "Bonny Koane",
                                               Observer == "Katka" ~ "Katerina Sam",
                                               TRUE ~ "Samuel Jeppy"),
                                     "."),
         SpeciesComments = case_when(Spec_Code == "CharStel" ~ "ssp. stellae",
                                     Spec_Code == "PhylPoli" ~ "ssp. poliocephalus",
                                     TRUE ~ "")) %>% 
  mutate(Commonname = "", State = "", Countrycode = "", Distance = "", Area = "") %>% 
  # ordering columns according to eBird template
  select(Commonname, Genus, Species, Number, SpeciesComments, Plot_Point, Latitude,
         Longitude, Date, Start_time, State, Countrycode, Protocol, NumberofObservers,
         Duration, Allobservationsreported, Distance, Area, Submissioncomments) %>% 
  arrange(Date, Start_time) %>% 
  mutate(Date = as.character(Date))

write_csv(pointcounts, file = "PNGbirds_foreBird_PointCounts.csv", 
          col_names = F)



# modifying MacKinnon ####

MacKinnon <- inner_join(birds_MK, spec_info, by = "Spec_Code") %>% 
  select(-c(1:3, 5:6)) %>% 
  group_by(Date, Start.time, Stop.time, Plot_Point, Observer, Spec_Code, Genus, Species) %>% 
  summarise(Number = n()) %>% mutate(LocationName = Plot_Point) %>% 
  inner_join(point_info, by = "Plot_Point") %>% 
  mutate(Protocol = "Traveling", 
         NumberofObservers = ifelse(Observer=="Bonny"|Observer=="Katka", 1, 2), 
         Allobservationsreported = "Y",
         Submissioncomments = paste0("MacKinnon walk. Observation by ",
                                     ifelse(Observer=="Bonny", "Bonny Koane",
                                            ifelse(Observer=="Katka", "Katerina Sam",
                                                   "Katerina Sam and Bonny Koane")), "."),
         SpeciesComments = ifelse(Spec_Code=="CharStel", "ssp. stellae",
                                  ifelse(Spec_Code=="PhylPoli", "ssp. poliocephalus", "")))

write.csv(MacKinnon, file = "PNGbirds_foreBird_MacKinnon.csv", quote = F,
          row.names = F, col.names = F)



# modifying mistnetting ####

MistNetting <- inner_join(birds_MN, spec_info, by = "Spec_Code") %>% 
  select(-c(1:5, 7, 14:18)) %>% 
  group_by(Date, Start.time, Stop.time, Plot_Point, Observer, Spec_Code, Genus, Species) %>% 
  summarise(Time = Time, Number = n()) %>% mutate(LocationName = Plot_Point) %>% 
  inner_join(point_info, by = "Plot_Point") %>% 
  mutate(Protocol = "P33", 
         NumberofObservers = ifelse(Observer=="Bonny"|Observer=="Katka"|Observer=="Samuel", 1, 2), 
         Allobservationsreported = "N", # only from mist so Incomplete
         Submissioncomments = paste0("Observation by ",
                                     ifelse(Observer=="Bonny", "Bonny Koane",
                                            ifelse(Observer=="Katka", "Katerina Sam",
                                                   "Samuel Jeppy")), "."),
         SpeciesComments = paste0(ifelse(Number=="1", as.character(Time), ""),
                                  ifelse(Spec_Code=="CharStel", "ssp. stellae",
                                  ifelse(Spec_Code=="PhylPoli", "ssp. poliocephalus", "")))) 

write.csv(PointCounts, file = "PNGbirds_foreBird_PointCounts.csv", quote = F,
          row.names = F, col.names = F)

