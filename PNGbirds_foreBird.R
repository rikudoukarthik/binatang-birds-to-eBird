############## Modifying PNG birds data for eBird upload #############
######################################################################
### Bird surveys conducted by Katerina Sam and New Guinea Binatang Research Centre ###
### from 2010 to 2015. Modifying raw data to make it suitable for upload to eBird. ###
############### Code by Karthik Thrikkadeeri (June 2021) ##############

# library ####
library(tidyverse)
library(lubridate)

# importing ####

# species codes with corresponding scientific names from raw data file (Sheet "PNGspec")
# splitting to Genus and Species
spec_info <- read.delim("clipboard", as.is = F) %>% 
  separate(ScientificName, into = c("Genus","Species"), sep = " ") %>% 
  mutate(Genus = as.factor(Genus), Species = as.factor(Species))

# GPS coordinates of survey points from raw data file (Sheet "GPS")
point_info <- read.delim("clipboard", as.is = F)

# Point Count data
birds_PC <- read.delim("clipboard", as.is = F)

# MacKinnon data
birds_MK <- read.delim("clipboard", as.is = F)

# mistnetting data
birds_MN <- read.delim("clipboard", as.is = F)


# modifying point counts ####

# summarising observations of same species in same survey by introducing Number column
PointCounts <- inner_join(birds_PC, spec_info, by = "Spec_Code") %>% 
  select(-c(1:4, 6:7, 11:14, 17:18)) %>% 
  group_by(Date, Start.time, Stop.time, Plot_Point, Observer, Spec_Code, Genus, Species) %>% 
  summarise(Number = n()) %>% mutate(LocationName = Plot_Point) %>% 
  inner_join(point_info, by = "Plot_Point") %>% 
  mutate(Protocol = "Stationary", NumberofObservers = 1, 
         Duration = 15, 
         Allobservationsreported = "Y",
         Submissioncomments = paste0("Observation by ",
                                     ifelse(Observer=="Bonny", "Bonny Koane",
                                            ifelse(Observer=="Katka", "Katerina Sam",
                                                   "Samuel Jeppy")), "."),
         SpeciesComments = ifelse(Spec_Code=="CharStel", "ssp. stellae",
                           ifelse(Spec_Code=="PhylPoli", "ssp. poliocephalus", ""))) %>% 
  ungroup() %>% select(-c(3:6))

write.csv(PointCounts, file = "PNGbirds_foreBird_PointCounts.csv", quote = F,
          row.names = F, col.names = F)



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

