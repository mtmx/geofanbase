options(scipen = 999)

library(rtweet)
library(sf)
library(lubridate)
library(tidyverse)
library(markdowntemplates)
library(data.table)

liste_clubs <- tribble(
  ~initiales_club, ~alias_club,  
  "SRFC", "staderennais",
  "FCN", "FCNantes",
  "FCL", "FCLorient",
  "EAG", "EAGuingamp",
  "SB", "SB29",
  "ASC", "AmiensSC",
  "SCO", "AngersSCO",
  "FCGB", "girondins",
  "SMC", "SMCaen",
  "DFCO", "DFCO_Officiel",
  "LOSC", "losclive",
  "OL", "OL",
  "OM", "OM_Officiel",
  "ASM", "AS_Monaco",
  "MHSC", "MontpellierHSC",
  "OGCN", "ogcnice",
  "NO", "nimesolympique",
  "PSG", "PSG_inside",
  "SR", "StadeDeReims",
  "ASSE", "ASSEofficiel",
  "RCSA", "RCSA",
  "TFC", "ToulouseFC",
  "ACA", "ACAjaccio",
  "GFCA", "gfc_ajaccio",
  "AJA", "AJA",
  "ASB", "ASBeziersFoot",
  "LBC", "LaBerrichonne",
  "CF", "ClermontFoot",
  "GF", "GF38_Officiel",
  "HAC", "HAC_Foot",
  "RCL", "RCLens",
  "FCM", "FCMetz",
  "ASNL", "asnlofficiel",
  "CN", "ChamoisNiortais",
  "USO", "US_Orleans",
  "PFC", "ParisFC",
  "RS", "RedStarFC",
  "FCSM", "FCSM_officiel",
  "ESTAC", "estac_officiel",
  "VAFC", "VAFC",  
  "SL", "stadelavallois",
  "SCB", "SCBastia",
  "TOFC", "ToursFC")
  


# import
liste_flw_clubs_FULL <- fread("./data_102018/liste_flw_clubs_FULL.csv") %>%
  mutate(user_id = as.character(user_id)) %>%
  left_join(liste_clubs, by = "alias_club")
liste_flw_clubs_FULLLL_loc.3.geoo <- fread("./data_102018/liste_flw_clubs_FULLLL_loc.3.geoo.csv") 
liste_flw_FULL_data <- fread("./data_102018/liste_flw_FULL_data.csv") 
liste_flw_clubs_FULLL_loc.1 <- fread("./data_102018/liste_flw_clubs_FULLL_loc.1.csv")
liste_flw_clubs_FULLL_loc.2 <- fread("./data_102018/liste_flw_clubs_FULLL_loc.2.csv")

# conversion en geo.sf
liste_flw_clubs_FULLLL_loc.3.geoo <-
  liste_flw_clubs_FULLLL_loc.3.geoo %>%
  filter(!is.na(geometry.lng)) %>%
  st_as_sf(coords = c("geometry.lng", "geometry.lat"),
           crs = 4326, remove = FALSE)

# récupération du code commune 
#comms_FR <- st_read("https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/communes.geojson")

liste_flw_clubs_FULLLL_loc.3.COMM <-  st_intersection(liste_flw_clubs_FULLLL_loc.3.geoo , comm.frdom.wgs94)

liste_flw_clubs_FULLLL_loc.3.PAYS <-  st_intersection(liste_flw_clubs_FULLLL_loc.3.geoo , REF_NED_pays.niv1b)


##################################################

# coordonnées et code commune pour chaque flw
liste_flw_FULL_dist.geo <-
  liste_flw_FULL_data %>%
  select(user_id, location, lang,account_lang) %>%
  left_join(liste_flw_clubs_FULLL_loc.1 %>% distinct(location,location.s), by = "location") %>%
  left_join(liste_flw_clubs_FULLL_loc.2 %>% distinct(location.s,id_loc.2), by = "location.s") %>%
  left_join(liste_flw_clubs_FULLLL_loc.3.COMM %>% mutate(zone = "FRAMET") %>% select(id_loc.2,location.s, CODGEO,components._type, zone), by = "location.s") %>%
  left_join(liste_flw_clubs_FULLLL_loc.3.PAYS %>% select(id_loc.2,location.s, gu_a3, geounit), by = "location.s") #%>%
  #filter(components._type %in% c('city','building','neighbourhood','postcode'))



  