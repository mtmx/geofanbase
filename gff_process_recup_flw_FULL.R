
library(rtweet)
library(sf)
library(lubridate)
library(tidyverse)

# liste des comptes twitters
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
  "NM", "nimesolympique",
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
  "VAFC", "VAFC"
)
  
liste_clubs_NAT <- tribble(
  ~initiales_club, ~alias_club,  
  "SV", "stadelavallois",
  "SCB", "SCBastia",
  "TOFC", "ToursFC")
  


liste_clubs_berri <- tribble(
  ~initiales_club, ~alias_club,  
  "LBC", "LaBerrichonne")
  

recup_flw <- function(initiales_club, alias_club)
{
  
# nombre de followers
nb_flw_club <- lookup_users(paste0(alias_club))
# récupération de la liste
df <- get_followers(
  paste0(alias_club), n = nb_flw_club$followers_count, retryonratelimit = TRUE
)  %>% mutate(alias_club = alias_club)
}


liste_flw_clubs_L1L2NAT <-
map2(liste_clubs$initiales_club, liste_clubs$alias_club, ~ recup_flw(.x,.y)) %>%
  bind_rows()

liste_flw_clubs_LBC <-
  map2(liste_clubs_berri$initiales_club, liste_clubs_berri$alias_club, ~ recup_flw(.x,.y)) %>%
  bind_rows()


# recup
liste_flw_clubs_L1L2NAT <- fread("./liste_flw_clubs_L1L2NAT.csv") %>% mutate(user_id = as.character(user_id))

# rajout sup psg

liste_flw_clubs_L1L2NATPSG <- liste_flw_clubs_L1L2NAT %>%
  filter(!alias_club %in% 'PSG_inside') %>%
  bind_rows(liste_flw_clubs_PSG)

liste_flw_clubs_FULL <- liste_flw_clubs_L1L2NATPSG %>%
  filter(!alias_club %in% 'LaBerriOfficiel') %>%
  rbind.data.frame(liste_flw_clubs_LBC)

# sortie
fwrite(liste_flw_clubs_FULL, "./data_102018/liste_flw_clubs_FULL.csv") 
# import
liste_flw_clubs_FULL <- fread("./data_102018/liste_flw_clubs_FULL.csv") 


############
# recup info sur followers

#parametres
#nb_ids <- liste_flw_clubs_OUEST_dist %>% nrow()
nb_ids <- liste_flw_dist.lot %>% summarise(nb =n()) %>% as.numeric()
nb_id_lots <- 85000
nb_lots <- ceiling(nb_ids / nb_id_lots) %>% as.numeric()


# fonction pour boucler sur fichiers de plus de 80 000 lignes
lookup_users_tot <- function(n){

  # parametres fluides
  
  # 1er df
  if (n == 1) {
    df <- liste_flw_dist.lot %>% slice(1:nb_id_lots)
    # df intermédiaires  
  } else if (n > 1 & n < nb_lots) {
    df <- liste_flw_dist.lot %>% slice( (nb_id_lots*(n-1))+1 : nb_id_lots)
    # dernier df  
  } else {
    df <- liste_flw_dist.lot %>% slice( (nb_id_lots*(n-1))+1 : nb_ids )
  }
  
  # pause de 15 minutes  
  Sys.sleep(15*60 + 15)
  
  # recup infos  
  df_user <- lookup_users(df %>% pull() )
  
}

# jouer sur 
liste_flw_PSG_data.2 <- map_df(seq(from = 1, to = nb_lots, by =1), lookup_users_tot) 
liste_flw_PSG_data <- liste_flw_PSG_data.1 %>% bind_rows(liste_flw_PSG_data.2)


liste_flw_LBC_data <- lookup_users(liste_flw_clubs_LBC %>% select(user_id) %>% pull() )


library(data.table)

# aggrégation des csv en un df
library(fs)
#https://www.gerkelab.com/blog/2018/09/import-directory-csv-purrr-readr/
data_dir <- "./data"
csv_files <- fs::dir_ls(data_dir, regexp = "\\.csv$")

liste_flw_L1L2NAT_data <- csv_files %>% 
  map_dfr(read_csv) %>%
  mutate(user_id = as.character(user_id)) %>%
  mutate(status_id = as.character(status_id)) %>%
  mutate(geo_coords = as.character(geo_coords))


liste_flw_L1L2NATPSG_data <- liste_flw_L1L2NAT_data %>% bind_rows(liste_flw_PSG_data.ok)
  
#fwrite(liste_flw_L1L2NATPSG_data, "./data/liste_flw_L1L2NATPSG_data.csv")
liste_flw_L1L2NATPSG_data <- fread( "./data/liste_flw_L1L2NATPSG_data.csv")

# reconstitution du fichier avec ajout delta LBC
liste_flw_LBC_data.b <- liste_flw_LBC_data %>% 
  left_join(liste_flw_L1L2NATPSG_data %>% select(user_id) %>% mutate(in_ori = "ok"), by ="user_id") %>%
  filter(is.na(in_ori)) %>%
  select( "user_id" , "status_id" ,"screen_name", "lang" ,"geo_coords", "name"  , "account_created_at", "account_lang" ,      "description" , "location"  , "place_full_name" ,   "country" ,"country_code" )

liste_flw_FULL_data <- liste_flw_L1L2NATPSG_data %>% as.data.frame() %>%
  rbind.data.frame(liste_flw_LBC_data.b %>% as.data.frame())


# sortie
fwrite(liste_flw_FULL_data, "./data_102018/liste_flw_FULL_data.csv") 

### traitement geo
ref_locations.L1L2NAT <- liste_flw_L1L2NAT_data %>%
  distinct(location) %>%
  mutate(id_loc.1 = row_number())


ref_locations.L1L2NATPSG <- liste_flw_L1L2NATPSG_data %>%
  distinct(location) %>%
  mutate(id_loc.1 = row_number())

ref_locations.FULL <- liste_flw_FULL_data %>%
  distinct(location) %>%
  mutate(id_loc.1 = row_number())

ref_locations.FULL <- liste_flw_LBC_data.b %>%
  distinct(location) %>%
  mutate(id_loc.1 = row_number())
  

# deja requeté : ref_locations_clubs_OUEST
# import
#ref_locations_clubs_OUEST <- fread("./ref_locations_clubs_OUEST.csv") %>% as.data.frame()

liste_flw_clubs_FULLL_loc.1 <- liste_flw_L1L2NATPSG_data %>% 
  group_by(location, lang) %>%
  summarise(nb_cpt = n()) %>%
  ungroup() %>%
  left_join(ref_locations.L1L2NATPSG, by = "location") %>%
  mutate(#location.s = iconv(location, to='ASCII//TRANSLIT'),
         location.s = str_replace_all(location,'"',''),
         location.s = str_replace_all(location.s,'#|@|_|^',' '),
         location.s = str_replace_all(location.s,':',','),
         location.s = trimws(location.s),
         location.s = tolower(location.s)) 

liste_flw_clubs_FULLL_loc.2 <- liste_flw_clubs_FULLL_loc.1 %>%
  group_by(location.s) %>%
  summarise(nb = n()) %>%
  mutate(id_loc.2 = row_number()) %>%
  filter(nb > 2) 

# déjà géocodé via clubs OUEST ?
liste_flw_clubs_FULLL_loc.3 <-
  liste_flw_clubs_FULLL_loc.2 %>% 
  left_join(ref_locations_clubs_OUEST %>% select(location.s,components._type), by = c("location.s" = "location.s" ) ) %>%
  filter(is.na(components._type)) %>%
  filter(nchar(location.s) > 1) %>%
  select(-components._type)




##############
### geocodage
liste_flw_clubs_FULLL_loc.3.lot1  <- liste_flw_clubs_FULLL_loc.3 %>% slice(1:2400)
liste_flw_clubs_FULLL_loc.3.lot2  <- liste_flw_clubs_FULLL_loc.3 %>% slice((2400+1):(2400*2))
liste_flw_clubs_FULLL_loc.3.lot3  <- liste_flw_clubs_FULLL_loc.3 %>% slice((2400*2+1):(2400*3))
liste_flw_clubs_FULLL_loc.3.lot4  <- liste_flw_clubs_FULLL_loc.3 %>% slice((2400*3+1):(2400*4))
liste_flw_clubs_FULLL_loc.3.lot5  <- liste_flw_clubs_FULLL_loc.3 %>% slice((2400*4+1):(2400*5))
liste_flw_clubs_FULLL_loc.3.lot6  <- liste_flw_clubs_FULLL_loc.3 %>% slice((2400*5+1):(2400*6))
liste_flw_clubs_FULLL_loc.3.lot7  <- liste_flw_clubs_FULLL_loc.3 %>% slice((2400*6+1):(2400*7))
liste_flw_clubs_FULLL_loc.3.lot8  <- liste_flw_clubs_FULLL_loc.3 %>% slice((2400*7+1):(2400*8))
liste_flw_clubs_FULLL_loc.3.lot9  <- liste_flw_clubs_FULLL_loc.3 %>% slice((2400*8+1):(2400*9))
liste_flw_clubs_FULLL_loc.3.lot10 <- liste_flw_clubs_FULLL_loc.3 %>% slice((2400*9+1):(2400*10))
liste_flw_clubs_FULLL_loc.3.lot11 <- liste_flw_clubs_FULLL_loc.3 %>% slice((2400*10+1):(2400*11))
liste_flw_clubs_FULLL_loc.3.lot12 <- liste_flw_clubs_FULLL_loc.3 %>% slice((2400*11+1):(2400*12))
liste_flw_clubs_FULLL_loc.3.lot13 <- liste_flw_clubs_FULLL_loc.3 %>% slice((2400*12+1):(2400*13))
liste_flw_clubs_FULLL_loc.3.lot14 <- liste_flw_clubs_FULLL_loc.3 %>% slice((2400*13+1):(2400*14))
liste_flw_clubs_FULLL_loc.3.lot15 <- liste_flw_clubs_FULLL_loc.3 %>% slice((2400*14+1):(2400*15))

# geocodage
#install_github("ropensci/opencage")
library(opencage)
keymatopencage <- "2b8482d5da5846b1891574a237efd4f7"
keymatopencage_matpoub <- "245d9d8f28644738975bf5ad4af0b6bb"
keymatopencage_matgar <- "02f916f25f124cdbb1b39634d4ac1415"


opc <- function(adr){
  ref_location.geo <- opencage_forward(placename = adr, key = keymatopencage_matgar,limit =1)
  output <- ref_location.geo$results
}

# execution par lot
liste_flw_clubs_FULLL_loc.3.lot1k.geo <- map_df(liste_flw_clubs_FULLL_loc.3.lot1k$location.s, opc)
     


liste_flw_clubs_FULLL_loc.3.lot1.geoo <- liste_flw_clubs_FULLL_loc.3.lot1 %>%
  left_join( liste_flw_clubs_FULLL_loc.3.lot1.geo %>%
               select(query, components.country_code, geometry.lat, geometry.lng, components._type, bounds.northeast.lat,bounds.northeast.lng,bounds.southwest.lat,bounds.southwest.lng) %>%
               mutate(#nb = as.numeric(nb),
                      #id_loc.2 = as.numeric(id_loc.2),
                      bounds.northeast.lat = as.numeric(bounds.northeast.lat),
                      bounds.northeast.lng = as.numeric(bounds.northeast.lng),
                      bounds.southwest.lat = as.numeric(bounds.southwest.lat),
                      bounds.southwest.lng = as.numeric(bounds.southwest.lng)), 
             by = c('location.s' ='query'))


# concaténation des ref sans geometry
liste_flw_clubs_FULLLL_loc.3.geoo <- ref_locations_clubs_OUEST %>%
  bind_rows(liste_flw_clubs_FULLL_loc.3.lot1.geoo)%>%
  bind_rows(liste_flw_clubs_FULLL_loc.3.lot2.geoo)%>%
  bind_rows(liste_flw_clubs_FULLL_loc.3.lot3.geoo)%>%
  bind_rows(liste_flw_clubs_FULLL_loc.3.lot4.geoo)%>%
  bind_rows(liste_flw_clubs_FULLL_loc.3.lot5.geoo)%>%
  bind_rows(liste_flw_clubs_FULLL_loc.3.lot6.geoo)%>%
  bind_rows(liste_flw_clubs_FULLL_loc.3.lot7.geoo)%>%
  bind_rows(liste_flw_clubs_FULLL_loc.3.lot8.geoo)%>%
  bind_rows(liste_flw_clubs_FULLL_loc.3.lot9.geoo)%>%
  bind_rows(liste_flw_clubs_FULLL_loc.3.lot10.geoo)%>%
  bind_rows(liste_flw_clubs_FULLL_loc.3.lot11.geoo)%>%
  bind_rows(liste_flw_clubs_FULLL_loc.3.lot12.geoo)%>%
  bind_rows(liste_flw_clubs_FULLL_loc.3.lot13.geoo)%>%
  bind_rows(liste_flw_clubs_FULLL_loc.3.lot14.geoo)%>%
  bind_rows(liste_flw_clubs_FULLL_loc.3.lot15.geoo)


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
liste_flw_L1L2NATPSG_dist.geo <-
  liste_flw_L1L2NATPSG_data %>%
  select(user_id, location, lang,account_lang) %>%
  left_join(liste_flw_clubs_FULLL_loc.1 %>% distinct(location,location.s), by = "location") %>%
  left_join(liste_flw_clubs_FULLL_loc.2 %>% distinct(location.s,id_loc.2), by = "location.s") %>%
  #left_join(liste_flw_clubs_OUEST_loc.2.geoo %>% select(id_loc.2,components._type, starts_with("bounds")), by = "id_loc.2") %>%
  left_join(liste_flw_clubs_FULLLL_loc.3.COMM %>% mutate(zone = "FRAMET") %>% select(id_loc.2,location.s, CODGEO,components._type, zone), by = "location.s") %>%
  left_join(liste_flw_clubs_FULLLL_loc.3.PAYS %>% select(id_loc.2,location.s, gu_a3, geounit), by = "location.s") #%>%
  #filter(components._type %in% c('city','building','neighbourhood','postcode'))


fwrite(liste_flw_clubs_FULLL_loc.1, "data/liste_flw_clubs_FULLL_loc.1.csv")
fwrite(liste_flw_clubs_FULLL_loc.2, "data/liste_flw_clubs_FULLL_loc.2.csv")
  