# referentiel stade geo
library(datapasta)
library(readxl)
ref_club_stade <- read_xlsx("./data_102018/ref_club_stade.xls") %>%
  mutate(initiales_nom_club_actuel =  case_when(initiales_nom_club_actuel %in% "SB29" ~ "SB",
                                                initiales_nom_club_actuel %in% "RCS" ~ "RCSA",
                                                TRUE ~ as.character(initiales_nom_club_actuel))) 


ref.club <- liste_clubs_couleur %>% left_join(ref_club_stade %>%
                                                select(club_domicile_actuel, stade,  ville, latitude, longitude, initiales_nom_club_actuel, logo_png),
                                              by = c("initiales_club" = "initiales_nom_club_actuel")) %>%
  filter(!is.na(club_domicile_actuel))

#  ASB Beziers
# CF Clermont
# GF 38 Grenoble
# CN Chamois Niortais
#USO US_Orleans

#PFC ParisFC
#RS RedStarFC
# SV stadelavallois
# TOFC ToursFC

rattrap.ref.club <- tribble(
  ~initiales_club, ~alias_club,~col_club,~club_domicile_actuel, ~stade, ~ville,~latitude,~longitude,  ~logo_png,
  "ASB","ASBeziersFoot" , "#FF0800" , "AS Béziers", "Stade de la Méditerranée","Béziers", 43.334664, 3.266949, "./logos_clubs/ASB.png",
  "CF","ClermontFoot", "#D70A53" , "Clermont Foot 63", "Stade Gabriel-Montpied", "Clermont-Ferrand", 45.815669, 3.121687,"./logos_clubs/CF.png",
  "GF", "GF38_Officiel", "#6495ED" ,"Grenoble Foot 38", "Stade des Alpes","Grenoble",45.187390, 5.740314,"./logos_clubs/GF.png",
  "CN","ChamoisNiortais" ,"#2E5894", "Chamois Niortais", "Stade René-Gaillard", "Niort", 46.317330, -0.489853,"./logos_clubs/CN.png",
  "USO", "US_Orleans", "#FFD300" , "US Orléans", "Stade de la Source", "Orléans", 47.840414, 1.941780,"./logos_clubs/USO.png",
  "PFC", "ParisFC" , "#062A78" , "Paris FC", "Stade Charléty", "Paris", 48.818717, 2.347006,"./logos_clubs/PFC.png",
  "RS", "RedStarFC", "#0E7C61" , "Red Star", "Stade Bauer","Saint-Ouen",48.906037, 2.341558,"./logos_clubs/RS.png",
  "SL", "stadelavallois" , "#FF6103" , "Stade Lavallois", "Stade Francis-Le-Basser", "Laval", 48.082865, -0.755450, "./logos_clubs/SL.png",
  "TOFC", "ToursFC" , "#6CADDE", "Tours FC", "Stade de la Vallée du Cher", "Tours", 47.375877, 0.728261, "./logos_clubs/TOFC.png")
  
ref.club <- ref.club %>% rbind.data.frame(rattrap.ref.club)

# modifications Amiens
ref.club <- ref.club %>%
  filter(!initiales_club %in% 'ASC') %>%
  rbind.data.frame(tribble(
    ~initiales_club, ~alias_club,~col_club,~club_domicile_actuel, ~stade, ~ville,~latitude,~longitude,  ~logo_png,
    "ASC", "AmiensSC" , "#91A3B0", "Amiens SC", "Stade Crédit Agricole La Licorne", "Amiens", 49.893275, 2.263468, "./logos_clubs/ASC.png"))

# capacite stade
capacite_stade <- read_xlsx("./data_102018/capacite_stade.xlsx")


ref.club.capa <- ref.club %>% left_join(capacite_stade, by = c('club_domicile_actuel' ='club')) %>%
  mutate(capacite_stade =  case_when( initiales_club %in% 'TOFC' ~ 10128,
                                      initiales_club %in% 'SL' ~ 18607,
                                      initiales_club %in% 'SCB' ~ 16500, TRUE ~ as.numeric(capacite_stade)))

ref.club.geo <- ref.club.capa %>%
  #filter(!is.na(latitude)) %>%
  st_as_sf(., coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(2154) %>%
  as.data.frame()

ref.club.epsg2154 <- ref.club.geo %>%
  mutate(x.ctr = map_dbl(geometry, ~st_centroid(.x)[[1]]),
          y.ctr = map_dbl(geometry, ~st_centroid(.x)[[2]])) %>%
  as.data.frame() %>%
  select(-geometry)

# # problème pour parametre fonction apres, on recupere x y a la mano
# ref.club.geo.x <- ref.club.geo %>% mutate(x = map_dbl(geometry, ~st_centroid(.x)[[1]]) ) %>% as.data.frame() %>% select(x)
# ref.club.geo.y <- ref.club.geo %>% mutate(y = map_dbl(geometry, ~st_centroid(.x)[[2]]) ) %>% as.data.frame() %>% select(y)
# ref.club.geo.xy <- ref.club.geo %>% bind_cols(ref.club.geo.x) %>% bind_cols(ref.club.geo.y)
# 
# ref.club.epsg2154 <- ref.club.capa %>% left_join(ref.club.geo.xy %>% select(initiales_club,x,y), by = "initiales_club") %>% as.data.frame()
# rm(ref.club.geo.x, ref.club.geo.y, ref.club.geo.xy)
# 
# library(magrittr)
# ref.club.epsg2154 <- ref.club.capa %>% as.data.frame() %>% cbind( do.call(rbind, st_geometry( ref.club.geo )) %>% set_colnames(c("x","y")) ) 
