#####################
## shapefile communes

if (dir.exists("/tmp/ADMINEXPRESS_1-0__SHP_LAMB93_FXX_2017-01-18")) {
  # importer shape des communes France métro
  comm <- st_read("/tmp/ADMINEXPRESS_1-0__SHP_LAMB93_FXX_2017-01-18/ADMINEXPRESS/1_DONNEES_LIVRAISON_2017-01-18/ADE_1-0_SHP_LAMB93_FR/COMMUNE.shp" , stringsAsFactors = F) %>% st_transform(crs = 2154) %>%
    mutate(CODGEO= ifelse(substr(INSEE_COM,1,2) == '75' ,'75056',
                          ifelse(substr(INSEE_COM,1,3) == '132' ,'13055',
                                 ifelse(substr(INSEE_COM,1,4) == '6938' ,'69123',INSEE_COM)))) %>%
    group_by(CODGEO) %>%
    summarize(NOM_COMM = first(NOM_COM),
              STATUT = first(STATUT),
              POPULATION = sum(POPULATION)) %>% 
    st_buffer(dist = 0) %>%
    mutate(superficie_ha = as.numeric(st_area(.)) /10000)
  
  
} else {
  url_comm <- "https://wxs-telechargement.ign.fr/x02uy2aiwjo9bm8ce5plwqmr/telechargement/prepackage/ADMINEXPRESS-PACK_2017-01-18$ADMINEXPRESS_1-0__SHP_LAMB93_FXX_2017-01-18/file/ADMINEXPRESS_1-0__SHP_LAMB93_FXX_2017-01-18.7z"
  download.file(url_comm, destfile = "/tmp/ADMINEXPRESS_1-0__SHP_LAMB93_FXX_2017-01-18.7z")
  system("7z x -o/tmp /tmp/ADMINEXPRESS_1-0__SHP_LAMB93_FXX_2017-01-18.7z")
  # importer shape des communes France métro
  
  comm <- st_read("/tmp/ADMINEXPRESS_1-0__SHP_LAMB93_FXX_2017-01-18/ADMINEXPRESS/1_DONNEES_LIVRAISON_2017-01-18/ADE_1-0_SHP_LAMB93_FR/COMMUNE.shp" , stringsAsFactors = F) %>% st_transform(crs = 2154) %>%
    mutate(CODGEO= ifelse(substr(INSEE_COM,1,2) == '75' ,'75056',
                          ifelse(substr(INSEE_COM,1,3) == '132' ,'13055',
                                 ifelse(substr(INSEE_COM,1,4) == '6938' ,'69123',INSEE_COM)))) %>%
    group_by(CODGEO) %>%
    summarize(NOM_COMM = first(NOM_COM),
              STATUT = first(STATUT),
              POPULATION = sum(POPULATION)) %>% 
    st_buffer(dist = 0) %>%
    mutate(superficie_ha = as.numeric(st_area(.)) /10000)
  
}


######@
## direct depuis poste

comm <- st_read("/Users/matai/Documents/OpenData_W/shp/COG2016/GEOFLA_2-2_COMMUNE_SHP_LAMB93_FXX_2016-06-28/GEOFLA/1_DONNEES_LIVRAISON_2016-06-00236/GEOFLA_2-2_SHP_LAMB93_FR-ED161/COMMUNE/COMMUNE.shp" , stringsAsFactors = F) %>% st_transform(crs = 2154) %>%
  mutate(CODGEO= ifelse(substr(INSEE_COM,1,2) == '75' ,'75056',
                        ifelse(substr(INSEE_COM,1,3) == '132' ,'13055',
                               ifelse(substr(INSEE_COM,1,4) == '6938' ,'69123',INSEE_COM)))) %>%
  group_by(CODGEO) %>%
  summarize(NOM_COMM = first(NOM_COM),
            STATUT = first(STATUT),
            POPULATION = sum(POPULATION)) %>% 
  mutate(NOM_COMM= ifelse(CODGEO == '75056' ,'PARIS',
                        ifelse(CODGEO == '13055' ,'MARSEILLE',
                               ifelse(CODGEO == '69123' ,'LYON',NOM_COMM)))) %>%
  st_buffer(dist = 0) %>%
  mutate(superficie_ha = as.numeric(st_area(.)) /10000)

comm.wgs84 <- comm %>% st_transform(4326)

## communes france metro + DOM

comm.frdom.wgs94 <- st_read("/Users/matai/Documents/OpenData_W/shp/COG2016/GEOFLA_2-2_COMMUNE_SHP_LAMB93_FXX_2016-06-28/GEOFLA/1_DONNEES_LIVRAISON_2016-06-00236/GEOFLA_2-2_SHP_LAMB93_FR-ED161/COMMUNE/COMMUNE.shp" , stringsAsFactors = F) %>% st_transform(crs = 4326) %>%
rbind.data.frame( st_read("/Users/matai/Documents/OpenData_W/shp/COG2016/GEOFLA_2-2__SHP_RGM04UTM38S_D976_2016-06-28/GEOFLA/1_DONNEES_LIVRAISON_2016-06-00236/GEOFLA_2-2_SHP_RGM04UTM38S_D976-ED161/COMMUNE/COMMUNE.shp" , stringsAsFactors = F) %>% st_transform(crs = 4326) ) %>%
  rbind.data.frame( st_read("/Users/matai/Documents/OpenData_W/shp/COG2016/GEOFLA_2-2_COMMUNE_SHP_RGR92UTM40S_D974_2016-06-28/GEOFLA/1_DONNEES_LIVRAISON_2016-06-00236/GEOFLA_2-2_SHP_RGR92UTM40S_D974-ED161/COMMUNE/COMMUNE.shp" , stringsAsFactors = F) %>% st_transform(crs = 4326) ) %>%
  rbind.data.frame( st_read("/Users/matai/Documents/OpenData_W/shp/COG2016/GEOFLA_2-2_COMMUNE_SHP_UTM20W84GUAD_D971_2016-06-28/GEOFLA/1_DONNEES_LIVRAISON_2016-06-00236/GEOFLA_2-2_SHP_UTM20W84GUAD_D971-ED161/COMMUNE/COMMUNE.shp" , stringsAsFactors = F) %>% st_transform(crs = 4326) ) %>%
  rbind.data.frame( st_read("/Users/matai/Documents/OpenData_W/shp/COG2016/GEOFLA_2-2_COMMUNE_SHP_UTM20W84MART_D972_2016-06-28/GEOFLA/1_DONNEES_LIVRAISON_2016-06-00236/GEOFLA_2-2_SHP_UTM20W84MART_D972-ED161/COMMUNE/COMMUNE.shp" , stringsAsFactors = F) %>% st_transform(crs = 4326) ) %>%
  rbind.data.frame( st_read("/Users/matai/Documents/OpenData_W/shp/COG2016/GEOFLA_2-2_COMMUNE_SHP_UTM22RGFG95_D973_2016-06-28/GEOFLA/1_DONNEES_LIVRAISON_2016-06-00236/GEOFLA_2-2_SHP_UTM22RGFG95_D973-ED161/COMMUNE/COMMUNE.shp" , stringsAsFactors = F) %>% st_transform(crs = 4326) ) %>%
  mutate(CODGEO= ifelse(substr(INSEE_COM,1,2) == '75' ,'75056',
                        ifelse(substr(INSEE_COM,1,3) == '132' ,'13055',
                               ifelse(substr(INSEE_COM,1,4) == '6938' ,'69123',INSEE_COM)))) %>%
  group_by(CODGEO) %>%
  summarize(NOM_COMM = first(NOM_COM),
            STATUT = first(STATUT),
            POPULATION = sum(POPULATION)) %>% 
  st_buffer(dist = 0) 


# ajout commune monaco
comm.MCO <- st_read("/Users/matai/Documents/OpenData_W/shp/COG2016/GEOFLA_MCO.shp" , stringsAsFactors = F) %>% 
  st_transform(crs = 2154) %>%
  rename(CODGEO = id) %>%
  mutate(NOM_COMM = "MONACO", STATUT = "", POPULATION = 37550, superficie_ha = 200)
 
comm <- comm %>% rbind.data.frame(comm.MCO)

comm.frdom.wgs94 <- comm.frdom.wgs94 %>% rbind.data.frame(comm.MCO %>% select(-superficie_ha) %>% st_transform(4326))
# centroides
comm.frdom.wgs94.ctr <- comm.frdom.wgs94 %>% st_centroid(of_largest_polygon = FALSE) %>%
  mutate(x_ctr = map_dbl(geometry, ~st_centroid(.x)[[1]]),
         y_ctr = map_dbl(geometry, ~st_centroid(.x)[[2]])) 


#### mailles supra communales

library(COGugaison)

library(rmapshaper)
dep <- comm %>% 
  left_join(table_supracom_2016 %>% select(CODGEO, DEP), by = "CODGEO") %>%
  group_by(DEP) %>% summarise()

dep.s <- dep %>%
  rmapshaper::ms_simplify(keep = 0.05)


# centroides
comm.ctr <- comm %>% st_centroid(of_largest_polygon = FALSE) %>%
  mutate(x_ctr = map_dbl(geometry, ~st_centroid(.x)[[1]]),
         y_ctr = map_dbl(geometry, ~st_centroid(.x)[[2]])) 

####################@
##################@
#### carte MONDE


# récupération du code pays
library(rnaturalearth)
library(sf)
library(janitor)
library(readxl)

REF_NED_pays.niv1.sp <- ne_countries(scale = "large")
REF_NED_pays.niv1 <- st_as_sf(REF_NED_pays.niv1.sp)

# régions UK
REF_NED_pays.niv2.UK.sp <- ne_states(country = 'united kingdom')
REF_NED_pays.niv2.UK <- st_as_sf(REF_NED_pays.niv2.UK.sp)
REF_NED_pays.niv2.UK.nat <- REF_NED_pays.niv2.UK %>% group_by(gu_a3,geonunit) %>% summarise() %>% rename(geounit = geonunit)
# fusion pays + régions UK
REF_NED_pays.niv1b <- REF_NED_pays.niv1 %>% select(gu_a3,geounit) %>% filter(!gu_a3 %in% 'GBR') %>% rbind(REF_NED_pays.niv2.UK.nat)
rm(REF_NED_pays.niv2.UK.sp,REF_NED_pays.niv2.UK, REF_NED_pays.niv2.UK.nat, REF_NED_pays.niv1.sp,REF_NED_pays.niv1  )

##################
### carte MONDE PAYS

### pays libellés en français
library(stringr)
library(rvest)

# libelles
REF_LIBFR_PAYS <- read_html("https://fr.wikipedia.org/wiki/ISO_3166-1") %>%
  html_nodes(xpath = "/html/body/div[3]/div[3]/div[4]/div/table[2]") %>%
  html_table(fill = T) %>%
  as.data.frame() %>%
  clean_names()

REF_LIBFR_PAYS <- REF_LIBFR_PAYS %>%
  add_row(num =1001, alpha_3 = "KOS", alpha_2 = "KO", codes_is_oregions = "(ISO 3166-2)", nom_francais = "Kosovo", nom_iso_du_pays_ou_territoire = "KOSOVO", nom_dans_la_langue_originale_en_forme_longue_traduction_en_francais = "Kosovo") %>%
  add_row(num =1002, alpha_3 = "PSX", alpha_2 = "PS", codes_is_oregions = "(ISO 3166-2)", nom_francais = "Palestine", nom_iso_du_pays_ou_territoire = "PALESTINE", nom_dans_la_langue_originale_en_forme_longue_traduction_en_francais = "Palestine") %>%
  add_row(num =1003, alpha_3 = "SDS", alpha_2 = "SD", codes_is_oregions = "(ISO 3166-2)", nom_francais = "Soudan du Sud", nom_iso_du_pays_ou_territoire = "SOUDAN DU SUD", nom_dans_la_langue_originale_en_forme_longue_traduction_en_francais = "Soudan du Sud") %>%
  add_row(num =1004, alpha_3 = "ENG", alpha_2 = "EN", codes_is_oregions = "(ISO 3166-2)", nom_francais = "Angleterre", nom_iso_du_pays_ou_territoire = "ANGLETERRE", nom_dans_la_langue_originale_en_forme_longue_traduction_en_francais = "Angleterre") %>%
  add_row(num =1005, alpha_3 = "NIR", alpha_2 = "NI", codes_is_oregions = "(ISO 3166-2)", nom_francais = "Irlande du Nord", nom_iso_du_pays_ou_territoire = "IRLANDE DU NORD", nom_dans_la_langue_originale_en_forme_longue_traduction_en_francais = "Irlande du Nord") %>%
  add_row(num =1006, alpha_3 = "SCT", alpha_2 = "SC", codes_is_oregions = "(ISO 3166-2)", nom_francais = "Ecosse", nom_iso_du_pays_ou_territoire = "ECOSSE", nom_dans_la_langue_originale_en_forme_longue_traduction_en_francais = "Ecosse") %>%
  add_row(num =1007, alpha_3 = "WLS", alpha_2 = "WL", codes_is_oregions = "(ISO 3166-2)", nom_francais = "Pays de Galles", nom_iso_du_pays_ou_territoire = "PAYS DE GALLES", nom_dans_la_langue_originale_en_forme_longue_traduction_en_francais = "Pays de Galles")

# rectifications à la marge
REF_LIBFR_PAYS <- REF_LIBFR_PAYS %>% 
  mutate(nom_francais.s = case_when( nom_francais %in% 'Chypre (pays)' ~ "Chypre",
                                     nom_francais %in% 'Comores (pays)' ~ "Comores",
                                     nom_francais %in% 'Équateur (pays)' ~ "Équateur",
                                     nom_francais %in% 'Géorgie (pays)' ~ "Géorgie",
                                     nom_francais %in% 'Grenade (pays)' ~ "Grenade",
                                     nom_francais %in% 'Irlande (pays)' ~ "Irlande",
                                     nom_francais %in% 'Luxembourg (pays)' ~ "Luxembourg",
                                     nom_francais %in% 'République de Macédoine (pays)' ~ "Macédoine",
                                     nom_francais %in% 'Maurice (pays)' ~ "Ile Maurice",
                                     nom_francais %in% 'République centrafricaine' ~ "Centrafrique",
                                     nom_francais %in% 'République démocratique du Congo' ~ "RD Congo",
                                     nom_francais %in% 'République du Congo' ~ "Congo",
                                     nom_francais %in% 'Taïwan / (République de Chine (Taïwan))' ~ "Taïwan",
                                     TRUE ~ as.character(nom_francais)))

REF_NED_pays.niv1c <- REF_NED_pays.niv1b %>% left_join(REF_LIBFR_PAYS %>% select(alpha_3,nom_francais,nom_francais.s,nom_iso_du_pays_ou_territoire ), by = c("gu_a3"= 'alpha_3'))

ref_SW_selection.NED <- REF_NED_pays.niv1c %>% select(gu_a3,geounit,nomfr_selection_NED=nom_francais.s, nom_iso_du_pays_ou_territoire, geometry) %>% 
  #left_join(ref_SW_selection, by = c('geounit'= 'nom_selection_NED')) %>%
  #rename(nom_selection_NED =geounit) %>%
  #filter(!is.na(team_id)) %>%
  group_by(gu_a3) %>% slice(1) %>% ungroup()


REF_NED_pays.s <- rmapshaper::ms_simplify(input = as(REF_NED_pays.niv1c %>% filter(!gu_a3 %in% c('ATA')), 'Spatial'),  keep = 0.05, method = "vis", keep_shapes = TRUE) %>% st_as_sf() %>% st_transform(3035) 

REF_NED_pays.s.ctr <- REF_NED_pays.s %>% 
  st_centroid(of_largest_polygon = TRUE) %>%
  mutate(lat_ctr = map_dbl(geometry, ~st_centroid(.x)[[1]]),
         lng_ctr = map_dbl(geometry, ~st_centroid(.x)[[2]])) %>%
  as.data.frame() %>% select(-geometry)
  
