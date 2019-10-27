## comptage globaux FLW_dist

nb_FWLdist <- liste_flw_FULL_data %>% nrow()

nb_FWLdist_geo_PAYS <- liste_flw_FULL_data %>% 
  left_join(liste_flw_FULL_dist.geo %>%
              select(user_id, gu_a3, geounit), by = "user_id") %>%
  filter(!is.na(gu_a3) ) %>% nrow()


# # comptage du nombre de flws selon le nombre de clubs suivi
cpt_FLW_nbclubs <- liste_flw_clubs_FULL %>%
  group_by(user_id) %>% summarise(nb =n())
cpt_FLW_nbclubs.cpt <- cpt_FLW_nbclubs %>%
  group_by(nb) %>% summarise(nb_flw = n())


# comptage du nombre de flws par pays
cpt_PAYS_flw_TOT <- liste_flw_FULL_dist.geo %>%
  # merge FRANCE et monaco
  mutate(gu_a3 = case_when(gu_a3 %in% 'MCO' ~ "FRA", TRUE ~ as.character(gu_a3))) %>%
  mutate(geounit = case_when(gu_a3 %in% 'FRA' ~ "France", TRUE ~ as.character(geounit))) %>%
  select(user_id, gu_a3, geounit) %>%
  filter(!is.na(gu_a3)) %>%
  group_by(gu_a3,geounit) %>% 
  summarise(nb = n()) %>%
  mutate(nb = as.numeric(nb)) %>%
  ungroup() %>%
  mutate(pct = nb / sum(nb))

# comptage du nombre de flws par commune
cpt_COMM_flw_TOT <- #liste_flw_FULL_dist.geo %>%
  liste_flw_dist.geo %>%
  filter(components._type %in% c('city','building','neighbourhood','postcode')) %>%
  select(user_id, CODGEO) %>%
  group_by(CODGEO) %>% 
  summarise(nb = n()) %>%
  mutate(nb = as.numeric(nb)) %>%
  mutate(pct = nb / sum(nb)) %>%
  # problème centroide france 
  filter(!is.na(CODGEO) ) %>%
  as.data.frame() 

nb_FWLdist_geo_COMM <- cpt_COMM_flw_TOT %>% summarise(nb = sum(nb))


# comptage par PAYS et par CLUB
cpt_PAYS_flw_club <- liste_flw_clubs_FULL %>%
  left_join(liste_flw_FULL_dist.geo %>%
              # merge FRANCE et monaco
              mutate(gu_a3 = case_when(gu_a3 %in% 'MCO' ~ "FRA", TRUE ~ as.character(gu_a3))) %>%
              mutate(geounit = case_when(gu_a3 %in% 'FRA' ~ "France", TRUE ~ as.character(geounit))) %>%
              select(user_id, gu_a3, geounit), by = "user_id") %>%
  group_by(gu_a3, initiales_club) %>% 
  summarise(nb = n(), geounit = first(geounit)) %>%
  mutate(nb = as.numeric(nb)) %>%
  # problème centroide france 
  filter(!is.na(gu_a3) ) %>%
  as.data.frame()

# comptage du nombre de flw par club
cpt_flw_CLUB <- liste_flw_clubs_FULL %>%
  group_by( initiales_club) %>% 
  summarise(nb = n()) %>%
  left_join(liste_clubs_FULL.fakes %>%
              select(initiales_club, pct_flw_estim_fakes),
            by ="initiales_club") %>%
  mutate(nbOK = nb * (1-(pct_flw_estim_fakes/100))) %>%
  mutate(pctOK = nbOK / nb)

  cpt_flw_CLUB <- liste_flw %>%
  group_by( alias) %>% 
    summarise(nb = n()) 
  
  
# pct flws geocodés
cpt_GEO_flw_club <- cpt_PAYS_flw_club %>%
  filter(!is.na(gu_a3)) %>%
  group_by(initiales_club) %>%
  summarise(nb = sum(nb)) %>%
  left_join(cpt_flw_CLUB %>% rename(nb_TOT = nb), by = "initiales_club") %>%
  mutate(pct_geo = nb / nb_TOT)

# redressement nb geocodés PAYS / nb geocode COMM /  nb total flw

# comptage geo pays
cpt_PAYS_flw_club %>% group_by(initiales_club) %>% summarise(nb = sum(nb))


# club max flw par COMM
cpt_COMM_flw_club.max <- cpt_COMM_flw_club %>% ungroup() %>% group_by(CODGEO) %>% filter(nb == max(nb))



