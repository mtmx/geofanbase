
##################
###### PAYS


# nb flw dist par pays
cptOK_PAYS_flw_TOT <- cpt_PAYS_flw_TOT %>%
  mutate(nb = nb * 1 /(nb_FWLdist_geo_PAYS/nb_FWLdist)) %>%
  mutate(pct = nb / sum(nb))


# nb flw par club par pays
cptOK_PAYS_flw_CLUB <- cpt_PAYS_flw_club %>%
  mutate(nb = nb * 1 /(nb_FWLdist_geo_PAYS/nb_FWLdist)) %>%
  left_join(cpt_flw_CLUB %>%
              select(initiales_club, pctOK),
            by ="initiales_club") %>%
  mutate(nb = nb * pctOK) %>%
  select(-pctOK) %>%
  group_by(gu_a3) %>%
  mutate(pct.PAYS = nb / sum(nb)) %>%
  ungroup() %>%
  group_by(initiales_club) %>%
  mutate(pct.CLUB = nb / sum(nb))

# club max flw par PAYS
cptOK_PAYS_flw_CLUB.max <- cptOK_PAYS_flw_CLUB %>% ungroup() %>% group_by(gu_a3) %>% filter(nb == max(nb))


##################
### COMMUNE FRANCE


#### POP totale
cpt_COMM_redress <- comm.frdom.wgs94 %>% as.data.frame() %>%
  select(CODGEO, POPULATION) %>%
  #ungroup() %>%
  #mutate(pct_POP = POPULATION / sum(POPULATION)) %>%
  left_join(cpt_COMM_flw_TOT, by = "CODGEO") %>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) %>%
  #group_by(CODGEO) %>%
  mutate(pct_POPULATION.TOT = POPULATION / sum(POPULATION),
         pct_nb.TOT = nb / sum(nb)) %>%
  mutate(ratio_pct = pct_POPULATION.TOT/ pct_nb.TOT ) %>%
  mutate_if(is.numeric, funs(ifelse(is.infinite(.), 1, .))) 




# comptage du nombre de flw distincts par commune
cpt_COMM_flw_DIST <- liste_flw_dist.geo %>%
              filter(components._type %in% c('city','building','neighbourhood','postcode')) %>%
              select(user_id, CODGEO) %>%
  group_by(CODGEO) %>% 
  summarise(nb = n()) %>%
  mutate(nb = as.numeric(nb))

# comptage distincts par commune apres redressement
cptOK_COMM_flw_DIST <- cpt_COMM_flw_DIST %>%
  # redressement 
  left_join(cpt_COMM_redress %>% 
              select(CODGEO, ratio_pct), by = "CODGEO") %>%
  mutate(nb = nb * ratio_pct) %>%
  mutate(pct = nb / sum(nb)) %>%
  # problème centroide france 
  filter(!is.na(CODGEO) ) %>%
  as.data.frame() 


#### CLUBS

# comptage du nombre de flw par club et par commune
cpt_COMM_flw_CLUB <- liste_flw %>%
  left_join(liste_flw_dist.geo %>%
              filter(components._type %in% c('city','building','neighbourhood','postcode')) %>%
              select(user_id, CODGEO), by = "user_id") %>%
  group_by(CODGEO, alias) %>% 
  summarise(nb = n()) %>%
  mutate(nb = as.numeric(nb))

# comptage par club et par commune FRAMET apres redressement
cptOK_COMM_flw_CLUB <- cpt_COMM_flw_CLUB %>%
  # redressement pct fakes
  # left_join(cpt_flw_CLUB %>%
  #             select(alias_club, pctOK), by = "alias_club") %>%
  # mutate(nb = nb * pctOK) %>%
  # redressement 
  left_join(cpt_COMM_redress %>% 
              select(CODGEO, ratio_pct), by = "CODGEO") %>%
  mutate(nb = nb * ratio_pct) %>%
  mutate(pct = nb / sum(nb)) %>%
  # problème centroide france 
  filter(!is.na(CODGEO) ) %>%
  as.data.frame() 

