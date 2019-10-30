
# source
# http://www.maartenlambrechts.com/2018/07/31/beasts-of-europe.html


# divide the european continent into a 150 by 150 cell grid
fr_grid <- st_make_grid(comm.ctr, n = 100)
fr_grid <- fr_grid %>% st_sf() %>% mutate(id = row_number())

fr_grid.ctr <- fr_grid %>%
  mutate(x_ctr = map_dbl(geometry, ~st_centroid(.x)[[1]]),
         y_ctr = map_dbl(geometry, ~st_centroid(.x)[[2]])) %>%
  as.data.frame() %>% select(-geometry)

fr_grid.ctr.geo <- fr_grid %>%
  mutate(x_ctr = map_dbl(geometry, ~st_centroid(.x)[[1]]),
         y_ctr = map_dbl(geometry, ~st_centroid(.x)[[2]])) 
  
# version hex
fr_grid.hex <- dep %>% 
  st_make_grid(n = 70,square = FALSE)
fr_grid.hex <- fr_grid.hex %>% st_sf() %>% mutate(id = row_number())
fr_grid.hex.ctr <- fr_grid.hex %>%
  mutate(x_ctr = map_dbl(geometry, ~st_centroid(.x)[[1]]),
         y_ctr = map_dbl(geometry, ~st_centroid(.x)[[2]])) %>%
  as.data.frame() %>% select(-geometry)

#####################@
######## calcul pro rata surface

# # calcul du nombre de flw par CLUB par carreau
# grid.cpt_flw_CLUB <- comm %>%
#   left_join(cpt_COMM_flw_club, by = "CODGEO") %>%
#   filter(initiales_club %in% 'SRFC') %>%
#   select(nb) %>%
#   # calculate average herd size in each grid cell while preserving
#   # the observed total (extensive = TRUE)
#   st_interpolate_aw(to = fr_grid, extensive = TRUE) %>%
#   # return centroid coordinates for each grid cell
#   st_centroid() %>%
#   cbind(st_coordinates(.)) %>%
#   # arrange by value to plot lowest values first so that
#   # larger bubbles sit on top of smaller ones
#   arrange(nb) %>%
#   #filter out cells with missing data
#   filter(!is.na(nb))
# 
# # calcul du nombre de flw total DIST par carreau
# grid.cpt_flw_DIST <- comm %>%
#   left_join(cpt_COMM_flw_TOT, by = "CODGEO") %>%
#   select(nb) %>%
#   # calculate average herd size in each grid cell while preserving
#   # the observed total (extensive = TRUE)
#   st_interpolate_aw(to = fr_grid, extensive = TRUE) %>%
#   # return centroid coordinates for each grid cell
#   st_centroid() %>%
#   cbind(st_coordinates(.)) %>%
#   # arrange by value to plot lowest values first so that
#   # larger bubbles sit on top of smaller ones
#   arrange(nb) %>%
#   #filter out cells with missing data
#   filter(!is.na(nb))
# 
# #unification stats  
# grid.cpt_flw_CLUB.pct <- grid.cpt_flw_CLUB %>%
#   mutate(X = round(X,0), Y = round(Y,0)) %>%
#   left_join(grid.cpt_flw_DIST %>%
#               as.data.frame() %>%
#               mutate(X = round(X,0), Y = round(Y,0)) %>%
#               select(Group.1, nb_tot = nb, X, Y), by = c('X','Y')) %>%
#   mutate(pct.CLUB = nb /  nb_tot)


###################
##### via centroides communes
comm.grid <- st_intersection(comm.ctr, fr_grid)  

# plus grosses communes par carreau grid
comm.grid.liste.communes <- comm.grid %>%
  as.data.frame() %>%
  select(-geometry) %>%
  group_by(id) %>%
  filter(POPULATION == max(POPULATION)) %>%
  as.data.frame()

# calcul du nombre de flw total DIST par carreau
grid.cpt_flw_DIST <- comm %>%
  left_join(cptOK_COMM_flw_DIST, by = "CODGEO") %>%
  select(CODGEO, nb) %>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) %>%
  left_join(comm.grid %>%
              as.data.frame() %>%
              select(CODGEO, id), by = "CODGEO") %>%
  group_by(id) %>%
  summarise(nb = sum(nb)) %>%
  as.data.frame()


# calcul du nombre de flw par CLUB par carreau
grid.cpt_flw_CLUB <- comm %>%
  full_join(cptOK_COMM_flw_CLUB, by = "CODGEO") %>%
  #filter(initiales_club %in% 'SRFC') %>%
  select(CODGEO,initiales_club, nb) %>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) %>%
  left_join(comm.grid %>%
              as.data.frame() %>%
              select(CODGEO, id), by = "CODGEO") %>%
  group_by(id,initiales_club) %>%
  summarise(nb = sum(nb)) %>%
  as.data.frame()

#unification stats  
grid.cpt_flw_CLUB.pct <- grid.cpt_flw_CLUB %>%
  left_join(grid.cpt_flw_DIST %>%
              as.data.frame() %>%
              select(id, nb_tot = nb), by = "id") %>%
  ungroup() %>%
  mutate(pct.CLUB = nb /  nb_tot) %>%
  left_join(fr_grid.ctr %>%
              as.data.frame() %>%
              select(id, x_ctr, y_ctr), by = "id") %>%
  arrange(nb)
  
# club max flw par carreau GRID
grid.cpt_flw_CLUB.pct.max <- grid.cpt_flw_CLUB %>%
  ungroup() %>%
  arrange(desc(nb)) %>%
  group_by(id) %>%
  #slice(1:1) %>%
  #top_n(n=1) %>%
  filter(nb == max(nb)) %>%
  left_join(fr_grid.ctr %>%
              as.data.frame() %>%
              select(id, x_ctr, y_ctr), by = "id") %>%
  arrange(initiales_club) %>%
  left_join(liste_clubs_couleur %>%
              select(initiales_club, col_club),
            by = "initiales_club") %>%
  filter(!is.na(initiales_club))

# 2nd club
grid.cpt_flw_CLUB.pct.max.2 <- grid.cpt_flw_CLUB %>%
  ungroup() %>%
  arrange(desc(nb)) %>%
  group_by(id) %>%
  slice(2:2) %>%
  left_join(fr_grid.ctr %>%
              as.data.frame() %>%
              select(id, x_ctr, y_ctr), by = "id") %>%
  arrange(initiales_club) %>%
  left_join(liste_clubs_couleur %>%
              select(initiales_club, col_club),
            by = "initiales_club") %>%
  filter(!is.na(initiales_club))


grid.cpt_flw_CLUB.pct.max.top2 <- grid.cpt_flw_CLUB.pct.max %>% ungroup() %>%
  left_join(grid.cpt_flw_CLUB.pct.max.2 %>% ungroup() %>%
              select(id , initiales_club_n2 = initiales_club, col_club_n2 = col_club), by ="id") %>%
  ungroup()






####################
##### carto

# pct par club
  ggplot() +
  # contours départements
  geom_sf(data = dep.s, fill = "grey98", color = "grey90", stroke = 0.2) +
  
    geom_point(data = grid.cpt_flw_CLUB.pct %>%
                 filter(initiales_club %in% 'OL'),
               aes(x = x_ctr, y = y_ctr,size = nb, 
                   #fill = pct.CLUB),
                   fill = cut(pct.CLUB, breaks = c(seq(from = 0, to = 0.8, by = 0.1), 1)) ),
            shape = 21, color = 'grey90', stroke = 0.2,  show.legend = TRUE) +
    # contours départements
    geom_sf(data = dep.s, fill = NA, color = "grey90", stroke = 0.2) +
    #scale_fill_distiller(palette = "Reds", direction = 1) +

  
    scale_fill_brewer(palette = "Reds", name = "%") +
    #scale_size_continuous(range = c(0.02,5 ), trans = "log2", limits = c(10, 32000)) +
    #scale_size_continuous(range = c(0.2,3.5 )) +
    #scale_size_continuous(range = c(0.1,4 ), trans = "log1p", limits = c(10, 32000)) +
    scale_size_continuous(range = c(0.05,7 ), trans = "sqrt", limits = c(2, 32000), name = "nb") +
  
    scale_x_continuous(name = "") +
    scale_y_continuous(name = "") +
    coord_sf(crs = 2154) +
    #Add the animal icon
    #annotation_raster(animal.img, xmin = 2000000, xmax = 2500000, ymin = 4700000, ymax = 5200000) +
    #Ensures circles are sized by area
    #scale_size_area(max_size = 8) +
    theme_fivethirtyeight() +
    theme(axis.text = element_blank(),
          legend.position = c(0.9,0.6),
          legend.direction = "vertical",
          panel.grid = element_line(size = 0),
          panel.background = element_rect(fill = NA)) +
    labs(
      title = "Part de fans du club",
      subtitle = "par carreau de 10 km de côté",
      caption = "Source : API Twitter, septembre/octobre 2018"
    ) 

# club max

ggplot() +
  geom_point(#data = grid.cpt_flw_CLUB.pct.max ,
             data = grid.hex.cpt_flw_CLUB.pct.max,
             aes(x = x_ctr, y = y_ctr,size = nb, fill = initiales_club),
             shape = 21,
             color = 'grey90',
             #color = NA,
             stroke = 0.2,  show.legend = TRUE) +
  # contours departements
  geom_sf(data = dep.s, fill = NA, color = "grey90", stroke = 0.5) +
  #scale_fill_distiller(palette = "Reds", direction = 1) +
  scale_fill_manual(values = grid.hex.cpt_flw_CLUB.pct.max %>%
                      ungroup() %>%
                      arrange(initiales_club) %>%
                      filter(!is.na(initiales_club)) %>% 
                      distinct(initiales_club,.keep_all = TRUE) %>%
                      pull(col_club),
                    guide =  FALSE) +
  # images logos au lieu du stade
  # geom_image(data = affluence_club_visiteur_domicile %>% left_join(corresp_initialesclubs, by = c('club_actuel'= 'nom_club_actuel')),
  #            aes(x= moy_affluence.y, y = moy_affluence.x, size = nb_matchs.x, image = logo_png))

  #scale_size_continuous(range = c(0.1,4 ), trans = "log1p") +
  scale_size_continuous(range = c(1.5,5 ), trans = "sqrt", guide = FALSE) +
  #scale_size_continuous(range = c(5,5 )) +
  #scale_size_continuous(range = c(0.1,4 )) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") +
  coord_sf(crs = 2154) +
  #Add the animal icon
  #annotation_raster(animal.img, xmin = 2000000, xmax = 2500000, ymin = 4700000, ymax = 5200000) +
  #Ensures circles are sized by area
  #scale_size_area(max_size = 8) +
  #theme_ipsum() +
  theme_fivethirtyeight() +
  theme(axis.text = element_blank(),
        panel.grid = element_line(size = 0),
        panel.background = element_rect(fill = NA)) +
  labs(
    title = "Club ayant le plus de fans",
    subtitle = "par carreau de 10 km de côté",
    caption = "Source : API Twitter, septembre/octobre 2018"
  ) 
  

# duel pct par club


grid.cpt_flw_CLUB.pct.duel <- 
  grid.cpt_flw_CLUB.pct %>%
  ungroup() %>%
  select(-pct.CLUB) %>%
  filter(initiales_club %in% c('EAG', 'SB')) %>%
  group_by(id) %>%
  mutate(ratio.CLUB = nb / sum(nb)) %>%
  ungroup() %>%
  filter(initiales_club %in% c('EAG')) %>%
  # somme totale
  left_join(grid.cpt_flw_CLUB.pct %>%
              ungroup() %>%
              select(-pct.CLUB) %>%
              filter(initiales_club %in% c('EAG', 'SB')) %>%
              group_by(id) %>%
              mutate(nb.TOT = sum(nb) ) %>%
              distinct(id, .keep_all = TRUE) %>%
              select(id, nb.TOT), by = "id") #%>%
  # coordonnées carreau grid
  # left_join(fr_grid.ctr %>%
  #             as.data.frame() %>%
  #             select(id, x_ctr, y_ctr), by = "id") %>%
  # arrange(nb.TOT)



ggplot() +
  geom_point(data = grid.cpt_flw_CLUB.pct.duel ,
             aes(x = x_ctr, y = y_ctr,size = nb.TOT, 
                 fill = ratio.CLUB),
                 #fill = cut(ratio.CLUB, breaks = c(seq(from = 0, to = 1, by = 0.1), 1)) ),
             shape = 21, color = 'grey90', stroke = 0.02,  show.legend = TRUE) +
  # contours départements
  geom_sf(data = dep.s, fill = NA, color = "grey90", stroke = 0.2) +
  #scale_fill_distiller(palette = "Reds", direction = 1) +
  scale_fill_distiller(palette = "RdBu") +
  #scale_fill_brewer(palette = "RdBu") +
  #scale_size_continuous(range = c(0.02,5 ), trans = "log2", limits = c(10, 32000)) +
  #scale_size_continuous(range = c(0.2,3.5 )) +
  scale_size_continuous(range = c(0.1,5 ), trans = "sqrt") +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") +
  coord_sf(crs = 2154) +
  #Add the animal icon
  #annotation_raster(animal.img, xmin = 2000000, xmax = 2500000, ymin = 4700000, ymax = 5200000) +
  #Ensures circles are sized by area
  #scale_size_area(max_size = 8) +
  theme_minimal() +
  theme(axis.text = element_blank(), panel.grid = element_line(size = 0))







########################@
##################
### ggiraph
library(ggiraph)

# fonction pour gérer accents
conv_accents <- function(x) {
  x <- gsub(pattern = "è", replacement = "&egrave;", x = x)
  x <- gsub(pattern = "é", replacement = "&eacute;", x = x)
  x <- gsub(pattern = "ê", replacement = "&ecirc;", x = x)
  x <- gsub(pattern = "ë", replacement = "&euml;", x = x)
  x <- gsub(pattern = "î", replacement = "&icirc;", x = x)
  x <- gsub(pattern = "ï", replacement = "&iuml;", x = x)
  x <- gsub(pattern = "û", replacement = "&ucirc;", x = x)
  x <- gsub(pattern = "ü", replacement = "&uuml;", x = x)
  x <- gsub(pattern = "ô", replacement = "&ocirc;", x = x)
  x <- gsub(pattern = "à", replacement = "&agrave;", x = x)
  x <- gsub(pattern = "â", replacement = "&acirc;", x = x)
  x <- gsub(pattern = "ç", replacement = "&ccedil;", x = x)
  
  x <- gsub(pattern = "è", replacement = "&Egrave;", x = x)
  x <- gsub(pattern = "é", replacement = "&Eacute;", x = x)
  x <- gsub(pattern = "ê", replacement = "&Ecirc;", x = x)
  x <- gsub(pattern = "ë", replacement = "&Euml;", x = x)
  x <- gsub(pattern = "î", replacement = "&Icirc;", x = x)
  x <- gsub(pattern = "ï", replacement = "&Iuml;", x = x)
  x <- gsub(pattern = "û", replacement = "&Ucirc;", x = x)
  x <- gsub(pattern = "ü", replacement = "&Uuml;", x = x)
  x <- gsub(pattern = "ô", replacement = "&Ocirc;", x = x)
  x <- gsub(pattern = "à", replacement = "&Agrave;", x = x)
  x <- gsub(pattern = "â", replacement = "&Acirc;", x = x)
  x <- gsub(pattern = "ç", replacement = "&Ccedil;", x = x)
  x <- gsub(pattern = "'", replacement = "&apos;", x = x)
  
  return(x)
}
# style du popup
tooltip_css <- "background-color:white;padding:2px;font-size: 80%;color: white;opacity:0.2"

my_gg <-
  ggplot() +
  # contours departements
  geom_sf(data = dep.s, fill = "grey98", color = "grey95", stroke = 0.5) +
  geom_point_interactive(data = grid.cpt_flw_CLUB.pct.max %>% as.data.frame() %>% select(id, initiales_club, nb, x_ctr, y_ctr) %>%
               left_join(ref.club.epsg2154 %>% as.data.frame() %>% select(initiales_club, col_club, club_domicile_actuel, logo_png), by = c("initiales_club") ) %>%
                 left_join(comm.grid.liste.communes %>% select(id, NOM_COMM), by = "id")  %>%
                 mutate(tip = paste0("<style> div.leaflet-popup-content {width:auto!important;}</style>",
                                     "<img src = ",paste0('"' ,logo_png,'"'), " height=\"30\"width=\"30\">", "<br>",
                                     "<b>","<font size=1.5 color=white>" , conv_accents(NOM_COMM),"</b>","</font>", "<br>")) %>%
                 mutate(tip_img = paste0("<img src = ",paste0('"' ,logo_png,'"'), " height=\"30\"width=\"30\">"))%>%
                 filter(!is.na(initiales_club)),
             aes(x = x_ctr, y = y_ctr,size = nb, fill = initiales_club,
                 tooltip = tip,
                 data_id = id),
             shape = 22,
             color = 'grey90',
             #color = NA,
             stroke = 0.2,  show.legend = FALSE) +

  #scale_fill_distiller(palette = "Reds", direction = 1) +
  scale_fill_manual(values = grid.cpt_flw_CLUB.pct.max %>%
                      ungroup() %>%
                      arrange(initiales_club) %>%
                      filter(!is.na(initiales_club)) %>% 
                      distinct(initiales_club,.keep_all = TRUE) %>%
                      pull(col_club)) +

  #scale_size_continuous(range = c(0.5,5 ), trans = "sqrt") +
  scale_size_continuous(range = c(0.8,3 ), trans = "sqrt") +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") +
  coord_sf(crs = 2154, datum = NA) +
  theme_fivethirtyeight() +
  theme(axis.text = element_blank(),
        panel.grid = element_line(size = 0),
        panel.background = element_rect(fill = "white"),
        text = element_text(family="Helvetica")) #+
  # labs(
  #   title = "Club ayant le plus de fans",
  #   subtitle = "par carreau de 10 km de côté",
  #   caption = "Source : API Twitter, septembre/octobre 2018"
  # ) 
# 
# tooltip_css <- "background-color:grey;padding:2px;font-size: 80%;color: white;opacity:0.8"
# 
# x <- girafe(ggobj = my_gg, width = 1, height_svg = 5 )
# x <- girafe_options(x, 
#                opts_tooltip(css = tooltip_css),
#                opts_tooltip(offx = -40, offy = -30, use_cursor_pos = TRUE),
#                opts_tooltip(use_fill = FALSE),
#                opts_hover(css = "fill:red;r:10pt;"),
#                opts_zoom(max = 2),
#                opts_toolbar(position = "bottomleft") )
# x

tooltip_css <- "background-color:white;padding:2px;font-size: 80%;color: white;opacity:0.2"

#x <- girafe(ggobj = my_gg, width = 1, height_svg = 6 )
x <- girafe(ggobj = my_gg)
x <- girafe_options(x, 
               opts_tooltip(css = tooltip_css),
               opts_tooltip(offx = -40, offy = -30, use_cursor_pos = TRUE),
               opts_tooltip(use_fill = FALSE),
               opts_hover(css = "fill:red;r:10pt;"),
               opts_zoom(min = 1, max = 2),
               opts_toolbar(position = "topright", saveaspng = FALSE) )
x

library(widgetframe)
htmlwidgets::saveWidget(frameableWidget(x),
                        selfcontained = FALSE, 
                        libdir = "./deps",
                        'club_max_fans_v1.html')


#################
### visu carte avec hulls

library(ggforce)


# club avec %>30 flw par carreau GRID

grid.cpt_flw_CLUB.pct <- grid.cpt_flw_CLUB %>%
  left_join(grid.cpt_flw_DIST %>%
              as.data.frame() %>%
              select(id, nb_tot = nb), by = "id") %>%
  ungroup() %>%
  mutate(pct.CLUB = nb /  nb_tot) %>%
  left_join(fr_grid.ctr %>%
              as.data.frame() %>%
              select(id, x_ctr, y_ctr), by = "id") %>%
  arrange(nb)

grid.cpt_flw_CLUB.pctsup30 <-
  grid.cpt_flw_CLUB.pct %>%
  filter(pct.CLUB > 0.8) %>%
  arrange(initiales_club) %>%
  left_join(liste_clubs_couleur %>%
              select(initiales_club, col_club),
            by = "initiales_club") %>%
  filter(!is.na(initiales_club))


ggplot() +
  # contours departements
  geom_sf(data = dep.s, fill = "grey98", color = "grey95", stroke = 0.5) +
  # geom_point(data = grid.cpt_flw_CLUB.pct.max %>% as.data.frame() %>% select(id, initiales_club, nb, x_ctr, y_ctr) %>%
  #                          left_join(ref.club.epsg2154 %>% as.data.frame() %>% select(initiales_club, col_club, club_domicile_actuel, logo_png), by = c("initiales_club") ) %>%
  #                          left_join(comm.grid.liste.communes %>% select(id, NOM_COMM), by = "id")  %>%
  #                          filter(!is.na(initiales_club)),
  #                        aes(x = x_ctr, y = y_ctr,size = nb, fill = initiales_club),
  #                        shape = 22,
  #                        color = 'grey90',
  #                        #color = NA,
  #                        stroke = 0.2,  show.legend = FALSE) +
geom_mark_hull(data = grid.cpt_flw_CLUB.pctsup30 ,
                 aes(x = x_ctr, y = y_ctr, color = initiales_club),
               concavity = 6) +
  #scale_fill_distiller(palette = "Reds", direction = 1) +
  scale_fill_manual(values = grid.cpt_flw_CLUB.pct.max %>%
                      ungroup() %>%
                      arrange(initiales_club) %>%
                      filter(!is.na(initiales_club)) %>% 
                      distinct(initiales_club,.keep_all = TRUE) %>%
                      pull(col_club)) +
  
  #scale_size_continuous(range = c(0.5,5 ), trans = "sqrt") +
  scale_size_continuous(range = c(0.8,3 ), trans = "sqrt") +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") +
  coord_sf(crs = 2154, datum = NA) +
  theme_fivethirtyeight() +
  theme(axis.text = element_blank(),
        panel.grid = element_line(size = 0),
        panel.background = element_rect(fill = "white"),
        text = element_text(family="Helvetica"))


# version finale ggiraph

my_gg <-
  ggplot() +
  # contours departements
  geom_sf(data = dep.s, fill = "grey95", color = "grey75", size = 0.1) +
  # geom_point_interactive(data = grid.cpt_flw_CLUB.pct.max %>%
  #                          left_join(ref.club %>% as.data.frame(), by = c("initiales_club") ) %>%
  #                          left_join(comm.grid.liste.communes, by = "id") %>%
  #                          mutate(tip = paste0("<style> div.leaflet-popup-content {width:auto!important;}</style>",
  #                                              "<img src = ",paste0('"' ,logo_png,'"'), " height=\"30\"width=\"30\">", "<br>",
  #                                              "<b>","<font size=1.5 color=white>" , conv_accents(NOM_COMM),"</b>","</font>", "<br>")) %>%
  #                          mutate(tip_img = paste0("<img src = ",paste0('"' ,logo_png,'"'), " height=\"30\"width=\"30\">")) %>%
  #                          filter(!is.na(initiales_club)) ,
  geom_point_interactive(data = grid.cpt_flw_CLUB.pct.max %>% as.data.frame() %>% select(id, initiales_club, nb, x_ctr, y_ctr) %>%
                           left_join(ref.club.epsg2154 %>% as.data.frame() %>% select(initiales_club, col_club, club_domicile_actuel, logo_png), by = c("initiales_club") ) %>%
                           left_join(comm.grid.liste.communes %>% select(id, NOM_COMM), by = "id")  %>%
                           mutate(tip = paste0("<style> div.leaflet-popup-content {width:auto!important;}</style>",
                                               "<img src = ",paste0('"' ,logo_png,'"'), " height=\"30\"width=\"30\">", "<br>",
                                               "<b>","<font size=1.5 color=white>" , conv_accents(NOM_COMM),"</b>","</font>", "<br>")) %>%
                           mutate(tip_img = paste0("<img src = ",paste0('"' ,logo_png,'"'), " height=\"30\"width=\"30\">"))%>%
                           filter(!is.na(initiales_club)),
                         aes(x = x_ctr, y = y_ctr,
                             size = nb, 
                             fill = initiales_club,
                             tooltip = tip,
                             data_id = id),
                         shape = 21,
                         color = 'grey90',
                         #color = NA,
                         stroke = 0.2,  show.legend = FALSE) +
  
  scale_fill_manual(values = grid.cpt_flw_CLUB.pct.max %>%
                      ungroup() %>%
                      arrange(initiales_club) %>%
                      filter(!is.na(initiales_club)) %>% 
                      distinct(initiales_club,.keep_all = TRUE) %>%
                      pull(col_club)) +
  
  #scale_size_continuous(range = c(0.1,5 ), trans = "sqrt", limits = c(5, 32000)) +
  scale_size_continuous(range = c(0.8,3 ), trans = "sqrt") +
  scale_x_continuous(name = "", expand = c(0,0)) +
  scale_y_continuous(name = "", expand = c(0,0)) +
  coord_sf(crs = 2154, datum = NA) +
  #theme_ipsum_rc() +
  theme(axis.text = element_blank(),
        #grid = FALSE,
        text=element_text(family="Tahoma"),
        #legend.position = c(0.9,0.6),
        # legend.position = "none",
        legend.text = element_text(size=3,margin = margin(t = 10)),
        legend.spacing.x = unit(0.1, 'cm'),
        legend.spacing.y = unit(0.3, 'cm'),
        legend.direction = "vertical",
        panel.grid = element_line(size = 0),
        panel.border = element_blank(),
        panel.background = element_blank())
#panel.background = element_rect(fill = "white")) #+
# theme(axis.text = element_blank(),
#       panel.grid = element_line(size = 0),
#       panel.background = element_rect(fill = NA),
#       text = element_text(family="Helvetica")) #+
# labs(
#   title = "Club ayant le plus de fans",
#   subtitle = "par carreau de 10 km de côté",
#   caption = "Source : API Twitter, septembre/octobre 2018"
# ) 

tooltip_css <- "background-color:white;padding:2px;font-size: 80%;color: white;opacity:0.2"

x <- girafe(ggobj = my_gg, width_svg = 6.5, height_svg = 4.5)
x <- girafe(ggobj = my_gg, width_in_pixels = 2)
x <- girafe(ggobj = my_gg)
girafe_options(x, 
               opts_tooltip(css = tooltip_css),
               opts_tooltip(offx = -40, offy = -30, use_cursor_pos = TRUE),
               opts_tooltip(use_fill = TRUE),
               opts_hover(css = "fill:red;stroke:black"),
               opts_zoom(max = 1),
               opts_toolbar(saveaspng = FALSE) )
