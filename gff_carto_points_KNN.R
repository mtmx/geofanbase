library(tidyverse) # ggplot2, dplyr, tidyr, readr, purrr, tibble
library(magrittr) # pipes
library(forcats) # easier factor handling,
library(lintr) # code linting
library(sf) # spatial data handling
library(rnaturalearth) # country borders geometries from naturalearth.org
library(foreach) # parallel computing
library(doParallel) # parallel computing
library(kknn) # categorical knn"



cptOK_COMM_flw_CLUB.tr <- cptOK_COMM_flw_CLUB %>%
  filter(!is.na(initiales_club)) %>%
  select(CODGEO, initiales_club, nb) %>%
  spread(initiales_club, nb) %>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) %>%
  mutate(TOT = rowSums(.[2:44]))

cptOK_COMM_flw_CLUB.geo <- comm %>% select(CODGEO) %>%
  left_join(cptOK_COMM_flw_CLUB.tr, by = "CODGEO") %>%
  filter(!is.na(TOT))

sf_data <- cptOK_COMM_flw_CLUB.geo %>% 
  filter(substr(CODGEO,1,2) %in% c('29')) %>%
  #mutate_at(.vars= vars(ACA:VAFC), .funs = funs((./TOT)*10 )) 
  mutate_at(.vars= vars(ACA:TOT), .funs = funs(round(./5,0)))

sf_data <- cptOK_COMM_flw_CLUB.geo %>% 
  #filter(substr(CODGEO,1,2) %in% c('22','56','35','29','44','53')) %>%
  #filter(substr(CODGEO,1,2) %in% c('29')) %>%
  #select(CODGEO,PSG, OM, OL,ASM, ASSE, FCN, LOSC, SRFC, FCGB, OGCN, TFC, SRFC, MHSC, EAG, RCL, FCL, SMC,SL,VAFC, ASNL,FCM, SR, SCO, RCSA, SCB) %>%
  select(CODGEO, ACA:VAFC) %>%
  #mutate_at(.vars= vars(c(SRFC,SB,FCN,FCL,EAG,PSG)), .funs = funs((./TOT)*10 ))  %>%
  #mutate_at(.vars= vars(c(SRFC,SB,FCN,FCL,EAG,PSG, OM, SL)), .funs = funs(round(./5,0)))  %>%
  mutate_at(.vars= vars(ACA:VAFC), .funs = funs(round(.,0)))
  
  #mutate_at(.vars= vars(AJA:VAFC), .funs = funs(round(sqrt(.),0)))
  #mutate_at(.vars= vars(c(PPSG, OM, OL,ASM, ASSE, FCN, LOSC, SRFC, FCGB, OGCN, TFC, SRFC, MHSC, EAG, RCL, FCL, SMC, ASNL,FCM, SR, SCO, RCSA,SL, SCB)), .funs = funs(round(sqrt(.),0)))  #%>%
  #mutate_at(.vars= vars(c(SRFC,SB,FCN,FCL,EAG,PSG, OM, SL)), .funs = funs(round(.,0)))  %>%
  #mutate_if(is.numeric, .funs = funs(round(.,0))) %>%
  #select(-TOT)

sf_data <- cptOK_COMM_flw_CLUB.geo %>% 
  #filter(substr(CODGEO,1,2) %in% c('06','83','13','04','84','30','34')) %>%
  #filter(substr(CODGEO,1,2) %in% c('29')) %>%
  select(CODGEO,OM,OL) %>%
  #mutate_at(.vars= vars(c(SRFC,SB,FCN,FCL,EAG,PSG)), .funs = funs((./TOT)*10 ))  %>%
  #mutate_at(.vars= vars(c(SRFC,SB,FCN,FCL,EAG,PSG, OM, SL)), .funs = funs(round(./5,0)))  %>%
  #mutate_at(.vars= vars(AJA:VAFC), .funs = funs(round(.,0)))
  mutate_at(.vars= vars(OM:OL), .funs = funs(round(sqrt(.),0)))
#mutate_at(.vars= vars(ACA:VAFC), .funs = funs(round(sqrt(.),0)))
#mutate_at(.vars= vars(c(OGCN,OM,ASM,PSG,NO,MHSC,SCB, ASB)), .funs = funs(round(sqrt(.),0)))


######################
# stat max par commune

# COMM_flw_CLUB.max <- cptOK_COMM_flw_CLUB %>%
#     ungroup() %>%
#     arrange(desc(nb)) %>%
#     group_by(CODGEO) %>%
#     filter(nb == max(nb)) %>%
#     select()
# 
#   etrs <- "+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs"
#   comm.ctr.etrs <- comm.ctr %>% select(CODGEO) %>% st_transform(etrs)
# 
#   comm.ctr.etrs <- comm.ctr.etrs %>% st_centroid(of_largest_polygon = FALSE) %>%
#     mutate(x_ctr = map_dbl(geometry, ~st_centroid(.x)[[1]]),
#            y_ctr = map_dbl(geometry, ~st_centroid(.x)[[2]]))
# 
# 
#   COMM_flw_CLUB.max.geo <- comm.ctr %>% as.data.frame() %>% select(CODGEO, x_ctr, y_ctr) %>%
#     left_join(COMM_flw_CLUB.max, by = "CODGEO") %>%
#     filter(substr(CODGEO,1,2) %in% c('22','56','35','29')) %>%
#     filter(!is.na(nb)) %>%
#     select(x = x_ctr, y = y_ctr, initiales_club)
# 
#   pts_train <- COMM_flw_CLUB.max.geo

###https://www.cultureofinsight.com/blog/2018/05/02/2018-04-08-multivariate-dot-density-maps-in-r-with-sf-ggplot2/


# credit to Jens von Bergmann for this algo https://github.com/mountainMath/dotdensity/blob/master/R/dot-density.R
random_round <- function(x) {
  v=as.integer(x)
  r=x-v
  test=runif(length(r), 0.0, 1.0)
  add=rep(as.integer(0),length(r))
  add[r>test] <- as.integer(1)
  value=v+add
  ifelse(is.na(value) | value<0,0,value)
  return(value)
}

# data frame of number of dots to plot for each party (1 for every 100 votes)
num_dots <- as.data.frame(sf_data) %>% 
  select(ACA:VAFC) %>% 
  #select(OGCN,OM,ASM,PSG,NO,MHSC,SCB, ASB) %>%
  mutate_all(random_round)

# generates data frame with coordinates for each point + what party it is assiciated with
sf_dots <- map_df(names(num_dots), 
                  ~ st_sample(sf_data, size = num_dots[,.x], type = "random") %>% # generate the points in each polygon
                    st_cast("POINT") %>%                                          # cast the geom set as 'POINT' data
                    st_coordinates() %>%                                          # pull out coordinates into a matrix
                    as_tibble() %>%                                               # convert to tibble
                    setNames(c("x","y")) %>%                                  # set column names
                    mutate(initiales_club = .x)                                            # add categorical party variable
) %>% 
  slice(sample(1:n())) # once map_df binds rows randomise order to avoid bias in plotting order



## carto

# ggplot() +
#   # contours departements
#   #geom_sf(data = dep.s, fill = "grey96", color = "grey", stroke = 1.5) +
#   # images logos au lieu du stade
#   geom_point(data = sf_dots ,
#              aes(x= x, y = y, color = initiales_club), size = 0.5) +
#   scale_x_continuous(name = "") +
#   scale_y_continuous(name = "") +
#   #coord_sf(crs = 2154) +
#   theme_economist_white() +
#   theme(axis.text = element_blank(), panel.grid = element_line(size = 0), text = element_text(family="Helvetica")) 
# 



library(data.table)
clubs_train <- sf_dots %>% rename(club = initiales_club, lon = x, lat  =y)
#grid <- st_read("./grid.shp")

point_data <- clubs_train %>%
  #filter(!is.na(latitude)) %>%
  st_as_sf(., coords = c("lon", "lat"), crs = 2154) 




# zone
fr <- dep %>% mutate(pays = "FR") %>% group_by(pays) %>% summarise()

# fr <- dep %>% filter(DEP %in% c('29','22','35','56','44')) %>%
#   mutate(pays = "FR") %>% group_by(pays) %>% summarise()

# fr <- dep %>% filter(DEP %in% c('06','83','13','04','84','30','34')) %>%
#   mutate(pays = "FR") %>% group_by(pays) %>% summarise()

fr_buffered <- fr %>%
  st_buffer(dist = 10000) # in meters, 10km buffer

# specify grid width in pixels
width_in_pixels <- 800
# dx is the width of a grid cell in meters
dx <- ceiling( (st_bbox(fr_buffered)["xmax"] - 
                  st_bbox(fr_buffered)["xmin"]) / width_in_pixels)
# dy is the height of a grid cell in meters
# because we use quadratic grid cells, dx == dy
dy <- dx
# calculate the height in pixels of the resulting grid
height_in_pixels <- floor( (st_bbox(fr_buffered)["ymax"] - 
                              st_bbox(fr_buffered)["ymin"]) / dy)

grid <- st_make_grid(fr_buffered, 
                     cellsize = dx,
                     n = c(width_in_pixels, height_in_pixels),
                     what = "centers"
)

rm(dx, dy, height_in_pixels, fr_buffered)

#plot(grid)


clubs_train <- data.frame(club = point_data$club, 
                               lon = st_coordinates(point_data)[, 1], 
                               lat = st_coordinates(point_data)[, 2]) %>%
# only keep 8 most prominent clubs
#clubs_train %<>%
  group_by(club) %>% 
  nest() %>% 
  mutate(num = map_int(data, nrow)) %>% 
  arrange(desc(num)) %>% 
  #slice(1:4) %>% 
  #filter(club %in% c('PSG','OM','OL','ASM')) %>%
  #filter(club %in% c('RCSA','FCM','ASNL',"SR","FCSM","DFCO","AJA","ESTAC")) %>%
  filter(club %in% c('RCL','LOSC','VAFC',"ASC","SR","HAC")) %>%
  #filter(club %in% c('SRFC','FCN','FCL',"EAG","SB","SMC",'SCO',"SL")) %>%
  #filter(club %in% c('OM','ASB','MHSC',"OGCN","ASM","NO")) %>%
  unnest() %>% 
  select(-num)

# clean up
#rm(point_data)


# config
k <- 400 # "k" for k-nearest-neighbour-interpolation

# specify function which is executed for each tile of the grid
computeGrid <- function(grid, clubs_train, knn) {
  # create empty result data frame
  clubs_result <- data.frame(club = as.factor(NA), 
                                  lon = st_coordinates(grid)[, 1], 
                                  lat = st_coordinates(grid)[, 2])
  # run KKNN
  clubs_kknn <- kknn::kknn(club ~ ., 
                                train = clubs_train, 
                                test = clubs_result, 
                                kernel = "gaussian", 
                                k = knn)
  # bring back to result data frame
  # only retain the probability of the dominant club at that grid cell
  clubs_result %<>%
    # extract the interpolated club at each grid cell with the 
    # kknn::fitted function
    mutate(club = fitted(clubs_kknn),
           # only retain the probability of the interpolated club,
           # discard the other 7
           prob = apply(clubs_kknn$prob, 
                        1, 
                        function(x) max(x)),
           C.max = apply(clubs_kknn$C, 
                        1, 
                        function(x) max(x)),
           C.mean = apply(clubs_kknn$C, 
                        1, 
                        function(x) mean(x)),
           W.mean = apply(clubs_kknn$W, 
                          1, 
                          function(x) mean(x)),
           W.max = apply(clubs_kknn$W, 
                         1, 
                         function(x) max(x)),
           D.max = apply(clubs_kknn$D, 
                         1, 
                         function(x) max(x)),
           D.mean = apply(clubs_kknn$D, 
                          1, 
                          function(x) mean(x)))
  return(clubs_result)
}

# specify the number of cores below (adapt if you have fewer cores or
# want to reserve some computation power to other stuff)
registerDoParallel(cores = 4)

# specify number of batches and resulting size of each batch (in grid cells)
no_batches <- 20
batch_size <- ceiling(length(grid) / no_batches)

# PARALLEL COMPUTATION
start.time <- Sys.time()
clubs_result <- foreach(
  batch_no = 1:no_batches, 
  # after each grid section is computed, rbind the resulting df into
  # one big clubs_result df
  .combine = rbind, 
  # the order of grid computation doesn't matter: this speeds it up even more
  .inorder = FALSE) %dopar% {
    # compute indices for each grid section, depending on batch_size and current
    # batch
    start_idx <- (batch_no - 1) * batch_size + 1
    end_idx <- batch_no * batch_size
    # specify which section of the grid to interpolate, using regular
    # subsetting
    grid_batch <- grid[start_idx:ifelse(end_idx > length(grid), 
                                        length(grid),
                                        end_idx)]
    # apply the actual computation to each grid section
    df <- computeGrid(grid_batch, clubs_train, k)
  }
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

## Time difference of 3.773613 mins

# convert resulting df back to sf object, but do not remove raw geometry cols
clubs_raster <- st_as_sf(clubs_result, 
                              coords = c("lon", "lat"),
                              crs = 2154,
                              remove = F)
# clip raster to Germany again
clubs_raster <- clubs_raster[fr, ]
rm(clubs_result, k)

# 
# theme_map <- function(...) {
#   theme_minimal() +
#     theme(
#       text = element_text(family = "Ubuntu Regular", color = "#22211d"),
#       # remove all axes
#       axis.line = element_blank(),
#       axis.text.x = element_blank(),
#       axis.text.y = element_blank(),
#       axis.ticks = element_blank(),
#       axis.title.x = element_blank(),
#       axis.title.y = element_blank(),
#       # add a subtle grid
#       panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
#       panel.grid.minor = element_blank(),
#       plot.background = element_rect(fill = "#f5f5f2", color = NA), 
#       plot.margin = unit(c(.5, .5, .2, .5), "cm"),
#       panel.border = element_blank(),
#       panel.background = element_rect(fill = "#f5f5f2", color = NA), 
#       panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),
#       legend.background = element_rect(fill = "#f5f5f2", color = NA),
#       legend.title = element_text(size = 13),
#       legend.text = element_text(size = 11, hjust = 0, color = "#4e4d47"),
#       plot.title = element_text(size = 16, hjust = 0.5, color = "#4e4d47"),
#       plot.subtitle = element_text(size = 12, hjust = 0.5, color = "#4e4d47", 
#                                    margin = margin(b = -0.1, 
#                                                    t = -0.1, 
#                                                    l = 2, 
#                                                    unit = "cm"), 
#                                    debug = F),
#       plot.caption = element_text(size = 9, 
#                                   hjust = .5, 
#                                   margin = margin(t = 0.2, 
#                                                   b = 0, 
#                                                   unit = "cm"), 
#                                   color = "#939184"),
#       ...
#     )
# }



# ggplot(data = clubs_raster.2) +
#   geom_sf(data = fr, color = "gray60", fill = NA, size = .4) +
#   # add raster geom for the knn result
#   geom_raster(aes(x = lon, y = lat, fill = club, alpha = prob)) +
#   # add Germany outline
#   # geom_path(data = germany_outline, aes(x = long, y = lat, group = group),
#   #           colour = "gray60", size = .4) +
#   # remove propability legend
#   scale_alpha_continuous(guide = F) +
#   # add color brewer palette and legend titlw
#   # scale_fill_brewer(palette = "Dark2", 
#   #                   name = "club"
#   # ) +
#   # apply map theme
#   #theme_map() +
#   labs(title = "German clubs of the verb \"to chatter\"",
#        subtitle = "Based on a sample of >100k speakers across Germany, KNN with K=1000",
#        caption = "Map CC-BY-SA; Author: Timo Grossenbacher (@grssnbchr), \nGeometries: Natural Earth; Data: Leemann et al., 2015;")


### respect code couleur

CLUB_knn_k400_N <- clubs_raster %>%
  left_join(liste_clubs_couleur %>%
              select(initiales_club, col_club),
            by = c("club"= "initiales_club")) %>%
  filter(!is.na(club))

CLUB_knn_k600_PSGOMOLASM <- clubs_raster %>%
left_join(liste_clubs_couleur %>%
            select(initiales_club, col_club),
          by = c("club"= "initiales_club")) %>%
  filter(!is.na(club))

CLUB_knn_k400_N <- clubs_raster %>%
  left_join(liste_clubs_couleur %>%
              select(initiales_club, col_club),
            by = c("club"= "initiales_club")) %>%
  filter(!is.na(initiales_club))


CLUB_knn_k1000_PSGOMOL
CLUB_knn_PSGOMOL

# club max

ggplot() +
  geom_sf(data = fr, color = "grey60", fill = NA, size = .4) +
  # add raster geom for the knn result
  geom_raster(data = CLUB_knn_k600_N,
              aes(x = lon, y = lat, fill = club, alpha = prob)) +
  # contours departements
  geom_sf(data = dep, fill = NA, color = "grey40", lwd = 0.2) +
  # logos clubs
  geom_image(data = ref.club.epsg2154 %>% 
               #filter(initiales_club %in% c('PSG','OM','OL')) %>%
               #filter(initiales_club %in% c('RCSA','FCM','ASNL',"SR","FCSM","DFCO","AJA","ESTAC")) %>%
               filter(initiales_club %in% c('RCL','LOSC','VAFC',"ASC")) %>%
               #filter(initiales_club %in% c('SRFC','FCN','FCL',"EAG","SB","SMC","SL",'SCO')) %>%
               #filter(initiales_club %in% c('OM','ASB','MHSC',"OGCN","ASM","NO")) %>%
               as.data.frame(),
             aes(x= x.ctr, y = y.ctr,
                 width = capacite_stade, image = logo_png), size = 0.016) +
  # nom de villes
  geom_sf_text(data = comm %>%
               #filter(club %in% c('PSG','OM','OL')) %>%
               filter(!STATUT %in% "Commune simple") %>%
                 filter(!NOM_COMM %in% c('LENS', "LILLE", "VALENCIENNES", "AMIENS")),
                 #filter(!NOM_COMM %in% c('MARSEILLE', "BEZIERS", "MONTPELLIER", "NICE","NIMES")),
                 #filter(!NOM_COMM %in% c('RENNES', "BREST", "LORIENT", "GUINGAMP","LAVAL","NANTES","ANGERS")),
                 #filter(!NOM_COMM %in% c('METZ', "NANCY", "MONTBELIARD", "STRASBOURG","REIMS","DIJON","TROYES","AUXERRE")),
             aes(label = NOM_COMM),
             fontface = 'bold',
             label.padding = unit(0.5, "mm"),
             label.r = unit(0.1, "mm"),
             fill = "grey80",
             box.padding = unit(0.5, "lines"),
             family = "Roboto",
             size = 1.5) +
  # geom_point(data = ref.club.epsg2154 %>% 
  #              #filter(club %in% c('PSG','OM','OL')) %>%
  #              #filter(initiales_club %in% c('RCSA','FCM','ASNL',"SR","FCSM","DFCO")) %>%
  #              #filter(initiales_club %in% c('RCL','LOSC','VAFC',"ASC")) %>%
  #              #filter(initiales_club %in% c('SRFC','FCN','FCL',"EAG","SB","SMC","SL",'SCO')) %>%
  #              filter(initiales_club %in% c('OM','ASB','MHSC',"OGCN","ASM","NO")) %>%
  #              as.data.frame(),
  #            aes(x= x.ctr, y = y.ctr),
  #            shape = 21,
  #            fill = NA,
  #            color = "black",
  #            size = 3,
  #            lwd = 0.5) +
  #scale_fill_distiller(palette = "Reds", direction = 1) +
  scale_fill_manual(values = CLUB_knn_k600_N %>%
                      ungroup() %>%
                      st_drop_geometry() %>%
                      as.data.frame() %>%
                      arrange(club) %>%
                      filter(!is.na(club)) %>% 
                      distinct(club,.keep_all = TRUE) %>%
                      mutate(col_club = case_when(club %in% 'VAFC' ~ "#800080", TRUE ~ as.character(col_club))) %>%
                      pull(col_club),
                    guide =  FALSE) +
  # images logos au lieu du stade
  # geom_image(data = affluence_club_visiteur_domicile %>% left_join(corresp_initialesclubs, by = c('club_actuel'= 'nom_club_actuel')),
  #            aes(x= moy_affluence.y, y = moy_affluence.x, size = nb_matchs.x, image = logo_png))
  
  scale_alpha_continuous(guide = F) +
  #scale_size_continuous(range = c(1.5,5 ), trans = "sqrt", guide = FALSE) +
  #scale_size_continuous(range = c(5,5 )) +
  #scale_size_continuous(range = c(0.1,4 )) +
  # scale_x_continuous(name = "") +
  # scale_y_continuous(name = "") +
  # zoom
  # scale_x_continuous(limits = c( st_bbox(dep %>% filter(DEP %in% '88') %>%
  #                                          #st_transform(., raster::projection(basemap_c)) %>%
  #                                          st_buffer(dist = 200000))$xmin %>% as.numeric(),
  #                                st_bbox(dep %>% filter(DEP %in% '88') %>%
  #                                          #st_transform(., raster::projection(basemap_c)) %>%
  #                                          st_buffer(dist = 200000))$xmax %>% as.numeric()), name = "") +
  # scale_y_continuous(limits = c( st_bbox(dep %>% filter(DEP %in% '88') %>%
  #                                          #st_transform(., raster::projection(basemap_c)) %>%
  #                                          st_buffer(dist = 200000))$ymin %>% as.numeric(),
  #                                st_bbox(dep %>% filter(DEP %in% '88') %>%
  #                                          #st_transform(., raster::projection(basemap_c)) %>%
  #                                          st_buffer(dist = 20000))$ymax %>% as.numeric()), name = "") + 
  
  coord_sf(crs = 2154) +
  #Add the animal icon
  #annotation_raster(animal.img, xmin = 2000000, xmax = 2500000, ymin = 4700000, ymax = 5200000) +
  #Ensures circles are sized by area
  #scale_size_area(max_size = 8) +
  #theme_ipsum() +
  # theme_fivethirtyeight() +
  # theme(axis.text = element_blank(),
  #       panel.grid = element_line(size = 0),
  #       panel.background = element_rect(fill = NA)) +
  theme(axis.text = element_blank(),
        plot.title = element_text(family="Roboto",size = 10, margin=margin(10,0,0,0)),
        plot.subtitle = element_text( family="Roboto",size = 6, margin=margin(10,0,0,0)),
        text=element_text(family="Tahoma"),
        legend.key = element_blank(),
        legend.background = element_rect(color = "grey95"),
        legend.title = element_text(family="Roboto",size = 8),
        legend.text = element_text(family="Roboto",size = 6, margin = margin(t = 0.2)),
        legend.spacing.x = unit(0.1, 'cm'),
        legend.spacing.y = unit(0.2, 'cm'),
        #legend.position = c(0.9,0.6),
        #legend.position = "none",
        legend.position = "right",
        legend.direction = "vertical",
        legend.box = "vertical",
        #legend.text = element_text(size=3,margin = margin(t = 10)),
        # legend.spacing.x = unit(0.1, 'cm'),
        # legend.spacing.y = unit(0.3, 'cm'),
        #panel.background = element_rect(fill = "grey"),
        panel.background = element_blank(),
        panel.grid = element_line(size = 0)) +
  
  # labs(
  #   #title = paste0("Part de fans du club :\n", nom.club)#,
  #   #subtitle = "par carreau de 10 km de côté"#,
  #   #caption = "Source : API Twitter, septembre/octobre 2018"
  #   #caption = ""
  # ) 
  labs(
    title = "PSG vs OM vs OL"#,
    #subtitle = "par carreau de 10 km de côté",
    #caption = "Source : API Twitter, septembre/octobre 2018"
  ) 
  

# ggsave(filename = "./img/clubs_knn_SE_v1.png", 
#        device = 'png', width = 9,height = 9)

ggsave(filename = "./img/clubs_knn_N_v3.png", 
       dpi = 400,
       device = 'png',
       width = 20,height = 20)

##########
## zoom

zoom_to <- c(43.27, 5.38)  # ~ center of Kamchatka
zoom_level <- 8

# Lambert azimuthal equal-area projection around center of interest
target_crs <- sprintf('+proj=laea +lon_0=%f +lat_0=%f',
                      zoom_to[1], zoom_to[2])


st_bbox(dep %>%
          filter(DEP %in% '88') %>%
          st_buffer(dist = 200000))
