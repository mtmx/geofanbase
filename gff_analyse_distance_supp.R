
#hors France / moins de 50 km / France plus de 50 km ?

# comptage nombre de l'étranger
library(tidyverse)

# total flw par club
cpt_flw_CLUB <- fread( "./docs/data_sorties/cpt_flw_CLUB.csv")

library(waffle)

cptOK_FR_ETR_flw_CLUB <-
cptOK_PAYS_flw_CLUB %>%
  mutate(fr_etr = case_when(geounit %in% 'France' ~ "France", TRUE ~ "Etranger")) %>%
  group_by(initiales_club,fr_etr) %>%
  summarise(nb = sum(nb)) %>%
  mutate(pct = nb /sum(nb)) %>%
  left_join(cpt_flw_CLUB %>% 
              select(initiales_club, nb_tot = nb), by = "initiales_club") %>%
  mutate(nb_tot_prorata = nb_tot * pct) %>%
  mutate(nb_tot_prorata_s = nb_tot_prorata / 5000) %>%
  filter(initiales_club %in% c('PSG','OM','ASM','OL','ASSE','LOSC','FCN')) %>%
  left_join(ref.club %>%
              select(initiales_club, club_domicile_actuel),
            by = 'initiales_club') %>%
  mutate(nom_club = factor(club_domicile_actuel, levels = c("Paris Saint-Germain FC", "Olympique de Marseille","AS Monaco", "Olympique Lyonnais",  "AS Saint-Etienne", "Lille OSC", 'FC Nantes'))) %>%
  # mutate(nom_club = factor(club_domicile_actuel, levels = c("Paris\nSaint-Germain", "Olympique\nde Marseille","AS\n Monaco", "Olympique\nLyonnais",  "AS\nSaint-Etienne", "Lille OSC", 'FC Nantes'))) %>%
  mutate(fr_etr = factor(fr_etr, levels = c("Etranger", 'France'))) %>%
  mutate(nom_club = recode_factor(nom_club, "Paris Saint-Germain FC" = "Paris\nSaint-Germain", "Olympique de Marseille" = "Olympique\nde Marseille", "AS Monaco" = "AS Monaco", "Olympique Lyonnais" = "Olympique\nLyonnais", "AS Saint-Etienne" = "AS\nSaint-Etienne", "Lille OSC" = "Lille OSC", 'FC Nantes' = 'FC Nantes'))


library(dplyr)
library(waffle)
library(hrbrthemes)

ggplot(cptOK_FR_ETR_flw_CLUB, 
       aes(fill = fr_etr, values = nb_tot_prorata_s)) +
  geom_waffle(color = "white", size = .25, n_rows = 10, flip = TRUE) +
  facet_wrap(~nom_club, nrow = 1, strip.position = "bottom") +
  scale_x_discrete() + 
  scale_y_continuous(labels = function(x) x * 10, # make this multiplyer the same as n_rows
                     expand = c(0,0)) +
  scale_fill_manual(values = c("#ffb1a0", "#6495ed"), name ="") +
  guides(fill = guide_legend(reverse = TRUE)) +
  #ggthemes::scale_fill_tableau(name=NULL) +
  #coord_equal() +
  labs(
    title = "Nombre de followers par club français selon leur localisation",
    subtitle = "1 carré = 5 000 followers",
    # x = "Year",
    # y = "Count",
    caption = "Source : API Twitter fin 2018"
  ) +
  theme_minimal(base_family = "Roboto") +
  #theme_ipsum() +
  theme(panel.grid = element_blank(),
        legend.position = c(0.8,0.5),
        plot.title = element_text(margin = margin(t = 30, b = -20), hjust = 0.5),
        plot.subtitle = element_text(margin = margin(t = 30, b = -30), hjust = 0.5),
        #axis.ticks.y = element_line()
        strip.text.x = element_text(size = 10, face = "bold"),
        axis.text.y = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE))


# nuage de points nb flw vs part de flws français
library(ggrepel)
library(scales)

ggplot() +
  # geom_text(data = cpt_flw_CLUB %>%
  #             left_join( cptOK_PAYS_flw_CLUB %>%
  #                          select(initiales_club,geounit,pct.CLUB ) %>%
  #                          filter(geounit %in% 'France'), by = "initiales_club"),
  #           aes(x= pct.CLUB, y = nb, label = initiales_club), size = 2) +
  # images logos au lieu du stade
  geom_image(data = ref.club %>%
               left_join( cpt_flw_CLUB %>%
                            left_join( cptOK_PAYS_flw_CLUB %>%
                                         select(initiales_club,geounit,pct.CLUB ) %>%
                                         filter(geounit %in% 'France'), by = "initiales_club")
                          , by = "initiales_club"),
             aes(x= 1-pct.CLUB, y = nb, image = logo_png),
             size = 0.04) +
  #scale_fill_manual(name = "", values = c( "black", "orange","gold","purple","red")) +
  theme_ipsum() +
  scale_y_continuous(name = "Nombre de followers du compte officiel du club sur Twitter", labels =function(x) format(x, big.mark = " ", scientific = FALSE)) +
  scale_x_continuous(name = "Part des followers localisés à l'étranger", labels =scales::percent_format(accuracy = 2)) +
  #facet_grid(~alias_club) +
  theme(#legend.position = c(0.95, 0.5),
    legend.position = "none",
    text = element_text(family="Helvetica"),
    legend.key.size = unit(0.5,"cm")) +
  coord_flip() +
  labs(
    title = "Nombre de fans par club",
    subtitle = "et localisation en France",
    caption = "Source : API Twitter"
  ) 

# version ggiraph
my_gg <-
ggplot() +
  geom_image(data = ref.club %>%
               left_join( cpt_flw_CLUB %>%
                            left_join( cptOK_PAYS_flw_CLUB %>%
                                         select(initiales_club,geounit,pct.CLUB ) %>%
                                         filter(geounit %in% 'France'), by = "initiales_club")
                          , by = "initiales_club"),
             aes(x= 1-pct.CLUB, y = nb, image = logo_png),
             size = 0.03) +
  geom_point_interactive(data = ref.club %>%
                           left_join( cpt_flw_CLUB %>%
                                        left_join( cptOK_PAYS_flw_CLUB %>%
                                                     select(initiales_club,geounit,pct.CLUB ) %>%
                                                     filter(geounit %in% 'France'), by = "initiales_club")
                                      , by = "initiales_club") %>%
                           mutate(tip = paste0("<style> div.leaflet-popup-content {width:auto!important;}</style>",
                                               "<img src = ",paste0('"' ,logo_png,'"'), " height=\"30\"width=\"30\">", "<br>",
                                               "<b>","<font size=1.5 color=white>" , conv_accents(club_domicile_actuel),"</b>","</font>", "<br>")) %>%
                           mutate(tip_img = paste0("<img src = ",paste0('"' ,logo_png,'"'), " height=\"30\"width=\"30\">")) %>%
                           filter(!is.na(initiales_club)) ,
                         aes(x= 1-pct.CLUB, y = nb,
                             #size = nb, 
                             #fill = initiales_club,
                             tooltip = tip,
                             data_id = initiales_club),
                         shape = 21,
                         color = "grey95",
                         #color = NA,
                         fill = "black",
                         alpha = 0.01,
                         size =4,
                         stroke = 0.2, 
                         show.legend = FALSE) +
  
  #scale_fill_manual(name = "", values = c( "black", "orange","gold","purple","red")) +
  theme_ipsum() +
  scale_y_continuous(name = "Nombre de followers du compte officiel du club sur Twitter", labels =function(x) format(x, big.mark = " ", scientific = FALSE)) +
  scale_x_continuous(name = "Part des followers localisés à l'étranger", labels =scales::percent_format(accuracy = 2)) +
  #facet_grid(~alias_club) +
  theme(#legend.position = c(0.95, 0.5),
    legend.position = "none",
    text = element_text(family="Helvetica"),
    legend.key.size = unit(0.5,"cm")) +
  coord_flip() +
  labs(
    title = "Nombre de fans par club",
    subtitle = "et localisation en France",
    caption = "Source : API Twitter"
  ) 

tooltip_css <- "background-color:white;padding:0px;font-size: 80%;color: white;opacity:0.2"

x <- girafe(ggobj = my_gg, height_svg  = 5)
#x <- girafe(ggobj = my_gg, width = 1, height_svg = 10)
girafe_options(x, 
               opts_tooltip(css = tooltip_css),
               opts_tooltip(offx = -40, offy = -30, use_cursor_pos = TRUE),
               opts_tooltip(use_fill = TRUE),
               opts_hover(css = "fill:grey90;stroke:grey90;opacity:0.8"),
               opts_zoom(max = 1),
               opts_toolbar(saveaspng = FALSE) )



################

cptOK_FRETR_flw_CLUB.lrg <-
  cptOK_PAYS_flw_CLUB %>%
  mutate(fr_etr = case_when(geounit %in% 'France' ~ "France", TRUE ~ "Etranger")) %>%
  group_by(initiales_club,fr_etr) %>%
  summarise(nb = sum(nb)) %>%
  select(initiales_club,fr_etr, nb) %>%
  pivot_wider(names_from = "fr_etr", values_from = "nb", names_prefix = "nb_")


ggplot() +
  # images logos au lieu du stade
  geom_image(data = ref.club %>%
               left_join( cptOK_FRETR_flw_CLUB.lrg
                          , by = "initiales_club"),
             aes(x= nb_Etranger, y = nb_France, image = logo_png),
             size = 0.04) +
  #scale_fill_manual(name = "", values = c( "black", "orange","gold","purple","red")) +
  theme_ipsum() +
  scale_y_log10(name = "Nombre de fans (échelle logarithmique)", 
                labels =function(x) format(x, big.mark = " ", scientific = FALSE)) +
  scale_x_log10(name = "Parts de fans localisés en France", 
                     labels =function(x) format(x, big.mark = " ", scientific = FALSE)) +
  #facet_grid(~alias_club) +
  theme(#legend.position = c(0.95, 0.5),
    legend.position = "none",
    text = element_text(family="Helvetica"),
    legend.key.size = unit(0.5,"cm")) +
  labs(
    title = "Nombre de fans par club",
    subtitle = "et localisation en France",
    caption = "Source : API Twitter"
  ) 
