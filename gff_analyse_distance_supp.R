
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

