library(ggthemes)

# code couleur clubs

liste_clubs_couleur <- tribble(
  ~initiales_club, ~alias_club, ~col_club,  
  "SRFC", "staderennais", "#FF2800",
  "FCN", "FCNantes", "#FFEF00",
  "FCL", "FCLorient", "#FF8C00",
  "EAG", "EAGuingamp", "#000000",
  "SB", "SB29", "#FF4040",
  "ASC", "AmiensSC", "#91A3B0",
  "SCO", "AngersSCO", "#000000",
  "FCGB", "girondins", "#0018A8",
  "SMC", "SMCaen", "#5D8AA8",
  "DFCO", "DFCO_Officiel", "#FF0800",
  "LOSC", "losclive", "#ED2939",
  "OL", "OL", "#AB274F",
  "OM", "OM_Officiel", "#89CFF0",
  "ASM", "AS_Monaco", "#FF0000",
  "MHSC", "MontpellierHSC", "#ED872D",
  "OGCN", "ogcnice", "#B5651D",
  "NO", "nimesolympique", "#CE2029",
  "PSG", "PSG_inside", "#554177",
  "SR", "StadeDeReims", "#CB4154",
  "ASSE", "ASSEofficiel", "#00AD43",
  "RCSA", "RCSA", "#1F75FE",
  "TFC", "ToulouseFC", "#966FD6",
  "ACA", "ACAjaccio", "#FF0800",
  "GFCA", "gfc_ajaccio", "#333399",
  "AJA", "AJA", "#7CB9E8",
  "ASB", "ASBeziersFoot", "#FF0800",
  "LBC", "LaBerrichonne", "#F2003C",
  "CF", "ClermontFoot", "#D70A53",
  "GF", "GF38_Officiel", "#6495ED",
  "HAC", "HAC_Foot", "#1E90FF",
  "RCL", "RCLens", "#FFE302",
  "FCM", "FCMetz", "#C71585",
  "ASNL", "asnlofficiel", "#FF3855",
  "CN", "ChamoisNiortais", "#2E5894",
  "USO", "US_Orleans", "#FFD300",
  "PFC", "ParisFC", "#062A78",
  "RS", "RedStarFC", "#0E7C61",
  "FCSM", "FCSM_officiel", "#EEE600",
  "ESTAC", "estac_officiel", "#966FD6",
  "VAFC", "VAFC", "#FF2800", 
  "SL", "stadelavallois", "#FF6103",
  "SCB", "SCBastia", "#1F75FE",
  "TOFC", "ToursFC", "#6CADDE")



###########
### logos


# initiales clubs
corresp_initialesclubs <- tribble(
  ~ nom_club_actuel, ~ initiales_nom_club_actuel,  
  "Angers SCO" , "SCO",
  "OGC Nice", "OGCN",
  "Olympique de Marseille", "OM",
  "AS Monaco", "ASM",
  "Lille OSC", "LOSC",
  "Dijon FCO" ,"DFCO",              
  "Olympique Lyonnais","OL",
  "FC Nantes" ,"FCN",
  "FC Girondins de Bordeaux", "FCGB",
  "FC Metz" ,"FCM",
  "Toulouse FC" ,"TFC",
  "Paris Saint-Germain FC" , "PSG",
  "Stade Rennais FC" ,"SRFC",
  "Montpellier Hérault SC", "MHSC",
  "ESTAC Troyes","ESTAC",
  "EA Guingamp" ,"EAG",
  "AS Saint-Etienne", "ASSE",
  "RC Strasbourg Alsace","RCS",
  "Amiens SC" ,"AMSC",
  "SM Caen","SMC",
  "FC Lorient" ,"FCL",
  "AS Nancy Lorraine" , "ASNL",
  "SC Bastia","SCB",
  "Stade de Reims" ,"SR",
  "GFC Ajaccio","GFCA",
  "Évian Thonon Gaillard FC", "ETGFC",
  "RC Lens","RCL",
  "Valenciennes FC", "VAFC",
  "FC Sochaux-Montbéliard","FCSM",
  "AC Ajaccio", "ACA",
  "Stade Brestois 29" , "SB29",
  "AJ Auxerre", "AJA",
  "AC Arles Avignon" , "ACAA",
  "Grenoble Foot 38" ,"GF38",
  "US Boulogne CO" ,"USB",
  "Le Mans UC 72" , "MUC72",
  "Havre AC","HAC",
  "CS Sedan" ,"CSSA",
  "FC Istres", "FCI",
  "LB Châteauroux" ,"LBC",
  "AS Cannes" ,"ASC",
  "FC Gueugnon" ,"FCG",
  "FC Martigues" ,"FCMA",
  "SC Toulon et du Var", "SCT",
  "Nîmes Olympique", "NO",
  "Racing Paris 1" ,"RCF",
  "FC Mulhouse", "FCMU")

# import logos clubs
corresp_initialesclubs <- corresp_initialesclubs %>%
  mutate(logo_png = paste0("./logos_clubs/",initiales_nom_club_actuel,".png"))



