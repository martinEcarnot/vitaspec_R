
library(dplyr)
d0="C:/Users/U108-N806/Documents/STAGE_M2_ROSA_NIRS/VitaSPEC/"

tlip_CAL <- read.table(paste0(d0,"vitaspec_R/analyses_ref_csv/Partie 1.4.csv"), 
                   sep=";", 
                   header=TRUE, 
                   dec=",")
tlip_CAL$ech <- sub("-.*", "", tlip_CAL$code_nirs)

tlip_old <- read_excel(paste0(d0,"analyses_ref/fruits/Partie 1.2_old.xlsx"))
tlip_old$ech <- sub("-.*", "", tlip_old$code_nirs)

tlip_tot <- read_excel(paste0(d0,"analyses_ref/fruits/Partie 1.2.xlsx"))
tlip_tot$ech <- sub("-.*", "", tlip_tot$code_nirs)
  
  
  
doublons_tlip <- tlip_tot %>%
  add_count(ech) %>%
  filter(n > 1) %>%
  arrange(ech)

exclus_tlip_CAL <- tlip_CAL %>%
  anti_join(tlip_old, by = "ech")

exclus_tlip_CAL_tot <- tlip_CAL %>%
  anti_join(tlip_tot, by = "ech")


exclus_tlip_old <- tlip_old%>%
  anti_join(tlip_CAL, by = "ech")

exclus_tlip_old_tot <- tlip_old%>%
  anti_join(tlip_tot, by = "ech")


exclus_tlip_tot <- tlip_tot%>%
  anti_join(tlip_old, by = "ech")
