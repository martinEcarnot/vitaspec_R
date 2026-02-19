install.packages("readxl")
install.packages("ggpmisc")

library(readxl)
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(ggpmisc)

library(readr)
d0="C:/Users/U108-N806/Documents/STAGE_M2_ROSA_NIRS/VitaSPEC/"



# Recup echantillons nouvelles analyses
AG_old <- read_excel(paste0(d0,"analyses_ref/fruits/Partie 1.2_old.xlsx"))
AG_tot <- read_excel(paste0(d0,"analyses_ref/fruits/Partie 1.2.xlsx"))

AG_new <- anti_join(AG_tot, AG_old, by = "code_nirs")
write_csv(AG_new, paste0(d0,"vitaspec_R/ROSA_vitaSPEC/Data/AG_new.csv"))

Tlip_old <- read_excel(paste0(d0,"analyses_ref/fruits/Partie 1.4_old.xlsx"))
Tlip_tot <- read_excel(paste0(d0,"analyses_ref/fruits/Partie 1.4.xlsx"))

Tlip_new <- anti_join(Tlip_tot, Tlip_old, by = "code_nirs")
write_csv(Tlip_new, paste0(d0,"vitaspec_R/ROSA_vitaSPEC/Data/Tlip_new.csv"))

Ref <- AG_new %>% 
  full_join(Tlip_new, by = "code_nirs") %>% 
  mutate(code_nirs = str_remove(code_nirs, "-.*")) %>% 
  select(-last_col())


# Recup pred echantillons nouvelles analyses
Pred <- read_excel(paste0(d0,"pred/Pobe_24-25_NIRS_Meso_Frais_pred_moy.xlsx")) %>% 
  rename(code_nirs = ech) %>% 
  mutate(code_nirs = str_remove(code_nirs, "-.*")) %>% 
  rename('C14:0' = 'C14.0', 
         'C16:0'= 'C16.0', 
         'C18:0' = 'C18.0' ,
         'C18:1n9' = 'C18.1n9',
         'C18:1n7' = 'C18.1n7',
         'C18:2' = 'C18.2',
         'C18:3' = 'C18.3',
         'C20:0' = 'C20.0',
         'tlip%MS' = 'tlip.MS')
         
Pred_new <- semi_join(Pred,Ref, by = "code_nirs") %>% 
  select(all_of(intersect(names(Pred),names(Ref))))
         

# PIVOT
Pred_new_long <- Pred_new %>%
  pivot_longer(
    cols = -code_nirs,
    names_to = "Compose",   
    values_to = "Predite"    
  )

Ref_long <- Ref %>%
  pivot_longer(
    cols = -code_nirs,
    names_to = "Compose",
    values_to = "Mesuree"
  )

data_graph <- inner_join(Pred_new_long, Ref_long, by = c("code_nirs", "Compose"))


# GRAPH
p <- ggplot(data_graph, aes(x = Predite, y = Mesuree)) +
  geom_point(shape = 1, alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  geom_smooth(method = "lm", color = "blue", se = FALSE, size = 0.5) +
  stat_poly_eq(aes(label = after_stat(rr.label)), 
               formula = y ~ x, 
               parse = TRUE, 
               label.x = "left", 
               label.y = "top") +
  facet_wrap(~ Compose, scales = "free", ncol = 3) + 
  theme_bw() +
  labs(
    title = "Mesocarpe Frais - 24_25 - Nouvelles analyses",
    x = "Teneur Prédite par SPIR",
    y = "Teneur Mesuree"
  )
p

ggsave("nouvelles_analyses_chimiques.pdf", plot = p, path = paste0(d0,"vitaspec_R/ROSA_vitaSPEC/Results"),width = 12, height = 8)






