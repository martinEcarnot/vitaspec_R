
nb_modeles_rejet_max = "23"
nb_modeles_rejet = "7"
Z_seuil = 4.5

HR_25_DIADE <- read.csv(paste0(d0,"vitaspec_R/ROSA_vitaSPEC/Data/X/HR_25_DIADE_clean.csv"))

HR_pred <- readxl::read_excel(paste0(d0,"vitaspec_R/ROSA_vitaSPEC/predictions/HR/clean_sd/2025/HR_1_to_317_pred.xlsx")) %>% 
  mutate(
    ech = as.integer(str_extract(ech, "(\\d+)(?=_[^_]*$)")),
    partie_chiffree = str_remove(str_extract(spectrum, "_[0-9]+$"), "_"),
    Repetition = as.integer(case_when(
      nchar(partie_chiffree) <= 2 ~ as.numeric(partie_chiffree),                  
      nchar(partie_chiffree) > 2  ~ as.numeric(str_sub(partie_chiffree, -1, -1))
    ))
  ) %>% 
  select(-c(1), -partie_chiffree) %>% 
  distinct(ech, Repetition, .keep_all = TRUE)

HR_pred_clean <- semi_join(HR_pred, HR_25_DIADE, by = c("ech" = "ech", "Repetition" = "rep"))

fileHR_long <- HR_pred_clean %>%
  pivot_longer(
    cols = -c(ech, Repetition), 
    names_to = "Variable", 
    values_to = "Valeur"
  )

# calcul du MAD par échantillon par variable
fileHR_long <- fileHR_long %>%
  group_by(Variable, ech) %>%
  mutate(
    mad_val = ifelse(mad(Valeur, constant = 1.4826, na.rm = TRUE) == 0, 1e-6, mad(Valeur, constant = 1.4826, na.rm = TRUE)), 
    mediane_val = median(Valeur, na.rm = TRUE),
    Z = abs(Valeur - mediane_val) / mad_val,
    Statut = ifelse(Z > Z_seuil, "Outlier", "Normal") 
  ) %>%
  ungroup()


variables <- unique(fileHR_long$Variable)
liste_ech <- sort(as.numeric(as.character(unique(fileHR_long$ech))))
groupes_ech <- cut(seq_along(liste_ech), breaks = 2, labels = FALSE)

ech_partie1 <- liste_ech[groupes_ech == 1]
ech_partie2 <- liste_ech[groupes_ech == 2]

label_p1 <- paste("Échantillons", min(ech_partie1), "à", max(ech_partie1), ")")
label_p2 <- paste("Échantillons", min(ech_partie2), "à", max(ech_partie2), ")")

fileHR_long <- fileHR_long %>%
  mutate(
    ech_num = as.numeric(as.character(ech)),
    partie = case_when(
      ech_num %in% ech_partie1 ~ label_p1,
      ech_num %in% ech_partie2 ~ label_p2
    ),
    partie = factor(partie, levels = c(label_p1, label_p2)),
    Statut = factor(Statut, levels = c("Normal", "Outlier"))
  ) 

mon_theme <- theme_bw() + 
  theme(
    plot.title = element_text(size = 14, hjust = 0, face = "bold", margin = margin(b = 15)),
    strip.background = element_rect(fill = "gray85", color = "black"),
    strip.text = element_text(size = 11, color = "black"),
    panel.border = element_rect(color = "black", fill = NA),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 6),
    legend.position = "bottom"
  )

nom_fichier_pdf <- paste0(d0, "vitaspec_R/ROSA_vitaSPEC/Data/outliers/mad/detection_outlier_HR_MAD_Z",Z_seuil,".pdf")
pdf(nom_fichier_pdf, width = 11.7, height = 8.3)

couleurs_statut <- c("Normal" = "grey70", "Outlier" = "red")

for (var in variables) {
  
  p <- fileHR_long %>%
    filter(Variable == var) %>%
    
    ggplot(aes(x = ech, y = Valeur, fill = Statut)) + 
    
    geom_line(aes(group = ech), color = "black", linewidth = 0.3, alpha = 0.5) +
    
    geom_point(shape = 21, size = 2.5, color = "black", stroke = 0.5) +
    
    facet_wrap(~ partie, scales = "free_x", ncol = 1) + 
    
    scale_fill_manual(values = couleurs_statut) +
    
    labs(
      title = paste("Analyse du composé :", var, "(MAD >", Z_seuil),
      x = "Échantillons", 
      y = "Valeur prédite",
      fill = "Répétition :"
    ) +
    mon_theme
  
  print(p)
}

dev.off()

tableau_outliers <- fileHR_long %>%
  filter(Statut == "Outlier") %>%
  
  select(
    Composé = Variable, 
    Echantillon = ech, 
    Repetition, 
    Valeur_Predite = Valeur, 
    Mediane_Echantillon = mediane_val, 
    madval = mad_val,
    Score_MAD = Z
  ) %>%
  
  arrange(Composé, Echantillon, desc(Score_MAD))

write_xlsx(tableau_outliers, paste0(d0,"vitaspec_R/ROSA_vitaSPEC/Data/outliers/mad/liste_outliers_HR_MAD_",nb_modeles_rejet_max,"_Z",Z_seuil,".xlsx"))





MAD <- readxl::read_excel(paste0(d0,"vitaspec_R/ROSA_vitaSPEC/data/outliers/mad/liste_outliers_HR_MAD_",nb_modeles_rejet_max,"_Z",Z_seuil,".xlsx")) %>% 
  filter(Composé != "a.",
         Composé != "C14.0",
         Composé != "C18.0",
         Composé != "total.trans.carotenes.natif",
         Composé != "ratio.alpha.beta",
         Composé != "ratio.alpha.natif",
         Composé != "ratio.beta.natif",
         Composé != "X13.cis.beta.carotene",
         Composé != "total.beta.carotene",
         Composé != "total.carotene",
         Composé != "aT",
         Composé != "aT3",
         Composé != "dT3",
         Composé != "gT3",
         Composé != "lycopene",
         Composé != "total.T3", 
         Composé != "total.toco"
  )

MAD_groupe <- MAD %>%
  group_by(Echantillon) %>%
  summarise(
    rep = paste(Repetition, collapse = ", "),
    compose = paste(Composé, collapse = ", "),
    .groups = "drop"
  )


# Analyse par répétition d'échantillon
decision_tab <- MAD %>%
  group_by(Echantillon, Repetition) %>%
  summarise(
    nb_modeles_rejet = n(),                 # nombre de compose qui detecte cette rep
    score_mad_moyen = mean(Score_MAD),      # intensité moyenne de l'outlier
    modeles_impactes = paste(unique(Composé), collapse = ", "), # liste des composes
    .groups = 'drop'
  ) %>%
  arrange(desc(nb_modeles_rejet), desc(score_mad_moyen))


# top rep

mod_vip <- paste(c("C16.0", "C18.1n9", "C18.2","FFA"), collapse = "|")

decision_tab <- decision_tab %>%
  mutate(
    statut = case_when(
      nb_modeles_rejet >= 4 ~ "Rouge",
      nb_modeles_rejet %in% c(2, 3) & str_detect(modeles_impactes, mod_vip) ~ "Orange",
      TRUE ~ "Vert"
    ),
    statut = factor(statut, levels = c("Vert", "Orange", "Rouge"))
  )

tab_orange <- decision_tab %>% filter(statut == "Orange")



p_complet <- ggplot(MAD, aes(x = Composé, y = as.factor(interaction(Echantillon, Repetition)))) +
  geom_tile(aes(fill = Score_MAD)) +
  scale_fill_gradient(low = "#00BFC4", high = "#F8766D") +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10) 
  ) +
  labs(title = "Intensité des outliers par spectre et composé",
       subtitle = "zones rouges = fort consensus de detection",
       y = "Spectres (Echantillon x Répétition)")

legende_seule <- cowplot::get_legend(p_complet)
p_sans_legende <- p_complet + theme(legend.position = "none")


pdf(file = paste0(d0, "vitaspec_R/ROSA_vitaSPEC/data/outliers/mad/Intensite_outliers_Z",Z_seuil,".pdf"), width = 12, height = 8)
print(p_sans_legende)
print(plot_grid(legende_seule)) 

dev.off()

write_xlsx(tab_orange, paste0(d0,"vitaspec_R/ROSA_vitaSPEC/data/outliers/mad/tab_orange_Z",Z_seuil,".xlsx"))
write_xlsx(decision_tab, paste0(d0,"vitaspec_R/ROSA_vitaSPEC/data/outliers/mad/liste_spectres_a_supprimer_",nb_modeles_rejet,"_Z",Z_seuil,".xlsx"))
