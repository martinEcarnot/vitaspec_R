library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

root <- getwd()
prediction_dir <- file.path(root, "predictions")
output_dir <- file.path(prediction_dir, "meso_kinetics")
dir.create(output_dir, showWarnings = FALSE)

files <- c(
  Frais = "Meso_FRAIS_pred_moy.xlsx",
  `0j` = "Meso_SEC_0j_pred_moy.xlsx",
  `8j` = "Meso_SEC_8j_pred_moy.xlsx",
  `1 mois` = "Meso_SEC_1mois_pred_moy.xlsx",
  `3 mois` = "Meso_SEC_3mois_pred_moy.xlsx",
  `6 mois` = "Meso_SEC_6mois_pred_moy.xlsx"
)

paths <- file.path(prediction_dir, unname(files))
if (any(!file.exists(paths))) {
  stop("Fichier(s) manquant(s): ", paste(basename(paths[!file.exists(paths)]), collapse = ", "))
}

normalise_ech <- function(ech) {
  # Frais: R3-FE-P1; 0j: R3-P1-E; autres temps: R3-S1-T27-E.
  parcelle <- str_extract(ech, "(?:P|S)[0-9]+")
  parcelle <- str_replace(parcelle, "^S", "P")
  prefixe <- str_extract(ech, "^[^-]+")
  traitement <- case_when(
    str_detect(ech, "-[EK]$") ~ str_extract(ech, "[EK]$"),
    str_detect(ech, "-F[EK]-") ~ str_extract(ech, "(?<=-F)[EK](?=-)"),
    str_detect(ech, "-P[0-9]+$") ~ "P",
    TRUE ~ NA_character_
  )

  tibble(
    ech_commun = paste(prefixe, parcelle, sep = "-"),
    traitement = traitement
  )
}

predictions <- purrr::map_dfr(names(files), function(temps) {
  data <- read_excel(file.path(prediction_dir, files[[temps]]))
  if (!"ech" %in% names(data)) {
    stop("La colonne 'ech' est absente de ", files[[temps]])
  }

  identifiants <- normalise_ech(data$ech)
  bind_cols(data, identifiants) %>%
    mutate(
      temps = factor(temps, levels = names(files)),
      temps_jours = c(Frais = 0, `0j` = 10, `8j` = 18, `1 mois` = 40,
              `3 mois` = 100, `6 mois` = 190)[as.character(temps)]
    )
})

predictions_long <- predictions %>%
  pivot_longer(
    cols = where(is.numeric) & !any_of("temps_jours"),
    names_to = "variable",
    values_to = "valeur"
  ) %>%
  arrange(ech_commun, traitement, temps_jours, variable)

write.csv(
  predictions_long,
  file.path(output_dir, "meso_predictions_long.csv"),
  row.names = FALSE,
  na = ""
)

pdf(file.path(output_dir, "meso_kinetics.pdf"), width = 11, height = 7)
for (variable_name in unique(predictions_long$variable)) {
  plot_data <- filter(predictions_long, variable == variable_name)
  print(
    ggplot(plot_data, aes(
      x = temps_jours,
      y = valeur,
      group = interaction(ech_commun, traitement, drop = TRUE),
      colour = traitement
    )) +
      geom_line(na.rm = TRUE, alpha = 0.35) +
      geom_point(na.rm = TRUE, size = 1.4) +
      scale_x_continuous(
        breaks = c(0, 10, 18, 40, 100, 190),
        labels = c("Frais", "0j", "8j", "1 mois", "3 mois", "6 mois")
      ) +
      labs(
        title = variable_name,
        x = "Temps de conservation",
        y = "Valeur prédite",
        colour = "Traitement"
      ) +
      theme_minimal(base_size = 11) +
      theme(legend.position = "bottom")
  )
}
dev.off()

message("Données et graphiques écrits dans: ", output_dir)