compile_spectres_matrix <- function(..., colonnes_spectres, filtre_etat = "aucun") {
  
  data <- bind_rows(list(...))
  
  table <- list()
  
  for (i in colonnes_spectres) {
    df_temp <- data
    df_temp$x <- df_temp[[i]]
    df_temp <- df_temp[!is.na(df_temp$x[, 1]), ]
    df_temp <- df_temp %>% select(-starts_with("x20"))
    table[[i]] <- df_temp
  }
  df_compile <- bind_rows(table)
  
  if (filtre_etat == "Exo") {
    df_compile <- df_compile %>% filter(etat == "Exo")
  } else if (filtre_etat == "Meso_frais") {
    df_compile <- df_compile %>% filter(str_detect(etat, "Meso"))
  } else if (all(filtre_etat != "aucun")) {
    #filtre sur un ou plusieurs états(ex filtre_etat = c("Meso_frais", "Meso_Lyoph"))
    df_compile <- df_compile %>% filter(etat %in% filtre_etat)
  }
  
  return(df_compile)
}


compile_spectres_df <- function(..., colonnes_spectres, filtre_etat = "aucun") {
  
  data <- bind_rows(list(...))
  
  table <- list()
  
  for (i in colonnes_spectres) {
    df_temp <- data
    df_temp$x <- df_temp[[i]]
    df_temp <- df_temp[!is.na(df_temp$x[, 1]), ]
    df_temp <- df_temp %>% select(-starts_with("x20"))
    table[[i]] <- df_temp
  }
  df_compile <- bind_rows(table)
  
  if (filtre_etat == "Exo") {
    df_compile <- df_compile %>% filter(etat == "Exo")
  } else if (filtre_etat == "Meso") {
    df_compile <- df_compile %>% filter(str_detect(etat, "Meso"))
  } else if (all(filtre_etat != "aucun")) {
    df_compile <- df_compile %>% filter(etat %in% filtre_etat)
  }
  
  spectres_df <- as.data.frame(df_compile$x)
  df_compile <- df_compile %>%
    select(-x) %>%
    bind_cols(spectres_df)
  
  return(df_compile)
}