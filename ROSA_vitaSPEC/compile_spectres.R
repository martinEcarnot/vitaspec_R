compile_spectres_matrix <- function(data, colonnes_spectres, filtre_nom = "aucun") {
  
  table <- list()
  
  for (i in colonnes_spectres) {
    
    df_temp <- data
    df_temp$x <- df_temp[[i]]
    df_temp <- df_temp[!is.na(df_temp$x[, 1]), ]
    df_temp <- df_temp %>% select(-starts_with("x20"))
    table[[i]] <- df_temp
  }
  df_compile <- bind_rows(table)
  
  if (filtre_nom == "sans_X") {
    df_compile <- df_compile %>% filter(!str_detect(spname, "X"))
  } else if (filtre_nom == "avec_X") {
    df_compile <- df_compile %>% filter(str_detect(spname, "X"))
  }
  
  return(df_compile)
}



compile_spectres_df <- function(data, colonnes_spectres, filtre_nom = "aucun") {
  
  table <- list()
  
  for (i in colonnes_spectres) {
    
    df_temp <- data
    df_temp$x <- df_temp[[i]]
    df_temp <- df_temp[!is.na(df_temp$x[, 1]), ]
    df_temp <- df_temp %>% select(-starts_with("x20"))
    table[[i]] <- df_temp
  }
  df_compile <- bind_rows(table)
  
  if (filtre_nom == "sans_X") {
    df_compile <- df_compile %>% filter(!str_detect(spname, "X"))
  } else if (filtre_nom == "avec_X") {
    df_compile <- df_compile %>% filter(str_detect(spname, "X"))
  }
  
  spectres_df <- as.data.frame(df_compile$x)
  df_compile <- df_compile %>%
    select(-x) %>%
    bind_cols(spectres_df)
  
  return(df_compile)
}






















