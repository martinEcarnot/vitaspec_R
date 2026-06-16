get_best_wold <- function(pls_mod, y_train, n_rows) {
  # press et rss
  press_vals <- (RMSEP(pls_mod, estimate = "CV")$val[1, 1, -1])^2 * n_rows
  rss_vals <- (RMSEP(pls_mod, estimate = "train")$val[1, 1, -1])^2 * n_rows
  
  # rss0 (variance totale)
  rss0 <- sum((y_train - mean(y_train))^2)
  rss_prev <- c(rss0, rss_vals[-length(rss_vals)])
  
  # wold
  wold_ratios <- press_vals / rss_prev
  
  # regle decision
  best_wold <- which(wold_ratios > 0.95)[1] - 1
  
  if(is.na(best_wold)) best_wold <- length(wold_ratios) 
  if(best_wold == 0) best_wold <- 1 
  
  return(best_wold)
}