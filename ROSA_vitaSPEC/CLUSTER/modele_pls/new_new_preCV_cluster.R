
new_new_preCV = function(data, x, y, list_pre, ncomp, seg, titl, y_name, rep, sortie){
  
  # grille de combinaisons
  grille_parametres <- expand_grid(
    numero_rep = 1:rep,
    idx_pre = 1:length(list_pre)
  )
  
  nom_pdf <- paste0(sortie,"/",y_name,"/", "graphiques_", y_name, ".pdf")
  pdf(file = nom_pdf, width = 8, height = 6)
  
  comparaison <- pmap_dfr(grille_parametres, function(numero_rep, idx_pre) {
    
    # tracker
    cat("Molécule:", y_name, " Pretraitement:", idx_pre, "/", length(list_pre), " Repetition:", numero_rep, "/", rep, "\n")
    flush.console() 
    
    # pre
    xp = pre(x, list_pre[[idx_pre]])
    df_temp <- data.frame(y = y)
    df_temp$xp <- xp
    
    # process de cv entier
    n <- nrow(data)
    segs <- cvsegments(n, seg)
    cvpred <- rep(0, n)
    
    for (k in 1:seg){
      cvmod = plsr(y~xp, data = df_temp[-segs[[k]],], ncomp = ncomp, scale = FALSE, validation = "CV")
      
      bestncomp = selectNcomp(cvmod, method = "onesigma", plot = FALSE)
      if(bestncomp == 0) bestncomp <- 1 
      cvpred[segs[[k]]] = predict(cvmod, newdata = df_temp[segs[[k]],])[,,bestncomp]
    }
    
    PRESS.pls <- sum((y - cvpred)^2) 
    RMSEcv_glo <- sqrt(PRESS.pls/n)
    
    # recup nom du pretraitement
    mat <- list_pre[[idx_pre]]
    parts <- sapply(1:nrow(mat), function(idx) { 
      nom <- mat[idx, 1][[1]]
      val <- mat[idx, 2][[1]]
      if (length(val) > 1) {
        val_str <- paste0("c(", paste(val, collapse = ","), ")")
        return(paste0(nom, "_", val_str))
      } else if (val != "") {
        return(paste0(nom, "_", val))
      } else {
        return(nom)
      }
    })
    nom_pre <- paste0(paste(parts, collapse = "_"), "_")
    
    # mod global
    mod_global <- plsr(y ~ xp, data = df_temp, ncomp = ncomp, scale = FALSE, validation = "CV", segments = segs)
    
    # metriques
    res_rmsecv <- RMSEP(mod_global, estimate = "CV")$val[1, 1,] 
    
    res_r2 <- R2(mod_global, estimate = "train")$val[1, 1,]
    res_r2_cv <- R2(mod_global, estimate = "CV")$val[1, 1,]
    
    best_wold_global <- get_best_wold(pls_mod = mod_global, y_train = df_temp$y, n_rows = n)
    
    x_comps <- 0:(length(res_r2)- 1)
     
    
    # graph
    par(mar = c(5, 4, 4, 4) + 0.3)
    
    bestncomp_global <- selectNcomp(mod_global, method = "onesigma", plot = TRUE, 
                                    main = paste("Prétraitement :", nom_pre))
    
    # tableau de résultats
    df_res <- data.frame(matrix(ncol = 0, nrow = 1))
    for(m in 1:length(res_rmsecv)){ 
      lv_num <- m - 1
      df_res[1, paste0("LV", lv_num, "_RMSEcv")] <- res_rmsecv[m]
      df_res[1, paste0("LV", lv_num, "_R2")] <- res_r2[m]
      df_res[1, paste0("LV", lv_num, "_R2cv")] <- res_r2_cv[m]
    }
    
    df_res <- cbind(Repetition = numero_rep, Pretraitement = nom_pre, RMSEcv_glo = RMSEcv_glo, onesigma_Ncomp = bestncomp_global, Wold_Ncomp = best_wold_global, df_res)
    
    par(new = TRUE)
    
    # R2
    plot(x_comps, res_r2, type = "b", col = "#90C987", pch = 17, lty = 2, lwd = 2,
         axes = FALSE, xlab = "", ylab = "", ylim = c(0, 1)) 
    
    # R2cv
    lines(x_comps, res_r2_cv, type = "b", col = "#4eb265", pch = 16, lty = 1, lwd = 2)
    
    # axe
    axis(4, col = "black", col.axis = "black")
    mtext("R2", side = 4, line = 2.5, col = "black", cex = 0.9)
    
    mtext(paste("Repetition :",numero_rep,"/", rep,"| RMSEcv global =", round(RMSEcv_glo, 3)), side = 3, line = 0.5, cex = 0.85)
    
    # legende
    legend("top", bty = "n", cex = 0.8,
           legend = c("RMSEcv", "R2", "R2_cv"), 
           col = c("black", "#90C987", "#4eb265"), 
           pch = c(1, 17, 16), 
           lty = c(1, 2, 1),
           lwd = c(1, 2, 2))
    
    return(df_res)
  })
  
  # arret pdf
  dev.off()
  
  # save
  tab <- paste0("Resultats_", y_name, ".xlsx")
  write_xlsx(comparaison, paste0(sortie,"/",y_name,"/",tab))
}