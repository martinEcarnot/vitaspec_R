vitaspec_preCV = function(x,y,list_pre,ncomp)  {
  
  r2_tt=matrix(nrow=ncomp+1,ncol=length(list_pre))  # matrix(nrow=ncomp+1,ncol=nseq-6)  #
  fmtt=list()
  
  for (j in 1:length(list_pre)) {
  xp=pre(x,list_pre[[j]])
  # iout=which(dat$x[,1650] < 0.4 | is.na(dat[,colnames(dat)==ag1])) 
  # datok=dat[-iout,]
  K=5
  
  # generate sgm list for Leave-one-out
  segm <- list(rep1 = as.list(1:nrow(x)))
  # segm <- segmkf(n = nrow(x), K = nrow(x))
  fm = cvfit(xp, y,fun=plsr,segm=segm, ncomp=ncomp)
  fmtt=append(fmtt, list(fm))  #
  r2_tt[,j] = mse(fm, ~ ncomp)$cor2
  pf=t(list_pre[[j]])
  dim(pf)=c(1,2*ncol(pf))
  pf=paste0(pf, collapse = "_")
  pftot=c(pftot,pf)
}
best_pre_lo=which(r2_tt[-1,] == max(r2_tt[-c(1,2),]), arr.ind = TRUE)[1,]
matplot(r2_tt, type = 'l', lty = 1, col = 1:ncol(r2_tt), ylab="R2_Validation_Croisée", xlab="Nombre de Variable Latentes")  # ,ylim= c(0,100)
title(ag1)
# legend("topright", legend = paste("Colonne", 1:ncol(r2_tt)), col = 1:ncol(r2_tt), lty = 1, cex = 0.8)
# legend("bottomleft", legend = pftot, col = 1:ncol(r2_tt), lty = 1, cex = 0.6)
fm=fmtt[[best_pre_lo[2]]]
print(pftot[best_pre_lo[2]])
print("R2")
print(mse(fm, ~ ncomp)$cor2)
print("SEP")
print(mse(fm, ~ ncomp)$sep)
fm1=lapply(fm[1:3],function (x) {x[x$ncomp==best_pre_lo[1],]})
plot(fm1$fit$y1,fm1$y$y1, xlab="CValidation", ylab="Reference")
abline(a = 0, b = 1, col = "red", lty = 2)  # a=0, b=1 pour y=x; couleur rouge et ligne en pointillés
fit <- lm(fm1$y$y1 ~ fm1$fit$y1)
abline(fit, col = "blue") 
summary_fit <- summary(fit)
r_squared <- summary_fit$r.squared
legend("topleft", legend = c("y = x", bquote(Validation_Croisée: ~ R^2 == .(round(r_squared, 2))),bquote(pre :  .(pftot[best_pre_lo[2]])),bquote(ncomp : .(best_pre_lo[[1]]))), col = c("red", "blue", "white", "white"),lty = c(2, 1),bty = "n")
# legend("topleft",      legend = c("y = x", bquote(Validation_Croisée: ~ R^2 == .(round(r_squared, 2)))), col = c("red", "blue"),lty = c(2, 1),bty = "n")
title(ag1)
}