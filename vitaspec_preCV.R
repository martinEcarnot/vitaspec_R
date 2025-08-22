vitaspec_preCV = function(x,y,list_pre,ncomp,titl, plotLV=TRUE, plotYY=TRUE)  {

# From a list of pretraitments, make cross-validation of data set with different pretr. and plot pred vs obs for best LV
  
r2_tt=matrix(nrow=ncomp+1,ncol=length(list_pre))  # matrix(nrow=ncomp+1,ncol=nseq-6)  #
fmtt=list()
pftot=NULL
K=5
# nseq=20
# seqlo=seq(1,2150,nseq)
# nseq=length(seqlo)
r2_tt=matrix(nrow=ncomp,ncol=length(list_pre))  # matrix(nrow=ncomp+1,ncol=nseq-6)  #
fmtt=list()
fmttn=list()
for (j in 1:length(list_pre)) {  # 1:
  print(paste0("Prétraitement ",j, "/",length(list_pre)))
  # p=rbind(list('adj',''),list('red',c(seqlo[j],2151-seqlo[j+6],1)),list('sder',c(1,3,15)))
  # print(seqlo[j])
  # print(2151-seqlo[j+4])
  # dat$xp=pre(dat$x,p)
  dat$xp=pre(dat$x,list_pre[[j]])
  iout=which(is.na(dat[,colnames(dat)==ag1]))
  if (length(iout)>0) {datok=dat[-iout,]} else datok=dat
  
  # generate sgm list for Leave-one-out
  segm <- list(rep1 = as.list(1:nrow(datok)))  # segm <- segmkf(n = nrow(datok), K = 5)
  # fm = cvfit(datok$xp, datok[,colnames(datok)==ag1],fun=plsr,segm=segm, ncomp=ncomp)
  fmc = gcvlv(datok$xp, datok[,colnames(datok)==ag1],segm,score = r2, fun = plskern, nlv = 1:ncomp, verb = F)  # !!! pas cor2 avec LOO 
  
  fmtt=append(fmtt, list(fmc)) # fmttn=append(fmttn, list(fm))
  r2_tt[,j] = mser(fmc)$cor2  # r2_tt[,j] = mse(fm, ~ ncomp)$cor2
  pf=t(list_pre[[j]])  # # list(seqlo[j], 2151-seqlo[j+4])
  dim(pf)=c(1,2*ncol(pf))
  pf=paste0(pf, collapse = "_")
  pftot=c(pftot,pf)
}
best_pre_lo=which(r2_tt == max(r2_tt[2:14,]), arr.ind = TRUE)[1,]
if (plotLV) {
  matplot(r2_tt, type = 'l', lty = 1, col = 1:ncol(r2_tt), ylab="R2_Validation_Croisée", xlab="Nombre de Variables Latentes")
  title(ag1)
  # pftot=c("Prétraitement A","Prétraitement B","Prétraitement C") # Pour rapport Alternance Amel
  legend("bottomright", legend = pftot, col = 1:ncol(r2_tt), lty = 1, cex = 0.8)  #cex = 0.6
}
fm=fmtt[[best_pre_lo[2]]]  # fmn=fmttn[[best_pre_lo[2]]]
print(pftot[best_pre_lo[2]], 2)
print("R2")
print(round(mser(fm)$cor2,2))  # print(round(mse(fmn, ~ ncomp)$cor2[-1], 2))
print("SEP")
print(round(mser(fm)$sep,2))    # print(round(mse(fmn, ~ ncomp)$sep[-1],2))
cat("\n")
cat("\n")

if (plotYY) {
  fm1=fm$y[fm$y$nlv==best_pre_lo[1],]
  plot(fm1$yp,fm1$yref, xlab="Teneur Prédite par SPIR", ylab="Teneur Mesurée")
  abline(a = 0, b = 1, col = "red", lty = 2)  # a=0, b=1 pour y=x; couleur rouge et ligne en pointillés
  fit <- lm(fm1$yp ~ fm1$yref)
  abline(fit, col = "blue")
  summary_fit <- summary(fit)
  r_squared <- summary_fit$r.squared
  legend("topleft", legend = c("y = x", bquote(Validation_Croisée: ~ R^2 == .(round(r_squared, 2))),bquote(pre :  .(pftot[best_pre_lo[2]])),bquote(ncomp : .(best_pre_lo[[1]])),bquote(n_ech : .(length(fm1$yref)))), col = c("red", "blue", "white", "white", "white"),lty = c(2, 1),bty = "n")
  title(ag1)
}

}