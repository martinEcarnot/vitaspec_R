asd2vitaspec = function (spfile,fm_PCA,fm_all, id_model,param) {

library(nirsextra)
library(rchemo)

# load("Vitaspec_fruit_models")
# param=read.csv("Vitaspec_fruit_param.csv")

sp=asd_read(spfile)
spnew=sp$spectrum/sp$reference

cond="meso_frais"
param=param[grepl(cond,param$Cal_data),]

# PCA
scord=scordis(fm_PCA,spnew)$res$dstand

# Prédiction
pred=NULL
nag=dim(param)[1]
# Bug de pre, ne marche pas si un seul spectre...
spnew=rbind(spnew,spnew)

for (i in 1:nag) {
  expr <- parse(text = param[i, ]$pre)
  p <- eval(expr, envir = parent.frame())
  spnx=pre(spnew,p)
  imodel=which(grepl(cond,id_model[,2]) & grepl(param[i,]$Propriete,id_model[,1]))
  fm=fm_all[[imodel]]
  fm$xscales=rep(1,length(fm$xmeans))
  fm$yscales=rep(1,length(fm$ymeans))
  pred=cbind(pred,predict(fm, spnx)$pred)
}
colnames(pred)=param$Propriete
pred=pred[1,]
pred[pred<0]=0

return(list(distance=scord,biochimie=pred))
}