vitaspec_load_Raman = function(d)  {
  # A partir d'un repertoire contenant tous les repertoires d'une serie de mesure, lit tous les spectres

  library(nirsextra)
  ram=matrix(,ncol=512)
  ram0=matrix(,ncol=512)
  nam=NULL
  # Lister les sous-dossiers de niveau 1 uniquement
  d1 <- list.dirs(path = d, full.names = TRUE, recursive = FALSE)
  
  # Boucle sur chaque dossier de niveau 1
  for (d2 in d1) {
    # Chercher les fichiers .txt dans le dossier
    fichiers_txt <- list.files(path = d2, pattern = "\\.txt$", full.names = TRUE)
    
    # Vérifie s'il y a des fichiers .txt
    if (length(fichiers_txt) == 1 & !grepl("Admin_PASS_Performance",basename(d2))) {
      # cat("Lecture du dossier :", basename(d2), "\n")
      sp1=read.table(fichiers_txt,skip = 95, header=TRUE, sep="\t",row.names = NULL)[,-6]
      colnames(sp1)=c("index","Raman_shift","ram0","bckgnd","rawval")
      sp1$ram=sp1$rawval-sp1$bckgnd
      ram=rbind(ram,sp1$ram)
      ram0=rbind(ram0,sp1$ram0)
      nam=c(nam,basename(d2))
    } else {
      # cat(d2,"Nombre de fichiers .txt = ",length(fichiers_txt))
    }
  }
  ram=ram[-1,]
  ram0=ram0[-1,]
  colnames(ram)=sp1$Raman_shift
  rownames(ram)=nam
  colnames(ram0)=sp1$Raman_shift
  rownames(ram0)=nam
  rm(nam)
  sp=sp2df(ram)
  sp$ram0=ram0
  colnames(sp)[1]="ram"
  return(sp)
}
  