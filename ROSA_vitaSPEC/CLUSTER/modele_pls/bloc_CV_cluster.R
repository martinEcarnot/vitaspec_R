library(ggplot2)
library(dplyr)
library(writexl)
library(openxlsx)
library(nirsextra)
library(tidyverse)
library(rchemo)
library(stringr)
library(purrr)
library(ggpmisc)
library(pls)
library(tidymodels)
library(future)
library(furrr)
library(future.callr)
library(rchemo)


d0 <- "/storage/replicated/cirad_users/ecarnotm/data/vitaspec_R/ROSA_vitaSPEC/CLUSTER/modele_pls/"

source(paste0(d0,"list_pre_test_carot_cluster.R"))
source(paste0(d0,"list_pre_test_AG_cluster.R"))

source(paste0(d0,"new_new_preCV_cluster.R"))
source(paste0(d0,"get_best_wold_cluster.R"))

rpd <- rchemo::rpd
S0 <- paste0(d0,"test_pretraitements/")

fm_all=list()
id_model=list()
#idparam <- "meso_frais"
idparam <- "meso_silica"
set.seed(123)

ag <- read.table(paste0(d0,"ag.csv"),
                 sep=";", 
                 header=TRUE, 
                 dec=",")

ncomp = 15
compose_etudie <- colnames(ag)[-c(3:20)]
print(compose_etudie)

for (k in compose_etudie){
  files <- paste0(S0,k)
  
  if (!dir.exists(files)) {
    dir.create(files, recursive = TRUE)}
}

dat <- read.table(paste0(d0,"dat_sec_DIADE_matrix.csv"),
                  sep = ",",
                  header = TRUE,
                  dec = ".")

compose_etudie %>%
  walk(function(ag1) {
    print(ag1)
    datok <- dat %>%
      filter(!is.na(.data[[ag1]]))
    
    y_vals <- as.numeric(datok[[ag1]])
    x_mat  <- as.matrix(datok$x)
    
    # try({
    #   vitaspec_preCV(
    #     x = x_mat,
    #     y = y_vals,
    #     list_pre = list_pre_test_AG,
    #     ncomp = ncomp, 
    #     titl = "Meso_sec",
    #     y_name = ag1,
    #   )
    # })
    try({
      new_new_preCV(
        data = datok,
        x = x_mat,
        y = y_vals,
        list_pre = list_pre_test_AG,
        ncomp = ncomp,
        seg = 10,
        titl = "Meso_sec",
        y_name = ag1,
        rep = 3,
        sortie = S0)
    })
  })