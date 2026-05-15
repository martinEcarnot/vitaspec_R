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

source(paste0(d0,"list_pre_test_tot.R"))

source(paste0(d0,"new_new_preCV_cluster.R"))
source(paste0(d0,"get_best_wold_cluster.R"))

rpd <- rchemo::rpd
S0 <- paste0(d0,"test_pretraitements/")

fm_all=list()
id_model=list()
#idparam <- "meso_frais"
idparam <- "HR"
set.seed(123)

ag_table <- read.table(paste0(d0,"ag_HR.csv"),
                       sep=";", 
                       header=TRUE, 
                       dec=",")

ag <- ag_table$x

ncomp = 15
compose_etudie <- ag[-c(1:4,6:23)]
print(compose_etudie)

for (k in compose_etudie){
  files <- paste0(S0,k)
  
  if (!dir.exists(files)) {
    dir.create(files, recursive = TRUE)}
}

dat <- read.table(paste0(d0,"dat_HR_25_DIADE_clean.csv"),
                  sep = ",",
                  header = TRUE,
                  dec = ".")

compose_etudie %>%
  walk(function(ag1) {
    print(ag1)
    datok <- dat %>%
      filter(!is.na(.data[[ag1]]))
    
    y_vals <- as.numeric(datok[[ag1]])
    x_mat  <- x_mat <- as.matrix(datok %>% select(starts_with("x", ignore.case = FALSE)))
    
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
        list_pre = list_pre_test_tot,
        ncomp = ncomp,
        seg = 10,
        titl = "HR",
        y_name = ag1,
        rep = 50,
        sortie = S0)
    })
  })