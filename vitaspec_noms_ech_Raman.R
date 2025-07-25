vitaspec_noms_ech_Raman = function(noms, noms_complets)  {
  # convertit les noms des ech Raman en nom complet
  
  uech=unique(noms_complets)
  num_complet=sub("-.*", "", uech)
  for (i in 1:length(noms)) {
    num=sub(".*hr([^-]+)-.*", "\\1", noms[i])
    id=which(num == num_complet)
    if (length(id) == 1) {noms[i]=uech[id]}
  }
  return(noms)
}
  
  