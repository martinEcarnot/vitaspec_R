# Chargement des prétraitements

list_pre_test_AG=list(

  
  rbind(list('adj',''),list('red',c(800,750,1)),list('snv',''),list('sder',c(2,3,15))),
  rbind(list('adj',''),list('red',c(950,650,1)),list('snv',''),list('sder',c(2,3,15))),
  rbind(list('adj',''),list('red',c(950,750,1)),list('snv',''),list('sder',c(2,3,15))),
  rbind(list('adj',''),list('red',c(1000,700,1)),list('snv',''),list('sder',c(2,3,15))),
  rbind(list('adj',''),list('red',c(1000,750,1)),list('snv',''),list('sder',c(2,3,15)))
  
  
  
  
  
# rbind(list('adj',''), list('snv',''), list('sder',c(1,3,5))),
# rbind(list('adj',''), list('snv',''), list('sder',c(2,3,15))),



# rbind(list('adj',''), list('ref2abs',''), list('red',c(950,750,1)), list('snv','')),
# rbind(list('adj',''), list('ref2abs',''), list('red',c(950,750,1)), list('snv',''), list('sder',c(0, 2, 21))),
# rbind(list('adj',''), list('ref2abs',''), list('red',c(950,750,1)), list('snv',''), list('sder',c(0, 2, 25))),
# rbind(list('adj',''), list('ref2abs',''), list('red',c(950,750,1)), list('snv',''), list('sder',c(0, 2, 27))),
# rbind(list('adj',''), list('ref2abs',''), list('red',c(950,750,1)), list('snv',''), list('sder',c(0, 2, 31))),
# rbind(list('adj',''), list('ref2abs',''), list('red',c(950,750,1)), list('snv',''), list('sder',c(0, 3, 15))),
# rbind(list('adj',''), list('ref2abs',''), list('red',c(950,750,1)), list('snv',''), list('sder',c(0, 3, 17))),
# rbind(list('adj',''), list('ref2abs',''), list('red',c(950,750,1)), list('snv',''), list('sder',c(0, 3, 19))),
# rbind(list('adj',''), list('ref2abs',''), list('red',c(950,750,1)), list('snv',''), list('sder',c(0, 3, 21))),
# rbind(list('adj',''), list('ref2abs',''), list('red',c(950,750,1)), list('snv',''), list('sder',c(0,3,31))),
# rbind(list('adj',''), list('ref2abs',''), list('red',c(950,750,1)), list('snv',''), list('sder',c(1,3,15))),
# rbind(list('adj',''), list('ref2abs',''), list('red',c(950,750,1)), list('snv',''), list('sder',c(1,3,25))),
# rbind(list('adj',''), list('ref2abs',''), list('red',c(950,750,1)), list('snv',''), list('sder',c(1,3,31))),
# rbind(list('adj',''), list('ref2abs',''), list('red',c(950,750,1)), list('snv',''), list('sder',c(2,3,21))),
# rbind(list('adj',''), list('ref2abs',''), list('red',c(950,750,1)), list('snv',''), list('detr','2')),
# rbind(list('adj',''), list('ref2abs',''), list('red',c(950,750,1)), list('snv',''), list('detr','2'), list('sder',c(0, 3, 21))),
# rbind(list('adj',''), list('ref2abs',''), list('red',c(950,750,1)), list('msc','')),
# 
# rbind(list('adj',''), list('red',c(950,750,1)), list('msc','')),
# rbind(list('adj',''), list('red',c(950,750,1)), list('msc',''), list('sder',c(1,3,15))),
# rbind(list('adj',''), list('red',c(950,750,1)), list('msc',''), list('sder',c(1,3,21))),
# 
# rbind(list('adj',''),list('red',c(950,750,1)),list('snv','')),
# rbind(list('adj',''),list('red',c(950,750,1)),list('snv','')),


# rbind(list('adj',''), list('red',c(950,750,1)), list('snv',''), list('detr','2'), list('sder',c(0, 2, 21))),
# rbind(list('adj',''), list('red',c(950,750,1)), list('snv',''), list('detr','2'), list('sder',c(0, 2, 25))),
# rbind(list('adj',''), list('red',c(950,750,1)), list('snv',''), list('detr','2'), list('sder',c(0,3,21))),
# 
# rbind(list('adj',''), list('red',c(950,750,1)), list('snv',''), list('detr','2'), list('sder',c(1,3,15))),

# rbind(list('adj',''),list('red',c(950,750,1)),list('snv',''),list('sder',c(1,3,15))),

# rbind(list('adj',''),list('red',c(950,20,1)),list('snv',''),list('sder',c(2,3,15))),
# rbind(list('adj',''),list('red',c(950,20,1)),list('snv',''),list('sder',c(1,3,15))),
# 
# rbind(list('adj',''), list('red',c(950,750,1)), list('snv',''), list('sder',c(0,3,15))),
# rbind(list('adj',''), list('red',c(950,750,1)), list('snv',''), list('sder',c(0,3,21))),
# rbind(list('adj',''), list('red',c(950,750,1)), list('snv',''), list('sder',c(0,3,25))),
# rbind(list('adj',''), list('red',c(950,750,1)), list('snv',''), list('sder',c(0,3,31))),
# rbind(list('adj',''), list('red',c(950,750,1)), list('snv',''), list('sder',c(1,3,15))),
# rbind(list('adj',''), list('red',c(950,750,1)), list('snv',''), list('sder',c(1,3,21))),
# rbind(list('adj',''), list('red',c(950,750,1)), list('snv',''), list('sder',c(1,3,31))),
# 
# 
# rbind(list('adj',''), list('red',c(950,750,1)), list('snv',''), list('detr','2')),
# rbind(list('adj',''), list('red',c(950,750,1)), list('snv',''), list('detr','2'), list('sder',c(1,3,9))),
# 
# rbind(list('snv',''),list('sder',c(1,3,9)),list('red',c(10,10,1)))

) 
     
  
 
  
  




















































