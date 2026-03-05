#profil selon l'axe temporel (z) pour la ROI rectangulaire sélectionnée




Z.axis_profil<-
  function(im,x=seq(1,512),y=seq(1,512)){
    tmp<-array(dim=85)
    for (i in 1:85)tmp[i]<-max(im@data[x,y,i])
    tmp
  }
