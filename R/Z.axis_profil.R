#################Z profile

#' Z.axis_profil
#' profile following temporal (z) axis for selected ROI
#'
#' @inheritParams remove_noise
#' @param x [numeric] (**with default**) pixel along x-axis (default x=seq(1,512))
#' @param y  [numeric]  (**with default**) pixel along y-axis (default y=seq(1,512))
#'
#' @return tmp [array]
#'
#' @export
#'
#' @examples
#'  \dontrun{
#' Z<-Z.axis_profil(im,x=seq(50,75),seq(310,320))
#' temp<-seq(25,445,5)
#' plot(temp,Z,type="l",col="blue",xlab="Temperature (°C)",ylab="intensite")
#' }



Z.axis_profil<-
  function(im,x=seq(1,512),y=seq(1,512)){
    tmp<-array(dim=85)
    for (i in 1:85)tmp[i]<-max(im@data[x,y,i])
    tmp
  }
