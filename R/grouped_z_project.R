############## merged images

#' grouped_z_project
#' remove noise by deleting outliers
#'
#' @inheritParams remove-noise
#' @param grouped_size [numeric] (**with default**) number of image merged in a group
#' @param op  [string]  (**with default**)  operation between the merged images (default op=sum)
#'
#' @return tmp, n [list]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ls<-list.files("images/",pattern = "tif$",full.names = TRUE)
#' im<-terra::rast(ls)
#' im%>%
#'   as.array%>%
#'     remove_noise(radius=5,bg=505)%>%
#'       rast
#' }

grouped_z_project<-
  function(im,grouped_size,op=sum){
    nb_frames<-dim(im@data)[3]
    stopifnot(nb_frames%%grouped_size==0)
    tmp<-im
    n<-nb_frames/grouped_size
    for(i in 1:n)
      tmp@data[,,i]<-apply(im@data[,,seq((i-1)*grouped_size+1,i*grouped_size)],c(1,2),op)
    return(list(tmp=tmp,n=n))
  }
