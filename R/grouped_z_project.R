############## merged images

#' grouped_z_project
#' merged images
#'
#' @inheritParams remove_noise
#' @param grouped_size [numeric] (**with default**) number of image merged in a group
#' @param FUN  [function]  (**with default**)  function between the merged images (default FUN=sum)
#'
#' @return im [array] limited to n slices with n=number of slice in the initial image divided by the grouped_size
#'
#' @export
#'
#' @examples
#'  \dontrun{
#' tmp<-grouped_z_project(im,85)
#' }

grouped_z_project<-
  function(im,grouped_size,FUN=sum){
    nb_frames<-dim(im)[3]
    stopifnot(nb_frames%%grouped_size==0)
    tmp<-im
    n<-nb_frames/grouped_size
    for(i in 1:n)
      tmp[,,i]<-apply(im[,,seq((i-1)*grouped_size+1,i*grouped_size)],c(1,2),FUN)
    return(im=tmp[,,seq(1,n)])
  }
