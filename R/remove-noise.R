#####################Function remove-noise and dependcies##################################################

#' remove-noise
#' remove noise by deleting outliers
#'
#' @param im [array] (**optional**) an image as matrix or array
#' @param radius [numeric] (**with default**) radius value (default radius=5)
#' @param threshold  [numeric]  (**with default**)  threshold value (default threshold=50)
#' @param bg  [numeric]  (**with default**) background values (default bg=0)
#'
#' @return filtered_image [array]
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

remove_noise<-function(im,radius=5,threshold=50,bg=0){
  local_med <- local_median(im, radius,bg)
  outlier_mask <- abs(im - local_med) > threshold

  # Remove outliers by replacing them with local mean or other methods
  filtered_image <- im
  filtered_image[outlier_mask] <- local_med[outlier_mask]
  filtered_image
}

###############
#' local_median
#' calcul of median with size radius x radius
#'
#' @inheritParams remove-noise
#'
#' @return med_mat
#'
#' @importFrom Rcpp sourceCpp
#'
#' @export
#'
#' @examples
#'

local_median <- function(im, radius,bg=0) {
  med_mat <- array(bg, dim=dim(image))
  Rcpp::sourceCpp("median.cpp")
  for(k in 1:dim(image)[3]){
    for (i in 1:(nrow(image)-radius)) {
      for (j in 1:(ncol(image)-radius)) {
        med_mat[i, j,k] <- cpp_med(image[seq(i,i + radius-1),seq(j,j+radius-1),k])

      }
    }
  }

  return(med_mat)
}

