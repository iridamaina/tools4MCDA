#' Normalization of a data object
#'
#' It normalizes a data object x to [0,1] using the formula
#' (x-xmin)/(xmax-xmin).
#' @importFrom raster values
#'
#' @param x The data object can be a vector, matrix, dataframe, Shapefile, or
#' RasterLayer object.
#' @param class If missing, the class is numeric, otherwise must be defined.
#' Other options are Shapefile and RasterLayer objects.
#' @return A data object with values in [0,1].
#' @author D. Politikos
#' @examples
#'
#' library(raster)
#' library(tools4MCDA)
#' #-- Example 1 --#
#' x <- rnorm(100)
#' fuzzyMember(x)
#'
#' #-- Example 2 --#
#' v1 <- rnorm(10)
#' v2 <- rpois(10,lambda=2)
#' v3 <- rexp(10)
#' x <- data.frame(v1,v2,v3)
#' fuzzyMember(x$v1)
#' apply(x, 2, fuzzyMember)
#'
#' #-- Example 3 --#
#' r <- raster(ncol=5, nrow=5)
#' r <- init(r, rnorm)
#' rFM <- fuzzyMember(r, "raster")
#' par(mfrow=c(2,1))
#' plot(r)
#' plot(rFM)
#' @export
fuzzyMember <- function(x, class = F) {
  if (class == "raster") {
    raster::values(x) <- (raster::values(x) - min(raster::values(x), na.rm = T))/(max(raster::values(x), na.rm = T) -
                                                                                    min(raster::values(x), na.rm = T))
  } else {
    x <- (x - min(x, na.rm = T))/(max(x, na.rm = T) - min(x, na.rm = T))
  }
  return(x)
}
