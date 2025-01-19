#' Grading of a dataset
#'
#' Grades a dataset based on scores.
#'
#' @importFrom stats stepfun
#'
#' @param object The data object to apply the criteria. Data can be vectors,
#' matrices, dataframes, shapefiles and rasters.
#' @param cuts Cuts of the data, on which we define the scores.
#' @param scores Integer scores based on cuts (values MUST be between 0 and 5).
#' @return Return of data object with values from 0 to 5.
#' @author D. Politikos
#' @examples
#'
#'
#'   #------------ Example 1 -----------#
#'   library(tools4MCDA)
#'   object <- runif(100, -1000, 1000)
#'   # Assign cuts
#'   cuts <- c(-Inf, 0, 50, 100, 150, 200, 500)
#'   # Assign scores
#'   scores <- c(NA, 1, 2, 3, 4, 5, 0)
#'   object_scored = fgrade(object, cuts, scores)
#'   # Plot of object and scored object
#'
#'   par(mfrow=c(2,1))
#'   hist(object)
#'   barplot(table(object_scored), main = "Scored object")
#'
#'   #------------ Example 2 -----------#
#'   library(tools4MCDA)
#'   library(raster)
#'   # Generate a raster r
#'   r <- raster(ncol=5, nrow=5)
#'   raster::values(r) <- rnorm(25, 10, 10)
#'   # Assign cuts and scores
#'   cuts <- c(-Inf, 0, 5, 10, 15, 20)
#'   scores <- c(1, 2, 3, 4, 5, 0)
#'   # Create the scored raster: r_scored
#'   r_scored = r
#'   # Update r_scored based on cuts and scores
#'   raster::values(r_scored) = fgrade(raster::values(r_scored), cuts, scores)
#'   # Make the plots of r and r_scored rasters
#'
#'   par(mfrow=c(2,1))
#'   plot(r, main = "Unscored")
#'   plot(r_scored, main = "Scored")
#'
#' @export
fgrade <- function(object, cuts, scores) {
  fstep <- stepfun(cuts[-1], scores)
  f = fstep(object)
  if (max(scores, na.rm = T) > 5)
    stop("scores must be <5")
  if (length(which(scores < 0)) > 0)
    stop("scores must be positive")
  return(f)
}
