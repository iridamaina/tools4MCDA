#### Apply Analytic Hierarchy Process (AHP) ####


#' Analytic Hierarchy Process (AHP)
#'
#' Compute the consistency ratio of the initial pair-wise comparison matrix
#' 
#' @importFrom FuzzyAHP pairwiseComparisonMatrix consistencyRatio
#'
#' @param pairwise_matrix The nxn pair-wise comparison matrix.
#' @return \item{cr}{The consistency ratio.}
#'
#' \item{pairwise_matrix_adj}{ The adjusted pairwise matrix with acceptable
#' consistency (C.R. < 0.1).  }
#'
#' \item{delta}{ Measure the maximum absolute difference of elements between
#' the pairwise matrix and adjusted pairwise matrix. It should be <2.  }
#'
#' \item{sig}{ A metric that measures the standard deviation of the difference
#' of elements between the pairwise matrix and adjusted pairwise matrix. It
#' should be ~1.  }
#' @references Kavadas, S., I. Maina, D. Damalas, I. Dokos, M. Pantazi, and V.
#' Vassilopoulou (2015). Multi-Criteria Decision Analysis as a tool to extract
#' fishing footprints and estimate fishing pressure: application to small scale
#' coastal fisheries and implications for management in the context of the
#' Maritime Spatial Planning Directive. Mediterranean Marine Science
#' 16:294-304. \url{http://dx.doi.org/10.12681/mms.1087}
#'
#' Saaty, T.L. (2001). Decision Making for Leaders: The Analytic Hierarchy
#' Process for Decisions in a Complex World, New Edition 2001 (3 Revised).
#' Pittsburgh, PA: RWS Publications, ISBN 978-0962031786.
#'
#' Xu, Z. (2004). A practical method for improving consistency of judgment
#' matrix in the ahp. Journal of Systems Science and Complexity, 17(2).
#' @author D. Politikos
#' @examples
#' library(FuzzyAHP)
#' library(tools4MCDA)
#' matrix_values <- c(1,3,4,5,7,
#'                   1/3,1,2,2,4,
#'                   1/4,1/2,1,2,3,
#'                   1/5,1/2,1/2,1,3,
#'                   1/7,1/4,1/3,1/3,1)
#' matrix_values = matrix(matrix_values,nrow = 5,ncol = 5, byrow = TRUE)
#' ahp_outputs = getCR(matrix_values)
#' ahp_outputs
#' @export
getCR<-function(pairwise_matrix){
RI <- c(0, 0, 0.5251686, 0.8836651, 1.1081014, 1.2492774, 
        1.3415514, 1.4048466, 1.4507197, 1.4857266, 1.5141022, 
        1.5356638, 1.5545925, 1.5703498, 1.5839958, 1.5955704, 
        1.6053208, 1.6140648, 1.62189, 1.6288505, 1.6355145, 
        1.6410749, 1.6462439, 1.6509834, 1.6554325, 1.6592237, 
        1.663105, 1.6662368, 1.6696396, 1.6723214, 1.6751007, 
        1.6778474, 1.6801459, 1.6824754, 1.6844494, 1.6865981, 
        1.6884438, 1.6901943, 1.6918461, 1.6935071, 1.6950605, 
        1.6965334, 1.6979425, 1.6992006, 1.7004654, 1.7016392, 
        1.702778, 1.7038778, 1.7050314, 1.7060381, rep(1.7060381, 
                                                       1000))
PairwiseMatInit = pairwise_matrix
if (nrow(pairwise_matrix) != ncol(pairwise_matrix)) 
  stop("Pairwise matrix must be square")
e <- eigen(pairwise_matrix)
lmax = max(Re(e$values[abs(Im(e$values)) < 1e-06]))
w <- Re(e$vectors[, 1])/sum(Re(e$vectors[, 1]))
PairwiseMatInit = pairwiseComparisonMatrix(PairwiseMatInit)
CR = consistencyRatio(PairwiseMatInit, print.report = FALSE)
return(CR)
}

