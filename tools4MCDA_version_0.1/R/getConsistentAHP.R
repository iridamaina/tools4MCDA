#' Analytic Hierarchy Process (AHP)
#'
#' Compute the weights of analytic hierarchy process by adjusting the pair-wise
#' comparison matrix till consistency ratio criterion is achieved (C.R. <0.1).
#'
#' @importFrom FuzzyAHP pairwiseComparisonMatrix consistencyRatio calculateWeights
#'
#' @param pairwise_matrix The nxn pair-wise comparison matrix.
#' @param lambda A weighting factor in (0,1). Default value is 0.5. The higher
#' the lambda value the higher the adjustment to the pair-wise comparison
#' matrix.
#' @return \item{weights}{ Weights to measure the importance of each component (1,2,...,n).
#' } \item{cr}{ The consistency ratio. }
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
#' \dontrun{
#' library(FuzzyAHP)
#' library(tools4MCDA)
#' matrix_values <- c(1,3,4,5,7,
#'                   1/3,1,2,2,4,
#'                   1/4,1/2,1,2,3,
#'                   1/5,1/2,1/2,1,3,
#'                   1/7,1/4,1/3,1/3,1)
#' matrix_values = matrix(matrix_values,nrow = 5,ncol = 5, byrow = TRUE)
#' ahp_outputs = getConsistentAHP(matrix_values)
#' ahp_outputs
#' }
#' @export
getConsistentAHP <- function(pairwise_matrix, lambda = 0.5) {
  RI <- c(0, 0, 0.52, 0.89, 1.12, 1.26, 1.36, 1.41, 1.46, 1.49, 1.52, 1.54, 1.56, 1.58,
          1.59)
  PairwiseMatInit = pairwise_matrix
    if (nrow(pairwise_matrix) != ncol(pairwise_matrix))
    stop("Pairwise matrix must be square")
  
  # Calculate largest eigevalue and principal eigenvector
  e <- eigen(pairwise_matrix)
  lmax = max(Re(e$values[abs(Im(e$values)) < 1e-06]))
  w <- Re(e$vectors[, 1])/sum(Re(e$vectors[, 1]))
  
  # Calculate consistency ratio of PairwiseMatInit
  PairwiseMatInit = pairwiseComparisonMatrix(PairwiseMatInit)
  CR = consistencyRatio(PairwiseMatInit, print.report = FALSE)
  PairwiseMatInit = PairwiseMatInit@values
  k = 0
  
  # while loop till the consistency ratio (<0.1) is achieved
  while (CR > 0.1) {
    wmat <- diag(1, length(w))
    for (i in 1:length(w)) {
      for (j in 1:length(w)) {
        wmat[i, j] = w[j]/w[i]
      }
    }
    PairwiseMat_wmat = pairwise_matrix * wmat
    id = which(PairwiseMat_wmat == max(PairwiseMat_wmat), arr.ind = TRUE)
    pairwise_matrix[id[1], id[2]] <- lambda * pairwise_matrix[id[1], id[2]] + (1 - lambda) *
      (w[id[1]]/w[id[2]])
    pairwise_matrix[id[2], id[1]] <- 1/pairwise_matrix[id[1], id[2]]
    e <- eigen(pairwise_matrix)
    lmax = max(Re(e$values[abs(Im(e$values)) < 1e-06]))
    w <- Re(e$vectors[, 1])/sum(Re(e$vectors[, 1]))
    CI = (lmax - nrow(pairwise_matrix))/(nrow(pairwise_matrix) - 1)
    CR = CI/RI[nrow(pairwise_matrix)]
    k = k + 1
    if (k > 200) stop("Iterations > 200, adjust pairwise_matrix")
    
  }
  
  # Calculate weights, consistency ratio and diagnostics of AHP
  delta = max(abs(pairwise_matrix - PairwiseMatInit))
  sig = sqrt(sum((pairwise_matrix - PairwiseMatInit)^2))/nrow(pairwise_matrix)
  pairwise_matrix = pairwiseComparisonMatrix(pairwise_matrix)
  weights = calculateWeights(pairwise_matrix)
  CR = consistencyRatio(pairwise_matrix, print.report = FALSE)
  
  if (delta > 2)
    warning("delta is > 2")
  if (sig > 1.5)
    warning("sig is > 1.5")
  
  return(list("weights" = weights@weights, "cr" = CR, "matrix_adj" = pairwise_matrix@values,
              delta = delta, sig = sig))
}
