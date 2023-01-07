#' Diallel Method 2 data in Alpha Lattice.
#'
#' The Diallel Method 2 data laid out in Alpha Lattice Design.
#'
#'@docType data
#'
#'@usage data(dm2alpha)
#'
#' @format A data frame for Diallel analysis Method 2 containing 105 crosses and 15 parents.
#' \describe{
#'  \item{replication}{two replications}
#'  \item{block}{twelve blocks}
#'  \item{parent1}{fifteen inbred genotype}
#'  \item{parent2}{fifteen inbred genotype}
#'  \item{TW}{data for test weight}
#'
#' }
#'@seealso
#'    \code{\link{alphaltcchk}}
#'    ,\code{\link{alphaltc}}
#'    ,\code{\link{rcbdltcchk}}
#'    ,\code{\link{dm2rcbd}}
#'
#' @examples  result2 =  dm2(dm2alpha, replication, parent1, parent2, TW, block)

"dm2alpha"
