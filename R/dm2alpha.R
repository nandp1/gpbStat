#' Diallel Method 2 data in Alpha Lattice.
#'
#' The Diallel Method 2 data laid out in Alpha Lattice Design.
#'
#'@docType data
#'
#'@usage data(dm2alpha)
#'
#' @format A data frame for Diallel analysis Method 2 containing four variables of 105 crosses and 15 parents.
#' \describe{
#'  \item{rep}{four replications}
#'  \item{blk}{twelve blocks}
#'  \item{parent1}{five inbred genotype}
#'  \item{parent2}{three inbred genotype}
#'  \item{TW}{data for test weight}
#'
#' }
#'@seealso
#'    \code{\link{alphaltcchk}}
#'    ,\code{\link{alphaltc}}
#'    ,\code{\link{rcbdltcchk}}
#'    ,\code{\link{dm2rcbd}}
#'
#' @examples  result2 =  dm2(dm2alpha, rep, parent1, parent2, TW, blk)

"dm2alpha"
