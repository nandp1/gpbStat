#' Diallel Method 2 data in RCBD
#'
#' The Diallel Method 2 data laid out in Randomized Complete Block Design (RCBD).
#'
#'@docType data
#'
#'@usage data(rcbdltc)
#'
#' @format A data frame for Diallel analysis Method 2 containing four variables of 105 crosses and 15 parents.
#' \describe{
#'  \item{rep}{four replications}
#'  \item{parent1}{five inbred genotype}
#'  \item{parent2}{three inbred genotype}
#'  \item{DTP}{data for days to pollen shed}
#'
#' }
#'@seealso
#'    \code{\link{alphaltcchk}}
#'    ,\code{\link{alphaltc}}
#'    ,\code{\link{rcbdltcchk}}
#'    ,\code{\link{dm2alpha}}
#'
#' @examples  result2 =  dm2(dm2rcbd, rep, parent1, parent2, DTP)

"dm2rcbd"
