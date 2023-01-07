#' Line x Tester data (only Crosses) in Alpha Lattice design.
#'
#' The Line x Tester data of containing only crosses laid out in Alpha Lattice design.
#'
#'@docType data
#'
#'@usage data(alphaltc)
#' @format A data frame of five variables of 15 crosses derived from five lines and three testers.
#' \describe{
#'  \item{replication}{four replications}
#'  \item{block}{five blocks}
#'  \item{line}{five inbred genotype}
#'  \item{tester}{three inbred genotype}
#'  \item{yield}{trait of intrest}
#' }
#'
#'@seealso
#'    \code{\link{rcbdltc}}
#'    ,\code{\link{alphaltcchk}}
#'    ,\code{\link{rcbdltcchk}}
#'
#'
#' @examples  result = ltc(alphaltc, replication, line, tester, yield, block)

"alphaltc"
