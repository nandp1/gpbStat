#' Line x Tester data (only Crosses) with single plant observations laid in Alpha Lattice design.
#'
#' The Line x Tester data containing single plant observations of only crosses laid out in Alpha Lattice design.
#'
#'@docType data
#'
#'@usage data(alphaltcs)
#' @format A data frame of 15 crosses derived from five lines and three testers.
#' \describe{
#'  \item{replication}{four replications}
#'  \item{block}{five blocks}
#'  \item{line}{five inbred genotype}
#'  \item{tester}{three inbred genotype}
#'  \item{obs}{four single plant observations}
#'  \item{yield}{yield as a dependent trait}
#' }
#'
#'@seealso
#'    \code{\link{rcbdltcs}}
#'    ,\code{\link{alphaltcchk}}
#'    ,\code{\link{rcbdltcchk}}
#'    ,\code{\link{rcbdltcmt}}
#'
#' @examples  result = ltcs(alphaltcs, replication, line, tester, obs, yield, block)

"alphaltcs"
