#' Line x Tester data (only Crosses) in Alpha Lattice design.
#'
#' The Line x Tester data of containing only crosses laid out in Alpha Lattice design.
#'
#'@docType data
#'
#'@usage data(alphaltcmt)
#' @format A data frame of 15 crosses derived from five lines and three testers.
#' \describe{
#'  \item{replication}{four replications}
#'  \item{block}{five blocks}
#'  \item{line}{five inbred genotype}
#'  \item{tester}{three inbred genotype}
#'  \item{hsw}{hundred seed weight}
#'  \item{sh}{shelling per cent}
#'  \item{gy}{grain yield}
#' }
#'
#'@seealso
#'    \code{\link{rcbdltc}}
#'    ,\code{\link{alphaltcchk}}
#'    ,\code{\link{rcbdltcchk}}
#'    ,\code{\link{rcbdltcmt}}
#'
#' @examples  result = ltcmt(alphaltcmt, replication, line, tester, alphaltcmt[,5:7], block)

"alphaltcmt"
