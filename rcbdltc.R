#' Line x Tester data in RCBD
#'
#' The sample Line x Tester data containing only crosses laid out in Randomized Complete Block Design (RCBD).
#'
#'@docType data
#'
#'@usage data(rcbdltc)
#'
#' @format A data frame of four variables of 15 crosses derived from five lines and three testers.
#' \describe{
#'  \item{replication}{four replications}
#'  \item{line}{five inbred genotype}
#'  \item{tester}{three inbred genotype}
#'  \item{yield}{trait of intrest}
#'
#' }
#'@seealso
#'    \code{\link{alphaltcchk}}
#'    ,\code{\link{alphaltc}}
#'    ,\code{\link{rcbdltcchk}}
#'
#' @examples  result = ltc(rcbdltc, replication, line, tester, yield)

"rcbdltc"
