#' Line x Tester data (only Crosses) in Randomized Complete Block design.
#'
#' The Line x Tester data of containing only crosses laid out in Randomized Complete Block design.
#'
#'@docType data
#'
#'@usage data(rcbdltcmt)
#' @format A data frame of 15 crosses derived from five lines and three testers.
#' \describe{
#'  \item{replication}{four replications}
#'  \item{line}{five inbred genotype}
#'  \item{tester}{three inbred genotype}
#'  \item{ph}{plant height}
#'  \item{eh}{ear height}
#' }
#'
#'@seealso
#'    \code{\link{rcbdltc}}
#'    ,\code{\link{alphaltcchk}}
#'    ,\code{\link{rcbdltcchk}}
#'    ,\code{\link{alphaltcmt}}
#'
#' @examples  result = ltcmt(rcbdltcmt, replication, line, tester, rcbdltcmt[,4:5])

"rcbdltcmt"
