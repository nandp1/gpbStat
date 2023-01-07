#' Line x Tester data (Crosses and Checks) in RCBD
#'
#' The sample Line x Tester data of containing crosses and checks laid out in Randomized Complete Block Design (RCBD). The data is composed of five lines, three testers
#' and three checks.
#'
#' @docType data
#'
#'@usage data(rcbdltcchk)
#' @format A dataframe of six variables.
#' \describe{
#'  \item{replication}{four replications}
#'  \item{line}{five lines}
#'  \item{tester}{three testers}
#'  \item{yield}{trait of intrest}
#'
#' }
#'@seealso
#'    \code{\link{rcbdltc}}
#'    ,\code{\link{alphaltc}}
#'    ,\code{\link{alphaltcchk}}
#'
#' @examples result = ltcchk(rcbdltcchk, replication, line, tester, check, yield)

"rcbdltcchk"
