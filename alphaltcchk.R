#' Line x Tester data (Crosses and Checks) in Alpha Lattice
#'
#' The sample Line x Tester data of containing crosses and checks laid out in Alpha Lattice design. The data is composed of five lines, three testers
#' and three checks.
#'
#'@docType data
#'
#'@usage data(alphaltcchk)
#' @format A dataframe of six variables.
#' \describe{
#'  \item{replication}{three replications}
#'  \item{block}{six blocks}
#'  \item{line}{five lines}
#'  \item{tester}{three testers}
#'  \item{check}{three check}
#'  \item{yield}{trait of intrest}
#' }
#'@seealso
#'    \code{\link{rcbdltc}}
#'    ,\code{\link{alphaltc}}
#'    ,\code{\link{rcbdltcchk}}
#'
#' @examples result = ltcchk(alphaltcchk, replication, line, tester, check, yield, block)

"alphaltcchk"
