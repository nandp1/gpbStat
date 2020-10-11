#' Line x Tester data for RCBD
#'
#' The sample data containing only crosses laid out in RCBD.
#'
#'@docType data
#'
#'@usage data(rcbdlt)
#'
#' @format A data frame of four variables of 15 crosses derived from five lines and three testers.
#' \describe{
#'  \item{replication}{four replications}
#'  \item{line}{five inbred genotype}
#'  \item{tester}{three inbred genotype}
#'  \item{yield}{trait of intrest}
#'
#' }
#' @examples results2 = ltcrcbd(rcbdlt, replication, line, tester, yield)

"rcbdlt"
