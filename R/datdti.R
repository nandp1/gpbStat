#' Data of estimating drought tolerance indices without replication
#'
#' The sample data containing 15 genotypes evaluated under non-stress and stress conditions without replications
#'
#'@docType data
#'
#'@usage data(datdti)
#' @format A dataframe of eight variables.
#' \describe{
#'  \item{ENV}{two environment}
#'  \item{GEN}{fifteen genotypes}
#'  \item{CL}{trait cob length}
#'  \item{CG}{trait cob girth}
#'  \item{NKR}{trait number of kernel rows}
#'  \item{NKPR}{trait number of kernels per row}
#'  \item{HSW}{trait hundred seed weight}
#'  \item{GY}{trait grain yield}
#' }
#'@seealso
#'    \code{\link{datrdti}}
#'    ,\code{\link{alphaltc}}
#'    ,\code{\link{rcbdltc}}
#'
#' @examples result = dti(datdti, environment = ENV, genotype = GEN, datdti[,3:8], ns = 'NS-DWR', st = 'ST-DWR')

"datdti"
