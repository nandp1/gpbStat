#' Data of estimating drought tolerance indices with replication
#'
#' The sample data containing 15 genotypes evaluated under non-stress and stress conditions with replications
#'
#'@docType data
#'
#'@usage data(datrdti)
#' @format A dataframe of nine variables.
#' \describe{
#'  \item{ENV}{two environment}
#'  \item{GEN}{fifteen genotypes}
#'  \item{REP}{two replications}
#'  \item{CL}{trait cob length}
#'  \item{CG}{trait cob girth}
#'  \item{NKR}{trait number of kernel rows}
#'  \item{NKPR}{trait number of kernels per row}
#'  \item{HSW}{trait hundred seed weight}
#'  \item{GY}{trait grain yield}
#' }
#'@seealso
#'    \code{\link{datdti}}
#'    ,\code{\link{alphaltc}}
#'    ,\code{\link{rcbdltc}}
#'
#' @examples result = dti(datrdti, environment = ENV, genotype = GEN, datrdti[,4:9],
#'                    ns = 'NS-DWR', st = 'ST-DWR')

"datrdti"
