#' @name   dti
#' @aliases dti
#'
#' @title Estimation of Drought Tolerance Indices.
#'
#' @param data dataframe containing following variables
#' @param environment column with two levels i.e., non-stress and stress conditions
#' @param genotype genotypes evaluated
#' @param traits trait of interest
#' @param ns name of level indicating evaluation under non-stress (irrigated) conditions
#' @param st name of level indicating evaluation under stress conditions
#'
#' @note The function can handle both replicated and non-replicated data refer the examples.
#'
#' @return \item{\code{TOL}}{Stress tolerance.}\item{\code{STI}}{Stress tolerance index.}\item{\code{SSPI}}{Stress susceptibility percentage index.}\item{\code{YI}}{Yield index.}\item{\code{YSI}}{Yield stability index.}\item{\code{RSI}}{Relative
#' stress index.}\item{\code{MP}}{Mean productivity.}\item{\code{GMP}}{Geometric mean productivity}\item{\code{HMP}}{Harmonic mean.}\item{\code{MRP}}{Mean relative performance.}\item{\code{PYR}}{Percent yield Reduction.}
#'
#' @author Nandan Patil \email{tryanother609@gmail.com}
#' @details Estimation various Drought Tolerance Indices of genotypes evaluated under stress and non-stress conditions of both replicated and non-replicated data.
#'
#'
#' @references
#' Lamba, K., Kumar M., Singh V., Chaudhary L., Sharma R., Yashveer S. and Dalal, M. S. (2023). Heat stress tolerance indices for identification of the heat tolerant wheat genotypes.
#'            Scientific Reports, 13(1). https://doi.org/10.1038/s41598-023-37634-8
#'
#'@seealso \code{\link[gpbStat]{ltc}, \link[gpbStat]{ltcchk}, \link[gpbStat]{ltcmt}}
#'
#' @import stats
#' @import graphics
#' @import magrittr
#' @importFrom tidyr pivot_longer
#' @importFrom purrr map
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr select
#' @importFrom dplyr bind_cols
#' @export
#'
#' @examples \dontrun{# Estimating drought tolerance indices
#' library(gpbStat)
#'
#' data(datdti)
#' result1 =  dti(datdti, environment = ENV, genotype = GEN, datdti[,3:8],
#'               ns = 'NS-DWR', st = 'ST-DWR')
#' result1
#'
#' data(datrdti)
#' result2 = dti(datrdti, environment = ENV, genotype = GEN, datrdti[,4:9],
#'             ns = 'NS-DWR', st = 'ST-DWR')
#' result2
#'}

dti = function(data, environment, genotype, traits, ns, st){

    genotype <- deparse(substitute(genotype))
    environment <- deparse(substitute(environment))

    df1 = cbind.data.frame(ENV = data[[environment]], GEN = data[[genotype]],
                         traits)

    nam = names(traits)
    dfns  = subset(df1, df1$ENV == ns)

    dfns1 = dfns %>%
      pivot_longer(cols = nam) %>%
      split(.$name) %>%
      map(~.x %>% select(-name))

    dfns1 = lapply(dfns1, function(x)
      data.frame(value = with(x, tapply(value, GEN, mean)))
    )

    dfst = subset(df1, df1$ENV == st)

    dfst1 = dfst %>%
      pivot_longer(cols = nam) %>%
      split(.$name) %>%
      map(~.x %>% select(-name))

    dfst1 = lapply(dfst1, function(x)
      data.frame(value = with(x, tapply(value, GEN, mean)))
    )

    ########## Calculating Indices

    xp = lapply(dfns1, function(x)
      mean(x$value)
    )

    xs = lapply(dfst1, function(x)
      mean(x$value)
    )

    TOL = Map('-', dfns1, dfst1) # TOL
    TOL = lapply(TOL, setNames, 'TOL')


    YSI = Map('/', dfst1, dfns1) # YSI
    YSI = lapply(YSI, setNames, 'YSI')

    STI = Map('*', dfst1, dfns1)
    xp1 = Map('*', xp, xp)
    STI = Map('/', STI, xp1)
    STI = lapply(STI, setNames, 'STI')

    SSPI = Map('-', dfns1, dfst1)
    xp2 = Map('*', xp, 2)
    SSPI = Map('/', SSPI, xp2)
    SSPI = Map('*', SSPI, 100)
    SSPI = lapply(SSPI, setNames, 'SSPI')

    yi = Map('/', dfst1, xs)
    yi = lapply(yi, setNames, 'YI')

    rsi = Map('/', dfns1, dfst1)
    rsi1 = Map('/', xs, xp)
    rsi = Map('/', rsi, rsi1)
    rsi = lapply(rsi, setNames, 'RSI')

    mp = Map('+', dfns1, dfst1)
    mp = Map('/', mp, 2)
    mp = lapply(mp, setNames, 'MP')

    gmp = Map('*', dfst1, dfns1)
    gmp = Map('sqrt', gmp)
    gmp = lapply(gmp, setNames, 'GMP')

    hm = Map('*', dfst1, dfns1)
    hm1 = Map('+', dfst1, dfns1)
    hm = Map('/', hm, hm1)
    hm = Map('*', hm, 2)
    hm = lapply(hm, setNames, 'HM')

    mrp1 = Map('/', dfst1, xs)
    mrp2 = Map('/', dfns1, xp)
    mrp = Map('+', mrp1, mrp2)
    mrp = lapply(mrp, setNames, 'MRP')

    pyr = Map('-', dfns1, dfst1)
    pyr = Map('/', pyr, dfns1)
    pyr = Map('*', pyr, 100)
    pyr = lapply(pyr, setNames, 'PYR')

    lis = c(TOL, STI, YSI, yi, rsi, SSPI, mp, gmp, hm, mrp, pyr)

    mm = map(split(lis, names(lis)), bind_cols)
    mm = lapply(mm, function(x)
    x = rownames_to_column(x, var = 'GEN'))

    return(mm)

}
