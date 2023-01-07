#' @name   ltcmt
#' @aliases ltcmt
#'
#' @title Analysis of Line x Tester data for multiple traits containing only Crosses laid out in RCBD or Alpha Lattice design.
#'
#' @param data dataframe containing following variables
#' @param replication replication
#' @param line line
#' @param tester tester
#' @param traits multiple traits of interest
#' @param block block (for alpha lattice design only)
#'
#' @note The block variable is inserted at the last if the experimental design is Alpha Lattice. For RCBD no need to have block factor.
#'
#' @return \item{\code{Mean}}{Table of means.}\item{\code{ANOVA}}{ANOVA with all the factors.}\item{\code{GCA.Line}}{GCA effects of lines.}\item{\code{GCA.Tester}}{GCA effects of testers.}
#' \item{\code{SCA}}{SCA effects of crosses.}\item{\code{CV}}{Coefficent of Variation.}\item{\code{Genetic.Variance.Covariance}}{Genetic component Variance and covariance.}
#' \item{\code{Std.Error}}{Standard error for combining ability effects.}\item{\code{C.D.}}{Critical Difference at 5 pecent for combining ability effects.}\item{\code{Add.Dom.Var}}{Additive and Dominance component of Variance.}
#' \item{\code{Contribution.of.Line.Tester}}{Contribution of Lines, Testers and Line x Tester towards total variation.}
#'
#' @author Nandan Patil \email{tryanother609@gmail.com}
#' @details Analyzing the line by tester data of multiple trais only using the data from crosses which are evaluated in RCBD and Alpha lattice design. All the factors are considered as fixed.
#'
#'
#' @references
#' Kempthorne, O. (1957), Introduction to Genetic Statistics. John Wiley and Sons, New York.
#'              , 468-472.
#' Singh, R. K. and Chaudhary, B. D. (1977). Biometrical Methods in Quantitative Genetic Analysis. Kalyani Publishers, New Delhi.
#'
#'@seealso \code{\link[gpbStat]{ltcchk}}
#'
#' @import stats
#' @import graphics
#' @export
#'
#' @examples \dontrun{#Line Tester analysis data with only crosses in RCBD
#' library(gpbStat)
#' data(rcbdltcmt)
#' result1 = ltcmt(rcbdltcmt, replication, line, tester, rcbdltcmt[,4:5])
#' result1
#'
#' #Line Tester analysis data with only crosses in Alpha Lattice
#' library(gpbStat)
#' data(alphaltcmt)
#' result2 = ltcmt(alphaltcmt, replication, line, tester, alphaltcmt[,5:7], block)
#' result2
#' }

ltcmt = function(data, replication, line, tester, traits, block)
{
  if (!missing(block)) {
    replication <- deparse(substitute(replication))
    block <- deparse(substitute(block))
    line <- deparse(substitute(line))
    tester <- deparse(substitute(tester))
    cat("\nAnalysis of Line x Tester for Multiple traits", "\n")
    dataset = data.frame(Replication = as.factor(data[["replication"]]), Block = as.factor(data[["block"]]),
                         Line = as.factor(data[["line"]]), Tester = as.factor(data[["tester"]]), traits)

    ### Levels
    r = nlevels(dataset$Replication)
    l = nlevels(dataset$Line)
    t = nlevels(dataset$Tester)
    v = ncol(traits)

    ### Converting data to list
    az = as.list.data.frame(traits)
    az = lapply(az, as.data.frame)

    az=lapply(names(az), function(i){
      x <- az[[ i ]]
      # set first column to a new name
      names(x)[1] <- i
      x
    })

    names(az) = c(colnames(traits))

    rep = dataset$Replication
    lin = dataset$Line
    tes = dataset$Tester
    blk = dataset$Block

    az = lapply(az, cbind, Replication = as.character(rep))
    az = lapply(az, cbind, Line = as.character(lin))
    az = lapply(az, cbind, Tester = as.character(tes))
    az = lapply(az, cbind, Block = as.character(blk))

    dat = lapply(az, function(x) x[,c(2,5,3,4,1)])


    ### Mean for each of the trait
    z00 = function(x){
      tapply(x[, 5], x[, 3:4], mean, na.rm = TRUE)
    }
    indmean  = lapply(dat, z00)
    indmean

    ### Overall ANOVA and Cross ANOVA
    z0 = function(x){
      Genotype <- as.factor(paste(x[, 3], x[, 4]))
      model1 <- aov(x[,5] ~ Replication +  Replication:Block + Genotype, data = x)
      matrix1 <- as.matrix(anova(model1))
    }
    f0 = lapply(dat, z0)
    f0

    ### Line Tester ANOVA
    z1 = function(x){
      model4 <- aov(x[,5] ~ Line * Tester, data = x)
      matrix4 <- as.matrix(anova(model4))
    }
    f1 = lapply(dat, z1)
    f1

    ### GCA lines
    z4 = function(x){
      Means1 = tapply(x[, 5], x[, 3], mean)
      Means1 - mean(x[, 5])
    }
    f4 = lapply(dat, z4)

    f4 = do.call(cbind, f4)
    f4

    ### GCA testers
    z5 = function(x){
      Means2 = tapply(x[, 5], x[, 4], mean)
      Means2 - mean(x[, 5])
    }
    f5 = lapply(dat, z5)
    f5 = do.call(cbind, f5)
    f5

    ### SCA
    z6 = function(x){
      avg <- tapply(x[, 5], x[, 3:4], mean, na.rm = TRUE)
      sca  = t(t(avg-rowMeans(avg)) - colMeans(avg)) + mean(avg)
    }
    SCA = lapply(dat, z6)
    SCA

    ### Final ANOVA
    z7 = function(x){
      Genotype <- as.factor(paste(x[, 3], x[, 4]))
      model1 <- aov(x[,5] ~ Replication +  Replication:Block + Genotype, data = x)
      matrix1 <- as.matrix(anova(model1))
      matrix1

      model4 <- aov(x[,5] ~ Line * Tester, data = x)
      matrix4 <- as.matrix(anova(model4))
      matrix4

      matrix <- rbind(matrix1[c(1,3,2), ], matrix4[1:3, ], matrix1[4, ])
      total1 <- sum(matrix1[, 1])
      total2 <- sum(matrix1[, 2])
      Total <- c(total1, total2, NA, NA, NA)
      matrix <- rbind(matrix, Total)
      rownames(matrix) <- c("Replication", "Blocks within Replication",
                            "Crosses",
                            "Lines", "Testers", "Lines X Testers",
                            "Error", "Total")
      matrix
    }

    anv = lapply(dat, z7)
    anv
    ### Mean of traits
    mea = lapply(dat, function(x) mean(x[,5]))

    ### Co Efficient of variation
    z8 = function(x){
      cm <- x[7, 3]
    }

    mse = lapply(anv, z8)

    sqr = mapply('sqrt', mse)
    cs = mapply('/', sqr, mea)
    cv = as.vector(cs)*100
    names(cv) <- c(names(dat))
    cv

    ### Genotypic Phenotypic Variance and Covariance
    z9  = function(x){
      rmss = x[1, 3]
      cmss = x[3, 3]
      lmss = x[4, 3]
      tmss = x[5, 3]
      ltmss = x[6, 3]
      emss = x[7, 3]
      data.frame(rmss, cmss, lmss, tmss, emss)
    }
    ss = lapply(anv, z9)
    ss
    EV = lapply(ss, function(x) x$emss)
    GV = lapply(ss, function(x) (x$cmss - x$rmss)/r)
    PV = mapply('+', GV, EV)
    PCV = lapply(PV, function(x) (sqrt(x)*100))
    PCV = mapply('/', PCV, mea)
    GCV = lapply(GV, function(x) (sqrt(x)*100))
    GCV = mapply('/', GCV, mea)
    ECV = lapply(EV, function(x) (sqrt(x)*100))
    ECV = mapply('/', ECV, mea)
    BSH = mapply('/', GV, PV)
    PGECV = list(PV, GV, EV, PCV, GCV, ECV, BSH)
    PGECV = unlist(PGECV)
    PGECV = matrix(PGECV, nrow = v, ncol = 7)
    rownames(PGECV) = c(names(dat))
    colnames(PGECV) = c("Phenotypic Variance", "Genotypic Variance", "Environmental Variance",
                        "Phenotypic coefficient of Variation",  "Genotypic coefficient of Variation", "Environmental coefficient of Variation"
                        , "Broad sense heritability")
    PGECV
    ### Standard Error and Critical Difference
    emss = mse
    s1 = lapply(emss, function(x) sqrt(x/(r*t)))
    s2 = lapply(emss, function(x) sqrt(x/(r*l)))
    s3 = lapply(emss, function(x) sqrt(x/r))
    s4 = lapply(emss, function(x) sqrt(2*x/(r*t)))
    s5 = lapply(emss, function(x) sqrt(2 * x/(r * l)))
    s6 = lapply(emss, function(x) sqrt(2 * x/r))
    ses <- list(s1, s2, s3, s4, s5, s6)
    ses= unlist(ses)
    sesmat = matrix(ses,nrow = v,ncol = 6)
    colnames(sesmat) <- c("S.E. gca for line", "S.E. gca for tester",
                          "S.E. sca effect", "S.E. (gi - gj)line",
                          "S.E. (gi - gj)tester", "S.E. (sij - skl)tester")
    rownames(sesmat) <- c(names(dat))
    sesmat
    ### Critical difference
    df =  lapply(anv,function(x) x[7, 1])
    df
    cri = lapply(df, function(x) abs(qt(0.05/2, x)))
    se = c(s1, s2, s3, s4, s5, s6)
    cd = mapply('*', se, cri)
    cd = unlist(cd)
    cdmat = matrix(cd,nrow = v,ncol = 6)
    rownames(cdmat) <- c(names(dat))
    colnames(cdmat) = c("C.D. gca for line", "C.D. gca for tester",
                        "C.D. sca effect", "C.D. (gi - gj)line",
                        "C.D. (gi - gj)tester", "C.D. (sij - skl)tester")
    cdmat
    ### Additive Variance and Dominance Variance
    z10 = function(x){
      cov1 <- (x[4, 3] - x[6, 3])/(r * t)
      cov2 <- (x[5, 3] - x[6, 3])/(r * l)
      cov3 <- (((l - 1) * x[4, 3] + (t - 1) * x[5, 3])/(l + t - 2) - x[6, 3])/(r * (2 * l * t - l - t))
      cov4 <- ((x[4, 3] - x[7, 3]) + (x[5, 3] -x[7, 3]) + (x[6, 3] - x[7, 3]))/(3 * r) +(6 * r * cov3 - r * (l + t) * cov3)/(3 * r)
      data.frame(cov1, cov2, cov3, cov4)
    }
    cov = lapply(anv, z10)
    cov1 = lapply(cov, function(x) x$cov1)
    cov2 = lapply(cov, function(x) x$cov2)
    cov3 = lapply(cov, function(x) x$cov3)
    cov4 = lapply(cov, function(x) x$cov4)

    F = 0
    Var.Add0 = lapply(cov, function(x) x$cov3*(4/(1 + F)))
    var.Dom0 = lapply(anv, function(x) ((x[6, 3] - x[7, 3])/r) * (2/(1 + F)))

    F = 1
    Var.Add1 = lapply(cov, function(x) x$cov3 * (4/(1 + F)))
    Var.Dom1 = lapply(anv, function(x) ((x[6, 3] - x[7, 3])/r) * (2/(1 + F)))

    vari = list(cov1, cov2, cov3, cov4, Var.Add0, Var.Add1, var.Dom0, Var.Dom1)
    vari = unlist(vari)
    varimat = matrix(vari, nrow = v, ncol = 8)
    colnames(varimat) =  c("Cov H.S. (line)", "Cov H.S. (tester)",
                           "Cov H.S. (average)", "Cov F.S. (average)",
                           "Addittive Variance(F=0)", "Addittive Variance(F=1)",
                           "Dominance Variance(F=0)", "Dominance Variance(F=1)")
    rownames(varimat) = c(names(dat))
    varimat
    ### Contribution of Line, Testers and Line x Tester
    c1 = lapply(anv, function(x) x[4, 2] * 100/x[3, 2])
    c2 = lapply(anv, function(x) x[5, 2] * 100/x[3, 2])
    c3 = lapply(anv, function(x) x[6, 2] * 100/x[3, 2])
    cc = list(c1,c2,c3)
    cc = unlist(cc)
    cmat = matrix(cc, nrow = v, ncol = 3)
    rownames(cmat) = c(names(dat))
    colnames(cmat) = c("Lines", "Tester", " Line x Tester")
    cmat
    res = list(Mean = indmean, ANOVA = anv, GCA.Line = f4, GCA.Tester = f5, SCA = SCA,
               CV = cv, Genetic.Variance.Covariance. = PGECV, Std.Error = sesmat,
               C.D. = cdmat, Add.Dom.Var = varimat, Contribution.of.Line.Tester = cmat)
    return(res)
  }
  else {

    replication <- deparse(substitute(replication))
    replication <- as.factor(replication)
    line <- deparse(substitute(line))
    line <- as.factor(line)
    tester <- deparse(substitute(tester))
    tester <- as.factor(tester)
    dataset <-data.frame(Replications= as.factor(data[["replication"]]),
                         Lines = as.factor(data[["line"]]), Testers = as.factor(data[["tester"]]), traits)


    ### Levels
    r = nlevels(dataset$Replication)
    l = nlevels(dataset$Line)
    t = nlevels(dataset$Tester)
    v = ncol(traits)

    ### Converting data to list
    az = as.list.data.frame(traits)
    az = lapply(az, as.data.frame)

    az=lapply(names(az), function(i){
      x <- az[[ i ]]
      # set first column to a new name
      names(x)[1] <- i
      x
    })

    names(az) = c(colnames(traits))

    rep = dataset$Replication
    lin = dataset$Line
    tes = dataset$Tester

    az = lapply(az, cbind, Replication = as.character(rep))
    az = lapply(az, cbind, Line = as.character(lin))
    az = lapply(az, cbind, Tester = as.character(tes))

    dat = lapply(az, function(x) x[,c(2,3,4,1)])


    ##################

    ### Mean for each of the trait
    z00 = function(x){
      tapply(x[, 4], x[, 2:3], mean, na.rm = TRUE)
    }
    indmean  = lapply(dat, z00)
    indmean

    ### Overall ANOVA and Cross ANOVA
    z0 = function(x){
      Genotype <- as.factor(paste0(x[, 2], x[, 3]))
      model1 <- aov(x[,4] ~ Replication +   Genotype, data = x)
      matrix1 <- as.matrix(anova(model1))
    }
    f0 = lapply(dat, z0)
    f0

    ### Line Tester ANOVA
    z1 = function(x){
      model4 <- aov(x[,4] ~ Line * Tester, data = x)
      matrix4 <- as.matrix(anova(model4))
    }
    f1 = lapply(dat, z1)
    f1
    ### GCA lines
    z4 = function(x){
      Means1 = tapply(x[, 4], x[, 2], mean)
      Means1 - mean(x[, 4])
    }
    f4 = lapply(dat, z4)
    f4 = do.call(cbind, f4)
    f4

    ### GCA testers
    z5 = function(x){
      Means2 = tapply(x[, 4], x[, 3], mean)
      Means2 - mean(x[, 4])
    }

    f5 = lapply(dat, z5)
    f5 = do.call(cbind, f5)
    f5

    ### SCA
    z6 = function(x){
      avg <- tapply(x[, 4], x[, 2:3], mean, na.rm = TRUE)
      t(t(avg-rowMeans(avg)) - colMeans(avg)) + mean(avg)
    }
    SCA = lapply(dat, z6)

    SCA

    ### Final ANOVA
    z7 = function(x){
      Genotype <- as.factor(paste(x[, 2], x[, 3]))
      model1 <- aov(x[,4] ~ Replication +  Genotype, data = x)
      matrix1 <- as.matrix(anova(model1))
      matrix1

      model4 <- aov(x[,4] ~ Line * Tester, data = x)
      matrix4 <- as.matrix(anova(model4))
      matrix4

      matrix <- rbind(matrix1[c(1,2), ], matrix4[1:3, ], matrix1[3, ])
      total1 <- sum(matrix1[, 1])
      total2 <- sum(matrix1[, 2])
      Total <- c(total1, total2, NA, NA, NA)
      matrix <- rbind(matrix, Total)
      rownames(matrix) <- c("Replication",
                            "Crosses",
                            "Lines", "Testers", "Lines X Testers",
                            "Error", "Total")
      matrix
    }
    anv = lapply(dat, z7)
    anv

    ### Mean of traits
    mea = lapply(dat, function(x) mean(x[,4]))

    ### Co Efficient of variation
    z8 = function(x){
      cm <- x[6, 3]
    }
    mse = lapply(anv, z8)
    sqr = mapply('sqrt', mse)
    cs =  mapply('/', sqr, mea)
    cv = as.vector(cs)*100
    cv

    ### Genotypic Phenotypic Variance and Covariance
    z9  = function(x){
      rmss = x[1, 3]
      cmss = x[2, 3]
      lmss = x[3, 3]
      tmss = x[4, 3]
      ltmss = x[5, 3]
      emss = x[6, 3]
      data.frame(rmss, cmss, lmss, tmss, emss)
    }
    ss = lapply(anv, z9)
    ss
    EV = lapply(ss, function(x) x$emss)
    GV = lapply(ss, function(x) (x$cmss - x$rmss)/r)
    PV = mapply('+', GV, EV)
    PCV = lapply(PV, function(x) (sqrt(x)*100))
    PCV = mapply('/', PCV, mea)
    GCV = lapply(GV, function(x) (sqrt(x)*100))
    GCV = mapply('/', GCV, mea)
    ECV = lapply(EV, function(x) (sqrt(x)*100))
    ECV = mapply('/', ECV, mea)
    BSH = mapply('/', GV, PV)
    PGECV = list(PV, GV, EV, PCV, GCV, ECV, BSH)

    PGECV = unlist(PGECV)
    PGECV = matrix(PGECV, nrow = v, ncol = 7)

    rownames(PGECV) = c(colnames(traits))
    colnames(PGECV) = c("Phenotypic Variance", "Genotypic Variance", "Environmental Variance",
                        "Phenotypic coefficient of Variation",  "Genotypic coefficient of Variation", "Environmental coefficient of Variation"
                        , "Broad sense heritability")
    PGECV

    ### Standard Error and Critical Difference
    emss = mse
    s1 = lapply(emss, function(x) sqrt(x/(r*t)))
    s2 = lapply(emss, function(x) sqrt(x/(r*l)))
    s3 = lapply(emss, function(x) sqrt(x/r))
    s4 = lapply(emss, function(x) sqrt(2*x/(r*t)))
    s5 = lapply(emss, function(x) sqrt(2 * x/(r * l)))
    s6 = lapply(emss, function(x) sqrt(2 * x/r))
    ses <- list(s1, s2, s3, s4, s5, s6)
    ses= unlist(ses)
    sesmat = matrix(ses,nrow = v,ncol = 6)
    colnames(sesmat) <- c("S.E. gca for line", "S.E. gca for tester",
                          "S.E. sca effect", "S.E. (gi - gj)line",
                          "S.E. (gi - gj)tester", "S.E. (sij - skl)tester")
    rownames(sesmat) <- c(colnames(traits))
    sesmat

    ### Critical difference
    df =  lapply(anv,function(x) x[7, 1])
    df
    cri = lapply(df, function(x) abs(qt(0.05/2, x)))
    se = c(s1, s2, s3, s4, s5, s6)
    cd = mapply('*', se, cri)
    cd = unlist(cd)
    cdmat = matrix(cd,nrow = v,ncol = 6)
    rownames(cdmat) <- c(colnames(traits))
    colnames(cdmat) = c("C.D. gca for line", "C.D. gca for tester",
                        "C.D. sca effect", "C.D. (gi - gj)line",
                        "C.D. (gi - gj)tester", "C.D. (sij - skl)tester")
    cdmat

    ### Additive Variance and Dominance Variance
    z10 = function(x){
      cov1 <- (x[3, 3] - x[5, 3])/(r * t)
      cov2 <- (x[4, 3] - x[5, 3])/(r * l)
      cov3 <- (((l - 1) * x[3, 3] + (t - 1) * x[4, 3])/(l + t - 2) - x[5, 3])/(r * (2 * l * t - l - t))
      cov4 <- ((x[3, 3] - x[6, 3]) + (x[4, 3] -x[6, 3]) + (x[5, 3] - x[6, 3]))/(3 * r) +(6 * r * cov3 - r * (l + t) * cov3)/(3 * r)
      data.frame(cov1, cov2, cov3, cov4)
    }
    cov = lapply(anv, z10)
    cov1 = lapply(cov, function(x) x$cov1)
    cov2 = lapply(cov, function(x) x$cov2)
    cov3 = lapply(cov, function(x) x$cov3)
    cov4 = lapply(cov, function(x) x$cov4)

    F = 0
    Var.Add0 = lapply(cov, function(x) x$cov3*(4/(1 + F)))
    var.Dom0 = lapply(anv, function(x) ((x[5, 3] - x[6, 3])/r) * (2/(1 + F)))

    F = 1
    Var.Add1 = lapply(cov, function(x) x$cov3 * (4/(1 + F)))
    Var.Dom1 = lapply(anv, function(x) ((x[5, 3] - x[6, 3])/r) * (2/(1 + F)))

    vari = list(cov1, cov2, cov3, cov4, Var.Add0, Var.Add1, var.Dom0, Var.Dom1)
    vari = unlist(vari)
    varimat = matrix(vari, nrow = v, ncol = 8)
    colnames(varimat) =  c("Cov H.S. (line)", "Cov H.S. (tester)",
                           "Cov H.S. (average)", "Cov F.S. (average)",
                           "Addittive Variance(F=0)", "Addittive Variance(F=1)",
                           "Dominance Variance(F=0)", "Dominance Variance(F=1)")
    rownames(varimat) = c(paste(colnames(traits)))

    varimat

    ### Contribution of Line, Testers and Line x Tester
    c1 = lapply(anv, function(x) x[3, 2] * 100/x[2, 2])
    c2 = lapply(anv, function(x) x[4, 2] * 100/x[2, 2])
    c3 = lapply(anv, function(x) x[5, 2] * 100/x[2, 2])
    cc = list(c1,c2,c3)
    cc = unlist(cc)
    cmat = matrix(cc, nrow = v, ncol = 3)
    rownames(cmat) = c(colnames(traits))
    colnames(cmat) = c("Lines", "Tester", " Line x Tester")
    cmat
    res = list(Mean = indmean, ANOVA = anv, GCA.Line = f4, GCA.Tester = f5, SCA = SCA,
               CV = cv, Genetic.Variance.Covariance = PGECV, Std.Error = sesmat,
               C.D. = cdmat, Add.Dom.Var = varimat, Contribution.of.Line.Tester = cmat)
    return(res)
  }
}
