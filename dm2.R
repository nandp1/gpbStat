#' @name   dm2
#' @aliases  dm2
#'
#' @title Analysis of Diallel Method 2 data containing only Crosses laid out in RCBD or Alpha Lattice design.
#'
#' @param data dataframe containing following variables
#' @param rep replication
#' @param parent1 parent 1
#' @param parent2 parent 2
#' @param var trait of interest
#' @param block block (for alpha lattice only)
#'
#' @note The blocks are mentioned at end of the function if the experimental design is Alpha Lattice. For RCBD no need mention the blocks.
#'
#' @return \item{\code{Means}}{Two way mean table.}\item{\code{ANOVA}}{ANOVA for the given variable.}\item{\code{Coefficient of Variation}}{Coefficient of Variation of the variable.}\item{\code{Diallel ANOVA}}{Diallel ANVOA for the given trait.}
#' \item{\code{Genetic Variance}}{GCA & SCA varaince.}\item{\code{Combining ability effects}}{Two way table containing Combining
#' ability effects of parents and crosses}\item{\code{Standard Error}}{Standard Errror for comining ability effects.}\item{\code{Critical Difference}}{Critical Difference at 5 pecent for combining ability effects.}
#'
#' @author Nandan Patil \email{tryanother609@gmail.com}
#' @details Analyzing the Diallel Method 2 data containing only crosses which are evaluated in RCBD & Alpha lattice design. All the factors are considered as fixed.
#'
#' @references
#' Griffing, B. (1956) Concept of General and Specific Combining Ability in relation to Diallel Crossing Systems. Australian Journal of Biological Sciences, 9(4), 463-493.
#'
#' Dabholkar, A. R. (1999). Elements of Bio Metrical Genetics. Concept Publishing Company, New Delhi.
#'
#' Singh, R. K. and Chaudhary, B. D. (1977). Biometrical Methods in Quantitative Genetic Analysis. Kalyani Publishers, New Delhi.
#'
#'@seealso \code{\link[gpbStat]{ltcchk}, \link[gpbStat]{ltc}}
#'
#' @import stats
#' @import graphics
#' @export
#'
#' @examples \dontrun{#Diallel Method 2 analysis containing only crosses in RCBD.
#' library(gpbStat)
#' data(dm2rcbd)
#' result1 =  dm2(dm2rcbd, rep, parent1, parent2, DTP)
#' result1
#'
#' #Diallel Method 2 analysis containing only crosses in Alpha Lattice
#' library(gpbStat)
#' data(dm2alpha)
#' result2 =  dm2(dm2alpha, replication, parent1, parent2, TW, block)
#' result2
#'
#' # Save results to csv file
#' lapply(result2, function(x) write.table(data.frame(x), 'result2.csv'  , append= T, sep=','))
#' }
######

dm2 = function(data, rep, parent1, parent2, var, block)
{
  if(!missing(block)){
    rep = deparse(substitute(rep))
    parent1 = deparse(substitute(parent1))
    parent2 = deparse(substitute(parent2))
    block = deparse(substitute(block))
    var = deparse(substitute(var))
    dat = data.frame(Replication = as.factor(data[[rep]]),Block = as.factor(data[[block]]), Parent1 = as.factor(data[[parent1]]),
                     Parent2 = as.factor(data[[parent2]]), var = data[[var]])

    r = length(levels(dat[,1]))
    p = length(levels(dat[,3]))
    b = length(levels(dat[,2]))

    Treatments = as.factor(paste(dat[,3],dat[,4]))
    # ANOVA
    model1 = aov(var ~ Replication + Block:Replication + Treatments, data=dat)
    matrix1<-as.matrix(anova(model1))

    ### Two way Table

    Means <- tapply(dat[, 5], dat[, 3:4], mean, na.rm = TRUE)
    Means1 <- Means
    Means1[lower.tri(Means1)] <- 0
    Means2 <- Means1 + t(Means1) - diag(diag(Means1))
    n <- nrow(Means)
    Sum <- tapply(dat[, 5], dat[, 3:4], sum, na.rm = TRUE)

    # Naming parent
    dimnames(Means) <- list(paste0("Parent", 1:n), paste0("Parent", 1:n))

    SS.gca <- ((sum((rowSums(Means2) + diag(Means2))^2) -
                  (4 * (sum(rowSums(Means1)))^2)/n)/(n + 2))
    SS.sca <- sum((Means1)^2) - sum((rowSums(Means2) + diag(Means2))^2)/(n + 2) + (2 * (sum(rowSums(Means1)))^2)/((n + 1) * (n + 2))



    df <- c(n - 1, n * (n - 1)/2, matrix1[4,1])

    ### Sum of Squares
    SS <- c(SS.gca, SS.sca, matrix1[4,2]/r)
    ### Mean sum of Squares
    MS <- SS/df
    F.Test <- c(MS[-3]/MS[3], NA)
    P.Value <- c(pf(F.Test[-3], df[-3], df[3], lower.tail = FALSE,
                    log.p = FALSE), NA)

    ANOVA <- data.frame(Df = df, `Sum Sq` = SS, `Mean Sq` = MS,
                        `F value` = F.Test, `Pr(>F)` = P.Value,
                        check.names = FALSE)
    rownames(ANOVA) <- c("gca", "sca", "Error")
    class(ANOVA) <- c("anova", "data.frame")


    gca.v <- (MS[1] - MS[3])/(n + 2)
    sca.v <- (MS[2] - MS[3])
    gca.v.to.sca.v <- ((MS[1] - MS[3])/(n + 2))/(MS[2] - MS[3])


    Genetic.Components <- data.frame(Components = c(gca.v, sca.v, gca.v.to.sca.v), row.names = list("gca", "sca", "gca/sca"))

    G <- diag(((rowSums(Means2) + diag(Means2)) - (2 * (sum(rowSums(Means1))))/n)/(n + 2))

    S <- matrix(NA, nrow = n, ncol = n, byrow = TRUE)
    for (i in 1:n) {
      for (j in 1:n) {
        if (i >= j) {
          S[i, j] <- 0
        }
        else {
          S[i, j] <- Means[i, j] - (rowSums(Means2)[i] +
                                      diag(Means2)[i] + rowSums(Means2)[j] + diag(Means2)[j])/(n +
                                                                                                 2) + (2 * sum(rowSums(Means1)))/((n + 1) *
                                                                                                                                    (n + 2))
        }
      }
    }

    Effects <- G + S
    Effects[lower.tri(Effects)] <- NA
    dimnames(Effects) <- dimnames(Means)

    MSE = matrix1[4,3]

    # Co efficient of variation
    me = mean(dat[,"var"])
    cv = (sqrt(MSE)/me)*100


    # Standard error
    SE.gi <- sqrt((n - 1)/(n * (n + 2)) * MSE/r)
    SE.sii <- sqrt((n^2 + n + 2)/((n + 1) * (n + 2)) * MSE/r)
    SE.sij <- sqrt(n * (n - 1)/((n + 1) * (n + 2)) * MSE/r)
    SE.gi.gj <- sqrt(2/(n + 2) * MSE/r)
    SE.sii.sjj <- sqrt(2 * (n - 2)/(n + 2) * MSE/r)
    SE.sij.sik <- sqrt(2 * (n + 1)/(n + 2) * MSE/r)
    SE.sij.skl <- sqrt(2 * n/(n + 2) * MSE/r)
    StdErr <- c(SE.gi, SE.sii, SE.sij, SE.gi.gj, SE.sii.sjj,
                SE.sij.sik, SE.sij.skl)
    nam = c("SE.gi", "SE.sii", "SE.sij", "SE.gi.gj", "SE.sii.sjj",
            "SE.sij.sik", "SE.sij.skl")
    StdErr = setNames(StdErr, nam)

    #CD

    df = matrix1[4, 1]
    critc = abs(qt(0.05/2, df))

    CD = StdErr*critc

    nam = c("CD.gi", "CD.sii", "CD.sij", "CD.gi.gj", "CD.sii.sjj",
            "CD.sij.sik", "CD.sij.skl")

    CD = setNames(CD, nam)

    result = list(Means = Means2, ANOVA = matrix1, "Co efficient of Variation" = cv, "Diallel ANOVA" = ANOVA, "Genetic variances" = Genetic.Components,
                  "Combining ability effects" = Effects,
                  "Standard Error" = StdErr, "Critical Diffiernece" = CD)
    return(result)

  }
  else{

    rep = deparse(substitute(rep))
    parent1 = deparse(substitute(parent1))
    parent2 = deparse(substitute(parent2))
    var = deparse(substitute(var))
    dat = data.frame(Replication = as.factor(data[[rep]]), Parent1 = as.factor(data[[parent1]]), Parent2 = as.factor(data[[parent2]]),
                     var = data[[var]])

    r = length(levels(dat[,1]))
    p = length(levels(dat[,2]))


    dat$Genotypes <- paste(dat[["Parent1"]], dat[["Parent2"]],
                           sep = "-")

    # Overall ANOVA
    model1 = aov(var ~ Replication + Genotypes, data=dat)
    matrix1 = as.matrix(anova(model1))


    ### Two way Table

    Means <- tapply(dat[, 4], dat[, 2:3], mean, na.rm = TRUE)
    Means1 <- Means
    Means1[lower.tri(Means1)] <- 0
    Means2 <- Means1 + t(Means1) - diag(diag(Means1))
    n <- nrow(Means)
    Sum <- tapply(dat[, 4], dat[, 2:3], sum, na.rm = TRUE)

    # Naming parent
    dimnames(Means) <- list(paste0("Parent", 1:n), paste0("Parent", 1:n))

    SS.gca <- ((sum((rowSums(Means2) + diag(Means2))^2) -
                  (4 * (sum(rowSums(Means1)))^2)/n)/(n + 2))
    SS.sca <- sum((Means1)^2) - sum((rowSums(Means2) + diag(Means2))^2)/(n +
                                                                           2) + (2 * (sum(rowSums(Means1)))^2)/((n + 1) * (n + 2))

    # anova cross and parents
    Genotypes <- paste(dat[["Parent1"]], dat[["Parent2"]], sep = "-")
    model11 = aov(var ~ Genotypes + Replication, dat)
    matrix11 = as.matrix(anova(model11))

    df <- c(n - 1, n * (n - 1)/2, matrix1[3,1])

    ### Sum of Squares
    SS <- c(SS.gca, SS.sca, matrix11[3,2]/r)
    ### Mean sum of Squares
    MS <- SS/df
    F.Test <- c(MS[-3]/MS[3], NA)
    P.Value <- c(pf(F.Test[-3], df[-3], df[3], lower.tail = FALSE,
                    log.p = FALSE), NA)


    ANOVA <- data.frame(Df = df, `Sum Sq` = SS, `Mean Sq` = MS,
                        `F value` = F.Test, `Pr(>F)` = P.Value,
                        check.names = FALSE)
    rownames(ANOVA) <- c("gca", "sca", "Error")
    class(ANOVA) <- c("anova", "data.frame")

    #### Genetic variance
    gca.v <- (MS[1] - MS[3])/(n + 2)
    sca.v <- (MS[2] - MS[3])
    gca.v.to.sca.v <- ((MS[1] - MS[3])/(n + 2))/(MS[2] -
                                                   MS[3])


    Genetic.Components <- data.frame(Components = c(gca.v,
                                                    sca.v, gca.v.to.sca.v), row.names = list("gca", "sca", "gca/sca"))

    ###### GCA efects
    G <- diag(((rowSums(Means2) + diag(Means2)) - (2 * (sum(rowSums(Means1))))/n)/(n +
                                                                                     2))
    ####### SCA effects

    S <- matrix(NA, nrow = n, ncol = n, byrow = TRUE)
    for (i in 1:n) {
      for (j in 1:n) {
        if (i >= j) {
          S[i, j] <- 0
        }
        else {
          S[i, j] <- Means[i, j] - (rowSums(Means2)[i] +
                                      diag(Means2)[i] + rowSums(Means2)[j] + diag(Means2)[j])/(n +
                                                                                                 2) + (2 * sum(rowSums(Means1)))/((n + 1) *
                                                                                                                                    (n + 2))
        }
      }
    }

    ### Combined effects matrix
    Effects <- G + S
    Effects[lower.tri(Effects)] <- NA
    dimnames(Effects) <- dimnames(Means)

    MSE = matrix11[3,3]

    # Co efficient of variation
    me = mean(dat[,"var"])
    cv = (sqrt(MSE)/me)*100

    # Standard error
    SE.gi <- sqrt((n - 1)/(n * (n + 2)) * MSE/r)
    SE.sii <- sqrt((n^2 + n + 2)/((n + 1) * (n + 2)) * MSE/r)
    SE.sij <- sqrt(n * (n - 1)/((n + 1) * (n + 2)) * MSE/r)
    SE.gi.gj <- sqrt(2/(n + 2) * MSE/r)
    SE.sii.sjj <- sqrt(2 * (n - 2)/(n + 2) * MSE/r)
    SE.sij.sik <- sqrt(2 * (n + 1)/(n + 2) * MSE/r)
    SE.sij.skl <- sqrt(2 * n/(n + 2) * MSE/r)
    StdErr <- c(SE.gi, SE.sii, SE.sij, SE.gi.gj, SE.sii.sjj,
                SE.sij.sik, SE.sij.skl)
    nam = c("SE.gi", "SE.sii", "SE.sij", "SE.gi.gj", "SE.sii.sjj",
            "SE.sij.sik", "SE.sij.skl")
    StdErr = setNames(StdErr, nam)

    #CD
    df = matrix11[3, 1]
    critc = abs(qt(0.05/2, df))
    CD = StdErr*critc

    nam = c("CD.gi", "CD.sii", "CD.sij", "CD.gi.gj", "CD.sii.sjj",
            "CD.sij.sik", "CD.sij.skl")
    CD = setNames(CD, nam)

    result = list(Means = Means2, "ANOVA" = matrix1, "Co efficient of ariation" = cv, "Diallel ANOVA" = ANOVA,
                  "Genetic variances" = Genetic.Components, "Combining ability effects" = Effects,
                  "Standard Error" = StdErr, "Critical Diffiernece" = CD)
    return(result)
  }
}
