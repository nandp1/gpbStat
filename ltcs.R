#' @name   ltcs
#' @aliases ltcs
#'
#' @title Analysis of Line x Tester data on single plant basis containing only Crosses laid out in RCBD or Alpha Lattice design.
#'
#' @param data dataframe containing following variables
#' @param replication replication
#' @param line line
#' @param tester tester
#' @param obs single plant observations
#' @param y dependent variable
#' @param block block (for alpha lattice design only)
#'
#' @note The block variable is inserted at the last if the experimental design is Alpha Lattice. For RCBD no need to have block factor.
#'
#' @return \item{\code{Mean}}{Table of means.}\item{\code{ANOVA}}{ANOVA with all the factors.}\item{\code{GCA.Line}}{GCA effects of lines.}\item{\code{GCA.Tester}}{GCA effects of testers.}
#' \item{\code{SCA}}{SCA effects of crosses.}\item{\code{CV}}{Coefficent of Variation.}
#' \item{\code{Std.Error}}{Standard error for combining ability effects.}\item{\code{C.D.}}{Critical Difference at 5 pecent for combining ability effects.}
#' \item{\code{Contribution.of.Line.Tester}}{Contribution of Lines, Testers and Line x Tester towards total variation.}
#'
#' @author Nandan L Patil \email{tryanother609@gmail.com}
#' @details Analyzing the line by tester data single plant observations evaluated in RCBD and Alpha lattice design. All the factors are considered as fixed.
#'
#'
#' @references
#' Kempthorne, O. (1957), Introduction to Genetic Statistics. John Wiley and Sons, New York.
#'              , 468-472.
#' Singh, R. K. and Chaudhary, B. D. (1977). Biometrical Methods in Quantitative Genetic Analysis. Kalyani Publishers, New Delhi.
#' Arunachalam, V. (1974), The fallacy behind use of modified line x tester design. The Indian Journal of Genetics and Plant Breeding, 34: 280-287.
#'
#'@seealso \code{\link[gpbStat]{ltc}}, \link[gpbStat]{ltcmt}
#'
#' @import stats
#' @import graphics
#' @export
#'
#' @examples \dontrun{#Line Tester analysis data with only crosses in RCBD
#' library(gpbStat)
#' data(rcbdltcs)
#' result1 = ltcs(rcbdltcs, replication, line, tester, obs, yield)
#' result1
#'
#' #Line Tester analysis data with only crosses in Alpha Lattice
#' library(gpbStat)
#' data(alphaltcs)
#' result2 = ltcs(alphaltcs, replication, line, tester, obs, yield, block)
#' result2
#' }

ltcs = function (data, replication, line, tester, obs, y, block)
  {

  if (!missing(block)) {

  replication <- deparse(substitute(replication))
  replication <- as.factor(replication)
  block <- deparse(substitute(block))
  block <- as.factor(block)
  line <- deparse(substitute(line))
  line <- as.factor(line)
  tester <- deparse(substitute(tester))
  tester <- as.factor(tester)
  obs = deparse(substitute(obs))
  obs <- as.factor(obs)
  Y <- deparse(substitute(y))

  Y <- deparse(substitute(y))
  message("\nAnalysis of Line x Tester on single plant basis: ", Y, "\n")
  dataset <- cbind.data.frame(Replications = as.factor(data[["replication"]]),
                              Blocks = as.factor(data[["block"]]), Lines = as.factor(data[["line"]]),
                              Testers = as.factor(data[["tester"]]), Obs = as.factor(data[["obs"]]),
                              Y = data[[Y]])

  r = length(levels(dataset$Replications))
  l = length(levels(dataset$Lines))
  t = length(levels(dataset$Testers))
  n = length(levels(dataset$Obs))


  dataset$Treatments <- as.factor(paste0(dataset$Lines, dataset$Testers, dataset$Obs))
  dataset$Hybrids = as.factor(paste0(dataset$Lines, dataset$Testers))

  model1 <- aov(Y ~ Replications + Blocks:Replications +
                  Hybrids, data = dataset)

  matrix1 <- as.matrix(anova(model1))
  matrix1

  model4 <- aov(Y ~ Lines * Testers, data = dataset)
  matrix4 <- as.matrix(anova(model4))
  matrix4

  modellts <- aov(Y ~ Lines + Testers + Blocks:Replications +
                    Replications*Hybrids, data = dataset)
  matrixlts <- as.matrix(anova(modellts))
  matrixlts


  model2 <- aov(Y ~ Lines * Testers, data = dataset)
  matrix2 <- as.matrix(anova(model2))
  matrix2

  modellts <- aov(Y ~ Lines + Testers + Replications*Hybrids, data = dataset)
  matrixlts <- as.matrix(anova(modellts))
  matrixlts

  ### ANOVA for single plant basis

  matixlts = rbind(matrix2[1:3,], matrixlts[5:6,])
  matixlts
  rownames(matixlts) <- c("Line", "Tester",
                          "Line x Tester", "Replication x Hybrid",
                          "Error")

  twt <- tapply(dataset[, 6], dataset[, 3:4], mean, na.rm = TRUE)

  Means <- twt
  testers <- ncol(twt)
  lines <- nrow(twt)


  #sca
  avg <- tapply(dataset[, 6], dataset[, 3:4], mean, na.rm = TRUE)
  sca  = t(t(avg-rowMeans(avg)) - colMeans(avg)) + mean(avg)


  Means1 <- tapply(dataset[, 6], dataset[, 3], mean, na.rm = TRUE)

  GCA.lines = round(Means1 - mean(dataset[, 6], na.rm = TRUE),
                    2)

  Means2 = tapply(dataset[, 6], dataset[, 4], mean, na.rm = TRUE)

  GCA.testers = round(Means2 - mean(dataset[, 6], na.rm = TRUE),
                      2)

  ### Crosses ANOVA
  Crosses <- as.factor(paste(dataset[, 3], dataset[,4]))
  model3 <- aov(Y ~ Crosses, data = dataset)
  matrix3 <- as.matrix(anova(model3))

  ### Overall ANOVA
  matrix <- rbind(matrix1[c(1,3), ], matrix3[1,], matrixlts[1,], matrixlts[2,], matrix2[3,], matrixlts[5,], matrixlts[6, ])
  total1 <- sum(matrix1[, 1])
  total2 <- sum(matrix1[, 2])
  matrix5 <- c(total1, total2, NA, NA, NA)
  matrix <- rbind(matrix, matrix5)

  rownames(matrix) <- c("Replication","Blocks within Replication", "Crosses",
                        "Lines", "Testers", "Lines X Testers", 'Replication x Hybrids' ,
                        "Error", "Total")

  cm <- matrix[8, 3]
  me = mean(dataset[, "Y"])
  cv = (sqrt(cm)/me) * 100

  ###Testing significance of combining ability effects against rep x hybrids mss

  cm1 = matrix[7, 3]
  s1 <- sqrt(cm1/(r * t))
  s2 <- sqrt(cm1/(r * l))
  s3 <- sqrt(cm1/r)
  s4 <- sqrt(2 * cm1/(r * t))
  s5 <- sqrt(2 * cm1/(r * l))
  s6 <- sqrt(2 * cm1/r)
  ses = c(s1, s2, s3, s4, s5, s6)
  names(ses) = c("S.E. gca for line", "S.E. gca for tester",
                 "S.E. sca effect", "S.E. (gi - gj)line",
                 "S.E. (gi - gj)tester", "S.E. (sij - skl)tester")

  df = matrix[6, 1]
  critc = abs(qt(0.05/2, df))
  se = c(s1, s2, s3, s4, s5, s6)
  cd = se * critc
  names(cd) = c("C.D. gca for line", "C.D. gca for tester",
                "C.D. sca effect", "C.D. (gi - gj)line",
                "C.D. (gi - gj)tester", "C.D. (sij - skl)tester")

  a = (matrix[4,3] - matrix[6,3])/r

  b = (matrix[3,3] - matrix[6,3])/r

  c = (matrix[5,3] - matrix[6,3])/r

  x = (t*(a + c - 2*b) + l*(b +c -2*a) - (((t*t)*(a+c) + l*l*(b + c)- t*l*(a+b)))/2)/(t*l-t*t-l*l)

  y = ((t*(a + c - 2*b) + l*(b +c -2*a))/2)/(t*l - t*t -l*l)

  ls= c(x,y)
  names(ls) = c("GCA variance", "SCA Variance")


  ### Contribution of Lines, Testers and Line x Testers
  c1 <- matrix[3, 2] * 100/matrix[2, 2]
  c2 <- matrix[4, 2] * 100/matrix[2, 2]
  c3 <- matrix[5, 2] * 100/matrix[2, 2]

  cross = matrix
  ltanova = matixlts

  pclt = c(c1, c2, c3)
  names(pclt) = c("Lines", "Tester", " Line x Tester")

  result = list(Means = Means, `Overall ANOVA` = cross,
                `Coefficient of Variation` = cv, `Line x Tester ANOVA` = ltanova,
                `GCA lines` = GCA.lines, `GCA testers` = GCA.testers,
                `SCA crosses` = sca, `Proportional Contribution` = pclt,
                `Critical differance` = cd, `Least Square Estimates`= ls)

  return(result)
  }
  else{
    replication <- deparse(substitute(replication))
    replication <- as.factor(replication)
    line <- deparse(substitute(line))
    line <- as.factor(line)
    tester <- deparse(substitute(tester))
    tester <- as.factor(tester)
    obs = deparse(substitute(obs))
    obs <- as.factor(obs)
    Y <- deparse(substitute(y))

    Y <- deparse(substitute(y))
    message("\nAnalysis of Line x Tester on single plant basis: ", Y, "\n")
    dataset <- cbind.data.frame(Replications = as.factor(data[["replication"]]),
                                Lines = as.factor(data[["line"]]),
                                Testers = as.factor(data[["tester"]]), Obs = as.factor(data[["obs"]]),
                                Y = data[[Y]])
    r = length(levels(dataset$Replications))
    l = length(levels(dataset$Lines))
    t = length(levels(dataset$Testers))
    n = length(levels(dataset$Obs))


    dataset$Treatments <- as.factor(paste0(dataset$Lines, dataset$Testers, dataset$Obs))
    dataset$Hybrids = as.factor(paste0(dataset$Lines, dataset$Testers))

    model1 <- aov(Y ~ Replications + Hybrids, data = dataset)

    matrix1 <- as.matrix(anova(model1))
    matrix1

    model4 <- aov(Y ~ Lines * Testers, data = dataset)
    matrix4 <- as.matrix(anova(model4))
    matrix4

    modellts <- aov(Y ~ Lines + Testers + Replications*Hybrids, data = dataset)
    matrixlts <- as.matrix(anova(modellts))
    matrixlts


    model2 <- aov(Y ~ Lines * Testers, data = dataset)
    matrix2 <- as.matrix(anova(model2))
    matrix2

    modellts <- aov(Y ~ Lines + Testers + Replications*Hybrids, data = dataset)
    matrixlts <- as.matrix(anova(modellts))
    matrixlts

    ### ANOVA for single plant basis

    matixlts = rbind(matrix2[1:3,], matrixlts[5:6,])
    matixlts
    rownames(matixlts) <- c("Line", "Tester",
                            "Line x Tester", "Replication x Hybrid",
                            "Error")

    twt <- tapply(dataset[, 5], dataset[, 2:3], mean, na.rm = TRUE)

    Means <- twt
    testers <- ncol(twt)
    lines <- nrow(twt)


    #sca
    avg <- tapply(dataset[, 5], dataset[, 2:3], mean, na.rm = TRUE)
    sca  = t(t(avg-rowMeans(avg)) - colMeans(avg)) + mean(avg)


    Means1 <- tapply(dataset[, 5], dataset[, 2], mean, na.rm = TRUE)

    GCA.lines = round(Means1 - mean(dataset[, 5], na.rm = TRUE),
                      2)

    Means2 = tapply(dataset[, 5], dataset[, 3], mean, na.rm = TRUE)

    GCA.testers = round(Means2 - mean(dataset[, 5], na.rm = TRUE),
                        2)

    ### Crosses ANOVA
    Crosses <- as.factor(paste(dataset[, 2], dataset[,
                                                     3]))
    model3 <- aov(Y ~ Crosses, data = dataset)
    matrix3 <- as.matrix(anova(model3))

    ### Overall ANOVA
    matrix <- rbind(matrix1[1, ], matrix3[1,], matrixlts[1,], matrixlts[2,], matrix2[3,], matrixlts[5,], matrixlts[6, ])
    total1 <- sum(matrix1[, 1])
    total2 <- sum(matrix1[, 2])
    matrix5 <- c(total1, total2, NA, NA, NA)
    matrix <- rbind(matrix, matrix5)

    rownames(matrix) <- c("Replication","Crosses",
                          "Lines", "Testers", "Lines X Testers", 'Replication x Hybrids' ,
                          "Error", "Total")


    cm <- matrix[7, 3]
    me = mean(dataset[, "Y"])
    cv = (sqrt(cm)/me) * 100


    ###Testing significance of combining ability effects against rep x hybrids mss

    cm1 = matrix[6, 3]
    s1 <- sqrt(cm1/(r * t))
    s2 <- sqrt(cm1/(r * l))
    s3 <- sqrt(cm1/r)
    s4 <- sqrt(2 * cm1/(r * t))
    s5 <- sqrt(2 * cm1/(r * l))
    s6 <- sqrt(2 * cm1/r)
    ses = c(s1, s2, s3, s4, s5, s6)
    names(ses) = c("S.E. gca for line", "S.E. gca for tester",
                   "S.E. sca effect", "S.E. (gi - gj)line",
                   "S.E. (gi - gj)tester", "S.E. (sij - skl)tester")

    df = matrix[6, 1]
    critc = abs(qt(0.05/2, df))
    se = c(s1, s2, s3, s4, s5, s6)
    cd = se * critc
    names(cd) = c("C.D. gca for line", "C.D. gca for tester",
                  "C.D. sca effect", "C.D. (gi - gj)line",
                  "C.D. (gi - gj)tester", "C.D. (sij - skl)tester")

    a = (matrix[4,3] - matrix[6,3])/r

    b = (matrix[3,3] - matrix[6,3])/r

    c = (matrix[5,3] - matrix[6,3])/r

    x = (t*(a + c - 2*b) + l*(b +c -2*a) - (((t*t)*(a+c) + l*l*(b + c)- t*l*(a+b)))/2)/(t*l-t*t-l*l)

    y = ((t*(a + c - 2*b) + l*(b +c -2*a))/2)/(t*l - t*t -l*l)

    ls= c(x,y)
    names(ls) = c("GCA variance", "SCA Variance")


    ### Contribution of Lines, Testers and Line x Testers
    c1 <- matrix[3, 2] * 100/matrix[2, 2]
    c2 <- matrix[4, 2] * 100/matrix[2, 2]
    c3 <- matrix[5, 2] * 100/matrix[2, 2]

    cross = matrix
    ltanova = matixlts

    pclt = c(c1, c2, c3)
    names(pclt) = c("Lines", "Tester", " Line x Tester")

    result = list(Means = Means, `Overall ANOVA` = cross,
                  `Coefficient of Variation` = cv, `Line x Tester ANOVA` = ltanova,
                  `GCA lines` = GCA.lines, `GCA testers` = GCA.testers,
                  `SCA crosses` = sca, `Proportional Contribution` = pclt,
                  `Critical differance` = cd, `Least Square Estimates`= ls)

    return(result)
  }
}

