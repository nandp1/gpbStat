#' @name   ltc
#' @aliases ltc
#'
#' @title Analysis of Line x Tester data containing only Crosses laid out in RCBD or Alpha Lattice design.
#'
#' @param data dataframe containing following variables
#' @param replication replication
#' @param line line
#' @param tester tester
#' @param y trait of interest
#' @param block block (for alpha lattice design only)
#'
#' @note The block variable is inserted at the last if the experimental design is Alpha Lattice. For RCBD no need to have block factor.
#'
#' @return \item{\code{Overall ANOVA}}{ANOVA with all the factors.}\item{\code{Coefficient of Variation}}{ANOVA with all the factors.}\item{\code{Genetic Variance}}{Phenotypic
#' and Genotypic variance for the given trait.}\item{\code{Genetic Variability}}{Phenotypic coefficient of variability and Genotypic coefficient of variability and
#' Environmental coefficient of Variation.}\item{\code{Proportional Contribution}}{Propotional contribution of Lines, Tester and Line x Tester interaction.}\item{\code{GCA lines}}{Combining
#' ability effects of lines.}\item{\code{GCA testers}}{Combining ability effects of testers.}\item{\code{SCA crosses}}{Combining ability effects of crosses}\item{\code{Line x Tester
#' ANOVA}}{ANOVA with all the factors.}\item{\code{GV Singh & Chaudhary}}{Genetic component of Variance as per Singh and Chaudhary, 1977.}\item{\code{Standard Errors}}{Standard error for combining ability effects.}\item{\code{Critical Difference}}{Critical Difference at 5 pecent for combining ability effects.}
#'
#' @author Nandan Patil \email{tryanother609@gmail.com}
#' @details Analyzing the line by tester data only using the data from crosses which are evaluated in alpha lattice design. All the factors are considered as fixed.
#'
#'
#' @references
#' Kempthorne, O. (1957), Introduction to Genetic Statistics. John Wiley and Sons, New York.
#'              , 468-472.
#' Singh, R. K. and Chaudhary, B. D. (1977). Biometrical Methods in Quantitative Genetic Analysis. Kalyani Publishers, New Delhi.
#'
#'@seealso \code{\link[gpbStat]{ltcchk}, \link[gpbStat]{dm2}, \link[gpbStat]{ltcmt}}
#'
#' @import stats
#' @import graphics
#' @export
#'
#' @examples \dontrun{#Line Tester analysis data with only crosses in RCBD
#' library(gpbStat)
#' data(rcbdltc)
#' result1 = ltc(rcbdltc, replication, line, tester, yield)
#' result1
#'
#' #Line Tester analysis data with only crosses in Alpha Lattice
#' library(gpbStat)
#' data(alphaltc)
#' result2 = ltc(alphaltc, replication, line, tester, yield, block)
#' result2
#' }


ltc <-
  function (data, replication, line, tester, y, block)
    {

    if(!missing(block)){

    replication <- deparse(substitute(replication))
    replication <- as.factor(replication)
    block <- deparse(substitute(block))
    block <- as.factor(block)
    line <- deparse(substitute(line))
    line <- as.factor(line)
    tester <- deparse(substitute(tester))
    tester <- as.factor(tester)
    Y <- deparse(substitute(y))
    message("\nAnalysis of Line x Tester: ", Y, "\n")
    dataset <-cbind.data.frame(Replications= as.factor(data[["replication"]]), Blocks = as.factor(data[["block"]]), Lines = as.factor(data[["line"]]), Testers = as.factor(data[["tester"]]), Y = data[[Y]])


    r<- length(levels(dataset[,1]))
    b<- length(levels(dataset[,2]))
    l<- length(levels(dataset[,3]))
    t<- length(levels(dataset[,4]))


    Treatments <-as.factor(paste(dataset[,3],dataset[,4]))
    # ANOVA
    model1<-aov(Y ~ Replications + Blocks:Replications + Treatments, data=dataset)
    matrix1<-as.matrix(anova(model1))

    # Line Tester ANOVA
    model4 <- aov(Y ~ Lines * Testers, data = dataset)
    matrix4 <- as.matrix(anova(model4))   ### LT ANOVA as a matrix
    dataset2 <- na.omit(dataset)

    # Two way Table
    twt <- tapply(dataset2[, 5], dataset2[, 3:4], mean, na.rm = TRUE)  ### converting data to matrix form
    Means <- twt
    testers <- ncol(twt) ## no. of testers
    lines <- nrow(twt)  ## no. of lines

    ## SCA effects estimation
    SCA <- twt
    for (i in 1:lines) {
      for (j in 1:testers) {
        SCA[i, j] <- round(Means[i, j] - mean(Means[, j], na.rm = TRUE) -
                             mean(Means[i, ], na.rm = TRUE) + mean(Means, na.rm = TRUE),
                           3)
      }
    }

    ## Est. of GCA effects of lines
    Means1 <- tapply(dataset2[,5], dataset2[,3],mean,na.rm=TRUE)
    GCA.lines = round(Means1 - mean(dataset2[, 5], na.rm = TRUE), 3)
    ## Est. of GCA effects of testers
    Mean2 = tapply(dataset2[, 5], dataset2[, 4], mean, na.rm = TRUE)
    GCA.testers = round(Mean2 - mean(dataset2[, 5], na.rm = TRUE), 3)

    Crosses <- as.factor(paste(dataset2[, 3], dataset2[, 4]))
    # ANOVA for Crosses only
    model3 <- aov(Y ~ Crosses, data = dataset2)
    matrix3 <- as.matrix(anova(model3))
    dataset3 <- subset(dataset, is.na(dataset[, 3]) | is.na(dataset[, 4]))

    matrix <- rbind(matrix1[1:3, ], matrix3[1, ], matrix4[1:3, ], matrix1[4, ])
    # Total sum Sum of squares
    total1 <- sum(matrix1[, 1])
    total2 <- sum(matrix1[, 2])
    total2 <- sum(matrix1[, 3])
    matrix5 <- c(total1, total2, NA, NA, NA)
    matrix <- rbind(matrix, matrix5)
    matrix <- matrix[-4,]
    rownames(matrix) <- c("Replication", "Crosses", "Blocks within Replication",
                          "Lines", "Testers", "Lines X Testers",
                          "Error", "Total")


    # Picking the Error MSS
    cm <- matrix[7, 3]

    # Calculating C.V.
    me = mean(dataset[,"Y"])
    cv = (sqrt(cm)/me)*100

    # Picking MSS
    rmss = matrix[1, 3]
    cmss = matrix[2, 3]
    lmss = matrix[4, 3]
    tmss = matrix[5, 3]
    ltmss = matrix[6, 3]
    emss = matrix[7, 3]

    # Pheno and Gen Variances
    gv = (cmss - rmss)/r
    pv = gv + emss
    ev = emss
    pgv = c(gv, pv, ev)
    names(pgv) = c("Genotypic Variance", "Phenotypic Variance", "Environmental Variance")

    # coefficient of variation
    pcv = (sqrt(pv)*100)/me
    gcv = (sqrt(gv)*100)/me
    ecv = (sqrt(ev)*100)/me
    bsh = gv/pv
    pgcv = c(pcv, gcv, ecv, bsh)
    names(pgcv) = c("Phenotypic coefficient of Variation", "Genotypic coefficient of Variation", "Environmental coefficient of Variation")

    # Cal. of Standard errors
    s1 <- sqrt(cm/(r * t))
    s2 <- sqrt(cm/(r * l))
    s3 <- sqrt(cm/r)
    s4 <- sqrt(2 * cm/(r * t))
    s5 <- sqrt(2 * cm/(r * l))
    s6 <- sqrt(2 * cm/r)
    # Vector of S.E.
    ses = c(s1, s2, s3, s4, s5, s6)
    names(ses) = c("S.E. gca for line", "S.E. gca for tester", "S.E. sca effect", "S.E. (gi - gj)line",
                   "S.E. (gi - gj)tester",
                   "S.E. (sij - skl)tester")

    ### Cal. of Critical Difference value
    df = matrix[7, 1]
    critc = abs(qt(0.05/2, df))
    se = c(s1, s2, s3, s4, s5, s6)
    cd = se*critc
    names(cd) = c("C.D. gca for line", "C.D. gca for tester", "C.D. sca effect", "C.D. (gi - gj)line",
                  "C.D. (gi - gj)tester",
                  "C.D. (sij - skl)tester")

    #### Estimation of Genetic Component of Variances (singh and Chaudhary, 1979)
    cov1 <- (matrix[4, 3] - matrix[6, 3])/(r * t)
    cov2 <- (matrix[5, 3] - matrix[6, 3])/(r * l)
    cov3 <- (((l - 1) * matrix[4, 3] + (t - 1) * matrix[5, 3])/(l + t - 2) - matrix[6, 3])/(r * (2 * l * t - l - t))
    cov4 <- ((matrix[3, 3] - matrix[6, 3]) + (matrix[4, 3] -
                                                matrix[6, 3]) + (matrix[5, 3] - matrix[6, 3]))/(3 * r) +(6 * r * cov3 - r * (l + t) * cov3)/(3 * r)
    F <- 0
    var.A0 <- cov3 * (4/(1 + F))
    var.D0 <- ((matrix[6, 3] - matrix[7, 3])/r) * (2/(1 + F))
    F <- 1
    var.A1 <- cov3 * (4/(1 + F))
    var.D1 <- ((matrix[5, 3] - matrix[7, 3])/r) * (2/(1 + F))
    c1 <- matrix[4, 2] * 100/matrix[2, 2]
    c2 <- matrix[5, 2] * 100/matrix[2, 2]
    c3 <- matrix[6, 2] * 100/matrix[2, 2]

    # Results

    cross = matrix
    matrix1 <- matrix[4:7, ]
    ltanova = matrix1

    scgv = c(cov1, cov2, cov3, cov4, var.A0, var.A1, var.D0, var.D1)
    names(scgv) = c("Cov H.S. (line)", "Cov H.S. (tester)", "Cov H.S. (average)",
                    "Cov F.S. (average)", "F = 0, Adittive genetic variance",
                    "F = 1, Adittive genetic variance",
                    "F = 0, Variance due to Dominance",
                    "F = 1, Variance due to Dominance")

    pclt  = c(c1, c2, c3)
    names(pclt) = c("Lines", "Tester", " Line x Tester")

    result = list("Means" = Means, "Overall ANOVA" = cross, "Coefficient of Variation" = cv, "Genetic Variance" = pgv, "Genetic Variability "= pgcv,
                  "Line x Tester ANOVA" = ltanova,
                  "GCA lines" = GCA.lines, "GCA testers" = GCA.testers, "SCA crosses" = SCA,
                  "Proportional Contribution" = pclt,
                  "GV Singh & Chaudhary" = scgv,
                  "Standard Errors" = ses, "Critical differance" = cd
    )
    return(result)
    }

    else {
      replication <- deparse(substitute(replication))
      replication <- as.factor(replication)
      line <- deparse(substitute(line))
      line <- as.factor(line)
      tester <- deparse(substitute(tester))
      tester <- as.factor(tester)
      Y <- deparse(substitute(y))
      cat("\nAnalysis of Line x Tester: ", Y, "\n")
      dataset <-data.frame(Replications= as.factor(data[["replication"]]),
                           Lines = as.factor(data[["line"]]), Testers = as.factor(data[["tester"]]), Y = data[[Y]])

      l<- length(levels(dataset[,2]))
      r<- length(levels(dataset[,1]))
      t<- length(levels(dataset[,3]))

      Treatments<-as.factor(paste(dataset[,2],dataset[,3]))

      # ANOVA
      model1<-aov(Y ~ Replications + Treatments,data=dataset)
      matrix1<-as.matrix(anova(model1))

      # Line Tester ANOVA
      model4 <- aov(Y ~ Lines * Testers, data = dataset)
      matrix4 <- as.matrix(anova(model4))

      # Two way Table
      dataset2 <- na.omit(dataset)
      twt <- tapply(dataset2[, 4], dataset2[, 2:3], mean, na.rm = TRUE)  ### converting data to matrix form
      Means <- twt
      testers <- ncol(twt) ## no. of testers
      lines <- nrow(twt)  ## no. of lines

      #### Calculation of SCA effects
      SCA <- twt
      for (i in 1:lines) {
        for (j in 1:testers) {
          SCA[i, j] <- round(twt[i, j] - mean(twt[, j], na.rm = TRUE) -
                               mean(twt[i, ], na.rm = TRUE) + mean(twt, na.rm = TRUE),
                             3)
        }
      }

      ## Est. of GCA effects of lines
      Means1<-tapply(dataset2[,4], dataset2[,2],mean,na.rm=TRUE)
      GCA.lines = round(Means1 - mean(dataset2[, 4], na.rm = TRUE), 3)
      ## Est. of GCA effects of testers
      Means2 = tapply(dataset2[, 4], dataset2[, 3], mean, na.rm = TRUE)
      GCA.testers = round(Means2 - mean(dataset2[, 4], na.rm = TRUE), 3)

      Crosses <- as.factor(paste(dataset2[, 2], dataset2[, 3]))
      # ANOVA for Crosses only
      model3 <- aov(Y ~ Crosses, data = dataset2)
      matrix3 <- as.matrix(anova(model3))
      dataset3 <- subset(dataset, is.na(dataset[, 2]) | is.na(dataset[, 3]))

      matrix <- rbind(matrix1[1:2, ], matrix3[1, ], matrix4[1:3, ], matrix1[3, ])

      # Total sum Sum of squares.
      total1 <- sum(matrix1[, 1])
      total2 <- sum(matrix1[, 2])
      matrix5 <- c(total1, total2, NA, NA, NA)
      matrix <- rbind(matrix, matrix5)
      matrix <- matrix[-3,]
      rownames(matrix) <- c("Replication", "Crosses",
                            "Lines", "Testers", "Lines X Testers",
                            "Error", "Total")

      # Picking the Error MSS
      cm <- matrix[6, 3]

      # Calculating C.V.
      me = mean(dataset[,"Y"])
      cv = (sqrt(cm)/me)*100

      # Picking MSS
      rmss = matrix[1, 3]
      cmss = matrix[2, 3]
      lmss = matrix[3, 3]
      tmss = matrix[4, 3]
      ltmss = matrix[5, 3]
      emss = matrix[6, 3]

      # Pheno and Gen Variances
      gv = (cmss - rmss)/r
      pv = gv + emss
      ev = emss
      pgv = c(gv, pv, ev)
      names(pgv) = c("Genotypic Variance", "Phenotypic Variance", "Environmental Variance")

      # coefficient of variation
      pcv = (sqrt(pv)*100)/me
      gcv = (sqrt(gv)*100)/me
      ecv = (sqrt(ev)*100)/me
      bsh = gv/pv
      pgcv = c(pcv, gcv, ecv, bsh)
      names(pgcv) = c("Phenotypic coefficient of Variation", "Genotypic coefficient of Variation", "Environmental coefficient of Variation")

      # Cal. of Standard errors
      s1 <- sqrt(cm/(r * t))
      s2 <- sqrt(cm/(r * l))
      s3 <- sqrt(cm/r)
      s4 <- sqrt(2 * cm/(r * t))
      s5 <- sqrt(2 * cm/(r * l))
      s6 <- sqrt(2 * cm/r)
      # Vector of S.E.
      ses = c(s1, s2, s3, s4, s5, s6)
      names(ses) = c("S.E. gca for line", "S.E. gca for tester", "S.E. sca effect", "S.E. (gi - gj)line",
                     "S.E. (gi - gj)tester",
                     "S.E. (sij - skl)tester")

      ### Cal. of Critical Difference value
      df = matrix[6, 1]
      critc = abs(qt(0.05/2, df))
      se = c(s1, s2, s3, s4, s5, s6)
      cd = se*critc
      names(cd) = c("C.D. gca for line", "C.D. gca for tester", "C.D. sca effect", "C.D. (gi - gj)line",
                    "C.D. (gi - gj)tester",
                    "C.D. (sij - skl)tester")

      # Estimation of Genetic Component of Variances (singh and Chaudhary, 1979)
      cov1 <- (matrix[3, 3] - matrix[5, 3])/(r * t)
      cov2 <- (matrix[4, 3] - matrix[5, 3])/(r * l)
      cov3 <- (((l - 1) * matrix[3, 3] + (t - 1) * matrix[4, 3])/(l +
                                                                    t - 2) - matrix[5, 3])/(r * (2 * l * t - l - t))
      cov4 <- ((matrix[3, 3] - matrix[6, 3]) + (matrix[4, 3] -
                                                  matrix[6, 3]) + (matrix[5, 3] - matrix[6, 3]))/(3 * r) +(6 * r * cov3 - r * (l + t) * cov3)/(3 * r)
      F <- 0
      var.A0 <- cov3 * (4/(1 + F))
      var.D0 <- ((matrix[5, 3] - matrix[6, 3])/r) * (2/(1 + F))
      F <- 1
      var.A1 <- cov3 * (4/(1 + F))
      var.D1 <- ((matrix[5, 3] - matrix[6, 3])/r) * (2/(1 + F))
      c1 <- matrix[3, 2] * 100/matrix[2, 2]
      c2 <- matrix[4, 2] * 100/matrix[2, 2]
      c3 <- matrix[5, 2] * 100/matrix[2, 2]

      # Results

      cross = matrix
      matrix1 <- matrix[3:6, ]
      ltanova = matrix1

      scgv = c(cov1, cov2, cov3, cov4, var.A0, var.A1, var.D0, var.D1)
      names(scgv) = c("Cov H.S. (line)", "Cov H.S. (tester)", "Cov H.S. (average)",
                      "Cov F.S. (average)", "F = 0, Adittive genetic variance",
                      "F = 1, Adittive genetic variance",
                      "F = 0, Variance due to Dominance",
                      "F = 1, Variance due to Dominance")


      pclt  = c(c1, c2, c3)
      names(pclt) = c("Lines", "Tester", " Line x Tester")

      result = list("Means" = Means, "Overall ANOVA" = cross, "Coefficient of Variation" = cv, "Genetic Variance" = pgv, "Genetic Variability "= pgcv,
                    "Line x Tester ANOVA" = ltanova,
                    "GCA lines" = GCA.lines, "GCA testers" = GCA.testers, "SCA crosses" = SCA,
                    "Proportional Contribution" = pclt,
                    "GV Singh & Chaudhary" = scgv,
                    "Standard Errors" = ses, "Critical differance" = cd)
      return(result)
    }

  }
