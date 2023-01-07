#' @name ltcchk
#' @aliases ltcchk
#'
#' @title Analysis of Line x Tester data containing crosses and checks laid out in RCBD or Alpha Lattice experimental design.
#'
#' @param data dataframe containing following variables
#' @param replication replication variable
#' @param line line variable
#' @param tester tester variable
#' @param check check variable
#' @param y trait of interest
#' @param block block variable (for alpha lattice design only)
#'
#' @note The block variable is inserted at the last if the experimental design is Alpha Lattice. For RCBD no need to have block factor.
#'
#' @return \item{\code{Overall ANOVA}}{ANOVA with all the factors.}\item{\code{Coefficient of Variation}}{ANOVA with all the factors.}\item{\code{Genetic Variance}}{Phenotypic
#' and Genotypic variance for the given trait.}\item{\code{Genetic Variability}}{Phenotypic coefficient of variability and Genotypic coefficient of variability and
#' Environmental coefficient of Variation.}\item{\code{Proportional Contribution}}{Propotional contribution of Lines, Tester and Line x Tester interaction.}\item{\code{GCA lines}}{Combining
#' ability effects of lines.}\item{\code{GCA testers}}{Combining ability effects of testers.}\item{\code{SCA crosses}}{Combining ability effects of crosses}\item{\code{Line x Tester
#' ANOVA}}{ANOVA with all the factors.}\item{\code{GV Singh & Chaudhary}}{Genetic component of Variance as per Singh and Chaudhary, 1977.}\item{\code{Standard Errors}}{Standard error for combining ability effects.}\item{\code{Critical Difference}}{Critical Difference at 5 percent for combining ability effects.}
#'
#' @author Nandan Patil
#'
#' @details Analyzing the line by tester data only using the data from crosses which are evaluated in alpha lattice design. All the factors are considered as fixed.
#'
#' @author Nandan Patil \email{tryanother609@gmail.com}
#' @details Analyzing the line by tester data only using the data from crosses which are evaluated in alpha lattice design.
#'All the factors are considered as fixed.
#'
#' @references
#' Kempthorne, O. (1957), Introduction to Genetic Statistics. John Wiley and Sons, New York.
#'              , 468-472.
#' Singh, R. K. and Chaudhary, B. D. (1977). Biometrical Methods in Quantitative Genetic Analysis. Kalyani Publishers, New Delhi.
#'
#'@seealso \code{\link[gpbStat]{ltc}, \link[gpbStat]{dm2}, \link[gpbStat]{ltcmt}}
#'
#'
#' @import stats
#' @import graphics
#' @export
#'
#' @examples \dontrun{#Line x Tester analysis with crosses and checks in RCBD
#'library(gpbStat)
#'data(rcbdltcchk)
#'results = ltcchk(rcbdltcchk, replication, line, tester, check, yield)
#'results
#'
#'#Line X Tester analysis with crosses and checks in Alpha Lattice
#'library(gpbStat)
#'data(alphaltcchk)
#'results1 = ltcchk(alphaltcchk, replication, line, tester, check, yield, block)
#'results1}


ltcchk <-
  function (data, replication, line, tester, check, y, block)
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
      check <- deparse(substitute(check))
      check <- as.factor(check)
      Y <- deparse(substitute(y))
      message("\nAnalysis of Line x Tester: ", Y, "\n")
      dataset <- data.frame(Replications= as.factor(data[["replication"]]), Blocks = as.factor(data[["block"]]), Lines = as.factor(data[["line"]]), Testers = as.factor(data[["tester"]]), Checks = as.factor(data[["check"]]), Y = data[[Y]])

      r<- length(levels(dataset[,1]))
      b<- length(levels(dataset[,2]))
      l<- length(levels(dataset[,3]))
      t<- length(levels(dataset[,4]))
      c<- length(levels(dataset[,5]))

      Treatments <-as.factor(paste(dataset[,3],dataset[,4], dataset[,5]))
      # ANOVA
      model1<-aov(Y ~ Replications + Blocks:Replications + Treatments, data=dataset)
      matrix1<-as.matrix(anova(model1))

      # Line Tester ANOVA
      model4 <- aov(Y ~ Lines * Testers, data = dataset)
      matrix4 <- as.matrix(anova(model4))   ### LT ANOVA as a matrix
      dataset22 = dataset[,-c(2, 5)]
      dataset2 <- na.omit(dataset22)

      # Two way Table
      twt <- tapply(dataset2[, 4], dataset2[, 2:3], mean, na.rm = TRUE)  ### converting data to matrix form
      Means <- twt
      testers <- ncol(twt) ## no. of testers
      lines <- nrow(twt)  ## no. of lines

      #### Calculation of SCA effects
      SCA <- twt
      for (i in 1:lines) {
        for (j in 1:testers) {
          SCA[i, j] <- round(Means[i, j] - mean(Means[, j], na.rm = TRUE) -
                               mean(Means[i, ], na.rm = TRUE) + mean(Means, na.rm = TRUE),
                             3)
        }
      }

      ## Est. of GCA effects of lines
      Means1 <- tapply(dataset2[,4], dataset2[,2],mean,na.rm=TRUE)
      GCA.lines = round(Means1 - mean(dataset2[, 4], na.rm = TRUE), 3)
      ## Est. of GCA effects of testers
      Mean2 = tapply(dataset2[, 4], dataset2[, 3], mean, na.rm = TRUE)
      GCA.testers = round(Mean2 - mean(dataset2[, 4], na.rm = TRUE), 3)

      ## Only checks
      datachk <- subset(dataset, is.na(dataset[, 3]) | is.na(dataset[, 4]))

      ## ANVOA checks
      modelchk = aov(Y ~ Checks + Replications, datachk)
      matrixchk <- as.matrix(anova(modelchk))

      ## Only crosses
      datoscrs <- subset(dataset, is.na(dataset[, 5]))
      datoscrs$cross <- paste(datoscrs[["Lines"]], datoscrs[["Testers"]],
                              sep = "-")
      # ANVOA cross
      modelcross = aov(Y ~ cross + Replications, datoscrs)
      matrixcross <- as.matrix(anova(modelcross))

      matrix <- rbind(matrix1[c(1,3,2), ], matrixcross[1,], matrixchk[1, ], matrix4[1:3, ], matrix1[4, ])

      # Total sum Sum of squares
      total1 <- sum(matrix1[, 1])
      total2 <- sum(matrix1[, 2])
      total2 <- sum(matrix1[, 3])
      matrix5 <- c(total1, total2, NA, NA, NA)
      matrix <- rbind(matrix, matrix5)

      rownames(matrix) <- c("Replication", "Blocks within Replication", "Treatments",
                            "Crosses", "Checks",
                            "Lines", "Testers", "Lines X Testers",
                            "Error", "Total")

      for (i in 1:10) {
        matrix[i, 3] <- matrix[i, 2]/matrix[i, 1]
        matrix[i, 4] <- round(matrix[i, 3]/matrix[9, 3], 3) ## Est. Cal. F value
        matrix[i, 5] <- round(1 - pf(matrix[i, 4], matrix[i, 1], matrix[9, 1]), 4)

        if (i == 6 | i == 7) {   ### to define the f value of Line and Tester factors
          matrix[i, 4] <- round(matrix[i, 3]/matrix[8, 3],
                                3)
          matrix[i, 5] <- round(1 - pf(matrix[i, 4], matrix[i,1], matrix[8, 1]), 4)
        }
      }

      matrix[9, 4] <- NA   ### for putting NA for error factor
      matrix[9, 5] <- NA   ### for putting NA for error factor
      matrix[10, 3]<- NA
      matrix[10, 4]<- NA
      matrix[10, 5]<- NA

      # Picking the Error MSS
      cm <- matrix[9, 3]

      # Calculating C.V.
      me = mean(dataset[,"Y"])
      cv = (sqrt(cm)/me)*100

      # Picking MSS
      rmss = matrix[1, 3]
      cmss = matrix[3, 3]
      lmss = matrix[6, 3]
      tmss = matrix[7, 3]
      ltmss = matrix[8, 3]
      emss = matrix[9, 3]

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
      df = matrix[9, 1]
      critc = abs(qt(0.05/2, df))
      se = c(s1, s2, s3, s4, s5, s6)
      cd = se*critc
      names(cd) = c("C.D. gca for line", "C.D. gca for tester", "C.D. sca effect", "C.D. (gi - gj)line",
                    "C.D. (gi - gj)tester",
                    "C.D. (sij - skl)tester")

      #### Estimation of co variances
      cov1 <- (matrix[6, 3] - matrix[8, 3])/(r * t)
      cov2 <- (matrix[7, 3] - matrix[8, 3])/(r * l)
      cov3 <- (((l - 1) * matrix[6, 3] + (t - 1) * matrix[7, 3])/(l + t - 2) - matrix[8, 3])/(r * (2 * l * t - l - t))
      cov4 <- ((matrix[6, 3] - matrix[9, 3]) + (matrix[7, 3] -
                                                  matrix[9, 3]) + (matrix[8, 3] - matrix[9, 3]))/(3 * r) +(6 * r * cov3 - r * (l + t) * cov3)/(3 * r)
      F <- 0
      var.A0 <- cov3 * (4/(1 + F))
      var.D0 <- ((matrix[8, 3] - matrix[9, 3])/r) * (2/(1 + F))
      F <- 1
      var.A1 <- cov3 * (4/(1 + F))
      var.D1 <- ((matrix[8, 3] - matrix[9, 3])/r) * (2/(1 + F))
      c1 <- matrix[6, 2] * 100/matrix[4, 2]
      c2 <- matrix[7, 2] * 100/matrix[4, 2]
      c3 <- matrix[8, 2] * 100/matrix[4, 2]


      # Overall ANOVA Results
      overall = matrix

      # LT ANOVA
      matrix1 <- matrix[6:8, ]
      ltanova = matrix1
      scgv = c(cov1, cov2, cov3, cov4, var.A0, var.A1, var.D0, var.D1)
      names(scgv) = c("Cov H.S. (line)", "Cov H.S. (tester)", "Cov H.S. (average)",
                      "Cov F.S. (average)", "F = 0, Adittive genetic variance",
                      "F = 1, Adittive genetic variance",
                      "F = 0, Variance due to Dominance",
                      "F = 1, Variance due to Dominance")


      pclt  = c(c1, c2, c3)
      names(pclt) = c("Lines", "Tester", " Line x Tester")

      result = list("Means" = Means, "Overall ANOVA" = matrix, "Coefficient of Variation" = cv, "Genetic Variance" = pgv, "Genetic Variability "= pgcv,
                    "Line x Tester ANOVA" = ltanova,
                    "GCA lines" = GCA.lines, "GCA testers" = GCA.testers, "SCA crosses" = SCA,
                    "Proportional Contribution" = pclt,
                    "GV Singh & Chaudhary" = scgv,
                    "Standard Errors" = ses, "Critical differance" = cd)

      return(result)
    }

    else {
      replication <- deparse(substitute(replication))
      replication <- as.factor(replication)
      line <- deparse(substitute(line))
      line <- as.factor(line)
      tester <- deparse(substitute(tester))
      tester <- as.factor(tester)
      check <- deparse(substitute(check))
      check <- as.factor(check)
      Y <- deparse(substitute(y))
      cat("\nAnalysis of Line x Tester with crosses and checks: ", Y, "\n")
      dataset <-data.frame(Replications= as.factor(data[["replication"]]),
                           Lines = as.factor(data[["line"]]), Testers = as.factor(data[["tester"]]), Checks = as.factor(data[["check"]]), Y = data[[Y]])

      l<- length(levels(dataset[,2]))
      r<- length(levels(dataset[,1]))
      t<- length(levels(dataset[,3]))
      c<- length(levels(dataset[,4]))
      Treatments<-as.factor(paste(dataset[,2],dataset[,3], dataset[,4]))

      # ANOVA
      model1<-aov(Y ~ Replications + Treatments, data=dataset)
      matrix1<-as.matrix(anova(model1))

      # Line Tester ANOVA
      model4 <- aov(Y ~ Lines * Testers, data = dataset)
      matrix4 <- as.matrix(anova(model4))

      # Two way Table
      dataset22 <- (dataset[,-4])
      dataset2 <- na.omit(dataset22)
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
      Means1 <- tapply(dataset2[,4], dataset2[,2],mean,na.rm=TRUE)
      GCA.lines = round(Means1 - mean(dataset2[, 4], na.rm = TRUE), 3)
      ## Est. of GCA effects of testers
      Means2 = tapply(dataset2[, 4], dataset2[, 3], mean, na.rm = TRUE)
      GCA.testers = round(Means2 - mean(dataset2[, 4], na.rm = TRUE), 3)

      ## Only checks
      datachk <- subset(dataset, is.na(dataset[, 2]) | is.na(dataset[, 3]))

      ## ANVOVA checks
      modelchk = aov(Y ~ Checks + Replications, datachk)
      matrixchk <- as.matrix(anova(modelchk))

      ## Only crosses
      datoscrs <- subset(dataset, is.na(dataset[, 4]))
      datoscrs$cross <- paste(datoscrs[["Lines"]], datoscrs[["Testers"]],
                              sep = "-")
      # ANVOVA cross
      modelcross = aov(Y ~ cross + Replications, datoscrs)
      matrixcross <- as.matrix(anova(modelcross))

      matrix <- rbind(matrix1[1:2, ], matrixcross[1, ], matrixchk[1, ], matrix4[1:3,], matrix1[3, ])

      # Total sum Sum of squares.
      total1 <- sum(matrix1[, 1])
      total2 <- sum(matrix1[, 2])
      matrix5 <- c(total1, total2, NA, NA, NA)
      matrix <- rbind(matrix, matrix5)
      rownames(matrix) <- c("Replication", "Treatments", "Crosses", "Checks",
                            "Lines", "Testers", "Lines X Testers",
                            "Error", "Total")

      for (i in 1:9) {
        matrix[i, 3] <- matrix[i, 2]/matrix[i, 1]
        matrix[i, 4] <- round(matrix[i, 3]/matrix[8, 3], 3) ## Est. Cal. F value
        matrix[i, 5] <- round(1 - pf(matrix[i, 4], matrix[i, 1], matrix[8, 1]), 4)

        if (i == 5 | i == 6) {   ### to define the f value of Line and Tester factors
          matrix[i, 4] <- round(matrix[i, 3]/matrix[7, 3],
                                3)
          matrix[i, 5] <- round(1 - pf(matrix[i, 4], matrix[i,1], matrix[7, 1]), 4)
        }
      }

      matrix[8, 4] <- NA   ### for putting NA for error factor
      matrix[8, 5] <- NA   ### for putting NA for error factor
      matrix[9, 3]<- NA
      matrix[9, 4]<- NA
      matrix[9, 5]<- NA

      # Picking the Error MSS
      cm <- matrix[8, 3]

      # Calculating C.V.
      me = mean(dataset[,"Y"])
      cv = (sqrt(cm)/me)*100

      # Picking MSS
      rmss = matrix[1, 3]
      cmss = matrix[2, 3]
      lmss = matrix[5, 3]
      tmss = matrix[6, 3]
      ltmss = matrix[7, 3]
      emss = matrix[8, 3]

      # Pheno and Gen Variances
      gv = (cmss - rmss)/r
      pv = gv + emss
      ev = emss
      pgv = c(gv, pv, ev)
      names(pgv) = c("Genotypic Variance", "Phenotypic Variance", "Environmental Variance")

      # Coefficient of Variation
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
      df = matrix[8, 1]
      critc = abs(qt(0.05/2, df))
      se = c(s1, s2, s3, s4, s5, s6)
      cd = se*critc
      names(cd) = c("C.D. gca for line", "C.D. gca for tester", "C.D. sca effect", "C.D. (gi - gj)line",
                    "C.D. (gi - gj)tester",
                    "C.D. (sij - skl)tester")

      # Estimation of co variances
      cov1 <- (matrix[5, 3] - matrix[7, 3])/(r * t)
      cov2 <- (matrix[6, 3] - matrix[7, 3])/(r * l)
      cov3 <- (((l - 1) * matrix[5, 3] + (t - 1) * matrix[6, 3])/(l +
                                                                    t - 2) - matrix[7, 3])/(r * (2 * l * t - l - t))
      cov4 <- ((matrix[5, 3] - matrix[8, 3]) + (matrix[6, 3] -
                                                  matrix[8, 3]) + (matrix[7, 3] - matrix[8, 3]))/(3 * r) +(6 * r * cov3 - r * (l + t) * cov3)/(3 * r)
      F <- 0
      var.A0 <- cov3 * (4/(1 + F))
      var.D0 <- ((matrix[7, 3] - matrix[8, 3])/r) * (2/(1 + F))
      F <- 1
      var.A1 <- cov3 * (4/(1 + F))
      var.D1 <- ((matrix[7, 3] - matrix[8, 3])/r) * (2/(1 + F))
      c1 <- matrix[5, 2] * 100/matrix[3, 2]
      c2 <- matrix[6, 2] * 100/matrix[3, 2]
      c3 <- matrix[7, 2] * 100/matrix[3, 2]

      # Overall anova
      matrix1 <- matrix[c(1:9), ]
      overall = matrix1

      # LT anova
      matrix1 <- matrix[c(5:7), ]
      ltanova = matrix1

      scgv = c(cov1, cov2, cov3, cov4, var.A0, var.A1, var.D0, var.D1)
      names(scgv) = c("Cov H.S. (line)", "Cov H.S. (tester)", "Cov H.S. (average)",
                      "Cov F.S. (average)", "F = 0, Adittive genetic variance",
                      "F = 1, Adittive genetic variance",
                      "F = 0, Variance due to Dominance",
                      "F = 1, Variance due to Dominance")

      pclt  = c(c1, c2, c3)
      names(pclt) = c("Lines", "Tester", " Line x Tester")

      result = list("Means" = Means, "Overall ANOVA" = matrix, "Coefficient of Variation" = cv, "Genetic Variance" = pgv, "Genetic Variability "= pgcv,
                    "Line x Tester ANOVA" = ltanova,
                    "GCA lines" = GCA.lines, "GCA testers" = GCA.testers, "SCA crosses" = SCA,
                    "Proportional Contribution" = pclt,
                    "GV Singh & Chaudhary" = scgv,
                    "Standard Errors" = ses, "Critical differance" = cd)

      return(result)
  }

   }
