#' Compute ideological distance.
#'
#' @param congress Numeric. Session number.
#' @param chamber Character. "house", "senate" or "both".
#' @param state Character. Two character state postal abbreviation (e.g., "CA") or "US" (President). Lower case OK (e.g, "ca").
#' @param data.version Character. DW-NOMINATE flavor: "common", "common.weekly" or "dw".
#' @param dimension Character. "first", "second" or "both".
#' @param wt2D Logical. Weight second dimension.
#' @param mcdonald Logical. Include Larry McDonald (GA-D).
#' @param stump Logical. Include Bob Stump (AZ-D).
#' @param method2D Character. Method of computing 2D distance: "euclidean" or "manhattan".
#' @return An R dataframe
#' @export

ideologicalDistance <- function(congress = 100, chamber = "house",
  state = NULL, data.version = "common", dimension = "both", wt2D = TRUE,
  mcdonald = TRUE, stump = TRUE, method2D = "euclidean") {

  if (!all(chamber %in% c("both", "house", "senate"))) {
    stop('"chamber" must be "house", "senate" or "both".')
  }

  if (!all(dimension %in% c("first", "second", "both"))) {
    stop('"dimension" must be "first", "second" or "both".')
  }

  if (!all(method2D %in% c("euclidean", "manhattan"))) {
    stop('"method2D" must "euclidean" or "manhattan".')
  }

  distanceBetweenMeans <- function(x, y, dim = dimension, wtd = wt2D) {
    if (dim == "first") {
      if (nrow(x) > 0 & nrow(y) > 0) {
        output <- abs(mean(x$dwnom1) - mean(y$dwnom1))
      } else {
        output <- NA
      }
    } else if (dim == "second") {
      if (wtd == TRUE) {
        if (nrow(x) > 0 & nrow(y) > 0) {
          output <- abs(mean(x$d2) - mean(y$d2))
        } else {
          output <- NA
        }
      } else {
        if (nrow(x) > 0 & nrow(y) > 0) {
          output <- abs(mean(x$dwnom2) - mean(y$dwnom2))
        } else {
          output <- NA
        }
      }
    }
    output
  }

  crossIndex <- function(x, y) {
    expand.grid(seq_len(nrow(x)), seq_len(nrow(y)))
  }

  # pairwiseDistance <- function(x, y, wtd = wt2D) {
  #   if (nrow(x) > 0 & nrow(y) > 0) {
  #     idx <- crossIndex(x, y)
  #     if (wtd == TRUE) {
  #       delta.sq <- (x[idx$Var1, c("dwnom1", "d2")] -
  #                    y[idx$Var2, c("dwnom1", "d2")])^2
  #     } else {
  #       delta.sq <- (x[idx$Var1, c("dwnom1", "dwnom2")] -
  #                    y[idx$Var2, c("dwnom1", "dwnom2")])^2
  #     }
  #     mean(sqrt(rowSums(delta.sq)))
  #   } else {
  #     NA
  #   }
  # }

  pairwiseDistance <- function(x, y, wtd = wt2D, method = method2D) {
    if (nrow(x) > 0 & nrow(y) > 0) {
      idx <- crossIndex(x, y)
      if (method == "euclidean") {
        if (wtd == TRUE) {
          delta.sq <- (x[idx$Var1, c("dwnom1", "d2")] -
                       y[idx$Var2, c("dwnom1", "d2")])^2
        } else {
          delta.sq <- (x[idx$Var1, c("dwnom1", "dwnom2")] -
                       y[idx$Var2, c("dwnom1", "dwnom2")])^2
        }
        mean(sqrt(rowSums(delta.sq)))
      } else if (method == "manhattan") {
        if (wtd == TRUE) {
          delta.abs <- abs(x[idx$Var1, c("dwnom1", "d2")] -
                           y[idx$Var2, c("dwnom1", "d2")])
        } else {
          delta.abs <- abs(x[idx$Var1, c("dwnom1", "dwnom2")] -
                           y[idx$Var2, c("dwnom1", "dwnom2")])
        }
        mean(sqrt(rowSums(delta.abs)))
      }
    } else {
     NA
    }
  }

  ## Data ##

  if (data.version == "common.weekly") {
    wt <- 0.4153
    # URL <- "ftp://k7moa.com/wf1/Weekly_DW-NOMINATE_31_12.DTA"
    # ideal.points <- foreign::read.dta(URL)
    ideal.points <- vip::ideal.points.wk31
    y1 <- seq(1857, 2015, 2)
  } else if (data.version == "common") {
    wt <- 0.4113
    # URL <- "ftp://k7moa.com/junkord/HANDSL01113C20_BSSE_12.DTA"
    # ideal.points <- foreign::read.dta(URL)
    ideal.points <- vip::ideal.points.r5
    y1 <- seq(1857, 2013, 2)
  } else if (data.version == "dw") {
    wt <- 0.3988
    # URL <- "ftp://k7moa.com/junkord/HL01113D21_BSSE_12.DTA"
    # ideal.points <- foreign::read.dta(URL)
    # URL <- "ftp://k7moa.com/junkord/SL01113D21_BSSE_12.DTA"
    # ideal.points.senate <- foreign::read.dta(URL)
    # ideal.points <- rbind(ideal.points, ideal.points.senate)
    ideal.points <- vip::ideal.points.dw
    y1 <- seq(1857, 2013, 2)
  }

  congressID <- unique(ideal.points$cong)
  id2 <- congressID[35:length(congressID)]
  y2 <- y1 + 2
  yrs <- paste0(y1, "-", y2)

  state.codes <- vip::stateCodes()

  ideal.points$statenm <- trimws(ideal.points$statenm)

  if (is.null(state) == FALSE) {
    st <- state.codes[state.codes$state.abb == state, "statenm"]
    ideal.points <- ideal.points[ideal.points$statenm == st, ]
  }

  if (chamber == "senate") {
    ideal.points <- ideal.points[ideal.points$cd == 0, ]
    ideal.points <- ideal.points[ideal.points$statenm != "USA", ]
  } else if (chamber == "house") {
    ideal.points <- ideal.points[ideal.points$cd != 0, ]
    if (mcdonald == FALSE) {
      ideal.points <- ideal.points[ideal.points$idno != 14252, ]
    }
    if (stump == FALSE) {
      ideal.points <- ideal.points[ideal.points$idno != 94454, ]
    }
  } else if (chamber == "both") {
    if (mcdonald == FALSE) {
      ideal.points <- ideal.points[ideal.points$idno != 14252, ]
    }
    if (stump == FALSE) {
      ideal.points <- ideal.points[ideal.points$idno != 94454, ]
    }
    # all, including President otherwise uncomment:
    # ideal.points <- ideal.points[ideal.points$statenm != "USA", ]
  }

  dat <- ideal.points[ideal.points$cong == congress, ]
  dat$d2 <- wt * dat$dwnom2
  dat$party2 <- NA
  dat[dat$party == 100, "party2"] <- "D"
  dat[dat$party == 200, "party2"] <- "R"
  D <- dat[dat$party == 100, ]
  R <- dat[dat$party == 200, ]

  south <- state.codes[state.codes$south != 0, "statenm"]
  DS <- D[D$statenm %in% south, ]
  DN <- D[D$statenm %in% south == FALSE, ]
  RS <- R[R$statenm %in% south, ]
  RN <- R[R$statenm %in% south == FALSE, ]

  if (dimension != "both") {
    data.frame(congress = congress,
               DR = distanceBetweenMeans(D, R),
               D.RN = distanceBetweenMeans(D, RN),
               D.RS = distanceBetweenMeans(D, RS),
               DN.R = distanceBetweenMeans(DN, R),
               DS.R = distanceBetweenMeans(DS, R),
               DN.DS = distanceBetweenMeans(DN, DS),
               RN.RS = distanceBetweenMeans(RN, RS),
               DN.RN = distanceBetweenMeans(DN, RN),
               DS.RN = distanceBetweenMeans(DN, RS),
               DN.RS = distanceBetweenMeans(DS, RN),
               DS.RS = distanceBetweenMeans(DS, RS))
  } else {
    data.frame(congress = congress,
               DR = pairwiseDistance(D, R, method = method2D),
               D.RN = pairwiseDistance(D, RN, method = method2D),
               D.RS = pairwiseDistance(D, RS, method = method2D),
               DN.R = pairwiseDistance(DN, R, method = method2D),
               DS.R = pairwiseDistance(DS, R, method = method2D),
               DN.DS = pairwiseDistance(DN, DS, method = method2D),
               RN.RS = pairwiseDistance(RN, RS, method = method2D),
               DN.RN = pairwiseDistance(DN, RN, method = method2D),
               DS.RN = pairwiseDistance(DN, RS, method = method2D),
               DN.RS = pairwiseDistance(DS, RN, method = method2D),
               DS.RS = pairwiseDistance(DS, RS, method = method2D))
  }
}
