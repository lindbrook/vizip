#' Compute ideological dispersion.
#'
#' @param congress Numeric. Session number.
#' @param chamber Character. "house", "senate" or "both".
#' @param state Character. Two character state postal abbreviation (e.g., "CA") or "US" (President). Lower case OK (e.g, "ca").
#' @param data.version Character. DW-NOMINATE flavor: "common", "common.weekly" or "dw".
#' @param dimension Character. "first", "second" or "both".
#' @param wt2D Logical. Weight second dimension.
#' @param mcdonald Logical. Include Larry McDonald (GA-D).
#' @param stump Logical. Include Bob Stump (AZ-D).
#' @return An R dataframe
#' @export

ideologicalDispersion <- function(congress = 100, chamber = "house",
  state = NULL, data.version = "common", dimension = "both", wt2D = TRUE,
  mcdonald = TRUE, stump = TRUE) {

  if (!all(chamber %in% c("both", "house", "senate"))) {
    stop('"chamber" must be "house", "senate" or "both".')
  }

  if (!all(data.version %in% c("common", "common.weekly", "dw"))) {
    stop('"data.version" must be "common", "common.weekly" or "dw".')
  }

  if (!all(dimension %in% c("first", "second", "both"))) {
    stop('"dimension" must be "first", "second" or "both".')
  }

  index <- function(x) as.data.frame(t(utils::combn(nrow(x), 2)))

  pairwiseDispersion <- function(x, dim = dimension, wtd = wt2D) {
    if (dim == "first") {
      if (nrow(x) > 1) {
        idx <- index(x)
        output <- mean(abs(x[idx$V1, "dwnom1"] - x[idx$V2, "dwnom1"]))
      } else if (nrow(x) == 1) {
        output <- 0
      } else {
        output <- NA
      }
    } else if (dim == "second") {
      if (nrow(x) > 1) {
        idx <- index(x)
        if (wtd == TRUE) {
          output <- mean(abs(x[idx$V1, "d2"] - x[idx$V2, "d2"]))
        } else {
          output <- mean(abs(x[idx$V1, "dwnom2"] - x[idx$V2, "dwnom2"]))
        }
      } else if (nrow(x) == 1) {
        output <- 0
      } else {
        output <- NA
      }
    } else if (dim == "both") {
        if (nrow(x) > 1) {
          idx <- index(x)
          if (wtd == TRUE) {
            delta.sq <- (x[idx$V1, c("dwnom1", "d2")] -
                         x[idx$V2, c("dwnom1", "d2")])^2
          } else {
            delta.sq <- (x[idx$V1, c("dwnom1", "dwnom2")] -
                         x[idx$V2, c("dwnom1", "dwnom2")])^2
          }

          output <- mean(sqrt(rowSums(delta.sq)))
        } else if (nrow(x) == 1) {
          output <- 0
        } else {
          output <- NA
        }
      }
      output
    }

  stdDev <- function(x, dim = dimension, wtd = wt2D) {
    if (dim == "first") {
      if (nrow(x) > 1) {
        output <- stats::sd(x$dwnom1)
      } else if (nrow(x) == 1) {
        output <- 0
      } else {
        output <- NA
      }
    } else if (dim == "second") {
       if (nrow(x) > 1) {
        if (wtd == TRUE) {
          output <- stats::sd(x$d2)
        } else {
          output <- stats::sd(x$dwnom2)
        }
      } else if (nrow(x) == 1) {
        output <- 0
      } else {
        output <- NA
      }
    } else if (dim == "both") {
      if (nrow(x) > 1) {
       if (wtd == TRUE) {
         output <- sqrt(mean((x$dwnom1 - mean(x$dwnom1))^2 +
                             (x$d2 - mean(x$d2))^2))
       } else {
         output <- sqrt(mean((x$dwnom1 - mean(x$dwnom1))^2 +
                             (x$dwnom2 - mean(x$dwnom2))^2))
       }
     } else if (nrow(x) == 1) {
       output <- 0
     } else {
       output <- NA
     }
    }
    output
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

  pairs <- vapply(list(D, R, DN, DS, RN, RS), pairwiseDispersion, numeric(1L))
  sds <- vapply(list(D, R, DN, DS, RN, RS), stdDev, numeric(1L))
  nm <- c("D", "R", "DN", "DS", "RN", "RS")
  out <- stats::setNames(data.frame(t(c(congress, sds, pairs))),
    c("congress", paste0(nm, ".sd"), nm))
  data.frame(year = y1[id2 %in% congress], out)
}
