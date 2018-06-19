#' Compute ideological overlap.
#'
#' @param congress Numeric. Session number.
#' @param chamber Character.
#' @param state Character. Two character state postal abbreviation (e.g., "CA") or "US" (President). Lower case OK (e.g, "ca").
#' @param data.version Character. DW-NOMINATE flavor: "common", "common.weekly" or "dw".
#' @param mcdonald Logical. Include Larry McDonald (GA-D).
#' @param stump Logical. Include Bob Stump (AZ-D).
#' @param wt2D Logical. Weight second dimension. Default is FALSE.
#' @param overlap.trim Numeric. Sets the smaller quantile to trim the range of scores. Must be on [0, 0.5). "0" uses minimum Republican score and maximum Democrat score.
#' @return An R dataframe
#' @export

ideologicalOverlap <- function(congress = 100, chamber = "house", state = NULL,
  data.version = "common", mcdonald = TRUE, stump = TRUE, wt2D = FALSE,
  overlap.trim = 0) {

  if (!all(chamber %in% c("both", "house", "senate"))) {
    stop('"chamber" must be "house", "senate" or "both".')
  }

  if ((overlap.trim >= 0 & overlap.trim < 0.5) == FALSE) {
    stop('"overlap.trim" must be between 0 and 0.5.')
  }

  pcaLine <- function(party = "D", weighted = wt2D) {
    if (party == "D") {
      x <- get("D")
      color <- "blue"
    } else if (party == "R") {
      x <- get("R")
      color <- "red"
    }

    if (weighted == TRUE) {
      fmla <- stats::formula(~ dwnom1 + d2)
    } else {
      fmla <- stats::formula(~ dwnom1 + dwnom2)
    }

    v <- stats::prcomp(fmla, data = x)$rotation
    slope <- -v[-ncol(v), ncol(v)] / v[ncol(v), ncol(v)]

    if (weighted == TRUE) {
      intercept <- mean(x$d2) - slope * mean(x$dwnom1)
    } else {
      intercept <- mean(x$dwnom2) - slope * mean(x$dwnom1)
    }

    stats::setNames(c(intercept, slope), c("intercept", "slope"))
  }

  angleBisector <- function() {
    d.pcaLine <- pcaLine()
    r.pcaLine <- pcaLine("R")

    x.coord <- (r.pcaLine["intercept"] - d.pcaLine["intercept"]) /
               (d.pcaLine["slope"] - r.pcaLine["slope"])

    y.coord <- d.pcaLine["slope"] * x.coord + d.pcaLine["intercept"]

    slope.product <- sign(d.pcaLine["slope"] * r.pcaLine["slope"])

    if (slope.product == 1) {
      bisector.radian <- 0.5 * (atan(d.pcaLine["slope"]) -
                                atan(r.pcaLine["slope"]))
      bisector.slope <- tan(atan(bisector.radian) + atan(r.pcaLine["slope"]))
    } else if (slope.product == -1 & sign(d.pcaLine["slope"]) == -1) {
      d.radians <- pi / 2 + (pi / 2 + atan(d.pcaLine["slope"]))
      delta <- 0.5 * abs(d.radians - atan(r.pcaLine["slope"]))
      bisector.radian <- pmin(d.radians, atan(r.pcaLine["slope"])) + delta
      bisector.slope <- tan(bisector.radian)
    } else if (slope.product == -1 & sign(r.pcaLine["slope"]) == -1) {
      r.radians <- pi / 2 + (pi / 2 + atan(r.pcaLine["slope"]))
      delta <- 0.5 * abs(atan(d.pcaLine["slope"]) - r.radians)
      bisector.radian <- pmin(atan(d.pcaLine["slope"]), r.radians) + delta
      bisector.slope <- tan(bisector.radian)
    }

    bisector.intercept <- y.coord - bisector.slope * x.coord
    stats::setNames(c(bisector.intercept, bisector.slope),
      c("intercept", "slope"))
  }

  orthogonalCoordinates <- function(dat = D, weighted = wt2D) {
    ortho.coords <- lapply(seq_len(nrow(dat)), function(i) {
      if (weighted == TRUE) {
        obs <- dat[i, c("dwnom1", "d2")]
        obs.intercept <- obs$d2 - slope * obs$dwnom1
      } else {
        obs <- dat[i, c("dwnom1", "dwnom2")]
        obs.intercept <- obs$dwnom2 - slope * obs$dwnom1
      }
      x.proj <- obs.intercept / (ortho.slope - slope)
      y.proj <- ortho.slope * x.proj
      unname(c(x.proj, y.proj))
    })

    stats::setNames(data.frame(do.call(rbind, ortho.coords)), c("x", "y"))
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
    st <- state.codes[state.codes$state.abb == toupper(state), "statenm"]
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
    # all, including President
    # ideal.points <- ideal.points[ideal.points$statenm != "USA", ]
  }

  dat <- ideal.points[ideal.points$cong == congress, ]
  dat$d2 <- wt * dat$dwnom2
  dat$party2 <- NA
  dat[dat$party == 100, "party2"] <- "D"
  dat[dat$party == 200, "party2"] <- "R"
  D <- dat[dat$party == 100, ]
  R <- dat[dat$party == 200, ]

  if (overlap.trim != 0) {
    d.quant <- stats::quantile(D$dwnom1, c(overlap.trim, 1 - overlap.trim))
    r.quant <- stats::quantile(R$dwnom1, c(overlap.trim, 1 - overlap.trim))
    d.trim <- stats::setNames(d.quant, c("lo", "hi"))
    r.trim <- stats::setNames(r.quant, c("lo", "hi"))
    D <- D[D$dwnom1 >= d.trim["lo"] & D$dwnom1 <= d.trim["hi"], ]
    R <- R[R$dwnom1 >= r.trim["lo"] & R$dwnom1 <= r.trim["hi"], ]
  }

  south <- state.codes[state.codes$south != 0, "statenm"]
  DS <- D$statenm %in% south
  DN <- D$statenm %in% south == FALSE
  RS <- R$statenm %in% south
  RN <- R$statenm %in% south == FALSE

  # Overlap in First Dimension
  overlap1 <- stats::setNames(c(min(R$dwnom1), max(D$dwnom1)), c("min", "max"))
  intersect1.D <- D$dwnom1 >= overlap1["min"] & D$dwnom1 <= overlap1["max"]
  intersect1.R <- R$dwnom1 >= overlap1["min"] & R$dwnom1 <= overlap1["max"]

  # Overlap in Two Dimensions

  classfication.line <- angleBisector()

  intercept <- classfication.line["intercept"]
  slope <- classfication.line["slope"]
  ortho.slope <- -1 / slope
  x.ortho <- intercept / (ortho.slope - slope)

  ortho.left <- orthogonalCoordinates()
  ortho.right <- orthogonalCoordinates(R)
  overlap.interval <- stats::setNames(c(min(ortho.right$x), max(ortho.left$x)),
    c("min", "max"))

  y.coord <- ortho.slope * overlap.interval
  b <- y.coord - slope * overlap.interval

  if (any(ortho.left$x > x.ortho) & any(ortho.right$x < x.ortho)) {
    intersect.left <- ortho.left$x >= overlap.interval["min"] &
                      ortho.left$x <= overlap.interval["max"]
    intersect.right <- ortho.right$x >= overlap.interval["min"] &
                       ortho.right$x <= overlap.interval["max"]

  } else if (all(ortho.left$x < x.ortho) & any(ortho.right$x < x.ortho)) {
    intersect.left <- rep(FALSE, nrow(ortho.left))
    intersect.right <- ortho.right$x < x.ortho

  } else if (any(ortho.left$x >= x.ortho) & all(ortho.right$x > x.ortho)) {
    intersect.left <- ortho.left$x > x.ortho
    intersect.right <- rep(FALSE, nrow(ortho.right))

  } else if (all(ortho.left$x < x.ortho) & all(ortho.right$x > x.ortho)) {
    intersect.left <- rep(FALSE, nrow(ortho.left))
    intersect.right <- rep(FALSE, nrow(ortho.right))
  }

  out <- data.frame(congress,
                    left.intersect1 = sum(intersect1.D),
                    left.complement1 = sum(intersect1.D == FALSE),
                    right.intersect1 = sum(intersect1.R),
                    right.complement1 = sum(intersect1.R == FALSE),
                    left.intersect = sum(intersect.left),
                    left.complement = sum(intersect.left == FALSE),
                    right.intersect = sum(intersect.right),
                    right.complement = sum(intersect.right == FALSE))

  data.frame(congress,
             D1 = out$left.intersect1 /
                  (out$left.intersect1 + out$left.complement1),
             R1 = out$right.intersect1 /
                  (out$right.intersect1 + out$right.complement1),
             T1 = (out$left.intersect1 + out$right.intersect1) /
                  (out$left.intersect1 + out$left.complement1 +
                   out$right.intersect1 + out$right.complement1),
             D = out$left.intersect /
                 (out$left.intersect + out$left.complement),
             R = out$right.intersect /
                 (out$right.intersect + out$right.complement),
             T = (out$left.intersect + out$right.intersect) /
                 (out$left.intersect + out$left.complement +
                  out$right.intersect + out$right.complement))
}
