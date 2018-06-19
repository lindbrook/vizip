#' 2D Plot (prototype).
#'
#` polarization2D.v12.R
#' @param congress Numeric. Session number.
#' @param chamber Character. "house", "senate" or "both".
#' @param state Character. Two character state postal abbreviation (e.g., "CA") or "US" (President). Lower case OK (e.g, "ca").
#' @param data.version Character. DW-NOMINATE flavor: "common", "common.weekly" or "dw".
#' @param mcdonald Logical. Include Larry McDonald (GA-D). idno = 14252.
#' @param stump Logical. Include Bob Stump (AZ-D). idno = 94454.
#' @param alpha Numeric. Alpha level transparency.
#' @param wt2D Logical. Weight second dimension.
#' @param all Logical. Show legislator points.
#' @param overlap.trim Numeric. Sets the smaller quantile to trim the range of scores. Must be on [0, 0.5). "0" uses minimum Republican score and maximum Democrat score.
#' @param ... Additional plotting parameters.
#' @return An R plot.
#' @export
#' @import graphics
#' @examples
#' # polarization2D(chamber = "both")
#' # polarization2D(state = "CA")
#' # polarization2D(state = "ny")

polarization2D <- function(congress = 100, chamber = "house", state = NULL,
  data.version = "common", mcdonald = TRUE, stump = TRUE, alpha = 2/3,
  overlap.trim = 0, wt2D = TRUE, all = TRUE,  ...) {

  if (!all(chamber %in% c("both", "house", "senate"))) {
    stop('"chamber" must be "house", "senate" or "both".')
  }

  if (!all(data.version %in% c("common", "common.weekly", "dw"))) {
    stop('"data.version" must be "common", "common.weekly" or "dw".')
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

  pcaPooled <- function(weighted = wt2D) {
    if (weighted == TRUE) {
      fmla <- stats::formula(~ dwnom1 + d2)
    } else {
      fmla <- stats::formula(~ dwnom1 + dwnom2)
    }

    v <- stats::prcomp(fmla, data = dat)$rotation
    m <- -v[-ncol(v), ncol(v)] / v[ncol(v), ncol(v)]

    if (weighted == TRUE) {
      intercept <- mean(dat$d2) - m * mean(dat$dwnom1)
    } else {
      intercept <- mean(dat$dwnom2) - m * mean(dat$dwnom1)
    }

    abline(intercept, m, col = "green")
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

  pcaDraw <- function(party = "D", weighted = wt2D) {
    if (party == "D") {
      x <- get("D")
      color <- "blue"
      seg <- pcaLine()
    } else if (party == "R") {
      x <- get("R")
      color <- "red"
      seg <- pcaLine("R")
    }

    x1 <- x$dwnom1[which.min(x$dwnom1)]
    y1 <- seg["slope"] * x1 + seg["intercept"]
    x2 <- x$dwnom1[which.max(x$dwnom1)]
    y2 <- seg["slope"] * x2 + seg["intercept"]
    segments(x1, y1, x2, y2, lwd = 2, col = color)
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

  ## graph ##

  plot(D$dwnom1, D$d2, xlim = c(-1.05, 1.05), ylim = c(-1.05, 1.05),
    asp = 1, pch = NA, xlab = "1st Dimension", ylab = "2nd Dimension")

  # d.south <- D[DS, c("dwnom1", "d2")]

  hullBoundary <- function(x, color) {
    hull <- suppressWarnings(alphahull::ashape(x, alpha = 0.2))
    e <- data.frame(hull$edges)
    invisible(lapply(seq_len(nrow(e)), function(i) {
      segments(e[i, "x1"], e[i, "y1"], e[i, "x2"], e[i, "y2"], col = color)
    }))
  }

  d.data <- D[, c("dwnom1", "d2")]
  r.data <- R[, c("dwnom1", "d2")]

  # hullBoundary(d.data, "blue")
  # hullBoundary(r.data, "red")

  # hull <- chull(d.data)
  # hull.pts <- c(hull, hull[1])
  # lines(D$dwnom1[hull.pts], D$d2[hull.pts], col = "black")

  convexHull <- function(party = "D", wt2D = TRUE) {
    if (party == "D") {
      x <- get("D")
      color <- "blue"

      if (wt2D) {
        dat <- x[, c("dwnom1", "d2")]
      } else {
        dat <- x[, c("dwnom1", "dwnom2")]
      }
    } else if (party == "R") {
      x <- get("R")
      color <- "red"

      if (wt2D) {
        dat <- x[, c("dwnom1", "d2")]
      } else {
        dat <- x[, c("dwnom1", "dwnom2")]
      }
    }

    hull.pts <- grDevices::chull(dat)
    vertices <- c(hull.pts, hull.pts[1])
    lines(dat$dwnom1[vertices], dat$d2[vertices], col = color)
  }

  kdeContour <- function(party = "D", wt2D = TRUE, bandwidth = 1/8) {
    if (party == "D") {
      x <- get("D")
      color <- "blue"

      if (wt2D) {
        dat <- x[, c("dwnom1", "d2")]
      } else {
        dat <- x[, c("dwnom1", "dwnom2")]
      }
    } else if (party == "R") {
      x <- get("R")
      color <- "red"

      if (wt2D) {
        dat <- x[, c("dwnom1", "d2")]
      } else {
        dat <- x[, c("dwnom1", "dwnom2")]
      }
    }

    kde <- KernSmooth::bkde2D(dat, bandwidth)

    contour(x = kde$x1, y = kde$x2, z = kde$fhat,
      col = grDevices::adjustcolor(color, 0.5), add = TRUE)
  }

  # points(d.data, pch = 16, col = grDevices::adjustcolor("blue", 0.25))
  # points(r.data, pch = 16, col = grDevices::adjustcolor("red", 0.25))

  invisible(lapply(c("D", "R"), kdeContour))

  hullPolygon <- function(x, color) {
    hull <- suppressWarnings(alphahull::ashape(x, alpha = 0.2))
    e <- data.frame(hull$edges)

    e2 <- e[, c("ind1", "ind2")]
    ordered.vertices <- vector(length = nrow(e))

    ordered.vertices[1] <- e2$ind1[1]

    for (i in 2:length(ordered.vertices)) {
      dat <- e2[e2$ind1 == ordered.vertices[i - 1] |
                e2$ind2 == ordered.vertices[i - 1], ]

      if (!all(dat[1, ] %in% ordered.vertices)) {
        ordered.vertices[i] <- unlist(dat[1, ][dat[1, ] %in%
          ordered.vertices == FALSE])
      } else if (!all(dat[2, c("ind1", "ind2")] %in% ordered.vertices)) {
        ordered.vertices[i] <- unlist(dat[2, ][dat[2, ] %in%
          ordered.vertices == FALSE])
      }
    }

    polygon(x[ordered.vertices, ], col = grDevices::adjustcolor(color, 0.25))
  }

  if (all) {
    if (wt2D) {
      plotrix::draw.ellipse(0, 0, 1, wt, lwd = 0.5)

      # complement
      points(D[DS & !intersect.left, c("dwnom1", "d2")], pch = 16,
        col = grDevices::adjustcolor("dodgerblue", alpha))
      points(D[DN & !intersect.left, c("dwnom1", "d2")], pch = 1,
        col = grDevices::adjustcolor("blue", alpha))
      points(R[RS & !intersect.right, c("dwnom1", "d2")], pch = 15,
        col = grDevices::adjustcolor("violet", alpha))
      points(R[RN & !intersect.right, c("dwnom1", "d2")], pch = 0,
        col = grDevices::adjustcolor("red", alpha))

      # intersection
      points(D[DS & intersect.left, c("dwnom1", "d2")], pch = 21,
        bg = grDevices::adjustcolor("dodgerblue", alpha))
      points(D[DN & intersect.left, c("dwnom1", "d2")], pch = 21,
        bg = grDevices::adjustcolor("blue", alpha))
      points(R[RS & intersect.right, c("dwnom1", "d2")], pch = 22,
        bg = grDevices::adjustcolor("violet", alpha))
      points(R[RN & intersect.right, c("dwnom1", "d2")], pch = 22,
        bg = grDevices::adjustcolor("red", alpha))
    } else {
      plotrix::draw.ellipse(0, 0, 1, 1, lwd = 0.5)

      # complement
      points(D[DS & !intersect.left, c("dwnom1", "dwnom2")], pch = 16,
        col = grDevices::adjustcolor("dodgerblue", alpha))
      points(D[DN & !intersect.left, c("dwnom1", "dwnom2")], pch = 1,
        col = grDevices::adjustcolor("blue", alpha))
      points(R[RS & !intersect.right, c("dwnom1", "dwnom2")], pch = 15,
        col = grDevices::adjustcolor("violet", alpha))
      points(R[RN & !intersect.right, c("dwnom1", "dwnom2")], pch = 0,
        col = grDevices::adjustcolor("red", alpha))

      # intersection
      points(D[DS & intersect.left, c("dwnom1", "dwnom2")], pch = 21,
        bg = grDevices::adjustcolor("dodgerblue", alpha))
      points(D[DN & intersect.left, c("dwnom1", "dwnom2")], pch = 21,
        bg = grDevices::adjustcolor("blue", alpha))
      points(R[RS & intersect.right, c("dwnom1", "dwnom2")], pch = 22,
        bg = grDevices::adjustcolor("violet", alpha))
      points(R[RN & intersect.right, c("dwnom1", "dwnom2")], pch = 22,
        bg = grDevices::adjustcolor("red", alpha))
    }
  }

  distanceArrow <- function(x) {
    arrows(x[1, 1], x[1, 2], x[2, 1], x[2, 2], code = 3, length = 1/8, lwd = 3)
    # arrows(x[1, 1], x[1, 2], x[2, 1], x[2, 2], code = 3, length = 1/8, lwd = 3,
    #   angle = 90)
  }

  distanceArrowB <- function(x) {
    arrows(x[1, 1], x[1, 2], x[2, 1], x[2, 2], code = 3, length = 1/8)
  }

  if (wt2D) {
    x.D <- stats::median(D$dwnom1)
    y.D <- stats::median(D$d2)
    x.R <- stats::median(R$dwnom1)
    y.R <- stats::median(R$d2)

    x.DN <- stats::median(D[DN, "dwnom1"])
    y.DN <- stats::median(D[DN, "d2"])
    x.DS <- stats::median(D[DS, "dwnom1"])
    y.DS <- stats::median(D[DS, "d2"])

    x.RN <- stats::median(R[RN, "dwnom1"])
    y.RN <- stats::median(R[RN, "d2"])
    x.RS <- stats::median(R[RS, "dwnom1"])
    y.RS <- stats::median(R[RS, "d2"])

    # points(x.DN, y.DN, pch = 25, col = "black", bg = "green")
    # points(x.DS, y.DS, pch = 24, col = "black", bg = "green")

  } else {
    x.D <- stats::median(D$dwnom1)
    y.D <- stats::median(D$dwnom2)
    x.R <- stats::median(R$dwnom1)
    y.R <- stats::median(R$dwnom2)

    x.DN <- stats::median(D[DN, "dwnom1"])
    y.DN <- stats::median(D[DN, "dwnom2"])
    x.DS <- stats::median(D[DS, "dwnom1"])
    y.DS <- stats::median(D[DS, "dwnom2"])

    x.RN <- stats::median(R[RN, "dwnom1"])
    y.RN <- stats::median(R[RN, "dwnom2"])
    x.RS <- stats::median(R[RS, "dwnom1"])
    y.RS <- stats::median(R[RS, "dwnom2"])

    # points(x.DN, y.DN, pch = 25, col = "black", bg = "green")
    # points(x.DS, y.DS, pch = 24, col = "black", bg = "green")
  }



  dist.data <- rbind(c(x.D, y.D), c(x.R, y.R))

  dist.dataB <- list(rbind(c(x.DN, y.DN), c(x.DS, y.DS)),
                     rbind(c(x.RN, y.RN), c(x.RS, y.RS)))

  dist.df <- stats::setNames(data.frame(dist.data), c("x", "y"))

  distanceArrow(dist.data)

  invisible(lapply(dist.dataB, distanceArrowB))

  ols <- stats::lm(y ~ x, data = dist.df)
  arrow.slope <- stats::coef(ols)[2]
  theta <- atan(arrow.slope)
  h <- c(stats::dist(dist.data))
  delta.x <- h / 2 * cos(theta)
  delta.y <- h / 2 * sin(theta)

  text(dist.df[1, "x"] + delta.x, dist.df[1, "y"] + delta.y, pos = 3,
    labels = round(h, 3), srt = theta * 180L / pi)


  points(x.D, y.D, pch = 21, bg = "green")
  points(x.R, y.R, pch = 22, bg = "green")
  points(x.DN, y.DN, pch = 25, col = "black", bg = "green")
  points(x.DS, y.DS, pch = 24, col = "black", bg = "green")
  points(x.RN, y.RN, pch = 25, col = "black", bg = "green")
  points(x.RS, y.RS, pch = 24, col = "black", bg = "green")


  abline(v = 0, col = "lightgray")
  abline(h = 0, col = "lightgray")
  abline(b["max"], slope, lty = "dotted", col = "blue")
  abline(b["min"], slope, lty = "dotted", col = "red")

  d.rng <- range(D$dwnom1)
  r.rng <- range(R$dwnom1)
  arrows(d.rng[1], 0.95, d.rng[2], 0.95, col = "blue", angle = 90,
    length = 0.05, code = 3)
  arrows(r.rng[1], 0.9, r.rng[2], 0.9, col = "red", angle = 90, length = 0.05,
    code = 3)
  text(mean(d.rng), 0.95, labels = round(abs(d.rng[1] - d.rng[2]), 2),
    cex = 0.8, pos = 3, col = "blue")
  text(mean(r.rng), 0.9, labels = round(abs(r.rng[1] - r.rng[2]), 2),
    cex = 0.8, pos = 1, col = "red")

  if (wt2D) {
    d.rng <- range(D$d2)
    r.rng <- range(R$d2)
    d.cor <- stats::cor(D$dwnom1, D$d2)
    r.cor <- stats::cor(R$dwnom1, R$d2)
  } else {
    d.rng <- range(D$dwnom2)
    r.rng <- range(R$dwnom2)
    d.cor <- stats::cor(D$dwnom1, D$dwnom2)
    r.cor <- stats::cor(R$dwnom1, R$dwnom2)
  }

  arrows(1.15, d.rng[1], 1.15, d.rng[2], col = "blue", angle = 90,
    length = 0.05, code = 3)
  arrows(1.1, r.rng[1], 1.1, r.rng[2], col = "red", angle = 90, length = 0.05,
    code = 3)
  text(1.2, mean(d.rng), labels = round(abs(d.rng[1] - d.rng[2]), 2),
    cex = 0.8, col = "blue", srt = -90)
  text(1.05, mean(r.rng), labels = round(abs(r.rng[1] - r.rng[2]), 2),
    cex = 0.8, col = "red", srt = -90)

  text(-1, -1, labels = paste0("r = ", round(d.cor, 2)), col = "blue",
    cex = 0.8)
  text(1, -1, labels = paste0("r = ", round(r.cor, 2)), col = "red",
    cex = 0.8)

  inter1 <- sum(sum(intersect1.D), sum(intersect1.R))
  inter2 <- sum(sum(intersect.left), sum(intersect.right))
  text(-1, 1, labels = paste0("T1 = ", round(inter1, 2)), cex = 0.8)
  text(1, 1, labels = paste0("T2 = ", round(inter2, 2)), cex = 0.8)

  abline(intercept, slope, col = "black")
  # pcaPooled()
  # pcaDraw()
  # pcaDraw("R")

  rug(D[DS & intersect1.D, "dwnom1"], side = 3, col = "dodgerblue")
  rug(D[DN & intersect1.D, "dwnom1"], side = 3, col = "blue")
  rug(R[RS & intersect1.R, "dwnom1"], side = 3, col = "violet")
  rug(R[RN & intersect1.R, "dwnom1"], side = 3, col = "red")

  points(x.D, y.D, pch = 21, bg = "green")
  points(x.R, y.R, pch = 22, bg = "green")

  d.angle <- atan(pcaLine()["slope"]) * 180 / pi
  r.angle <- atan(pcaLine("R")["slope"]) * 180 / pi
  b.angle <- atan(slope) * 180 / pi

  axis(3, at = c(x.D, x.R), labels = NA, col.ticks = "green", lwd = 1.5)
  axis(4, at = c(y.D, y.R), labels = NA, col.ticks = "green", lwd = 1.5)
  axis(3, at = 0, labels = NA)
  axis(4, at = 0, labels = NA)

  title(main = paste0(congress, " (", y1[which(id2 == congress)], ") D:b:R = ",
    round(d.angle, 2), ", ", round(b.angle, 2), ", ", round(r.angle, 2)))
  title(sub = paste0("D:R = ",
                     round(nrow(D) / nrow(dat), 2), ", ",
                     round(nrow(R) / nrow(dat), 2),
                     " D1 = ", round(abs(x.D - x.R), 2),
                     " D2 = ", round(stats::dist(dist.data), 2)))
}
