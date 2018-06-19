#' 1D Plot.
#'
#' Distribution of scores along 1st dimension.
#' @param congress Numeric. Session number.
#' @param chamber Character. "house", "senate" or "both". "both" includes President.
#' @param state Character. Two character state postal abbreviation (e.g., "CA") or "US" (President). Lower case is OK (e.g, "ca").
#' @param data.version Character. DW-NOMINATE flavor: "common", "common.weekly" or "dw".
#' @param mcdonald Logical. Include Larry McDonald (GA-D).
#' @param stump Logical. Include Bob Stump (AZ-D).
#' @param alpha Numeric. Alpha level transparency.
#' @param ... Additional plotting parameters.
#' @return An R plot.
#' @export
#' @import graphics

polarization1D <- function(congress = 100, chamber = "house", state = NULL,
  data.version = "common", mcdonald = TRUE, stump = TRUE, alpha = 1/3, ...) {

  yDensity <- function(id2) {
    vapply(id2, function(i) {
      data.select <- ideal.points[ideal.points$cong == i, ]
      data.select <- data.select[data.select$statenm != "USA", ]
      D <- data.select[data.select$party == 100, ]
      R <- data.select[data.select$party == 200, ]
      hD <- stats::density(D$dwnom1)
      hR <- stats::density(R$dwnom1)
      max(c(hD$y, hR$y))
    }, numeric(1L))
  }

  ## Data ##

  if (data.version == "common.weekly") {
    wt <- 0.4153
    # URL <- "ftp://k7moa.com/wf1/Weekly_DW-NOMINATE_31_12.DTA"
    # ideal.points <- foreign::read.dta(URL)
    ideal.points <- vizip::ideal.points.wk31
    y1 <- seq(1857, 2015, 2)
  } else if (data.version == "common") {
    wt <- 0.4113
    # URL <- "ftp://k7moa.com/junkord/HANDSL01113C20_BSSE_12.DTA"
    # ideal.points <- foreign::read.dta(URL)
    ideal.points <- vizip::ideal.points.r5
    y1 <- seq(1857, 2013, 2)
  } else if (data.version == "dw") {
    wt <- 0.3988
    # URL <- "ftp://k7moa.com/junkord/HL01113D21_BSSE_12.DTA"
    # ideal.points <- foreign::read.dta(URL)
    # URL <- "ftp://k7moa.com/junkord/SL01113D21_BSSE_12.DTA"
    # ideal.points.senate <- foreign::read.dta(URL)
    # ideal.points <- rbind(ideal.points, ideal.points.senate)
    ideal.points <- vizip::ideal.points.dw
    y1 <- seq(1857, 2013, 2)
  }

  congressID <- unique(ideal.points$cong)
  id2 <- congressID[35:length(congressID)]
  state.codes <- vizip::stateCodes()
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
    # ideal.points <- ideal.points[ideal.points$statenm != "USA", ]
    if (mcdonald == FALSE) {
      ideal.points <- ideal.points[ideal.points$idno != 14252, ]
    }
    if (stump == FALSE) {
      ideal.points <- ideal.points[ideal.points$idno != 94454, ]
    }
  }

  dat <- ideal.points[ideal.points$cong == congress, ]
  dat$d2 <- wt * dat$dwnom2
  dat$party2 <- NA
  dat[dat$party == 100, "party2"] <- "D"
  dat[dat$party == 200, "party2"] <- "R"
  D <- dat[dat$party == 100, ]
  R <- dat[dat$party == 200, ]
  densityD <- stats::density(D$dwnom1)
  densityR <- stats::density(R$dwnom1)
  y.max <- max(yDensity(id2))

  plot(NA, xlim = c(-1, 1), ylim = c(0, y.max), xlab = "Score",
    ylab = "Density")
  abline(v = 0, col = "lightgray")

  polygon(densityD, col = grDevices::adjustcolor("blue", alpha.f = alpha),
    border = NA)
  polygon(densityR, col = grDevices::adjustcolor("red", alpha.f = alpha),
    border = NA)
  axis(3, at = stats::median(D$dwnom1), col = "blue", col.axis = "blue",
    cex.axis = 0.75, labels = round(stats::median(D$dwnom1), 3))
  axis(3, at = stats::median(R$dwnom1), col = "red", col.axis = "red",
    cex.axis = 0.75, labels = round(stats::median(R$dwnom1), 3))
  axis(3, at = 0, labels = 0, cex.axis = 0.75)

  arrows(stats::quantile(D$dwnom1, 1), y.max - 0.125,
    stats::quantile(D$dwnom1, 0), y.max - 0.125, angle = 90, code = 3,
    length = 0.05, col = grDevices::adjustcolor("blue", alpha.f = alpha))
  arrows(stats::quantile(R$dwnom1, 1), y.max - 0.125,
    stats::quantile(R$dwnom1, 0), y.max - 0.125, angle = 90, code = 3,
    length = 0.05, col = grDevices::adjustcolor("red", alpha.f = alpha))

  text(min(D$dwnom1), y.max - 0.125, labels = round(min(D$dwnom1), 2),
    cex = 0.75, col = "blue", pos = 3)
  text(max(D$dwnom1), y.max - 0.125, labels = round(max(D$dwnom1), 2),
    cex = 0.75, col = "blue", pos = 3)
  text(min(R$dwnom1), y.max - 0.125, labels = round(min(R$dwnom1), 2),
    cex = 0.75, col = "red", pos = 1)
  text(max(R$dwnom1), y.max - 0.125, labels = round(max(R$dwnom1), 2),
    cex = 0.75, col = "red", pos = 1)

  d.typical <- stats::median(D$dwnom1)
  r.typical <- stats::median(R$dwnom1)

  arrows(d.typical, y.max - 2, r.typical, y.max - 2, angle = 90, code = 3,
    length = 0.05)
  arrows(d.typical, y.max - 2, r.typical, y.max - 2, code = 3, length = 0.05)
  ideo.dist <- round(abs(d.typical - r.typical), 3)
  # points(mean(c(d.typical, r.typical)), y.max - 2, pch = "|", cex = 0.75)
  text(mean(c(d.typical, r.typical)), y.max - 2, labels = ideo.dist, pos = 3,
    cex = 0.75)

  D.x <- mean(c(stats::quantile(D$dwnom1, 0.25),
                stats::quantile(D$dwnom1, 0.75)))
  R.x <- mean(c(stats::quantile(R$dwnom1, 0.25),
                stats::quantile(R$dwnom1, 0.75)))

  text(D.x, 0.75, col = "black", cex = 0.75, labels = paste("D =", nrow(D)))
  text(R.x, 0.75, col = "black", cex = 0.75, labels = paste("R =", nrow(R)))

  audit <- max(D$dwnom1) - min(R$dwnom1)

  if (audit > 0) {
    inter.D <- D[D$dwnom1 <= max(D$dwnom1) & D$dwnom1 >= min(R$dwnom1), ]
    inter.R <- R[R$dwnom1 <= max(D$dwnom1) & R$dwnom1 >= min(R$dwnom1), ]
    overlap <- round(sum(nrow(inter.D), nrow(inter.R)) /
                     sum(nrow(D), nrow(R)), 2)

    d.overlap <- round(nrow(inter.D) / nrow(D), 2)
    r.overlap <- round(nrow(inter.R) / nrow(R), 2)
  }

  if (audit >= 0.2) {
    arrows(max(D$dwnom1), y.max / 2, min(R$dwnom1), y.max / 2, angle = 90,
      code = 3, length = 0.025)

    text(stats::median(c(max(D$dwnom1), min(R$dwnom1))), y.max / 2 + 0.15,
      pos = 3, labels = sum(nrow(inter.D), nrow(inter.R)), cex = 0.75)
    text(min(R$dwnom1), y.max / 2, labels = nrow(inter.D), cex = 0.75,
      col = "blue", pos = 3)
    text(max(D$dwnom1), y.max / 2, labels = nrow(inter.R), cex = 0.75,
      col = "red", pos = 3)

    text(stats::median(c(max(D$dwnom1), min(R$dwnom1))), y.max / 2 - 0.15,
      labels = overlap, pos = 1, cex = 0.75)
    text(min(R$dwnom1), y.max / 2, labels = d.overlap, pos = 1, cex = 0.75,
      col = "blue")
    text(max(D$dwnom1), y.max / 2, pos = 1, labels = r.overlap, cex = 0.75,
      col = "red")

  } else if (audit > 0 & audit < 0.2) {
    delta <- (0.2 - audit) / 2
    d.max <- max(D$dwnom1) + delta
    r.min <- min(R$dwnom1) - delta
    arrows(max(D$dwnom1), y.max / 2, min(R$dwnom1), y.max / 2,
      angle = 90, code = 3, length = 0.025)

    text(stats::median(c(d.max, r.min)), y.max / 2 + 0.15,
      labels = sum(nrow(inter.D), nrow(inter.R)), pos = 3, cex = 0.75)
    text(r.min, y.max / 2, labels = nrow(inter.D), pos = 3, cex = 0.75,
      col = "blue")
    text(d.max, y.max / 2, labels = nrow(inter.R), pos = 3, cex = 0.75,
      col = "red")

    text(stats::median(c(d.max, r.min)), y.max / 2 - 0.15, labels = overlap,
      cex = 0.75, pos = 1)
    text(r.min, y.max / 2, labels = d.overlap, pos = 1, cex = 0.75,
      col = "blue")
    text(d.max, y.max / 2, labels = r.overlap, pos = 1, cex = 0.75, col = "red")
  }

  title(main = paste0(congress, " (", y1[which(id2 == congress)], ")"),
    line = 2.25)
}
