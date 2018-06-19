#' Ideal Points.
#'
#' @param congress Numeric. Session number.
#' @param chamber Character. "house", "senate" or "both" (includes President).
#' @param state Character. Two character state postal abbreviation (e.g., "CA") or "US" (President). Lower case OK (e.g, "ca").
#' @param data.version Character. DW-NOMINATE flavor: "common", "common.weekly" or "dw".
#' @param mcdonald Logical. Include Larry McDonald (GA-D). Sessions 94-98.
#' @param stump Logical. Include Bob Stump (AZ-D). Sessions 95-97.
#' @return An R data frame.
#' @export

idealPoints <- function(congress = 100, chamber = "house", state = NULL,
  data.version = "common", mcdonald = TRUE, stump = TRUE) {

  state.codes <- vizip::stateCodes()

  if (is.null(state) == FALSE) {
    if (any(toupper(state) %in% state.codes$state.abb) == FALSE) {
      txt1 <- 'All values for "state" must be either be "US" (President)'
      txt2 <- 'or a valid two letter state abbreviation.'
      stop(paste(txt1, txt2))
    }
  }

  if (!all(chamber %in% c("both", "house", "senate"))) {
    stop('"chamber" must be "house", "senate" or "both".')
  }

  if (!all(data.version %in% c("common", "common.weekly", "dw"))) {
    stop('"data.version" must be "common", "common.weekly" or "dw".')
  }

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

  ideal.points$statenm <- trimws(ideal.points$statenm)
  ideal.points$name <- trimws(ideal.points$name)

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

  dat$state.abb <- NA
  dat$state.name <- NA
  dat$south <- NA
  vars <- c("state.abb", "state.name", "south")

  for (nm in dat$statenm) {
    dat[dat$statenm == nm, vars] <- state.codes[state.codes$statenm == nm, vars]
  }

  dat
}
