#' Legislators at endpoints of overlap interval (1st Dimension).
#'
#' Most liberal Republican (lowest score) and most conservative Democrat (highest score).
#' @param congress Numeric. Session number.
#' @param chamber Character. "house", "senate" or "both".
#' @param data.version Character. DW-NOMINATE flavor: "common", "common.weekly" or "dw".
#' @param mcdonald Logical. Include Larry McDonald (GA-D).
#' @param stump Logical. Include Bob Stump (AZ-D).
#' @return An R data frame. NA indicates no overlap.
#' @export

overlapEndPoints <- function(congress = 100, chamber = "house",
  data.version = "common", mcdonald = TRUE, stump = TRUE) {

  dat <- vizip::idealPoints(congress, chamber, state = NULL, data.version,
    mcdonald, stump)
  vars <- c("cong", "name", "state.abb", "party2", "dwnom1", "dwnom2", "d2",
            "south", "idno")
  D <- dat[dat$party2 == "D", ]
  R <- dat[dat$party2 == "R", ]

  if (max(D$dwnom1, na.rm = TRUE) > min(R$dwnom1, na.rm = TRUE)) {
    rbind(R[which.min(R$dwnom1), vars], D[which.max(D$dwnom1), vars])
  } else {
    NA
  }
}
