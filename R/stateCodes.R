#' State codes and names
#'
#' Allows use of the standard two letter postal state abbreviations (e.g., "CA") in other functions.
#' @section Notes: The President's state is "US".
#' @section Notes: "south" is the eleven states of the Confederacy plus Kentucky and Oklahoma (Nolan McCarty, Keith T. Poole and Howard Rosenthal. 2006. "Polarized America: The Dance of Ideology and Unequal Riches". MIT Press. p. 74).
#' @export

stateCodes <- function() {
  ideal.points <- vip::ideal.points.wk31
  ideal.points$statenm <- trimws(ideal.points$statenm)
  ideal.points <- ideal.points[ideal.points$statenm != "USA", ]
  voteview.nm <- unique(ideal.points[, c("state", "statenm")])
  voteview.nm <- voteview.nm[order(voteview.nm$statenm), ]

  state.codes <- data.frame(state.abb = datasets::state.abb,
                            state.name = datasets::state.name,
                            state = voteview.nm$state,
                            statenm = voteview.nm$statenm,
                            stringsAsFactors = FALSE)

  us <- data.frame(state.abb = "US",
                   state.name = "USA",
                   state = 99,
                   statenm = "USA",
                   stringsAsFactors = FALSE)

  state.codes <- rbind(state.codes, us)

  confederacy <- c("SC", "MS", "FL", "AL", "GA", "LA", "TX", "VA", "AR", "TN",
                   "NC")

  south <- c(confederacy, "KY", "OK")
  south.order <- c(1:11, rep(99, 2))
  state.codes$south <- NA

  for (i in seq_along(south)) {
    state.codes[state.codes$state.abb == south[i], "south"] <- south.order[i]
  }

  state.codes$south[is.na(state.codes$south)] <- 0
  state.codes
}
