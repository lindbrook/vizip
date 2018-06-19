#' Common space DW-NOMINATE scores data (weekly).
#'
#' @format
#'  \describe{
#'     \item{\code{cong}}{Congressional Session}
#'     \item{\code{idno}}{ICPSR ID Number}
#'     \item{\code{state}}{State Code}
#'     \item{\code{cd}}{Congressional District Number}
#'     \item{\code{statenm}}{State Name}
#'     \item{\code{party}}{Party Code}
#'     \item{\code{occupancy}}{ICPSR Occupancy Code}
#'     \item{\code{lastmeans}}{Last Means of Attaining Office}
#'     \item{\code{name}}{Name}
#'     \item{\code{dwnom1}}{1st Dimension Coordinate}
#'     \item{\code{dwnom2}}{2nd Dimension Coordinate}
#'     \item{\code{logl}}{Log-Likelihood}
#'     \item{\code{nchoices}}{Number of Votes}
#'     \item{\code{errors}}{Number of Classification Errors}
#'     \item{\code{gmp}}{Geometric Mean Probability}
#' }
#' @source \url{http://voteview.org/Weekly_Constant_Space_DW-NOMINATE_Scores.htm}
#' @source \url{ftp://k7moa.com/wf1/Weekly_DW-NOMINATE_31_12.DTA}
"ideal.points.wk31"

#' Common space DW-NOMINATE scores data.
#'
#' @format
#'  \describe{
#'     \item{\code{cong}}{Congressional Session}
#'     \item{\code{idno}}{ICPSR ID Number}
#'     \item{\code{state}}{State Code}
#'     \item{\code{cd}}{Congressional District Number}
#'     \item{\code{statenm}}{State Name}
#'     \item{\code{party}}{Party Code}
#'     \item{\code{name}}{Name}
#'     \item{\code{dwnom1}}{1st Dimension Coordinate}
#'     \item{\code{dwnom2}}{2nd Dimension Coordinate}
#'     \item{\code{bootse1}}{1st Dimension Bootstrapped Standard Error}
#'     \item{\code{bootse2}}{2nd Dimension Bootstrapped Standard Error}
#'     \item{\code{corr12}}{Correlation Between 1st and 2nd Dimension Bootstrapped Estimates}
#'     \item{\code{logl}}{Log-Likelihood}
#'     \item{\code{nchoices}}{Number of Votes}
#'     \item{\code{errors}}{Number of Classification Errors}
#'     \item{\code{gmp}}{Geometric Mean Probability}
#' }
#' @source \url{http://voteview.org/dwnomin_joint_house_and_senate.htm}
#' @source \url{ftp://k7moa.com/junkord/HANDSL01113C20_BSSE_12.DTA}
"ideal.points.r5"

#' Regular DW-NOMINATE scores data.
#'
#' @format
#'  \describe{
#'     \item{\code{cong}}{Congressional Session}
#'     \item{\code{idno}}{ICPSR ID Number}
#'     \item{\code{state}}{State Code}
#'     \item{\code{cd}}{Congressional District Number}
#'     \item{\code{statenm}}{State Name}
#'     \item{\code{party}}{Party Code}
#'     \item{\code{name}}{Name}
#'     \item{\code{dwnom1}}{1st Dimension Coordinate}
#'     \item{\code{dwnom2}}{2nd Dimension Coordinate}
#'     \item{\code{bootse1}}{1st Dimension Bootstrapped Standard Error}
#'     \item{\code{bootse2}}{2nd Dimension Bootstrapped Standard Error}
#'     \item{\code{corr12}}{Correlation Between 1st and 2nd Dimension Bootstrapped Estimates}
#'     \item{\code{logl}}{Log-Likelihood}
#'     \item{\code{nchoices}}{Number of Votes}
#'     \item{\code{errors}}{Number of Classification Errors}
#'     \item{\code{gmp}}{Geometric Mean Probability}
#' }
#' @source \url{http://voteview.org/dwnomin_joint_house_and_senate.htm}
#' @source \url{ftp://k7moa.com/junkord/HL01113D21_BSSE_12.DTA}
#' @source \url{ftp://k7moa.com/junkord/SL01113D21_BSSE_12.DTA}
"ideal.points.dw"
