#' Find the probability of wounding based on strength and wound characteristics
#'
#' @param strength the strength of the attacking model
#' @param toughness the toughness of the defending model
#'
#' @return the likely success based on the probability to 2 decimal places
#' @export
#'
#' @examples
#' wound_probability(strength = 4, toughness = 5)
wound_probability <- function(strength = 4, toughness = 4) {

  wound_prob <-
    ifelse(strength == toughness,
           0.5,
           ifelse(strength > toughness & strength <  2* toughness,
                  0.66,
                  ifelse(strength >=  2* toughness,
                         0.83,
                         ifelse(toughness > strength & toughness < 2*strength,
                                0.33,
                                0.166 ))))
  wound_prob
}
