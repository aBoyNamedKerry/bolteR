#' Provide the number of successful wound expected based on the proportion of
#' likely hits, strength and toughness characteristic of the attack
#'
#' Numbers are rounded up to the nearest whole number
#'
#' @param dice number of dice rolled
#' @param hit_prob probability of a successful hit
#' @param strength strength of attacking model
#' @param toughness toughness of defending model
#' @param lethal_hits are the wound lethal?
#'
#' @return a numeric scalar object
#' @export
#'
#' @examples
#' wound_checker(dice = 24, hit_prob = 0.33, strength = 4, toughness = 5)
wound_checker <- function(dice = 12, hit_prob = 0.66, strength = 4, toughness = 4, lethal_hits = FALSE) {

#derive probability of successful wound
 wound_prob <- wound_probability(strength = strength, toughness = toughness)

 hits <- round(dice * hit_prob)

 wound_success <- round(hits * wound_prob)

 if(lethal_hits) {

   lethals <- round(dice * 0.166)

   hits = round(dice * hit_prob  - lethals)

   wound_success = round(hits * wound_prob + lethals)

 }



 wound_success
}
