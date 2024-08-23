
#' The dice_checkr function is designed to simulate rolling a dice a number of
#' and obtain a value of better than based on the number of sides on the dice
#'
#' @param sides the number of faces the dice has
#' @param rolls the number of rolls you wish to simulate
#' @param value the value or better you are wishing to obtain
#' @param rerolls can you reroll the dice
#' @param reroll_val are there any values in particular to reroll, just a roll of
#' 1 for examples?
#' @param params holder argument, not currently used in this function
#'
#' @return a scalar with proportion of successful rolls,
#' will be equivalent to the general probably i.e. 1/6 if looking for a 6 on a
#' standard die
#' @export
#'
#' @examples
#' dice_checkr(value = 6, rerolls = FALSE)
#'
dice_checkr <- function(sides = 1:6, rolls = 1000, value = 3, rerolls = FALSE,
                        reroll_val = 1:2, params  = ...) {

  if(!is.numeric(sides)) stop("sides argument must be a numeric vector")

  if(!is.numeric(rolls) | !is.numeric(value) | !is.numeric(reroll_val)) stop("argument must be numeric")

  dice <- sample(x = sides, size = rolls, replace =TRUE)

  dice_value <- dice[dice >=value]

  success = length(dice_value) / length(dice)

if(rerolls) {

  dice_miss = dice[dice <value]

  reroll_dice <- sample(x = sides, size = length(dice_miss),
                      replace = TRUE)

  reroll_dice_value  <-  reroll_dice[reroll_dice >=value]

  success <-  (length(dice_value) + length(reroll_dice_value)) / length(dice)
}

 success
}
