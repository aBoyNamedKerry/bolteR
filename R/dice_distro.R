
#' Distribution for desired values when for rolling x number of dice y times
#' The aim is to simulate the expected variance
#'
#' @param sides Number of sides or faces on the die
#' @param rolls the number of rolls you wish to make
#' @param value the value or better you wish to obtain
#' @param times the number of time you wish to roll the dice
#' @param rerolls can you reroll certain value
#' @param reroll_val the value to be rerolled defaults to 1:2
#'
#' @return A list with the total successes in a per times rolled
#' The average probability of success
#' The max probability of success across all rolls
#' The min probability of success across all rolls
#' The upper and lower bound of probability to approximately 99% of a normal
#' distribution.
#' The average number of successes to expect based on the simulation.
#'
#' @export
#'
#' @examples
#' # roll a 2 or better on 20 dice
#' dice_distro(rolls = 20, value = 2, rerolls = F)
dice_distro <- function(sides = 1:6, rolls = 1000, value = 3, times= 1:1000,
                        rerolls = FALSE, reroll_val = 1:2) {

# pass in dice_checkr function
  results = sapply(times, dice_checkr, sides = sides, rolls = rolls,
                   value = value, rerolls = rerolls, reroll_val = reroll_val)

  avg_result = mean(results, na.rm = TRUE)

  min_result = min(results)

  max_result = max(results)

  var_results = sd(results, na.rm = TRUE)

  var_lower = avg_result - 3*var_results
    var_lower = ifelse(var_lower < 0, 0, var_lower) # set floor

  var_upper = avg_result + 3*var_results
    var_upper = ifelse(var_upper < 0, 0, var_upper) # set ceiling

  successes =  round(rolls * avg_result)

  summary = paste("Mean prob = ", round(avg_result,3),
                  "\nMax prob = ", round(max_result, 3),
                  "\nMin prob = ", round(min_result,3),
                  "\nUpper prob = ", round(var_upper, 3),
                  "\nLower prob = ", round(var_lower,3),
                  "\nAverage Succesful rolls = ", successes)


  #print results
  cat(summary, sep = "\n")

  dice_list = (list(results = results, avg_result = avg_result,
                    min_result = min_result, max_result = max_result,
                    var_results = var_results,
              var_lower, var_upper))


}
