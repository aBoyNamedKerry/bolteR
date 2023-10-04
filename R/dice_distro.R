
dice_distro <- function(sides = 1:6, rolls = 1000, value = 3, times= 1:1000,
                        rerolls = TRUE) {

# pass in dice_checkr function
  results = sapply(times, dice_checkr, sides = sides, rolls = rolls,
                   value = value, rerolls = rerolls)

  avg_results = mean(results, na.rm = TRUE)

  min_results = min(results)

  max_results = max(results)

  var_results = sd(results, na.rm = TRUE)

  var_lower = avg_results - 3*var_results
    var_lower = ifelse(var_lower < 0, 0, var_lower) # set floor

  var_upper = avg_results + 3*var_results
    var_upper = ifelse(var_upper < 0, 0, var_upper) # set ceiling

  successes =  round(rolls * avg_results)

  summary = paste("Mean prob = ", round(avg_results,3),
                  "\nMax prob = ", round(max_results, 3),
                  "\nMin prob = ", round(min_results,3),
                  "\nUpper prob = ", round(var_upper, 3),
                  "\nLower prob = ", round(var_lower,3),
                  "\nSuccesful rolls = ", successes)


  #print results
  cat(summary, sep = "\n")

  dice_list = (list(results, avg_results, min_results, max_results, var_results,
              var_lower, var_upper))


}
