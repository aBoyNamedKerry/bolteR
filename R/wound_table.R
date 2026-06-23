#' Provides a table showing the number of expected successful wounds
#' given a series of different strength profiles into a toughness.
#'
#'
#' The default toughness is set to 4 and the strength set to be half, less than,
#' equal, greater than and double accordingly.
#'
#' @param dice  the number of dice to roll
#' @param strength the strength profiles of the attacks
#' @param hit_prob the probability of a successful hit
#' @param toughness the toughness of the target profile
#' @param lethal_hits whether the weapon profile has lethal hits? Set to FALSE
#' @param reroll_wounds are wounds rerolled as part of the attack? Set to FALSE
#'
#' @import data.table
#' @importFrom purrr map map2
#' @return a data frame
#' @export
#'
#' @examples
#' wound_table(dice = 16, toughness = 4, lethal_hits = TRUE)
wound_table <- function(dice = 24, strength = c(2,3,4,5,8),toughness = 4,
                        lethal_hits = FALSE, hit_prob = 0.66,
                        reroll_wounds =FALSE)  {

  if(length(strength) != 5) stop(
    "stength must be a vector of length 5, e.g. c(2,3,4,5,8)"
    )

  if(lethal_hits) {
    lethal_list <- rep(c(FALSE, TRUE), length(strength))
    strength <- sort(rep(strength,2))

  #produce results
  df_list <- map2(strength, lethal_list, ~{
    data.frame(wound_checker(dice = dice, strength = .x, lethal_hits = .y,
                             hit_prob = hit_prob, toughness = toughness,
                             reroll_wounds = reroll_wounds))
  }
  )

  #create column names
  col_names <- (c("Weak", "Weak_lethal", "Weaker", "Weaker_lethal",
                  "Equal", "Equal_lethal", "Stronger","Stronger_lethal",
                  "Strong", "Strong_lethal"))

  } else {

    df_list <- map(strength, ~{
      data.frame(wound_checker(dice = dice, strength = .x, hit_prob = hit_prob,
                               toughness = toughness, reroll_wounds = reroll_wounds))

    })

    col_names <- (c("Weak", "Weaker",
                    "Equal",  "Stronger",
                    "Strong"))
  }



  #bind together the list of data frames
  df <- do.call("cbind", df_list)

  #rename based on the above
  names(df) <- col_names

  #set as a data.table
  setDT(df)

  if(lethal_hits) {
  #add new columns if lethal hits selected for comparison
  df <- df[, `:=`(
    Weak_diff =
      paste0(round((Weak_lethal - Weak) / Weak * 100,2), "%"),
    Weaker_diff =
      paste0(round((Weak_lethal - Weaker) / Weaker * 100,2), "%"),
    Equal_diff =
      paste0(round((Equal_lethal - Equal) / Equal * 100,2), "%"),
    Stronger_diff =
      paste0(round((Stronger_lethal - Stronger) / Stronger * 100,2), "%"),
    Strong_diff =
      paste0(round((Strong_lethal - Strong) / Strong * 100,2), "%")
  )]

  setcolorder(df, c("Weak", "Weak_lethal", "Weak_diff",
                    "Weaker", "Weaker_lethal", "Weaker_diff",
                    "Equal", "Equal_lethal", "Equal_diff",
                    "Stronger","Stronger_lethal", "Stronger_diff",
                    "Strong", "Strong_lethal", "Strong_diff"))
  }

  df


}
