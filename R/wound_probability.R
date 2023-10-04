wound_probability <- function(strength = 4, toughness = 5) {

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
