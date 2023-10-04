wound_checker <- function(dice = 12, hit_prob = 0.66, strength = 4, toughness = 4, lethal_hits = FALSE) {

#derive probability of successful wound
 wound_prob <- wound_probability(strength = strength, toughness = toughness)

 hits <- round(dice * hit_prob)

 wound_success <- round(hits * wound_prob)

 if(lethal_hits) {

   lethals <- round(dice * 0.166)

   hits = round(dice * 0.66  - lethals)

   wound_success = round(hits * wound_prob + lethals)

 }



 wound_success
}
