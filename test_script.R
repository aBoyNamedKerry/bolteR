devtools::load_all()

dice_checkr(value = 6, rerolls = F)

dice_distro(rolls = 24,value = 6)

dice_distro(rolls = 10000, value = 2, rerolls = FALSE)


## comparing lethals

#roll 24 dice

#with lethals
dice <- 24

hit_perc = 0.66

wound_perc = 0.166

lethals <- round(dice * 0.166)

remaining_dice <- round(dice * 0.66  - lethals)

wound_dice <- round(remaining_dice * wound_perc)

successes_lethals = wound_dice + lethals

#without lethals

hit_dice <- round(dice * hit_perc)

wound_dice <-  round(hit_dice * wound_perc)


diff = (successes_lethals - wound_dice) / wound_dice


#with 3+ wound

wound_perc = 0.66

wound_dice = round(hit_dice * wound_perc)


#with 2+ wound

wound_perc = 0.83

wound_dice = round(hit_dice * wound_perc)



#with 5+ wound
wound_perc = 0.166

wound_dice = round(hit_dice * wound_perc)


## scenarios:
#strength and toughness equal
really_weak <- wound_checker(dice = 24, strength = 2)
really_weak_lethal <- wound_checker(dice = 24, strength = 2, lethal_hits = TRUE)

weaker <- wound_checker(dice = 24, strength = 3)
weaker_lethals <- wound_checker(dice = 24, strength = 3, lethal_hits = TRUE)

equal<- wound_checker(dice = 24)
equal_lethals <- wound_checker(dice = 24, lethal_hits = TRUE)

stronger <- wound_checker(dice = 24, strength = 5)
stronger_lethal <- wound_checker(dice = 24, strength = 5, lethal_hits = TRUE)

really_strong <- wound_checker(dice = 24, strength = 8)
really_strong_lethal <- wound_checker(dice = 24, strength = 8, lethal_hits = TRUE)


strength <- sort(rep(c(2,3,4, 5,8),2))

lethal_bi <- rep(c(FALSE, TRUE), 5)

col_names <- (c("really_weak", "really_weak_lethal", "weak", "weak_lethal", "equal",
        "equal_lethal", "strong","strong_lethal", "really_strong", "really_strong_lethal"))


#with function
df_list <- purrr::map2(strength, lethal_bi, ~{
    data.frame(wound_checker(dice = 16, strength = .x, lethal_hits = .y))

})

df <- do.call("cbind", df_list)

names(df) <- col_names

data.table::setDT(df)

df
#add new columns
df[, `:=`(
  really_weak_comp =
    paste0(round((really_weak_lethal - really_weak) / really_weak * 100,2), "%"),
  weak_comp =
    paste0(round((weak_lethal - weak) / weak * 100,2), "%"),
  equal_comp =
    paste0(round((equal_lethal - equal) / equal * 100,2), "%"),
  strong_comp =
    paste0(round((strong_lethal - strong) / strong * 100,2), "%"),
  really_strong_comp =
    paste0(round((really_strong_lethal - really_strong) / really_strong * 100,2), "%")
  )]

df

#within
purrr::map(col_names, ~{

  data.frame(.x = purrr::map2_dbl(strength, lethal_bi, ~{
  wound_checker(dice = 24, strength = .x, lethal_hits = .y)

})

)}
)


