
dice <- dice_checkr(sides = 1:6, value = 3)

test_that("output as expected", {
  expect_equal(1, length(dice))

  expect_type(dice, "double")
})
