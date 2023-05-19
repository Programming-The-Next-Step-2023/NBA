library("play.by.play")

context("Test the game_data function")

test_that("the game_data function returns df of correct size", {


  gd <- game_data(22200002)

  expect_equal(nrow(gd), 634)
})
