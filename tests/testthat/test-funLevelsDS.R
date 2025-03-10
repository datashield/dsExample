library(testthat)

options(
  nfilter.tab = 3,
  nfilter.subset = 3,
  nfilter.glm = 0.33,
  nfilter.string = 80,
  nfilter.stringShort = 20,
  nfilter.kNN = 3,
  nfilter.levels.density = 0.33,
  nfilter.levels.max = 40,
  nfilter.noise = 0.25,
  nfilter.privacy.old = 5)

x <- factor(
  c(
    rep("apple", 30),
    rep("banana", 40),
    rep("cherry", 50)
  )
)

test_that("funLevelsDS returns expected levels with message", {
  options(datashield.privacyControlLevel = "banana")
  expect_equal(
    funLevelsDS("x", "Here are the levels"),
    "Here are the levels: apple, banana, cherry"
  )
})

test_that("funLevelsDS blocks the function if privacy level is not banana or permissive", {
  options(datashield.privacyControlLevel = "non-permissive")
  expect_error(
    funLevelsDS("x", "Here are the levels"),
    "BLOCKED: The server is running in 'non-permissive' mode which has caused this method to be blocked"
  )
  options(datashield.privacyControlLevel = "banana")
})

test_that(".getDensitySetting returns the correct disclosure setting", {
  expect_equal(
    .getDensitySetting(),
    0.33)
})

test_that(".calculateThreshold calculates correct threshold", {
  input <- c(1, 2, 3, 4, 5)
  expect_equal(.calculateThreshold(input, 0.2), 1)
})

test_that(".throwErrorIfRisk does not throw error when risk is low", {
  input <- c("A", "A", "A", "B", "B", "B", "B", "B", "B")
  levels_out <- c("A", "B")
  threshold <- 3
  expect_silent(
    .throwErrorIfRisk(input, levels_out, threshold)
  )
})

test_that(".throwErrorIfRisk throws error when disclosure risk is too high", {
  input <- c("A", "B", "C", "D")
  levels_out <- c("A", "B", "C", "D")
  threshold <- 2
  expect_error(
    .throwErrorIfRisk(input, levels_out, threshold),
    "The levels cannot be returned due to a disclosure risk"
  )
})

test_that(".checkLevelsDisclosureRisk throws error when threshold exceeded", {
  input <- c("A", "A", "B", "B", "C", "D")
  levels_out <- c("A", "B", "C", "D")
  expect_error(
    .checkLevelsDisclosureRisk(input, levels_out),
    "The levels cannot be returned due to a disclosure risk"
  )
})

test_that(".checkLevelsDisclosureRisk does not throw error when within threshold", {
  input <- c("A", "A", "A", "A", "B", "B", "B", "B", "B")
  levels_out <- c("A", "B")
  expect_silent(
    .checkLevelsDisclosureRisk(input, levels_out)
  )
})
