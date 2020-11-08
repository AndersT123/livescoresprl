test_that("get_match_request fails with bad matchday input", {
  expect_error(get_match_request(key = "key", 21))
})
test_that("get_home_teams returns a character", {
  expect_type(get_home_teams(livescoresprl::content_w1), "character")
})
