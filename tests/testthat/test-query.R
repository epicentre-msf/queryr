test_that("query works as expected", {

  data(ll)

  # test basic query
  q1 <- query(ll, date_exit < date_admit)
  expect_is(q1, "data.frame")
  expect_equal(names(q1), c("variable1", "value1", "variable2", "value2"))
  expect_true(all(q1$value1 < q1$value2))

  # test arg cols_base
  q2 <- query(ll, date_exit < date_admit, cols_base = c(id, site))
  expect_equal(ncol(q2), 6)

  q3 <- query(ll, date_exit < date_admit, cols_base = all_of(c("id", "site")))
  expect_equal(q2, q3)

  # test .x selector
  q4 <- query(ll, .x >= as.Date("2020-06-01"), cols_dotx = starts_with("date"))
  expect_true(length(unique(q4$variable1)) > 1L)

  # test args qcol, qval
  q5 <- query(ll, is.na(age_unit), qcol = "test", qval = "ABCD")
  expect_true(all(q5$test == "ABCD"))

  # test ref to external object within query expression
  age_unit_valid <- c("Years", "Months", "Weeks", "Days")
  q6 <- query(ll, !age_unit %in% age_unit_valid, cols_base = id:site)
  expect_true(all(!q6$value1 %in% age_unit_valid))

  # test pivot_long arg
  q7 <- query(ll, date_exit < date_admit, pivot_long = FALSE)
  expect_equal(names(q7), c("date_exit", "date_admit"))

  q8 <- query(ll, date_exit < date_admit, pivot_var = "name", pivot_val = "val")
  expect_equal(names(q8), c("name1", "val1", "name2", "val2"))

  # test arg as_chr
  q9 <- query(ll, date_exit < date_admit, as_chr = FALSE)
  expect_is(q9$value1, "Date")
  expect_is(q9$value2, "Date")

  q10 <- query(ll, date_exit < date_admit, as_chr = TRUE)
  expect_is(q10$value1, "character")
  expect_is(q10$value2, "character")

  # test arg count
  q11 <- query(ll, is.na(.x), cols_dotx = starts_with("date"), count = TRUE)
  expect_equal(names(q11), c("variable1", "value1", "n"))

  # test option queryr_cols_base
  options(queryr_cols_base = quote(id:site))
  q12 <- query(ll, date_exit < date_admit)
  expect_true(all(c("id", "site") %in% names(q12)))
  options(queryr_cols_base = NULL) # reset option queryr_cols_base to NULL
})
