test_that("query_list works as expected", {

  queryr_cols_base_orig <- getOption("queryr_cols_base")
  options(queryr_cols_base = NULL)

  data(ll)
  data(sll)

  dat_list <- list(ipd = ll, icu = sll)


  # test simple query on single element
  q1 <- query_list(
    dat_list,
    cond = age > 50,
    element = "ipd",
    cols_base = c(id, site),
    pivot_long = FALSE
  )

  expect_is(q1, "data.frame")
  expect_true(all(q1$age > 50))

  # test integer index rather than name for argument element
  q2 <- query_list(
    dat_list,
    cond = age > 50,
    element = 1,
    cols_base = c(id, site),
    pivot_long = FALSE
  )

  expect_identical(q1, q2)


  # test query requiring join across elements
  q3 <- query_list(
    dat_list,
    cond = age > 50 & sll_status == "Confirmed",
    element = "ipd",
    cols_base = c(id, site),
    join_by = c("id" = "tc_id"),
    pivot_long = FALSE
  )

  expect_true(all(q3$age > 50 & q3$sll_status == "Confirmed"))

  # test query where *all* relevant variables are outside focal element
  q4 <- query_list(
    dat_list,
    sll_status == "Confirmed" & !sll_outcome %in% "Died",
    element = 1,
    cols_base = c(id, site),
    join_by = c("id" = "tc_id"),
    pivot_long = FALSE
  )

  expect_true(all(q4$sll_status == "Confirmed" & !q4$sll_outcome %in% "Died"))

  # test option queryr_cols_base
  options(queryr_cols_base = quote(id:site))

  q5 <- query_list(
    dat_list,
    cond = age > 50 & sll_status == "Confirmed",
    element = "ipd",
    join_by = c("id" = "tc_id"),
    pivot_long = FALSE
  )

  expect_true(all(c("id", "site") %in% names(q5)))

  # reset option queryr_cols_base to original value
  options(queryr_cols_base = queryr_cols_base_orig)

})
