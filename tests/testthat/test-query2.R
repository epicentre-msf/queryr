test_that("query2 works as expected", {

  queryr_cols_base_orig <- getOption("queryr_cols_base")
  options(queryr_cols_base = NULL)

  data(ll)
  data(sll)

  # test simple query
  q1 <- query2(
    ll,
    sll,
    cols_base1 = c(id, site),
    join_type = "inner",
    join_by = c("id" = "tc_id"),
    cond3 = !outcome %in% "Died" & sll_outcome == "Died"
  )

  expect_is(q1, "data.frame")
  expect_true(all(!q1$value1 %in% "Died"))

  # add base column from data2
  q2 <- query2(
    ll,
    sll,
    cols_base1 = c(id, site),
    cols_base2 = sll_id,
    join_type = "inner",
    join_by = c("id" = "tc_id"),
    cond3 = !outcome %in% "Died" & sll_outcome == "Died"
  )

  expect_equal(nrow(q1), nrow(q2))
  expect_true("sll_id" %in% names(q2))

  # test pivot_long = FALSE
  q3 <- query2(
    ll,
    sll,
    cols_base1 = c(id, site),
    cols_base2 = sll_id,
    join_type = "inner",
    join_by = c("id" = "tc_id"),
    cond3 = !outcome %in% "Died" & sll_outcome == "Died",
    pivot_long = FALSE
  )

  expect_true(all(c("outcome", "sll_outcome") %in% names(q3)))

  # test query with cond1 and cond2
  q4 <- query2(
    ll,
    sll,
    status != "Confirmed",
    sll_status == "Confirmed",
    cols_base1 = c(id, site),
    cols_base2 = sll_id,
    join_type = "inner",
    join_by = c("id" = "tc_id"),
    pivot_long = FALSE
  )

  expect_true(all(c("status", "sll_status") %in% names(q4)))

  # test query with cond1 and cond3
  q5 <- query2(
    ll,
    sll,
    status == "Confirmed",
    cols_base1 = c(id, site),
    cols_base2 = sll_id,
    join_type = "left",
    join_by = c("id" = "tc_id"),
    cond3 = outcome != sll_outcome,
    pivot_long = FALSE
  )

  expect_true(all(c("status", "outcome", "sll_outcome") %in% names(q5)))

  # test left join
  q6 <- query2(
    ll,
    sll,
    !status %in% "Confirmed",
    cols_base1 = c(id, site),
    join_type = "left",
    join_by = c("id" = "tc_id")
  )

  expect_equal(nrow(q6), nrow(subset(ll, !status %in% "Confirmed")))

  # reset option queryr_cols_base to original value
  options(queryr_cols_base = queryr_cols_base_orig)

})
