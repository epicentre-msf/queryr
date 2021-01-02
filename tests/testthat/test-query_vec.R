context("query_vec")

test_that("Test query_vec with x as a single data frame", {

  data("ll")
  data("ll_queries")

  # ensure option queryr_cols_base NULL prior to running tests
  queryr_cols_base_orig <- getOption("queryr_cols_base")
  options(queryr_cols_base = NULL)

  # test for equal results between query_vec() and query()
  expect_equal(
    query_vec(ll, "date_exit < date_admit")[,-1],
    query(ll, date_exit < date_admit)
  )

  # test vectorization
  q_vec <- query_vec(ll, ll_queries$query, name = ll_queries$query_id)

  expect_setequal(
    c("ID_01", "DATES_01", "DATES_02", "FACTORS_01", "LOGIC_01"),
    q_vec$query_id
  )

  # test vectorization with missing name arg
  q_vec_unnamed <- query_vec(ll, ll_queries$query)

  expect_setequal(
    q_vec_unnamed$query_id,
    ll_queries$query
  )

  # test arg cols_base
  expect_equal(
    query_vec(ll, "date_exit < date_admit", cols_base = c(id, site))[,-1],
    query(ll, date_exit < date_admit, cols_base = c(id, site))
  )

  # test if as_chr = TRUE, value cols retain original class
  q_chr <- query_vec(ll, "date_exit < date_admit", as_chr = TRUE)
  expect_is(q_chr$value1, "character")
  expect_is(q_chr$value2, "character")

  # test if as_chr = FALSE, value cols retain original class
  q_dates <- query_vec(ll, "date_exit < date_admit", as_chr = FALSE)
  expect_is(q_dates$value1, "Date")
  expect_is(q_dates$value2, "Date")

  # test arg name_col
  q_name_col <- query_vec(ll, "date_exit < date_admit", name_col = "blah")
  expect_true("blah" %in% names(q_name_col))

  # test option queryr_cols_base
  options(queryr_cols_base = quote(id:site))
  q_opt <- query_vec(ll, "date_exit < date_admit")
  expect_true(all(c("id", "site") %in% names(q_opt)))

  # reset option queryr_cols_base to original value
  options(queryr_cols_base = queryr_cols_base_orig)
})



test_that("Test query_vec with x as a list of data frames", {

  data("ll")
  data("sll")
  x <- list(ipd = ll, icu = sll)

  # ensure option queryr_cols_base NULL prior to running tests
  queryr_cols_base_orig <- getOption("queryr_cols_base")
  options(queryr_cols_base = NULL)

  # test for equal results between query_vec() and query_list()
  expect_equal(
    query_vec(x, "date_exit < date_admit", element = "ipd")[,-1],
    query_list(x, date_exit < date_admit, element = "ipd")
  )

  # test vectorization
  q_vec <- query_vec(
    x,
    cond = c("age > 50", "lab_result == \"Positive\""),
    element = c("ipd", "ipd"),
    name = c("q1", "q2")
  )

  expect_setequal(
    c("q1", "q2"),
    q_vec$query_id
  )

  # test vectorization with missing name arg
  q_vec_unnamed <- query_vec(
    x,
    cond = c("age > 50", "lab_result == \"Positive\""),
    element = c("ipd", "ipd")
  )

  expect_setequal(
    q_vec_unnamed$query_id,
    c("age > 50", "lab_result == \"Positive\"")
  )

  # test query expression referencing variables in multiple forms
  q_vec_mult <- query_vec(
    x,
    cond = c("age > 50", "age > 50 & sll_status == \"Confirmed\""),
    element = c("ipd", "ipd"),
    cols_base = c(id, site),
    join_by = c("id" = "tc_id")
  )

  expect_setequal(
    q_vec_mult$query_id,
    c("age > 50", "age > 50 & sll_status == \"Confirmed\"")
  )

  # test arg cols_base
  expect_equal(
    query_vec(x, "date_exit < date_admit", element = "ipd", cols_base = c(id, site))[,-1],
    query_list(x, date_exit < date_admit, element = "ipd", cols_base = c(id, site))
  )

  # test option queryr_cols_base
  options(queryr_cols_base = quote(id:site))
  q_opt <- query_vec(x, "date_exit < date_admit", element = "ipd")
  expect_true(all(c("id", "site") %in% names(q_opt)))

  # reset option queryr_cols_base to original value
  options(queryr_cols_base = queryr_cols_base_orig)
})
