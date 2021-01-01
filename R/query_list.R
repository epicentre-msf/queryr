#' Data validation queries across a list of data frames
#'
#' @description
#' Find observations matching a query that concerns one or more data frames
#' within a list of data frames, and return tidy, stackable output. Like
#' [`query`] but enables query expressions that reference variables in multiple
#' data frames.
#'
#' If the query expression references variables from data frames (i.e. list
#' elements) other than the focal element, the relevant variable(s) will be
#' joined to the focal element before the query expression is evaluated, see
#' arguments `join_type` and `join_by` below.
#'
#' @inheritParams query
#'
#' @param x A list of data frames
#' @param cond Expression to evaluate with respect to one or more variables in
#'   one or more of the data frames within `x`.
#' @param element Name or integer index of the focal list element of `x` for the
#'   given query. If the query expression `cond` references variables from list
#'   elements apart from `element`, the relevant variable(s) will be joined to
#'   `x[[element]]` before the query expression is evaluated, based on the
#'   `join_type` and `join_by` arguments described below.
#' @param join_type If `cond` references variables within elements of `x` apart
#'   from `x[[element]]`, what type of join should be used to join the relevant
#'   elements? Options are "left" (the default) and "inner". Based on dplyr
#'   \code{\link[dplyr]{join}} types.
#' @param join_by A character vector of variables to join by. If the join key
#'   columns have different names in `x[[element]]` and `x[[other]]`, use a
#'   named vector. For example, `join_by = c("a" = "b")` will match
#'   `x[[element]]$a` to `x[[other]]$b`.
#'
#' @return
#' A data frame reflecting the rows of `x[[element]]` that match the given
#' query. Returned columns include:
#' - (optional) columns matched by argument `cols_base`
#' - columns referenced within the query expression (pivoted to long form by
#'   default)
#'
#' @importFrom dplyr left_join inner_join
#' @importFrom rlang enquo
#' @export query_list
query_list <- function(x,
                       cond,
                       element,
                       cols_base,
                       join_type = "left",
                       join_by,
                       pivot_long = TRUE,
                       pivot_var = "variable",
                       pivot_val = "value",
                       as_chr = TRUE) {


  ## set cols_base
  opt_cols_base <- getOption("queryr_cols_base")

  if (missing(cols_base) & !is.null(opt_cols_base)) {
    cols_base <- opt_cols_base
  }

  cols_base <- rlang::enquo(cols_base)

  ## set join
  join_type <- match.arg(join_type, c("inner", "left"))

  join_fn <- switch(
    join_type,
    "inner" = dplyr::inner_join,
    "left" = dplyr::left_join
  )

  ## primary element
  data <- x[[element]]

  ## join in relevant vars from other list elements (if any)
  vars_cond <- all.vars(substitute(cond))
  vars_external <- setdiff(vars_cond, names(data))
  vars_external_els <- vapply(vars_external, find_element, "", x = x)

  for (i in unique(vars_external_els)) { # for each external element...
    vars_el <- vars_external[vars_external_els == i]
    data_join <- x[[i]][, c(join_by, vars_el), drop = FALSE]
    data <- join_fn(data, data_join, by = join_by)
  }

  ## execute query
  query_(
    x = data,
    cond = deparse(substitute(cond)),
    cols_base = cols_base,
    as_chr = as_chr,
    pivot_long = pivot_long,
    pivot_var = pivot_var,
    pivot_val = pivot_val
  )
}



#' @noRd
find_element <- function(x, var) {
  els <- vapply(x, function(x) var %in% names(x), FALSE)
  if (sum(els) > 1) {
    stop("Variable ", var, " found in more than 1 element of `x`")
  } else if (sum(els) == 1) {
    out <- names(els[els])
  } else {
    out <- NA_character_
  }
  out
}

