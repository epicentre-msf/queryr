#' Data validation queries with tidy, stackable output
#'
#' @description
#' Find observations within a data frame matching a given query (a logical
#' expression relating to one or more variables), and return tidy output that
#' can be stacked across different queries on different variables. Stackability
#' is achieved by pivoting the columns indicated in the query expression to
#' long-form, e.g. "variable1", "value1", "variable2", "value2", ...
#'
#' The query expression can optionally incorporate a "`.x`" selector to refer to
#' a set of multiple variables, which is specified separately using
#' tidy-selection (see section **Using a .x selector**).
#'
#' By default, only the data columns referenced in the query expression are
#' returned, but additional columns can optionally be added with argument
#' `cols_base`.
#'
#' @param data A data frame
#' @param cond An expression to evaluate with respect to variables within
#'   `data`. Can specify multiple variables using a "`.x`" selector within the
#'   expression (e.g. `.x > 0`) and then specifying the columns that `.x` refers
#'   to with argument `cols_dotx`.
#' @param cols_dotx Tidy-selection of one or more columns represented by a .x
#'   selector. Only used if `cond` contains a .x selector. See section **Using a
#'   .x selector** below.
#' @param cols_base (Optional) Tidy-selection of other columns within `data` to
#'   retain in the output. Can optionally be set for an entire session using
#'   option "queryr_cols_base", e.g. `options(queryr_cols_base =
#'   quote(id:site))`.
#' @param qcol (Optional) Name of query identifier column. Defaults to "query".
#'   Only appended if argument `qval` is specified.
#' @param qval (Optional) Value of query identifier column
#' @param pivot_long Logical indicating whether to pivot the variables
#'   referenced within `cond` to a long (i.e. stackable) format, with default
#'   column names "variable1", "value1", "variable2", "value2", ... Defaults to
#'   `TRUE`.
#' @param pivot_var Prefix for pivoted variable column(s). Defaults to
#'   "variable". Only used if `pivot_long = TRUE`.
#' @param pivot_val Prefix for pivoted value column(s). Defaults to "value".
#'   Only used if `pivot_long = TRUE`.
#' @param as_chr Logical indicating whether to coerce the columns referenced in
#'   the query expression `cond` to character prior to returning. This enables
#'   row-binding multiple queries with variables of different classes, but is
#'   only important if `pivot_long = TRUE`. Defaults to `TRUE`.
#' @param count Logical indicating whether to summarize the output by counting
#'   the number of unique combinations across all returned columns (with count
#'   column "n"). Defaults to `FALSE`.
#'
#' @section Using a .x selector:
#' A "`.x`" selector within the query expression `cond` represents a set of
#' multiple variables, which are specified separately using argument
#' `cols_dotx`. When `cond` contains a `.x` selector, the query expression is
#' evaluated repeatedly with each relevant variable from `cols_dotx`
#' individually substituted into the `.x` position of the expression. The
#' results of these multiple 'subqueries' are then combined with
#' \code{\link[dplyr:bind_rows]{dplyr::bind_rows}}.
#'
#' Note that if a .x selector is used with argument `pivot_long = FALSE`, the
#' row-binding of multiple subqueries may result in a sparse output with
#' respect to the variables represented by `.x`, because for each subquery only
#' the columns matched by expression `cond` are returned.
#'
#' @return
#' A data frame reflecting the rows of `data` that match the given query.
#' Returned columns include:
#' - (optional) query ID column `qcol` (if `qval` is specified)
#' - (optional) columns matched by argument `cols_base`
#' - columns referenced within the query expression (pivoted to long form by
#'   default)
#' - (optional) count column "n" (if `count` = TRUE)
#'
#' @examples
#' # load example dataset, an epidemiological 'linelist'
#' data(ll)
#'
#' # find observations where date_exit is earlier than date_admit
#' query(
#'   ll,
#'   date_exit < date_admit,
#'   cols_base = id:site
#' )
#'
#' # find any date value in the future using a .x column selector
#' query(
#'   ll,
#'   .x > Sys.Date(),
#'   cols_dotx = starts_with("date"),
#'   cols_base = id:site
#' )
#'
#' # include a query identifier column in the output
#' query(
#'   ll,
#'   .x > Sys.Date(),
#'   cols_dotx = starts_with("date"),
#'   cols_base = id:site,
#'   qval = "DATES_01"
#' )
#'
#' # incorporate an external object into the query expression
#' valid_age_units <- c("Years", "Months", "Weeks", "Days")
#'
#' query(
#'   ll,
#'   !age_unit %in% valid_age_units,
#'   cols_base = id:site,
#' )
#'
#' @importFrom dplyr select all_of bind_rows group_by_all count ungroup
#' @importFrom rlang `!!` enquo
#' @export query
query <- function(data,
                  cond,
                  cols_dotx,
                  cols_base,
                  qcol = "query",
                  qval = NULL,
                  pivot_long = TRUE,
                  pivot_var = "variable",
                  pivot_val = "value",
                  as_chr = TRUE,
                  count = FALSE) {

  opt_cols_base <- getOption("queryr_cols_base")

  if (missing(cols_base) & !is.null(opt_cols_base)) {
    cols_base <- opt_cols_base
  }

  cols_base <- enquo(cols_base)

  if (!missing(cols_dotx)) {

    cols_dotx <- enquo(cols_dotx)
    vars_data <- names(dplyr::select(data, !!cols_dotx))
    out <- list()

    for (i in seq_along(vars_data)) {

      var <- str2lang(vars_data[i])
      cond_swap <- as.expression(substitute(cond))
      cond_swap <- do.call("substitute", list(cond_swap[[1]], list(.x = var)))
      cond_swap <- deparse(cond_swap)

      out[[i]] <- query_helper(
        data,
        cond = cond_swap,
        cols_base = cols_base,
        qcol = qcol,
        qval = qval,
        as_chr = as_chr,
        pivot_long = pivot_long,
        pivot_var = pivot_var,
        pivot_val = pivot_val
      )
    }

    out <- dplyr::bind_rows(out)

  } else {

    cond <- deparse(substitute(cond))

    out <- query_helper(
      data,
      cond = cond,
      cols_base = cols_base,
      qcol = qcol,
      qval = qval,
      as_chr = as_chr,
      pivot_long = pivot_long,
      pivot_var = pivot_var,
      pivot_val = pivot_val
    )
  }

  if (count) {
    out <- ungroup(count(group_by_all(out)))
  }

  return(out)
}


#' @noRd
#' @importFrom dplyr select all_of
#' @importFrom rlang `!!` enquo
#' @importFrom tidyr pivot_longer
query_helper <- function(x,
                         cond,
                         cols_base,
                         qcol,
                         qval,
                         as_chr,
                         pivot_long,
                         pivot_var,
                         pivot_val) {

  if (!is.call(substitute(cond))) cond <- parse(text = cond)
  if (is.call(substitute(cols_base))) cols_base <- enquo(cols_base)

  vars_cond <- unique(intersect(all.vars(substitute(cond)), names(x)))

  rows <- eval(substitute(cond), x, parent.frame(n = 2))
  rows[is.na(rows)] <- FALSE
  xsub <- x[rows, , drop = FALSE]

  out <- dplyr::select(xsub, all_of(vars_cond))

  if (as_chr) {
    for (j in vars_cond) {
      out[[j]] <- as.character(out[[j]])
    }
  }

  if (pivot_long) {
    for (i in seq_along(vars_cond)) {
      name_var <- paste0(pivot_var, i)
      name_val <- paste0(pivot_val, i)
      out <- tidyr::pivot_longer(
        out,
        cols = vars_cond[i],
        names_to = name_var,
        values_to = name_val
      )
    }
  }

  if (!missing(cols_base)) {
    out_id <- dplyr::select(xsub, !!cols_base)
    out <- dplyr::bind_cols(out_id, out)
  }

  if (!is.null(qval)) {
    cols_orig <- names(out)
    out[[qcol]] <- rep(qval, nrow(out))
    out <- out[,c(qcol, cols_orig), drop = FALSE]
  }

  row.names(out) <- NULL

  return(out)
}

