#' Data validation queries vectorized over multiple query expressions
#'
#' @description
#' Data validation queries as in [`query`], but vectorized over a set of query
#' expressions in string format (and optionally a corresponding vector of query
#' names/IDs). Results of the multiple queries are stacked and returned in a
#' single tidy data frame, with columns referenced in the query expressions
#' pivoted to long-form (e.g. "variable1", "value1", "variable2", "value2",
#' ...).
#'
#' @param data Data frame to query
#' @param cond Character vector of expressions to evaluate with respect to
#'   variables within `data`.
#' @param name (Optional) Character vector giving query names/IDs for each of
#'   the expressions within `cond`. If missing the expressions themselves (in
#'   string format) are used as names.
#' @param cols_base (Optional) Tidy-selection of other columns within `data` to
#'   retain in the final output. Can be set for an entire session using option
#'   "queryr_cols_base", e.g. `options(queryr_cols_base = quote(id:site))`.
#' @param name_col Column name for the query names/IDs. Defaults to "query_id".
#' @param pivot_var Prefix for pivoted variable column(s). Defaults to
#'   "variable".
#' @param pivot_val Prefix for pivoted value column(s). Defaults to "value".
#' @param as_chr Logical indicating whether to coerce the columns referenced in
#'   the query expression(s) to character prior to returning. This enables
#'   row-binding multiple queries with variables of different classes. Defaults
#'   to `TRUE`.
#'
#' @return
#' A data frame reflecting the rows of `data` that match the given queries.
#' Returned columns include:
#' - query name/ID column (name taken from argument `name_col`)
#' - (optional) columns matched by argument `cols_base`
#' - columns referenced within the query expressions, pivoted to long form
#'
#' @seealso [`query`]
#'
#' @examples
#' data(ll)          # example dataset, an epidemiological linelist
#' data(ll_queries)  # example data frame defining queries to run on ll
#'
#' # run all queries defined in ll_queries
#' query_vec(
#'   ll,
#'   cond = ll_queries$query,
#'   name = ll_queries$query_id,
#'   cols_base = c(id, site)
#' )
#'
#' @importFrom rlang enquo
#' @importFrom dplyr bind_rows
#' @export query_vec
query_vec <- function(data,
                      cond,
                      name,
                      cols_base,
                      name_col = "query_id",
                      pivot_var = "variable",
                      pivot_val = "value",
                      as_chr = TRUE) {

  ## set arg cols_base
  opt_cols_base <- getOption("queryr_cols_base")

  if (missing(cols_base) & !is.null(opt_cols_base)) {
    cols_base <- opt_cols_base
  }

  cols_base <- rlang::enquo(cols_base)

  ## vectorize query_() over cond
  out_list <- mapply(
    query_,
    cond = cond,
    MoreArgs = list(
      x = data,
      cols_base = cols_base,
      as_chr = as_chr,
      pivot_long = TRUE,
      pivot_var = pivot_var,
      pivot_val = pivot_val
    ),
    SIMPLIFY = FALSE
  )

  ## use query names from arg `name` if given
  if (!missing(name)) {
    names(out_list) <- name
  }

  ## bind list elements and return
  dplyr::bind_rows(
    out_list,
    .id = name_col
  )
}

