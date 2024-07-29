#' Data validation queries vectorized over multiple query expressions
#'
#' @description
#' Data validation queries with [`query`] or [`query_list`], but vectorized over
#' a set of query expressions in string format (and optionally a corresponding
#' vector of query names/IDs). Results of the multiple queries are stacked and
#' returned in a single tidy data frame, with columns referenced in the query
#' expressions pivoted to long-form (e.g. "variable1", "value1", "variable2",
#' "value2", ...).
#'
#' @param x A data frame or a list of data frames to query. If a single data
#'   frame will vectorize with [`query`], whereas given a list of data frames
#'   will use [`query_list`].
#' @param cond Character vector of expressions to evaluate with respect to
#'   variables within `x`.
#' @param element If `x` is a list of data frames, the names or integer indexes
#'   of the focal list element of `x` corresponding to each query expression
#'   (i.e. each element of `cond`). Only used if `x` is a list of data frames
#'   (see [`query_list`]).
#' @param name (Optional) Character vector giving query names/IDs for each of
#'   the expressions within `cond`. If missing the expressions themselves (in
#'   string format) are used as names.
#' @param cols_base (Optional) Tidy-selection of other columns within `x` (or
#'   `x[[element]]`) to retain in the final output. Can be set for an entire
#'   session using option "queryr_cols_base", e.g. `options(queryr_cols_base =
#'   quote(id:site))`.
#' @param name_col Column name for the query names/IDs. Defaults to "query_id".
#' @param join_type If `x` is a list of data frames and `cond` references
#'   variables within elements of `x` apart from `x[[element]]`, what type of
#'   join should be used to join the relevant elements? Options are "left" (the
#'   default) and "inner". Based on dplyr \code{\link[dplyr]{join}} types. Can
#'   specify different join types for different query expressions by passing a
#'   vector the same length as `cond`.
#' @param join_by A character vector of variables to join by, or list of vectors
#'   the same length as `cond`. If the join key columns have different names in
#'   `x[[element]]` and `x[[other]]`, use a named vector. For example, `join_by
#'   = c("a" = "b")` will match `x[[element]]$a` to `x[[other]]$b`. Can specify
#'   different join columns for different query expressions by passing a _list_
#'   of vectors the same length as `cond`.
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
query_vec <- function(x,
                      cond,
                      element,
                      name,
                      cols_base,
                      name_col = "query_id",
                      join_type = "left",
                      join_by = NULL,
                      pivot_var = "variable",
                      pivot_val = "value",
                      as_chr = TRUE) {


  ## set arg cols_base
  opt_cols_base <- getOption("queryr_cols_base")

  if (missing(cols_base) & !is.null(opt_cols_base)) {
    cols_base <- opt_cols_base
  }

  cols_base <- rlang::enquo(cols_base)

  ## arg validation
  if (is_list_of_dfs(x) & missing(element)) {
    stop(
      "If 'x' is a list of data frames argument 'element' must be specified",
      call. = FALSE
    )
  }
  if (!is_list_of_dfs(x) & !missing(element)) {
    warning(
      "Argument 'x' is not a list of data frames so argument 'element' will be ignored",
      call. = FALSE
    )
  }
  if (!is_list_of_dfs(x) & !is.null(join_by)) {
    warning(
      "Argument 'x' is not a list of data frames so argument 'join_by' will be ignored",
      call. = FALSE
    )
  }
  if (!missing(name) && length(name) != length(cond)) {
    stop(
      "If argument 'name' is given it must be the same length as 'cond'",
      call. = FALSE
    )
  }

  ## vectorize with either query_() or query_list_(), depending on arg x
  if (is_list_of_dfs(x)) {
    ## vectorize query_list_() over cond, element, join_type, and join_by

    if (is.list(join_by) && length(join_by) > 1) {

      out_list <- mapply(
        query_list_,
        cond = cond,
        element = element,
        join_type = join_type,
        join_by = join_by,
        MoreArgs = list(
          x = x,
          cols_base = cols_base,
          pivot_long = TRUE,
          pivot_var = pivot_var,
          pivot_val = pivot_val,
          as_chr = as_chr
        ),
        SIMPLIFY = FALSE
      )

    } else {

      # if (length(join_type) == 1) join_type <- rep(join_type, length(cond))
      # if (length(join_by) == 1)   join_by   <- rep(join_by,   length(cond))

      out_list <- mapply(
        query_list_,
        cond = cond,
        element = element,
        MoreArgs = list(
          x = x,
          cols_base = cols_base,
          join_type = join_type,
          join_by = join_by,
          pivot_long = TRUE,
          pivot_var = pivot_var,
          pivot_val = pivot_val,
          as_chr = as_chr
        ),
        SIMPLIFY = FALSE
      )

    }

  } else {
    ## vectorize query_() over cond
    out_list <- mapply(
      query_,
      cond = cond,
      MoreArgs = list(
        x = x,
        cols_base = cols_base,
        as_chr = as_chr,
        pivot_long = TRUE,
        pivot_var = pivot_var,
        pivot_val = pivot_val
      ),
      SIMPLIFY = FALSE
    )
  }

  ## use query names from arg `name` if given
  if (!missing(name)) {
    names(out_list) <- name
  }

  ## bind list elements and return
  dplyr::bind_rows(out_list, .id = name_col)
}



#' @noRd
is_list_of_dfs <- function(x) {
  all(vapply(x, function(x) "data.frame" %in% class(x), FALSE))
}

