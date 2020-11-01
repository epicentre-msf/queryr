#' Data validation queries with tidy, stackable output
#'
#' @description
#' Find observations within a data frame matching a given query (a logical
#' expression relating to one or more variables), and return tidy output that
#' can be stacked across different queries on different variables. Stackability
#' is achieved by pivoting the columns indicated in the query expression to
#' long-form, e.g. "variable1", "value1", "variable2", "value2", ...
#'
#' The query expression can optionally incorporate up to two dot-selectors
#' ("`.x`" and "`.y`"), which each refer to a *set* of variables specified
#' separately using tidy-selection (see section **Using a dot-selector**). If
#' both selectors are used in a given query expression, the sets of variables
#' they respectively match can either be "crossed" such that all combinations
#' are evaluated, or evaluated in parallel.
#'
#' By default, only the data columns referenced in the query expression are
#' returned, but additional columns can optionally be added with argument
#' `cols_base`.
#'
#' @param data A data frame
#' @param cond An expression to evaluate with respect to variables within
#'   `data`. Can specify multiple variables using a dot-selector ("`.x`" and
#'   "`.y`") within the expression (e.g. `.x > 0`) and then separately
#'   specifying the columns that the selector refers to with arguments
#'   `cols_dotx`/`cols_doty`.
#' @param cols_dotx,cols_doty Tidy-selection of one or more columns represented
#'   by a .x or .y selector. Only used if `cond` contains the relevant selector.
#'   See section **Using a dot-selector** below.
#' @param crossed if `cond` contains both a .x and .y selector, should the
#'   variables matched by `cols_dotx` and `cols_doty` be "crossed" such that all
#'   combinations are evaluated (`TRUE`), or should they be evaluated in
#'   parallel (`FALSE`). The latter requires that the number of variables
#'   matched by `cols_dotx` and `cols_doty` is the same. Defaults to `FALSE`.
#' @param cols_base (Optional) Tidy-selection of other columns within `data` to
#'   retain in the output. Can optionally be set for an entire session using
#'   option "queryr_cols_base", e.g. `options(queryr_cols_base =
#'   quote(id:site))`.
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
#' @section Using a dot-selector:
#' A query expression can optionally incorporate up to two dot-selectors
#' ("`.x`" and "`.y`"), which each refer to a *set* of variables specified
#' separately using tidy-selection (arguments `cols_dotx` and `cols_doty`).
#'
#' When `cond` contains a `.x` selector, the query expression is evaluated
#' repeatedly with each relevant variable from `cols_dotx` individually
#' substituted into the `.x` position of the expression. The results of these
#' multiple 'subqueries' are then combined with
#' \code{\link[dplyr:bind_rows]{dplyr::bind_rows}}.
#'
#' If `cond` contains both a .x and .y selector, the sets of variables matched
#' by `cols_dotx` and `cols_doty` respectively can either be "crossed" such that
#' all combinations are evaluated, or evaluated in parallel. Evaluating in
#' parallel requires that the number of variables matched by `cols_dotx` and
#' `cols_doty` is the same.
#'
#' Consider a hypothetical query checking that, if a patient has a particular
#' symptom, the date of onset of that symptom is not missing. E.g. \cr
#' `cond = .x == "Yes" & is.na(.y)` \cr
#' `cols_dotx = c(symptom_fever, symptom_headache)` \cr
#' `cols_doty = c(date_symptom_fever, date_symptom_headache)` \cr
#'
#' If argument `crossed` is `FALSE`, the relevant variables from `cols_dotx` and
#' `cols_doty` will be evaluated in parallel, as in: \cr
#' `has_symptom_fever == "Yes" & is.na(date_symptom_fever)` \cr
#' `has_symptom_headache == "Yes" & is.na(date_symptom_headache)` \cr
#'
#' Conversely, if argument `crossed` is `TRUE`, all combinations of the relevant
#' variables will be evaluated, which for this particular query wouldn't make
#' sense: \cr
#' `symptom_fever == "Yes" & is.na(date_symptom_fever)` \cr
#' `symptom_fever == "Yes" & is.na(date_symptom_headache) # not relevant` \cr
#' `symptom_headache == "Yes" & is.na(date_symptom_fever) # not relevant` \cr
#' `symptom_headache == "Yes" & is.na(date_symptom_headache)` \cr
#'
#' Note that if a dot-selector is used with argument `pivot_long = FALSE`, the
#' row-binding of multiple subqueries may result in a sparse output with respect
#' to the variables represented by the dot-selector, because for each subquery
#' only the columns matched by expression `cond` are returned.
#'
#' @return
#' A data frame reflecting the rows of `data` that match the given query.
#' Returned columns include:
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
#' # incorporate an external object into the query expression
#' lab_result_valid <- c("Positive", "Negative", "Inc.", NA)
#'
#' query(
#'   ll,
#'   !lab_result %in% lab_result_valid,
#'   cols_base = id:site,
#' )
#'
#' @importFrom dplyr select bind_rows group_by_all count ungroup
#' @importFrom rlang `!!` enquo
#' @export query
query <- function(data,
                  cond,
                  cols_dotx,
                  cols_doty,
                  crossed = FALSE,
                  cols_base,
                  pivot_long = TRUE,
                  pivot_var = "variable",
                  pivot_val = "value",
                  as_chr = TRUE,
                  count = FALSE) {

  ## set arg cols_base
  opt_cols_base <- getOption("queryr_cols_base")

  if (missing(cols_base) & !is.null(opt_cols_base)) {
    cols_base <- opt_cols_base
  }

  cols_base <- enquo(cols_base)

  ## check for .x selector
  has_dotx <- ".x" %in% all.vars(substitute(cond))
  missing_cols_dotx <- missing(cols_dotx)

  if (has_dotx & missing_cols_dotx) {
    stop("`cond` includes a .x selector but `cols_dotx` is missing", call. = FALSE)
  } else if (!has_dotx & !missing_cols_dotx) {
    warning("`cols_dotx` is specified but `cond` does not include a .x selector", call. = FALSE)
  }

  ## check for .y selector
  has_doty <- ".y" %in% all.vars(substitute(cond))
  missing_cols_doty <- missing(cols_doty)

  if (has_doty & missing_cols_doty) {
    stop("`cond` includes a .y selector but `cols_doty` is missing", call. = FALSE)
  } else if (!has_doty & !missing_cols_doty) {
    warning("`cols_doty` is specified but `cond` does not include a .y selector", call. = FALSE)
  }

  if (has_dotx) {
    ## if query expression uses .x and/or .y selector

    # evaluate cols_dotx
    cols_dotx <- names(dplyr::select(data, !!enquo(cols_dotx)))

    if (has_doty) {
      cols_doty <- names(dplyr::select(data, !!enquo(cols_doty)))

      if (crossed) {
        cols_exp <- expand.grid(
          cols_doty = cols_doty,
          cols_dotx = cols_dotx,
          stringsAsFactors = FALSE
        )
        cols_dotx <- cols_exp$cols_dotx
        cols_doty <- cols_exp$cols_doty

      } else {
        n_cols_dotx <- length(cols_dotx)
        n_cols_doty <- length(cols_doty)

        if (n_cols_dotx != n_cols_doty) {
          stop("`crossed` is FALSE but the number of columns matched by ",
               "`cols_dotx` (", n_cols_dotx, ") differs from the number ",
               "matched by `cols_doty` (", n_cols_doty, ")", call. = FALSE)
        }
      }
    } else {
      cols_doty <- rep(".y", length(cols_dotx))
    }

    # prep query expression for substitution of each cols_dotx into .x
    cond_base <- as.expression(substitute(cond))[[1]]

    # run separate sub-query for each variable in cols_dotx
    subqueries <- list()

    for (i in seq_along(cols_dotx)) {

      # substitute cols_dotx[i] with .x and cols_doty[i] with .y
      cond_swap <- do.call(
        "substitute",
        list(
          cond_base,
          list(.x = str2lang(cols_dotx[i]), .y = str2lang(cols_doty[i]))
        )
      )

      subqueries[[i]] <- query_(
        data,
        cond = deparse(cond_swap),
        cols_base = cols_base,
        as_chr = as_chr,
        pivot_long = pivot_long,
        pivot_var = pivot_var,
        pivot_val = pivot_val
      )
    }

    out <- dplyr::bind_rows(subqueries)

  } else {
    ## else no .x selector

    out <- query_(
      data,
      cond = deparse(substitute(cond)),
      cols_base = cols_base,
      as_chr = as_chr,
      pivot_long = pivot_long,
      pivot_var = pivot_var,
      pivot_val = pivot_val
    )
  }

  # summarize by counting unique combinations across all variables
  if (count) {
    out <- ungroup(count(group_by_all(out)))
  }

  return(out)
}



#' @noRd
#' @importFrom dplyr select any_of bind_cols
#' @importFrom rlang `!!` enquo
query_ <- function(x,
                   cond,
                   cols_base,
                   as_chr,
                   pivot_long,
                   pivot_var,
                   pivot_val,
                   cols_extra = NULL,
                   vars_cond_extra = NULL) {

  cond <- parse(text = cond)

  if (missing(cols_base)) {
    cols_base <- NULL
  }

  # parse query expression to find variable name also in `x`
  vars_cond <- c(vars_cond_extra, all.vars(substitute(cond)))
  vars_cond <- intersect(vars_cond, names(x))

  # evaluate query expression
  rows <- eval(substitute(cond), x, parent.frame(n = 2))
  rows[is.na(rows)] <- FALSE
  xsub <- x[rows, , drop = FALSE]

  # limit to variables within query expression
  out <- xsub[,vars_cond, drop = FALSE]

  # convert variables within query expression to character
  if (as_chr) {
    for (j in vars_cond) {
      out[[j]] <- as.character(out[[j]])
    }
  }

  # convert variables within query expression to long-form
  if (pivot_long & length(vars_cond) > 0) {
    out <- pivot_simple(out, pivot_var, pivot_val)
  }

  # combine cols_base, cols_extra, and cols referenced in cond
  cols_add <- setdiff(cols_extra, names(out))
  out_id <- dplyr::select(xsub, !!cols_base, dplyr::any_of(cols_add))
  out <- dplyr::bind_cols(out_id, out)

  row.names(out) <- NULL

  return(out)
}




#' @noRd
#' @importFrom stats setNames
pivot_simple <- function(df,
                         pivot_var,
                         pivot_val) {

  vars <- names(df)

  names_var <- paste0(pivot_var, seq_along(vars))
  names_val <- paste0(pivot_val, seq_along(vars))
  column_order <- c(rbind(names_var, names_val))

  df_vals <- setNames(df, names_val)

  df_vars <- as.data.frame(
    lapply(setNames(vars, names_var), rep, times = nrow(df)),
    stringsAsFactors = FALSE
  )

  cbind(df_vals, df_vars)[,column_order, drop = FALSE]
}

