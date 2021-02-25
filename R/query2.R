#' Data validation queries across two data frames
#'
#' @description
#' Find observations matching a query that concerns two data frames, and return
#' tidy, stackable output. Entails three steps:
#'
#' 1. separately query each of the two data frames using
#' \code{\link{query}}
#' 2. combine the resulting query outputs based on a given join type (semi,
#' anti, left, or inner)
#' 3. execute a third query on the joined output
#'
#' Each of the query steps is optional — unspecified query expressions are
#' replaced with `TRUE` such that all rows of the relevant input are returned.
#'
#' @param data1 Data frame to query (#1)
#' @param data2 Data frame to query (#2)
#' @param cond1 (Optional) Expression to evaluate with respect to `data1`. If
#'   missing will be set to `TRUE` to select all rows.
#' @param cond2 (Optional) Expression to evaluate with respect to `data2`. If
#'   missing will be set to `TRUE` to select all rows.
#' @param cols_base1 (Optional) Tidy-selection of other columns within `data1`
#'   to retain in the final output. Can be set for an entire session using
#'   option "queryr_cols_base", e.g. `options(queryr_cols_base =
#'   quote(id:site))`.
#' @param cols_base2 (Optional) Tidy-selection of other columns within `data2`
#'   to retain in the final output.
#' @param join_type How to join the output from the two initial queries ("semi", "anti",
#'   "left", or "inner"). Based on dplyr \code{\link[dplyr]{join}} types.
#' @param join_by A character vector of variables to join by. If the join key
#'   columns have different names in `data1` and `data2`, use a named vector.
#'   For example, `by = c("a" = "b")` will match `data1$a` to `data2$b`.
#' @param cond3 (Optional) Expression to evaluate with respect to the joined
#'   output of the two initial queries. If missing will be set to `TRUE` to
#'   select all rows.
#'
#'   Note that if `join_type` is a filtering join ("anti" or "semi"), only
#'   variables from `data1` can be referenced in `cond3` (referencing a variable
#'   that only exists in `data2` will result in an error).
#'
#'   If `join_type` is instead a mutating join ("left" or "inner"), all
#'   variables from `data1` and `data2` will be available to `cond3`, even if
#'   not otherwise referenced with `cond1`/`cond2` or `cols_base1`/`cols_base2`.
#'
#' @param pivot_long Logical indicating whether to pivot the variables
#'   referenced within the query expression(s) to a long (i.e. stackable)
#'   format, with default column names "variable1", "value1", "variable2",
#'   "value2", ... Defaults to `TRUE`. If `cond3` is specified and `pivot_long`
#'   is `TRUE`, the pivot happens only in the final query (i.e. `cond3`).
#' @param pivot_var Prefix for pivoted variable column(s). Defaults to
#'   "variable". Only used if `pivot_long = TRUE`.
#' @param pivot_val Prefix for pivoted value column(s). Defaults to "value".
#'   Only used if `pivot_long = TRUE`.
#' @param as_chr Logical indicating whether to coerce the columns referenced in
#'   the query expression(s) to character prior to returning. This enables
#'   row-binding multiple queries with variables of different classes, but is
#'   only important if `pivot_long = TRUE`. Defaults to `TRUE`.
#'
#' @return
#' A data frame reflecting the rows of `data1` that match the given
#' query. Returned columns include:
#' - Columns matched by argument `cols_base1`
#' - Columns matched by argument `cols_base2` (only if join type is "left" or
#' "inner")
#' - Columns referenced within the relevant condition statements (pivoted to
#' long form by default). \cr
#'
#'   If the join type is a mutating join ("left" or "inner"), variables from
#'   `data1` or `data2` referenced in *any* of the condition statements
#'   (`cond1`, `cond2`, or `cond3`) will appear in the output. However, with a
#'   filtering join ("anti" or "semi") only variables from `data1` will appear
#'   in the output.
#'
#' @examples
#' # example datasets: two related epidemiological linelists
#' data(ll)  # ll from treatment center (all cases, confirmed and non-confirmed)
#' data(sll) # summary linelist (only confirmed/probable cases)
#'
#' # find patients in ll that don't appear in sll
#' query2(
#'   ll,
#'   sll,
#'   cols_base1 = c(id, site, status),
#'   join_type = "anti",
#'   join_by = c("id" = "tc_id")
#' )
#'
#' # find patients with different outcome status in ll vs sll
#' query2(
#'   ll,
#'   sll,
#'   cols_base1 = id:site,
#'   join_type = "inner",
#'   join_by = c("id" = "tc_id"),
#'   cond3 = status != sll_status
#' )
#'
#' @importFrom dplyr anti_join semi_join left_join inner_join rename all_of
#' @importFrom rlang enquo
#' @export query2
query2 <- function(data1,
                   data2,
                   cond1,
                   cond2,
                   cols_base1,
                   cols_base2,
                   join_type,
                   join_by,
                   cond3,
                   pivot_long = TRUE,
                   pivot_var = "variable",
                   pivot_val = "value",
                   as_chr = TRUE) {

  ## set cols_base
  opt_cols_base <- getOption("queryr_cols_base")

  if (missing(cols_base1) & !is.null(opt_cols_base)) {
    cols_enquo_base1 <- opt_cols_base
  } else {
    cols_enquo_base1 <- rlang::enquo(cols_base1)
  }

  cols_enquo_base2 <- rlang::enquo(cols_base2)

  ## set join
  join_type <- match.arg(join_type, c("semi", "anti", "left", "inner"))

  join_fn <- switch(
    join_type,
    "semi" = dplyr::semi_join,
    "anti" = dplyr::anti_join,
    "left" = dplyr::left_join,
    "inner" = dplyr::inner_join,
  )

  ## set cond args
  if (missing(cond1)) cond1 <- quote(TRUE)
  if (missing(cond2)) cond2 <- quote(TRUE)
  if (missing(cond3)) cond3 <- quote(TRUE)

  ## vars reference in query expressions or join_by
  vars_cond1 <- all.vars(substitute(cond1))
  vars_cond2 <- all.vars(substitute(cond2))
  vars_cond3 <- all.vars(substitute(cond3))

  vars_by1 <- extract_by(join_by)
  vars_by2 <- join_by

  cols_extra1 <- unique(c(vars_by1, vars_cond3))
  cols_extra2 <- unique(c(vars_by2, vars_cond3))

  ## run individual queries on data1 and data2, respectively
  q1 <- query_(
    data1,
    cond = deparse(substitute(cond1), width.cutoff = 500L),
    cols_base = cols_enquo_base1,
    pivot_long = FALSE,
    as_chr = FALSE,
    cols_extra = cols_extra1
  )

  q2 <- query_(
    data2,
    cond = deparse(substitute(cond2), width.cutoff = 500L),
    cols_base = cols_enquo_base2,
    pivot_long = FALSE,
    as_chr = FALSE,
    cols_extra = cols_extra2
  )

  ## if mutating join, remove cols from q2 that already exist in q1, unless
  # they are part of join_by
  if (join_type %in% c("left", "inner")) {
    cols_rm <- setdiff(intersect(names(q2), names(q1)), join_by)
    q2 <- q2[,!names(q2) %in% cols_rm, drop = FALSE]
  }

  ## execute join
  qjoin <- join_fn(q1, q2, by = join_by)

  ## execute cond3
  query_(
    qjoin,
    cond = deparse(substitute(cond3), width.cutoff = 500L),
    cols_base = selection_to_vec(cols_enquo_base1, data1),
    pivot_long = pivot_long,
    pivot_var = pivot_var,
    pivot_val = pivot_val,
    as_chr = as_chr,
    cols_extra = selection_to_vec(cols_enquo_base2, data2),
    vars_cond_extra = unique(c(vars_cond1, vars_cond2))
  )
}



#' @noRd
extract_by <- function(x) {
  if (!is.null(names(x))) {
    out <- names(x)
  } else {
    out <- x
  }
  out
}


#' @noRd
#' @importFrom dplyr select
#' @importFrom rlang `!!`
selection_to_vec <- function(x, dat) {
  names(dplyr::select(dat, !!x))
}

