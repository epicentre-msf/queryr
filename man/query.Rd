% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/query.R
\name{query}
\alias{query}
\title{Data validation queries with tidy, stackable output}
\usage{
query(
  data,
  cond,
  cols_dotx,
  cols_doty,
  crossed = FALSE,
  cols_base,
  pivot_long = TRUE,
  pivot_var = "variable",
  pivot_val = "value",
  as_chr = TRUE,
  count = FALSE
)
}
\arguments{
\item{data}{A data frame}

\item{cond}{An expression to evaluate with respect to variables within
\code{data}. Can specify multiple variables using a dot-selector ("\code{.x}" and
"\code{.y}") within the expression (e.g. \code{.x > 0}) and then separately
specifying the columns that the selector refers to with arguments
\code{cols_dotx}/\code{cols_doty}.}

\item{cols_dotx, cols_doty}{Tidy-selection of one or more columns represented
by a .x or .y selector. Only used if \code{cond} contains the relevant selector.
See section \strong{Using a dot-selector} below.}

\item{crossed}{if \code{cond} contains both a .x and .y selector, should the
variables matched by \code{cols_dotx} and \code{cols_doty} be "crossed" such that all
combinations are evaluated (\code{TRUE}), or should they be evaluated in
parallel (\code{FALSE}). The latter requires that the number of variables
matched by \code{cols_dotx} and \code{cols_doty} is the same. Defaults to \code{FALSE}.}

\item{cols_base}{(Optional) Tidy-selection of other columns within \code{data} to
retain in the output. Can optionally be set for an entire session using
option "queryr_cols_base", e.g. \code{options(queryr_cols_base = quote(id:site))}.}

\item{pivot_long}{Logical indicating whether to pivot the variables
referenced within \code{cond} to a long (i.e. stackable) format, with default
column names "variable1", "value1", "variable2", "value2", ... Defaults to
\code{TRUE}.}

\item{pivot_var}{Prefix for pivoted variable column(s). Defaults to
"variable". Only used if \code{pivot_long = TRUE}.}

\item{pivot_val}{Prefix for pivoted value column(s). Defaults to "value".
Only used if \code{pivot_long = TRUE}.}

\item{as_chr}{Logical indicating whether to coerce the columns referenced in
the query expression \code{cond} to character prior to returning. This enables
row-binding multiple queries with variables of different classes, but is
only important if \code{pivot_long = TRUE}. Defaults to \code{TRUE}.}

\item{count}{Logical indicating whether to summarize the output by counting
the number of unique combinations across all returned columns (with count
column "n"). Defaults to \code{FALSE}.}
}
\value{
A data frame reflecting the rows of \code{data} that match the given query.
Returned columns include:
\itemize{
\item (optional) columns matched by argument \code{cols_base}
\item columns referenced within the query expression (pivoted to long form by
default)
\item (optional) count column "n" (if \code{count} = TRUE)
}
}
\description{
Find observations within a data frame matching a given query (a logical
expression relating to one or more variables), and return tidy output that
can be stacked across different queries on different variables. Stackability
is achieved by pivoting the columns indicated in the query expression to
long-form, e.g. "variable1", "value1", "variable2", "value2", ...

The query expression can optionally incorporate up to two dot-selectors
("\code{.x}" and "\code{.y}"), which each refer to a \emph{set} of variables specified
separately using tidy-selection (see section \strong{Using a dot-selector}). If
both selectors are used in a given query expression, the sets of variables
they respectively match can either be "crossed" such that all combinations
are evaluated, or evaluated in parallel.

By default, only the data columns referenced in the query expression are
returned, but additional columns can optionally be added with argument
\code{cols_base}.
}
\section{Using a dot-selector}{

A query expression can optionally incorporate up to two dot-selectors
("\code{.x}" and "\code{.y}"), which each refer to a \emph{set} of variables specified
separately using tidy-selection (arguments \code{cols_dotx} and \code{cols_doty}).

When \code{cond} contains a \code{.x} selector, the query expression is evaluated
repeatedly with each relevant variable from \code{cols_dotx} individually
substituted into the \code{.x} position of the expression. The results of these
multiple 'subqueries' are then combined with
\code{\link[dplyr:bind_rows]{dplyr::bind_rows}}.

If \code{cond} contains both a .x and .y selector, the sets of variables matched
by \code{cols_dotx} and \code{cols_doty} respectively can either be "crossed" such that
all combinations are evaluated, or evaluated in parallel. Evaluating in
parallel requires that the number of variables matched by \code{cols_dotx} and
\code{cols_doty} is the same.

Consider a hypothetical query checking that, if a patient has a particular
symptom, the date of onset of that symptom is not missing. E.g. \cr
\code{cond = .x == "Yes" & is.na(.y)} \cr
\code{cols_dotx = c(symptom_fever, symptom_headache)} \cr
\code{cols_doty = c(date_symptom_fever, date_symptom_headache)} \cr

If argument \code{crossed} is \code{FALSE}, the relevant variables from \code{cols_dotx} and
\code{cols_doty} will be evaluated in parallel, as in: \cr
\code{has_symptom_fever == "Yes" & is.na(date_symptom_fever)} \cr
\code{has_symptom_headache == "Yes" & is.na(date_symptom_headache)} \cr

Conversely, if argument \code{crossed} is \code{TRUE}, all combinations of the relevant
variables will be evaluated, which for this particular query wouldn't make
sense: \cr
\code{symptom_fever == "Yes" & is.na(date_symptom_fever)} \cr
\code{symptom_fever == "Yes" & is.na(date_symptom_headache) # not relevant} \cr
\code{symptom_headache == "Yes" & is.na(date_symptom_fever) # not relevant} \cr
\code{symptom_headache == "Yes" & is.na(date_symptom_headache)} \cr

Note that if a dot-selector is used with argument \code{pivot_long = FALSE}, the
row-binding of multiple subqueries may result in a sparse output with respect
to the variables represented by the dot-selector, because for each subquery
only the columns matched by expression \code{cond} are returned.
}

\examples{
# load example dataset, an epidemiological 'linelist'
data(ll)

# find observations where date_exit is earlier than date_admit
query(
  ll,
  date_exit < date_admit,
  cols_base = id:site
)

# find any date value in the future using a .x column selector
query(
  ll,
  .x > Sys.Date(),
  cols_dotx = starts_with("date"),
  cols_base = id:site
)

# incorporate an external object into the query expression
lab_result_valid <- c("Positive", "Negative", "Inc.", NA)

query(
  ll,
  !lab_result \%in\% lab_result_valid,
  cols_base = id:site,
)

}
