#' Example dataset, an epidemiological linelist from a treatment centre
#'
#' @format A data.frame with 12 rows and 10 variables:
#' \describe{
#' \item{id}{Patient identifier}
#' \item{site}{Site identifier}
#' \item{age}{Patient age in years}
#' \item{status}{Patient status}
#' \item{date_onset}{Date of symptom onset}
#' \item{date_admit}{Date of admission to treatment centre}
#' \item{date_lab}{Date of laboratory test}
#' \item{lab_result}{Result of laboratory test}
#' \item{date_exit}{Date of exit from treatment centre}
#' \item{outcome}{Patient outcome}
#' }
"ll"


#' Example dataset, a summary epidemiological linelist containing only
#' confirmed/probable cases
#'
#' @format A data.frame with 10 rows and 8 variables:
#' \describe{
#' \item{sll_id}{Patient identifier in the summary linelist}
#' \item{tc_admit}{Was patient admitted to a treatment centre?}
#' \item{tc_id}{Patient ID at treatment centre}
#' \item{tc_site}{Site of treatment centre}
#' \item{sll_age}{Date of symptom onset}
#' \item{sll_status}{Date of admission to hospital}
#' \item{sll_date_outcome}{Date of laboratory test}
#' \item{sll_outcome}{Patient outcome}
#' }
"sll"

