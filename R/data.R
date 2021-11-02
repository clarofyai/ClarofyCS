# PUBLIC IRON FLOTATION DATA SET
#'
#' Contains sample iron flotation time series data for optimisation
#'
#'
#' @format A data.frame with 24 columns and 2539 rows
#' \describe{
#' \item{XIronConcentrate}{Primary Dependent variable - Iron concentration in flotation product that we want to maximise}
#' \item{XSilicaConcentrate}{Secondary Dependent variable - Silica concentration in flotation product that we want to minimise}
#' \item{AminaFlow}{Flotation regent dose rate as a process control variable}
#' \item{StarchFlow}{Flotation regent dose rate as a process control variable}
#' \item{TimeStamp}{Time formated column}
#' \item{Upstream Forecasting Variables}{The variables that impact on the dependent variables but which may not be process control variables}
#' }
#'
#' @source Data are from a public Kaggle data set
#'
#' @examples
#' iron_flotation_data
#'
"iron_flotation_data"
