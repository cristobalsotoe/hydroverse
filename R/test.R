#' Calculate Streamflow Statistics
#'
#' Computes mean, max, and min for a streamflow time series.
#'
#' @param flow Numeric vector of streamflow values.
#' @return A list with mean, max, and min values.
#' @export
#' @examples
#' flow <- c(10, 20, 15, 25)
#' streamflow_stats(flow)
streamflow_stats <- function(flow) {
  if (!is.numeric(flow)) stop("flow must be numeric")
  list(
    mean = mean(flow, na.rm = TRUE),
    max = max(flow, na.rm = TRUE),
    min = min(flow, na.rm = TRUE)
  )
}
