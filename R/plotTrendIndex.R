#' Plot trend index
#'
#' Plot trend index as a time series and optionally a smoothed version.
#'
#' @param date A `Date` vector of same length as `close`.
#' @param close Index value at close time as a `numeric` vector of same length as `date`.
#' @param smoother Should a [stats::lowess()] smoothed version of the trend index
#' be plotted as well? Either `TRUE` and `FALSE`.
#' @param smootherSpan the smoother span. This gives the proportion of points in
#' the plot which influence the smooth at each value. Larger values give more
#' smoothness.
#'
#'
#' @return Nothing silently.
#' @export
plotTrendIndex = function(date, close, smoother, smootherSpan){
    # Plot the trend index
    color = "#434343"
    par(mar = c(4, 4, 1, 1))
    plot(x = date, y = close, type = "l",
         xlab = "Date", ylab = "Trend index", col = color, fg = color, col.lab = color, col.axis = color)
    # Optionally, display smoother
    if(smoother){
        smooth_curve <- lowess(x = as.numeric(date), y = close, f = smootherSpan)
        lines(smooth_curve, col = "#E6553A", lwd = 3)
    }
}
