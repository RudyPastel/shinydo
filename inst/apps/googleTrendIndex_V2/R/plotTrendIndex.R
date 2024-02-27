plotTrendIndex = function(date, close, smoother, smootherSpan){
    # Plot the trend index
    color = "#434343"
    par(mar = c(4, 4, 1, 1))
    plot(x = date, y = close, type = "l",
         xlab = "Date", ylab = "Trend index",
         col = color, fg = color, col.lab = color,
         col.axis = color)
    # Optionally, display smoother
    if(smoother){
        smooth_curve <- lowess(x = as.numeric(date),
                               y = close, f = smootherSpan)
        lines(smooth_curve, col = "#E6553A", lwd = 3)
    }
}
