#' Generate random trend index data
#'
#' A data frame with the following columns.
#'  * `type` (`character`)
#'  * `date` (`Date`)
#'  * `close` (`numeric`)
#'
#' @param searchEngine The name of a search engine as a character string.
#'
#' @export
generateRandomTrendIndexData = function(){
    date = seq(from =  as.Date("2007-01-01"), to = as.Date("2017-07-31"), by = 'month')
    base = (1 + cos(2 * pi * seq_along(date) / 12)) / 2


    A = base + seq_along(base) / length(base) +
        runif(n = length(base), min = -1, max = 1) / 3
    A = A / (max(A) - min(A))
    A = pmin(pmax(A,0), 1)


    B = 1 + base - seq_along(base) / length(base) +
        runif(n = length(base), min = -1, max = 1) / 3
    B = B / (max(B) - min(B))
    B = pmin(pmax(B,0), 1)


    C = 0 * base + exp(seq_along(base) / length(base)) - 1 +
        runif(n = length(base), min = -1, max = 1) / 3
    C = C / (max(C) - min(C))
    C = pmin(pmax(C,0), 1)

    trendData = data.frame(
        stringsAsFactors = FALSE,
        type = rep(x = c('A', 'B', 'C'), each = length(base)),
        date = c(date, date, date),
        close = c(A, B, C)
    )

    return(trendData)
}

#' @rdname generateRandomTrendIndexData
#' @export
getTrendIndexData = function(searchEngine){
    generateRandomTrendIndexData()
}
