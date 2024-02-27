#' Get ui and server functions
#'
#' Build a ui & server pair for [shiny::shinyApp()].
#'
#' @param theme Name of a Shiny theme as listed by [shinythemes::shinythemes].
#' @template searchEngines
#'
getUi = function(searchEngines, theme = "lumen"){
    fluidPage(theme = shinytheme(theme = theme),

              lapply(
                  X = searchEngines,
                  FUN = function(searchEngine, choices){
                      indexTrendModuleUI(id = searchEngine,
                                         searchEngine = searchEngine,
                                         choices = choices)
                  },
                  choices = unique(generateRandomTrendIndexData()$type)
              )

    )
}

#' @rdname getUi
getServer = function(searchEngines){
    function(input, output) {
        for (searchEngine in searchEngines){
            indexTrendModuleServer(
                id = searchEngine,
                trend_data = getTrendIndexData(searchEngine = searchEngine)
            )
        }
    }
}

#' Start the trend index dashboard
#'
#' Explore the trend index data for various search engines
#'
#' @template searchEngines
#' @inheritParams shiny::shinyApp
#'
#' @export
#'
startTrendIndexDashboard = function(
    searchEngines = getSearchEngineNames(),
    onStart = NULL,
    options = list(),
    uiPattern = "/",
    enableBookmarking = NULL){

    shiny::shinyApp(
        ui = getUi(searchEngines = searchEngines),
        server = getServer(searchEngines = searchEngines),
        onStart = onStart,
        options = options,
        uiPattern = uiPattern,
        enableBookmarking = enableBookmarking
    )

}
