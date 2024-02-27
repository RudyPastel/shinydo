#' Start an internal shiny app
#'
#' Locate internal shiny apps, list their names and run one.
#'
#' @param appName The name as a single character string.
#' @param ... Passed to [shiny::runApp()]
#'
#' @return
#'
#' [getInternalAppFolder()] returns a folder path as a character string.
#' [getInternalAppNames()] returns app names as a character vector.
#' [runInternalApp()] run the apps and returns nothing.
#'
#' @export
runInternalApp = function(appName, ...){
    stopifnot(appName %in% getInternalAppNames())
    runAppArgs = list(...)
    runAppArgs[['appDir']] = file.path(getInternalAppFolder(), appName)
    do.call(what = shiny::runApp, args = runAppArgs)
}

#' @rdname runInternalApp
#' @export
getInternalAppNames = function(){
    list.dirs(
        path = getInternalAppFolder(),
        recursive = FALSE,
        full.names = FALSE
    )
}

#' @rdname runInternalApp
#' @export
getInternalAppFolder = function(appName = ''){
    system.file(package = 'shinydo', 'apps', appName)
}
