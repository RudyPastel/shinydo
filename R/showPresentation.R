#' Show presentation
#'
#' Locate presentations, list their names and run one in the default web browser.
#'
#' @param presentationName The name of the presentation as a character string.
#'
#' @export
showPresentation = function(presentationName = 'shinydo-presentation.html'){
    presenationFolder = system.file(package = 'shinydo', 'presentations')
    presentationPath = file.path(presenationFolder, presentationName)
    if (!file.exists(presentationPath)){
        base::stop(
            'Presentation ', presentationName,
            ' could not be found in ', presenationFolder,'.'
        )
    }
    browseURL(url = presentationPath)
}

#' @rdname showPresentation
#' @export
getPresentationFolder = function(presentationName = ''){
    system.file(package = 'shinydo', 'presentations', presentationName)
}

#' @rdname showPresentation
#' @export
getPresentationNames = function(){
    list.files(
        path = getPresentationFolder(),
        recursive = FALSE,
        full.names = FALSE
    )

}
