show_d3 <- function(tmpfile, arguments){
    if(any(names(arguments) == "browser")){
        browser <- eval(arguments$browser)
    }
    
    htmlFile <- tempfile(fileext=".html")
    cat(tmpfile, file = htmlFile)
    
    viewer <- getOption("viewer")
    if(is.null(viewer) | browser == TRUE) {
        utils::browseURL(htmlFile)
        } else {
            viewer(htmlFile)
            }
}
