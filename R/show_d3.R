show_d3 <- function(tmpfile, ...){
    htmlFile <- tempfile(fileext=".html")
    cat(tmpfile, file = htmlFile)
    
    viewer <- getOption("viewer")
    if(is.null(viewer)) {
        utils::browseURL(htmlFile)
        } else {
            viewer(htmlFile)
            }
}
