#' Create a new D3 plot
#'
#'  \code{d3plot()} initializes a d3plot object. It can be used to declare input data frame for a D3 plot and specify various aesthetics as optional input arguments.
#'
#' \code{d3plot()} is used to construct D3 plots. They can be incrementally created
#' using the forward-pipe operator from the \code{magrittr} package or without this operator. It is strongly recommended to use the forward-pipe operator.
#'
#'  @export
#'  @importFrom jsonlite toJSON
#'  @param x the x-axis (independent) variable
#'  @param y the y-axis (dependent) variable
#'  @param data the dataset containing the variables
#'  @param ... additional, optional arguments
#'
#'  @examples
#'  \dontrun{
#'  d3plot(x = mpg, y = cyl, data = mtcars) %>% d3_point()
#'  }
#'  @seealso \link{\code{d3_point}}
#'

d3plot <- function(x, y, data = NULL, ...){
  arguments <- as.list(match.call())[-1]
  if(is.null(data)){
  tmp.data <- data.frame(x = x, y = y)
  if(any(names(tmp.data) == "group"))
    tmp.data$group <- group
  if(any(names(tmp.data) == "color"))
    tmp.data$color <- color
  } else {
    x <- eval(arguments$x, data)
    y <- eval(arguments$y, data)
    tmp.data <- data.frame(x = x, y = y)
    if(any(names(arguments) == "group"))
      tmp.data$group <- eval(arguments$group, data)
    if(any(names(arguments) == "color"))
      tmp.data$color <- eval(arguments$color, data)
  }
  d3.data <- toJSON(tmp.data)
  class(d3.data) <- "d3plot"
  return(d3.data)
}


