#' Create a new D3 scatter plot
#'
#'  \code{d3_point} creates a D3 scatter plot. It takes data either directly or passed to it from \code{\link{d3plot}} function.
#' @export
#' @param data data in JSON format, created by d3plot() or from another source
#' @param ... additional, optional arguments
#'
#' @examples
#' \dontrun{
#' # OK
#' d3_point(d3plot(x = mpg, y = cyl, color = cyl, id = rownames(mtcars), data = mtcars))
#' # Better
#' d3plot(x = Sepal.Length, y = Petal.Length, color = Species, data = iris) %>% d3_point(radius = 5)
#' }
#' @seealso \code{\link{d3plot}}
#'

d3_point <- function(data, ...){
  arguments <- as.list(match.call())[-1]
  tmpfile <- paste0(write_head(arguments), '
<script type="text/javascript"> var dataset =', data, ';
function type (d){
  d.x = +d.x;
  d.y = +d.y;
return d;
};')

tmpfile <- paste0(tmpfile, setup_canvas(arguments),' function render(data) {', setup_axes(),'

// Enter/binding
svg.selectAll("circle")
  .data(data)
  .enter().append("circle")
  .attr("cx", function (d) { return xScale(d.x); })
  .attr("cy", function (d) { return yScale(d.y); })')
if(grepl("color", data)){
  tmpfile <- paste0(tmpfile, '.attr("fill", function(d) { return color(d.color); })')
} else tmpfile <- paste0(tmpfile, '.attr("fill", "black")')
  if(any(names(arguments) == "radius")){
    tmpfile <- paste0(tmpfile, '.attr("r", ', eval(arguments$radius), ")")
    } else
    tmpfile <- paste0(tmpfile, '.attr("r", 5)')

tmpfile <- paste0(tmpfile,
'
  .append("title")
  .text(function(d){
    return d.id;
  });
};
render(dataset);
')
tmpfile <- paste0(tmpfile, close_html())
show_d3(tmpfile, ...)
}
