#' Create a new D3 scatter plot
#'
#'  \code{d3_point} creates a D3 scatter plot. It takes data either directly or passed to it from \code{\link{d3plot}} function.
#' @export
#' @param data data in JSON format, created by d3plot() or from another source
#' @param radius (optional) Affects the size of the dots, numerical scale
#' @param opacity (optional) Affects the opacity of the dot works best with larger radius, 0-1 scale
#' @param stroke (optional) Adds an outline to the point works best with larger radius, specify css color
#' @param strokeMatch (optional) Matches the outline of the point to the color of the point looks best when opacity >1, boolean
#' @param Title (optional) Gives graph a title, string ""
#' @param yTitle (optional) Gives y axis a title, string ""
#' @param xTitle (optional) Gives x axis a title, string ""
#' @param yGrid (optional) Gives graph gridlines from the y ticks, boolean
#' @param xGrid (optional) Gives graph gridlines from the x ticks, boolean
#' @param gridColor (optional) Affects the color of the gridlines, specify css color
#' @param gridOpacity (optional) Affects the opacity of the gridlines, 0-1 scale
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
  tmpfile <- paste0(d3plot:::write_head(arguments), '
<script type="text/javascript"> var dataset =', data, ';
function type (d){
  d.x = +d.x;
  d.y = +d.y;
return d;
};')
  
  
  tmpfile <- paste0(tmpfile, d3plot:::setup_canvas(arguments),' function render(data) {', d3plot:::setup_axes(arguments),'
// Enter/binding
svg.selectAll("circle")
  .data(data)
  .enter().append("circle")
  .attr("cx", function (d) { return xScale(d.x); })
  .attr("cy", function (d) { return yScale(d.y); })')
  if(any(names(arguments) == "strokeMatch")){
    if(eval(arguments$strokeMatch) == T){
      tmpfile <- paste0(tmpfile, '.style("stroke", function(d) { return color(d.color); })')}}
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
  tmpfile <- paste0(tmpfile, d3plot:::close_html())
  show_d3(tmpfile, ...)
}
