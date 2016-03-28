#' Create a new D3 line plot
#'
#'  \code{d3_line()} creates a D3 line plot. It takes data either directly or passed to it from \link{\code{d3plot()}} function.
#'
#'
#'  @export
#'  @param data data in JSON format, created by d3plot() or from another source
#'  @param ... additional, optional arguments
#'
#'  @examples
#'  \dontrun{
#'  # OK
#'  d3_point(d3plot(x = mpg, y = cyl, color = cyl, id = rownames(mtcars), data = mtcars))
#'  # Better
#'  d3plot(x = Sepal.Length, y = Petal.Length, color = Species, data = iris) %>% d3_line()
#'  }
#'  @seealso \link{\code{d3plot}}
#'

d3_line <- function(data, ...){
  arguments <- as.list(match.call())[-1]
  tmpfile <- paste0(write_head(arguments), '
<script type="text/javascript"> var dataset =', data, ';
function type (d){
  d.x = +d.x;
  d.y = +d.y;
return d;
};')
  if(grepl("color", data)){
  tmpfile <- paste0(tmpfile, 'nestedData = d3.nest().key(function(d) { return d.color; }).entries(dataset);')
  }
  if(grepl("group", data)){
    tmpfile <- paste0(tmpfile, 'nestedData = d3.nest().key(function(d) { return d.group; }).entries(dataset);')
  }

tmpfile <- paste0(tmpfile, setup_canvas(arguments),' function render(data) {', setup_axes(),'
var line = d3.svg.line()
    .x(function(d) { return xScale(d.x); })
    .y(function(d) { return yScale(d.y); });
')
if(grepl("color", data) | grepl("group", data)){
  tmpfile <- paste0(tmpfile, 'var groups = svg.selectAll(".group")
      .data(nestedData, function(d) { return d.key; })
                    .enter().append("g");
                    groups.append("path")
                    .attr("class", "line")
                    .attr("d", function(d) { return line(d.values); })
                    .attr("stroke-width", "1px")
                    .attr("fill", "none")
                    .style("stroke", function(d) { return color(d.key); });')
} else tmpfile <- paste0(tmpfile,'svg.append("path")
      .datum(data)
       .attr("class", "line")
        .attr("fill", "none")
        .attr("stroke", "black")
        .attr("stroke-width", "1px")
       .attr("d", line);')
tmpfile <- paste0(tmpfile,'
};
render(dataset);
')
tmpfile <- paste0(tmpfile, close_html())
show_d3(tmpfile, ...)
}
