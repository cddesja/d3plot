#' Create a new D3 scatter plot
#'
#'  \code{d3_points()} creates a D3 scatter plot. It takes data either directly or passed to it from \link{\code{d3plot()}} function.
#'
#'
#'  @export
#'  @param data data in JSON format, created by d3plot() or from another source
#'  @param css additional css stlying
#'  @param ... additional, optional arguments
#'
#'  @examples
#'  \dontrun{
#'  # OK
#'  d3_point(d3plot(x = mpg, y = cyl, data = mtcars))
#'  # Better
#'  d3plot(x = mpg, y = cyl, data = mtcars) %>% d3_point()
#'  }
#'  @seealso \link{\code{d3plot}}
#'

d3_point <- function(data, css = FALSE){
  if(css == FALSE)
  tmpfile <- paste('
  <!DOCTYPE html>
                   <html>
                   <head>
                   <title>Simple Scatterplot</title>
                   <style type="text/css">

                   body {
                   font: 12px Helvetica;
                   }

                   circle {
                   fill: blue;
                   stroke-width: 2px;
                   }

                   line {
                   stroke-width: 2px;
                   }

                   .axis path{
                   stroke: #3D1F1F;
                   fill: none;
                   stroke-width: 1;
                   }

                   .axis text {
                   shape-rendering: crispEdges;
                   fill: #3D1F1F;
                   }
                   </style>
                   </head>
                   <body>
                   <script src="http://d3js.org/d3.v3.min.js" charset="utf-8"></script>
                   <script type="text/javascript">

                   var dataset =', data, ';
                   var margin = { left: 50, top: 10, right: 50, bottom: 50 };
                   var outerHeight = 300;
                   var outerWidth = 500;
                   var innerHeight = outerHeight - margin.top - margin.bottom;
                   var innerWidth = outerWidth - margin.left - margin.right;

                   var svg = d3.select("body")
                   .append("svg")
                   .attr("width", outerWidth)
                   .attr("height", outerHeight);

                   var g = svg.append("g")
                   .attr("transform", "translate(" + margin.left + "," + margin.top + ")" );

                   var xAxisG = g.append("g")
                   .attr("transform", "translate(-" + margin.left + "," + innerHeight + ")");
                   var yAxisG = g.append("g")
                   .attr("transform", "translate(-10,0)")
                   .attr("class" ,"axis");

                   var xScale = d3.scale.linear().range([margin.left, innerWidth]);
                   var yScale = d3.scale.linear().range([innerHeight, margin.top]);
                   var xAxis = d3.svg.axis().scale(xScale).orient("bottom");
                   var yAxis = d3.svg.axis().scale(yScale).orient("left");

                   function render(data) {
                   xScale.domain(d3.extent(data, function (d) { return d.x; }));
                   yScale.domain(d3.extent(data, function (d) { return d.y; }));

                   xAxisG
                   .attr("class" ,"axis")
                   .call(xAxis);
                   yAxisG
                   .attr("class" ,"axis")
                   .call(yAxis);

                   function type (d){
                   d.x = +d.x;
                   d.y = +d.y;
                   return d;
                   }

                   // Enter/binding
                   var circles = svg.selectAll("circle")
                   .data(data)
                   .enter().append("circle")
                   .on("mouseover", function() {
                   d3.select(this)
                   .style("fill", "red")
                   .style("fill-opacity", .75)
                   .style("stroke", "black")
                   .attr("r", 15)
                   })
                   .on("mouseout", function() {
                   d3.select(this)
                   .style("fill", "blue")
                   .style("fill-opacity", 1)
                   .style("stroke", "blue")
                   .attr("r", 5)
                   });


                   // Update
                   circles
                   .attr("cx", function (d) { return xScale(d.x); })
                   .attr("cy", function (d) { return yScale(d.y); })
                   .attr("r", 5 )
                   .attr("fill", "blue")
                   .attr("stroke", 1);



                   // Exit
                   circles.exit().remove();
                   }


                   render(dataset);
                   </script>
                   </body>
                   </html>
                   ')
  htmlFile <- tempfile(fileext=".html")
  cat(tmpfile, file = htmlFile)
  viewer <- getOption("viewer")
  viewer(htmlFile)
}
