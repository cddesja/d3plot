#' 3d demo https://gist.github.com/hlvoorhees/5986172
#'@examples{
#'\dontrun[
#'d3plot(x = Sepal.Length, y = Sepal.Width, z = Petal.Length, color = Species, data = iris) |> d3_3dpoint()
#'d3plot(x = cyl, y = gear, z = wt, color = vs, data = mtcars) |> d3_3dpoint()
#']
#'}
#'@export
#'@export
d3_3dpoint <- function(data, ...){
  tmp <- paste0('<!DOCTYPE html >
<html >
       <head>
       <meta http-equiv="X-UA-Compatible" content="chrome=1" />
       <meta http-equiv="Content-Type" content="text/html;charset=utf-8" />
       <title>3D Scatter Plot</title>
       <script type="text/javascript" src="http://d3js.org/d3.v3.min.js"></script>
       <script type="text/javascript" src="http://x3dom.org/x3dom/dist/x3dom-full.js"></script>
       <link rel="stylesheet" type="text/css" href="http://www.x3dom.org/download/dev/x3dom.css"/>
       <style>
          body { margin: auto;
            background-color: white;
          }
          div { 
            backgroumd-color: white;
          }
          header {
            background-color: white;
          }
       </style>
       </head>
       <body>
       <header>
        
       </header>
       <div id="divPlot"></div>
       <script>
var rows =', data, ';

d3.select("html").style("height","100%").style("width","100%")
       d3.select("body").style("height","100%").style("width","100%")
       d3.select("#divPlot").style("width", "100%").style("height", "100%")
       var x3d = d3.select("#divPlot")
.append("x3d")
              .style( "width", "100%" )
              .style( "height", "100%")
              .style( "border", "none" )
              var scene = x3d.append("scene")
              scene.append("orthoviewpoint")
              .attr( "centerOfRotation", [0, 0, 0])
              .attr( "fieldOfView", [-9, -17, 17, 17])
              .attr( "orientation", [-0.5, 1, 0.2, 1.12*Math.PI/4])
              .attr( "position", [12, 10, 10])

            //  var rows = initializeDataGrid();
              var axisRange = [0, 12];
              var scales = [];
              var initialDuration = 0;
              var defaultDuration = 800;
              var ease = "linear";
              var time = 0;
              var axisKeys = ["x", "y", "z"]

              // Helper functions for initializeAxis() and drawAxis()
              function axisName( name, axisIndex ) {
              return ["x","y","z"][axisIndex] + name;
              }

              function constVecWithAxisValue( otherValue, axisValue, axisIndex ) {
              var result = [otherValue, otherValue, otherValue];
              result[axisIndex] = axisValue;
              return result;
              }

              // Used to make 2d elements visible
              function makeSolid(selection, color) {
              selection.append("appearance")
              .append("material")
              .attr("diffuseColor", color||"black")
              return selection;
              }

              // Initialize the axes lines and labels.
              function initializePlot() {
              initializeAxis(0);
              initializeAxis(1);
              initializeAxis(2);
              }

              function initializeAxis( axisIndex )
              {
              var key = axisKeys[axisIndex];
              drawAxis( axisIndex, key, initialDuration );

              var scaleMin = axisRange[0];
              var scaleMax = axisRange[1];

              // the axis line
              var newAxisLine = scene.append("transform")
              .attr("class", axisName("Axis", axisIndex))
              .attr("rotation", ([[0,0,0,0],[0,0,1,Math.PI/2],[0,1,0,-Math.PI/2]][axisIndex]))
              .append("shape")
              newAxisLine
              .append("appearance")
              .append("material")
              .attr("emissiveColor", "black")
              newAxisLine
              .append("polyline2d")
              // Line drawn along y axis does not render in Firefox, so draw one
              // along the x axis instead and rotate it (above).
              .attr("lineSegments", "0 0," + scaleMax + " 0")

              // axis labels
              var newAxisLabel = scene.append("transform")
              .attr("class", axisName("AxisLabel", axisIndex))
              .attr("translation", constVecWithAxisValue( 0, scaleMin + 1.1 * (scaleMax-scaleMin), axisIndex ))

              var newAxisLabelShape = newAxisLabel
              .append("billboard")
              .attr("axisOfRotation", "0 0 0") // face viewer
              .append("shape")
              .call(makeSolid)

              var labelFontSize = 0.6;

              newAxisLabelShape
              .append("text")
              .attr("class", axisName("AxisLabelText", axisIndex))
              .attr("solid", "true")
              .attr("string", key)
              .append("fontstyle")
              .attr("size", labelFontSize)
              .attr("family", "Helvetica")
              .attr("justify", "END MIDDLE" )
              }

              // Assign key to axis, creating or updating its ticks, grid lines, and labels.
var cirColor = d3.scale.category10();
              function drawAxis( axisIndex, key, duration ) {


var scale = d3.scale.linear()
              .domain( [1,10] ) // demo data range
              .range( axisRange )

var xScale = d3.scale.linear()
              .domain( [1,10] ) // demo data range
              .range( axisRange )

var yScale = d3.scale.linear()
              .domain( [1,8] ) // demo data range
              .range( axisRange )

var zScale = d3.scale.linear()
              .domain( [1,8] ) // demo data range
              .range( axisRange )


scales[0] = xScale;
scales[1] = yScale;
scales[2] = zScale;

var numTicks = 5;
var tickSize = 0.1;
var tickFontSize = 0.5;



              // ticks along each axis
              var ticks = scene.selectAll( "."+axisName("Tick", axisIndex) )
              .data( scale.ticks( numTicks ));
              var newTicks = ticks.enter()
              .append("transform")
              .attr("class", axisName("Tick", axisIndex));
              newTicks.append("shape").call(makeSolid)
              .append("box")
              .attr("size", tickSize + " " + tickSize + " " + tickSize);
              // enter + update
              ticks.transition().duration(duration)
              .attr("translation", function(tick) {
              return constVecWithAxisValue( 0, scale(tick), axisIndex ); })
              ticks.exit().remove();

              // tick labels


 // base grid lines
              if (axisIndex==0 || axisIndex==2) {

              var gridLines = scene.selectAll( "."+axisName("GridLine", axisIndex))
              .data(scale.ticks( numTicks ));
              gridLines.exit().remove();

              gridLines.selectAll("shape polyline2d").transition().duration(duration)
              .attr("lineSegments", "0 0, " + axisRange[1] + " 0")

              gridLines.transition().duration(duration)
              .attr("translation", axisIndex==0
              ? function(d) { return scale(d) + " 0 0"; }
              : function(d) { return "0 0 " + scale(d); }
              )
              }
              }

              // Update the data points (spheres) and stems.
              function plotData( duration ) {

              if (!rows) {
              console.log("no rows to plot.")
              return;
              }

              var x = scales[0], y = scales[1], z = scales[2];
              var sphereRadius = 0.2;

              // Draw a sphere at each x,y,z coordinate.
              var datapoints = scene.selectAll(".datapoint").data( rows );
              datapoints.exit().remove()

              var newDatapoints = datapoints.enter()
              .append("transform")
              .attr("class", "datapoint")
              .attr("scale", [sphereRadius, sphereRadius, sphereRadius])
              .append("shape");
              newDatapoints
              .append("appearance")
              .append("material");
              newDatapoints
              .append("sphere")
              // Does not work on Chrome; use transform instead
              //.attr("radius", sphereRadius)

              datapoints.selectAll("shape appearance material")
        //     .attr("diffuseColor", "black")
.attr("diffuseColor", function(rows) { return cirColor(rows.color); })
.append("title")
.text(function(rows){
    return rows.color;
              });

              datapoints.transition().ease(ease).duration(duration)
              .attr("translation", function(row) {
              return x(row[axisKeys[0]]) + " " + y(row[axisKeys[1]]) + " " + z(row[axisKeys[2]])})

              // Draw a stem from the x-z plane to each sphere at elevation y.
              // This convention was chosen to be consistent with x3d primitive ElevationGrid.
              var stems = scene.selectAll(".stem").data( rows );
              stems.exit().remove();

              var newStems = stems.enter()
              .append("transform")
              .attr("class", "stem")
              .append("shape");
              newStems
              .append("appearance")
              .append("material")
              .attr("emissiveColor", none)
              newStems
              .append("polyline2d")
              .attr("lineSegments", function(row) { return "0 1, 0 0"; })

              stems.transition().ease(ease).duration(duration)
              .attr("translation",
              function(row) { return x(row[axisKeys[0]]) + " 0 " + z(row[axisKeys[2]]); })
              .attr("scale",
              function(row) { return [1, y(row[axisKeys[1]])]; })
              }

             // initializeDataGrid();
              initializePlot();
              setInterval( plotData( defaultDuration ), defaultDuration );

       </script>
       </body>
       </html>


')
d3plot:::show_d3(tmp)
}
