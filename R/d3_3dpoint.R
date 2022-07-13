#' 3d demo https://gist.github.com/hlvoorhees/5986172
#' \code {d3_3dpoint} creates a 3d scatter plot.
#' @export
#' @param data data in JSON format, created by d3plot() or from another source
#' @param radius (optional) Affects the size of the dots, numerical scale
#' @param orient (optional) Affects the starting orientation of the graph, pass in a string with any combination of two axis "Xy"
#' @param xLabel (optional) Changes the label of the x axis from "x" to specified string
#' @param yLabel (optional) Changes the label of the y axis from "y" to specified string
#' @param zLabel (optional) Changes the label of the x axis from "x" to specified string
#' @param dataMargin (optional) Affects the margin of domain around the data, numerical percentage 0-100
#' @param xMin (optional) Changes the start of the x axis domain to the specified number, numerical
#' @param xMax (optional) Changes the end of the x axis domain to the specified number, numerical
#' @param yMin (optional) Changes the start of the y axis domain to the specified number, numerical
#' @param yMax (optional) Changes the end of the y axis domain to the specified number, numerical
#' @param zMin (optional) Changes the start of the z axis domain to the specified number, numerical
#' @param zMax (optional) Changes the end of the z axis domain to the specified number, numerical
#' @param labelFontSize (optional) Affects the size of the axis labels, numerical best between 5-20
#' @param numTicks (optional) Affects the number of ticks, numerical
#' @param tickSize (optional) Affects the size of axis ticks, numerical
#' @param opacity (optional) Affects the opacity of the dots, 0-1 scale
#' @param labelColor (optional) Affects the color of the axis labels and ticks, string css colors
#' @param axisColor (optional) Affects the color of the axis, string css colors
#' @param stems (optional) Adds stems that connect to each point, boolean
#' @param stemsOpacity (optional) Affects the opacirt of the stems, 0-1 scale
#' @param browser (mandatory/optional) If code editor has viewer browser is optional if not browser opens visual in a browser window
#'@examples{
#'\dontrun[
#'d3plot(x = Sepal.Length, y = Sepal.Width, z = Petal.Length, color = Species, data = iris) |> d3_3dpoint()
#'d3plot(x = cyl, y = gear, z = wt, color = vs, data = mtcars) |> d3_3dpoint()
#']
#'}
#'@export
#'@export
d3_3dpoint <- function(data, ...){
  arguments <- as.list(match.call())[-1]
  tmp <- paste0('
<!DOCTYPE html>
<html>
  <head>
    <meta http-equiv="X-UA-Compatible" content="chrome=1" />
    <meta http-equiv="Content-Type" content="text/html;charset=utf-8" />
    <title>3D Scatter Plot</title>
    <script type="text/javascript" src="http://d3js.org/d3.v3.min.js"></script>
    <script
      type="text/javascript"
      src="https://cddesja.github.io/js/x3dom-full.js"
    ></script>
    <style>
      body {
        margin: auto;
        background-color: white;
      }
      div {
        backgroumd-color: white;
      }
      header {
        background-color: white;
      }

      /*
      * X3DOM JavaScript Library
      * http://www.x3dom.org
      *
      * (C)2009 Fraunhofer IGD, Darmstadt, Germany
      * Dual licensed under the MIT and GPL
      *
      * Based on code originally provided by
      * Philip Taylor: http://philip.html5.org
      */

      .x3dom-canvas {
        border: none;
        cursor: pointer;
        cursor: -webkit-grab;
        cursor: grab;
        width: 100%;
        height: 100%;
      }

      /* .x3dom-canvas-mousedown {
        cursor: -webkit-grabbing;
        cursor: grabbing;
      } */

      .x3dom-canvas:focus {
        outline: none;
      }
    </style>
  </head>

  <body>
    <header></header>
    <div id="divPlot"></div>
    <script>
          var rows =', data, ';
      d3.select("html").style("height", "100%").style("width", "100%");
      d3.select("body").style("height", "100%").style("width", "100%");
      d3.select("#divPlot").style("width", "100%").style("height", "100%");

      var x3d = d3
        .select("#divPlot")
        .append("x3d")
        .style("width", "100%")
        .style("height", "100%")
        .style("border", "none");

      var scene = x3d.append("scene");

      scene
        .append("orthoviewpoint")
        .attr("centerOfRotation", [0, 0, 0])
        .attr("fieldOfView", [-9, -11.5, 17, 17])')
        if(any(names(arguments) == "orient")){
          if ((tolower(eval(arguments$orient)) == "xy"|(tolower(eval(arguments$orient)) == "yx"))){
            tmp <- paste0(tmp, '.attr("orientation", [0, 0, 0, 0])')
          } else if ((tolower(eval(arguments$orient)) == "xz"|(tolower(eval(arguments$orient)) == "zx"))){
            tmp <- paste0(tmp, '.attr("orientation", [1, 0, 0, Math.PI / 2])')
          } else if ((tolower(eval(arguments$orient)) == "zy")|(tolower(eval(arguments$orient)) == "yz")){
            tmp <- paste0(tmp, '.attr("orientation", [0, 1, 0, -Math.PI / 2])')
          } else {
            tmp <- paste0(tmp, '.attr("orientation", [-0.5, 1, 0.2, (1.12 * Math.PI) / 4])')
          }
        } else {
          tmp <- paste0(tmp, '.attr("orientation", [-0.5, 1, 0.2, (1.12 * Math.PI) / 4])')
          }
        tmp <- paste0(tmp,'
        .attr("position", [0, 0, 0]);

      //  var rows = initializeDataGrid();
      var axisRange = [0, 12];
      var scales = [];
      var initialDuration = 0;
      var defaultDuration = 800;
      var ease = "linear";
      var time = 0;
      var axisKeys = ["x", "y", "z"];')
        if(any(names(arguments) == "xLabel")& any(names(arguments) == "yLabel") & any(names(arguments) == "zLabel")){
          tmp <- paste0(tmp, 'var axisNameKey = ["', eval(arguments$xLabel), '","',eval(arguments$yLabel),'","',eval(arguments$zLabel),'"];')
        } else if(any(names(arguments) == "xLabel")& any(names(arguments) == "yLabel")){
          tmp <- paste0(tmp, 'var axisNameKey = ["', eval(arguments$xLabel), '","',eval(arguments$yLabel),'","z"];')
        } else if(any(names(arguments) == "xLabel")&  any(names(arguments) == "zLabel")){
          tmp <- paste0(tmp, 'var axisNameKey = ["', eval(arguments$xLabel), '","y","',eval(arguments$zLabel),'"];')
        } else if(any(names(arguments) == "yLabel") & any(names(arguments) == "zLabel")){
          tmp <- paste0(tmp, 'var axisNameKey = ["x","',eval(arguments$yLabel),'","',eval(arguments$zLabel),'"];')
        }else if(any(names(arguments) == "xLabel")){
          tmp <- paste0(tmp, 'var axisNameKey = ["',eval(arguments$xLabel),'","y","z"];')
        }else if(any(names(arguments) == "yLabel")){
          tmp <- paste0(tmp, 'var axisNameKey = ["x","',eval(arguments$yLabel),'","z"];')
        }else if(any(names(arguments) == "zLabel")){
          tmp <- paste0(tmp, 'var axisNameKey = ["x","y","',eval(arguments$zLabel),'"];')
        }else
          tmp <- paste0(tmp, 'var axisNameKey = ["x", "y", "z"];')
        tmp <- paste0(tmp,'
      

      // Helper functions for initializeAxis() and drawAxis()
      function axisName(name, axisIndex) {
        return ["x", "y", "z"][axisIndex] + name;
      }

      function constVecWithAxisValue(otherValue, axisValue, axisIndex) {
        var result = [otherValue, otherValue, otherValue];
        result[axisIndex] = axisValue;
        return result;
      }

      // Used to make 2d elements visible
      function makeSolid(selection, color) {
        selection
          .append("appearance")
          .append("material")')
          if(any(names(arguments) == "labelColor")){
            tmp <- paste0(tmp, '.attr("diffuseColor", color || "', eval(arguments$labelColor), '")')
          } else
            tmp <- paste0(tmp, '.attr("diffuseColor", color || "black");')
          tmp <- paste0(tmp,'
          
        return selection;
      }

      // Initialize the axes lines and labels.
      function initializePlot() {
        initializeAxis(0);
        initializeAxis(1);
        initializeAxis(2);
      }

      function initializeAxis(axisIndex) {
        var key = axisKeys[axisIndex];
        drawAxis(axisIndex, key, initialDuration);
        
        var nameKey = axisNameKey[axisIndex];

        var scaleMin = axisRange[0];
        var scaleMax = axisRange[1];

        // the axis line
        var newAxisLine = scene
          .append("transform")
          .attr("class", axisName("Axis", axisIndex))
          .attr(
            "rotation",
            [
              [0, 0, 0, 0],
              [0, 0, 1, Math.PI / 2],
              [0, 1, 0, -Math.PI / 2],
            ][axisIndex]
          )
          .append("shape");
        newAxisLine
          .append("appearance")
          .append("material")')
          if(any(names(arguments) == "axisColor")){
            tmp <- paste0(tmp, '.attr("emissiveColor","', eval(arguments$axisColor), '")')
          } else
            tmp <- paste0(tmp, '.attr("emissiveColor", "#788585");')
          tmp <- paste0(tmp,'
          
        newAxisLine
          .append("polyline2d")
          // Line drawn along y axis does not render in Firefox, so draw one
          // along the x axis instead and rotate it (above).
          .attr("lineSegments", "0 0," + scaleMax + " 0");

        // axis labels
        var newAxisLabel = scene
          .append("transform")
          .attr("class", axisName("AxisLabel", axisIndex))
          .attr(
            "translation",
            constVecWithAxisValue(
              0,
              scaleMin + 1.1 * (scaleMax - scaleMin),
              axisIndex
            )
          );

        var newAxisLabelShape = newAxisLabel
          .append("billboard")
          .attr("axisOfRotation", "0 0 0") // face viewer
          .append("shape")
          .call(makeSolid);')

          
        if(any(names(arguments) == "labelFontSize")){
          tmp <- paste0(tmp, 'var labelFontSize = ', eval(arguments$labelFontSize), '/10;')
        } else
          tmp <- paste0(tmp, 'var labelFontSize = .6;')
        tmp <- paste0(tmp,'

        newAxisLabelShape
          .append("text")
          .attr("class", axisName("AxisLabelText", axisIndex))
          .attr("solid", "true")
          .attr("string", nameKey)
          .append("fontstyle")
          .attr("size", labelFontSize)
          .attr("family", "Helvetica")
          .attr("justify", "END MIDDLE");
      }

      // Assign key to axis, creating or updating its ticks, grid lines, and labels.
      var cirColor = d3.scale.category10();

      function drawAxis(axisIndex, key, duration) {
        ')
        if(any(names(arguments) == "dataMargin")){
          tmp <- paste0(tmp, 'var dataMargin = ', eval(arguments$dataMargin)/100, ';')
        } else
          tmp <- paste0(tmp, 'var dataMargin = 0;')
        tmp <- paste0(tmp,'
        var xBuff =
          (d3.max(rows, (d) => d.x) - d3.min(rows, (d) => d.x)) * dataMargin;
        var yBuff =
          (d3.max(rows, (d) => d.y) - d3.min(rows, (d) => d.y)) * dataMargin;
        var zBuff =
          (d3.max(rows, (d) => d.z) - d3.min(rows, (d) => d.z)) * dataMargin;
      
        var scale = d3.scale
          .linear()
          .domain([1, 10]) // sets the axis tick labels
          .range(axisRange); // range refers to length in px of axis')
        
        if(any(names(arguments) == "xMin")& any(names(arguments) == "xMax")){
          tmp <- paste0(tmp, '
            var xScale = d3.scale
              .linear()
              .domain([', eval(arguments$xMin), ',',eval(arguments$xMax),'])
              .range(axisRange); // range refers to length in px of axis')
        } else if(any(names(arguments) == "xMin")){
          tmp <- paste0(tmp, '
            var xScale = d3.scale
              .linear()
              .domain([', eval(arguments$xMin), ',d3.max(rows, (d) => d.x) + xBuff])
              .range(axisRange); // range refers to length in px of axis')
        } else if(any(names(arguments) == "xMax")){
          tmp <- paste0(tmp, '
            var xScale = d3.scale
              .linear()
              .domain([d3.min(rows, (d) => d.x) - xBuff,', eval(arguments$xMax), '])
              .range(axisRange); // range refers to length in px of axis')
        } else {
          tmp <- paste0(tmp, '
            var xScale = d3.scale
              .linear()
              .domain([
                d3.min(rows, (d) => d.x) - xBuff,
                d3.max(rows, (d) => d.x) + xBuff,
              ]) // demo data range
              .range(axisRange); // range refers to length in px of axis             
          ')
        }
        tmp <- paste0(tmp,'')
        
        if(any(names(arguments) == "yMin")& any(names(arguments) == "yMax")){
          tmp <- paste0(tmp, '
            var yScale = d3.scale
              .linear()
              .domain([', eval(arguments$yMin), ',',eval(arguments$yMax),'])
              .range(axisRange); // range refers to length in px of axis')
        } else if(any(names(arguments) == "yMin")){
          tmp <- paste0(tmp, '
            var yScale = d3.scale
              .linear()
              .domain([', eval(arguments$yMin), ',d3.max(rows, (d) => d.y) + yBuff])
              .range(axisRange); // range refers to length in px of axis')
        } else if(any(names(arguments) == "yMax")){
          tmp <- paste0(tmp, '
            var yScale = d3.scale
              .linear()
              .domain([d3.min(rows, (d) => d.y) - yBuff,', eval(arguments$yMax), '])
              .range(axisRange); // range refers to length in px of axis')
        } else {
          tmp <- paste0(tmp, '
            var yScale = d3.scale
              .linear()
              .domain([
                d3.min(rows, (d) => d.y) - yBuff,
                d3.max(rows, (d) => d.y) + yBuff,
              ]) // demo data range
              .range(axisRange); // range refers to length in px of axis             
          ')
        }
        tmp <- paste0(tmp,'')
        
        if(any(names(arguments) == "zMin")& any(names(arguments) == "zMax")){
          tmp <- paste0(tmp, '
            var zScale = d3.scale
              .linear()
              .domain([', eval(arguments$zMin), ',',eval(arguments$zMax),'])
              .range(axisRange); // range refers to length in px of axis')
        } else if(any(names(arguments) == "zMin")){
          tmp <- paste0(tmp, '
            var zScale = d3.scale
              .linear()
              .domain([', eval(arguments$zMin), ',d3.max(rows, (d) => d.z) + zBuff])
              .range(axisRange); // range refers to length in px of axis')
        } else if(any(names(arguments) == "zMax")){
          tmp <- paste0(tmp, '
            var zScale = d3.scale
              .linear()
              .domain([d3.min(rows, (d) => d.z) - zBuff,', eval(arguments$zMax), '])
              .range(axisRange); // range refers to length in px of axis')
        } else {
          tmp <- paste0(tmp, '
            var zScale = d3.scale
              .linear()
              .domain([
                d3.min(rows, (d) => d.z) - zBuff,
                d3.max(rows, (d) => d.z) + zBuff,
              ]) // demo data range
              .range(axisRange); // range refers to length in px of axis             
          ')
        }
        tmp <- paste0(tmp,'

        scales[0] = xScale;
        scales[1] = yScale;
        scales[2] = zScale;')
        
        if(any(names(arguments) == "numTicks")){
          tmp <- paste0(tmp, 'var numTicks = ', eval(arguments$numTicks), ';')
        } else
          tmp <- paste0(tmp, 'var numTicks = 5;')
        tmp <- paste0(tmp,'')
        
        if(any(names(arguments) == "tickSize")){
          tmp <- paste0(tmp, 'var tickSize = ', eval(arguments$tickSize)/100, ';')
        } else
          tmp <- paste0(tmp, 'var tickSize = 0.1;')
        tmp <- paste0(tmp,'
        
        var tickFontSize = 0.5;')

         if(any(names(arguments) == "tickNumbers")){
          if(eval(arguments$tickNumbers) == T){
            tmp <- paste0(tmp, '
            // ticks along each axis
            var ticks = scene
              .selectAll("." + axisName("Tick", 0))
              .data(xScale.ticks(numTicks));
            var newTicks = ticks
              .enter()
              .append("transform")
              .attr("class", axisName("Tick", 0));
            newTicks
              .append("shape")
              .call(makeSolid)
              .append("box")
              .attr("size", tickSize + " " + tickSize + " " + tickSize);
            // enter + update
            ticks
              .transition()
              .duration(duration)
              .attr("translation", function (tick) {
                return constVecWithAxisValue(0, xScale(tick), 0);
              });
            ticks.exit().remove();

            // tick labels
            var tickLabels = ticks
              .selectAll("billboard shape text")
              .data(function (d) {
                return [d];
              });
            var newTickLabels = tickLabels
              .enter()
              .append("billboard")
              .attr("axisOfRotation", "0 0 0")
              .append("shape")
              .call(makeSolid);
            newTickLabels
              .append("text")
              .attr("string", xScale.tickFormat(10))
              .attr("solid", "true")
              .append("fontstyle")
              .attr("size", tickFontSize)
              .attr("family", "Helvetica")
              .attr("shape-rendering", "crispEdges")
              .attr("justify", "END MIDDLE");
            // tickLabels // enter + update
            //   .attr("string", scale.tickFormat(10));
            // tickLabels.exit().remove();

            // ticks along each axis
            var ticks = scene
              .selectAll("." + axisName("Tick", 1))
              .data(yScale.ticks(numTicks));
            var newTicks = ticks
              .enter()
              .append("transform")
              .attr("class", axisName("Tick", 1));
            newTicks
              .append("shape")
              .call(makeSolid)
              .append("box")
              .attr("size", tickSize + " " + tickSize + " " + tickSize);
            // enter + update
            ticks
              .transition()
              .duration(duration)
              .attr("translation", function (tick) {
                return constVecWithAxisValue(0, yScale(tick), 1);
              });
            ticks.exit().remove();

            // tick labels
            var tickLabels = ticks
              .selectAll("billboard shape text")
              .data(function (d) {
                return [d];
              });
            var newTickLabels = tickLabels
              .enter()
              .append("billboard")
              .attr("axisOfRotation", "0 0 0")
              .append("shape")
              .call(makeSolid);
            newTickLabels
              .append("text")
              .attr("string", yScale.tickFormat(10))
              .attr("solid", "true")
              .append("fontstyle")
              .attr("size", tickFontSize)
              .attr("family", "Helvetica")
              .attr("shape-rendering", "crispEdges")
              .attr("justify", "END MIDDLE");
            // tickLabels // enter + update
            //   .attr("string", scale.tickFormat(10));
            // tickLabels.exit().remove();

            // ticks along each axis
            var ticks = scene
              .selectAll("." + axisName("Tick", 2))
              .data(zScale.ticks(numTicks));
            var newTicks = ticks
              .enter()
              .append("transform")
              .attr("class", axisName("Tick", 2));
            newTicks
              .append("shape")
              .call(makeSolid)
              .append("box")
              .attr("size", tickSize + " " + tickSize + " " + tickSize);
            // enter + update
            ticks
              .transition()
              .duration(duration)
              .attr("translation", function (tick) {
                return constVecWithAxisValue(0, zScale(tick), 2);
              });
            ticks.exit().remove();

            // tick labels
            var tickLabels = ticks
              .selectAll("billboard shape text")
              .data(function (d) {
                return [d];
              });
            var newTickLabels = tickLabels
              .enter()
              .append("billboard")
              .attr("axisOfRotation", "0 0 0")
              .append("shape")
              .call(makeSolid);
            newTickLabels
              .append("text")
              .attr("string", zScale.tickFormat(10))
              .attr("solid", "true")
              .append("fontstyle")
              .attr("size", tickFontSize)
              .attr("family", "Helvetica")
              .attr("shape-rendering", "crispEdges")
              .attr("justify", "END MIDDLE");
            // tickLabels // enter + update
            //   .attr("string", scale.tickFormat(10));
            // tickLabels.exit().remove();
            ')
        } else
          tmp <- paste0(tmp, '
          // ticks along each axis
          var ticks = scene
            .selectAll("." + axisName("Tick", axisIndex))
            .data(scale.ticks(numTicks));
          var newTicks = ticks
            .enter()
            .append("transform")
            .attr("class", axisName("Tick", axisIndex));
          newTicks
            .append("shape")
            .call(makeSolid)
            .append("box")
            .attr("size", tickSize + " " + tickSize + " " + tickSize);
          // enter + update
          ticks
            .transition()
            .duration(duration)
            .attr("translation", function (tick) {
              return constVecWithAxisValue(0, scale(tick), axisIndex);
            });
          ticks.exit().remove();
        ')} else
          tmp <- paste0(tmp, '
          // ticks along each axis
          var ticks = scene
            .selectAll("." + axisName("Tick", axisIndex))
            .data(scale.ticks(numTicks));
          var newTicks = ticks
            .enter()
            .append("transform")
            .attr("class", axisName("Tick", axisIndex));
          newTicks
            .append("shape")
            .call(makeSolid)
            .append("box")
            .attr("size", tickSize + " " + tickSize + " " + tickSize);
          // enter + update
          ticks
            .transition()
            .duration(duration)
            .attr("translation", function (tick) {
              return constVecWithAxisValue(0, scale(tick), axisIndex);
            });
          ticks.exit().remove();
        ')
        tmp <- paste0(tmp,'
        

        

        // base grid lines
        if (axisIndex == 0 || axisIndex == 2) {
          var gridLines = scene
            .selectAll("." + axisName("GridLine", axisIndex))
            .data(scale.ticks(numTicks));
          gridLines.exit().remove();')

        if(any(names(arguments) == "gridLines")){
          if(eval(arguments$gridLines) == T){
            tmp <- paste0(tmp, '
            // adds the gridlines for two axes 
            // should match up with the stems.
            var newGridLines = gridLines
              .enter()
              .append("transform")
              .attr("class", axisName("GridLine", axisIndex))
              .attr(
                "rotation",
                axisIndex == 0 ? [0, 1, 0, -Math.PI / 2] : [0, 0, 0, 0]
              )
              .append("shape");

            newGridLines
              .append("appearance")
              .append("material")
              .attr("emissiveColor", "#ededed");
            newGridLines.append("polyline2d");
            ')
          }
        }
        tmp <- paste0(tmp,'

          gridLines
            .selectAll("shape polyline2d")
            .transition()
            .duration(duration)
            .attr("lineSegments", "0 0, " + axisRange[1] + " 0");

          gridLines
            .transition()
            .duration(duration)
            .attr(
              "translation",
              axisIndex == 0
                ? function (d) {
                    return scale(d) + " 0 0";
                  }
                : function (d) {
                    return "0 0 " + scale(d);
                  }
            );
        }
      }

      // Update the data points (spheres) and stems.
      function plotData(duration) {
        if (!rows) {
          console.log("no rows to plot.");
          return;
        }

        var x = scales[0],
          y = scales[1],
          z = scales[2];')
        if(any(names(arguments) == "radius")){
          tmp <- paste0(tmp, 'var sphereRadius = ', eval(arguments$radius), '/100;')
        } else
          tmp <- paste0(tmp, 'var sphereRadius = 0.15;')
        tmp <- paste0(tmp,'
        

        // Draw a sphere at each x,y,z coordinate.
        var datapoints = scene.selectAll(".datapoint").data(rows);
        datapoints.exit().remove();

        var newDatapoints = datapoints
          .enter()
          .append("transform")
          .attr("class", "datapoint")
          .attr("scale", [sphereRadius, sphereRadius, sphereRadius])
          .style("opacity", 0.1)
          .append("shape");
        newDatapoints.append("appearance").append("material");
        newDatapoints.append("sphere");
        // Does not work on Chrome; use transform instead
        //.attr("radius", sphereRadius)

        datapoints
          .selectAll("shape appearance material")
          .attr("diffuseColor", "black")
          .attr("diffuseColor", function (rows) {
            return cirColor(rows.color);
          })')
          if(any(names(arguments) == "opacity")){
            tmp <- paste0(tmp, '.attr("transparency",1-', eval(arguments$opacity), ')')
          } 
          tmp <- paste0(tmp,'
          .append("title")
          .text(function (rows) {
            return rows.color;
          });

        datapoints
          .transition()
          .ease(ease)
          .duration(duration)
          .attr("translation", function (row) {
            return (
              x(row[axisKeys[0]]) +
              " " +
              y(row[axisKeys[1]]) +
              " " +
              z(row[axisKeys[2]])
            );
          });

        // Draw a stem from the x-z plane to each sphere at elevation y.
        // This convention was chosen to be consistent with x3d primitive ElevationGrid.
        var stems = scene.selectAll(".stem").data(rows);
        stems.exit().remove();

        var newStems = stems
          .enter()
          .append("transform")
          .attr("class", "stem")
          .append("shape");
        newStems
          .append("appearance")
          .append("material")')
          if(any(names(arguments) == "stems")){
            if(eval(arguments$stems) == T){
              tmp <- paste0(tmp, '.attr("emissiveColor", function (rows) { return cirColor(rows.color);})')
              } else tmp <- paste0(tmp,'.attr("emissiveColor", none)')
          } else tmp <- paste0(tmp,'.attr("emissiveColor", none)')
          tmp <- paste0(tmp, '')
          if(any(names(arguments) == "stemsOpacity")){
            tmp <- paste0(tmp, '.attr("transparency",1-', eval(arguments$stemsOpacity), ')')
          } 
          tmp <- paste0(tmp,';
  
        newStems.append("polyline2d").attr("lineSegments", function (row) {
          return "0 1, 0 0";
        });

        stems
          .transition()
          .ease(ease)
          .duration(duration)
          .attr("translation", function (row) {
            return x(row[axisKeys[0]]) + " 0 " + z(row[axisKeys[2]]);
          })
          .attr("scale", function (row) {
            return [1, y(row[axisKeys[1]])];
          });
      }

      // initializeDataGrid();
      initializePlot();
      plotData(defaultDuration);
      //   setInterval(plotData(defaultDuration), defaultDuration);
    </script>
  </body>
</html>

')
  d3plot:::show_d3(tmp,arguments)
}
