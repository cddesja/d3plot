setup_extents <- function(){
  setup <- paste0('
  // get extents and range
  const xExtent = d3.extent(data, function(d) { return d.x; }),
      xRange = xExtent[1] - xExtent[0],
      yExtent = d3.extent(data, function(d) { return d.y; }),
      yRange = yExtent[1] - yExtent[0];
  // set domain to be extent +- 5%
  xScale.domain([xExtent[0] - (xRange * .05), xExtent[1] + (xRange * .05)]);
  yScale.domain([yExtent[0] - (yRange * .05), yExtent[1] + (yRange * .05)]);')
  return(setup)
}
