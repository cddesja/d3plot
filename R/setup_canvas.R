setup_canvas <- function(...){
  setup <- paste0('var margin = {top: 50, right: 50, bottom: 30, left: 50},
  width = 600 - margin.left - margin.right,
  height = 500 - margin.top - margin.bottom
  yTicks = 5;

  var color = d3.scaleOrdinal(d3.schemeCategory10);

  var xScale = d3.scaleLinear()
  .range([0, width]);

  var yScale = d3.scaleLinear()
  .range([height, 0]);

  var xAxisBot = d3.axisBottom()
  .scale(xScale)
  .ticks(yTicks)
  .tickSizeOuter(0);

  var xAxisTop = d3.axisTop()
  .scale(xScale)
  .ticks(yTicks)
  .tickFormat("")
  .tickSizeOuter(0);

  var yAxisLeft = d3.axisLeft()
  .scale(yScale)
  .ticks(yTicks)
  .tickSizeOuter(0);

  var yAxisRight = d3.axisRight()
  .scale(yScale)
  .ticks(yTicks)
  .tickFormat("")
  .tickSizeOuter(0);

  var svg = d3.select("body").append("svg")
  .attr("width", width + margin.left + margin.right)
  .attr("height", height + margin.top + margin.bottom)
  .append("g")
  .attr("transform", "translate(" + margin.left + "," + margin.top + ")");')
  return(setup)
}
