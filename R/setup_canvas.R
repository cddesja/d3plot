setup_canvas <- function(...){
  setup <- paste0('var margin = {top: 50, right: 50, bottom: 30, left: 50},
  width = 600 - margin.left - margin.right,
  height = 500 - margin.top - margin.bottom
  yTicks = 5;

  var color = d3.scale.category10();

  var xScale = d3.scale.linear()
  .range([0, width]);

  var yScale = d3.scale.linear()
  .range([height, 0]);

  var xAxisBot = d3.svg.axis()
  .scale(xScale)
  .ticks(yTicks)
  .outerTickSize(0)
  .orient("bottom");

  var xAxisTop = d3.svg.axis()
  .scale(xScale)
  .ticks(yTicks)
  .tickFormat("")
  .outerTickSize(0)
  .orient("top");

  var yAxisLeft = d3.svg.axis()
  .scale(yScale)
  .ticks(yTicks)
  .outerTickSize(0)
  .orient("left");

  var yAxisRight = d3.svg.axis()
  .scale(yScale)
  .ticks(yTicks)
  .tickFormat("")
  .outerTickSize(0)
  .orient("right");

  var svg = d3.select("body").append("svg")
  .attr("width", width + margin.left + margin.right)
  .attr("height", height + margin.top + margin.bottom)
  .append("g")
  .attr("transform", "translate(" + margin.left + "," + margin.top + ")");')
  return(setup)
}
