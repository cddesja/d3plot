setup_canvas <- function(...){
  setup <- paste0('
  const heightValue = 600;
  const widthValue = 500;
  const margin = {top: 50, right: 50, bottom: 60, left: 50},
  width = heightValue - margin.left - margin.right,
  height = widthValue - margin.top - margin.bottom
  yTicks = 5;

  const color = d3.scaleOrdinal(d3.schemeCategory10);

  const xScale = d3.scaleLinear()
  .range([0, width]);

  const yScale = d3.scaleLinear()
  .range([height, 0]);

  const xAxisBot = d3.axisBottom()
  .scale(xScale)
  .ticks(yTicks)
  .tickSizeOuter(0);

  const xAxisTop = d3.axisTop()
  .scale(xScale)
  .ticks(yTicks)
  .tickFormat("")
  .tickSizeOuter(0);

  const yAxisLeft = d3.axisLeft()
  .scale(yScale)
  .ticks(yTicks)
  .tickSizeOuter(0);

  const yAxisRight = d3.axisRight()
  .scale(yScale)
  .ticks(yTicks)
  .tickFormat("")
  .tickSizeOuter(0);

  const svg = d3.select("body").append("svg")
  .attr("width", width + margin.left + margin.right)
  .attr("height", height + margin.top + margin.bottom)
  .append("g")
  .attr("transform", "translate(" + margin.left + "," + margin.top + ")");')
  return(setup)
}
