setup_canvas <- function(arguments){
  setup <- paste0('
  const heightValue = 500;
  const widthValue = 500;
  const margin = {top: 50, right: 50, bottom: 60, left: 50},
  width = widthValue - margin.left - margin.right,
  height = heightValue - margin.top - margin.bottom
  yTicks = 5;

  const color = d3.scaleOrdinal(d3.schemeCategory10);
  
  const svg = d3.select("body").append("svg")
  .attr("viewBox", `0 0 ${widthValue} ${heightValue}`);
  
  const xScale = d3.scaleLinear()
  .range([margin.left, widthValue - margin.right]);

  const yScale = d3.scaleLinear()
  .range([heightValue - margin.bottom, margin.top]);

  const xAxisBot = d3.axisBottom()
  .scale(xScale)
  .ticks(yTicks)')
  if(any(names(arguments) == "xGrid")){
    setup <- paste0(setup, '.tickSize(-height)')
  }
  setup <- paste0(setup, '
  .tickSizeOuter(0);

  const xAxisTop = d3.axisTop()
  .scale(xScale)
  .ticks(yTicks)
  .tickFormat("")')
  if(any(names(arguments) == "xGrid")){
    setup <- paste0(setup, '.tickSize(0)')
  } else setup <- paste0(setup,'.tickSizeOuter(0);')
  setup <- paste0(setup, '

  const yAxisLeft = d3.axisLeft()
  .scale(yScale)
  .ticks(yTicks)')
  if(any(names(arguments) == "yGrid")){
    setup <- paste0(setup, '.tickSize(-width)')
  }
  setup <- paste0(setup, '
  .tickSizeOuter(0);

  const yAxisRight = d3.axisRight()
  .scale(yScale)
  .ticks(yTicks)
  .tickFormat("")')
  if(any(names(arguments) == "yGrid")){
    setup <- paste0(setup, '.tickSize(0)')
  } else setup <- paste0(setup,'.tickSizeOuter(0);')
  setup <- paste0(setup, '
                  ')
  return(setup)
}
