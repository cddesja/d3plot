setup_axes <- function(...){
  setup <- paste0(setup_extents(),'
svg.append("g")
                       .attr("class", "x axis")
                       .attr("transform", "translate(0," + height + ")")
                       .call(xAxisBot)
                       .append("text")
                       .attr("class", "label")
                       .attr("x", width/2)
                       .attr("y", 29)
                       .style("text-anchor", "middle")
                       .text("");

                       svg.append("g")
                       .attr("class", "x axis")
                       .call(xAxisTop);

                       svg.append("g")
                       .attr("class", "y axis")
                       .call(yAxisLeft)
                       .append("text")
                       .attr("class", "label")
                       .attr("transform", "rotate(-90)")
                       .attr("y", -49)
                       .attr("x", -(height / 2))
                       .attr("dy", ".71em")
                       .style("text-anchor", "middle")
                       .text("");

                       svg.append("g")
                       .attr("class", "y axis")
                       .attr("transform", "translate(" + width + ", 0)")
                       .call(yAxisRight);
                       ')
  return(setup)
}
