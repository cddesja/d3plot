setup_axes <- function(arguments){
  setup <- paste0(setup_extents(),'
                      svg.append("g")
                       .attr("class", "x axis")
                       .attr("transform", `translate(0,${heightValue-margin.bottom})`)
                       .call(xAxisBot)
                       .append("text")
                       .attr("class", "label")
                       .attr("x", (width/2)+margin.left)
                       .attr("y", 29)
                       .style("text-anchor", "middle")')
                      if(any(names(arguments) == "xTitle")){
                        setup <- paste0(setup, '
                                            .text("',eval(arguments$xTitle),'");')
                      } else  setup <- paste0(setup,'
                                            .text("");')
                      setup <- paste0(setup,' ;
                       
                      svg.append("g")
                       .attr("class", "x axis")
                       .attr("transform", `translate(0,${margin.top})`)
                       .call(xAxisTop)
                       .append("text")
                       .attr("class", "label")
                       .attr("x", (width/2)+margin.left)
                       .attr("y", 0 - margin.top/2.5)
                       .style("font-size", "2em")
                       .style("text-anchor", "middle")')
                      if(any(names(arguments) == "Title")){
                        setup <- paste0(setup, '
                                            .text("',eval(arguments$Title),'");')
                      } else  setup <- paste0(setup,'
                                            .text("");')
                      setup <- paste0(setup,' ;;
                       
                      svg.append("g")
                       .attr("class", "y axis")
                       .attr("transform", `translate(${margin.left},0)`)
                       .call(yAxisLeft)
                       .append("text")
                       .attr("class", "label")
                       .attr("transform", "rotate(-90)")
                       .attr("y", 0 - margin.left/1.5)
                       .attr("x", -((height / 2)+margin.top))
                       .attr("dy", ".71em")
                       .style("text-anchor", "middle")')
                       if(any(names(arguments) == "yTitle")){
                        setup <- paste0(setup, '
                        .text("',eval(arguments$yTitle),'");')
                       } else  setup <- paste0(setup,'
                        .text("");')
                       setup <- paste0(setup,' ;
                       
                      svg.append("g")
                       .attr("class", "y axis")
                       .attr("transform", `translate(${widthValue-margin.left}, 0)`)
                       .call(yAxisRight);
                      
                      
')
  return(setup)
}
