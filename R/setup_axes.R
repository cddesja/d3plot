setup_axes <- function(arguments){
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
                      
                      // add chart title label 
                      svg.append("text")
                       .attr("x", width/2) //could also be written (widthValue + margin.left - margin.right)/2)
                       .attr("y", -20)
                       .attr("text-anchor", "middle")
                       .style("font-size", "20px")')
                      if(any(names(arguments) == "Title")){
                        setup <- paste0(setup, '.text("',eval(arguments$Title),'");')
                      } else  setup <- paste0(setup,'.text("");')
                      setup <- paste0(setup,'
                      
                      // add y axis lable
                      svg.append("text")
                      .attr("transform", "rotate(-90)")
                      .attr("y", 0 - margin.left)
                      .attr("x",0 - (height / 2))
                      .attr("dy", "1em")
                      .style("text-anchor", "middle")')
                      if(any(names(arguments) == "yTitle")){
                        setup <- paste0(setup, '
                                .text("',eval(arguments$yTitle),'");')
                      } else  setup <- paste0(setup,'
                                .text("");')
                      setup <- paste0(setup,'  
                      
                      // add x axis lable
                      svg.append("text")             
                      .attr("transform", "translate(" + (width/2) + " ," + (height + margin.top - 10) + ")")
                      .style("text-anchor", "middle")')
                      if(any(names(arguments) == "xTitle")){
                        setup <- paste0(setup, '
                                      .text("',eval(arguments$xTitle),'");')
                      } else  setup <- paste0(setup,'
                                      .text("");')
                      setup <- paste0(setup,'
                       ')
  return(setup)
}
