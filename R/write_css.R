write_css <- function(arguments){
  css <- paste0("<style type='text/css'>")
  if(any(names(arguments) == "fontsize")) {
    css <- paste0(css, " body { font: ", eval(arguments$fontsize), "px ")
  } else {
    css <- paste0(css, " body { font: 12px ")
    }
  if(any(names(arguments) == "font")) {
    css <- paste0(css, eval(arguments$font), ";}")
  } else {
    css <- paste0(css,  "Helvetica;}")
  }
  css <- paste0(css, " circle { stroke-width: 2px; } line { stroke-width: 2px; } .tick line { stroke: black; stroke-width: 1; } .axis path{ stroke: #3D1F1F; fill: none; stroke-width: 1; } .axis text { shape-rendering: crispEdges; fill: #3D1F1F;}")
  return(css)
  }

