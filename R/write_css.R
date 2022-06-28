write_css <- function(arguments){
  css <- paste0("<style type='text/css'>")
  if(any(names(arguments) == "fontsize")) {
    css <- paste0(css, " body { font: ", eval(arguments$fontsize), "px ")
  } else {
    css <- paste0(css, " body { font: 12px ")
  }
  css <- paste0(css,"text-align: center;
               
                width: 100vh;height:100%;position:;")
  if(any(names(arguments) == "font")) {
    css <- paste0(css, eval(arguments$font), ";}")
  } else {
    css <- paste0(css,  "Helvetica;}")
  }
  css <- paste0(css, " 
    circle {
      ")
      if(any(names(arguments) == "stroke-width")){
        css <- paste0(css, 'stroke-width:', eval(arguments$stroke-width), "px;")
      }
      css <- paste0(css, "
      ")
      if(any(names(arguments) == "stroke")){
        css <- paste0(css, 'stroke:', eval(arguments$stroke), ";")
      }
      css <- paste0(css, "
      ")
      if(any(names(arguments) == "opacity")){
        css <- paste0(css, 'fill-opacity:', eval(arguments$opacity), ";")
      }
      css <- paste0(css, "
      } 
    line { stroke-width: 2px; } 
    .tick line { ")
      if(any(names(arguments) == "gridColor")){
        css <- paste0(css, 'stroke:', eval(arguments$gridColor), ";")
      }
      css <- paste0(css, "
      stroke-width: 1;")
      if(any(names(arguments) == "gridOpacity")){
        css <- paste0(css, 'opacity:', eval(arguments$gridOpacity), ";")
      }
      css <- paste0(css, "
      } 
    .axis path{ stroke: #3D1F1F; fill: none; stroke-width: 1; } 
    .axis text { shape-rendering: crispEdges; fill: #3D1F1F;}")
  return(css)
}
