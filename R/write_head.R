write_head <- function(arguments){
  preface <- paste0("<!DOCTYPE html><html><head><title></title>")
  preface <- paste0(preface, write_css(arguments))
  preface <- paste0(preface, ' </style></head><body><script src="https://d3js.org/d3.v3.min.js" charset="utf-8"></script>')
  return(preface)
  }
