rm(list = ls())
###############
# Script Info #
###############
# PURPOSE: Use knitr to change .rmd into .md
# AUTHOR: http://jfisher-usgs.github.io/r/2012/07/03/knitr-jekyll/
# REVIEWED BY:
# VERSION: 0.1

KnitPost <- function(input, base.url = "/") {
  require(knitr)
  opts_knit$set(base.url = base.url)
  fig.path <- paste0("figs/", sub(".Rmd$", "", basename(input)), "/")
  opts_chunk$set(fig.path = fig.path)
  opts_chunk$set(fig.cap = "center")
  render_jekyll()
  knit(input, envir = parent.frame())
}

setwd("~/git/rCode/")
#COMPLETED
KnitPost("2014-02-07-animated_ggplot.Rmd")

#To DO



