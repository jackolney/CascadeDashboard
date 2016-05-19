# devtools::install_github("jackolney/WhoCascade/cascade", ref = "master")
# devtools::load_all(pkg = "~/git/WhoCascade/cascade")

if (version$os == "darwin13.4.0") {
    loc <- "/Library/Frameworks/R.framework/Versions/3.3/Resources/library"
} else if (version$os == "linux-gnu") {
    loc <- "/home/DIDE/jjo11/R/x86_64-pc-linux-gnu-library/3.2/"
}

.libPaths(c(.libPaths(), loc))

library(cascade)
library(deSolve)
library(dplyr)
library(DT)
library(ggplot2)
library(googlesheets)
library(grid)
library(gridExtra)
library(RColorBrewer)
library(readr)
library(rmarkdown)
library(scales)
library(shiny)
library(shinyjs)
library(testthat)
library(V8)
