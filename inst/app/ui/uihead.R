# devtools::install_github("jackolney/shinydashboard", ref = "master")
# devtools::load_all(pkg = "~/git/packages/shinydashboard")

# CheckInstall function
CheckInstall <- function(pkg) {
    is.element(pkg, installed.packages()[,1])
}

if (version$os == "darwin13.4.0") {
    loc <- "/Library/Frameworks/R.framework/Versions/3.3/Resources/library"
} else if (version$os == "linux-gnu") {
    loc <- "/home/DIDE/jjo11/R/x86_64-pc-linux-gnu-library/3.2/"
}

.libPaths(c(.libPaths(), loc))

library(DT)
library(ggplot2)
library(leaflet)
library(rhandsontable)
library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinyjs)
library(shinyTable)
library(shinythemes)
library(V8)

if (CheckInstall("rgdal")) {
    library(rgdal)
} else {
    warning("\tpackage 'rgdal' not installed\n\tinteractive map will be disabled")
}

# source global-lists & misc-functions
source("ui/global-lists.R", local = TRUE)
source("server/misc-functions.R", local = TRUE)
