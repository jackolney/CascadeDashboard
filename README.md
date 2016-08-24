# 'CascadeDashboard' -- a shiny app for investigating the current trajectory of HIV care

[![Build Status](https://travis-ci.org/jackolney/CascadeDashboard.svg)](https://travis-ci.org/jackolney/CascadeDashboard)

This model forms the basis of the shiny application available online [here](https://jackolney.shinyapps.io/CascadeDashboard/). The backend of the model consists of a deterministic model of HIV care written in C, source code available [here](https://github.com/jackolney/cascade).

## Accessing 'CascadeDashboard'

By far the easiest way of acccessing the dashboard is through the online version hosted by Rstudio - https://jackolney.shinyapps.io/CascadeDashboard/

However, some users may like to store a version locally on their machine. This can be achieved in one of two ways, but assumes prior working knowledge of [R](https://cran.r-project.org/) and [git](https://git-scm.com/):

### Method 1
Cloning the _master_ branch of this git repository and running the app natively through R as follows:

```R
git clone https://github.com/jackolney/CascadeDashboard
git checkout master
Rscript -e 'shiny::runApp()'
````
Although, this method assumes that the user has downloaded the following R packages:

- abind
- jackolney/cascade
- deSolve
- dplyr
- DT
- FME
- hadley/ggplot2
- ggrepel
- googlesheets
- grid
- gridExtra
- leaflet
- RColorBrewer
- readr
- reshape2
- rgdal
- rhandsontable
- rmarkdown
- scales
- shiny
- shinyBS
- jackolney/shinydashboard
- shinyjs
- shinythemes
- showtext
- V8

And crucially has downloaded the 'Geospatial Data Abstraction Library' and Google's V8 engine, which are available by running the following command on linux / Mac OS X machines:

`sudo -E apt-get -yq --no-install-suggests --no-install-recommends --force-yes install libgdal-dev libproj-dev libv8-dev`

### Method 2

This method is still __*under development*__, but aims at providing the 'CascadeDashboard' as a standalone R package that will download all it's dependencies in one go.

To test this package open R in the command line (ensuring [devtools](https://github.com/hadley/devtools) has been installed) and run the following:

```R
devtools::install_github('jackolney/CascadeDashboard', ref = 'as-package')
CascadeDashboard::launch()
```

Please open an issue or email me at [jack.olney11@imperial.ac.uk](mailto:jack.olney11@imperial.ac.uk) if you encounter any problems.

Copyright 2016 Jack Olney
