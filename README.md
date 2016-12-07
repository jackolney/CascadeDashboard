# 'CascadeDashboard' -- a shiny app for investigating the current trajectory of HIV care

[![Build Status](https://travis-ci.org/jackolney/CascadeDashboard.svg)](https://travis-ci.org/jackolney/CascadeDashboard)

This model forms the basis of the shiny application available online [here](https://jackolney.shinyapps.io/CascadeDashboard/). The backend of the model consists of a deterministic model of HIV care written in C, source code available [here](https://github.com/jackolney/cascade).

## Accessing 'CascadeDashboard'

By far the easiest way of acccessing the dashboard is through the online version hosted by Rstudio - https://jackolney.shinyapps.io/CascadeDashboard/

However, some users may like to store a version locally on their machine. This can be achieved by downloading the app as a 'package', but assumes prior working knowledge of [R](https://cran.r-project.org/) and [git](https://git-scm.com/):

### Installation

This method provides the 'CascadeDashboard' as a standalone R package that downloads all it's dependencies in one go.

To test this package open R in the command line (ensuring [devtools](https://github.com/hadley/devtools) has been installed) and run the following:

```R
devtools::install_github('jackolney/CascadeDashboard', ref = 'R-package')
CascadeDashboard::launch()
```

### Help

A potential error may arise if the 'Geospatial Data Abstraction Library' and Google's V8 engine are not installed.

To rectify the situation, run the following command on linux / Mac OS X machines:

`sudo -E apt-get -yq --no-install-suggests --no-install-recommends --force-yes install libgdal-dev libproj-dev libv8-dev`

Windows users are on their own here, but I aim to provide support soon.

Please open an issue or email me at [jack.olney11@imperial.ac.uk](mailto:jack.olney11@imperial.ac.uk) if you encounter any problems.

Copyright 2016 Jack Olney
