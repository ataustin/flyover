# flyover
High-leve visual comparisons of distributions in data sets


## Purpose
Data sets are fragile.  Whether you refactor your data wrangling code or you require stable distributions of variables through time, understanding how your data changes from one pull to another is important.  This package provides utilities to easily visualize differences between data sets.


## How it works
This package is largely powered by [ggplot2](https://ggplot2.tidyverse.org/) for building plots, and the awesome [trelliscopejs](https://hafen.github.io/trelliscopejs/) by [Ryan Hafen](https://ryanhafen.com/) for providing a viewer to navigate them.

Vignette coming soon.


## Install
`devtools::install_github("ataustin/flyover")`

Note that the dependencies are large and may take a long time to install.