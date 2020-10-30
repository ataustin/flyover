# flyover
Painlessly generate high-level visual comparisons of distributions between groups or data sets.


## Purpose
Data processes are fragile.  Whether you refactor your data wrangling pipes, wish to monitor data drift through time, or just want to see how groups vary across many attributes, generating and organizing this information is time-consuming.  When you have many variables to monitor and want something more than statistical thresholds to compare them, this package allows you to easily visualize differences between groups or data sets.


## How it works
This package is largely powered by [ggplot2](https://ggplot2.tidyverse.org/) for building plots, and the awesome [trelliscopejs](https://hafen.github.io/trelliscopejs/) by [Ryan Hafen](https://ryanhafen.com/) for providing a viewer to navigate them.

Vignette coming soon.


## Install
`devtools::install_github("ataustin/flyover")`

Note that the dependencies are large (especially trelliscopejs) and may take a long time to install.
